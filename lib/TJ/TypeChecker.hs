module TJ.TypeChecker () where

import Control.Monad.State

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

import TJ.Parser

data Type = TUnit
          | TVariable Int
          | TLabeled T.Text
          | TFunction [Type] Type
          deriving (Show, Eq, Ord)

data Context = Context { env :: Map.Map Expression Type
                       , nonGeneric :: Set.Set Type
                       , uid :: Int
                       }
               deriving (Show)

typeVar ctx = (TVariable $ uid ctx, ctx')
    where ctx' = ctx { uid = uid ctx + 1 }

typeVars :: Context -> [a] -> ([Type], Context)
typeVars ctx ls = foldr typeVar' ([], ctx) ls
    where typeVar' _ (types, ctx) = let (t, ctx') = typeVar ctx
                                    in (types ++ [t], ctx')

analyseList :: Context -> [Either Statement Expression] -> ([Type], Context)
analyseList ctx nodes = foldr analyse' ([], ctx) nodes
    where analyse' node (types, ctx) = let (t, ctx') = analyse ctx node
                                       in  (types ++ [t], ctx')

analyse :: Context -> Either Statement Expression -> (Type, Context)
analyse ctx node =
    case node of
        Left (SAssignment (Let ident expr)) ->
            let (t, _) = analyse' expr
            in  (t, ctx { env = Map.insert (EIdentifier ident) t (env ctx)})
        Left (SModule mod) ->
            let (_, ctx') = analyseList ctx $ map Left mod
            in  (TUnit, ctx')
        Right e -> case e of
            EIdentifier ident -> (getType' e, ctx)
            EApplication fn args ->
                let (retT, ctx') = typeVar ctx
                    argTypes = map getType' args
                in  ( unify' (TFunction (map getType' args) retT) (analyse' fn)
                    , ctx' )
            EBinOp op e1 e2 ->
                let (retT, ctx') = typeVar ctx
                    args = [e1, e2]
                    argTypes = map getType' args
                in  ( unify' (TFunction (map getType' args) retT) (opType op)
                    , ctx )
            EFunction _ args expr ->
                let (argTypes, ctx') = typeVars ctx args
                    envInsert env' (a, t) = Map.insert a t env'
                    env' = foldl envInsert (env ctx')
                                           (zip (map EIdentifier args) argTypes)
                    nonGeneric' = Set.union (nonGeneric ctx') (Set.fromList argTypes)
                    (retT, _) = analyse (ctx' { env = env', nonGeneric = nonGeneric' })
                                        (Right expr)
                in  (TFunction argTypes retT, ctx')
            EBlock exprs ->
                let (types, ctx') = analyseList ctx $ map Right exprs
                in  (last types, ctx')
            EStatement s -> analyse ctx (Left s)
            _ -> (getType' e, ctx)
    where getType' = getType ctx
          analyse' e = analyse ctx (Right e)
          unify' = unify ctx

opType :: Operation -> Type
opType op = TFunction [numT, numT] numT
    where numT = TLabeled "Number"

getType :: Context -> Expression -> Type
getType ctx node =
    case Map.lookup node (env ctx) of
        Just t -> let (t', _, _) = fresh ctx t in t'
        Nothing -> case node of
            ENumber _ -> TLabeled "Number"
            EString _ -> TLabeled "String"
            _ -> error $ "Undefined: " ++ show node

-- TODO
fresh ctx t = freshrec t ctx Map.empty
    where
        freshrec t ctx m =
            let p = prune ctx t
            in case p of
                TVariable _ -> if isGeneric ctx p
                    then if Map.notMember p m
                        then let (tvar, ctx') = typeVar ctx
                             in  (tvar, ctx', Map.insert p tvar m)
                        else error "Unfinished"
                    else error "Unfinished"
                _ -> error "Unfinished"

-- TODO
prune ctx t = t

-- TODO
isGeneric ctx t = False

-- TODO
unify ctx t1 t2 = t1
