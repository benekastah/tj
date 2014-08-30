module TJ.TypeChecker () where

import Control.Monad.State

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import TJ.Parser

data Type = TVariable Int
          | TFunction [Type] Type
          | TLabeled T.Text
          deriving (Show, Eq, Ord)

data Context = Context { env :: Map.Map Expression Type
                       , typeEnv :: Map.Map Type Type
                       , nonGeneric :: Set.Set Type
                       , uid :: Int
                       }
               deriving (Show)

type TypeChecker = State Context

voidType = TLabeled "Void"
numberType = TLabeled "Number"
stringType = TLabeled "String"

-- typeVar ctx = (TVariable (uid ctx), ctx')
--     where ctx' = ctx { uid = uid ctx + 1 }

typeVar :: TypeChecker Type
typeVar = do
    n <- gets uid
    put $ ctx { uid = n + 1}
    TVariable n

-- typeVars :: Context -> [a] -> ([Type], Context)
-- typeVars ctx ls = foldr typeVar' ([], ctx) ls
--     where typeVar' _ (types, ctx) = let (t, ctx') = typeVar ctx
--                                     in (types ++ [t], ctx')

typeVars :: [a] -> TypeChecker [Type]
typeVars ls = mapM (\_ -> typeVar) ls

-- analyseList :: Context -> [Either Statement Expression] -> ([Type], Context)
-- analyseList ctx nodes = foldr analyse' ([], ctx) nodes
--     where analyse' node (types, ctx) = let (t, ctx') = analyse ctx node
--                                        in  (types ++ [t], ctx')

analyseList :: [Either Statement Expression] -> [Type]
analyseList nodes = mapM analyse node

analyse :: Context -> Either Statement Expression -> (Type, Context)
analyse ctx node =
    case node of
        Left (SAssignment (Let ident expr)) ->
            let (t, _) = analyse ctx $ Right expr
            in  (t, ctx { env = Map.insert (EIdentifier ident) t (env ctx)})
        Left (SModule mod) ->
            let (_, ctx') = analyseList ctx $ map Left mod
            in  (voidType, ctx')
        Right e -> case e of
            EIdentifier ident -> getType ctx e
            EApplication fn args ->
                let (retT, ctx') = typeVar ctx
                    getTypeFold a (types, ctx) = let (t, ctx') = getType ctx a
                                                 in  (types ++ [t], ctx')
                    (argTypes, ctx'') = foldr getTypeFold ([], ctx') args
                    (retT', ctx''') = analyse ctx'' $ Right fn
                in  unify ctx''' (TFunction argTypes retT) retT'
            EBinOp op e1 e2 ->
                let (retT, ctx') = typeVar ctx
                    args = [e1, e2]
                    getTypeFold a (types, ctx) = let (t, ctx') = getType ctx a
                                                 in  (types ++ [t], ctx')
                    (argTypes, ctx'') = foldr getTypeFold ([], ctx') args
                in  unify ctx'' (TFunction argTypes retT) (opType op)
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
            _ -> getType ctx e

-- analyse :: Either Assignment Expression -> TypeChecker Type
-- analyse node = do
--     case node of
--         Left

opType :: Operation -> Type
opType op = TFunction [numberType, numberType] numberType

getType :: Context -> Expression -> (Type, Context)
getType ctx node =
    case Map.lookup node (env ctx) of
        Just t -> fresh ctx t
        Nothing -> case node of
            ENumber _ -> (numberType, ctx)
            EString _ -> (stringType, ctx)
            _ -> error $ "Undefined: " ++ show node

fresh ctx t = (t', ctx')
    where
        -- TODO The ctx'' vars in this function are sad. Make this nicer.
        freshrec ctx m t =
            let (p, ctx') = prune ctx t
            in case p of
                TVariable _ -> if isGeneric ctx' p
                    then case Map.lookup p m of
                        Just p' -> (p', ctx', m)
                        Nothing -> let (tvar, ctx'') = typeVar ctx'
                                   in  (tvar, ctx'', Map.insert p tvar m)
                    else (p, ctx', m)
                TLabeled name -> (TLabeled name, ctx', m)
                TFunction args ret ->
                    let (types, ctx'', m') = foldr freshfold ([], ctx', m) (ret:args)
                    in  (TFunction (tail types) (head types), ctx'', m')
        freshfold t (types, ctx, m) =
            let (t', ctx', m') = freshrec ctx m t
            in  (types ++ [t'], ctx', m')
        (t', ctx', _) = freshrec ctx Map.empty t

prune ctx t =
    case t of
        TVariable _ -> case Map.lookup t (typeEnv ctx) of
            Just inst ->
                let (inst', ctx') = prune ctx inst
                in  (inst', ctx { typeEnv = Map.insert t inst' (typeEnv ctx) })
            Nothing -> (t, ctx)
        _ -> (t, ctx)

isGeneric ctx t = (not yn, ctx')
    where (yn, ctx') = occursIn ctx (Set.toList $ nonGeneric ctx) t

occursIn ctx types t1 = foldr f (False, ctx) types
    where f t2 (yn, ctx) = let (yn', ctx') = occursInType ctx t1 t2
                           in  (or [yn, yn'], ctx')

occursInType ctx t1 t2 =
    if p2 == t1
        then (True, ctx')
        else case p2 of
            TFunction args ret -> occursIn ctx' (ret:args)
    where (p2, ctx') = prune ctx t2

-- TODO
unify :: Context -> Type -> Type -> (Type, Context)
unify ctx t1 t2 = (t1, ctx)
