module TJ.TypeChecker ( check
                      , checkStatement
                      , checkExpression
                      ) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Text.Lazy as T

import TJ.Parser

data Type = TVariable Int
          | TLabeled T.Text [Type]
          deriving (Show, Eq, Ord)

data Context = Context { env :: Map.Map Expression Type
                       , typeEnv :: Map.Map Type Type
                       , nonGeneric :: Set.Set Type
                       , freshMap :: Map.Map Type Type
                       , uid :: Int
                       }
               deriving (Show)

type TypeChecker = State Context

voidType = TLabeled "Void" []
numberType = TLabeled "Number" []
stringType = TLabeled "String" []
functionType args ret = TLabeled "->" (ret:args)

typeVar :: TypeChecker Type
typeVar = do
    ctx <- get
    put $ ctx { uid = (uid ctx) + 1}
    return $ TVariable $ uid ctx

typeVars :: [a] -> TypeChecker [Type]
typeVars ls = mapM (\_ -> typeVar) ls

scoped :: TypeChecker a -> TypeChecker a
scoped fn = do
    env' <- gets env
    nonGeneric' <- gets nonGeneric
    x <- fn
    ctx' <- get
    put $ ctx' { env = env', nonGeneric = nonGeneric' }
    return x

errOrReturn errs ret =
    case errs of
        [] -> return ret
        _ -> error $ unlines errs

analyse :: Either Statement Expression -> TypeChecker Type
analyse node =
    case node of
        Left (SAssignment (Let ident expr)) -> do
            t <- analyse $ Right expr
            ctx <- get
            put ctx { env = Map.insert (EIdentifier ident) t (env ctx)}
            return t
        Left (SModule mod) -> do
            t <- mapM analyse $ map Left mod
            return $ TLabeled "Module" t
        Left (SBlock exprs) -> scoped (do
            types <- mapM analyse $ map Right exprs
            return $ last (voidType:types)
            )
        Left (SJavascript js) -> do
            tvar <- typeVar
            return tvar
        Right e -> case e of
            EIdentifier ident -> getType e
            EApplication fn args -> do
                fnT <- analyse $ Right fn
                argTypes <- mapM (analyse . Right) args
                retT <- typeVar
                errs <- unify (functionType argTypes retT) fnT
                errOrReturn errs retT
            EBinOp op e1 e2 -> do
                let args = [e1, e2]
                argTypes <- mapM getType args
                retT <- typeVar
                errs <- unify (functionType argTypes retT) (opType op)
                errOrReturn errs retT
            EFunction _ args expr -> scoped (do
                argTypes <- typeVars args
                ctx <- get
                put $ ctx { env = foldr (\(a, t) env' -> Map.insert a t env')
                                        (env ctx)
                                        (zip (map EIdentifier args) argTypes)
                          , nonGeneric = Set.union (nonGeneric ctx)
                                                   (Set.fromList argTypes)
                          }
                retT <- analyse $ Right expr
                return $ functionType argTypes retT
                )
            EStatement s -> analyse (Left s)
            _ -> getType e

opType :: Operation -> Type
opType op = functionType [numberType, numberType] numberType

getType :: Expression -> TypeChecker Type
getType node = do
    env' <- gets env
    case Map.lookup node env' of
        Just t -> fresh t
        Nothing -> case node of
            ENumber _ -> return numberType
            EString _ -> return stringType
            _ -> error $ "Undefined: " ++ show node

fresh :: Type -> TypeChecker Type
fresh t = do
    ctx <- get
    put $ ctx { freshMap = Map.empty }
    f <- freshrec t
    put ctx { freshMap = freshMap ctx }
    return f

freshrec :: Type -> TypeChecker Type
freshrec t = do
    p <- prune t
    m <- gets freshMap
    case p of
        TVariable _ -> do
            gen <- isGeneric p
            if gen
                then case Map.lookup p m of
                    Just p' -> return p'
                    Nothing -> do
                        tvar <- typeVar
                        ctx <- get
                        put $ ctx { freshMap = Map.insert p tvar m }
                        return tvar
                else return p
        TLabeled name types -> do
            types' <- mapM freshrec types
            return $ TLabeled name types'

prune :: Type -> TypeChecker Type
prune t = do
    tenv <- gets typeEnv
    case t of
        TVariable _ -> case Map.lookup t tenv of
            Just inst -> do
                inst' <- prune inst
                ctx <- get
                put $ ctx { typeEnv = Map.insert t inst' tenv }
                return inst'
            Nothing -> return t
        _ -> return t

isGeneric :: Type -> TypeChecker Bool
isGeneric t = do
    ngen <- gets nonGeneric
    occurs <- occursIn (Set.toList $ ngen) t
    return $ not occurs

occursIn :: [Type] -> Type -> TypeChecker Bool
occursIn types t1 = do
    occurs <- mapM (occursInType t1) types
    return $ or occurs

occursInType :: Type -> Type -> TypeChecker Bool
occursInType t1 t2 = do
    p2 <- prune t2
    if p2 == t1
        then return True
        else case p2 of
            TLabeled name types -> occursIn types t1
            _ -> return False

unify :: Type -> Type -> TypeChecker [String]
unify t1 t2 = do
    p1 <- prune t1
    p2 <- prune t2
    case p1 of
        TVariable n -> do
            if p1 /= p2
                then do
                    occurs <- occursInType p1 p2
                    if occurs
                        then return $! ["recursive unification"]
                        else do
                            ctx <- get
                            put $ ctx { typeEnv = Map.insert p1 p2 $ typeEnv ctx }
                            return $! []
                else return $! []
        TLabeled name1 types1 -> case p2 of
            TLabeled name2 types2 ->
                if name1 /= name2 || (length types1) /= (length types2)
                    then return $! ["Type mismatch: " ++ (show p1) ++ " != " ++
                                 (show p2)]
                    else do
                        r <- mapM (\(a, b) -> unify a b) (zip types1 types2)
                        return $! concat r
            _ -> unify p2 p1

emptyContext = Context { env = Map.empty
                       , typeEnv = Map.empty
                       , nonGeneric = Set.empty
                       , freshMap = Map.empty
                       , uid = 0
                       }

check :: Either Statement Expression -> Type
check ast = evalState (analyse ast) emptyContext

checkStatement = check . Left

checkExpression = check . Right
