module TJ.CodeGen ( jsDocument
                  ) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.PrettyPrint.Leijen
import qualified Data.Text.Lazy as T

import TJ.Ast

data Context = Context { vars :: Set.Set Identifier
                       , liftVars :: Set.Set Identifier
                       , renames :: Map.Map Identifier Identifier
                       , dumpVars :: Bool
                       }

type CodeGen = State Context

docfold op docs = foldr op empty docs

semiDocfold op docs = docfold op $ map (<> semi) docs

mapLast fn [] = []
mapLast fn (expr:[]) = [fn expr]
mapLast fn (expr:xs) = expr:(mapLast fn xs)

returnLast xs = mapLast ret xs
    where ret expr = case expr of
                     EStatement (SReturn _) -> expr
                     _ -> EStatement $ SReturn expr

functionBlock (EStatement (SBlock exprs)) = SBlock $ returnLast exprs
functionBlock expr = functionBlock $ EStatement $ SBlock [expr]

assignment :: Bool -> Assignment -> CodeGen Doc
assignment decl (Let ident expr) = do
    expr' <- jsifyExpression expr
    ctx <- get
    let decl' = decl && not (Set.member ident $ vars ctx)
    ident' <- registerIdent decl ident
    return $ hsep [ text $ (if decl' then "var " else "") ++ show ident
                  , text "="
                  , expr' ]

scoped fn = do
    ctx <- get
    x <- fn
    put ctx
    return x

varDump = do
    ctx <- get
    vs <- mapM resolveIdent (Set.toList $ liftVars ctx)
    return $ if length vs > 0
        then Just $ text "var" <+> align (cat $ punctuate comma $ map (text . show) vs)
        else Nothing

resolveIdent ident = do
    ctx <- get
    return $ case Map.lookup ident (renames ctx) of
        Just ident' -> ident'
        _ -> ident

registerIdent shadow ident = do
    ctx <- get
    ident' <- if not shadow && Set.member ident (vars ctx)
        then renameIdent ident 0
        else return ident
    put $ ctx { vars = Set.insert ident' (vars ctx) }
    return ident'

renameIdent :: Identifier -> Int -> CodeGen Identifier
renameIdent ident n = do
    ctx <- get
    let ident' = Identifier $ (identName ident) `T.append` (T.pack $ show n)
    if Set.member ident' (vars ctx)
        then do
            put $ ctx { renames = Map.insert ident ident' (renames ctx)
                       , vars = Set.insert ident' (vars ctx)
                       }
            return ident'
        else renameIdent ident (n + 1)

processEnumMembers (n, xs) (EnumConstant ident maybeExpr) =
    case maybeExpr of
        Just $ ENumber n' -> (n' + 1.0, (EnumConstant ident maybeExpr):xs)
        Nothing -> (n + 1.0, (EnumConstant ident $ Just $ ENumber n):xs)
        _ -> (n, (EnumConstant ident maybeExpr):xs)
processEnumMembers (n, xs) member = (n, member:xs)

jsify :: Either Statement Expression -> CodeGen Doc
jsify (Left ast) =
    case ast of
        SAssignment assgn -> assignment True assgn
        SModule mod -> scoped $ do
            mod' <- mapM jsifyStatement mod
            let callMain = text "typeof main === 'function' && main();"
            return $ (semiDocfold (<$$>) mod') <$$> callMain
        SReturn expr -> do
            expr' <- jsifyExpression expr
            return $ text "return" <+> expr'
        SBlock exprs -> do
            ctx <- get
            docs <- mapM jsifyExpression exprs

            -- This lifts some var declarations to the top of a block. I think
            -- there should be a better way of doing this.
            maybeVars <- varDump
            let docs' = if dumpVars ctx
                then case maybeVars of
                    Just vars' -> vars':docs
                    _ -> docs
                else docs
            put $ ctx { dumpVars = False }

            let body = indent 4 $ semiDocfold (<$$>) $ docs'
            return $ braces $ line <> body
        SJavascript js -> return $ text $ T.unpack js
        SEnum ident members ->
            let members' = foldr processEnumMembers (0.0, []) members
            in  text "FAKE NO"

jsify (Right ast) =
    case ast of
        EIdentifier ident -> return $ text $ show ident
        ENumber n -> return $ text $ show n
        EString s -> return $ text $ show s
        EFunction maybeIdent params expr -> scoped $ do
            ctx <- get
            put $ ctx { dumpVars = True
                      , vars = foldr Set.insert (vars ctx) params
                      }
            body <- jsifyStatement $ functionBlock expr
            let lsIdent = case maybeIdent of
                            Just ident -> [text $ show ident]
                            _ -> []
            return $ hsep $ [text "function"] ++ lsIdent ++
                            [ tupled $ map (text . show) params
                            , body ]
        EBinOp op a b -> do
            a' <- jsifyExpression a
            b' <- jsifyExpression b
            let opDoc = case op of
                            Concat -> text "+"
                            _ -> text $ show op
            let docs = [a', opDoc, b']
            return $ parens $ hsep docs
        EApplication expr args -> do
            expr' <- jsifyExpression expr
            args' <- mapM jsifyExpression args
            return $ expr' <> (tupled args')
        EIf cond yes no -> do
            cond':yes':no':[] <- mapM jsifyExpression [cond, yes, no]
            return $ hsep $ [text "if", parens cond', yes', text "else", no']
        EStatement (SBlock exprs) -> do
            exprs' <- mapM jsifyExpression exprs
            return $ tupled exprs'
        EStatement (SAssignment assgn) -> do
            result <- assignment False assgn
            case assgn of
                Let ident _ -> do
                    ctx <- get
                    ident' <- resolveIdent ident
                    put $ ctx { liftVars = Set.insert ident' (liftVars ctx) }
            return result
        EStatement st -> jsifyStatement st

jsifyStatement = jsify . Left
jsifyExpression = jsify . Right

startState = Context { vars = Set.empty
                     , liftVars = Set.empty
                     , renames = Map.empty
                     , dumpVars = False
                     }

jsDocument ast = evalState (jsifyStatement ast) startState
