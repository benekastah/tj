module TJ.Parser ( parseTJ
                 , parseTJFile
                 ) where

import Control.Monad.Identity
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as P
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import TJ.Ast

type LanguageDef st = P.GenLanguageDef T.Text st Identity
type TokenParser st = P.GenTokenParser T.Text st Identity

-- Utility parsers
expr :: (a -> Expression) -> Parser a -> Parser Expression
expr wrapper p = do
    x <- p
    return $ wrapper x

-- TJ parsers
tjLangDef :: LanguageDef st
tjLangDef = P.LanguageDef {
    P.commentStart   = "/*",
    P.commentEnd     = "*/",
    P.commentLine    = "//",
    P.nestedComments = True,
    P.identStart     = letter,
    P.identLetter    = alphaNum <|> oneOf "_'",
    P.opStart        = P.opLetter tjLangDef,
    P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedNames  = ["let", "function", "javascript", "if", "else", "elif",
                        "enum"],
    P.reservedOpNames= [],
    P.caseSensitive  = True
    }

lexer :: TokenParser st
lexer = P.makeTokenParser tjLangDef

braces = P.braces lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
parens = P.parens lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer


-- Parsing of type definitions
typeIdentifier :: Parser Identifier
typeIdentifier = do
    ch <- upper
    ident <- optionMaybe $ try $ P.identifier lexer
    let name = case ident of
                   Just s -> ch:s
                   Nothing -> [ch]
    return $ Identifier $ T.pack name

typeAtom :: Parser Type
typeAtom = do
    ident <- typeIdentifier
    types <- many typeRef
    return $ TLabeled (identName ident) types

typeRef :: Parser Type
typeRef = typeAtom <|> parens typeAtom

typeAnnotation :: Parser p -> Parser Type
typeAnnotation op = do { op; typeRef }

exprTypeAnnotation :: Parser Type
exprTypeAnnotation = typeAnnotation $ reservedOp ":"

funcTypeAnnotation :: Parser Type
funcTypeAnnotation = typeAnnotation $ reservedOp "->"

enum :: Parser Statement
enum = do
    reserved "enum"
    ident <- typeIdentifier
    members <- braces $ enumMember `sepEndBy` semi
    return $ SEnum ident members

enumMember = do
    ident <- typeIdentifier
    assgn <- optionMaybe $ try $ reservedOp "="
    case assgn of
        Just _ -> do
            expr <- expression
            return $ EnumConstant ident $ Just expr
        Nothing -> do
            types <- optionMaybe $ try $ many1 typeRef
            case types of
                Just types' -> return $ EnumMember ident types'
                Nothing -> return $ EnumConstant ident Nothing

type' = enum

-- Parsing of statements
statement :: Parser Statement
statement = sassignment <|> type'

module' :: Parser Statement
module' = do
    whiteSpace
    statements <- statement `sepEndBy` semi
    eof
    return $ SModule statements

defun :: Parser Assignment
defun = do
    reserved "function"
    name <- identifier
    fn <- functionEnd
    return $ Let name fn

let' :: Parser Assignment
let' = do
    reserved "let"
    id <- identifier
    mT <- optionMaybe $ try exprTypeAnnotation
    reservedOp "="
    expr <- expression
    return $ Let id $ case mT of
        Just t -> ETyped expr t
        Nothing -> expr

assignment = try let' <|> defun

sassignment = do
    a <- assignment
    return $ SAssignment a


-- Parsing of expressions
functionEnd :: Parser Expression
functionEnd = do
    params <- paramList
    mT <- optionMaybe $ try funcTypeAnnotation
    expr <- expression
    return $ EFunction Nothing params $ case mT of
        Just t -> ETyped expr t
        Nothing -> expr

function :: Parser Expression
function = do
    reserved "function"
    functionEnd

typedIdentifier :: Parser Identifier
typedIdentifier = do
    ident <- identifier
    mT <- optionMaybe $ try exprTypeAnnotation
    return $ case mT of
        Just t -> IdentifierTyped ident t
        Nothing -> ident

paramList :: Parser ParamList
paramList = parens $ commaSep typedIdentifier

argList :: Parser ArgList
argList = parens $ commaSep expression

block :: Parser Expression
block = do
    let exprs = expression <|> expr (EStatement . SAssignment) assignment
    exprs <- braces $ exprs `sepEndBy` semi
    return $ EStatement $ SBlock exprs

jsInnerBlock t n = do
    ch <- if n == 0
        then (lookAhead $ try $ char '}') <|> anyChar
        else anyChar
    let n' = if ch == '{'
        then n + 1
        else n
    if ch == '}' && n == 0
        then return t
        else jsInnerBlock (T.snoc t ch) (if ch == '{' then n + 1 else n)

jsblock :: Parser Expression
jsblock = do
    reserved "javascript"
    js <- braces $ jsInnerBlock T.empty 0
    return $ EStatement $ SJavascript js

application :: Parser Expression
application = do
    expr <- atom
    argsSets <- many1 argList
    return $ foldr (\args expr -> EApplication expr args) expr
                                                          (reverse argsSets)

stringLiteral = do
    s <- P.stringLiteral lexer
    return $ EString $ T.pack s

ifRec :: Parser () -> Parser Expression
ifRec if' = do
    if'
    cond <- expression
    yes <- expression
    no <- (try $ do { reserved "else"; expression }) <|>
          (try $ ifRec $ reserved "elif")
    return $ EIf cond yes no

ifexpr = ifRec $ reserved "if"

maybeTyped :: Parser Expression -> Parser Expression
maybeTyped expr = do
    result <- expr
    mT <- optionMaybe $ try exprTypeAnnotation
    return $ case mT of
        Just t -> ETyped result t
        Nothing -> result

atom :: Parser Expression
atom = choice $ map (try . maybeTyped) [ number
                                       , stringLiteral
                                       , eidentifier
                                       , block
                                       , jsblock
                                       , parens expression
                                       , ifexpr
                                       ]

term :: Parser Expression
term = choice $ map try [ application
                        , function
                        , atom
                        ]

expr_table = [ [ binary "*" (EBinOp Multiply) AssocLeft
               , binary "/" (EBinOp Divide) AssocLeft ]
             , [ binary "+" (EBinOp Add) AssocLeft
               , binary "-" (EBinOp Subtract) AssocLeft ]
             , [ binary "++" (EBinOp Concat) AssocLeft ]
             ]

expression :: Parser Expression
expression = buildExpressionParser expr_table term <?> "expression"

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

identifier :: Parser Identifier
identifier = do
    skipMany space
    name <- P.identifier lexer
    return $ Identifier (T.pack name)

eidentifier = expr EIdentifier identifier

number :: Parser Expression
number = do
    n <- P.naturalOrFloat lexer
    return $ ENumber $ case n of
        Left i -> fromIntegral i
        Right n -> n

parseTJ input = parse module' "(unknown)" input

parseTJFile file = do
    input <- TextIO.readFile file
    return $ parseTJ input
