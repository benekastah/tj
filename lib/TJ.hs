module TJ (parseTJ) where

import Control.Monad.Identity
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as P

type LanguageDef st = P.GenLanguageDef T.Text st Identity
type TokenParser st = P.GenTokenParser T.Text st Identity

newtype Identifier = Identifier T.Text
                  deriving (Show)

type ParamList = [Identifier]
type ArgList = [Expression]

data Function = Function Identifier ParamList Expression
              | Lambda ParamList Expression
                deriving (Show)

data Expression = EIdentifier Identifier
                | ENumber Double
                | EBlock [Expression]
                | EFunction Function
                | EBinOp Operation Expression Expression
                | EApplication Expression ArgList
                | EAssignment Assignment
                  deriving (Show)

data Operation = Add | Subtract | Divide | Multiply
                 deriving (Show)

data Assignment = Let Identifier Expression
                | Defun Identifier Function
                  deriving (Show)

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
    P.reservedNames  = ["let", "function"],
    P.reservedOpNames= [],
    P.caseSensitive  = False
    }

lexer :: TokenParser st
lexer = P.makeTokenParser tjLangDef

parens = P.parens lexer
braces = P.braces lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer

module' :: Parser [Assignment]
module' = do
    spaces
    ids <- assignment `sepEndBy` semi
    eof
    return ids

defun :: Parser Assignment
defun = do
    reserved "function"
    name <- identifier
    params <- paramList
    expr <- expression
    return $ Defun name $ Function name params expr

let' :: Parser Assignment
let' = do
    reserved "let"
    id <- identifier
    reservedOp "="
    expr <- expression
    return $ Let id expr

assignment = try let' <|> defun

function :: Parser Function
function = do
    reserved "function"
    params <- paramList
    expr <- expression
    return $ Lambda params expr

efunction = expr EFunction function

paramList :: Parser ParamList
paramList = parens $ commaSep identifier

argList :: Parser ArgList
argList = parens $ commaSep expression

block :: Parser Expression
block = do
    exprs <- braces $ (expression <|> expr EAssignment assignment) `sepEndBy` semi
    return $ EBlock exprs

application :: Parser Expression
application = do
    expr <- atom
    argsSets <- many1 argList
    return $ foldr (\args expr -> EApplication expr args) expr argsSets

atom :: Parser Expression
atom = choice $ map try [ number
                        , eidentifier
                        , block
                        , parens expression
                        ]

term :: Parser Expression
term = choice $ map try [ application
                        , efunction
                        , atom
                        ]

expr_table = [ [ binary "*" (EBinOp Multiply) AssocLeft
               , binary "/" (EBinOp Divide) AssocLeft ]
             , [ binary "+" (EBinOp Add) AssocLeft
               , binary "-" (EBinOp Subtract) AssocLeft ]
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
