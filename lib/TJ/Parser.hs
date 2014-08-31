module TJ.Parser ( parseTJ
                 , parseTJFile
                 , Assignment(..)
                 , Expression(..)
                 , Identifier(..)
                 , Operation(..)
                 , Statement(..)
                 ) where

import Control.Monad.Identity
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as P

type LanguageDef st = P.GenLanguageDef T.Text st Identity
type TokenParser st = P.GenTokenParser T.Text st Identity

newtype Identifier = Identifier T.Text
                  deriving (Show, Ord, Eq)

type ParamList = [Identifier]
type ArgList = [Expression]

data Expression = EIdentifier Identifier
                | ENumber Double
                | EString T.Text
                | EBlock [Expression]
                | EFunction (Maybe Identifier) ParamList Expression
                | EBinOp Operation Expression Expression
                | EApplication Expression ArgList
                | EStatement Statement
                  deriving (Show, Ord, Eq)

data Operation = Add | Subtract | Divide | Multiply
                 deriving (Show, Ord, Eq)

data Assignment = Let Identifier Expression
                  deriving (Show, Ord, Eq)

type Module = [Statement]

data Statement = SAssignment Assignment
               | SModule Module
               deriving (Show, Ord, Eq)

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

module' :: Parser Statement
module' = do
    spaces
    ids <- assignment `sepEndBy` semi
    eof
    return $ SModule $ map SAssignment ids

defun :: Parser Assignment
defun = do
    reserved "function"
    name <- identifier
    params <- paramList
    expr <- expression
    return $ Let name $ EFunction (Just name) params expr

let' :: Parser Assignment
let' = do
    reserved "let"
    id <- identifier
    reservedOp "="
    expr <- expression
    return $ Let id expr

assignment = try let' <|> defun

function :: Parser Expression
function = do
    reserved "function"
    params <- paramList
    expr <- expression
    return $ EFunction Nothing params expr

paramList :: Parser ParamList
paramList = parens $ commaSep identifier

argList :: Parser ArgList
argList = parens $ commaSep expression

block :: Parser Expression
block = do
    exprs <- braces $ (expression <|> expr (EStatement . SAssignment) assignment) `sepEndBy` semi
    return $ EBlock exprs

application :: Parser Expression
application = do
    expr <- atom
    argsSets <- many1 argList
    return $ foldr (\args expr -> EApplication expr args) expr
                                                          (reverse argsSets)

stringLiteral = do
    s <- P.stringLiteral lexer
    return $ EString $ T.pack s

atom :: Parser Expression
atom = choice $ map try [ number
                        , stringLiteral
                        , eidentifier
                        , block
                        , parens expression
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
