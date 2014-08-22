module TJ (parseTJ) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Data.Text.Lazy as T
import Text.Parsec.Text.Lazy

data Identifier = Identifier T.Text
                  deriving (Show)
data Expression = EIdentifier Identifier
                | ENumber Double
                  deriving (Show)
data Assignment = Assignment Identifier Expression
                  deriving (Show)

module' :: Parser [Assignment]
module' = do
    ids <- assignment `sepEndBy` many1 newline
    eof
    return ids

assignment :: Parser Assignment
assignment = do
    skipMany space
    string "let"
    id <- identifier
    skipMany space
    char '='
    expr <- expression
    return $ Assignment id expr

-- function = do
--     skipMany space
--     string "function"
--     skipMany space
--     params <- paramList
--     skipMany space
--     bl <- block

expression :: Parser Expression
expression = try enumber <|> eidentifier

identifier :: Parser Identifier
identifier = do
    skipMany space
    c <- letter <|> char '_' <|> char '$'
    cs <- many (alphaNum <|> char '_' <|> char '$')
    return $ Identifier (T.cons c (T.pack cs))

eidentifier = do
    id <- identifier
    return $ EIdentifier id

decimal :: Parser Double
decimal = do
    skipMany space
    whole <- many1 digit
    has_decimal <- optionMaybe $ char '.'
    case has_decimal of
        Just _ -> do
            fractional <- many1 digit
            return $ read $ whole ++ ('.':fractional)
        Nothing -> return $ read whole

enumber :: Parser Expression
enumber = do
    n <- decimal
    return $ ENumber n

parseTJ input = parse module' "(unknown)" input
