module Main (main) where

-- import Data.Text.Lazy.IO
import qualified Data.Text.Lazy as T
import System.Environment (getArgs)
import System.IO
import Text.PrettyPrint.Leijen (displayIO, renderPretty)

import TJ.CodeGen
import TJ.Parser
import TJ.TypeChecker

data Args = Args String (Maybe String)

parseArgs [] = error "Too few arguments!"
parseArgs (inf:[]) = Args inf Nothing
parseArgs (inf:outf:[]) = Args inf (Just outf)
parseArgs _ = error "Too many arguments!"

getParsedArgs = do
    args <- getArgs
    return $ parseArgs args

main = do
    Args src out <- getParsedArgs
    parseResult <- parseTJFile src
    case parseResult of
        Left err -> putStrLn $ show err
        Right parsed -> do
            putStrLn $ show $ checkStatement parsed
            let doc = renderPretty 0.4 80 $ jsDocument parsed
            let docOut h = displayIO h doc
            case out of
                Just outf -> do
                    withFile outf WriteMode docOut
                    putStrLn $ "Compiled " ++ src ++ " to " ++ outf
                Nothing -> docOut stdout
