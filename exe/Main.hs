module Main (main) where

import Data.Text.Lazy.IO
import Prelude hiding (putStrLn)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as T

import TJ.Parser
import TJ.TypeChecker

main = do
    args <- getArgs
    parseResult <- parseTJFile (head args)
    case parseResult of
        Left err -> putStrLn $ T.pack $ show err
        Right parsed -> do
            putStrLn $ T.pack $ show parsed
            putStrLn $ T.pack $ show $ checkStatement parsed
