module Main (main) where

import Data.Text.Lazy.IO
import Prelude hiding (putStrLn)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as T

import TJ

main = do
    args <- getArgs
    putStrLn $ case parseTJ (T.pack $ head args) of
        Left err -> T.pack $ show err
        Right parsed -> T.pack $ show parsed
