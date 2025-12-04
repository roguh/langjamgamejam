module Main (main) where

import System.Environment
import qualified Data.Text as T

import Parser
import LispVal


main :: IO ()
main = do
    -- TODO better cli UX
    (expr:_) <- getArgs
    -- TODO use fancy monad magic?
    case Parser.readExpr (T.pack expr) of
        Left err -> putStrLn (show err)
        Right val -> putStrLn (T.unpack (showVal val))
