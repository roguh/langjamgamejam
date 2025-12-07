module Main (main) where

import qualified Data.Text as T
import Eval
import LispVal
import Parser
import System.Environment

main :: IO ()
main = do
  -- TODO better cli UX
  (expr : _) <- getArgs
  -- TODO use fancy monad magic?
  case Parser.readExpr (T.pack expr) of
    Left err -> putStrLn (show err)
    Right val -> do
      putStrLn $ show val
      result <- run $ eval val
      putStrLn $ show result
