module Main (main) where

import qualified Data.Text as T
-- r.i.p. import Eval
import Parser
import Compile
import System.Environment
import System.IO

-- Print to stderr fp 2
debug :: String -> IO ()
debug s = hPutStr stderr (s ++ "\n")

main :: IO ()
main = do
  -- TODO better cli UX
  (expr : _) <- getArgs
  -- TODO use fancy monad magic?
  case Parser.readExpr (T.pack expr) of
    Left err -> putStrLn (show err)
    Right val -> do
      debug "Parsing"
      debug expr
      debug "Compiling Scheme to JavaScript (?!?!?)"
      debug $ show val
      -- result <- run $ eval val
      -- putStrLn $ show result
      putStrLn . T.unpack $ toJS val
