{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( eval,
  )
where

import Control.Monad.Reader
  ( MonadReader (ask, local),
  )
import qualified Data.Map as Map
import qualified Data.Text as T
import LispVal

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b) = return $ Bool b
eval (List []) = return Nil
eval Nil = return Nil
-- write uses showVal to convert a lispval into lisp code
eval (List [Atom "write", rest]) = return . String $ showVal rest
-- Take two or more args passed to write and convert to List
eval (List ((Atom "write") : rest)) = return . String . showVal $ List rest
-- Lookup a variable using a Map lookup
eval n@(Atom _) = getVar n
-- if-statement special form: evaluate predicate before evaluating either branch
eval (List [Atom "if", predicate, tExpr, fExpr]) = do
  predVal <- eval predicate
  case predVal of
    (Bool True) -> eval tExpr
    (Bool False) -> eval fExpr
    _ -> error "if-statement predicate must be boolean"
eval (List [Atom "let", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ evens pairs
  vals <- mapM eval . evens $ tail pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  local (const env') $ eval expr

-- TODO letrec and let* yayyy

evens :: [t] -> [t]
evens [] = []
evens (x : _ : xs) = x : evens xs
evens (_ : []) = []

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = error ("wrong type for an atom " ++ (show n))

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x -> return x
    Nothing -> error ("undefined variable " ++ (T.unpack atom))
getVar n = error ("wrong type for a variable " ++ (show n))
