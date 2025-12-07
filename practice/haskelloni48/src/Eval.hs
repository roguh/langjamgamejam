{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( eval,
    run,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader
  ( MonadReader (ask, local),
    runReaderT,
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
  vals <- mapM eval $ odds pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  local (const env') $ eval expr
eval (List [Atom "letrec", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ evens pairs
  vals <- mapM eval $ odds pairs
  -- The variables are bound to fresh locations holding undefined values,
  let envNils = Map.fromList (map (\a -> (extractVar a, Nil)) atoms) <> env

  -- the inits are evaluated in the resulting environment (in some unspecified order),
  -- each variable is assigned to the result of the corresponding init,
  envFinal <-
    ( foldM
        ( \envLocal (a, b) -> do
            result <- eval b
            return $ Map.insert (extractVar a) result envLocal
        )
        envNils
        (zip atoms vals)
      )

  -- the body is evaluated in the resulting environment,
  -- and the value(s) of the last expression in body is(are) returned.
  local (const envFinal) $ eval expr

evens :: [t] -> [t]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [t] -> [t]
odds [] = []
odds (_ : xs) = evens xs

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

basicEnv :: Map.Map T.Text LispVal
basicEnv =
  Map.fromList []

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

run :: Eval b -> IO b
run action = runASTinEnv basicEnv action
