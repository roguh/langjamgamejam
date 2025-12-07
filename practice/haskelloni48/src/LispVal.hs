{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal
  ( LispVal (..),
    Eval (..),
    IFunc (..),
    EnvCtx,
    -- LispException(..),
    showVal,
  )
where

-- import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as T

type EnvCtx = Map.Map T.Text LispVal

-- Reader monad to provide lexical scope, IO monad for easy IO
newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

data LispVal
  = Atom T.Text
  | Bool Bool
  | Nil
  | Number Integer
  | String T.Text
  | Fun T.Text IFunc -- Function name and action
  | Lambda IFunc EnvCtx
  | List [LispVal]
  | Vector [LispVal]
  deriving (Eq)

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Eq IFunc where
  (==) _ _ = False

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
  (Atom atom) -> atom
  (Bool False) -> "#f"
  (Bool True) -> "#t"
  Nil -> "Nil"
  (Number num) -> T.pack $ show num
  (String str) -> T.concat ["\"", str, "\""]
  (Fun name _) -> T.concat ["(function <", name, ">)"]
  (Lambda _ _) -> "(lambda function)"
  (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
  (Vector contents) -> T.concat ["#(", T.unwords $ showVal <$> contents, ")"]
