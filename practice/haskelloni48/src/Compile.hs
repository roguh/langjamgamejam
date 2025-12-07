{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile (toJS) where

import Control.Monad.Reader
  ( MonadReader (ask, local),
    Reader,
    runReader,
  )
import qualified Data.Map as Map
import qualified Data.Text as T
import LispVal
import Util

-- For now, keep track of which variables are defined
data Ctx = Ctx
  { vars :: Map.Map T.Text (),
    depth :: Int
  }
  deriving (Show)

newtype CompileCtx a = CompileCtx {unCtx :: Reader Ctx a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Ctx
    )

initCtx = Ctx {vars = Map.fromList [("car", ()), ("cdr", ())], depth = 0}

toJS :: LispVal -> T.Text
toJS v =
  let body = runReader (unCtx $ toJS_ v) initCtx
   in joinT "\n" ["const __scope__ = {\"car\": (l) => l ? l[0] : [], \"cdr\": (l) => l.slice(1)};", body]

toJS_ :: LispVal -> CompileCtx T.Text
toJS_ (Atom name) = do
  env <- ask
  case Map.lookup name (vars env) of
    Just () -> return $ T.concat ["__scope__[\"", name, "\"]"]
    Nothing -> return name -- Assume it's a global
toJS_ (Bool False) = return "false"
toJS_ (Bool True) = return "true"
toJS_ Nil = return "null"
toJS_ (Number num) = return . T.pack $ show num
toJS_ (String str) = return $ T.concat ["\"", str, "\""]
toJS_ (List [Atom "quote", (List contents)]) = do
  each <- mapM toJS_ contents
  return $ T.concat ["[", joinT ", " each, "]"]
toJS_ (List [Atom "quote", contents]) = toJS_ contents
toJS_ (Vector contents) = do
  each <- mapM toJS_ contents
  return $ T.concat ["[", joinT ", " each, "]"]
toJS_ (List [Atom "if", predicate, tExpr, fExpr]) = do
  p <- deeper predicate
  t <- deeper tExpr
  f <- deeper fExpr
  return $ joinT "\n" ["(", p, ") ? (", t, ") : (", f, ")"]
toJS_ (List [Atom "let", List pairs, expr]) = do
  env <- ask
  let varNames = evens pairs
  let exprs = odds pairs
  bindings <-
    mapM
      ( \(a, b) ->
          ( deeper b
              >>= ( \r ->
                      return . lead (depth env) $
                        T.concat
                          [ "__scope__[\"",
                            ensureVar a,
                            "\"]", -- Variable name
                            " = (",
                            r, -- Variable value
                            "),"
                          ]
                  )
          )
      )
      (zip varNames exprs)
  -- Now evaluate the body and keep track of scheme-defined variables
  let vars' = Map.fromList (map (\a -> (ensureVar a, ())) varNames) <> (vars env)
  e <- local (const $ env {vars = vars'}) $ deeper expr
  return $
    joinT "\n" $
      ["((__scope__) => ("]
        ++ bindings
        ++ [ e,
             ")",
             ")({...__scope__})" -- clone the parent scope to form a new scope
           ]
toJS_ (List (func@(Atom _) : args)) = do
  funcT <- toJS_ func
  argsT <- mapM toJS_ args
  return . T.concat $
    [ funcT, -- Function name
      "(",
      joinT ", " argsT, -- Arguments
      ")"
    ]

-- toJS lvl e = error ("javascript does not support " ++ show e)

ensureVar (Atom v) = v
ensureVar n = error $ "type mismatch for variable " ++ show n

lead n = mappend $ T.replicate n " "

deeper :: LispVal -> CompileCtx T.Text
deeper v = do
  env <- ask
  result <- local (const $ env {depth = (depth env) + 1}) $ toJS_ v
  return $ lead (depth env) result

joinT :: T.Text -> [T.Text] -> T.Text
joinT = T.intercalate
