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

initCtx :: Ctx
initCtx = Ctx {vars = Map.map (const ()) stdlib, depth = 0}

stdlib :: Map.Map T.Text T.Text
stdlib =
  Map.fromList
    [ ("car", "(l) => l ? l[0] : []"),
      ("cdr", "(l) => l.slice(1)"),
      -- TODO foldable?
      ("*", "(a, b) => a * b"),
      ("+", "(a, b) => a + b"),
      ("-", "(a, b) => b === undefined ? -a : a - b"),
      ("/", "(a, b) => a / b"),
      ("%", "(a, b) => a % b"),
      ("^", "(a, b) => a ** b"),
      ("&", "(a, b) => a & b"),
      ("|", "(a, b) => a | b"),
      ("<<", "(a, b) => a << b"),
      (">>", "(a, b) => a >> b"),
      ("~", "(a) => ~a"),
      ("==", "(a, b) => a === b"),
      ("!=", "(a, b) => a !== b"),
      ("<", "(a, b) => a < b"),
      (">", "(a, b) => a > b"),
      ("<=", "(a, b) => a <= b"),
      (">=", "(a, b) => a >= b"),
      ("&&", "(a, b) => a && b"),
      ("||", "(a, b) => a || b"),
      ("!", "(a) => !a")
    ]

toJS :: LispVal -> T.Text
toJS v =
  let body = runReader (unCtx $ convert v) initCtx
   in joinT "" $
        ["const __scope__ = {"]
          ++ (map (\(var, val) -> T.concat ["\"", var, "\": ", val, ","]) (Map.toList stdlib))
          ++ ["};\n\n", body]

convert :: LispVal -> CompileCtx T.Text
convert (Atom name) = do
  env <- ask
  case Map.lookup name (vars env) of
    Just () -> return $ T.concat ["__scope__[\"", name, "\"]"]
    Nothing -> return name -- Assume it's a global
convert (Bool False) = return "false"
convert (Bool True) = return "true"
convert Nil = return "null"
convert (Number num isBigInt) = return $ T.concat [T.pack $ show num, if isBigInt || num > maxF64Int then "n" else ""]
convert (Double num) = return $ T.pack $ show num
convert (String str) = return $ T.concat ["\"", str, "\""]
convert (List [Atom "quote", (List contents)]) = do
  each <- mapM convert contents
  return $ T.concat ["[", joinT ", " each, "]"]
convert (List [Atom "quote", contents]) = convert contents
convert (Vector contents) = do
  each <- mapM convert contents
  return $ T.concat ["[", joinT ", " each, "]"]
convert (List [Atom "if", predicate, tExpr, fExpr]) = do
  p <- deeper predicate
  t <- deeper tExpr
  f <- deeper fExpr
  return $ joinT "\n" ["(", p, ") ? (", t, ") : (", f, ")"]
convert (List [Atom "let", List pairs, expr]) = do
  env <- ask
  let varNames = evens pairs
  let exprs = odds pairs
  bindings <-
    mapM
      ( \(a, b) ->
          ( deeper b
              >>= ( \r ->
                      return . lead (1 + (depth env)) $
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
  exprT <- local (const $ env {vars = vars'}) $ deeper expr
  return $
    joinT "\n" $
      [lead (depth env) "((__scope__) => ("]
        ++ bindings
        ++ [ lead (depth env) exprT,
             lead (depth env) "))({...__scope__})" -- clone the parent scope to form a new scope
           ]
convert (List [Atom "lambda", (List args), body]) = do
  let argsT = map ensureVar args
  bodyT <- deeper body
  return $ T.concat ["(", joinT ", " argsT, ") => (", bodyT, ")"]
convert (List (func@(Atom _) : args)) = do
  funcT <- convert func
  argsT <- mapM convert args
  return . T.concat $
    [ funcT, -- Function name
      "(",
      joinT ", " argsT, -- Arguments
      ")"
    ]
convert e = error ("javascript does not support " ++ show e)

ensureVar :: LispVal -> T.Text
ensureVar (Atom v) = v
ensureVar n = error $ "type mismatch for variable " ++ show n

lead :: Int -> T.Text -> T.Text
lead n = mappend $ T.replicate (2 * n) " "

deeper :: LispVal -> CompileCtx T.Text
deeper v = do
  env <- ask
  result <- local (const $ env {depth = (depth env) + 1}) $ convert v
  return $ lead (depth env) result

joinT :: T.Text -> [T.Text] -> T.Text
joinT = T.intercalate

maxF64Int :: Integer
maxF64Int = 562949953421311
