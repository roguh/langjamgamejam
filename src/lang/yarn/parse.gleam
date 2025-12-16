import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result

import atto.{type ParseError, do, drop, fail_msg, map, pure, token}
import atto/error
import atto/ops
import atto/text.{match}

import lang/yarn/ast

fn comment() {
  use <- atto.label("comment")
  use yes <- do(
    atto.try({
      use <- drop(token("/"))
      use <- drop(token("/"))
      pure(Nil)
    }),
  )
  case yes {
    option.Some(_) -> {
      use <- drop(match("[^\n]*"))
      pure(Nil)
    }
    _ -> pure(Nil)
  }
}

fn hs(p) {
  use <- atto.label("horizontal space")
  use x <- do(p)
  use <- drop(match("[ \t]*"))
  pure(x)
}

fn ws(p) {
  use <- atto.label("newline")
  use x <- do(p)
  use <- drop(match("[ \t\n]*"))
  use <- drop(comment())
  pure(x)
}

fn yarn_key() {
  use key <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> hs())
  use <- drop(token(":") |> hs())
  pure(key)
}

fn dialog_line() {
  use <- atto.label("yarn dialog line")
  use name <- do(atto.try(yarn_key()))
  use text <- do(match("[^=\n]+") |> ws())
  pure(ast.Line(text, name))
}

fn yarn_body() {
  use <- atto.label("yarn body")
  use lines <- do(ops.many(dialog_line()))
  pure(lines)
}

fn header() {
  use <- atto.label("yarn header")
  use key <- do(yarn_key())
  use value <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> ws())
  pure(#(key, value))
}

fn node() {
  use <- atto.label("yarn node")
  // TODO allow title to be in any order in the headers list
  use <- drop(pure(Nil) |> ws())
  use <- drop(match("title:") |> hs())
  // Node titles must start with a letter, and can contain letters, numbers and underscores.
  // Node names cannot contain a . (period).
  use title <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> ws())
  use headers <- do(ops.many(header()))
  use <- drop(match("---") |> ws())
  use body <- do(yarn_body())
  use <- drop(match("===") |> ws())
  pure(ast.YarnNode(title, headers, body))
}

fn nodes() {
  ops.some(node())
}

fn yarn() {
  use x <- do(nodes())
  use <- drop(atto.eof())
  pure(ast.Yarn(x))
}

pub fn parse(input: String) -> ast.YarnArtifact {
  let src = text.new(input)
  let parser_ = yarn()
  let result = atto.run(parser_, src, Nil)
  case result {
    Ok(e) -> Ok(e)
    Error(err) ->
      Error(ast.YarnError(
        error.pretty(err, src, False),
        err.span.start.line,
        err.span.start.col,
        err.span.end.line,
        err.span.end.col,
      ))
  }
}

pub fn empty_yarn() {
  ast.Yarn(nodes: [])
}
