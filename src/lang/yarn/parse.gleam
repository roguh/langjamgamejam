import gleam/option

import atto.{do, drop, pure, token}
import atto/error
import atto/ops
import atto/text.{match}
import atto/text_util

import lang/yarn/ast

fn comment() {
  use <- atto.label("comment")
  atto.try({
    use <- drop(match("//[^\n]*"))
    pure(Nil)
  })
}

fn hs(p) {
  use <- atto.label("horizontal space")
  use x <- do(p)
  use <- drop(match("[ \t]*"))
  pure(x)
}

fn ws(p) {
  use <- atto.label("spaces")
  use x <- do(p |> text_util.ws())
  // TODO ops.many() doesn't work well with empty parsers
  use <- drop(comment() |> text_util.ws())
  pure(x)
}

fn yarn_key() {
  use key <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> hs())
  use <- drop(token(":") |> hs())
  pure(key)
}

fn dialog_line() {
  use <- atto.label("dialog line")
  use name <- do(atto.try(yarn_key()))
  use text <- do(match("[^<{}=\n]+") |> ws())
  let tags = []
  pure(ast.Line([ast.Text(text)], name, tags))
}

fn choices() {
  use <- atto.label("choices")
  use <- drop(match("->") |> hs())
  use text <- do(match("[^=<{}\n]+") |> ws())
  use choices <- do(
    atto.try({
      use <- drop(token("{") |> ws())
      use x <- do(yarn_body())
      use <- drop(token("}") |> ws())
      pure(x)
    }),
  )
  let tags = []
  let cond = option.None
  pure(ast.Choice([ast.Text(text)], option.unwrap(choices, []), tags, cond))
}

fn jump() {
  use <- atto.label("jump label")
  use <- drop(match("jump") |> ws())
  use label <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> ws())
  pure(ast.Jump(label))
}

fn command() {
  use <- atto.label("command")
  use <- drop(match("<<") |> ws())
  use cmd <- do(ops.choice([jump()]))
  use <- drop(match(">>") |> ws())
  pure(ast.Command(cmd))
}

fn yarn_body() {
  use <- atto.label("body")
  use lines <- do(ops.many(ops.choice([command(), choices(), dialog_line()])))
  pure(lines)
}

fn header() {
  use <- atto.label("header")
  use key <- do(yarn_key())
  use value <- do(match("[^\n/]*") |> ws())
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
