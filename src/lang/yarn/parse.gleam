import gleam/float
import gleam/int
import gleam/list
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

fn choice() {
  use <- atto.label("choices")
  use <- drop(match("->") |> hs())
  use text <- do(match("[^=<{}\n]+") |> ws())
  use body <- do(
    atto.try({
      use <- drop(token("{") |> ws())
      use x <- do(yarn_body())
      use <- drop(token("}") |> ws())
      pure(x)
    }),
  )
  let tags = []
  let cond = option.None
  pure(ast.ChoiceItem([ast.Text(text)], option.unwrap(body, []), tags, cond))
}

fn choices() {
  ops.some(choice()) |> atto.map(ast.Choice)
}

fn jump() {
  use <- atto.label("jump statement")
  use <- drop(match("jump") |> ws())
  use label <- do(match("[a-zA-Z][a-zA-Z0-9_]*") |> ws())
  pure(ast.Jump(label))
}

fn var() {
  match("\\$[a-zA-Z][a-zA-Z0-9_]*") |> ws()
}

fn val() {
  ops.choice([
    {
      use <- drop(match("true"))
      pure(ast.YBool(True))
    },
    {
      use <- drop(match("false"))
      pure(ast.YBool(False))
    },
    text_util.signed(text_util.float(False), float.negate)
      |> atto.map(ast.YNumber("", _)),
    text_util.signed(text_util.decimal(), int.negate)
      |> atto.map(int.to_float)
      |> atto.map(ast.YNumber("", _)),
    text_util.string_lit(
      ops.choice([
        text_util.simple_char_lit(),
        text_util.char_lit(),
      ]),
    )
      |> atto.map(ast.YString),
  ])
  |> ws()
}

fn expr0() {
  ops.choice([
    val() |> atto.map(ast.Val),
    var() |> atto.map(ast.Var),
  ])
}

fn sum() {
  use left <- do(try_(expr0() |> ws()))
  use rest <- do(
    ops.many(
      try_({
        use op <- do(ops.choice([token("+"), token("-")]) |> ws())
        use right <- do(expr0() |> ws())
        pure(#(op, right))
      }),
    ),
  )
  pure(rest |> list.fold(left, fn(acc, kv) { ast.BinOp(kv.0, acc, kv.1) }))
}

fn comparison() {
  use left <- do(try_(sum() |> ws()))
  use rest <- do(
    ops.many(
      try_({
        use op <- do(ops.choice([token("<"), token(">")]) |> ws())
        use right <- do(sum() |> ws())
        pure(#(op, right))
      }),
    ),
  )
  pure(rest |> list.fold(left, fn(acc, kv) { ast.BinOp(kv.0, acc, kv.1) }))
}

fn expr() {
  comparison()
}

fn set() {
  use <- atto.label("set statement")
  use <- drop(match("set") |> ws())
  use v <- do(var())
  use <- drop(match("to") |> ws())
  use e <- do(expr())
  pure(ast.Set(v, e))
}

fn if_() {
  use <- atto.label("if statement")
  use <- drop(match("if") |> ws())
  use e <- do(expr())
  use <- drop(match(">>") |> ws())
  use body <- do(yarn_body())
  use elseifs <- do(
    ops.many(
      try_({
        use <- drop(match("<<elseif") |> ws())
        use e <- do(expr())
        use <- drop(match(">>") |> ws())
        use body <- do(yarn_body())
        pure(#(e, body))
      }),
    ),
  )
  use else_ <- do(
    atto.try({
      use <- drop(match("<<else>>") |> ws())
      yarn_body()
    }),
  )
  use <- drop(match("<<endif") |> ws())
  let list_to_none = fn(l) {
    case l {
      [] -> option.None
      _ -> option.Some(l)
    }
  }
  let all_elses =
    elseifs
    |> list.fold(else_ |> option.unwrap([]), fn(acc, kv) {
      [ast.Cmd(ast.If(kv.0, kv.1, list_to_none(acc)))]
    })
  pure(ast.If(e, body, list_to_none(all_elses)))
}

fn command() {
  use <- atto.label("command")
  use <- drop(try_(match("<<") |> ws()))
  use cmd <- do(ops.choice([jump(), set(), if_()]))
  use <- drop(match(">>") |> ws())
  pure(ast.Cmd(cmd))
}

fn try_(parser) {
  use r <- do(atto.try(parser))
  case r {
    option.Some(v) -> pure(v)
    option.None -> atto.fail_msg("Failed to parse expression")
  }
}

fn yarn_body() {
  use <- atto.label("body")
  ops.many(ops.choice([try_(choices()), try_(command()), dialog_line()]))
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
