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

fn line_contents() {
  ops.some(
    ops.choice([
      {
        echo "plain"
        use <- atto.label("plain text line")
        match("[^<{}=\n]+") |> atto.map(ast.Text)
      },
      {
        echo "inline"
        use <- atto.label("inline expression")
        use <- drop(token("{") |> hs())
        use e <- do(expr() |> hs())
        use <- drop(token("}") |> hs())
        pure(ast.Inline(e))
      },
    ]),
  )
  |> ws()
}

fn line() {
  use <- atto.label("line")
  use name <- do(atto.try(yarn_key()))
  use contents <- do(line_contents())
  let tags = []
  pure(ast.Line(contents, name, tags))
}

fn choice() {
  use <- atto.label("choice")
  use <- drop(match("->") |> hs())
  use text <- do(line_contents())
  use cond <- do(
    atto.try({
      use <- drop(match("<<if") |> hs())
      use e <- do(expr())
      use <- drop(match(">>") |> hs())
      pure(e)
    })
    |> ws(),
  )
  use body <- do(
    atto.try({
      use <- drop(token("{") |> ws())
      use x <- do(yarn_body())
      use <- drop(token("}") |> ws())
      pure(x)
    }),
  )
  let tags = []
  pure(ast.ChoiceItem(text, option.unwrap(body, []), tags, cond))
}

fn choices() {
  ops.some(choice()) |> atto.map(ast.Choice)
}

fn line_group_item() {
  use <- atto.label("line group item")
  use <- drop(try_(match("=>") |> hs()))
  use text <- do(match("[^=<{}\n]+") |> ws())
  use cond <- do(
    atto.try({
      use <- drop(match("<<if") |> hs())
      use e <- do(expr())
      use <- drop(match(">>") |> ws())
      pure(e)
    }),
  )
  use body <- do(
    atto.try({
      use <- drop(token("{") |> ws())
      use x <- do(yarn_body())
      use <- drop(token("}") |> ws())
      pure(x)
    }),
  )
  let tags = []
  pure(ast.ChoiceItem([ast.Text(text)], option.unwrap(body, []), tags, cond))
}

fn line_group() {
  ops.some(line_group_item()) |> atto.map(ast.LineGroup)
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

fn funcname() {
  match("[a-zA-Z][a-zA-Z0-9_]*") |> ws()
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

fn primary() {
  use <- atto.label("primary expression")
  ops.choice([
    try_(val()) |> atto.map(ast.Val),
    try_(var()) |> atto.map(ast.Var),
    {
      use name <- do(try_(funcname() |> hs()))
      use <- drop(try_(token("(")) |> hs())
      // TODO more than 1 arg
      use arg <- do(try_(expr()) |> hs())
      use <- drop(token(")"))
      pure(ast.Call(ast.Val(ast.YString(name)), [arg]))
    },
    {
      use <- drop(token("(") |> hs())
      use body <- do(expr() |> hs())
      use <- drop(token(")"))
      pure(body)
    },
  ])
}

fn unary() {
  use <- atto.label("unary operator")
  use unary_ops <- do(ops.many(ops.choice([token("!"), token("~")]) |> ws()))
  use right <- do(primary())
  pure(unary_ops |> list.fold(right, fn(acc, op) { ast.UnaryOp(op, acc) }))
}

fn exp() {
  use <- atto.label("exponentiation binary ops")
  binop([try_(token("^"))], unary)
}

fn prod() {
  use <- atto.label("prod binary ops")
  binop(
    [
      try_(token("*")),
      try_(token("/")),
      try_(match("//")),
      token("%"),
    ],
    exp,
  )
}

fn sum() {
  use <- atto.label("sum binary ops")
  binop(
    [
      token("+"),
      token("-"),
    ],
    prod,
  )
}

fn bitshift() {
  use <- atto.label("shift binary ops")
  binop(
    [
      try_(match(">>")),
      try_(match("<<")),
    ],
    sum,
  )
}

fn comparison_lt_gt() {
  use <- atto.label("lt gt binary ops")
  binop(
    [
      try_(match("<")),
      try_(match(">")),
      match("<="),
      match(">="),
    ],
    bitshift,
  )
}

fn comparison_eq() {
  use <- atto.label("eq binary ops")
  binop(
    [
      match("=="),
      match("!="),
    ],
    comparison_lt_gt,
  )
}

fn bitwise_and() {
  use <- atto.label("& binary op")
  binop([token("&")], comparison_eq)
}

fn bitwise_or() {
  use <- atto.label("| binary op")
  binop([token("|")], bitwise_and)
}

fn comparison_and() {
  use <- atto.label("and binary op")
  binop([match("&&")], bitwise_or)
}

fn comparison() {
  use <- atto.label("or binary ops")
  binop([match("||")], comparison_and)
}

fn binop(operators, next_level) {
  use <- atto.label("binary operator")
  use left <- do(try_(next_level() |> ws()))
  use rest <- do(
    ops.many(
      try_({
        use op <- do(
          ops.choice(operators)
          |> ws(),
        )
        use right <- do(next_level() |> ws())
        pure(#(op, right))
      }),
    ),
  )
  // TODO right-associative
  pure(rest |> list.fold(left, fn(acc, kv) { ast.BinOp(kv.0, acc, kv.1) }))
}

fn expr() {
  // Thanks for the refresh, Varun Ramesh
  // https://blog.varunramesh.net/posts/algebraic-expressions-using-parser-combinators/#left-associative-binary-operators
  use <- atto.label("expression")
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

fn decl() {
  use <- atto.label("set statement")
  use <- drop(match("declare") |> ws())
  use v <- do(var())
  use <- drop(match("=") |> ws())
  use e <- do(expr())
  pure(ast.Decl(v, e))
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

fn once() {
  use <- atto.label("once statement")
  use <- drop(match("once>>") |> ws())
  use body <- do(yarn_body())
  use <- drop(match("<<endonce") |> ws())
  pure(ast.Once(option.None, body, option.None))
}

fn command() {
  use <- atto.label("command")
  use <- drop(try_(match("<<") |> ws()))
  use cmd <- do(ops.choice([jump(), set(), decl(), once(), if_()]))
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
  ops.many(
    ops.choice([try_(choices()), try_(command()), try_(line_group()), line()]),
  )
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
