import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string

import atto.{do, drop, fail_msg, map, pure, token}
import atto/error
import atto/ops
import atto/text.{match}
import atto/text_util.{ws}

// type P(v) =
//  atto.Parser(v, String, String, a, b)
//

const max_safe_int = 9_007_199_254_740_991

type Expr {
  Name(List(String))
  Int(Int, String)
  Float(Float)
  String(String)
  List(List(Expr))
  ParenList(List(Expr))
  Dict(List(#(Expr, Expr)))
  Nil
}

type Stmt {
  ExprStmt(Expr)
  Decl(List(String), Expr)
  Assign(List(String), Expr)
  Lambda(List(String), List(Stmt))
  Block(List(Stmt))
  If(Expr, List(Stmt), List(Stmt))
  While(Expr, List(Stmt))
}

/// Convert an expression to JavaScript code.
fn expr_to_js(expr: Expr) -> String {
  case expr {
    Float(f) -> "/* Float */ " <> float.to_string(f)
    String(s) -> "\"" <> s <> "\""
    Int(i, s) ->
      case int.absolute_value(i) < max_safe_int {
        True -> int.to_string(i)
        False -> s <> "n"
      }
    Name(n) -> string.join(n, ".")
    List(items) -> "[" <> string.join(list.map(items, expr_to_js), ", ") <> "]"
    ParenList(items) ->
      "(" <> string.join(list.map(items, expr_to_js), ", ") <> ")"
    Dict(items) ->
      "{"
      <> string.join(
        list.map(items, fn(kv) { expr_to_js(kv.0) <> ": " <> expr_to_js(kv.1) }),
        ", ",
      )
      <> "}"
    Nil -> "null"
  }
}

fn stmt_to_js(stmt: Stmt) -> String {
  case stmt {
    ExprStmt(expr) -> expr_to_js(expr)
    Decl(name, expr) ->
      "let " <> string.join(name, ".") <> " = " <> expr_to_js(expr)
    Assign(name, expr) -> string.join(name, ".") <> " = " <> expr_to_js(expr)
    Lambda(params, body) ->
      "function("
      <> string.join(params, ", ")
      <> ") {\n"
      <> stmts_to_js(body)
      <> "\n}"
    Block(stmts) -> "{\n" <> stmts_to_js(stmts) <> "\n}"
    If(cond, then, else_) ->
      "if ("
      <> expr_to_js(cond)
      <> ") {\n"
      <> stmts_to_js(then)
      <> case else_ {
        [] -> "}\n"
        _ -> "} else {\n" <> stmts_to_js(else_) <> "}\n"
      }
    While(cond, body) ->
      "while (" <> expr_to_js(cond) <> ") {\n" <> stmts_to_js(body) <> "\n}"
  }
}

fn stmts_to_js(stmts: List(Stmt)) -> String {
  string.join(list.map(stmts, stmt_to_js), ";\n")
}

fn stmt() {
  ops.choice([
    try_(if__()),
    block(),
    expr() |> map(ExprStmt),
  ])
}

fn stmts() {
  ops.sep(stmt(), by: token(";") |> ws())
}

fn if__() {
  use <- atto.label("if")
  use <- drop(match("if") |> ws())
  use <- drop(token("(") |> ws())
  use cond <- do(expr())
  use <- drop(token(")") |> ws())
  use <- drop(match("then") |> ws())
  use then <- do(stmts())
  use else_ <- do(
    atto.try({
      use <- drop(match("else") |> ws())
      stmts()
    }),
  )
  pure(If(cond, then, option.unwrap(else_, [])))
}

fn block() {
  use <- atto.label("block")
  use <- drop(token("{") |> ws())
  use stmts <- do(stmts())
  use <- drop(token("}") |> ws())
  pure(Block(stmts))
}

fn expr() {
  ops.choice([
    // Must use try so the number parsers don't interfere with each other
    try_(float__()),
    try_(int__()),
    try_(hex__()),
    name(),
    list__(),
    dict(),
    paren_list(),
    str__() |> map(String),
  ])
}

fn try_(parser) {
  use r <- do(atto.try(parser))
  case r {
    option.Some(v) -> pure(v)
    option.None -> fail_msg("Failed to parse expression")
  }
}

fn str__() {
  text_util.string_lit(
    ops.choice([
      text_util.simple_char_lit(),
      text_util.char_lit(),
    ]),
  )
  |> ws()
}

fn betwixt_sep(left, parser, right) {
  use <- drop(left |> ws())
  use x <- do(ops.sep(parser, by: token(",") |> ws()))
  use <- drop(right |> ws())
  pure(x)
}

fn list__() {
  use <- atto.label("list")
  betwixt_sep(token("["), expr(), token("]")) |> map(List)
}

fn dict() {
  use <- atto.label("dict")
  betwixt_sep(token("{"), kv_pair(), token("}")) |> map(Dict)
}

fn paren_list() {
  use <- atto.label("paren_list")
  betwixt_sep(token("("), expr(), token(")")) |> map(ParenList)
}

fn kv_pair() {
  use k <- do(expr())
  use <- drop(token(":") |> ws())
  use v <- do(expr())
  pure(#(k, v))
}

fn identifier() {
  use <- atto.label("identifier")
  use x <- do(match("[A-Za-z_][A-Za-z0-9_]*"))
  case list.contains(["if", "else", "for", "while", "in"], x) {
    True -> fail_msg("Reserved keyword")
    False -> pure(x)
  }
}

fn name() {
  use <- atto.label("name")
  // TODO add [] and ()
  ops.sep1(identifier(), token("."))
  |> ws()
  |> map(Name)
}

fn float__() {
  use <- atto.label("float")
  text_util.float(False)
  |> text_util.signed(float.negate)
  |> ws()
  |> map(Float)
}

fn int__() {
  // Parse integers of any precision
  use r <- do(
    match("[0-9]+")
    |> text_util.signed(fn(s) { "-" <> s })
    |> ws()
    |> map(fn(s) { result.map(int.base_parse(s, 10), fn(i) { #(i, s) }) }),
  )
  case r {
    Ok(#(i, s)) -> pure(Int(i, s))
    Error(_) -> fail_msg("Failed to parse integer")
  }
}

fn hex__() {
  // Parse integers of any precision
  use <- drop(match("0[xX]"))
  use r <- do(
    match("[0-9a-fA-F]+")
    |> text_util.signed(fn(s) { "-" <> s })
    |> ws()
    |> map(fn(s) { result.map(int.base_parse(s, 16), fn(i) { #(i, s) }) }),
  )
  case r {
    Ok(#(i, s)) -> pure(Int(i, s))
    Error(_) -> fail_msg("Failed to parse hexadecimal integer")
  }
}

pub fn parse(input: String, output_id: String) -> Result(String, String) {
  let src = text.new(input)
  let result =
    atto.run(
      {
        use x <- do(stmts())
        use <- drop(atto.eof())
        pure(x)
      },
      src,
      Nil,
    )
  case result {
    Ok(e) ->
      Ok(
        "/* Parsed */\ndocument.getElementById('"
        <> output_id
        <> "').innerHTML = ("
        <> json.to_string(json.string(stmts_to_js(e)))
        <> ").replace(/(\\r\\n|\\n|\\r)/g, '<br>');",
      )
    Error(err) -> Error(error.pretty(err, src, False))
  }
}
