import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result

import atto.{do, drop, fail_msg, map, pure, token}
import atto/error
import atto/ops
import atto/text.{match}
import atto/text_util.{ws}

import tree.{
  type CompilationArtifact, AnnieProgram, Block, Dict, ExprStmt, Float, If, Int,
  Lambda, List, Name, ParenList, String, While,
}

fn stmt() {
  ops.choice([
    if__(),
    while(),
    block(),
    lambda(),
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

fn while() {
  use <- atto.label("while")
  use <- drop(match("while") |> ws())
  use <- drop(token("(") |> ws())
  use cond <- do(expr())
  use <- drop(token(")") |> ws())
  use <- drop(match("do") |> ws())
  use body <- do(stmts())
  pure(While(cond, body))
}

fn lambda() {
  use <- atto.label("lambda")
  use <- drop(token("\\") |> ws())
  use params <- do(ops.sep(name(), by: token(",") |> ws()))
  use <- drop(match("\\\\->") |> ws())
  use body <- do(stmts())
  pure(Lambda(params, body))
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

fn prog() {
  use x <- do(stmts())
  use <- drop(atto.eof())
  pure(AnnieProgram(x, Nil))
}

pub fn parse(input: String) -> CompilationArtifact {
  let src = text.new(input)
  let parser_ = prog()
  let result = atto.run(parser_, src, Nil)
  case result {
    Ok(e) -> Ok(e)
    Error(err) -> Error(error.pretty(err, src, False))
  }
}

pub const empty_program = AnnieProgram([], Nil)
