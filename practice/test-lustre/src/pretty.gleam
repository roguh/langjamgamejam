import gleam/float
import gleam/int
import gleam/list
import gleam/string

import syntax.{
  type Expr, type Program, type Stmt, Assign, Block, Decl, Dict, ExprStmt, Float,
  If, Int, Lambda, List, Name, Nil, ParenList, String, While,
}

pub fn pretty_expr(e: Expr) {
  case e {
    Name(names) -> string.join(names, ".")
    Int(value, _) -> int.to_string(value)
    Float(value) -> float.to_string(value)
    String(s) -> "\"" <> s <> "\""
    List(items) -> "[" <> string.join(list.map(items, pretty_expr), ", ") <> "]"
    ParenList(items) ->
      "(" <> string.join(list.map(items, pretty_expr), ", ") <> ")"
    Dict(items) ->
      "{"
      <> string.join(
        list.map(items, fn(kv) {
          pretty_expr(kv.0) <> ": " <> pretty_expr(kv.1)
        }),
        ", ",
      )
      <> "}"
    Nil -> "Nil"
  }
}

pub fn pretty_stmt(s: Stmt) {
  case s {
    ExprStmt(e) -> pretty_expr(e)
    Decl(names, expr) ->
      "Decl " <> string.join(names, ".") <> " = " <> pretty_expr(expr)
    Assign(names, expr) ->
      "Assign " <> string.join(names, ".") <> " = " <> pretty_expr(expr)
    Lambda(params, body) ->
      "Lambda("
      <> string.join(list.map(params, pretty_expr), ", ")
      <> ") {\n"
      <> string.join(list.map(body, pretty_stmt), "\n")
      <> "\n}"
    Block(stmts) ->
      "Block {\n" <> string.join(list.map(stmts, pretty_stmt), "\n") <> "\n}"
    If(cond, then, els) ->
      "If "
      <> pretty_expr(cond)
      <> " Then {\n"
      <> string.join(list.map(then, pretty_stmt), "\n")
      <> "\n} Else {\n"
      <> string.join(list.map(els, pretty_stmt), "\n")
      <> "\n}"
    While(cond, body) ->
      "While "
      <> pretty_expr(cond)
      <> " {\n"
      <> string.join(list.map(body, pretty_stmt), "\n")
      <> "\n}"
  }
}

pub fn pretty(prog: Program) -> String {
  let r = string.join(list.map(prog.stmts, pretty_stmt), "\n")
  "// " <> int.to_string(string.length(r)) <> "bytes.\n" <> r
}
