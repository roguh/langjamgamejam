import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/string

import tree.{
  type Expr, type Program_, type Stmt, Assign, Block, Decl, Dict, ExprStmt, Float,
  If, Int, Lambda, List, Name, Nil, ParenList, String, While,
}

const max_safe_int = 9_007_199_254_740_991

pub fn generate(prog: Program_, output_id: String) {
  "document.getElementById("
  <> json.to_string(json.string(output_id))
  <> ").innerHTML = ("
  <> json.to_string(json.string(program_to_js(prog)))
  <> ").replace(/(\\r\\n|\\n|\\r)/g, '<br>');"
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
      <> string.join(list.map(params, expr_to_js), ", ")
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

fn program_to_js(prog: Program_) -> String {
  stmts_to_js(prog.stmts)
}
