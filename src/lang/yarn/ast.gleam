import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/string

// String for now
// pub type Op {
//   Eq
//   Neq
//   Gt
//   Lt
//   Lte
//   Gte
//   Or
//   Xor
//   Not
//   And
//   Add
//   Sub
//   Mul
//   Div
//   Rem
//   // Additional feature: exponentiation
//   Exp
// }

pub type YarnVal {
  YNumber(String, Float)
  YBool(Bool)
  YString(String)
  // Additional feature: markup properties can be enums
  YEnum(String, String)
}

pub type YarnExpr {
  Val(YarnVal)
  // Add lists to Yarn to support funny test programs
  List(List(YarnExpr))
  BinOp(String, YarnExpr, YarnExpr)
  UnaryOp(String, YarnExpr)
  // Variables always prefixed with $
  Var(String)
}

pub type YarnCommand {
  Jump(String)
  Detour(String)
  Return
  Stop
  // Declares are evaluated every time they are used, more like a lambda
  // "Smart Variables"
  Declare(String, YarnExpr)
  Set(String, YarnExpr)
  If(YarnExpr, YarnBody, Option(YarnBody))
  Once(Option(YarnExpr), YarnBody, Option(YarnBody))
  DeclEnum(String, List(String))
  ArbitraryCommand(String, List(YarnExpr))
}

pub type YarnMarkup {
  YarnMarkup(
    tag: String,
    // Properties can be any of the following types:
    //  Integers
    //  Floats
    //  'true' or 'false'
    //  Strings
    attr: List(#(String, YarnVal)),
    content: List(LineElement),
  )
}

pub type LineElement {
  Text(String)
  Inline(YarnExpr)
  Markup(YarnMarkup)
}

pub type YarnLineGroupItem {
  LineGroupItem(
    content: List(LineElement),
    tags: List(String),
    condition: Option(YarnExpr),
  )
}

pub type YarnBody {
  // See the pretty-printer and parser for syntax
  Line(content: List(LineElement), name: Option(String), tags: List(String))
  Choice(
    content: List(LineElement),
    next: List(YarnBody),
    tags: List(String),
    condition: Option(YarnExpr),
  )
  LineGroup(List(YarnLineGroupItem))
  Command(YarnCommand)
}

pub type YarnNode {
  YarnNode(
    title: String,
    headers: List(#(String, String)),
    body: List(YarnBody),
  )
}

pub type Yarn {
  Yarn(nodes: List(YarnNode))
}

pub type YarnError {
  YarnError(error: String, line: Int, col: Int, end_line: Int, end_col: Int)
}

pub type YarnArtifact =
  Result(Yarn, YarnError)

pub fn error_in(ar: YarnArtifact, ix: Int, default: a, error: a) {
  case ar {
    Ok(_) -> default
    Error(YarnError(_, line, _, end_line, _)) if line == ix || end_line == ix ->
      error
    _ -> default
  }
}

// TODO handle
// import gleam/float
// const precise_int_limit: Float = 10_000.0

pub fn pretty_val(val: YarnVal) -> String {
  case val {
    YString(s) -> "\"" <> s <> "\""
    YNumber(s, _n) -> s
    YBool(b) -> bool.to_string(b)
    YEnum(name, val) -> name <> "." <> val
  }
}

pub fn pretty_expr(expr: YarnExpr) -> String {
  case expr {
    Val(val) -> pretty_val(val)
    List(exprs) ->
      "[" <> exprs |> list.map(pretty_expr) |> string.join(",") <> "]"
    BinOp(op, left, right) ->
      pretty_expr(left) <> " " <> op <> " " <> pretty_expr(right)
    UnaryOp(op, expr) -> op <> pretty_expr(expr)
    Var(name) -> name
  }
}

pub fn pretty_cmd(c: YarnCommand, depth: Int) -> String {
  let prefix = string.repeat("  ", depth)
  let prefix2 = string.repeat("  ", depth + 1)

  prefix
  <> "<<"
  <> case c {
    Jump(label) -> "jump " <> label
    Detour(label) -> "detour " <> label
    Return -> "return"
    Stop -> "stop"
    Declare(name, expr) -> "declare " <> name <> " = " <> pretty_expr(expr)
    Set(name, expr) -> "set " <> name <> " to " <> pretty_expr(expr)
    If(cond, then_, else_) ->
      "if "
      <> pretty_expr(cond)
      <> ">>\n"
      <> pretty_body(then_, depth + 1)
      // TODO handle <<elseif cond>> syntax
      <> option.unwrap(
        else_
          |> option.map(fn(else_body) {
            "\n" <> prefix <> "<<else>>\n" <> pretty_body(else_body, depth + 1)
          }),
        "",
      )
      <> "\n<<endif"
    Once(cond, body, else_) ->
      "once"
      <> option.unwrap(cond |> option.map(fn(e) { " " <> pretty_expr(e) }), "")
      <> ">>\n"
      <> pretty_body(body, depth + 1)
      <> option.unwrap(
        else_
          |> option.map(fn(else_body) {
            "\n" <> prefix <> "<<else>>\n" <> pretty_body(else_body, depth + 1)
          }),
        "",
      )
      <> "\n<<endonce"
    DeclEnum(name, values) ->
      "<<enum>>"
      <> name
      <> " {\n"
      <> values
      |> list.map(fn(v) { prefix2 <> "<<case " <> v <> ">>" })
      |> string.join("\n")
      <> "\n<<endenum"
    ArbitraryCommand(command, args) ->
      command
      <> " "
      <> args
      |> list.map(pretty_expr)
      |> string.join("\n")
      <> ">>\n"
  }
  <> ">>"
}

pub fn pretty_tags(tags: List(String)) -> String {
  tags |> list.map(fn(t) { " #" <> t }) |> string.join(" ")
}

fn pretty_markup(markup: YarnMarkup) -> String {
  let attrs =
    markup.attr
    |> list.map(fn(kv) { kv.0 <> "=" <> pretty_val(kv.1) })
    |> string.join("")
  "["
  <> markup.tag
  <> attrs
  <> "]"
  <> pretty_line(markup.content)
  <> "[/"
  <> markup.tag
  <> "]"
}

pub fn pretty_line(content: List(LineElement)) -> String {
  content
  |> list.map(fn(e) {
    case e {
      Text(t) -> t
      Inline(e) -> "{" <> pretty_expr(e) <> "}"
      Markup(m) -> pretty_markup(m)
    }
  })
  |> string.join(" ")
}

pub fn pretty_body(b: YarnBody, depth: Int) -> String {
  string.repeat("  ", depth)
  <> case b {
    Line(content, name_, tags) ->
      option.unwrap(name_ |> option.map(fn(n) { n <> ": " }), "")
      <> pretty_line(content)
      <> pretty_tags(tags)
      <> "  // line "
      <> int.to_string(depth)
    Choice(content, next, tags, cond) ->
      "-> "
      <> pretty_line(content)
      <> option.unwrap(
        cond |> option.map(fn(c) { "<<if " <> pretty_expr(c) <> ">>" }),
        "",
      )
      <> pretty_tags(tags)
      <> "  // choice "
      <> int.to_string(depth)
      <> next |> list.map(pretty_body(_, depth + 1)) |> string.join("\n  ")
    Command(cmd) -> pretty_cmd(cmd, depth)
    LineGroup(items) ->
      items
      |> list.map(fn(g) {
        "=> "
        <> pretty_line(g.content)
        <> option.unwrap(
          g.condition |> option.map(fn(c) { "<<if " <> pretty_expr(c) <> ">>" }),
          "",
        )
        <> pretty_tags(g.tags)
        <> "  // line-group "
        <> int.to_string(depth)
      })
      |> string.join("\n  ")
  }
}

pub fn pretty_node(n: YarnNode) -> String {
  [#("title", n.title), ..n.headers]
  |> list.map(fn(kv) { kv.0 <> ": " <> kv.1 })
  |> string.join("\n")
  <> "\n---\n"
  <> n.body |> list.map(pretty_body(_, 0)) |> string.join("\n")
  <> "\n===\n"
}

pub fn pretty(a: Yarn) -> String {
  a.nodes |> list.map(pretty_node) |> string.join("\n\n")
}
