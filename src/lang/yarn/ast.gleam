import gleam/list
import gleam/option
import gleam/string

pub type YarnBody {
  Line(content: String, name: option.Option(String))
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

pub fn pretty_body(n: YarnBody) -> String {
  case n {
    Line(content, name_) ->
      case name_ {
        option.Some(name) -> name <> ": " <> content
        option.None -> content
      }
  }
}

pub fn pretty_node(n: YarnNode) -> String {
  "title: "
  <> n.title
  <> "\n--\n"
  <> n.body |> list.map(pretty_body) |> string.join("\n")
  <> "\n===\n"
}

pub fn pretty(a: Yarn) -> String {
  a.nodes |> list.map(pretty_node) |> string.join("\n\n")
}
