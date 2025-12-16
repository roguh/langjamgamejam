import gleam/json
import gleam/list
import lang/yarn/ast

pub fn generate(prog: ast.Yarn, output_id: String) {
  "document.getElementById("
  <> output_id |> json.string |> json.to_string
  <> ").innerHTML = ("
  <> "howdy"
  <> ").replace(/(\\r\\n|\\n|\\r)/g, '<br>');"
}
