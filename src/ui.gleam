import gleam/int
import gleam/list
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

import lang/yarn/ast.{type YarnArtifact}

pub fn y(key: String, value: String) {
  attribute.style(key, value)
}

pub fn p(content: String) {
  html.p([], [html.text(content)])
}

pub fn i(content: String) {
  html.p([y("font-style", "italic")], [html.text(content)])
}

pub fn code(content: String, color: String) {
  html.pre([], [
    html.code([y("color", color)], [html.text(content)]),
  ])
}

pub fn view(yarn_artifact: YarnArtifact) -> Element(a) {
  case yarn_artifact {
    Ok(content) ->
      html.div([], [
        i("No compilation errors."),
        code(ast.pretty(content), "#fff"),
      ])
    Error(error) ->
      html.div([], [
        i(
          "Error found at line "
          <> error.line |> int.to_string
          <> ":"
          <> error.col |> int.to_string,
        ),
        code(error.error, "red"),
      ])
  }
}
