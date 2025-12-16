import gleam/int
import gleam/list
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

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
        i("Succesfully converted to:"),
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

pub fn editor(source: String, artifact: YarnArtifact, edit_handler) {
  html.div(
    [
      y("display", "flex"),
      y("flex-direction", "row"),
      y("margin", "1em 0 2em 0"),
    ],
    [
      html.div(
        [
          y("min-height", "10em"),
          y("padding", "14px"),
          y("background-color", "black"),
        ],
        source
          |> string.split("\n")
          |> list.index_map(fn(_, ix) {
            html.p(
              [
                y("margin", "0"),
                y("padding", "0 2px"),
                y("padding-top", "0.4px"),
                y(
                  "background-color",
                  ast.error_in(artifact, ix + 1, "black", "darkred"),
                ),
              ],
              [
                html.text(ix + 1 |> int.to_string),
              ],
            )
          }),
      ),
      html.textarea(
        [
          y("min-height", "10em"),
          y("margin-bottom", "0"),
          event.on_input(edit_handler),
        ],
        source,
      ),
    ],
  )
}
