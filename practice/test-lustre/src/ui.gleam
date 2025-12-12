import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

import pretty
import tree.{type CompilationArtifact}

pub fn y(key: String, value: String) {
  attribute.style(key, value)
}

pub fn p(content: String) {
  html.p([], [html.text(content)])
}

pub fn code(content: String, color: String) {
  html.pre([], [
    html.code([y("color", color)], [
      html.text(content),
    ]),
  ])
}

pub fn view(compilation_artifact: CompilationArtifact) -> Element(a) {
  case compilation_artifact {
    Ok(content) -> code(pretty.pretty(content), "white")
    Error(error) -> code(error, "red")
  }
}
