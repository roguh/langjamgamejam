import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import lang/yarn/ast
import lustre/attribute

import lustre/element/html

import ui.{p, y}

fn fancy_labeled(label: String, content, color) {
  html.p([], [
    case label {
      "" -> html.text("")
      l ->
        html.em(
          [
            y("border", "var(--pico-color) 3px"),
            y("border-radius", "7px"),
            y("padding", "0.2em 0.5em"),
            y("margin-right", "0.5em"),
            y("background-color", color),
            // No dark/light mode
            y("color", "white"),
          ],
          [
            html.text(l),
          ],
        )
    },
    html.text(content),
  ])
}

fn print_expr(_e: ast.YarnExpr) {
  "(expr)"
}

fn gen_lineelem(l: ast.LineElement) {
  case l {
    ast.Text(t) -> t
    ast.Markup(_) -> "(markup)"
    ast.Inline(e) -> e |> print_expr
  }
}

fn gen_line(l: List(ast.LineElement)) {
  l |> list.map(gen_lineelem) |> string.join("")
}

fn gen_lines(b: ast.YarnBody, char_colors: Dict(String, String)) {
  let catch_else = fn(else_body) {
    else_body
    |> option.map(fn(else_body_) {
      fancy_node([
        fancy_labeled_cmd("else", "", "#661008"),
        ..else_body_
        |> list.map(gen_lines(_, char_colors))
      ])
    })
    |> option.unwrap(html.text(""))
  }
  case b {
    ast.Line(c, name, _tags) ->
      fancy_labeled(
        name |> option.unwrap(""),
        c |> gen_line,
        char_colors
          |> dict.get(name |> option.unwrap(""))
          |> result.unwrap("darkgreen"),
      )
    ast.LineGroup(items) ->
      html.div(
        [],
        items
          |> list.index_map(fn(c, ix) {
            fancy_node([
              fancy_labeled(
                "=> RAND " <> ix + 1 |> int.to_string,
                c.content |> gen_line,
                "#666611",
              ),
              ..c.next
              |> list.map(gen_lines(_, char_colors))
            ])
          }),
      )
    ast.Choice(items) ->
      html.div(
        [],
        items
          |> list.index_map(fn(c, ix) {
            fancy_node([
              fancy_labeled(
                "-> #" <> ix + 1 |> int.to_string,
                c.content |> gen_line,
                "#446611",
              ),
              ..c.next
              |> list.map(gen_lines(_, char_colors))
            ])
          }),
      )
    ast.Cmd(ast.Jump(to)) -> fancy_labeled_cmd("jump", to, "#661008")
    ast.Cmd(ast.Decl(n, e)) ->
      fancy_labeled_cmd("declare", n <> " to " <> ast.pretty_expr(e), "#666600")
    ast.Cmd(ast.Set(n, e)) ->
      fancy_labeled_cmd("set", n <> " to " <> ast.pretty_expr(e), "#666600")
    ast.Cmd(ast.If(e, t, else_body)) ->
      fancy_node([
        fancy_labeled_cmd("if", ast.pretty_expr(e), "#661008"),
        fancy_node(
          t
          |> list.map(gen_lines(_, char_colors)),
        ),
        case else_body {
          Some([ast.Cmd(ast.If(e, tt, ff))]) ->
            fancy_node(
              list.append(
                [
                  fancy_labeled_cmd("elseif", ast.pretty_expr(e), "#661008"),
                  ..tt
                  |> list.map(gen_lines(_, char_colors))
                ],
                [catch_else(ff)],
              ),
            )
          f -> catch_else(f)
        },
      ])
    ast.Cmd(ast.Once(_cond, t, _else)) ->
      fancy_node([
        fancy_labeled_cmd("once", "", "#761804"),
        fancy_node(
          t
          |> list.map(gen_lines(_, char_colors)),
        ),
      ])
    _ -> p("(other element)")
  }
}

fn gen_node(node: ast.YarnNode, ix: Int) {
  let char_colors =
    [#("Captain", "darkgreen"), #("Navigator", "#348B42")] |> dict.from_list
  fancy_node([
    html.h4([y("margin", "0.5em 0"), y("margin-bottom", "1em")], [
      fancy_labeled("Node #" <> ix + 1 |> int.to_string, node.title, "#084411"),
    ]),
    ..node.body
    |> list.map(gen_lines(_, char_colors))
  ])
}

fn fancy_node(c) {
  html.div(
    [
      attribute.class("node"),
      y("padding", "0.5em"),
      y("margin", "0.5em"),
      y("margin-bottom", "2em"),
      y("border-radius", "5px"),
      y("border", "var(--pico-color) solid 4px"),
      y("border-top-width", "1px"),
      y("border-left-width", "1px"),
    ],
    c,
  )
}

fn fancy_labeled_cmd(label: String, content, color) {
  html.p([], [
    case label {
      "" -> html.text("")
      l ->
        html.strong(
          [
            y("border", "var(--pico-color) 3px"),
            y("border-radius", "7px"),
            y("padding", "0.2em 0.5em"),
            y("margin-right", "0.5em"),
            y("background-color", color),
            // No dark/light mode
            y("color", "white"),
          ],
          [
            html.text(l),
          ],
        )
    },
    html.text(content),
  ])
}

fn stylesheet() {
  html.style([], "p:last-child, div.node:last-child { margin-bottom: 0px; }")
}

pub fn view(graph: ast.Yarn) {
  html.div([], [stylesheet(), ..graph.nodes |> list.index_map(gen_node)])
}
