import lustre/element/keyed

import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import glearray
import lang/yarn/runner
import lustre/element/html
import lustre/event

import ui.{code, i, p, q, y}

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

pub fn view(vm: runner.State, on_continue, on_choice) {
  html.div([y("margin", "3em"), y("min-height", "40vh")], [
    html.div([y("margin-bottom", "3em")], [
      html.h3([], [html.text(vm.node)]),
      i("A tale discovered in a tome merely labeled: \"" <> vm.filename <> "\""),
    ]),
    html.div([], vm.say |> list.reverse |> list.map(p)),
    keyed.ol(
      [],
      vm
        |> runner.needs_choice
        |> list.index_map(fn(c, i) {
          let si = int.to_string(i)
          #(
            si,
            html.li([event.on_click(on_choice(i))], [
              html.text("Option " <> si),
              html.text(c),
            ]),
          )
        }),
    ),
    q(
      vm |> runner.needs_continue,
      html.button([event.on_click(on_continue)], [
        html.text(case vm.say {
          [] -> ">>> START STORY"
          _ -> ">>>"
        }),
      ]),
      html.text(""),
    ),
    case vm.state {
      runner.Stopped ->
        html.div([y("font-size", "200%")], [
          fancy_labeled("FIN!", "", "darkblue"),
        ])
      _ -> html.text("")
    },
  ])
}

pub fn view_instructions(vm: runner.State) {
  code(
    "Variables:\n"
    <> vm.vars
    |> dict.to_list
    |> list.map(fn(kv) { kv.0 <> " = " <> kv.1 |> runner.print_op })
    |> string.join("\n")
    <> "\n\n"
    <> vm.nodes
    |> dict.to_list
    |> list.map(fn(kv) {
      "Node: "
      <> kv.0
      <> "\n"
      <> kv.1
      |> glearray.to_list
      |> list.map(runner.print_instr)
      |> string.join("\n")
    })
    |> string.join("\n\n"),
  )
}
