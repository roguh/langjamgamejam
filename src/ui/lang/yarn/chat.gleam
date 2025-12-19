import lustre/element/keyed

import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import glearray
import lang/yarn/runner
import lustre/attribute
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

pub fn view(vm: runner.State, on_continue, on_choice, on_goto_node) {
  html.div([], [
    html.div([y("margin", "1em 0")], [
      html.h1(
        [
          y("display", "flex"),
          y("align-items", "center"),
          y("justify-content", "space-between"),
          y("max-width", "87.5%"),
        ],
        [
          html.text(vm.node),
          html.select(
            [
              y("display", "inline"),
              y("width", "auto"),
              y("margin", "0"),
              y("margin-left", "1em"),
              event.on_change(fn(val) {
                case val {
                  "" -> on_goto_node(vm.node)
                  _ -> on_goto_node(val)
                }
              }),
            ],
            [
              html.option([attribute.value("")], "GoTo?"),
              ..vm.nodes
              |> dict.to_list
              |> list.map(fn(kv) { html.option([attribute.value(kv.0)], kv.0) })
            ],
          ),
        ],
      ),
    ]),
    html.div(
      [y("margin", "1em 0"), y("min-height", "15vh")],
      vm.say |> list.reverse |> list.map(p),
    ),
    html.div(
      [
        y("min-height", "15vh"),
        y("max-width", "75%"),
        y("margin", "1em auto"),
        y("display", "flex"),
        y("flex-direction", "column"),
        y("align-items", "flex-end"),
        y("justify-content", "space-between"),
      ],
      [
        // TODO only show ONE control at a time, choose/continue/stopped
        // keyed.ol(
        //   [],
        //   vm
        //     |> runner.needs_choice
        //     |> list.index_map(fn(c, i) {
        //       let si = int.to_string(i)
        //       #(
        //         si,
        //         html.li([event.on_click(on_choice(i))], [
        //           html.text("Option " <> si),
        //           html.text(c),
        //         ]),
        //       )
        //     }),
        // ),
        q(
          vm |> runner.needs_continue,
          html.button([event.on_click(on_continue)], [
            html.text(case vm.say {
              [] -> ">>> Start story"
              _ -> "> Continue"
            }),
          ]),
          html.text(""),
        ),
        case vm.state {
          runner.Stopped ->
            html.div([y("font-size", "200%")], [
              fancy_labeled("Fin", "", "darkgreen"),
            ])
          _ -> html.text("")
        },
        html.div(
          [],
          vm.vars
            |> dict.to_list
            |> list.reverse
            |> list.map(fn(kv) {
              html.div(
                [
                  y("margin-top", "1em"),
                  y("font-size", "75%"),
                  y("display", "flex"),
                  y("justify-content", "space-between"),
                ],
                [
                  html.text(kv.0),
                  html.div([y("margin", "0 0.5em")], [html.text(" = ")]),
                  html.text(kv.1 |> runner.print_op),
                ],
              )
            }),
        ),
      ],
    ),
    html.div([y("margin-bottom", "1em")], [
      i("A tale discovered in a tome merely labeled: " <> vm.filename <> ""),
    ]),
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
