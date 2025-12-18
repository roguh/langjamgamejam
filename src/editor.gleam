import gleam/dict
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

import gleam/int
import gleam/list
import gleam/option.{unwrap}
import gleam/string

import lang/yarn/assets
import lang/yarn/ast
import lang/yarn/generate
import lang/yarn/interpreter
import lang/yarn/parse
import ui.{i, p, y}

type Model {
  Model(
    source: String,
    comp: ast.YarnArtifact,
    iteration: Int,
    name: String,
    custom_name: String,
    cat_is: String,
  )
}

type Event {
  UserInput(String)
  Load(String)
}

fn update(model: Model, event: Event) -> Model {
  case event {
    UserInput(source) ->
      Model(
        source,
        parse.parse(source),
        model.iteration + 1 % 2_000_000_000,
        // TODO concise record editing in erlang
        model.name,
        model.custom_name,
        model.cat_is,
      )
    Load(content) ->
      Model(
        assets.load(content),
        parse.parse(assets.load(content)),
        model.iteration + 1 % 2_000_000_000,
        unwrap(assets.name(content), model.custom_name),
        model.custom_name,
        model.cat_is,
      )
  }
}

fn view(model: Model) -> Element(Event) {
  let css_link = "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
  let graph_view = case model.comp {
    Ok(content) -> generate.generate(content)
    Error(_error) -> i("Check the Yarn code for errors")
  }
  let vm = interpreter.compile(model.source)
  let instr_view = case vm {
    Ok(s) ->
      s.nodes
      |> dict.to_list
      |> list.map(fn(kv) {
        "Node: "
        <> kv.0
        <> "\n"
        <> kv.1 |> list.map(interpreter.print_instr) |> string.join("\n")
      })
      |> string.join("\n\n")
      |> ui.code()
    _ -> html.text("")
  }

  let gen =
    list.map(assets.names(), fn(n: String) -> #(String, Element(Event)) {
      #(
        n,
        html.button(
          [
            y("margin-right", "1em"),
            event.on_click(Load(n)),
          ],
          [
            html.text(n),
          ],
        ),
      )
    })
  let game_buttons = keyed.ul([y("margin", "0"), y("padding", "0")], gen)
  html.div(
    [
      y("display", "flex"),
      y("flex-direction", "column"),
      y("padding", "1em 4em"),
    ],
    [
      html.link([
        attribute.rel("stylesheet"),
        attribute.type_("text/css"),
        attribute.href(css_link),
      ]),
      game_buttons,
      html.h2([], [html.text(model.name)]),
      graph_view,
      html.h3([], [
        html.text("Editor"),
      ]),
      p("Change this Yarn script and observe new behavior!"),
      ui.editor(model.source, model.comp, UserInput),
      instr_view,
      ui.view(model.comp),
      html.figure([], [
        html.img([
          attribute.src("https://cdn2.thecatapi.com/images/b7k.jpg"),
        ]),
        html.figcaption([y("font-y", "italic")], [
          html.text(
            "A cat looks straight at the camera while it lies sideways on a comfy wooden floor. "
            <> model.cat_is
            <> " preparing to play a game.",
          ),
        ]),
      ]),
    ],
  )
}

fn init(_args) -> Model {
  let t = "/tests/if.yarn"
  let foxnews = ["She is", "He is", "They are"]
  let cat_is = case list.sample(foxnews, 1) {
    [n] -> n
    _ -> "She is"
  }
  Model(
    assets.load(t),
    parse.parse(assets.load(t)),
    int.random(2_000_000_000),
    t,
    "(no name)",
    cat_is,
  )
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}
