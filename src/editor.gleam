import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

import gleam/list
import gleam/option

import lang/yarn/assets
import lang/yarn/ast
import lang/yarn/parse
import lang/yarn/runner
import ui.{i, p, y}
import ui/lang/yarn/chat
import ui/lang/yarn/yarn_ui

type Model {
  Model(
    source: String,
    comp: ast.YarnArtifact,
    iteration: Int,
    name: String,
    custom_name: String,
    cat_is: String,
    vm: runner.State,
  )
}

type Event {
  UserInput(String)
  Load(String)
  YarnChoice(Int)
  YarnContinue
}

fn update(model: Model, event: Event) -> Model {
  case event {
    UserInput(source) ->
      Model(
        ..model,
        source: source,
        comp: parse.parse(source),
        vm: runner.compile_or_null(source),
        iteration: model.iteration + 1 % 2_000_000_000,
      )
    Load(source) ->
      Model(
        ..model,
        source: assets.load(source),
        vm: runner.compile_or_null(assets.load(source)),
        comp: parse.parse(assets.load(source)),
        iteration: model.iteration + 1 % 2_000_000_000,
        name: assets.name(source) |> option.unwrap(model.custom_name),
      )
    // TWO OPTIONS IN YARN: pick a choice OR read/listen to more dialogue
    YarnChoice(index) -> Model(..model, vm: model.vm |> runner.choose(index))
    YarnContinue -> Model(..model, vm: model.vm |> runner.continue)
  }
}

fn view(model: Model) -> Element(Event) {
  let css_link = "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
  let graph_view = case model.comp {
    Ok(content) -> yarn_ui.view(content)
    Error(_error) -> i("Check the Yarn code for errors")
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
  let game_selector = keyed.ul([y("margin", "0"), y("padding", "0")], gen)
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
      model.vm |> chat.view(YarnContinue, YarnChoice),
      game_selector,
      html.h2([], [html.text(model.name)]),
      graph_view,
      html.h3([], [
        html.text("Editor"),
      ]),
      p("Change this Yarn script and observe new behavior!"),
      ui.editor(model.source, model.comp, UserInput),
      model.vm |> chat.view_instructions,
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
  let t = "/tests/choices.yarn"
  let foxnews = ["She is", "He is", "They are"]
  let cat_is = case list.sample(foxnews, 1) {
    [n] -> n
    _ -> "She is"
  }
  Model(
    source: assets.load(t),
    comp: parse.parse(assets.load(t)),
    iteration: 0,
    name: assets.name(t) |> option.unwrap("(no name)"),
    custom_name: "",
    vm: runner.compile_or_null(assets.load(t)),
    cat_is: cat_is,
  )
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  case lustre.start(app, "#app", Nil) {
    Ok(_) -> Nil
    e -> {
        echo e
        Nil
        }
  }
}
