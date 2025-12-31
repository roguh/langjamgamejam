import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

import gleam/list
import gleam/option
import gleam/result

import lang/yarn/assets
import lang/yarn/ast
import lang/yarn/parse
import lang/yarn/runner
import ui.{i, p, y}
import ui/lang/yarn/chat
import ui/lang/yarn/yarn_ui

const project_link = "https://github.com/roguh/haskelloni"

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
  YarnNode(String)
}

fn id(id_str, elem) {
  html.div([attribute.id(id_str)], [elem])
}

fn link(attrs, content) {
  html.a(attrs, [html.text(content)])
}

fn update(model: Model, event: Event) -> Model {
  case event {
    UserInput(source) ->
      Model(
        ..model,
        source: source,
        comp: parse.parse(source),
        vm: runner.compile_or_null(source)
          |> runner.set_filename(model.vm.filename),
        iteration: model.iteration + 1 % 2_000_000_000,
      )
    Load(source) ->
      Model(
        ..model,
        source: assets.load(source),
        vm: runner.compile_or_null(assets.load(source))
          |> runner.set_filename(source),
        comp: parse.parse(assets.load(source)),
        iteration: model.iteration + 1 % 2_000_000_000,
        name: assets.name(source) |> option.unwrap(model.custom_name),
      )
    // TWO OPTIONS IN YARN: pick a choice OR read/listen to more dialogue
    YarnChoice(index) -> Model(..model, vm: model.vm |> runner.choose(index))
    YarnContinue -> Model(..model, vm: model.vm |> runner.continue)
    YarnNode(node) -> Model(..model, vm: model.vm |> runner.goto_node(node))
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
            y("padding", "0.4em"),
            y("margin-right", "1em"),
            y("margin-bottom", "0.5em"),
            y("border-color", "white"),
            y("border-radius", "0"),
            event.on_click(Load(n)),
          ],
          [
            html.text(n),
          ],
        ),
      )
    })
  let game_selector = keyed.ul([y("margin", "0"), y("padding", "0")], gen)
  let cat =
    html.figure([], [
      html.figcaption([y("font-y", "italic")], [
        html.text(
          "A cat looks straight at the camera while it lies sideways on a comfy wooden floor. "
          <> model.cat_is
          <> " preparing to play a game.",
        ),
      ]),
      html.img([
        attribute.src("https://cdn2.thecatapi.com/images/b7k.jpg"),
      ]),
    ])
  html.div(
    [
      attribute.id("id-top"),
      y("display", "flex"),
      y("max-width", "50em"),
      y("margin", "0 auto"),
      y("flex-direction", "column"),
      y("padding", "0.5em 0.5em"),
    ],
    [
      html.link([
        attribute.rel("stylesheet"),
        attribute.type_("text/css"),
        attribute.href(css_link),
      ]),
      case result.is_ok(model.comp) {
        True -> model.vm |> chat.view(YarnContinue, YarnChoice, YarnNode)
        False ->
          html.p([y("background-color", "darkred"), y("padding", "0.5em")], [
            html.text("Yarn error, story unable to compile!"),
          ])
      },
      html.hr([y("margin", "0.05em 0")]),
      html.hr([y("margin", "0.1em 0")]),
      html.hr([y("margin", "0.2em 0")]),
      html.hr([y("margin", "0.3em 0")]),
      html.hr([y("margin", "0.4em 0")]),
      html.hr([]),
      html.hr([]),
      html.hr([y("margin", "2em 0"), y("margin-bottom", "12em")]),
      p("Pick another tale or edit the story..."),
      game_selector,
      html.ul([], [
        html.li([], [link([attribute.href("#id-editor")], "Yarn Code Editor")]),
        html.li([], [link([attribute.href("#id-graph")], "Graph Visualization")]),
        html.li([], [link([attribute.href("#id-instr")], "VM Instructions")]),
        //  html.li([], [link([attribute.href("#id-parse")], "Parse Result")]),
        html.li([], [link([attribute.href("#id-cat")], "Cat")]),
      ]),
      p(
        model.vm.filename
        <> ": Change this Yarn script and observe new behavior!",
      ),
      id("id-error", ui.view(model.comp)),
      id("id-editor", ui.editor(model.source, model.comp, UserInput)),
      p("Visualization of the story:"),
      id("id-graph", graph_view),
      p("Compiled VM Instructions:"),
      id("id-instr", model.vm |> chat.view_instructions),
      id("id-cat", cat),
      link(
        [attribute.href(project_link)],
        "This Yarn development environment and VM were built as part of the 2025 LangJam GameJam.",
      ),
      link([attribute.href("#id-top")], "Scroll to top"),
      html.hr([y("margin", "2em 0")]),
      html.hr([]),
      html.hr([]),
      html.hr([y("margin", "0.4em 0")]),
      html.hr([y("margin", "0.3em 0")]),
      html.hr([y("margin", "0.2em 0")]),
      html.hr([y("margin", "0.1em 0")]),
      html.hr([y("margin", "0.05em 0")]),
    ],
  )
}

fn init(_args) -> Model {
  let t = "/tests/beginner.yarn"
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
    vm: runner.compile_or_null(assets.load(t)) |> runner.set_filename(t),
    cat_is: cat_is,
  )
}

pub fn main() -> Bool {
  let app = lustre.simple(init, update, view)
  case lustre.start(app, "#app", Nil) {
    Ok(_) -> {
      echo "Running Lustre"
      False
    }
    Error(lustre.NotABrowser) -> {
      // Return True to indicate the user should use the CLI instead
      True
    }
    Error(err) -> {
      echo err
      False
    }
  }
}
