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

import games
import js/generate
import parse
import syntax.{type Program, empty_program}

const arena_id = "___annie_arena___"

type Model {
  Model(
    source: String,
    compilation_artifact: Result(Program, String),
    iteration: Int,
    game_name: String,
    custom_name: String,
  )
}

type Event {
  UserInput(String)
  LoadGame(String)
  CustomizeName(String)
}

fn y(key: String, value: String) {
  attribute.style(key, value)
}

fn p(content: String) {
  html.p([], [html.text(content)])
}

fn code(content: String, color: String) {
  html.pre([], [
    html.code([y("color", color)], [
      html.text(content),
    ]),
  ])
}

fn update(model: Model, event: Event) -> Model {
  case event {
    UserInput(source) ->
      Model(
        source,
        parse.parse(source),
        model.iteration + 1 % 2_000_000_000,
        // TODO concise record editing in erlang
        model.game_name,
        model.custom_name,
      )
    LoadGame(game_str) ->
      Model(
        games.load(game_str),
        parse.parse(games.load(game_str)),
        model.iteration + 1 % 2_000_000_000,
        unwrap(games.name(game_str), model.custom_name),
        model.custom_name,
      )
    CustomizeName(new_name) ->
      Model(
        model.source,
        model.compilation_artifact,
        model.iteration,
        model.game_name,
        new_name,
      )
  }
}

fn view(model: Model) -> Element(Event) {
  let css_link = "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
  let result = case model.compilation_artifact {
    Ok(content) -> code(string.inspect(content), "white")
    Error(error) -> code(error, "red")
  }
  let code = case model.compilation_artifact {
    Ok(content) -> generate.generate(content, arena_id)
    Error(_error) -> ""
  }
  // Use a keyed div to insert a new script element every time the user modifies the source code
  let script =
    keyed.div([], [
      #(
        int.to_string(model.iteration),
        html.script([attribute.type_("text/javascript")], code),
      ),
    ])
  let game_buttons =
    keyed.ul(
      [],
      // TODO game icons!
      list.map(games.names(), fn(n: String) -> #(String, Element(Event)) {
        #(
          n,
          html.button(
            [
              y("background-color", games.random_color()),
              y("margin-right", "1em"),
              event.on_click(LoadGame(n)),
            ],
            [
              html.text(n),
            ],
          ),
        )
      }),
    )
  html.div(
    [
      y("display", "flex"),
      y("flex-direction", "column"),
      y("padding", "4rem"),
    ],
    [
      html.link([
        attribute.rel("stylesheet"),
        attribute.type_("text/css"),
        attribute.href(css_link),
      ]),
      html.h1([], [html.text(model.game_name)]),
      game_buttons,
      html.div(
        [
          y("min-height", "80vh"),
          attribute.id(arena_id),
        ],
        [],
      ),
      html.h2([], [html.text("Hello, world! It's game making time :)")]),
      p(
        "It appears you've found a super fun code editor for the game shown above!",
      ),
      p(
        "Why not try editing? It's harmless and you can learn from it or reload the page to reset.",
      ),
      html.figure([], [
        html.img([
          y("height", "20vh"),
          y("min-height", "200px"),
          attribute.src("https://cdn2.thecatapi.com/images/b7k.jpg"),
        ]),
        html.figcaption([y("font-y", "italic")], [
          html.text(
            "A cat looks straight at the camera while it lies sideways on a comfy wooden floor."
            <> " She is preparing to play a game.",
          ),
        ]),
      ]),
      html.input([
        attribute.type_("text"),
        attribute.placeholder("Enter game source code"),
        event.on_input(UserInput),
      ]),
      result,
      script,
    ],
  )
}

fn init(_args) -> Model {
  Model("", Ok(empty_program), int.random(2_000_000_000), "", "")
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
