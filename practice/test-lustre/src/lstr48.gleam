import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

import gleam/int
import gleam/list
import gleam/option.{unwrap}

import games
import js/generate
import parse
import tree.{type AnnieProgram}
import ui.{i, p, y}

const arena_id = "___annie_arena___"

type Model {
  Model(
    source: String,
    compilation_artifact: Result(AnnieProgram, String),
    iteration: Int,
    game_name: String,
    custom_name: String,
    cat_is: String,
  )
}

type Event {
  UserInput(String)
  LoadGame(String)
  CustomizeName(String)
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
        model.cat_is,
      )
    LoadGame(game_str) ->
      Model(
        games.load(game_str),
        parse.parse(games.load(game_str)),
        model.iteration + 1 % 2_000_000_000,
        unwrap(games.name(game_str), model.custom_name),
        model.custom_name,
        model.cat_is,
      )
    CustomizeName(new_name) ->
      Model(
        model.source,
        model.compilation_artifact,
        model.iteration,
        model.game_name,
        new_name,
        model.cat_is,
      )
  }
}

fn view(model: Model) -> Element(Event) {
  let css_link = "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
  let compilation_view = ui.view(model.compilation_artifact)
  let code = case model.compilation_artifact {
    Ok(content) -> generate.generate(content, arena_id)
    Error(_error) -> ""
  }
  let err = case model.compilation_artifact {
    Ok(_) -> ""
    Error(_error) -> "check your code for an error"
  }
  // Use a keyed div to insert a new script element every time the user modifies the source code
  let script =
    keyed.div([], [
      #(
        int.to_string(model.iteration),
        html.script([attribute.type_("text/javascript")], code),
      ),
    ])
  let gen =
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
    })
  let game_buttons =
    keyed.ul(
      [y("margin", "0"), y("padding", "0")],
      // TODO game icons!
      [
        #(
          "from_file",
          html.button(
            [
              y("background-color", "darkblue"),
              y("margin-right", "1em"),
              event.on_click(LoadGame("\"custom!\"")),
            ],
            [
              html.text("Load from file"),
            ],
          ),
        ),
        ..gen
      ],
    )
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
      html.h1([], [html.text(model.game_name)]),
      i(err),
      html.div(
        [
          y("min-height", "80vh"),
          attribute.id(arena_id),
        ],
        [],
      ),
      p("Hint: scroll down"),
      html.div([y("height", "10vh")], []),
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
            "A cat looks straight at the camera while it lies sideways on a comfy wooden floor. "
            <> model.cat_is
            <> " preparing to play a game.",
          ),
        ]),
      ]),
      html.div([y("display", "flex"), y("flex-direction", "row")], [
        html.input([
          attribute.type_("text"),
          attribute.placeholder("Enter game source code"),
          attribute.value(model.source),
          event.on_input(UserInput),
        ]),
        html.button(
          [
            y("background-color", "darkblue"),
            y("margin-left", "1em"),
            attribute.type_("button"),
          ],
          [
            html.text("Download"),
          ],
        ),
      ]),
      compilation_view,
      script,
    ],
  )
}

fn init(_args) -> Model {
  let foxnews = ["She is", "He is", "They are"]
  let cat_is = case list.sample(foxnews, 1) {
    [n] -> n
    _ -> "She is"
  }
  Model(
    "",
    Ok(parse.empty_program),
    int.random(2_000_000_000),
    "",
    "(no name)",
    cat_is,
  )
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}
