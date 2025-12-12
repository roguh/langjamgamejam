import lustre
import lustre/attribute
import lustre/element/html
import lustre/element/keyed
import lustre/event

import gleam/int

import parse

const arena_id = "___annie_arena___"

type Model {
  Model(
    source: String,
    compilation_artifact: Result(String, String),
    iteration: Int,
  )
}

type Event {
  UserInput(String)
}

fn y(key: String, value: String) {
  attribute.style(key, value)
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
        parse.parse(source, arena_id),
        model.iteration + 1 % 2_000_000_000,
      )
  }
}

fn view(model: Model) {
  let css_link = "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
  let result = case model.compilation_artifact {
    Ok(content) -> code(content, "white")
    Error(error) -> code(error, "red")
  }
  let code = case model.compilation_artifact {
    Ok(content) -> content
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
  html.div(
    [
      y("display", "flex"),
      y("flex-direction", "column"),
      y("padding", "4rem"),
    ],
    [
      html.link([
        attribute.rel("ysheet"),
        attribute.type_("text/css"),
        attribute.href(css_link),
      ]),
      html.div(
        [
          y("min-height", "90vh"),
          attribute.id(arena_id),
        ],
        [],
      ),
      html.h1([], [html.text("Hello, world! Time to game make :)")]),
      html.figure(
        [
          y("justify-content", "center"),
        ],
        [
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
        ],
      ),
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
  Model("", Ok(""), int.random(2_000_000_000))
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
