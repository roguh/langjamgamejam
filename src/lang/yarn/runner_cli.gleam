import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import in
import lang/yarn/runner

@external(javascript, "global", "prompt")
fn read_line() -> String {
  in.read_line() |> result.unwrap("")
}

fn loop(vm: runner.State) {
  let say = fn() { vm |> runner.saying |> io.println }

  case runner.needs_choice(vm) |> list.length, runner.needs_continue(vm) {
    choices, _ if choices > 0 -> {
      runner.needs_choice(vm)
      |> list.index_map(fn(c, ix) {
        io.println(ix + 1 |> int.to_string <> ": " <> c)
      })
      let choice = case choices {
        1 -> {
          io.print("Enter number 0 or press ENTER to keep going > ")
          0
        }
        l -> {
          io.print(
            "Enter a number between 0 and " <> l |> int.to_string <> "> ",
          )
          Ok(read_line())
          |> result.map(string.trim)
          |> result.map(int.parse)
          |> result.map(result.unwrap(_, 1))
          |> result.unwrap(1)
        }
      }
      vm |> runner.choose(choice - 1) |> loop
    }
    _, True -> {
      read_line()
      say()
      vm |> runner.continue |> loop
    }
    _, _ -> {
      vm
    }
  }
}

pub fn start_loop(vm: runner.State, fname: String) -> String {
  io.println("Running yarn script: " <> fname)
  io.println("Hello! Press ENTER to start.")
  loop(vm)
  "FIN!"
}
