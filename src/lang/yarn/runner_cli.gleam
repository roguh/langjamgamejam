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
  let print_say = fn(vm_) {
    vm_
    |> runner.saying
    |> result.map(io.println)
    |> result.unwrap(Nil)
    vm_
  }

  let get_choice = fn(choices) {
    runner.needs_choice(vm)
    |> list.index_map(fn(c, ix) {
      io.println(ix + 1 |> int.to_string <> ": " <> c)
    })
    case choices {
      1 -> {
        io.print("Press ENTER to choose option 1>>>> ")
      }
      l -> {
        io.print(
          "Type a number between 1 and "
          <> l |> int.to_string
          <> " then press ENTER>>>> ",
        )
      }
    }
    Ok(read_line())
    |> result.map(string.trim)
    |> result.map(int.parse)
    |> result.map(result.unwrap(_, 1))
    |> result.unwrap(1)
    // User input is 1-indexed, the VM is 0-indexed
    |> int.subtract(1)
  }

  case runner.needs_choice(vm) |> list.length, runner.needs_continue(vm) {
    choices, _ if choices > 0 -> {
      vm |> runner.choose(get_choice(choices)) |> print_say |> loop
    }
    _, True -> {
      io.print("Press ENTER>>>> ")
      read_line()
      vm |> runner.continue |> print_say |> loop
    }
    _, False -> {
      vm
    }
  }
}

pub fn start_loop(vm: runner.State) {
  io.println("Hello! Welcome to this wonderful adventure. Node=" <> vm.node)
  loop(vm)
}

pub fn start_test(vm: runner.State, test_input: List(Int)) {
  runner.test_run(vm, test_input, io.println)
}
