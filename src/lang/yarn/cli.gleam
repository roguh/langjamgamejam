import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import argv
import clip
import clip/help
import clip/opt.{type Opt}
import lang/yarn/ast.{pretty, pretty_error}
import lang/yarn/parse
import lang/yarn/runner
import lang/yarn/runner_cli
import simplifile

const default_mode = Parse

type YarnMode {
  Parse
  PrintInstructions
  CompileJS
  CompileLua
  Interactive
  Test
}

type YarnRun {
  YarnRun(
    fname: String,
    source: String,
    mode: YarnMode,
    test_input: List(Int),
    debug_trace: Bool,
  )
}

type YarnCommand {
  Nothing
  YarnRunCommand(YarnRun)
}

fn debug_trace() -> Opt(Bool) {
  opt.new("trace")
  |> opt.short("t")
  |> opt.help("Enable trace printing for each instruction being executed.")
  |> opt.map(fn(_) { True })
  |> opt.default(False)
}

fn validate_test_input(s: String) -> Result(List(Int), String) {
  s
  |> string.split(",")
  |> list.fold(Ok([]), fn(res, s) {
    case res, int.parse(s) {
      Error(_), _ -> res
      _, Error(_) -> Error("Unable to parse integer \"" <> s <> "\"")
      _, Ok(i) if i < 1 ->
        Error("Input must be a positive integer, choices start at 1.")
      Ok(r), Ok(v) -> Ok([v, ..r])
    }
  })
  |> result.map(list.reverse)
}

fn test_input() -> Opt(Result(List(Int), String)) {
  opt.new("test-input")
  |> opt.short("i")
  |> opt.help("Simulate these comma-separated user choices.")
  |> opt.map(validate_test_input)
  |> opt.default(Ok([]))
}

fn file() -> Opt(Result(String, Nil)) {
  opt.new("fname")
  |> opt.short("f")
  |> opt.help("Filename to file containing Yarn source code")
  |> opt.optional
}

fn eval() -> Opt(Result(String, Nil)) {
  opt.new("eval")
  |> opt.short("e")
  |> opt.help("Yarn source code to evaluate directly")
  |> opt.optional
}

fn mode() -> Opt(YarnMode) {
  opt.new("mode")
  |> opt.short("m")
  |> opt.help(
    "What to do with the Yarn source. One of: parse, instr, js, lua, interact, test, or abbreviation p, s, j, l, i, t",
  )
  |> opt.map(fn(m) {
    case m |> string.lowercase {
      "parse" -> Parse
      "p" -> Parse
      "instr" -> PrintInstructions
      "s" -> PrintInstructions
      "js" -> CompileJS
      "j" -> CompileJS
      "lua" -> CompileLua
      "l" -> CompileLua
      "interact" -> Interactive
      "i" -> Interactive
      "test" -> Test
      "t" -> Test
      _ -> default_mode
    }
  })
  |> opt.default(default_mode)
}

fn command() -> clip.Command(Result(YarnCommand, String)) {
  clip.command({
    use test_input <- clip.parameter
    use debug_trace <- clip.parameter
    use fname <- clip.parameter
    use eval <- clip.parameter
    use mode <- clip.parameter
    let test_input_ =
      test_input
      |> result.map_error(fn(err) {
        io.println_error("Unable to parser --test-input: " <> err)
      })
      |> result.unwrap([])

    result.or(
      fname
        |> result.replace_error("No filename given")
        |> result.map(fn(f) {
          simplifile.read(f) |> result.map_error(simplifile.describe_error)
        })
        |> result.flatten
        |> result.map(fn(source) {
          YarnRunCommand(YarnRun(
            fname |> result.unwrap(""),
            source,
            mode,
            test_input_,
            debug_trace,
          ))
        }),
      result.or(
        eval
          |> result.replace_error("No filename or --eval given")
          |> result.map(fn(source) {
            YarnRunCommand(YarnRun(
              "<eval>",
              source,
              mode,
              test_input_,
              debug_trace,
            ))
          }),
        Ok(Nothing),
      ),
    )
  })
  |> clip.opt(test_input())
  |> clip.opt(debug_trace())
  |> clip.opt(file())
  |> clip.opt(eval())
  |> clip.opt(mode())
}

fn run_yarn(y: YarnRun) {
  case y.mode {
    Parse ->
      parse.parse(y.source)
      |> result.map(pretty)
      |> result.map_error(pretty_error)
    PrintInstructions -> runner.compile(y.source) |> result.map(runner.print)
    Test ->
      runner.compile(y.source)
      |> result.map(runner.debug_config(_, y.debug_trace))
      |> result.map(runner.set_filename(_, y.fname))
      |> result.map(runner_cli.start_test(_, y.test_input))
      |> result.replace("The end.")
    Interactive ->
      runner.compile(y.source)
      |> result.map(runner.debug_config(_, y.debug_trace))
      |> result.map(runner.set_filename(_, y.fname))
      |> result.map(runner_cli.start_loop)
      |> result.replace("The end.")
    CompileJS -> Error("JS compilation not implemented yet")
    CompileLua -> Error("Lua compilation not implemented yet")
  }
}

pub fn main() -> Nil {
  let result =
    command()
    |> clip.help(help.simple("run", "Run, parse, or compile Yarn code."))
    |> clip.run(argv.load().arguments)
    |> result.flatten
  case result {
    Error(e) -> io.println_error(e)
    Ok(Nothing) -> Nil
    Ok(YarnRunCommand(r)) ->
      r
      |> run_yarn
      |> result.map(io.println)
      |> result.map_error(io.println_error)
      |> result.unwrap(Nil)
  }
}
