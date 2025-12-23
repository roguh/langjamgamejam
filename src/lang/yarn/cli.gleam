import gleam/io
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
  Interpret
}

type YarnRun {
  YarnRun(fname: String, source: String, mode: YarnMode)
}

type YarnCommand {
  Nothing
  YarnRunCommand(YarnRun)
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
    "What to do with the Yarn source. One of: parse, instr, js, lua, interpret",
  )
  |> opt.map(fn(m) {
    case m |> string.lowercase {
      "parse" -> Parse
      "instr" -> PrintInstructions
      "js" -> CompileJS
      "lua" -> CompileLua
      "interpret" -> Interpret
      _ -> default_mode
    }
  })
  |> opt.default(default_mode)
}

fn command() -> clip.Command(Result(YarnCommand, String)) {
  clip.command({
    use fname <- clip.parameter
    use eval <- clip.parameter
    use mode <- clip.parameter
    result.or(
      fname
        |> result.replace_error("No filename given")
        |> result.map(fn(f) {
          simplifile.read(f) |> result.map_error(simplifile.describe_error)
        })
        |> result.flatten
        |> result.map(fn(source) {
          YarnRunCommand(YarnRun(fname |> result.unwrap(""), source, mode))
        }),
      result.or(
        eval
          |> result.replace_error("No filename or --eval given")
          |> result.map(fn(source) {
            YarnRunCommand(YarnRun("<eval>", source, mode))
          }),
        Ok(Nothing),
      ),
    )
  })
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
    Interpret ->
      runner.compile(y.source) |> result.map(runner_cli.start_loop(_, y.fname))
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
