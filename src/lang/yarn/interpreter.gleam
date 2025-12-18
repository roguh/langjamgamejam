import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import lang/yarn/ast
import lang/yarn/parse

pub type Request {
  Line(who: String, text: String)
  WaitOnChoice(text: String, choices: List(String))
}

pub type Operand {
  //VArray(List(Operand))
  VString(String)
  VFloat(Float)
  VBool(Bool)
  VNil
}

pub type ExecutionState {
  Stopped
  WaitingOnOptionSelection
  WaitingOnContinue
  Running
}

pub type State {
  State(
    state: ExecutionState,
    node: Result(String, Nil),
    ip: Int,
    nodes: Dict(String, List(Instruction)),
    init_values: Dict(String, Operand),
    var_table: Dict(String, Operand),
    need: Option(Request),
  )
}

pub type Instruction {
  Return
  JumpNode(node_name: String)
  JumpIfFalse(instr_offset: Int)
  Jump(instr_offset: Int)
  // Pop from the stack and set this variable
  Set(String)
  // Push value of variable to stack
  Get(String)
  Binary(String)
  Unary(String)
  Push(Operand)
  Pop
  // Pop two values, convert each to a string, and concatenate into one
  Concat
  // Stop execution
  Halt
  // Show text to the player
  Say(Option(String))
  // N strings describing each option are pushed to the stack
  // Ask the player to pick an option out of N items
  // Pop N strings
  // Jump to the selected option's body (or fallthrough)
  Pick(Int)
}

pub fn print_op(op: Operand) {
  case op {
    VString(s) -> "VString(" <> s <> ")"
    VFloat(n) -> "VNumber(" <> n |> float.to_string <> ")"
    VBool(True) -> "VBool(true)"
    VBool(False) -> "VBool(false)"
    VNil -> "VNil"
  }
}

pub fn print_instr(i: Instruction) {
  let s = int.to_string
  case i {
    Return -> "Return"
    JumpNode(node_name) -> "JumpNode(" <> node_name <> ")"
    JumpIfFalse(instr_offset) -> "JumpIfFalse(" <> instr_offset |> s <> ")"
    Jump(instr_offset) -> "Jump(" <> instr_offset |> s <> ")"
    Set(var_name) -> "Set(" <> var_name <> ")"
    Get(var_name) -> "Get(" <> var_name <> ")"
    Push(operand) -> "Push(" <> operand |> print_op <> ")"
    Pop -> "Pop"
    Concat -> "Concat"
    Halt -> "Halt"
    Say(text) -> "Say(" <> text |> option.unwrap("") <> ")"
    Pick(n) -> "Pick(" <> n |> s <> ")"
    Binary(op) -> "Binary(" <> op <> ")"
    Unary(op) -> "Unary(" <> op <> ")"
  }
}

fn compile_expr(e: ast.YarnExpr) {
  case e {
    ast.Val(ast.YNumber(_s, n)) -> [Push(VFloat(n))]
    ast.Val(ast.YBool(b)) -> [Push(VBool(b))]
    ast.Val(ast.YString(s)) -> [Push(VString(s))]
    ast.Val(ast.YEnum(_type, name)) -> [Push(VString(name))]
    ast.BinOp(op, left, right) ->
      list.append(list.append(compile_expr(left), compile_expr(right)), [
        Binary(op),
      ])
    ast.UnaryOp(op, expr) -> list.append(compile_expr(expr), [Unary(op)])
    ast.Var(var_name) -> [Get(var_name)]
    ast.List(_) -> todo
  }
}

fn compile_line(l: List(ast.LineElement)) {
  case l {
    [] -> [Push(VString(""))]
    [first, ..rest] ->
      list.append(
        case first {
          ast.Text(text) -> [Push(VString(text))]
          ast.Inline(expr) -> list.append(compile_expr(expr), [Unary("str")])
          ast.Markup(m) -> todo
        },
        rest
          |> list.map(fn(e) {
            case e {
              ast.Text(text) -> [Push(VString(text)), Concat]
              ast.Inline(expr) ->
                list.append(compile_expr(expr), [Unary("str"), Concat])
              ast.Markup(m) -> todo
            }
          })
          |> list.flatten,
      )
  }
}

fn compile_(b: List(ast.YarnBody)) -> List(Instruction) {
  b
  |> list.map(fn(b_) {
    case b_ {
      ast.Line(content, name, _tags) ->
        list.append(compile_line(content), [Say(name)])
      ast.Choice(cs) ->
        cs
        |> list.map(fn(c) {
          list.append(compile_line(c.content), [Pick(list.length(cs))])
        })
        |> list.flatten
      ast.Cmd(ast.Jump(node_name)) -> [JumpNode(node_name)]
      ast.Cmd(ast.Stop) -> [Halt]
      ast.Cmd(ast.Decl(name, expr)) ->
        list.append(compile_expr(expr), [Set(name)])
      ast.Cmd(ast.Set(name, expr)) ->
        list.append(compile_expr(expr), [Set(name)])
      ast.Cmd(ast.If(expr_, body_, else_)) -> {
        // Jump over body if expr is false
        // If expr is true, run the body and jump over else right after
        let expr = compile_expr(expr_)
        let body = compile_(body_)
        list.append(
          list.append(expr, [
            JumpIfFalse(
              // Jump to instruciton after body and after unconditional jump
              list.length(body) + 2,
            ),
            ..body
          ]),
          case else_ {
            option.None -> []
            option.Some(else__) -> {
              let compiled_else = compile_(else__)
              // Unconditional jump to after else
              [Jump(list.length(compiled_else) + 1), ..compiled_else]
            }
          },
        )
      }
      ast.Cmd(ast.DeclEnum(_, _)) -> todo
      ast.Cmd(ast.Arbitrary(_, _)) -> todo
      ast.Cmd(ast.Once(_, _, _)) -> todo
      ast.LineGroup(_) -> todo
      ast.Cmd(ast.Detour(node_name)) -> todo
      ast.Cmd(ast.Return) -> todo
    }
  })
  |> list.flatten
}

pub fn compile(source: String) -> Result(State, String) {
  source
  |> parse.parse
  |> result.map_error(fn(e) {
    "Error at line " <> int.to_string(e.line) <> ": " <> e.error
  })
  |> result.map(fn(y) {
    State(
      WaitingOnContinue,
      y.nodes |> list.first |> result.map(fn(n) { n.title }),
      0,
      y.nodes
        |> list.map(fn(n) { #(n.title, compile_(n.body)) })
        |> dict.from_list,
      dict.from_list([]),
      dict.from_list([]),
      option.None,
    )
  })
}

pub fn select_option(vm: State, index: Int) -> State {
  todo
}

pub fn select_continue(vm: State) -> State {
  todo
}

pub fn needs(vm: State) -> Option(Request) {
  vm.need
}

pub fn get_var(vm: State, name: String) -> Operand {
  vm.var_table |> dict.get(name) |> result.unwrap(VNil)
}

pub fn set_var(vm: State, name: String, value: Operand) -> State {
  State(..vm, var_table: vm.var_table |> dict.insert(name, value))
}
