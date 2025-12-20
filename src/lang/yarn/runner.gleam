//////////////////// AHHHHHHHHHHHHH  TODO COMPILE RIGHT
//////////////////// AHHHHHHHHHHHHH  TODO COMPILE RIGHT
/////////// PUBLIC API

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/result
import gleam/string
import glearray.{type Array}
import lang/yarn/ast
import lang/yarn/parse

pub type State {
  State(
    // TODO enforce state within type system?
    state: ExecutionState,
    node: String,
    ip: Int,
    stack: List(Operand),
    nodes: Dict(String, Array(Instruction)),
    init: Dict(String, Operand),
    vars: Dict(String, Operand),
    // This comes from the user
    choice: Int,
    // This is given to the user
    // It is either a list of dialogue, or a list of choices depending on the exec state
    say: List(String),
    filename: String,
  )
}

pub type ExecutionState {
  Stopped
  WaitingOnChoice
  WaitingOnContinue
  Running
}

pub type Operand {
  //VArray(List(Operand))
  VString(String)
  VFloat(Float)
  VBool(Bool)
  VNil
}

pub type Instruction {
  Return
  // Go to another node
  JumpNode(node_name: String)
  // Unconditional jump
  Jump(instr_offset: Int)
  // Conditional jump
  JumpIfFalse(instr_offset: Int)
  // Data jump: jump to number on top of stack
  JumpToTop
  // Pop from the stack and set this variable
  Set(String)
  // Push value of variable to stack
  Get(String)
  // Pop two and do the thing
  Binary(String)
  Unary(String)
  Push(Operand)
  Pop
  // Stop execution
  Halt
  // Show text to the player
  ISay(Option(String))
  ISayn(Int)
  IWaitChoice
  IWaitContinue
}

pub fn print_op(op: Operand) {
  case op {
    VString(s) -> s
    VFloat(n) -> n |> float.to_string
    VBool(True) -> "true"
    VBool(False) -> "false"
    VNil -> "VNil"
  }
}

fn rand_id() {
  int.to_string(int.random(100_000_000))
}

fn nameify(name) {
  name |> option.map(fn(v) { v <> ": " }) |> option.unwrap("")
}

pub fn print_instr(i: Instruction) {
  let s = int.to_string
  case i {
    Return -> "Return"
    JumpNode(node_name) -> "JumpNode(" <> node_name <> ")"
    JumpIfFalse(instr_offset) -> "JumpIfFalse(" <> instr_offset |> s <> ")"
    JumpToTop -> "JumpToTop"
    Jump(instr_offset) -> "Jump(" <> instr_offset |> s <> ")"
    Set(var_name) -> "Set(" <> var_name <> ")"
    Get(var_name) -> "Get(" <> var_name <> ")"
    Push(operand) -> "Push(" <> operand |> print_op <> ")"
    Pop -> "Pop"
    Halt -> "Halt"
    ISay(name) -> "ISay(" <> nameify(name) <> ")"
    ISayn(n) -> "ISayn(" <> n |> s <> ")"
    IWaitContinue -> "IWaitContinue"
    IWaitChoice -> "IWaitChoice"
    Binary(op) -> "Binary(" <> op <> ")"
    Unary(op) -> "Unary(" <> op <> ")"
  }
}

pub fn null_vm() {
  State(
    state: Stopped,
    ip: 0,
    choice: -1,
    say: [],
    init: dict.from_list([]),
    vars: dict.from_list([]),
    stack: [],
    node: "",
    nodes: dict.from_list([]),
    filename: "",
  )
}

fn compile_expr(e: ast.YarnExpr) {
  case e {
    ast.Val(ast.YNumber(_s, n)) -> [Push(VFloat(n))]
    ast.Val(ast.YBool(b)) -> [Push(VBool(b))]
    ast.Val(ast.YString(s)) -> [Push(VString(s))]
    ast.Val(ast.YEnum(_type, name)) -> [Push(VString(name))]
    ast.BinOp(op, left, right) ->
      // the most beautiful single-linked list appends -__-
      list.append(list.append(compile_expr(left), compile_expr(right)), [
        Binary(op),
      ])
    ast.UnaryOp(op, expr) -> list.append(compile_expr(expr), [Unary(op)])
    ast.Var(var_name) -> [Get(var_name)]
    ast.List(_) -> todo as "compile_expr list"
  }
}

fn compile_line(l: List(ast.LineElement)) {
  case l {
    [] -> [Push(VString(""))]
    [first, ..rest] ->
      // beautiful single-linked list appends -__-
      list.append(
        case first {
          ast.Text(text) -> [Push(VString(text))]
          ast.Inline(expr) -> list.append(compile_expr(expr), [Unary("str")])
          ast.Markup(m) -> todo as "compile_line markup"
        },
        rest
          |> list.map(fn(e) {
            case e {
              ast.Text(text) -> [Push(VString(text)), Binary("+")]
              ast.Inline(expr) ->
                list.append(compile_expr(expr), [Unary("str"), Binary("+")])
              ast.Markup(m) -> todo as "compile_line markup"
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
        list.append(compile_line(content), [ISay(name), IWaitContinue])
      ast.Choice(cs) ->
        cs
        |> list.map(fn(c) {
          // eeeeeeeeeeeeeeeeeeeeeeeeeeek
          list.append(compile_line(c.content), [
            ISayn(list.length(cs)),
            IWaitChoice,
          ])
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
      ast.Cmd(ast.Once(_, body_, _)) -> {
        let id = "$$once_" <> rand_id()
        let body = compile_(body_)
        list.append(
          [Get(id), Unary("!"), JumpIfFalse(list.length(body) + 2), ..body],
          [
            Push(VBool(True)),
            Set(id),
          ],
        )
      }
      ast.LineGroup(items) -> {
        let compiled_items =
          items
          |> list.map(fn(g) {
            list.append(compile_line(g.content), [
              ISay(option.None),
              IWaitContinue,
              ..compile_(g.next)
            ])
          })
        // TODO jump to random then jump to end unconditionally
        // how to get random? how to get index of last item?
        compiled_items |> list.flatten
      }
      ast.Cmd(ast.DeclEnum(_, _)) -> todo as "decl-enum not impl"
      ast.Cmd(ast.Arbitrary(_, _)) -> todo as "arbitrary not impl"
      ast.Cmd(ast.Detour(node_name)) -> todo as "detour not impl"
      ast.Cmd(ast.Return) -> todo as "return not implemented"
    }
  })
  |> list.flatten
}

pub fn compile_or_null(source, filename) {
  State(..compile(source) |> result.unwrap(null_vm()), filename: filename)
}

pub fn compile(source: String) -> Result(State, String) {
  source
  |> parse.parse
  |> result.map_error(fn(e) {
    "Error at line " <> int.to_string(e.line) <> ": " <> e.error
  })
  |> result.map(fn(y) {
    State(
      ..null_vm(),
      state: WaitingOnContinue,
      node: y.nodes
        |> list.first
        |> result.map(fn(n) { n.title })
        |> result.unwrap(""),
      nodes: y.nodes
        |> list.map(fn(n) {
          #(n.title, n.body |> compile_ |> glearray.from_list)
        })
        |> dict.from_list,
    )
  })
}

fn top(vm: State) -> Operand {
  vm.stack |> list.first |> result.unwrap(VNil)
}

fn pop2(vm: State) {
  case vm.stack {
    [a, b, ..] -> #(a, b)
    [a, ..] -> #(a, VNil)
    [] -> #(VNil, VNil)
  }
}

fn pop(vm: State) {
  case vm.stack {
    [a, ..] -> a
    [] -> VNil
  }
}

fn rest(vm: State) {
  case vm.stack {
    [_, ..rest] -> rest
    [] -> []
  }
}

fn push(vm: State, op: Operand) {
  [op, ..vm.stack]
}

pub fn test_run(vm: State, inputs: List(Int)) -> State {
  // Calls next until Stopped or forever
  // Useful for automated test cases
  case vm.state {
    Stopped -> vm
    WaitingOnContinue -> vm |> next |> test_run(inputs)
    WaitingOnChoice ->
      case inputs {
        [choice, ..rest] -> vm |> choose(choice) |> next |> test_run(rest)
        [] -> vm
      }
    Running -> vm |> next |> test_run(inputs)
  }
}

fn run_one_instr(vm, i: Instruction) {
  case i {
    Push(op) -> State(..vm, ip: vm.ip + 1, stack: vm |> push(op))
    ISay(name) ->
      State(..vm, ip: vm.ip + 1, say: [
        nameify(name) <> vm |> top |> print_op,
        ..vm.say
      ])
    IWaitContinue -> State(..vm, ip: vm.ip + 1, state: WaitingOnContinue)
    IWaitChoice -> State(..vm, ip: vm.ip + 1, state: WaitingOnChoice)
    // Pop N values, convert to strings
    ISayn(n) -> State(..vm, ip: vm.ip + 1, say: ["pick me", "or pick me"])
    Pop -> State(..vm, ip: vm.ip + 1, stack: vm |> rest)
    Set(name) -> State(..vm |> set_var(name, vm |> top), ip: vm.ip + 1)
    Get(name) ->
      State(..vm, ip: vm.ip + 1, stack: vm |> push(vm |> get_var(name)))
    Binary(op) ->
      State(..vm, ip: vm.ip + 1, stack: vm |> push(vm |> pop2 |> bin(op)))
    Unary(op) ->
      State(..vm, ip: vm.ip + 1, stack: vm |> push(vm |> pop |> un(op)))
    Jump(offset) -> State(..vm, ip: vm.ip + offset)
    JumpToTop ->
      State(..vm, ip: vm.ip + int_or(vm |> pop, 1), stack: vm |> rest)
    JumpIfFalse(offset) ->
      State(..vm, ip: vm.ip + if_false(vm |> top, offset, 1))
    JumpNode(n) -> State(..vm, node: n)
    Halt -> State(..vm, state: Stopped)
    Return -> todo as "runner return unimplemented"
  }
}

fn next(vm: State) -> State {
  let n =
    vm.nodes
    |> dict.get(vm.node)
    |> result.unwrap(glearray.from_list([]))
  let i = n |> glearray.get_or_default(vm.ip, Halt)
  case vm.state {
    // Run until next action..
    Running -> vm |> run_one_instr(i) |> next
    // no-op
    _ -> vm
  }
}

fn int_or(v: Operand, default: Int) {
  case v {
    VFloat(i) -> float.round(i)
    _ -> default
  }
}

fn if_false(v: Operand, a, b) {
  case v {
    VBool(False) -> a
    _ -> b
  }
}

fn bin(ab: #(Operand, Operand), op: String) -> Operand {
  let a_ = ab.0
  let b_ = ab.1
  case op {
    "==" -> VBool(a_ == b_)
    ">" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VBool(b >. a)
        VString(a), VString(b) -> VBool(string.compare(a, b) == order.Gt)
        // TODO runtime errors!
        _, _ -> todo as "> runtime error"
      }
    "<" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VBool(b <. a)
        VString(a), VString(b) -> VBool(string.compare(a, b) == order.Lt)
        // TODO runtime errors!
        _, _ -> todo as "< runtime error"
      }
    "+" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VFloat(b +. a)
        VString(a), VString(b) -> VString(a <> b)
        _, _ -> todo as "+ runtime error"
      }
    _ -> todo as "unknown binary op"
  }
}

fn un(a_: Operand, op: String) -> Operand {
  case op {
    "!" ->
      case a_ {
        VBool(a) -> VBool(!a)
        VNil -> VBool(True)
        VString("") -> VBool(True)
        VFloat(0.0) -> VBool(True)
        _ -> VBool(False)
      }
    _ -> todo as "unknown unary op"
  }
}

pub fn get_var(vm: State, name: String) -> Operand {
  vm.vars |> dict.get(name) |> result.unwrap(VNil)
}

pub fn set_var(vm: State, name: String, value: Operand) -> State {
  State(..vm, vars: vm.vars |> dict.insert(name, value))
}

// easier js interop for now...
pub fn set_var_bool(vm: State, name: String, value: Bool) -> State {
  State(..vm, vars: vm.vars |> dict.insert(name, VBool(value)))
}

pub fn choose(vm: State, index: Int) -> State {
  let choices = vm.say |> list.length
  case vm.state {
    WaitingOnChoice ->
      State(..vm, choice: index, state: case index {
        i if i < 0 || i >= choices -> {
          echo "Warning! choice index out of bounds"
          WaitingOnChoice
        }
        _ -> Running
      })
    _ -> {
      echo "Warning! it is a no-op to choose in this state"
      echo vm.state
      vm
    }
  }
}

pub fn continue(vm: State) -> State {
  case vm.state {
    WaitingOnContinue -> State(..vm, state: Running) |> next
    _ -> {
      echo "Warning! it is a no-op to continue in this state"
      echo vm.state
      vm
    }
  }
}

pub fn goto_node(vm: State, new) -> State {
  State(..vm, node: new, ip: 0, state: WaitingOnContinue, choice: -1, say: [])
}

pub fn needs_choice(vm: State) -> List(String) {
  case vm.state {
    WaitingOnChoice -> vm.say
    _ -> []
  }
}

pub fn needs_continue(vm: State) -> Bool {
  vm.state == WaitingOnContinue
}

pub fn saying(vm: State) -> List(String) {
  vm.say
}
