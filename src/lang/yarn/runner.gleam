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

import gleam/io

pub type State {
  State(
    state: ExecutionState,
    node: String,
    ip: Int,
    stack: List(Operand),
    jump_table: Dict(String, Int),
    // Each label shoulld be globally unique, Int is the instruction pointer within a node
    nodes: Dict(String, Array(Instruction)),
    init: Dict(String, Operand),
    vars: Dict(String, Operand),
    // This is given to the user
    // It is either a list of dialogue, or a list of choices depending on the exec state
    say: List(String),
    filename: String,
    trace_print: Bool,
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
  // Labels are converted to instruction pointers and added to jump table
  Label(String)
  // Return to previous node
  Return
  // Go to another node
  JumpNode(node_name: String)
  // Unconditional jump
  Jump(instr_offset: Int)
  // Conditional jump
  JumpIfFalse(instr_offset: Int)
  // Unconditional jump to label
  JumpLabel(label: String)
  // Conditional jump to label
  JumpLabelIfFalse(label: String)
  // Data jump: jump to number on top of stack
  JumpToTop
  // Pop from the stack and set this variable
  Set(String)
  // Push value of variable to stack
  Get(String)
  // Pop two and do the thing
  Binary(String)
  Unary(String)
  Native(String)
  // Call top of stack as a function with all function args below it
  CallTop
  Push(Operand)
  Pop
  // Stop execution
  Halt
  // Show text to the player
  ISay(Option(String))
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

pub fn print(s: State) {
  "Variables:\n"
  <> s.vars
  |> dict.to_list
  |> list.map(fn(kv) { kv.0 <> " = " <> kv.1 |> print_op })
  |> string.join("\n")
  <> "\n\n"
  <> s.nodes
  |> dict.to_list
  |> list.map(fn(kv) {
    "Node: "
    <> kv.0
    <> "\n"
    <> kv.1
    |> print_instrs(s.jump_table)
  })
  |> string.join("\n\n")
}

pub fn print_instrs(instrs: Array(Instruction), jump_table: Dict(String, Int)) {
  let get_label = fn(l) {
    jump_table
    |> dict.get(l)
    |> result.map(int.to_string)
    |> result.unwrap("unknown!")
  }

  let s = int.to_string
  instrs
  |> glearray.to_list
  |> list.index_map(fn(i, index) {
    index |> int.to_string
    <> ": "
    <> case i {
      Return -> "Return"
      JumpNode(node_name) -> "JumpNode(" <> node_name <> ")"
      JumpIfFalse(instr_offset) -> "JumpIfFalse(" <> instr_offset |> s <> ")"
      Jump(instr_offset) -> "Jump(" <> instr_offset |> s <> ")"
      JumpToTop -> "JumpToTop"
      JumpLabelIfFalse(label) ->
        "JumpIfFalse(" <> label <> ", " <> get_label(label) <> ")"
      JumpLabel(label) -> "Jump(" <> label <> ", " <> get_label(label) <> ")"
      Label(label) -> "Label(" <> label <> ", " <> get_label(label) <> ")"
      Set(var_name) -> "Set(" <> var_name <> ")"
      Get(var_name) -> "Get(" <> var_name <> ")"
      Push(operand) -> "Push(" <> operand |> print_op <> ")"
      Pop -> "Pop"
      Halt -> "Halt"
      ISay(name) -> "ISay(" <> nameify(name) <> ")"
      IWaitContinue -> "IWaitContinue"
      IWaitChoice -> "IWaitChoice"
      Binary(op) -> "Binary(" <> op <> ")"
      Unary(op) -> "Unary(" <> op <> ")"
      Native(op) -> "Native(" <> op <> ")"
      CallTop -> "CallTop"
    }
  })
  |> string.join("\n")
}

pub fn null_vm() {
  State(
    state: Stopped,
    ip: 0,
    say: [],
    jump_table: dict.from_list([]),
    init: dict.from_list([]),
    vars: dict.from_list([]),
    stack: [],
    node: "",
    nodes: dict.from_list([]),
    filename: "",
    trace_print: False,
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
    ast.Call(name, [arg]) ->
      list.append(compile_expr(arg), list.append(compile_expr(name), [CallTop]))
    ast.Call(_, _) -> todo as "only one argument functions for now"
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
          ast.Inline(expr) -> list.append(compile_expr(expr), [Native("str")])
          ast.Markup(_) -> todo as "compile_line markup"
        },
        rest
          |> list.map(fn(e) {
            case e {
              ast.Text(text) -> [Push(VString(text)), Binary("+")]
              ast.Inline(expr) ->
                list.append(compile_expr(expr), [Native("str"), Binary("+")])
              ast.Markup(_) -> todo as "compile_line markup"
            }
          })
          |> list.flatten,
      )
  }
}

fn compile_choice(
  cs,
  label: String,
  choice_as_top: List(Instruction),
  restate_chosen: Bool,
) {
  let endlabel = label <> "end_" <> rand_id()
  let choicelabels =
    cs
    |> list.index_map(fn(_v, index) {
      #(index, label <> "_" <> index |> int.to_string <> "_" <> rand_id())
    })
    |> dict.from_list
  let lbl = fn(i) {
    choicelabels
    |> dict.get(i)
    |> result.unwrap("")
  }

  //4 appends :)
  list.append(
    list.append(
      list.append(choice_as_top, [
        // Jump Table
        // The top of the stack is guaranteed to be between 0 and N-1
        // Jump to 1+choice, then jump to choice's label
        JumpToTop,
        ..cs
        |> list.index_map(fn(_, index) { JumpLabel(index |> lbl) })
      ]),
      cs
        |> list.index_map(fn(c, index) {
          // Run contents then jump to end of this choice to skip over all other choices
          list.append(
            [
              Label(index |> lbl),
              ..list.append(
                case restate_chosen {
                  True ->
                    list.append(c.content |> compile_line, [
                      ISay(option.None),
                      IWaitContinue,
                    ])
                  False -> []
                },
                c.next |> compile_,
              )
            ],
            [
              JumpLabel(endlabel),
            ],
          )
        })
        |> list.flatten,
    ),
    [Label(endlabel)],
  )
}

fn compile_(b: List(ast.YarnBody)) -> List(Instruction) {
  b
  |> list.map(fn(b_) {
    case b_ {
      ast.Line(content, name, _tags) ->
        list.append(compile_line(content), [ISay(name), IWaitContinue])
      ast.Choice(cs) ->
        compile_choice(
          cs,
          "choice",
          list.append(
            // Step 1/3: Leave choice contents as N strings on the stack
            cs
              |> list.map(fn(c) {
                list.append(c.content |> compile_line, [
                  // Step 2/3: Present the user N strings from the stack
                  ISay(option.None),
                ])
              })
              |> list.flatten,
            [
              // Step 3/3: The user will leave an integer between 0 and N-1 on top of the stack
              IWaitChoice,
              Push(VFloat(1.0)),
              Binary("+"),
            ],
          ),
          False,
        )
      ast.LineGroup(items) ->
        compile_choice(
          items,
          "linegroup",
          [
            // Leave a choice from 0 to N-1 on the stack as a string
            Push(VFloat(items |> list.length |> int.to_float)),
            Native("dice"),
          ],
          True,
        )
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
      ast.Cmd(ast.DeclEnum(_, _)) -> todo as "decl-enum not impl"
      ast.Cmd(ast.Arbitrary(_, _)) -> todo as "arbitrary not impl"
      ast.Cmd(ast.Detour(_)) -> todo as "detour not impl"
      ast.Cmd(ast.Return) -> todo as "return not implemented"
    }
  })
  |> list.flatten
}

pub fn compile_or_null(source) {
  compile(source) |> result.unwrap(null_vm())
}

pub fn compile_error(source, _) -> String {
  compile(source)
  |> result.map_error(fn(e) { e })
  |> result.unwrap_error("")
}

pub fn compile(source: String) -> Result(State, String) {
  source
  |> parse.parse
  |> result.map_error(ast.pretty_error)
  |> result.map(fn(y) {
    let ns =
      y.nodes
      |> list.map(fn(n) { #(n.title, n.body |> compile_) })
    let jump_table =
      ns
      |> list.map(fn(kv) {
        kv.1
        |> list.index_map(fn(instr, i) {
          case instr {
            Label(name) -> #(name, i)
            // Label X is at integer I in this node's instructions :)
            _ -> #("", 0)
            // lol hack
          }
        })
      })
      |> list.flatten
      |> dict.from_list

    State(
      ..null_vm(),
      jump_table: jump_table,
      state: WaitingOnContinue,
      node: y.nodes
        |> list.first
        |> result.map(fn(n) { n.title })
        |> result.unwrap(""),
      nodes: ns
        |> list.map(fn(kv) { #(kv.0, kv.1 |> glearray.from_list) })
        |> dict.from_list,
    )
  })
}

fn top(vm: State) -> Operand {
  vm.stack |> list.first |> result.unwrap(VNil)
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

fn pusha(array, op: Operand) {
  [op, ..array]
}

pub fn test_run(
  vm: State,
  inputs: List(Int),
  printer: fn(String) -> Nil,
) -> State {
  let print_say = fn(vm_) {
    vm_ |> saying |> result.map(printer) |> result.unwrap(Nil)
    vm_
  }
  // Calls next until Stopped or forever
  // Useful for automated test cases
  case vm |> needs_choice, vm |> needs_continue {
    [], False -> vm |> print_say
    [_, ..], _ -> {
      printer(
        vm
        |> needs_choice
        |> list.index_map(fn(c, index) {
          index |> int.add(1) |> int.to_string <> ": " <> c
        })
        |> string.join("\n"),
      )
      case inputs {
        [choice, ..rest] -> {
          printer(">>>> Choosing " <> choice |> int.to_string)
          vm
          |> choose(
            // User inputs are 1-indexed, VM is 0-indexed
            choice |> int.subtract(1),
          )
          |> print_say
          |> test_run(rest, printer)
        }
        [] -> {
          printer(">>>> No inputs left, but choice was requested")
          vm
        }
      }
    }
    _, True -> vm |> continue |> print_say |> test_run(inputs, printer)
  }
}

fn debug(vm: State, msg: String) -> Nil {
  case vm.trace_print {
    True -> io.println_error(msg)
    _ -> Nil
  }
}

fn run_one_instr(vm: State, i: Instruction) {
  vm
  |> debug(
    print_instrs(glearray.from_list([i]), vm.jump_table)
    <> "\nstack: "
    <> vm.stack |> list.map(print_op) |> string.join("; "),
  )

  case i {
    Push(op) -> State(..vm, ip: vm.ip + 1, stack: vm |> push(op))
    // TODO provide name to user as part of the public API
    ISay(name) ->
      State(
        ..vm,
        ip: vm.ip + 1,
        say: [nameify(name) <> vm |> top |> print_op, ..vm.say],
        stack: vm |> rest,
      )
    IWaitContinue -> State(..vm, ip: vm.ip + 1, state: WaitingOnContinue)
    IWaitChoice -> State(..vm, ip: vm.ip + 1, state: WaitingOnChoice)
    Pop -> State(..vm, ip: vm.ip + 1, stack: vm |> rest)
    Set(name) ->
      State(..vm |> set_var(name, vm |> top), ip: vm.ip + 1, stack: vm |> rest)
    Get(name) ->
      State(..vm, ip: vm.ip + 1, stack: vm |> push(vm |> get_var(name)))
    Binary(op) -> {
      case vm.stack {
        [a, b, ..rest] ->
          State(..vm, ip: vm.ip + 1, stack: [bin(a, b, op), ..rest])
        _ -> vm
      }
    }
    Native(op) ->
      State(
        ..vm,
        ip: vm.ip + 1,
        stack: vm |> rest |> pusha(vm |> pop |> native(op)),
      )
    Unary(op) ->
      State(
        ..vm,
        ip: vm.ip + 1,
        stack: vm |> rest |> pusha(vm |> pop |> un(op)),
      )
    // skip over labels, no-ops
    Label(_) -> State(..vm, ip: vm.ip + 1)
    JumpLabel(s) ->
      State(..vm, ip: vm.jump_table |> dict.get(s) |> result.unwrap(vm.ip + 1))
    JumpLabelIfFalse(s) ->
      State(
        ..vm,
        ip: if_false(
          vm |> top,
          // TODO improve error handling? no-op if bad label; no-op if top is false;
          {
            let i = vm.jump_table |> dict.get(s) |> result.unwrap(0)
            i
          },
          vm.ip + 1,
        ),
        stack: vm |> rest,
      )
    Jump(offset) -> State(..vm, ip: vm.ip + offset)
    CallTop ->
      State(..vm, ip: vm.ip + 1, stack: case vm.stack {
        [VString(name), arg, ..rest] -> [native(arg, name), ..rest]
        [_, e, ..] -> {
          echo "Warning, top of stack is not a string " <> e |> print_op
          vm.stack
        }
        _ -> vm.stack
      })
    JumpToTop ->
      State(..vm, ip: vm.ip + int_or(vm |> pop, 1), stack: vm |> rest)
    JumpIfFalse(offset) ->
      State(..vm, ip: vm.ip + if_false(vm |> top, offset, 1), stack: vm |> rest)
    JumpNode(n) -> vm |> jump_to_node(n) |> continue
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

fn bin(a_, b_, op: String) -> Operand {
  case op {
    "&&" -> VBool(as_bool(a_) && as_bool(b_))
    "==" -> VBool(a_ == b_)
    "!=" -> VBool(a_ != b_)
    ">" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VBool(b >. a)
        VString(a), VString(b) -> VBool(string.compare(a, b) == order.Gt)
        // TODO handle runtime errors from interpreter!
        _, _ -> todo as "> runtime error"
      }
    "<" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VBool(b <. a)
        VString(a), VString(b) -> VBool(string.compare(a, b) == order.Lt)
        // TODO handle runtime errors from interpreter!
        _, _ -> todo as "< runtime error"
      }
    "+" ->
      case a_, b_ {
        VFloat(a), VFloat(b) -> VFloat(b +. a)
        VString(a), VString(b) -> VString(b <> a)
        // TODO handle runtime errors from interpreter!
        _, _ -> todo as "+ runtime error"
      }
    _ -> todo as "unknown binary op"
  }
}

fn un(a_: Operand, op: String) -> Operand {
  case op {
    "!" -> VBool(!as_bool(a_))
    "~" -> VFloat(as_int(a_) |> int.bitwise_not |> int.to_float)
    _ -> todo as "unknown unary op"
  }
}

fn as_bool(o: Operand) -> Bool {
  case o {
    VBool(a) -> a
    VNil -> False
    VString("") -> False
    VFloat(0.0) -> False
    _ -> True
  }
}

fn as_int(o: Operand) {
  case o {
    VFloat(f) -> f |> float.round
    VString(s) -> s |> int.parse |> result.unwrap(0)
    VBool(True) -> 1
    VBool(False) -> 0
    VNil -> 0
  }
}

fn native(a_: Operand, op: String) -> Operand {
  case op {
    "str" -> VString(a_ |> print_op)
    "dice" -> VFloat(int.random(a_ |> as_int) |> int.add(1) |> int.to_float)
    _ -> todo as "unknown function"
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
      case index {
        i if i < 0 || i >= choices -> {
          echo "Warning! choice index out of bounds"
          vm
        }
        _ ->
          State(
            ..vm,
            state: WaitingOnContinue,
            say: [],
            stack: vm |> push(VFloat(index |> int.to_float)),
          )
          |> continue
      }
    _ -> {
      echo "Warning! it is a no-op to choose in this state"
      echo vm.state
      vm
    }
  }
}

pub fn continue(vm: State) -> State {
  case vm.state {
    WaitingOnContinue -> {
      let n = State(..vm, state: Running, say: []) |> next
      case n.state {
        // keep the last thing said at end state
        Stopped -> State(..n, say: vm.say)
        _ -> n
      }
    }
    _ -> {
      echo "Warning! it is a no-op to continue in this state"
      echo vm.state
      vm
    }
  }
}

pub fn goto_node(vm: State, new) -> State {
  State(..vm, node: new, ip: 0, state: WaitingOnContinue, say: [])
}

pub fn current_node(vm: State) -> String {
  vm.node
}

pub fn needs_choice(vm: State) -> List(String) {
  case vm.state {
    WaitingOnChoice -> vm.say |> list.reverse
    _ -> []
  }
}

pub fn jump_to_node(vm: State, node: String) {
  case vm.nodes |> dict.has_key(node) {
    False -> echo "Warning! node does not exist (" <> node <> ")"
    _ -> ""
  }
  State(..vm, node: node, say: [], stack: [], ip: 0, state: WaitingOnContinue)
}

pub fn needs_choice_arr(vm: State) -> Array(String) {
  vm |> needs_choice |> glearray.from_list
}

pub fn needs_continue(vm: State) -> Bool {
  vm.state == WaitingOnContinue
}

pub fn saying_list(vm: State) -> List(String) {
  case vm.state {
    WaitingOnChoice -> []
    Stopped -> []
    _ -> vm.say |> list.reverse
  }
}

pub fn saying(vm: State) -> Result(String, Nil) {
  case vm.state {
    WaitingOnChoice -> Error(Nil)
    Stopped -> Error(Nil)
    _ -> Ok(vm |> saying_list |> string.join("\n"))
  }
}

pub fn saying_or_empty(vm: State) -> String {
  vm |> saying |> result.unwrap("")
}

pub fn is_start(vm: State) -> Bool {
  vm.state == WaitingOnContinue && vm.say == [] && vm.ip == 0 && vm.stack == []
}

pub fn is_end(vm: State) -> Bool {
  vm.state == Stopped
}

pub fn node_names(vm: State) -> List(String) {
  vm.nodes |> dict.keys |> list.sort(string.compare)
}

pub fn debug_config(vm: State, debug_trace: Bool) -> State {
  State(..vm, trace_print: debug_trace)
}

pub fn set_filename(vm: State, filename: String) -> State {
  State(..vm, filename: filename)
}
