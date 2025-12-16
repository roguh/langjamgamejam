import gleam/dict.{type Dict}
import gleam/option.{type Option}

pub type Operand {
  VArray(List(Operand))
  VString(String)
  VFloat(Float)
  VBool(Bool)
  VNil
}

// It'd be wise to stick close to the official TypeScript implementation
pub type ExecutionState {
  Stopped
  WaitingOnOptionSelection
  WaitingOnContinue
  Running
}

pub type VariableStorage =
  Dict(String, Operand)

pub type OptionItem {
  OptionItem(label: String, line: Int, jump: Int, is_available: Bool)
}

pub type CallSite {
  CallSite(node_name: String, instr: Int)
}

pub type MetadataEntry {
  MetadataEntry(
    id: String,
    node: String,
    line: Int,
    tags: List(String),
    other: Dict(String, String),
  )
}

pub type MetadataTable =
  Dict(String, MetadataEntry)

// TODO string interning (speedy lookup and comparison and de-dupe)
pub type StringTable =
  Dict(String, List(VMString))

// From yarn Go impl yarn_spinnger.pb.go
// jump to (to position encoded in the instruction)
// jump (to string from stack)
// run line
// run command
// add option
// show options
// push string
// push float
// push bool
// push null
// jump if false
// pop
// call func
// push var
// store var
// stop
// run node
// instructions are encoded as bytes
// floats and bools and float/bool arrays are encoded as bytes
// strings are in a separate table
pub type Instruction

// From yarn Go impl yarn_spinnger.pb.go
// map label to position in the instructions
// location of a node
pub type Node {
  Node(
    name: String,
    instructions: List(Instruction),
    headers: List(#(String, String)),
  )
}

pub type Program {
  Program(
    name: String,
    nodes: Dict(String, Node),
    initial_values: Dict(String, Operand),
  )
}

pub type YarnFunction =
  fn(List(Operand)) -> Result(Operand, String)

// From YarnSpinner-TypeScript:
pub type VM {
  VM(
    current_node: Option(Node),
    // TODO array not linked-list eek
    program: Program,
    stack: List(Operand),
    state: ExecutionState,
    callstack: List(CallSite),
    pc: Int,
    variables: VariableStorage,
    option_set: List(OptionItem),
    library: Dict(String, YarnFunction),
  )
}

pub type VMString {
  Variable(Int)
  Literal(String)
}

fn top(vm: VM) {
  todo
}

/// Pick an option
/// It's an error to pick an option that is out-of-bounds or to pick an option when not in the select-option state
pub fn select_option(vm: VM, number: Int) {
  todo
}

pub fn jump_to_node(vm: VM, node_id: Int) {
  todo
}
