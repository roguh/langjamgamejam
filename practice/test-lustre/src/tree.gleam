/// No imports and keep this file minimal.
/// The Abstract Syntax Tree (AST) for expressions and statements.
pub type Expr {
  Name(List(String))
  Int(Int, String)
  Float(Float)
  String(String)
  List(List(Expr))
  ParenList(List(Expr))
  Dict(List(#(Expr, Expr)))
  Nil
}

pub type Stmt {
  ExprStmt(Expr)
  Decl(List(String), Expr)
  Assign(List(String), Expr)
  Lambda(List(Expr), List(Stmt))
  Block(List(Stmt))
  If(Expr, List(Stmt), List(Stmt))
  While(Expr, List(Stmt))
}

pub type Program {
  Program(stmts: List(Stmt), types: Nil)
}

pub type CompilationArtifact =
  Result(
    Program,
    String,
    // TODO: Richer error type in the future? Improve atto parser?
  )
