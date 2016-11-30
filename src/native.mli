exception Error of Error.Eval.t'

val mk_method :
  Ast.Loc.t -> bool -> Ast.ty -> Ast.id -> (Ast.id * Ast.ty) list -> Ast.ty ->
  Ast.feature
