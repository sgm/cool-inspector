val program : Ast.program Gram.Entry.t
val illegal_keywords : string list
val expr_precedence :
  ?t:string -> string ->
  int * Camlp4.Sig.Grammar.assoc option * (bool * bool) option
val get_attr_expr : Ast.program -> Ast.feature -> Ast.expr
val is_attr_param : Ast.program -> Ast.feature -> bool
val parse_string :
  ?basic:Ast.program -> ?filename:string -> string -> Ast.program
val parse_file : ?basic:Ast.program -> string -> Ast.program
