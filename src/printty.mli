type options = {
  indent : int;
  newline_at_block : bool;
  intermediate_form : bool;
  basic : string option;
}

val default_options : options

module Format : sig
  val pp_print_value :
    Format.formatter -> ?options:options -> Ast.value -> unit
  val pp_print_expr :
    Format.formatter -> ?options:options -> Ast.program -> Ast.expr -> unit
  val pp_print_feature :
    Format.formatter -> ?options:options -> ?visible_ref:bool ref ->
    Ast.program -> Ast.feature -> unit
  val pp_print_class :
    Format.formatter -> ?options:options -> Ast.program -> Ast.classe -> unit
  val pp_print_program :
    Format.formatter -> ?options:options -> Ast.program -> unit

  val print_value : ?options:options -> Ast.value -> unit
  val print_expr : ?options:options -> 'a -> Ast.expr -> unit
  val print_feature : ?options:options -> Ast.program -> Ast.feature -> unit
  val print_class : ?options:options -> Ast.program -> Ast.classe -> unit
  val print_program : ?options:options -> Ast.program -> unit
end

val string_of_value : ?options:options -> Ast.value -> string
val string_of_expr : ?options:options -> 'a -> Ast.expr -> string
val string_of_feature :
  ?options:options -> Ast.program -> Ast.feature -> string
val string_of_class : ?options:options -> Ast.program -> Ast.classe -> string
val string_of_program : ?options:options -> Ast.program -> string

val print_value : ?options:options -> Ast.value -> unit
val print_expr : ?options:options -> 'a -> Ast.expr -> unit
val print_feature : ?options:options -> Ast.program -> Ast.feature -> unit
val print_class : ?options:options -> Ast.program -> Ast.classe -> unit
val print_program : ?options:options -> Ast.program -> unit
