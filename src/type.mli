val conforms : Ast.program -> Ast.ty -> Ast.ty -> bool
val join : Ast.program -> Ast.ty -> Ast.ty -> Ast.ty

module ObjEnv : sig
  type t
  val get : Ast.program -> Ast.classe -> t
  val lookup : t -> Ast.id -> Ast.ty
  val extend : t -> (Ast.id * Ast.ty) list -> t
  val is_undefined : t -> Ast.id -> bool
end

module MethodEnv : sig
  type t
  val get : Ast.program -> t
  val lookup : t -> Ast.ty -> Ast.id -> Ast.ty list * Ast.ty
  val is_undefined : t -> Ast.ty -> Ast.id -> bool
end

val of_value : Ast.value -> Ast.ty
val of_expr :
  Ast.program -> ObjEnv.t -> MethodEnv.t -> Ast.expr -> Ast.ty
val check_feature : Ast.program -> MethodEnv.t -> Ast.feature -> unit
val check_class : Ast.program -> MethodEnv.t -> Ast.classe -> unit
val check_program : Ast.program -> unit
