include module type of Sig.Eval(Store)(Ast)

type 'a context

val program_of_context : 'a context -> Ast.program
val loc_of_context : 'a context -> Ast.Loc.t
val backtrace_of_context : 'a context -> Error.Eval.backtrace
val context_with_program : 'a context -> Ast.program -> 'a context

module Mappings : sig
  val classe : Ast.program -> Ast.ty -> (Ast.id * Ast.ty * Ast.value) list
  val implementation :
    Ast.program -> Ast.ty -> Ast.id -> Ast.ty * Ast.id list * Ast.expr
  val super : Ast.program -> Ast.ty -> Ast.ty
end

module type Functions = sig
  type t
  type u
  val empty_context : unit -> u context

  val classe : u context -> Ast.ty -> (Ast.id * Ast.ty * Ast.value) list
  val implementation :
    u context -> Ast.ty -> Ast.id -> Ast.ty * Ast.id list * Ast.expr
  val super : u context -> Ast.ty -> Ast.ty

  val env_lookup : u context -> Ast.env -> Ast.id -> Store.location
  val env_extend :
    u context -> Ast.env -> Ast.id -> Store.location -> Ast.env
  val env_extend_multi :
    u context -> Ast.env -> Ast.id list -> Store.location list -> Ast.env
  val store_lookup : u context -> Ast.store -> Store.location -> Ast.value
  val store_update :
    u context -> Ast.store -> Store.location -> Ast.value -> Ast.store
  val store_newloc : u context -> Ast.store -> Store.location
  val store_update_multi :
    u context -> Ast.store -> Store.location list -> Ast.value list -> Ast.store
  val store_newloc_multi :
    u context -> Ast.store -> int -> Store.location list

  val match_operator :
    u context -> Ast.value -> Ast.value -> Ast.op -> Ast.op list -> Ast.value ->
    unit

  val preamble : u context -> args -> unit
  val apply_rule : u context -> Ast.value -> Ast.store -> t
  val expr_eval_result : u context -> t -> Ast.value * Ast.store

  val handle_exception : u context -> exn -> t
end

module type S = sig
  type t
  type u
  val comp : u context -> Ast.value -> Ast.value -> Ast.op -> Ast.value option
  val arith : u context -> Ast.value -> Ast.value -> Ast.op -> Ast.value option
  val expr :
    u context -> Ast.ty -> Ast.value -> Ast.store -> Ast.env -> Ast.expr -> t
  val program : Ast.program -> t
end

module Make (F : Functions) : S with type u = F.u

include S with type t = Ast.value * Ast.store
val empty_context : unit -> u context

module Rec : sig
  include S with type t = tree * exn option
  val empty_context : unit -> u context
  val extract : t -> Ast.value * Ast.store
end
