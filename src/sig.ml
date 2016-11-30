module Token = struct
  type t =
    | KEYWORD of string
    | ILLEGAL of string
    | SYMBOL of string
    | TYPE of string
    | ID of string
    | INTEGER of string
    | STRING of string
    | COMMENT of string
    | BLANKS of string
    | NEWLINE
    | EOI
end

module Ast
  (Loc : Camlp4.Sig.Loc)
  (Store : sig type location type 'a t end)
  (Environment : sig type t end)
  (S : sig
    type id
    type ty
  end) =
struct
  type id = S.id
  type ty = S.ty

  type value =
    | Val_nil
    | Val_object of ty * (id * Store.location) list
    | Val_unit
    | Val_int of int
    | Val_boolean of bool
    | Val_string of string
    | Val_array_any of Store.location array
    | Val_failure

  type env = Environment.t
  type store = value Store.t

  type op =
    | Op_not
    | Op_neg
    | Op_less
    | Op_le
    | Op_plus
    | Op_minus
    | Op_mul
    | Op_div

  type native_method = value -> value list -> store -> env -> value * store
  type expr =
    | Expr_assign          of Loc.t * id * expr
    | Expr_var             of Loc.t * id
    | Expr_unit            of Loc.t
    | Expr_null            of Loc.t
    | Expr_this            of Loc.t
    | Expr_boolean         of Loc.t * bool
    | Expr_int             of Loc.t * int
    | Expr_string          of Loc.t * string
    | Expr_dispatch        of Loc.t * expr * id * expr list
    | Expr_static_dispatch of Loc.t * id * expr list
    | Expr_new             of Loc.t * ty
    | Expr_if              of Loc.t * expr * expr * expr
    | Expr_block           of Loc.t * expr list
    | Expr_block_var       of Loc.t * id * ty * expr
    | Expr_match           of Loc.t * expr * (Loc.t * id * ty * expr) list
    | Expr_while           of Loc.t * expr * expr
    | Expr_unary           of Loc.t * op * expr
    | Expr_binary          of Loc.t * expr * op * expr
    | Expr_native          of Loc.t * native_method * (id * ty) list * ty

  type feature =
    | Feat_attribute of Loc.t * id * ty
    | Feat_method    of Loc.t * bool * id * (id * ty) list * ty * expr
end

module Eval
  (Store : sig type location end)
  (Ast : sig type id and ty and env and store and value and expr end) =
struct
  type args = Ast.ty * Ast.value * Ast.store * Ast.env * Ast.expr
  type tree =
    | Tree_node           of string * args * (Ast.value * Ast.store) * tree list
    | Tree_class          of Ast.ty * (Ast.id * Ast.ty * Ast.value) list
    | Tree_implementation of Ast.ty * Ast.id * (Ast.ty * Ast.id list * Ast.expr)
    | Tree_super          of Ast.ty * Ast.ty
    | Tree_env_lookup     of Ast.env * Ast.id * Store.location
    | Tree_env_extend     of Ast.env * (Ast.id * Store.location) list * Ast.env
    | Tree_store_lookup   of Ast.store * Store.location * Ast.value
    | Tree_store_update   of
        Ast.store * (Store.location * Ast.value) list * Ast.store
    | Tree_store_newloc   of Ast.store * int * Store.location list
    | Tree_misc           of string
    | Tree_error          of string
end
