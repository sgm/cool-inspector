module Loc : Camlp4.Sig.Loc with type t = Token.Loc.t

module Environment : Environment.S

include module type of Sig.Ast(Loc)(Store)(Environment)
  (struct
    type id = Environment.id
    type ty
  end)

type classe
type program

val mk_id : ?loc:Loc.t -> string -> id
val mk_ty : ?loc:Loc.t -> string -> ty
val cp_id : ?loc:Loc.t -> id -> id
val cp_ty : ?loc:Loc.t -> ty -> ty
val loc_of_id : id -> Loc.t
val loc_of_ty : ty -> Loc.t
val id_of_ty : ty -> id

val string_of_id : id -> string
val string_of_ty : ty -> string
val eq_id : id -> id -> bool
val eq_ty : ty -> ty -> bool
val is_id_in : id list -> id -> bool
val is_ty_in : ty list -> ty -> bool

val ty_any : ty
val ty_io : ty
val ty_unit : ty
val ty_int : ty
val ty_boolean : ty
val ty_string : ty
val ty_array_any : ty
val ty_symbol : ty
val ty_null : ty
val ty_nothing : ty
val ty_native : ty
val ty_main : ty

val built_in_types : ty list
val value_classes : ty list
val basic_classes : ty list
val uninstantiable_classes : ty list
val final_classes : ty list

val string_of_op : op -> string

external loc_of_expr : expr -> Loc.t = "%field0"

external loc_of_feature : feature -> Loc.t = "%field0"
val is_attribute : feature -> bool
val is_method : feature -> bool
val is_native_attribute : feature -> bool
val feature_id : feature -> id
val attribute_type : feature -> ty
val method_override : feature -> bool
val method_parameters : feature -> (id * ty) list
val method_return_type : feature -> ty
val method_body : feature -> expr

val mk_class : Loc.t -> ty -> ty -> feature list -> classe
val loc_of_class : classe -> Loc.t
val class_type : classe -> ty
val class_super_type : classe -> ty
val class_features : classe -> feature list
val class_attributes : classe -> feature list
val class_methods : classe -> feature list

val mk_program : classe list -> program
val program_classes : program -> classe list
val join_programs : program -> program -> program

val default_value : ty -> value
val value_attrs : value -> (id * Store.location) list

val find_class : program -> ty -> classe
val find_method : classe -> id -> feature
val super_class : program -> classe -> classe
val get_feature_class : program -> feature -> classe

val get_cons : classe -> feature
val is_cons : program -> feature -> bool

val expr_cons : ?loc:Loc.t -> ty -> expr list -> expr
