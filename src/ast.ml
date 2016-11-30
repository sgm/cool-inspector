module Loc = Token.Loc

type id_impl = Loc.t * string

let mk_id ?(loc = Loc.ghost) s = (loc, s)
let mk_ty ?(loc = Loc.ghost) s = (loc, s)
let cp_id ?(loc = Loc.ghost) (_, s) = (loc, s)
let cp_ty ?(loc = Loc.ghost) (_, s) = (loc, s)
let loc_of_id (loc, _) = loc
let loc_of_ty (loc, _) = loc
let id_of_ty ty = ty

let string_of_id (_, s) = s
let string_of_ty (_, s) = s
let eq_id (_, s1) (_, s2) = (s1 = s2)
let eq_ty (_, s1) (_, s2) = (s1 = s2)
let is_id_in id_list id = List.exists (eq_id id) id_list
let is_ty_in ty_list ty = List.exists (eq_ty ty) ty_list

module Environment = Environment.Make
  (struct
    type t = id_impl
    let equal = eq_id
    let hash id = Hashtbl.hash (string_of_id id)
  end)

include Sig.Ast(Loc)(Store)(Environment)
  (struct
    type id = id_impl
    type ty = id_impl
  end)

type classe = Loc.t * ty * ty * feature list
type program = classe list

let ty_any       = mk_ty "Any"
let ty_io        = mk_ty "IO"
let ty_unit      = mk_ty "Unit"
let ty_int       = mk_ty "Int"
let ty_boolean   = mk_ty "Boolean"
let ty_string    = mk_ty "String"
let ty_array_any = mk_ty "ArrayAny"
let ty_symbol    = mk_ty "Symbol"
let ty_null      = mk_ty "Null"
let ty_nothing   = mk_ty "Nothing"
let ty_native    = mk_ty "native"
let ty_main      = mk_ty "Main"

let built_in_types = [ty_null; ty_nothing]
let value_classes = [ty_boolean; ty_int; ty_unit]
let basic_classes =
  [ty_any; ty_io; ty_string; ty_array_any; ty_symbol] @ value_classes
let uninstantiable_classes = [ty_any; ty_symbol] @ value_classes
let final_classes = [ty_string; ty_array_any; ty_symbol] @ value_classes

let string_of_op = function
  | Op_not -> "!"
  | Op_neg -> "-"
  | Op_le -> "<="
  | Op_less -> "<"
  | Op_plus -> "+"
  | Op_minus -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"

(* expr type inspection functions *)
external loc_of_expr : expr -> Loc.t = "%field0"

(* feature type inspection functions *)
external loc_of_feature : feature -> Loc.t = "%field0"
let is_attribute = function Feat_attribute _ -> true | _ -> false
let is_method = function Feat_method _ -> true | _ -> false
let is_native_attribute =
  function Feat_attribute (_, _, ty) -> eq_ty ty ty_native | _ -> false

let feature_id = function
  | Feat_attribute (_, id, _) -> id
  | Feat_method (_, _, id, _, _, _) -> id

let attribute_type = function
  | Feat_attribute (_, _, ty) -> ty
  | _ -> invalid_arg "attribute_type"

let method_override = function
  | Feat_method (_, override, _, _, _, _) -> override
  | _ -> invalid_arg "method_override"
let method_parameters = function
  | Feat_method (_, _, _, params, _, _) -> params
  | _ -> invalid_arg "method_parameters"
let method_return_type = function
  | Feat_method (_, _, _, _, ty, _) -> ty
  | _ -> invalid_arg "method_arguments"
let method_body = function
  | Feat_method (_, _, _, _, _, expr) -> expr
  | _ -> invalid_arg "method_body"

(* classe type inspection functions *)
let mk_class loc ty super features = (loc, ty, super, features)
let loc_of_class (loc, _, _, _) = loc
let class_type (_, ty, _, _) = ty
let class_super_type (_, _, ty_s, _) = ty_s
let class_features (_, _, _, feat_list) = feat_list
let class_attributes c = List.filter is_attribute (class_features c)
let class_methods c = List.filter is_method (class_features c)

(* program type inspection functions *)
let mk_program classes = classes
let program_classes p = p
let join_programs p1 p2 = p1 @ p2

(* Default values *)
let default_value ty =
  if eq_ty ty ty_int      then  Val_int 0          else
  if eq_ty ty ty_unit     then  Val_unit           else
  if eq_ty ty ty_boolean  then  Val_boolean false  else
                                Val_nil

(* Value attributes ids and locations *)
let value_attrs = function
  | Val_object (_, attrs) -> attrs
  | _ -> []

(* Find class *)
let find_class p ty =
  List.find (fun classe -> eq_ty (class_type classe) ty) (program_classes p)

(* Find method *)
let find_method classe id =
  List.find (fun m -> eq_id (feature_id m) id) (class_methods classe)

(* Returns super class *)
let super_class p classe =
  find_class p (class_super_type classe)

(* Returns the class which the feature pertains to *)
let get_feature_class p feat =
  List.find
    (fun classe ->
      let rec loop = function
      | feat' :: tl -> feat == feat' || loop tl
      | [] -> false
      in loop (class_features classe))
    (program_classes p)

let get_cons classe =
  find_method classe (id_of_ty (class_type classe))

let is_cons p feat =
  is_method feat &&
    (eq_id (feature_id feat) (id_of_ty (class_type (get_feature_class p feat))))

let expr_cons ?(loc = Loc.ghost) ty args =
  Expr_dispatch (loc, Expr_new (loc, ty), id_of_ty ty, args)

(*
let encode s =
  s (* TODO *)

let string_of_op_comp = function
  | Op_comp_le -> "<="
  | Op_comp_less -> "<"

let string_of_op_arith = function
  | Op_arith_plus -> "+"
  | Op_arith_minus -> "-"
  | Op_arith_mul -> "*"
  | Op_arith_div -> "/"

let rec string_of_expr p e = "(" ^ (String.concat " "
  (match e with
  (* Var *)
  | Expr_var (_, v) ->
      ["Variable"; v];
  (* Assign *)
  | Expr_assign (_, v, e1) ->
      ["Assignment"; v; string_of_expr p e1]
  (* Unit *)
  | Expr_unit _ ->
      ["Constant"; "()"; "Unit"]
  (* True, False *)
  | Expr_boolean (_, b) ->
      ["Constant"; string_of_bool b; "Boolean"]
  (* Int *)
  | Expr_int (_, i) ->
      ["Constant"; string_of_int i; "Int"]
  (* String *)
  | Expr_string (_, s) ->
      ["Constant"; encode s ^ "\""; "String"]
  (* Null *)
  | Expr_null _ ->
      ["Constant"; "null"; "Null"]
  (* Dispatch *)
  | Expr_dispatch (_, e1, f, args) ->
      ["Dispatch"; string_of_expr p e1; f] @ List.map (string_of_expr p) args
  (* SuperDispatch *)
  | Expr_static_dispatch (_, f, args) ->
      ["Dispatch"; "super"; f] @ List.map (string_of_expr p) args
  (* New *)
  | Expr_new (_, ty) ->
      ["New"; ty]
  (* If *)
  | Expr_if (_, e1, e2, e3) ->
      ["If"; string_of_expr p e1; string_of_expr p e2; string_of_expr p e3]
  (* Block *)
  | Expr_block (_, b) ->
      ["Block"] @ List.map (string_of_expr p) b
  (* Block-var *)
  | Expr_block_var (_, v, ty, e1) ->
      ["Var"; v; ty; string_of_expr p e1]
  (* Match *)
  | Expr_match (_, e0, l) ->
      ["Match"; string_of_expr p e0] (* TODO *)
  (* Loop *)
  | Expr_while (_, e1, e2) ->
      ["While"; string_of_expr p e1; string_of_expr p e2]
  (* Not *)
  | Expr_not (_, e1) ->
      ["!"; string_of_expr p e1]
  (* Comp *)
  | Expr_comp (_, e1, op, e2) ->
      [string_of_op_comp op; string_of_expr p e1; string_of_expr p e2]
  (* Neg *)
  | Expr_neg (_, e1) ->
      ["-"; string_of_expr p e1]
  (* Arith *)
  | Expr_arith (_, e1, op, e2) ->
      [string_of_op_arith op; string_of_expr p e1; string_of_expr p e2]
  (* This *)
  | Expr_this _ ->
      ["this"]
  (* Native *)
  | Expr_native (_, f, args, ty) ->
      ["native"];
)) ^ ")"

let string_of_feature p feat = "(" ^ (String.concat " "
  (match feat with
  | Feat_attribute (_, x, ty) ->
      ["Attribute"; x; ty]
  | Feat_method (_, override, id, params, ty0, expr) ->
      ["Method"; id] @
      List.map (fun (id, ty) -> id ^ " " ^ ty) params @
      [ty0; string_of_expr p expr]
)) ^ ")"

let string_of_class p classe = "(" ^ (String.concat " " (
  [class_type classe; class_super_type classe] @
  List.map (string_of_feature p) (class_features classe)
)) ^ ")"

let string_of_program p = "(" ^ (String.concat " " (
  List.map (string_of_class p) (program_classes p)
)) ^ ")"
*)
