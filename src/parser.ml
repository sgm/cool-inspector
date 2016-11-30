open Sig.Token
open Ast

let id_cons = mk_id ""
let attr_param_prefix = "'"
let build_class loc class_ty varformals super_ty actuals f_list =
  let assign attr expr =
    Expr_assign (loc_of_feature attr, feature_id attr, expr) in
  let hidden_id id =
    mk_id ~loc:(loc_of_id id) (attr_param_prefix ^ string_of_id id) in
  let feat_list = List.map ((|>) class_ty) f_list in
  let cons_id = id_of_ty class_ty in

  let (cons_body_list, old_features) =
    Support.List.partition_map
      (function
        | (Feat_method (_, _, id, _, _, Expr_block (_, b)), _)
          when eq_id id id_cons -> `A b
        | (feat, _) -> `B feat)
      feat_list in
  let cons_varformals =
    List.map (fun (var, ty) -> (hidden_id var, ty)) varformals in
  let varformals_attrs =
    List.map (fun (var, ty) -> Feat_attribute (loc, var, ty)) varformals in
  let has_native_attr =
    List.exists
      (function Feat_attribute(_, _, ty) -> eq_ty ty ty_native | _ -> false)
      old_features in
  let cons =
    if eq_ty super_ty ty_native || has_native_attr then
      Native.mk_method loc false class_ty cons_id cons_varformals class_ty
    else
    begin
      let attrs_init =
        Support.List.filter_map
          (function (attr, Some expr) -> Some (assign attr expr) | _ -> None)
          feat_list in
      let varformals_init =
        List.map2
          (fun (var, _) attr -> assign attr (Expr_var (loc, var)))
          cons_varformals
          varformals_attrs in

      let this = Expr_this (loc_of_ty class_ty) in
      let old_cons = List.flatten cons_body_list in
      let super_init =
        [Expr_dispatch (loc, this, id_of_ty super_ty, actuals)] in

      let cons_expr_list =
        varformals_init @ super_init @ attrs_init @ old_cons @ [this] in
      let cons_body = Expr_block (loc, cons_expr_list) in
      Feat_method (loc, false, cons_id, cons_varformals, class_ty, cons_body)
    end in
  let new_features = cons :: varformals_attrs @ old_features in
  mk_class loc class_ty super_ty new_features

let implicit_block = function
  | Expr_block (loc, []) -> Expr_unit loc
  | Expr_block (_, [e1]) -> e1
  | expr -> expr

(******************************************************************************)

let program = Gram.Entry.mk "program"
let expr = Gram.Entry.mk "expr"

EXTEND Gram
  GLOBAL:
    program expr;
  program:
    [ [ l = LIST1 classdecl; `EOI -> mk_program l ] ]
  ;
  classdecl:
    [ [ "class"; t = ty; f = varformals; cb = classbody ->
          build_class _loc t f ty_any [] cb
      | "class"; t1 = ty; f = varformals;
        "extends"; t2 = ty; a = actuals; cb = classbody ->
          build_class _loc t1 f t2 a cb
      | "class"; t = ty; f = varformals;
        "extends"; "native"; cb = classbody ->
          build_class _loc t f ty_native [] cb ] ]
  ;
  varformals:
    [ [ "("; l = LIST0 [ "var"; var = id_and_ty -> var ] SEP ","; ")" -> l ] ]
  ;
  classbody:
    [ [ "{"; l = LIST0 feature; "}" -> l ] ]
  ;
  feature:
    [ [ o = OPT "override"; "def"; i = id; f = formals; ":"; t = ty;
        "="; e = expr; ";" ->
          fun _ -> (Feat_method (_loc, o <> None, i, f, t, e), None)
      | o = OPT "override"; "def"; i = id; f = formals; ":"; t = ty;
        "="; "native"; ";" ->
          fun c -> (Native.mk_method _loc (o <> None) c i f t, None)
      | "var"; (i, t) = TRY [ id_and_ty ]; "="; e = expr; ";" ->
          fun _ -> (Feat_attribute (_loc, i, t), Some e)
      | "var"; i = id; "="; "native"; ";" ->
          fun _ -> (Feat_attribute (_loc, i, ty_native), None)
      | "{"; b = block; "}"; ";" ->
          fun c -> (Feat_method (_loc, false, id_cons, [], c, b), None) ] ]
  ;
  formals:
    [ [ "("; l = LIST0 id_and_ty SEP ","; ")" -> l ] ]
  ;
  actuals:
    [ [ "("; l = LIST0 expr SEP ","; ")" -> l ] ]
  ;
  block:
    [ [ l = LIST0 block_expr SEP ";" -> Expr_block (_loc, List.flatten l) ] ]
  ;
  expr:
    [ "=" LEFTA (* Lowest precedence *)
      [ i = TRY [ i = id; "=" -> i ]; e = SELF -> Expr_assign (_loc, i, e) ]
    | "if" NONA
      [ "if"; "("; e1 = SELF; ")"; e2 = SELF; "else"; e3 = SELF ->
          Expr_if (_loc, e1, e2, e3)
      | "while"; "("; e1 = SELF; ")"; e2 = SELF -> Expr_while (_loc, e1, e2) ]
    | "match" NONA
      [ e = SELF; "match"; c = cases -> Expr_match (_loc, e, c) ]
    | "<=" LEFTA
      [ e1 = SELF; "<="; e2 = SELF -> Expr_binary (_loc, e1, Op_le, e2)
      | e1 = SELF; "<"; e2 = SELF -> Expr_binary (_loc, e1, Op_less, e2) ]
    | "==" LEFTA
      [ e1 = SELF; loc = [ "==" -> _loc ]; e2 = SELF ->
          Expr_dispatch (_loc, e1, mk_id ~loc "equals", [e2]) ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> Expr_binary (_loc, e1, Op_plus, e2)
      | e1 = SELF; "-"; e2 = SELF -> Expr_binary (_loc, e1, Op_minus, e2) ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF -> Expr_binary (_loc, e1, Op_mul, e2)
      | e1 = SELF; "/"; e2 = SELF -> Expr_binary (_loc, e1, Op_div, e2) ]
    | "unary" NONA
      [ "!"; e = SELF -> Expr_unary (_loc, Op_not, e)
      | "-"; e = SELF -> Expr_unary (_loc, Op_neg, e) ]
    | "." LEFTA (* Highest precedence *)
      [ e = SELF; "."; i = id; a = actuals -> Expr_dispatch (_loc, e, i, a) ]
    | "primary" NONA
      [ primary ] ]
  ;
  primary:
    [ [ i = id; a = actuals ->
          Expr_dispatch (_loc, Expr_this _loc, i, a)
      | "super"; "."; i = id; a = actuals ->
          Expr_static_dispatch (_loc, i, a)
      | "new"; t = ty; a = actuals -> expr_cons ~loc:_loc t a
      | "{"; b = block; "}" -> b
      | "("; e = expr; ")" -> e
      | "null" -> Expr_null _loc
      | "("; ")" -> Expr_unit _loc
      | i = id -> Expr_var (_loc, i)
      | i = INTEGER -> Expr_int (_loc, int_of_string i)
      | s = STRING -> Expr_string (_loc, s)
      | "true" -> Expr_boolean (_loc, true)
      | "false" -> Expr_boolean (_loc, false)
      | "this" -> Expr_this _loc ] ]
  ;
  cases:
    [ [ "{"; l = LIST1 case; "}" ->  l ] ]
  ;
  block_expr:
    [ [ e = expr -> [e]
      | (loc, e, (i, t)) =
          [ "var"; var = id_and_ty; "="; e = expr; ";" -> (_loc, e, var) ];
        be = SELF ->
          Expr_block_var (loc, i, t, e) :: be ] ]
  ;
  case:
    [ [ "case"; (i, t) = id_and_ty; "=>"; b = block ->
          (_loc, i, t, implicit_block b)
      | "case"; loc = [ "null" -> _loc ]; "=>"; b = block ->
          (_loc, mk_id ~loc "null", cp_ty ~loc ty_null, implicit_block b) ] ]
  ;
  id:
    [ [ id = ID -> mk_id ~loc:_loc id ] ]
  ;
  ty:
    [ [ ty = TYPE -> mk_ty ~loc:_loc ty ] ]
  ;
  id_and_ty:
    [ [ i = id; ":"; t = ty -> (i, t) ] ]
  ;
END

let illegal_keywords = [
  "abstract"; "catch"; "do"; "final"; "finally"; "for"; "forSome"; "implicit";
  "import"; "lazy"; "object"; "package"; "private"; "protected"; "requires";
  "return"; "sealed"; "throw"; "trait"; "try"; "type"; "val"; "with"; "yield";
]
let () = Token.add_illegal_keywords illegal_keywords

(******************************************************************************)

let expr_precedence = Gram.precedence_of_entry "expr"

(* Returns the initialization expression of the attribute *)
let get_attr_expr p attr =
  let classe = get_feature_class p attr in
  let id = feature_id attr in
  let rec loop = function
  | Expr_assign (_, var, expr) :: tl when eq_id var id -> expr
  | _ :: tl -> loop tl
  | [] -> raise Not_found in
  match method_body (get_cons classe) with
  | Expr_block (_, block) -> loop block
  | Expr_native (_, f, _, ty) -> Expr_native (Loc.ghost, f, [], ty_nothing)
  | _ -> failwith "get_attr_expr"

(* Returns true if the attribute was created from the class parameters *)
let is_attr_param p attr =
  try
    let expr = get_attr_expr p attr in
    match expr with
      | Expr_var (_, var) ->
          let str_id = string_of_id var in
          let len_prefix = String.length attr_param_prefix in
          if String.length str_id <= len_prefix then
            false
          else
            String.sub str_id 0 len_prefix = attr_param_prefix
      | _ -> false
  with _ -> false

let parse_string ?basic ?filename s =
  let loc = match filename with Some s -> Loc.mk s | None -> Loc.ghost in
  let program = Gram.parse_string program loc s in
  match basic with Some p -> join_programs p program | None -> program

let parse_file ?basic filename =
  let in_channel = open_in filename in
  try
    let cs = Stream.of_channel in_channel in
    let program = Gram.parse program (Loc.mk filename) cs in
    close_in in_channel;
    match basic with Some p -> join_programs p program | None -> program
  with exn ->
    close_in in_channel;
    raise exn
