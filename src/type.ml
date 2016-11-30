open Ast

module Error = Error.Type
let error loc err = Loc.raise loc (Error.E err)

(* Conformation relation between types *)
let rec conforms p ty1 ty2 =
  if eq_ty ty1 ty2 then true else
  if eq_ty ty1 ty_any then false else
  if eq_ty ty1 ty_nothing then true else
  if eq_ty ty2 ty_any then true else
  if eq_ty ty2 ty_nothing then false else
  if eq_ty ty2 ty_null then false else
  if eq_ty ty1 ty_null then not (is_ty_in value_classes ty2) else
    conforms p (class_super_type (find_class p ty1)) ty2

(* LUB (least upper bound) operation on types *)
let join p ty1 ty2 =
  if conforms p ty1 ty2 then ty2 else
  if conforms p ty2 ty1 then ty1 else
    let rec get_ancestors l ty =
      let ty_s = class_super_type (find_class p ty) in
      if eq_ty ty_s ty_native then l else get_ancestors (ty_s :: l) ty_s
    in
    try
      let ancestors_ty1 = List.filter (conforms p ty2) (get_ancestors [] ty1) in
      let ancestors_ty2 = List.filter (conforms p ty1) (get_ancestors [] ty2) in
      List.find (is_ty_in ancestors_ty1) (List.rev ancestors_ty2)
    with Not_found -> failwith "join"

module ObjEnv = struct
  type t = (id * ty) list

  (* Get object environment from class *)
  let rec get p classe =
    let obj attr = (feature_id attr, attribute_type attr) in
    let o = List.map obj (class_attributes classe) in
    let ty_s = class_super_type classe in
    o @ if eq_ty ty_s ty_native then [] else get p (find_class p ty_s)

  (* Get type of variable from object environment *)
  let lookup o v =
    snd (List.find (fun (id, _) -> eq_id v id) o)

  (* Merges two object environments *)
  let extend old_o new_o =
    new_o @ old_o

  (* Returns whether a variable is undefined in the object environment or not *)
  let is_undefined o v =
    try
      ignore (lookup o v);
      false
    with Not_found ->
      true
end

module MethodEnv = struct
  type t = ((ty * id) * (ty list * ty)) list

  let get p =
    let classes = program_classes p in
    let rec methods primary classe =
      let c = class_type primary in
      let c' = class_super_type classe in
      List.map
        (fun methode ->
          let id = feature_id methode in
          let ty_params = List.map snd (method_parameters methode) in
          let ty = method_return_type methode in
          ((c, id), (ty_params, ty)))
        (class_methods classe) @
        if eq_ty c' ty_native then [] else methods primary (find_class p c')
    in
    List.flatten (List.map (fun classe -> methods classe classe) classes)

  (* Get types of method (arguments and return value) *)
  let lookup m ty f =
    snd (List.find (fun ((ty', f'), _) -> eq_ty ty ty' && eq_id f f') m)

  (* Returns whether a method is undefined in the methods mapping or not *)
  let is_undefined m ty f =
    try
      ignore (lookup m ty f);
      false
    with Not_found ->
      true
end

(* Type of values *)
let of_value = function
  | Val_nil -> ty_null
  | Val_object (ty, _) -> ty
  | Val_unit -> ty_unit
  | Val_int _ -> ty_int
  | Val_boolean _ -> ty_boolean
  | Val_string _ -> ty_string
  | Val_array_any _ -> ty_array_any
  | Val_failure -> ty_native

(* Type of expression *)
let rec of_expr p o m e =
  match e with
  (* Var *)
  | Expr_var (loc, v) ->
      (try
        ObjEnv.lookup o v
      with Not_found ->
        error (loc_of_id v) (Error.Undeclared_id v))
  (* Assign *)
  | Expr_assign (loc, v, e1) ->
      (try
        let t = ObjEnv.lookup o v in
        let t' = of_expr p o m e1 in
        if conforms p t' t
          then ty_unit
          else error (loc_of_expr e1) (Error.Mismatch (t', t))
      with Not_found ->
        error (loc_of_id v) (Error.Undeclared_id v))
  (* Unit *)
  | Expr_unit loc ->
      cp_ty ~loc ty_unit
  (* True, False *)
  | Expr_boolean (loc, b) ->
      cp_ty ~loc ty_boolean
  (* Int *)
  | Expr_int (loc, i) ->
      cp_ty ~loc ty_int
  (* String *)
  | Expr_string (loc, s) ->
      cp_ty ~loc ty_string
  (* Null *)
  | Expr_null loc ->
      cp_ty ~loc ty_null
  (* Dispatch *)
  | Expr_dispatch (loc, e0, f, args) ->
      let t0 = of_expr p o m e0 in
      (try
        let (t'_list, tn1) = MethodEnv.lookup m t0 f in
        let la = List.length args and lf = List.length t'_list in
        if la = lf then
        begin
          List.iter2
            (fun ei ti' ->
              let ti = of_expr p o m ei in
              if not (conforms p ti ti') then
                error (loc_of_expr ei) (Error.Mismatch (ti, ti')))
            args t'_list;
          tn1
        end else
          error loc (Error.Wrong_number_of_arguments (t0, f, la, lf))
      with Not_found ->
        error (loc_of_id f) (Error.Undeclared_method (t0, f)))
  (* SuperDispatch *)
  | Expr_static_dispatch (loc, f, args) ->
      let super =
        let s = "super" in
        let super_loc = Loc.move `stop (String.length s)
          (Loc.of_lexing_position (Loc.start_pos loc)) in
        mk_id ~loc:super_loc s in
      (try
        let t0 = ObjEnv.lookup o super in
        try
          let (t'_list, tn1) = MethodEnv.lookup m t0 f in
          let la = List.length args and lf = List.length t'_list in
          if la = lf then
          begin
            List.iteri
              (fun i (ei, ti') ->
                let ti = of_expr p o m ei in
                if not (conforms p ti ti') then
                  error (loc_of_expr ei) (Error.Mismatch (ti, ti')))
              (List.combine args t'_list);
            tn1
          end else
            error loc (Error.Wrong_number_of_arguments (t0, f, la, lf))
        with Not_found ->
          error (loc_of_id f) (Error.Undeclared_method (t0, f))
      with Not_found ->
        error (loc_of_id super) (Error.Undeclared_id super))
  (* New *)
  | Expr_new (loc, t) ->
      if not (is_ty_in built_in_types t) then
        if not (is_ty_in uninstantiable_classes t) then
          try ignore (find_class p t); t
          with Not_found -> error (loc_of_ty t) (Error.Undeclared_class t)
        else
          error loc (Error.Invalid_instantiation t)
      else
        error loc (Error.Non_class_instantiation t)
  (* If *)
  | Expr_if (loc, e0, e1, e2) ->
      let t0 = of_expr p o m e0 in
      if eq_ty t0 ty_boolean then
        let t1 = of_expr p o m e1 in
        let t2 = of_expr p o m e2 in
        try join p t1 t2 with _ -> error loc (Error.Join_failure (t1, t2))
      else
        error (loc_of_expr e0) (Error.Mismatch (t0, ty_boolean))
  (* Block-None *)
  | Expr_block (loc, []) ->
      cp_ty ~loc ty_unit
  (* Block-One *)
  | Expr_block (loc, [e]) ->
      of_expr p o m e
  (* Block-Local *)
  | Expr_block (loc, (Expr_block_var (loc', x, t, e1)) :: b) ->
      if ObjEnv.is_undefined o x then
        let t0 = of_expr p o m e1 in
        if conforms p t0 t then
          of_expr p (ObjEnv.extend o [(x, t)]) m (Expr_block (loc, b))
        else error (loc_of_expr e1) (Error.Mismatch (t0, t))
      else error (loc_of_ty t) (Error.Invalid_shadowing x)
  (* Block-Expr *)
  | Expr_block (loc, e1 :: b) ->
      let _t1 = of_expr p o m e1 in
      of_expr p o m (Expr_block (loc, b))
  (* Match *)
  | Expr_match (loc, e0, l) ->
      let t0 = of_expr p o m e0 in
      let rec test_branches = function
      | (loc', xi, ti, _) :: tl ->
          (* Test for type shadowing *)
          List.iter
            (fun (_, _, tj, _) ->
              if (not (eq_ty tj ty_null) && conforms p tj ti) ||
                (eq_ty tj ty_null && eq_ty ti ty_null)
              then
                error loc' (Error.Match_subsumption (ti, tj)))
            tl;
          (* Test if ti is a class type *)
          if is_ty_in built_in_types ti && string_of_id xi <> "null" then
            error (loc_of_ty ti) (Error.Match_non_class ti)
          else
            (* Test for conformity with t0 *)
            if not (conforms p t0 ti || conforms p ti t0) then
              error loc' (Error.Match_mismatch (ti, t0));
          test_branches tl
      | [] -> () in
      test_branches l;
      List.fold_left
        (fun ti1' (loc', xi, ti, ei) ->
          let ti' = of_expr p (ObjEnv.extend o [(xi, ti)]) m ei in
          try join p ti1' ti' with _ ->
            error loc' (Error.Join_failure (ti1', ti')))
        ty_nothing l
  (* Loop *)
  | Expr_while (loc, e1, e2) ->
      let t1 = of_expr p o m e1 in
      if eq_ty t1 ty_boolean
        then let _t2 = of_expr p o m e2 in ty_unit
        else error (loc_of_expr e1) (Error.Mismatch (t1, ty_boolean))
  (* Not *)
  | Expr_unary (loc, Op_not, e1) ->
      let t1 = of_expr p o m e1 in
      if eq_ty t1 ty_boolean
        then ty_boolean
        else error (loc_of_expr e1) (Error.Mismatch (t1, ty_boolean))
  (* Comp *)
  | Expr_binary (loc, e1, (Op_less | Op_le), e2) ->
      let t1 = of_expr p o m e1 in
      if eq_ty t1 ty_int then
        let t2 = of_expr p o m e2 in
        if eq_ty t2 ty_int then
          ty_boolean
        else error (loc_of_expr e2) (Error.Mismatch (t2, ty_int))
      else error (loc_of_expr e1) (Error.Mismatch (t1, ty_int))
  (* Neg *)
  | Expr_unary (loc, Op_neg, e1) ->
      let t1 = of_expr p o m e1 in
      if eq_ty t1 ty_int
        then ty_int
        else error (loc_of_expr e1) (Error.Mismatch (t1, ty_int))
  (* Arith *)
  | Expr_binary (loc, e1, (Op_plus | Op_minus | Op_mul | Op_div), e2) ->
      let t1 = of_expr p o m e1 in
      if eq_ty t1 ty_int then
        let t2 = of_expr p o m e2 in
        if eq_ty t2 ty_int then
          ty_int
        else error (loc_of_expr e2) (Error.Mismatch (t2, ty_int))
      else error (loc_of_expr e1) (Error.Mismatch (t1, ty_int))
  (* Native *)
  | Expr_native (loc, f, formals, ty) ->
      ty
  (* - Additional rules - *)
  (* This - the specification treats the keywords "this" and "super" as
      variables during type checking; this rule converts the keyword "this" to a
      variable and checks its type *)
  | Expr_this loc ->
      of_expr p o m (Expr_var (loc, mk_id ~loc "this"))
  (* - Invalid expressions - *)
  (* ERROR - block_var outside of a block *)
  | Expr_block_var (loc, v, ty, e1) ->
      error loc Error.Unexpected_var
  (* ERROR - invalid unary operator *)
  | Expr_unary (loc, op, e1) ->
      error loc (Error.Operator_mismatch op)
  (* ERROR - invalid binary operator *)
  | Expr_binary (loc, e1, op, e2) ->
      error loc (Error.Operator_mismatch op)

let check_feature p m feat =
  match feat with
  (* Attr *)
  | Feat_attribute (loc, x, t) ->
      if not (Parser.is_attr_param p feat) then
      begin
        if eq_ty t ty_nothing then
          error loc (Error.Attribute_nothing feat);
        let classe = get_feature_class p feat in
        let oc = ObjEnv.get p classe in
        let oc' = ObjEnv.get p (super_class p classe) in
        if not (ObjEnv.is_undefined oc' x) then
          error (loc_of_id x)
            (Error.Feature_override (class_super_type classe, feat));
        let t = ObjEnv.lookup oc x in
        let e = Parser.get_attr_expr p feat in
        let t' = of_expr p oc m e in
        if not (conforms p t' t) then
          error (loc_of_expr e) (Error.Mismatch (t', t))
      end
  (* Method *)
  | Feat_method (loc, false, id, formals, t0, expr) ->
      let classe = get_feature_class p feat in
      let c = class_type classe in
      let c' = class_super_type classe in
      if not (MethodEnv.is_undefined m c' id) then
        error (loc_of_id id) (Error.Feature_override (c', feat));
      let oc = ObjEnv.get p classe in
      let this = mk_id "this" and super = mk_id "super" in
      let t =
        let oc_ext = [(this, c); (super, c')] @ formals in
        of_expr p (ObjEnv.extend oc oc_ext) m expr in
      if not (conforms p t t0) then
        error (loc_of_expr expr) (Error.Mismatch (t, t0))
  (* Method-Override *)
  | Feat_method (loc, true, id, formals, t0, expr) ->
      let classe = get_feature_class p feat in
      let c = class_type classe in
      let c' = class_super_type classe in
      let t_list = List.map snd formals in
      let (t_list', t0') = MethodEnv.lookup m c' id in
      if List.length t_list <> List.length t_list' ||
        not (List.for_all2 eq_ty t_list t_list')
      then
        error loc (Error.Override_parameters_mismatch feat);
      if not (conforms p t0 t0') then
        error loc (Error.Override_return_mismatch feat);
      let oc = ObjEnv.get p classe in
      let this = mk_id "this" and super = mk_id "super" in
      let t =
        let oc_ext = [(this, c); (super, c')] @ formals in
        of_expr p (ObjEnv.extend oc oc_ext) m expr in
      if not (conforms p t t0) then
        error (loc_of_expr expr) (Error.Mismatch (t, t0))

let check_class p m classe =
  let ty_s = class_super_type classe in
  if is_ty_in final_classes ty_s then
    error (loc_of_class classe) (Error.Disallowed_inheritance (classe, ty_s));
  if not (is_ty_in [ty_any; ty_native] ty_s) then
    (match method_body (get_cons (find_class p ty_s)) with
    | Expr_native _ ->
        error (loc_of_class classe) (Error.Native_superclass (classe, ty_s))
    | _ -> ());
  let features = class_features classe in
  let rec loop = function
  | [] -> ()
  | feat :: tl ->
      List.iter
        (fun feat' ->
          let name' = feature_id feat' and name = feature_id feat in
          if is_method feat = is_method feat' && eq_id name name' then
            error (loc_of_feature feat')
              (Error.Feature_redefinition (class_type classe, feat, feat')))
        tl;
      check_feature p m feat;
      loop tl in
  loop features

let check_program p =
  let class_list = program_classes p in
  let aux_type_list = ref [] in
  let rec check_hierarchy_for_cycles i classe =
    let ty = class_type classe and ty_s = class_super_type classe in
    if not (eq_ty ty_s ty_native) then
      try
        let (i', _) = List.find (fun (_, ty') -> eq_ty ty ty') !aux_type_list in
        if i = i' then
          error (loc_of_class classe) (Error.Hierarchy_cycle classe)
      with Not_found ->
        aux_type_list := (i, ty) :: !aux_type_list;
        check_hierarchy_for_cycles i (find_class p ty_s) in
  let rec loop = function
  | [] -> ()
  | classe :: tl ->
      List.iter
        (fun classe' ->
          if eq_ty (class_type classe') (class_type classe) then
            error (loc_of_class classe')
              (Error.Class_redefinition (classe', classe)))
        tl;
      check_class p (MethodEnv.get p) classe;
      loop tl in
  List.iteri check_hierarchy_for_cycles class_list;
  loop class_list
