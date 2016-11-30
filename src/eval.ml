open Ast

module Error = Error.Eval

include Sig.Eval(Store)(Ast)

type 'a context = {
  program : program;
  loc : Loc.t;
  backtrace : Error.backtrace;
  data : 'a;
}

let empty_context data = {
  program = mk_program [];
  loc = Loc.ghost;
  backtrace = Error.empty_backtrace;
  data;
}

let program_of_context ctx = ctx.program
let loc_of_context ctx = ctx.loc
let backtrace_of_context ctx = ctx.backtrace
let context_with_program ctx program = { ctx with program }

let trace expr loc ctx =
  let backtrace = Error.push_call ctx.backtrace loc in
  { ctx with loc = loc_of_expr expr; backtrace }

module Mappings = struct
  let rec classe p ty =
    if eq_ty ty ty_native then [] else
      let class' = find_class p ty in
      let ty_s = class_super_type class' in
      classe p ty_s @ List.map
        (fun attr ->
          let ty_attr = attribute_type attr in
          (feature_id attr, ty_attr, default_value ty_attr))
        (class_attributes class')

  let rec implementation p ty m =
    let classe = find_class p ty in
    try
      let m' = find_method classe m in
      (ty, List.map fst (method_parameters m'), method_body m')
    with Not_found -> implementation p (class_super_type classe) m

  let super p ty =
    class_super_type (find_class p ty)
end

module type Functions = sig
  type t
  type u
  val empty_context : unit -> u context

  val classe : u context -> ty -> (id * ty * value) list
  val implementation : u context -> ty -> id -> ty * id list * expr
  val super : u context -> ty -> ty

  val env_lookup : u context -> env -> id -> Store.location
  val env_extend : u context -> env -> id -> Store.location -> env
  val env_extend_multi :
    u context -> env -> id list -> Store.location list -> env
  val store_lookup : u context -> store -> Store.location -> value
  val store_update : u context -> store -> Store.location -> value -> store
  val store_newloc : u context -> store -> Store.location
  val store_update_multi :
    u context -> store -> Store.location list -> value list -> store
  val store_newloc_multi : u context -> store -> int -> Store.location list

  val match_operator :
    u context -> value -> value -> op -> op list -> value -> unit

  val preamble : u context -> args -> unit
  val apply_rule : u context -> value -> store -> t
  val expr_eval_result : u context -> t -> value * store

  val handle_exception : u context -> exn -> t
end

module type S = sig
  type t
  type u
  val comp : u context -> value -> value -> op -> value option
  val arith : u context -> value -> value -> op -> value option
  val expr : u context -> ty -> value -> store -> env -> expr -> t
  val program : program -> t
end

module Make (F : Functions) : (S with type t = F.t and type u = F.u) = struct
  open F
  type t = F.t
  type u = F.u

  let error ctx err = Loc.raise ctx.loc (Error.E (err, ctx.backtrace))
  let no_rule_applies ctx = error ctx Error.No_rule_applies

  let binary_op ctx v1 v2 a b op f l =
    let operation =
      try Some (List.find (fun (op', _) -> op' = op) l)
      with Not_found -> None in
    match operation with
    | None -> None
    | Some (_, g) ->
        let (op_list, _) = List.split l in
        let v = f (g a b) in
        match_operator ctx v1 v2 op op_list v;
        Some v

  let comp ctx v1 v2 op =
    let calc a b f = binary_op ctx v1 v2 a b op f in
    match (v1, v2) with
    | (Val_int i1, Val_int i2) ->
        calc i1 i2 (fun b -> Val_boolean b)
          [
            (Op_less, ( <  ));
            (Op_le,   ( <= ));
          ]
    | _ -> None

  let arith ctx v1 v2 op =
    let calc a b f = binary_op ctx v1 v2 a b op f in
    match (v1, v2) with
    | (Val_int i1, Val_int i2) ->
        calc i1 i2 (fun i -> Val_int i)
          [
            (Op_plus,  ( + ));
            (Op_minus, ( - ));
            (Op_mul,   ( * ));
            (Op_div,   fun i1 i2 ->
                         if i2 = 0 || (i1 = min_int && i2 = ~-1) then
                           error ctx Error.Division_by_zero
                         else
                           i1 / i2)
          ]
    | _ -> None

  let rec expr ctx c so store env e = try
    let ctx = { ctx with loc = loc_of_expr e } in
    preamble ctx (c, so, store, env, e);
    match e with
    (* Assign *)
    | Expr_assign (loc, var, e1) ->
        let (v1, store2) = eval ctx c so store env e1 in
        let l1 = env_lookup ctx env var in
        let store3 = store_update ctx store2 l1 v1 in
        apply_rule ctx Val_unit store3
    (* Var *)
    | Expr_var (loc, var) ->
        let l = env_lookup ctx env var in
        let v = store_lookup ctx store l in
        apply_rule ctx v store
    (* Unit *)
    | Expr_unit loc ->
        apply_rule ctx Val_unit store
    (* Null *)
    | Expr_null loc ->
        apply_rule ctx Val_nil store
    (* This *)
    | Expr_this loc ->
        apply_rule ctx so store
    (* True, False *)
    | Expr_boolean (loc, b) ->
        apply_rule ctx (Val_boolean b) store
    (* Int *)
    | Expr_int (loc, i) ->
        apply_rule ctx (Val_int i) store
    (* String *)
    | Expr_string (loc, s) ->
        apply_rule ctx (Val_string s) store
    (* Dispatch *)
    | Expr_dispatch (loc, e0, f, args) ->
        (try
          let (v0, store1) = eval ctx c so store env e0 in
          if v0 = Val_nil then error ctx Error.Null_dispatch;
          let (rev_v_list, store_n1) = List.fold_left
            (fun (rev_v_list, store_i) ei ->
              let (vi, store_i1) = eval ctx c so store_i env ei in
              (vi :: rev_v_list, store_i1))
            ([], store1) args in
          let v_list = List.rev rev_v_list in
          let x = Type.of_value v0 in
          let (x', xs, en1) = implementation ctx x f in
          let xl_list = store_newloc_multi ctx store_n1 (List.length xs) in
          let store_n2 = store_update_multi ctx store_n1 xl_list v_list in
          let attrs = value_attrs v0 in
          let a_list = List.map fst attrs @ xs in
          let l_list = List.map snd attrs @ xl_list in
          let env_ax =
            env_extend_multi ctx (Environment.empty ()) a_list l_list in
          let (vn1, store_n3) =
            eval (trace en1 loc ctx) x' v0 store_n2 env_ax en1 in
          apply_rule ctx vn1 store_n3
        with Not_found -> no_rule_applies ctx)
    (* StaticDispatch *)
    | Expr_static_dispatch (loc, f, args) ->
        (try
          if so = Val_nil then error ctx Error.Null_dispatch;
          let (rev_v_list, store_n1) = List.fold_left
            (fun (rev_v_list, store_i) ei ->
              let (vi, store_i1) = eval ctx c so store_i env ei in
              (vi :: rev_v_list, store_i1))
            ([], store) args in
          let v_list = List.rev rev_v_list in
          let _x = Type.of_value so in
          let (c', xs, en1) = implementation ctx (super ctx c) f in
          let xl_list = store_newloc_multi ctx store_n1 (List.length xs) in
          let store_n2 = store_update_multi ctx store_n1 xl_list v_list in
          let attrs = value_attrs so in
          let a_list = List.map fst attrs @ xs in
          let l_list = List.map snd attrs @ xl_list in
          let env_ax =
            env_extend_multi ctx (Environment.empty ()) a_list l_list in
          let (vn1, store_n3) =
            eval (trace en1 loc ctx) c' so store_n2 env_ax en1 in
          apply_rule ctx vn1 store_n3
        with Not_found -> no_rule_applies ctx)
    (* New *)
    | Expr_new (loc, ty) ->
        let (v0, store1) =
          let class_t0 = classe ctx ty in
          let a_list = List.map (fun (ai, _, _) -> ai) class_t0 in
          let v_list = List.map (fun (_, _, vi) -> vi) class_t0 in
          let loc_list = store_newloc_multi ctx store (List.length class_t0) in
          let store1 = store_update_multi ctx store loc_list v_list in
          (Val_object (ty, List.combine a_list loc_list), store1) in
        apply_rule ctx v0 store1
    (* If-True, If-False *)
    | Expr_if (loc, e1, e2, e3) ->
        let (v1, store2) = eval ctx c so store env e1 in
        let (vi, store3) =
          match v1 with
          (* If-True *)
          | Val_boolean true  -> eval ctx c so store2 env e2
          (* If-False *)
          | Val_boolean false -> eval ctx c so store2 env e3
          (* TYPE ERROR *)
          | _ -> no_rule_applies ctx in
        apply_rule ctx vi store3
    (* Block-None *)
    | Expr_block (loc, []) ->
        apply_rule ctx Val_unit store
    (* Block-One *)
    | Expr_block (loc, [e1]) ->
        let (v1, store1) = eval ctx c so store env e1 in
        apply_rule ctx v1 store1
    (* Block-Var *)
    | Expr_block (loc, Expr_block_var (_, var, ty, e1) :: b) ->
        let (v1, store2) = eval ctx c so store env e1 in
        let l1 = store_newloc ctx store2 in
        let store3 = store_update ctx store2 l1 v1 in
        let env' = env_extend ctx env var l1 in
        let (v2, store4) = eval ctx c so store3 env' (Expr_block (loc, b)) in
        apply_rule ctx v2 store4
    (* Block-Expr *)
    | Expr_block (loc, e1 :: b) ->
        let (_v1, store1) = eval ctx c so store env e1 in
        let (vn, store2) = eval ctx c so store1 env (Expr_block (loc, b)) in
        apply_rule ctx vn store2
    (* Case-not-null, Case-null *)
    | Expr_match (loc, e0, l) ->
        let (v0, store2) = eval ctx c so store env e0 in
        let x = Type.of_value v0 in
        (try
          if eq_ty x ty_null then
            (* Case-null *)
            let (_, _, _, ei) =
              List.find (fun (_, _, ti, _) -> eq_ty ti ty_null) l in
            let (v1, store3) = eval ctx c so store2 env ei in
            apply_rule ctx v1 store3
          else
            (* Case-not-null *)
            let (_, vi, ti, ei) =
              List.find
                (fun (_, _, ti, _) -> Type.conforms ctx.program x ti)
                l in
            let l0 = store_newloc ctx store2 in
            let store3 = store_update ctx store2 l0 v0 in
            let env' = env_extend ctx env vi l0 in
            let (v1, store4) = eval ctx c so store3 env' ei in
            apply_rule ctx v1 store4
        with Not_found -> error ctx Error.No_match)
    (* Loop-True, Loop-False *)
    | Expr_while (loc, e1, e2) ->
        let (v1, store2) = eval ctx c so store env e1 in
        (match v1 with
        (* Loop-True *)
        | Val_boolean true ->
            let (_v2, store3) = eval ctx c so store2 env e2 in
            let (_, store4) = eval ctx c so store3 env e in
            apply_rule ctx Val_unit store4
        (* Loop-False *)
        | Val_boolean false ->
            apply_rule ctx Val_unit store2
        (* TYPE ERROR *)
        | _ -> no_rule_applies ctx)
    (* Not, Neg *)
    | Expr_unary (loc, op, e1) ->
        let (v, store2) = eval ctx c so store env e1 in
        (match (op, v) with
        (* Not *)
        | (Op_not, Val_boolean b) ->
            apply_rule ctx (Val_boolean (not b)) store2
        (* Neg *)
        | (Op_neg, Val_int i1) ->
            apply_rule ctx (Val_int ~-i1) store2
        (* TYPE ERROR *)
        | _ -> no_rule_applies ctx)
    (* Comp, Arith *)
    | Expr_binary (loc, e1, op, e2) ->
        let (v1, store2) = eval ctx c so store env e1 in
        let (v2, store3) = eval ctx c so store2 env e2 in
        let v =
          (* Comp *)
          match comp  ctx v1 v2 op with Some v -> v | _ ->
          (* Arith *)
          match arith ctx v1 v2 op with Some v -> v | _ ->
          (* TYPE ERROR *)
          no_rule_applies ctx in
        apply_rule ctx v store3
    (* - Additional rules - *)
    (* Native *)
    | Expr_native (loc, f, formals, ty) ->
        let actuals = List.map
          (fun (id, _) -> Store.lookup store (Environment.lookup env id))
          formals in
        let (v1, store1) =
          try f so actuals store env
          with Native.Error err -> error ctx err in
        apply_rule ctx v1 store1
    (* - Invalid expressions - *)
    (* ERROR - block_var outside of a block *)
    | Expr_block_var _ -> no_rule_applies ctx
  with exn ->
    let exn = match exn with
    | Out_of_memory -> (try error ctx Error.Heap_overflow with exn -> exn)
    | _ -> exn in
    handle_exception ctx exn
  and eval ctx c so store env e =
    let result = expr ctx c so store env e in
    expr_eval_result ctx result

  let program p =
    let ctx = { (empty_context ()) with program = p } in
    let store = Store.empty () in
    let env = Environment.empty () in
    expr ctx ty_native Val_nil store env (expr_cons ty_main [])
end

include Make
  (struct
    type t = value * store
    type u = unit
    let empty_context = empty_context

    let classe ctx = Mappings.classe ctx.program
    let implementation ctx = Mappings.implementation ctx.program
    let super ctx = Mappings.super ctx.program

    let env_lookup _ env id = Environment.lookup env id
    let env_extend _ env id l = Environment.extend env id l
    let env_extend_multi _ env id_list l_list =
      Environment.extend_multi env id_list l_list
    let store_lookup _ store l = Store.lookup store l
    let store_update _ store l v = Store.update store l v
    let store_newloc _ store = Store.newloc store
    let store_update_multi _ store l_list v_list =
      Store.update_multi store l_list v_list
    let store_newloc_multi _ store n = Store.newloc_multi store n

    let match_operator _ _ _ _ _ _ = ()

    let preamble _ _ = ()
    let apply_rule _ v store = (v, store)
    let expr_eval_result _ x = x

    let handle_exception _ exn = raise exn
  end)

module Rec_F = struct
  type t = tree * exn option
  type u = {
    mutable children : tree list;
    mutable prev_children : tree list list;
    mutable args : args list;
    mutable error : exn option;
  }

  let empty_context () =
    empty_context { children = []; prev_children = []; args = []; error = None }

  exception Halt

  let add_children ?(force = false) ctx node =
    if ctx.data.error = None || force then
      ctx.data.children <- node :: ctx.data.children

  let get_store ctx =
    match ctx.data.args with
    | [] -> Store.empty ()
    | (_, _, store, _, _) :: _ -> store

  let no_name = "???"
  let rule_name ?(children = []) = function
    | Expr_assign _ -> "Assign"
    | Expr_var _ -> "Var"
    | Expr_unit _ -> "Unit"
    | Expr_null _ -> "Null"
    | Expr_this _ -> "This"
    | Expr_boolean (_, true) -> "True"
    | Expr_boolean (_, false) -> "False"
    | Expr_int _ -> "Int"
    | Expr_string _ -> "String"
    | Expr_dispatch _ -> "Dispatch"
    | Expr_static_dispatch _ -> "StaticDispatch"
    | Expr_new _ -> "New"
    | Expr_if _ ->
        (match children with
        | Tree_node (_, _, (Val_boolean true, _), _) :: _ -> "If-True"
        | Tree_node (_, _, (Val_boolean false, _), _) :: _ -> "If-False"
        | _ -> "If-" ^ no_name)
    | Expr_block (_, []) -> "Block-None"
    | Expr_block (loc, [e1]) -> "Block-One"
    | Expr_block (loc, Expr_block_var _ :: _) -> "Block-Var"
    | Expr_block _ -> "Block-Expr"
    | Expr_match _ ->
        (match children with
        | Tree_node (_, _, (Val_failure, _), _) :: _ -> "Case-" ^ no_name
        | Tree_node (_, _, (v, _), _) :: _
          when eq_ty (Type.of_value v) ty_null -> "Case-null"
        | Tree_node _ :: _ -> "Case-not-null"
        | _ -> "Case-" ^ no_name)
    | Expr_while _ ->
        (match children with
        | Tree_node (_, _, (Val_boolean true, _), _) :: _ -> "Loop-True"
        | Tree_node (_, _, (Val_boolean false, _), _) :: _ -> "Loop-False"
        | _ -> "Loop-" ^ no_name)
    | Expr_unary (_, Op_not, _) -> "Not"
    | Expr_unary (_, Op_neg, _) -> "Neg"
    | Expr_binary (_, _, (Op_less | Op_le), _) -> "Comp"
    | Expr_binary (_, _, (Op_plus | Op_minus | Op_mul | Op_div), _) -> "Arith"
    | Expr_native (loc, f, formals, ty) -> "Native"
    | Expr_block_var _ -> no_name
    | Expr_unary _ -> no_name
    | Expr_binary _ -> no_name

  let get_children ctx =
    let children =
      List.filter ((<>) (Tree_error "")) (List.rev ctx.data.children) in
    let expr =
      match ctx.data.args with
      | [] -> assert false
      | (_, _, _, _, expr) :: _ -> expr in
    match expr with
    | Expr_int (_, i) ->
        let text = (string_of_int i) ^ " is an integer literal" in
        [Tree_misc text]
    | Expr_string (_, s) ->
        let lit = "\"" ^ Lexer.escape s ^ "\"" in
        let text1 = lit ^ " is a string literal" in
        let len = string_of_int (String.length s) in
        let text2 = len ^ " = length(" ^ lit ^ ")" in
        [Tree_misc text1; Tree_misc text2]
    | Expr_match (_, _, l) ->
        (match children with
        | Tree_node (_, _, (Val_failure, _), _) :: tl -> children
        | Tree_node (_, _, (v, _), _) as head :: tl ->
            let x = Type.of_value v in
            if eq_ty x ty_null then
              children
            else
              (try
                let (_, _, ti, _) =
                  List.find
                    (fun (_, _, ti, _) -> Type.conforms ctx.program x ti)
                    l in
                let text =
                  (string_of_ty ti) ^ " = first type T where " ^
                    (string_of_ty x) ^ " \xE2\x89\xA4 T" in
                head :: Tree_misc text :: tl
              with Not_found -> children)
        | _ -> children)
    | _ -> children

  let classe ctx ty =
    let class_ty = Mappings.classe (program_of_context ctx) ty in
    add_children ctx (Tree_class (ty, class_ty));
    class_ty

  let implementation ctx ty m =
    let impl = Mappings.implementation (program_of_context ctx) ty m in
    add_children ctx (Tree_implementation (ty, m, impl));
    impl

  let super ctx ty =
    let ty_s = Mappings.super (program_of_context ctx) ty in
    add_children ctx (Tree_super (ty, ty_s));
    ty_s

  let env_lookup ctx env id =
    let l = Environment.lookup env id in
    add_children ctx (Tree_env_lookup (env, id, l));
    l

  let env_extend ctx env id l =
    let env' = Environment.extend env id l in
    add_children ctx (Tree_env_extend (env, [(id, l)], env'));
    env'

  let env_extend_multi ctx env id_list l_list =
    let env' = Environment.extend_multi env id_list l_list in
    let idl_list = List.combine id_list l_list in
    if List.length idl_list > 0 then
      add_children ctx (Tree_env_extend (env, idl_list, env'));
    env'

  let store_lookup ctx store l =
    let v = Store.lookup store l in
    add_children ctx (Tree_store_lookup (store, l, v));
    v

  let store_update ctx store l v =
    let store' = Store.update store l v in
    add_children ctx (Tree_store_update (store, [(l, v)], store'));
    store'

  let store_newloc ctx store =
    let l = Store.newloc store in
    add_children ctx (Tree_store_newloc (store, 1, [l]));
    l

  let store_update_multi ctx store l_list v_list =
    let store' = Store.update_multi store l_list v_list in
    let lv_list = List.combine l_list v_list in
    if List.length lv_list > 0 then
      add_children ctx (Tree_store_update (store, lv_list, store'));
    store'

  let store_newloc_multi ctx store n =
    let l_list = Store.newloc_multi store n in
    if n > 0 then
      add_children ctx (Tree_store_newloc (store, n, l_list));
    l_list

  let match_operator ctx v1 v2 op op_list v3 =
    let set = String.concat ", " (List.map string_of_op op_list) in
    let text1 = string_of_op op ^ " \xE2\x88\x88 {" ^ set ^ "}" in
    add_children ctx (Tree_misc text1);
    let s =
      function
      | Val_int i -> string_of_int i
      | Val_boolean b -> if b then "true" else "false"
      | Val_string s -> s
      | v -> raise Exit in
    try
      let text2 = s v1 ^ " " ^ string_of_op op ^ " " ^ s v2 ^ " = " ^ s v3 in
      add_children ctx (Tree_misc text2)
    with Exit -> ()

  let preamble ctx args =
    if ctx.data.error <> None then raise Halt;
    ctx.data.prev_children <- ctx.data.children :: ctx.data.prev_children;
    ctx.data.children <- [];
    ctx.data.args <- args :: ctx.data.args

  let apply_rule ctx v store =
    let children = get_children ctx in
    let (name, args) =
      match ctx.data.args with
      | [] -> assert false
      | (_, _, _, _, expr) as args :: tl ->
          ctx.data.args <- tl;
          (rule_name ~children expr, args) in
    let result = Tree_node (name, args, (v, store), children) in
    ctx.data.children <-
      (match ctx.data.prev_children with
      | [] -> assert false
      | v :: tl -> ctx.data.prev_children <- tl; v);
    (result, ctx.data.error)

  let expr_eval_result ctx = function
    | (Tree_node (_, _, result, _) as node, _) ->
        add_children ~force:true ctx node;
        result
    | _ -> (Val_failure, get_store ctx)

  let handle_exception ctx exn =
    if exn <> Halt then
    begin
      let err =
        match exn with
        | Loc.Exc_located (_, (Error.E (err, _))) -> Some err
        | Error.E (err, _) -> Some err
        | _ -> None in
      let text =
        match err with
        | None -> "Internal error: " ^ Printexc.to_string exn
        | Some err -> Error.to_string (err, Error.empty_backtrace) in
      add_children ctx (Tree_error text);
      if ctx.data.error = None then ctx.data.error <- Some exn;
      apply_rule ctx Val_failure (get_store ctx)
    end
    else
      (Tree_error "", ctx.data.error)
end

module Rec = struct
  include Make(Rec_F)
  let empty_context = Rec_F.empty_context
  let extract = function
    | (Tree_node (_, _, result, _), _) -> result
    | _ -> failwith "extract"
end
