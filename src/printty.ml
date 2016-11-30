open Format
open Ast

type options = {
  indent : int;
  newline_at_block : bool;
  intermediate_form : bool;
  basic : string option;
}

let default_options = {
  indent = 2;
  newline_at_block = false;
  intermediate_form = false;
  basic = Some "basic.cool";
}

type dangle =
  | Dangle_none
  | Dangle_left
  | Dangle_right
  | Dangle_both

type precedence =
  | Prec_none
  | Prec_left of int
  | Prec_right of int
  | Prec_nonassoc of int * dangle

let is_prec_left prec =
  match prec with Prec_left _ -> true | _ -> false

let is_prec_right prec =
  match prec with Prec_right _ -> true | _ -> false

let get_prec_val = function
  | Prec_none -> 0
  | Prec_left p -> p
  | Prec_right p -> p
  | Prec_nonassoc (p, _) -> p

let get_prec_dangle = function
  | Prec_none -> Dangle_none
  | Prec_left _ -> Dangle_both
  | Prec_right _ -> Dangle_both
  | Prec_nonassoc (_, dangle) -> dangle

let as_prec_left prec =
  Prec_left (get_prec_val prec)

let as_prec_right prec =
  Prec_right (get_prec_val prec)

let prec_of_parser_level ?t level =
  let (prec_val, assoc, dangles) = Parser.expr_precedence ?t level in
  let dangle = match dangles with
  | None | Some (true, true) -> Dangle_both
  | Some (true, false) -> Dangle_left
  | Some (false, true) -> Dangle_right
  | Some (false, false) -> Dangle_none in
  match assoc with
  | None -> Prec_none
  | Some Camlp4.Sig.Grammar.NonA -> Prec_nonassoc (prec_val, dangle)
  | Some Camlp4.Sig.Grammar.RightA -> Prec_right prec_val
  | Some Camlp4.Sig.Grammar.LeftA -> Prec_left prec_val

let rec precedence ?(options = default_options) = function
  | Expr_assign _ -> prec_of_parser_level "="
  | Expr_if _ -> prec_of_parser_level ~t:"if" "if"
  | Expr_while _ -> prec_of_parser_level ~t:"while" "if"
  | Expr_match _ -> prec_of_parser_level "match"
  | Expr_binary (_, _, Op_le, _) -> prec_of_parser_level "<="
  | Expr_binary (_, _, Op_less, _) -> prec_of_parser_level "<="
  | Expr_dispatch (_, _, f, _)
    when not options.intermediate_form && string_of_id f = "equals" ->
      prec_of_parser_level "=="
  | Expr_binary (_, _, Op_plus, _) -> prec_of_parser_level "+"
  | Expr_binary (_, _, Op_minus, _) -> prec_of_parser_level "+"
  | Expr_binary (_, _, Op_mul, _) -> prec_of_parser_level "*"
  | Expr_binary (_, _, Op_div, _) -> prec_of_parser_level "*"
  | Expr_unary (_, Op_not, _) -> prec_of_parser_level ~t:"!" "unary"
  | Expr_unary (_, Op_neg, _) -> prec_of_parser_level ~t:"-" "unary"
  | Expr_dispatch (_, (Expr_new (_, ty) as e), f, _)
    when not options.intermediate_form && eq_id f (id_of_ty ty) ->
      precedence ~options e
  | Expr_dispatch _ -> prec_of_parser_level "."
  | _ -> Prec_none

let is_block = function
  | Expr_block (_, _) -> true
  | _ -> false

module Format = struct
  let rec pp_print_value ppf ?(options = default_options) v =
    let print_location l = pp_print_string ppf (Store.string_of_location l) in
    pp_open_box ppf options.indent;
      (match v with
      | Val_nil ->
          pp_print_string ppf "nil";
      | Val_object (ty, attrs) ->
          pp_print_string ppf (string_of_ty ty ^ "(");
          pp_print_cut ppf ();
          List.iteri
            (fun i (id, l) ->
              pp_print_string ppf (string_of_id id ^ " =");
              pp_print_space ppf ();
              print_location l;
              if i + 1 < List.length attrs then
              begin
                pp_print_string ppf ",";
                pp_print_space ppf ()
              end)
            attrs;
          pp_print_string ppf ")";
      | Val_unit ->
          pp_print_string ppf "Unit()";
      | Val_int i ->
          pp_print_string ppf "Int(";
          pp_print_cut ppf ();
          pp_print_int ppf i;
          pp_print_string ppf ")";
      | Val_boolean b ->
          pp_print_string ppf "Boolean(";
          pp_print_cut ppf ();
          pp_print_bool ppf b;
          pp_print_string ppf ")";
       | Val_string s ->
          pp_print_string ppf "String(";
          pp_print_cut ppf ();
          pp_print_string ppf "length =";
          pp_print_space ppf ();
          pp_print_int ppf (String.length s);
          pp_print_string ppf ",";
          pp_print_space ppf ();
          pp_print_string ppf ("\"" ^ (Lexer.escape s) ^ "\")");
      | Val_array_any loc_array ->
          pp_print_string ppf "Array(";
          pp_print_cut ppf ();
          pp_print_string ppf "length =";
          pp_print_space ppf ();
          pp_print_int ppf (Array.length loc_array);
          Array.iter
            (fun l ->
              pp_print_string ppf ",";
              pp_print_space ppf ();
              print_location l)
            loc_array;
          pp_print_string ppf ")";
      | Val_failure ->
          pp_print_string ppf "error");
    pp_close_box ppf ()

  let pp_print_expr ppf ?(options = default_options) p e =
    let obox b = pp_open_box ppf (if b then options.indent else 0) in
    let ohvbox b = pp_open_hvbox ppf (if b then options.indent else 0) in
    let cbox () = pp_close_box ppf () in
    let pstring s = pp_print_string ppf s in
    let pint i = pp_print_int ppf i in
    let pbool b = pp_print_bool ppf b in
    let break b = pp_print_break ppf 0 (if b then options.indent else 0) in
    let pspace b = pp_print_break ppf 1 (if b then options.indent else 0) in
    let paren s up down =
      let dangle = get_prec_dangle down in
      let is_ambiguous = (dangle = Dangle_both ||
                           (dangle = Dangle_left && is_prec_right up) ||
                           (dangle = Dangle_right && is_prec_left up)) in
      if is_ambiguous then
        let is_same_side = (is_prec_left up  && is_prec_left down ) ||
                           (is_prec_right up && is_prec_right down) in
        let (val_up, val_down) = (get_prec_val up, get_prec_val down) in
        let condition = val_up <> 0 && val_down <> 0 && (val_up > val_down ||
                          (val_up = val_down && not is_same_side)) in
        if condition then (pstring s; break false) in
    let pid id = pstring (string_of_id id) in
    let pty ty = pstring (string_of_ty ty) in

    let rec aux ?(implicit_block = false) open_boxes prec e =
      let prec_e = precedence ~options e in
      let oparen ?prec:(prec_e = prec_e) () = paren "(" prec prec_e in
      let cparen ?prec:(prec_e = prec_e) () = paren ")" prec prec_e in
      let pexpr o = aux o (prec_e) in
      let pexpr_0 o = aux o Prec_none in
      let pexpr_l ?(prec = prec_e) o = aux o (as_prec_left prec) in
      let pexpr_r ?(prec = prec_e) o = aux o (as_prec_right prec) in

      if open_boxes then (ohvbox false; obox true);
      (match e with
      (* Var *)
      | Expr_var (_loc, v) ->
          pid v;
      (* Assign *)
      | Expr_assign (_loc, v, e1) ->
          oparen ();
            pid v; pstring " ="; pspace false; pexpr false e1;
          cparen ();
      (* Unit *)
      | Expr_unit _loc ->
          pstring "()";
      (* True, False *)
      | Expr_boolean (_loc, b) ->
          pbool b;
      (* Int *)
      | Expr_int (_loc, i) ->
          pint i;
      (* String *)
      | Expr_string (_loc, s) ->
          pstring ("\"" ^ (Lexer.escape s) ^ "\"")
      (* Null *)
      | Expr_null _loc ->
          pstring "null"
      (* Constructor (surface syntax) *)
      | Expr_dispatch (_loc, (Expr_new (_, ty)), f, args)
        when not options.intermediate_form && eq_id f (id_of_ty ty) ->
          oparen ();
            pstring "new"; pspace false; pid f; pstring "("; break false;
              List.iteri
                (fun i e' ->
                  pexpr_0 false e';
                  if i < List.length args - 1 then
                  begin
                    pstring ",";
                    pspace false
                  end)
                args;
            pstring ")";
          cparen ();
      (* Equals (surface syntax) *)
      | Expr_dispatch (_loc, e1, f, [e2])
        when not options.intermediate_form && string_of_id f = "equals" ->
          oparen ();
            pexpr_l false e1;
            pspace false; pstring "=="; pspace false;
            pexpr_r false e2;
          cparen ();
      (* Dispatch *)
      | Expr_dispatch (_loc, e1, f, args) ->
          oparen ();
            (match e1 with
            | Expr_this(_) when not options.intermediate_form -> ()
            | _ -> pexpr true e1; pstring "."; break false);
            pid f; pstring "("; break false;
              List.iteri
                (fun i e' ->
                  pexpr_0 false e';
                  if i < List.length args - 1 then
                  begin
                    pstring ",";
                    pspace false
                  end)
                args;
            pstring ")";
          cparen ();
      (* SuperDispatch *)
      | Expr_static_dispatch (_loc, f, args) ->
          pstring "super."; break false;
          pid f; pstring "("; break false;
          List.iteri
            (fun i e' ->
              pexpr_0 false e';
              if i + 1 < List.length args then
              begin
                pstring ",";
                pspace false
              end)
            args;
          pstring ")";
      (* New *)
      | Expr_new (_loc, ty) ->
          pstring "new"; pspace false; pty ty;
      (* If *)
      | Expr_if (_loc, e1, e2, e3) ->
          oparen (); ohvbox false;
            obox true;
              pstring "if ("; pexpr_0 false e1; pstring ")";
            cbox ();
            if is_block e2 then obox false;
              pspace true;
              if not (is_block e2) then obox true;
              pexpr_0 false e2;
            cbox ();
            pspace false;
            pstring "else";
            if is_block e3 then obox false;
              pspace true;
              if not (is_block e3) then obox true;
              pexpr false e3;
            cbox ();
          cbox (); cparen ();
      (* Block *)
      | Expr_block (_loc, b) ->
          let psemicolon i = if i < List.length b - 1 then pstring ";" in
          cbox ();
            if not implicit_block then
            begin
              if options.newline_at_block then break false;
              pstring "{"
            end;
            List.iteri
              (fun i e' -> pspace true; pexpr_0 true e'; psemicolon i)
              b;
            if not implicit_block then
            begin
              pspace false;
              pstring "}"
            end;
          obox true;
      (* Block-var *)
      | Expr_block_var (_loc, v, ty, e1) ->
          pstring "var "; pid v; pstring " :"; pspace false;
          pty ty; pstring " ="; pspace false;
          pexpr_0 false e1;
      (* Match *)
      | Expr_match (_loc, e0, l) ->
          oparen ();
            pexpr false e0; pspace false; pstring "match"; pspace false;
            cbox ();
              if options.newline_at_block then break false;
              pstring "{";
              List.iter
                (fun (_, v, ty, e') ->
                  pspace true;
                  ohvbox false;
                    obox false;
                      pstring "case ";
                      if options.intermediate_form || string_of_id v <> "null"
                      then
                        (pid v; pstring " :"; pspace false; pty ty)
                      else
                        pid v;
                      pstring " =>";
                    cbox ();
                    obox false;
                      let (e_block, is_block) = match e' with
                      | Expr_block _ -> (e', true)
                      | _ -> (Expr_block (loc_of_expr e', [e']), false) in
                      let implicit_block =
                        not (options.intermediate_form && is_block) in
                      if not implicit_block then pspace true;
                      aux ~implicit_block false Prec_none e_block;
                    cbox ();
                  cbox ())
                l;
              pspace false;
              pstring "}";
            obox true;
          cparen ();
      (* Loop *)
      | Expr_while (_loc, e1, e2) ->
          oparen ();
            pstring "while ("; pexpr_0 false e1; pstring ")"; pspace false;
            pexpr_0 false e2;
          cparen ();
      (* Not, Neg *)
      | Expr_unary (_loc, op, e1) ->
          pstring (string_of_op op); pexpr false e1;
      (* Comp, Arith *)
      | Expr_binary (_loc, e1, op, e2) ->
          oparen ();
            pexpr_l false e1;
            pstring " "; pstring (string_of_op op); pspace false;
            pexpr_r false e2;
          cparen ();
      (* This *)
      | Expr_this _loc ->
          pstring "this";
      (* Native *)
      | Expr_native (_loc, f, formals, ty) ->
          pstring "native");
      if open_boxes then (cbox (); cbox ()) in
    aux false Prec_none e

  let pp_print_feature ppf ?(options = default_options) ?visible_ref p feat =
    let classe = get_feature_class p feat in
    let is_cons = not options.intermediate_form && is_cons p feat in
    let set_visible b = match visible_ref with None -> () | Some r -> r := b in
    set_visible true;
    match feat with
    | Feat_attribute (_loc, id, ty) ->
        pp_open_vbox ppf 0;
          pp_open_box ppf options.indent;
            let colon = if eq_ty ty ty_native then " =" else " :" in
            pp_print_string ppf ("var " ^ (string_of_id id) ^ colon);
            pp_print_space ppf ();
            pp_print_string ppf (string_of_ty ty);
            if not options.intermediate_form then
            begin
              let expr = Parser.get_attr_expr p feat in
              pp_print_string ppf " =";
              pp_print_space ppf ();
              pp_print_expr ppf ~options p expr
            end;
            pp_print_string ppf ";";
          pp_close_box ppf ();
        pp_close_box ppf ();
    | Feat_method (_loc, override, id, formals, ty, expr) when is_cons ->
        (match expr with
        | Expr_block (_, block) ->
            let rec discard_first_n n l =
              if n = 0 then l else discard_first_n (n - 1) (List.tl l) in
            let rec remove_last =
              function [] -> [] | [x] -> [] | h :: tl -> h :: remove_last tl in
            let init_length = List.length (class_attributes classe) +
              if eq_ty (class_super_type classe) ty_native then 0 else 1 in
            let block' = remove_last (discard_first_n init_length block) in
            if block' <> [] then
            begin
              pp_open_vbox ppf 0;
                pp_print_string ppf "{";
                List.iteri
                  (fun i expr' ->
                    let is_last = (i = List.length block' - 1) in
                    pp_print_break ppf 0 options.indent;
                    pp_open_hvbox ppf 0;
                      pp_open_box ppf options.indent;
                        pp_print_expr ppf ~options p expr';
                      pp_close_box ppf ();
                    pp_close_box ppf ();
                    if not is_last then pp_print_string ppf ";")
                  block';
                pp_print_cut ppf ();
                pp_print_string ppf "};";
              pp_close_box ppf ()
            end
            else
              set_visible false
        | _ ->
            set_visible false);
    | Feat_method (_loc, override, id, formals, ty, expr) ->
        pp_open_vbox ppf 0;
          pp_open_box ppf options.indent;
            if override then
            begin
              pp_print_string ppf "override";
              pp_print_space ppf ()
            end;
            pp_print_string ppf "def";
            pp_print_space ppf ();
            pp_print_string ppf (string_of_id id);
            pp_print_cut ppf ();
            pp_print_string ppf "(";
            List.iteri
              (fun i (id', ty') ->
                pp_print_string ppf
                  (string_of_id id' ^ " : " ^ string_of_ty ty');
                if i + 1 < List.length formals then
                begin
                  pp_print_string ppf ",";
                  pp_print_space ppf ()
                end)
              formals;
            pp_print_string ppf ") :";
            pp_print_space ppf ();
            pp_print_string ppf (string_of_ty ty ^ " =");
            pp_print_space ppf ();
            pp_print_expr ppf ~options p expr;
            pp_print_string ppf ";";
          pp_close_box ppf ();
        pp_close_box ppf ()

  let pp_print_class ppf ?(options = default_options) p classe =
    let ty_t = class_type classe in
    let ty_s = class_super_type classe in
    let features = class_features classe in
    pp_open_box ppf 0;
      pp_open_vbox ppf 0;
        pp_open_box ppf options.indent;
          pp_print_string ppf "class";
          pp_print_space ppf ();
          pp_print_string ppf (string_of_ty ty_t);
          if not options.intermediate_form then
          begin
            let str1 s = String.sub s 1 (String.length s - 1) in
            let varformals = method_parameters (get_cons classe) in
            pp_print_string ppf "(";
            List.iteri
              (fun i (id, ty) ->
                pp_print_string ppf ("var " ^
                  str1 (string_of_id id) ^ " : " ^ string_of_ty ty);
                if i + 1 < List.length varformals then
                  (pp_print_string ppf ",";
                  pp_print_space ppf ()))
              varformals;
            pp_print_string ppf ")"
          end;
          if options.intermediate_form || not (eq_ty ty_s ty_any) then
          begin
            pp_print_space ppf ();
            pp_print_string ppf "extends";
            pp_print_space ppf ();
            pp_print_string ppf (string_of_ty ty_s);
            if not options.intermediate_form then
            begin
              let _loc = Loc.ghost in
              let cons = get_cons classe in
              let dispatch = match method_body cons with
              | Expr_block (_, block) ->
                  List.nth block (List.length (method_parameters cons))
              | _ ->
                  Expr_dispatch (_loc, Expr_this _loc, mk_id "", []) in
              pp_print_string ppf "(";
              (match dispatch with
              | Expr_dispatch (_, _, _, args) ->
                  pp_print_cut ppf ();
                  List.iteri
                    (fun i expr ->
                      pp_print_expr ppf ~options p expr;
                      if i < List.length args - 1 then
                      begin
                        pp_print_string ppf ",";
                        pp_print_space ppf ()
                      end)
                    args;
              | _ -> ());
              pp_print_string ppf ")";
            end;
          end;
          if options.newline_at_block then pp_close_box ppf ();
          pp_print_space ppf ();
          pp_print_string ppf "{";
        if not options.newline_at_block then pp_close_box ppf ();
        pp_print_break ppf 0 options.indent;
        ignore
          (List.fold_left
            (fun (i, printed_prev, prev_was_attr) feat ->
              let visible_ref = ref printed_prev in
              let is_feat_attr = is_attribute feat in
              if options.intermediate_form ||
                not (Parser.is_attr_param p feat) then
              begin
                let print_breaks () = (not is_feat_attr) && !visible_ref in
                if prev_was_attr && print_breaks () then
                  pp_print_cut ppf ();
                if printed_prev then pp_print_break ppf 0 options.indent;
                pp_open_box ppf 0;
                  pp_print_feature ppf ~options ~visible_ref p feat;
                pp_close_box ppf ();
                if print_breaks () || i = List.length features - 1 then
                  pp_print_cut ppf ();
              end;
              (i + 1, !visible_ref, is_feat_attr))
            (0, false, false)
            features);
        pp_print_string ppf "}";
      pp_close_box ppf ();
    pp_close_box ppf ()

  let pp_print_program ppf ?(options = default_options) p =
    let class_list = program_classes p in
    pp_open_vbox ppf 0;
      List.iteri
        (fun i classe ->
          let filename = Loc.file_name (loc_of_class classe) in
          if Some filename <> options.basic then
          begin
            pp_print_class ppf ~options p classe;
            if i < List.length class_list - 1 then
            begin
              pp_print_cut ppf ();
              pp_print_cut ppf ();
            end
          end)
        class_list;
    pp_close_box ppf ()

  let print_value = pp_print_value std_formatter
  let print_expr ?options = pp_print_expr std_formatter ?options
  let print_feature = pp_print_feature std_formatter ?visible_ref:None
  let print_class = pp_print_class std_formatter
  let print_program = pp_print_program std_formatter
end

let pretty_str n f =
  let buff = Buffer.create n in
  let ppf = formatter_of_buffer buff in
  pp_open_box ppf 0; f ppf; pp_close_box ppf ();
  pp_print_flush ppf ();
  Buffer.contents buff
let string_of_value ?options v =
  pretty_str 16 (fun ppf -> Format.pp_print_value ppf ?options v)
let string_of_expr ?options p e =
  pretty_str 256 (fun ppf -> Format.pp_print_expr ppf ?options p e)
let string_of_feature ?options p feat =
  pretty_str 256 (fun ppf -> Format.pp_print_feature ppf ?options p feat)
let string_of_class ?options p classe =
  pretty_str 1024 (fun ppf -> Format.pp_print_class ppf ?options p classe)
let string_of_program ?options p =
  pretty_str 1024 (fun ppf -> Format.pp_print_program ppf ?options p)

let pr = Pervasives.print_string
let print_value ?options v = pr (string_of_value ?options v)
let print_expr ?options p e = pr (string_of_expr ?options p e)
let print_feature ?options p feat = pr (string_of_feature ?options p feat)
let print_class ?options p classe = pr (string_of_class ?options p classe)
let print_program ?options p = pr (string_of_program ?options p)
