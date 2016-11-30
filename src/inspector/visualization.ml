open Cairo

module Environment = Ast.Environment

let str_id id = Ast.string_of_id id
let str_ty ty = Ast.string_of_ty ty

let compress s =
  let result = ref "" in
  let blank = ref true in
  String.iter
    (function
    | ' ' | '\n' | '\r' | '\t' ->
      if not !blank then (result := !result ^ " "; blank := true)
    | c -> result := !result ^ (Printf.sprintf "%c" c); blank := false)
    s;
  let len = String.length !result in
  if len <= 75 then
    !result
  else
    String.sub !result 0 60 ^ "[...]" ^
      String.sub !result (len - 10) 10

let do_intersect ((ax0, ay0), (ax1, ay1)) ((bx0, by0), (bx1, by1)) =
  let intersect_x =
    (ax0 >= bx0 && ax0 <= bx1) ||
    (ax1 >= bx0 && ax1 <= bx1) ||
    (ax0 <= bx0 && ax1 >= bx1) in
  let intersect_y =
    (ay0 >= by0 && ay0 <= by1) ||
    (ay1 >= by0 && ay1 <= by1) ||
    (ay0 <= by0 && ay1 >= by1) in
  intersect_x && intersect_y

module Ast = struct
  type construct =
  | Construct_expr of Ast.expr
  | Construct_int of int
  | Construct_bool of bool
  | Construct_string of string
  | Construct_id of Ast.id
  | Construct_type of Ast.ty
  | Construct_case of (Ast.id * Ast.ty * Ast.expr)
  | Construct_op of Ast.op

  let string_of_expr = function
  | Ast.Expr_assign _ -> "assign"
  | Ast.Expr_var _ -> "var"
  | Ast.Expr_unit _ -> "unit"
  | Ast.Expr_null _ -> "null"
  | Ast.Expr_this _ -> "this"
  | Ast.Expr_boolean _ -> "boolean"
  | Ast.Expr_int _ -> "int"
  | Ast.Expr_string _ -> "string"
  | Ast.Expr_dispatch _ -> "dispatch"
  | Ast.Expr_static_dispatch _ -> "static-dispatch"
  | Ast.Expr_new _ -> "new"
  | Ast.Expr_if _ -> "if"
  | Ast.Expr_block _ -> "block"
  | Ast.Expr_block_var _ -> "block-var"
  | Ast.Expr_match _ -> "match"
  | Ast.Expr_while _ -> "while"
  | Ast.Expr_unary _ -> "unary-operator"
  | Ast.Expr_binary _ -> "binary-operator"
  | Ast.Expr_native _ -> "native"

  let string_of_construct = function
  | Construct_expr expr -> string_of_expr expr
  | Construct_int i -> string_of_int i
  | Construct_bool b -> string_of_bool b
  | Construct_string s -> "\"" ^ Lexer.escape s ^ "\""
  | Construct_id id -> str_id id
  | Construct_type ty -> str_ty ty
  | Construct_case _ -> "case"
  | Construct_op op -> Ast.string_of_op op

  let exprs = List.map (fun expr -> Construct_expr expr)
  let children_of_expr = function
  | Ast.Expr_assign (_, id, e) -> [Construct_id id; Construct_expr e]
  | Ast.Expr_var (_, id) -> [Construct_id id]
  | Ast.Expr_unit _ -> []
  | Ast.Expr_null _ -> []
  | Ast.Expr_this _ -> []
  | Ast.Expr_boolean (_, b) -> [Construct_bool b]
  | Ast.Expr_int (_, i) -> [Construct_int i]
  | Ast.Expr_string (_, s) -> [Construct_string s]
  | Ast.Expr_dispatch (_, e, m, args) ->
      Construct_expr e :: Construct_id m :: exprs args
  | Ast.Expr_static_dispatch (_, _, args) -> exprs args
  | Ast.Expr_new (_, ty) -> [Construct_type ty]
  | Ast.Expr_if (_, e1, e2, e3) -> exprs [e1; e2; e3]
  | Ast.Expr_block (_, b) -> exprs b
  | Ast.Expr_block_var (_, id, ty, e) ->
      [Construct_id id; Construct_type ty; Construct_expr e]
  | Ast.Expr_match (_, e, cases) ->
      Construct_expr e ::
        List.map (fun (_, id, ty, e) -> Construct_case (id, ty, e)) cases
  | Ast.Expr_while (_, e1, e2) -> exprs [e1; e2]
  | Ast.Expr_unary (_, op, e) -> [Construct_op op; Construct_expr e]
  | Ast.Expr_binary (_, e1, op, e2) -> Construct_op op :: exprs [e1; e2]
  | Ast.Expr_native _ -> []

  let children_of_construct = function
  | Construct_expr expr -> children_of_expr expr
  | Construct_int _ -> []
  | Construct_bool _ -> []
  | Construct_string _ -> []
  | Construct_id _ -> []
  | Construct_type _ -> []
  | Construct_case (id, ty, expr) -> [Construct_id id; Construct_type ty; Construct_expr expr]
  | Construct_op _ -> []

  type tree = {
    node_text : string;
    text_x : float; text_y : float;
    node_x : float; node_y : float;
    node_width : float; node_height : float;
    full_width : float; full_height : float;
    anchor_x : float; anchor_y : float;
    construct : construct;
    children : ((float * float) * tree) list;
  }

  let color_of_construct = function
  | Construct_expr _   -> ((0.0, 0.0, 0.0), (1.0, 0.8, 0.4))
  | Construct_int _    -> ((1.0, 1.0, 1.0), (0.0, 0.0, 0.6))
  | Construct_bool _   -> ((1.0, 1.0, 1.0), (0.0, 0.0, 0.6))
  | Construct_string _ -> ((1.0, 1.0, 1.0), (0.0, 0.0, 0.6))
  | Construct_id _     -> ((1.0, 1.0, 1.0), (0.0, 0.6, 0.0))
  | Construct_type _   -> ((1.0, 1.0, 1.0), (0.0, 0.6, 0.0))
  | Construct_case _   -> ((0.0, 0.0, 0.0), (0.4, 0.6, 1.0))
  | Construct_op _     -> ((1.0, 1.0, 1.0), (0.6, 0.0, 0.0))

  let draw cr ?bounds ?(corner_radius = 6.0) tree =
    let r = corner_radius in
    let rec aux node (x0, y0) =
      (* Draw edges and children *)
      let num_children = List.length node.children in
      let (xa, ya) = (x0 +. node.anchor_x, y0 +. node.anchor_y) in
      List.iteri
        (fun i ((cx, cy), child) ->
          let (cx, cy) = (x0 +. cx, y0 +. cy) in
          let (xb, yb) = (cx +. child.anchor_x, cy +. child.anchor_y) in
          let y2 = 0.5 *. (ya +. yb) in
          let d = copysign (y2 -. ya) (xb -. xa) in
          move_to cr xa ya;
          if abs_float (xb -. xa) < 2.0 *. abs_float d then
            curve_to cr xa (0.5 *. (yb +. y2)) xb (0.5 *. (ya +. y2)) xb yb
          else begin
            if i = 0 || i = num_children - 1 then
            begin
              curve_to cr xa y2 (xa +. d) y2 (xa +. d) y2;
              line_to cr (xb -. d) y2;
            end else
              move_to cr (xb -. d) y2;
            curve_to cr (xb -. d) y2 xb y2 xb yb;
          end;
          set_source_rgb cr 0.0 0.0 0.0;
          set_line_width cr 1.0;
          stroke cr;
          let do_draw = match bounds with
          | None -> true
          | Some bounds ->
              let cx' = cx +. child.full_width in
              let cy' = cy +. child.full_height in
              do_intersect ((cx, cy), (cx', cy')) bounds in
          if do_draw then aux child (cx, cy))
        node.children;
      (* Draw rounded nodes *)
      let (x, y) = (x0 +. node.node_x, y0 +. node.node_y) in
      let (w, h) = (node.node_width, node.node_height) in
      let ((text_r, text_g, text_b), (node_r, node_g, node_b)) =
        color_of_construct node.construct in
      move_to cr (x +. r) y;
      line_to cr (x +. w -. r) y;
      curve_to cr (x +. w) y (x +. w) y (x +. w) (y +. r);
      line_to cr (x +. w) (y +. h -. r);
      curve_to cr (x +. w) (y +. h) (x +. w) (y +. h) (x +. w -. r) (y +. h);
      line_to cr (x +. r) (y +. h);
      curve_to cr x (y +. h) x (y +. h) x (y +. h -. r);
      line_to cr x (y +. r);
      curve_to cr x y x y (x +. r) y;
      set_line_width cr 2.0;
      set_source_rgb cr 0.0 0.0 0.0;
      stroke_preserve cr;
      set_source_rgb cr node_r node_g node_b;
      fill cr;
      (* Draw text *)
      move_to cr (x0 +. node.text_x) (y0 +. node.text_y);
      set_source_rgb cr text_r text_g text_b;
      show_text cr node.node_text in
    save cr;
    aux tree (0.0, 0.0);
    restore cr

  let generate ?(text_margin = 2.0)
               ?(node_margin = 8.0)
               ?(vertical_padding = 12.0)
               ?(adjustment_ratio = 0.25)
               ?(adjustment_threshold = 8.0)
               cr expr =
    let rec gen construct =
      let node_text = string_of_construct construct in
      let fe = font_extents cr in
      let te = text_extents cr node_text in
      let text_width = te.width in
      let text_height = fe.ascent +. fe.descent (* fe.baseline *) in
      let children = List.map gen (children_of_construct construct) in
      let num_children = List.length children in
      let (xs, anchors, w, h, a_sum) =
        List.fold_left
          (fun (xs, anchors, w, h, a_sum) node ->
            let anchor = w +. node.anchor_x in
            let a_sum = a_sum +. anchor in
            let new_w = w +. node.full_width in
            let h = max h node.full_height in
            (w :: xs, anchor :: anchors, new_w, h, a_sum))
          ([], [], 0.0, 0.0, 0.0)
          children in
      let node_width = 2.0 *. text_margin +. text_width in
      let node_height = 2.0 *. text_margin +. text_height in
      let child_y = node_height +. node_margin +.
        if (num_children > 0) then vertical_padding else node_margin in
      let anchors_mean =
        if (num_children > 0)
        then a_sum /. (float num_children)
        else 0.5 *. node_width +. node_margin in

      let distances = List.sort
        (fun a b -> compare (abs_float a) (abs_float b))
        (List.map ((-.) anchors_mean) anchors) in
      let adjust d1 d2 =
        let a1 = abs_float d1 and a2 = abs_float d2 in
        if a1 < adjustment_threshold then d1
        else if a1 < adjustment_ratio *. (a2 -. a1) then d1
        else 0.0 in
      let aligned_anchor = anchors_mean -. match distances with
      | [] -> 0.0
      | 0.0 :: _ -> 0.0
      | [d] -> adjust d 0.0
      | d1 :: tl ->
          try
            let d2 = List.find (fun d -> compare d 0.0 <> compare d1 0.0) tl in
            adjust d1 d2
          with Not_found -> adjust d1 0.0 in
      let left = aligned_anchor -. 0.5 *. node_width in
      let offset_x = if left < node_margin then node_margin -. left else 0.0 in

      let node_x = left +. offset_x in
      let node_y = node_margin in
      let anchor_x = aligned_anchor +. offset_x in
      let anchor_y = node_y +. 0.5 *. node_height in
      let text_x = node_x +. text_margin -. te.x_bearing in
      let text_y = node_y +. text_margin +. fe.ascent in
      let full_width =
        max (w +. offset_x) (node_x +. node_width +. node_margin) in
      let full_height = child_y +. h in
      let children_x = List.rev_map (fun x -> x +. offset_x) xs in
      let children_y = List.map (fun _ -> child_y) children_x in
      {
        node_text;
        text_x; text_y;
        node_x; node_y;
        node_width; node_height;
        full_width; full_height;
        anchor_x; anchor_y;
        construct;
        children = List.combine (List.combine children_x children_y) children;
      } in
    gen (Construct_expr expr)
end

module BigStep = struct
  type name = {
    name_text : string;
    name_x : float; name_y : float;
    name_width : float;
    name_x_bearing : float;
  }

  type tree = {
    node_text : string;
    text_x : float; text_y : float;
    text_width : float;
    draw_bar : bool;
    bar_x : float; bar_y : float;
    bar_width : float;
    name : name option;
    full_width : float; full_height : float;
    children : ((float * float) * tree) list;
  }

  let children_of_node = function
  | Eval.Tree_node (_, _, _, children) -> children
  | _ -> []

  let build_table get_content deriv_tree =
    let h = Hashtbl.create 1024 in
    let index = ref 1 in
    let rec aux tree =
      let (before, after) = get_content tree in
      let store_content =
        List.iter
          (fun content ->
            if not (Hashtbl.mem h content) then
            begin
              Hashtbl.replace h content !index;
              incr index
            end) in
      store_content before;
      List.iter aux (children_of_node tree);
      store_content after in
    aux deriv_tree;
    h

  let build_store_table deriv_tree =
    let get_content = function
    | Eval.Tree_node (_, (_, _, s_in, _, _), (_, s_out), _) -> ([s_in], [s_out])
    | Eval.Tree_class _ -> ([], [])
    | Eval.Tree_implementation _ -> ([], [])
    | Eval.Tree_super _ -> ([], [])
    | Eval.Tree_env_lookup _ -> ([], [])
    | Eval.Tree_env_extend _ -> ([], [])
    | Eval.Tree_store_lookup (store, _, _) -> ([store], [])
    | Eval.Tree_store_update (store, _, store') -> ([store; store'], [])
    | Eval.Tree_store_newloc (store, _, _) -> ([store], [])
    | Eval.Tree_misc _ -> ([], [])
    | Eval.Tree_error _ -> ([], []) in
    build_table get_content deriv_tree

  let build_env_table deriv_tree =
    let get_content = function
    | Eval.Tree_node (_, (_, _, _, env, _), _, _) -> ([env], [])
    | Eval.Tree_class _ -> ([], [])
    | Eval.Tree_implementation _ -> ([], [])
    | Eval.Tree_super _ -> ([], [])
    | Eval.Tree_env_lookup (env, _, _) -> ([env], [])
    | Eval.Tree_env_extend (env, _, env') -> ([env; env'], [])
    | Eval.Tree_store_lookup _ -> ([], [])
    | Eval.Tree_store_update _ -> ([], [])
    | Eval.Tree_store_newloc _ -> ([], [])
    | Eval.Tree_misc _ -> ([], [])
    | Eval.Tree_error _ -> ([], []) in
    build_table get_content deriv_tree

  let store_name store_table store =
    try "S" ^ string_of_int (Hashtbl.find store_table store)
    with Not_found -> "S???"

  let env_name env_table env =
    try "E" ^ string_of_int (Hashtbl.find env_table env)
    with Not_found -> "E???"

  let string_of_node program store_table env_table = function
  | Eval.Tree_node (name, (ty, val_in, store_in, env, expr),
                              (val_out, store_out), _) ->
      let lhs = String.concat ", "
        [
          str_ty ty;
          compress (Printty.string_of_value val_in);
          store_name store_table store_in;
          env_name env_table env;
        ] in
      let str =
        lhs ^ " \xE2\x8A\xA2 " ^
        compress (Printty.string_of_expr program expr) ^ ": " ^
        compress (Printty.string_of_value val_out) ^ ", " ^
        store_name store_table store_out in
      (Some name, str)
  | Eval.Tree_class (ty, attr_list) ->
      let rhs =
        String.concat ", "
          (List.map
            (fun (id, ty, value) ->
              let a = str_id id in
              let t = str_ty ty in
              let v = compress (Printty.string_of_value value) in
              a ^ " : " ^ t ^ " = " ^ v)
            attr_list) in
      let str = "class(" ^ str_ty ty ^ ") = (" ^ rhs ^ ")" in
      (None, str)
  | Eval.Tree_implementation (ty, m, (ty_impl, args, expr)) ->
      let rhs = String.concat ", " (str_ty ty_impl :: List.map str_id args) in
      let str_body = compress (Printty.string_of_expr program expr) in
      let str = "implementation(" ^ str_ty ty ^ ", " ^ str_id m ^ ") = (" ^
         rhs ^ ", " ^ str_body ^ ")" in
      (None, str)
  | Eval.Tree_super (ty, ty_s) ->
      let str = "super(" ^ str_ty ty ^ ") = " ^ str_ty ty_s in
      (None, str)
  | Eval.Tree_env_lookup (env, id, loc) ->
      let e = env_name env_table env in
      let v = str_id id in
      let l = Store.string_of_location loc in
      let str = e ^ "(" ^ v ^ ") = " ^ l in
      (None, str)
  | Eval.Tree_env_extend (env, id_loc_list, env') ->
      let is_new = (Environment.length env = 0) in
      let e = if is_new then "" else env_name env_table env in
      let e' = env_name env_table env' in
      let rhs =
        String.concat ", "
          (List.map
            (fun (id, loc) ->
              let v = str_id id in
              let l = Store.string_of_location loc in
              match is_new with
              | false -> l ^ " / " ^ v
              | true  -> v ^ " : " ^ l)
            id_loc_list) in
      let str = e' ^ " = " ^ e ^ "[" ^ compress rhs ^ "]" in
      (None, str)
  | Eval.Tree_store_lookup (store, loc, value) ->
      let s = store_name store_table store in
      let l = Store.string_of_location loc in
      let v = compress (Printty.string_of_value value) in
      let str = s ^ "(" ^ l ^ ") = " ^ v in
      (None, str)
  | Eval.Tree_store_update (store, loc_val_list, store') ->
      let s = store_name store_table store in
      let s' = store_name store_table store' in
      let rhs =
        String.concat ", "
          (List.map
            (fun (loc, value) ->
              let l = Store.string_of_location loc in
              let v = compress (Printty.string_of_value value) in
              v ^ " / " ^ l)
            loc_val_list) in
      let str = s' ^ " = " ^ s ^ "[" ^ compress rhs ^ "]" in
      (None, str)
  | Eval.Tree_store_newloc (store, n, loc_list) ->
      let s = store_name store_table store in
      let (lhs, post) =
        match List.length loc_list with
        | 1 ->
            let l = Store.string_of_location (List.hd loc_list) in
            (l, "")
        | _ ->
            let set =
              String.concat ", " (List.map Store.string_of_location loc_list) in
            let text =
              ", for l in {" ^ compress set ^ "} and each l is distinct" in
            ("l", text) in
      let str = lhs ^ " = newloc(" ^ s ^ ")" ^ post in
      (None, str)
  | Eval.Tree_misc s -> (None, compress s)
  | Eval.Tree_error s -> (None, compress s)

  let draw cr ?bounds ?(line_width = 1.0) ?(adjust_to_view = false) tree =
    let view = if adjust_to_view then bounds else None in
    let rec aux node (x0, y0) =
      List.iter
        (fun ((cx, cy), child) ->
          let (cx, cy) = (x0 +. cx, y0 +. cy) in
          let do_draw =
            match bounds with
            | None -> true
            | Some bounds ->
                let cx' = cx +. child.full_width in
                let cy' = cy +. child.full_height in
                do_intersect ((cx, cy), (cx', cy')) bounds in
          if do_draw then aux child (cx, cy))
        node.children;
      let left = x0 +. node.text_x in
      let y = y0 +. node.text_y in
      let bar_left = x0 +. node.bar_x in
      let bar_right = bar_left +. node.bar_width in
      if node.draw_bar then
      begin
        save cr;
        set_line_width cr line_width;
        let remainder = mod_float line_width 2.0 in
        let off_y = if remainder = 0.0 || remainder > 1.0 then 0.0 else 0.5 in
        let bar_y = y0 +. node.bar_y in
        let dy = snd (Cairo.user_to_device cr ~x:0.0 ~y:bar_y) in
        let y = snd (Cairo.device_to_user cr ~x:0.0 ~y:(floor dy)) +. off_y in
        move_to cr bar_left y;
        line_to cr bar_right y;
        stroke cr;
        restore cr;
      end;
      let name_x =
        match node.name with
        | None -> bar_right
        | Some v ->
            let name_x0 = x0 +. v.name_x in
            let name_y0 = y0 +. v.name_y +. line_width /. 2.0 in
            let (name_x, name_y, r) =
              match view with
              | None -> (name_x0, name_y0, name_x0)
              | Some ((l, _), (r, _)) ->
                  let margin = name_x0 -. bar_right in
                  let name_width = v.name_width +. 2.0 *. margin in
                  let min_x = (max bar_left l) +. node.text_width +. margin in
                  let x = max (r -. name_width) min_x in
                  if x < name_x0 && name_x0 +. name_width > r then
                    (x, y, x -. margin)
                  else
                    (name_x0, name_y0, name_x0) in
            move_to cr name_x name_y;
            set_source_rgb cr 0.4 0.4 0.4;
            show_text cr v.name_text;
            r in
      let x =
        match view with
        | None -> left
        | Some ((l, _), (r, _)) ->
            let max_l = min name_x (min bar_right r) -. node.text_width in
            max bar_left (min max_l (max left l)) in
      move_to cr x y;
      set_source_rgb cr 0.0 0.0 0.0;
      show_text cr node.node_text in
    save cr;
    aux tree (0.0, 0.0);
    restore cr

  let generate ?(rule_margin = 15.0)
               ?(bar_margin = 1.0)
               ?(name_distance = 1.5)
               cr program store_table env_table deriv_tree =
    let rec gen deriv_tree =
      let (name_option, node_text) =
        string_of_node program store_table env_table deriv_tree in
      let fe = font_extents cr in
      let te = text_extents cr node_text in
      let children = List.map gen (children_of_node deriv_tree) in
      let (pos, w, r, h) =
        List.fold_left
          (fun (pos, w, _, h) node ->
            let new_w = w +. node.full_width -. rule_margin in
            let r =
              match node.name with
              | None -> new_w
              | Some v -> w +. v.name_x -. name_distance +. v.name_x_bearing in
            let h = max h node.full_height in
            ((w, node.full_height) :: pos, new_w, r, h))
          ([], ~-.rule_margin, 0.0, (fe.ascent +. fe.descent) /. 2.0)
          children in
      let offset_x = max ((te.width -. r) /. 2.0) 0.0 +. rule_margin in

      let bar_x = rule_margin in
      let bar_y = h +. bar_margin in
      let bar_width = max r te.width in
      let text_width = te.width in
      let text_x = bar_x +. (bar_width -. text_width) /. 2.0 -. te.x_bearing in
      let text_y = bar_y +. bar_margin +. fe.ascent in
      let draw_bar = name_option <> None || children <> [] in
      let (name, width) = match name_option with
      | None -> (None, bar_x +. bar_width +. rule_margin)
      | Some name_text ->
          let te_name = text_extents cr name_text in
          let name_x_bearing = te_name.x_bearing in
          let name_x =
            bar_x +. bar_width +. name_distance -. name_x_bearing in
          let name_y =
            if draw_bar then
              bar_y -. fe.descent +. fe.ascent /. 2.0
            else
              text_y in
          let name_width = te_name.width in
          let name =
            { name_text; name_x; name_y; name_width; name_x_bearing } in
          (Some name, name_x +. name_width +. rule_margin) in
      let full_width = max width (w +. 2.0 *. rule_margin) in
      let full_height = text_y +. fe.descent in
      let children_pos =
        List.rev_map (fun (x, y) -> (x +. offset_x, h -. y)) pos in
      {
        node_text;
        text_x; text_y;
        text_width;
        draw_bar;
        bar_x; bar_y;
        bar_width;
        name;
        full_width; full_height;
        children = List.combine children_pos children;
      } in
  gen deriv_tree
end
