(* include Camlp4.PreCast.MakeGram(Lexer) *)
include Camlp4.Struct.Grammar.Static.Make(Lexer)

let entries = ref []
let extend entry (position, rules) =
  entries := (Entry.name entry, position, rules) :: !entries;
  extend entry (position, rules)

let flatten_tree =
  let module S = Camlp4.Struct.Grammar.Structure.Make(Lexer) in
  let module PP = Camlp4.Struct.Grammar.Print.Make(S) in
  (Obj.magic PP.flatten_tree : tree -> symbol list list)

let ename internal_entry =
  let module S = Camlp4.Struct.Grammar.Structure.Make(Lexer) in
  let s_ename e = e.S.ename in
  (Obj.magic s_ename : internal_entry -> string) internal_entry

let string_of_symbol =
  let module S = Camlp4.Struct.Grammar.Structure.Make(Lexer) in
  let module PP = Camlp4.Struct.Grammar.Print.Make(S) in
  fun s ->
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    (Obj.magic PP.print_symbol ppf : symbol -> unit) s;
    Format.pp_print_flush ppf ();
    Buffer.contents buf

module Dangles = struct
  (* TODO: test this module more extensively *)
  type 'a vtree =
    | VtId of 'a
    | VtAnd of 'a vtree * 'a vtree
    | VtOr of 'a vtree * 'a vtree
  type 'a valid_if = VNever | VAlways | VEmpty of 'a vtree
  type symbol_kind = SKTerminal of string | SKVar of int * int

  let vId i = VEmpty (VtId i)

  let vAnd vi1 vi2 =
    match vi1 with VNever -> VNever | VAlways -> vi2 | VEmpty vt1 ->
      match vi2 with VNever -> VNever | VAlways -> vi1 | VEmpty vt2 ->
        if vt1 = vt2 then vi1 else VEmpty (VtAnd (vt1, vt2))

  let rec vOr vi1 vi2 =
    match vi1 with VNever -> vi2 | VAlways -> VAlways | VEmpty vt1 ->
      match vi2 with VNever -> vi1 | VAlways -> VAlways | VEmpty vt2 ->
        if vt1 = vt2 then vi1 else VEmpty (VtOr (vt1, vt2))

  let v_subst vi1 i vi2 =
    match vi1 with
    | VNever | VAlways -> vi1
    | VEmpty t ->
        let rec loop = function
        | VtId j -> if i = j then vi2 else VEmpty (VtId j)
        | VtAnd (vt1, vt2) -> vAnd (loop vt1) (loop vt2)
        | VtOr (vt1, vt2) -> vOr (loop vt1) (loop vt2) in
        loop t

  let start_of_rule ei li (symbol_list, _) =
    let rec loop symbol_list =
      let empty_if = ref VNever in
      let rec f ?(b = false) v = function
      | Smeta (n, sl1, _) :: sl2 ->
          let nth = List.nth sl1 in
          (match n with
          | "FOLD0" -> f ~b v (Slist0 (nth 0) :: sl2)
          | "FOLD1" -> f ~b v (Slist1 (nth 0) :: sl2)
          | "FOLD0 SEP" -> f ~b v (Slist0sep (nth 0, nth 1) :: sl2)
          | "FOLD1 SEP" -> f ~b v (Slist1sep (nth 0, nth 1) :: sl2)
          | _ -> failwith ("start_of_rule: unknown symbol (Smeta " ^ n ^ ")"))
      | Slist0 s :: sl -> f v [s] @ f ~b v sl
      | Slist1 s :: sl -> f ~b v (s :: sl)
      | Slist0sep (s, t) :: sl -> f v [s; t] @ f ~b v sl
      | Slist1sep (s, t) :: sl -> f ~b v (s :: Sopt t :: sl)
      | Sopt s :: sl -> f v [s] @ f ~b v sl
      | Stry s :: sl -> f ~b v (s :: sl)
      | Stree t :: sl ->
          let rec tree_loop t_acc tv = function
          | h :: tl ->
              let (lv, vrl) = loop h in
              tree_loop (t_acc @ vrl) (vOr tv lv) tl
          | [] -> (t_acc, tv) in
          let (tree_rl, tv) = tree_loop [] VNever (flatten_tree t) in
          if tv = VNever then tree_rl else tree_rl @ f ~b (vAnd v tv) sl
      | Snterm ie :: sl ->
          let name = ename ie in
          let (i, (_, _, _)) =
            Support.List.findi (fun _ (n, _, _) -> n = name) !entries in
          (SKVar (i, 0), v) :: f ~b (vAnd v (vId (i, 0))) sl
      | Snterml (ie, l) :: sl ->
          let name = ename ie in
          let (i, (_, _, level_list)) =
            Support.List.findi (fun _ (n, _, _) -> n = name) !entries in
          let (j, _) =
            Support.List.findi (fun _ (n, _, _) -> n = Some l) level_list in
          (SKVar (i, j), v) :: f ~b (vAnd v (vId (i, j))) sl
      | Sself :: sl ->
          (SKVar (ei, li), v) :: f ~b (vAnd v (vId (ei, li))) sl
      | Snext :: sl ->
          (SKVar (ei, li + 1), v) :: f ~b (vAnd v (vId (ei, li + 1))) sl
      | Stoken _ | Skeyword _ as s :: _ ->
          [(SKTerminal (string_of_symbol s), v)]
      | [] -> if b then empty_if := v; [] in
      (!empty_if, f ~b:true VAlways symbol_list) in
    loop symbol_list

  let end_of_rule ei li (symbol_list, _) =
    let empty = fun _ -> [] in
    let cons i s acc = fun v -> (s, v) :: acc (vAnd v (vId i)) in
    let append a1 a2 = fun v -> a1 v @ a2 v in
    let rec loop symbol_list =
      let empty_if = ref VNever in
      let rec f ?(b = false) acc v = function
      | Smeta (n, sl1, _) :: sl2 ->
          let nth = List.nth sl1 in
          (match n with
          | "FOLD0" -> f ~b acc v (Slist0 (nth 0) :: sl2)
          | "FOLD1" -> f ~b acc v (Slist1 (nth 0) :: sl2)
          | "FOLD0 SEP" -> f ~b acc v (Slist0sep (nth 0, nth 1) :: sl2)
          | "FOLD1 SEP" -> f ~b acc v (Slist1sep (nth 0, nth 1) :: sl2)
          | _ -> failwith ("end_of_rule: unknown symbol (Smeta " ^ n ^ ")"))
      | Slist0 s :: sl -> f ~b (append (f empty v [s]) acc) v sl
      | Slist1 s :: sl -> f ~b acc v (s :: sl)
      | Slist0sep (s, t) :: sl -> f ~b (append (f empty v [t; s]) acc) v sl
      | Slist1sep (s, t) :: sl -> f ~b acc v (Sopt t :: s :: sl)
      | Sopt s :: sl -> f ~b (append (f empty v [s]) acc) v sl
      | Stry s :: sl -> f ~b acc v (s :: sl)
      | Stree t :: sl ->
          let rec tree_loop t_acc tv = function
          | h :: tl ->
              let (lv, vrl) = loop h in
              tree_loop (append t_acc vrl) (vOr tv lv) tl
          | [] -> (t_acc, tv) in
          let (tree_rl, tv) = tree_loop empty VNever (flatten_tree t) in
          let tree_acc = if tv = VNever then tree_rl else append tree_rl acc in
          f ~b:(b && tv <> VNever) tree_acc (vAnd v tv) sl
      | Snterm ie :: sl ->
          let name = ename ie in
          let (i, (_, _, _)) =
            Support.List.findi (fun _ (n, _, _) -> n = name) !entries in
          f ~b (cons (i, 0) (SKVar (i, 0)) acc) (vAnd v (vId (i, 0))) sl
      | Snterml (ie, l) :: sl ->
          let name = ename ie in
          let (i, (_, _, level_list)) =
            Support.List.findi (fun _ (n, _, _) -> n = name) !entries in
          let (j, _) =
            Support.List.findi (fun _ (n, _, _) -> n = Some l) level_list in
          f ~b (cons (i, j) (SKVar (i, j)) acc) (vAnd v (vId (i, j))) sl
      | Sself :: sl ->
          f ~b (cons (ei, li) (SKVar (ei, li)) acc) (vAnd v (vId (ei, li))) sl
      | Snext :: sl ->
          f ~b (cons (ei, li + 1) (SKVar (ei, li + 1)) acc)
            (vAnd v (vId (ei, li + 1))) sl
      | Stoken _ | Skeyword _ as s :: sl ->
          f (cons (0, 0) (SKTerminal (string_of_symbol s)) empty) VNever sl
      | [] -> if b then empty_if := v; acc in
      (!empty_if, f ~b:true empty VAlways symbol_list) in
    let (empty_if, f) = loop symbol_list in
    (empty_if, f VAlways)

  let rules_ends entries =
    List.mapi
      (fun ei (entry_name, _, level_list) ->
        (entry_name, List.mapi
          (fun li (level_name, _, rule_list) ->
            (level_name, List.map
              (fun rule -> (start_of_rule ei li rule, end_of_rule ei li rule))
              rule_list))
          level_list))
      entries

  let empty_cond level_list level =
    let rec loop i at_level acc = function
    | (_, level_ends) :: tl ->
        if at_level || level = i then
          if List.exists (fun ((vi, _), _) -> vi = VAlways) level_ends then
            VAlways
          else
            let level_rules_vor =
              List.fold_left (fun v ((vi, _), _) -> vOr v vi) acc level_ends in
            loop (i + 1) true level_rules_vor tl
        else
          loop (i + 1) false acc tl
    | [] -> acc in
    loop 0 false VNever level_list

  let is_in (e1, l1) (e2, l2) = (e1 = e2 && l1 >= l2)

  let ends = ref []
  let can_be_empty = ref []

  let rec is_valid = function
  | VNever -> false
  | VAlways -> true
  | VEmpty t ->
      let rec loop = function
      | VtId (ei, li) ->
          (try
            snd (List.find (fun (p, _) -> is_in (ei, li) p) !can_be_empty)
          with Not_found ->
            can_be_empty := ((ei, li), false) :: !can_be_empty;
            let (_, ee) = List.nth !ends ei in
            let v = is_valid (empty_cond ee li) in
            can_be_empty := ((ei, li), v) ::
              (List.remove_assoc (ei, li) !can_be_empty);
            v)
      | VtAnd (vt1, vt2) -> loop vt1 && loop vt2
      | VtOr (vt1, vt2) -> loop vt1 || loop vt2 in
      loop t

  let of_rule entry_index level_index rule_index =
    if !ends = [] then ends := rules_ends !entries;
    let rec loop visited is_start el =
      List.fold_left
        (fun acc (sk, v) ->
          acc || match sk with
          | SKTerminal _ -> false
          | SKVar (ei, li) ->
              if is_valid v then
                is_in (entry_index, level_index) (ei, li)
                  ||
                if List.exists (is_in (ei, li)) visited then false else
                  let (_, ee) = List.nth !ends ei in
                  let le = List.mapi
                    (fun i (_, l) -> (li + i, l))
                    (Support.List.drop li ee) in
                  List.fold_left
                    (fun acc' (li', l) ->
                      acc' || List.fold_left
                        (fun acc' ((sv', sel'), (ev', eel')) ->
                          let el' = if is_start then sel' else eel' in
                          acc' || loop ((ei, li') :: visited) is_start el')
                        false
                        l)
                    false
                    le
              else
                false)
        false
        el in
    let (_, entry_ends) = List.nth !ends entry_index in
    let (_, level_ends) = List.nth entry_ends level_index in
    let ((_, sel), (_, eel)) = List.nth level_ends rule_index in
    (loop [] true sel, loop [] false eel)

  let of_terminal entry_index level_index terminal rule_list =
    let rule_index = match terminal with
    | None -> if List.length rule_list > 1 then None else Some 0
    | Some s ->
        let rec loop i = function
        | (symbl, _) :: tl ->
            if List.exists ((=) (Skeyword s)) symbl
              then Some i
              else loop (i + 1) tl
        | [] -> None in
        loop 0 rule_list in
    match rule_index with
    | None -> None
    | Some rule_index -> Some (of_rule entry_index level_index rule_index)
end

let precedence_of_entry entry ?t level =
  let rec levels_of_entry n i = function
  | (entry_name, _, level_list) :: _ when entry_name = n -> (i, level_list)
  | _ :: es -> levels_of_entry n (i + 1) es
  | [] -> failwith "precedence_of_entry" in
  let rec prec_of_level n entry_index i = function
  | (Some level_name, assoc, rule_list) :: _ when level_name = n ->
      (i + 1, assoc, Dangles.of_terminal entry_index i t rule_list)
  | _ :: ls -> prec_of_level n entry_index (i + 1) ls
  | [] -> invalid_arg "precedence_of_entry" in
  let (entry_index, expr_levels) = levels_of_entry entry 0 !entries in
  prec_of_level level entry_index 0 expr_levels
