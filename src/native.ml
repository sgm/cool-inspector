open Ast

module Error = Error.Eval
exception Error of Error.t'

let error err = raise (Error err)
let no_rule_applies () = error Error.No_rule_applies

let rec clone store loc_map = function
  | Val_object (ty, attrs) ->
      let (store_n, attrs_n, loc_map_n) = List.fold_left
        (fun (store_i, attrs_i, loc_map_i) (id, li) ->
          try
            let lj = List.assoc li loc_map_i in
            let attrs_i1 = (id, lj) :: attrs_i in
            (store_i, attrs_i1, loc_map_i)
          with Not_found ->
            let distinct_from = List.map snd loc_map_i in
            let lj = Store.newloc ~distinct_from store_i in
            let (v, store_j, loc_map_i1) =
              clone store_i ((li, lj) :: loc_map_i) (Store.lookup store_i li) in
            let store_i1 = Store.update store_j lj v in
            let attrs_i1 = (id, lj) :: attrs_i in
            (store_i1, attrs_i1, loc_map_i1))
        (store, [], loc_map)
        attrs in
      (Val_object (ty, List.rev attrs_n), store_n, loc_map_n)
  | Val_array_any loc_array ->
      let (store_n, loc_list_n, loc_map_n) = Array.fold_left
        (fun (store_i, loc_list_i, loc_map_i) li ->
          try
            let lj = List.assoc li loc_map_i in
            let loc_list_i1 = lj :: loc_list_i in
            (store_i, loc_list_i1, loc_map_i)
          with Not_found ->
            let distinct_from = List.map snd loc_map_i in
            let lj = Store.newloc ~distinct_from store_i in
            let (v, store_j, loc_map_i1) =
              clone store_i ((li, lj) :: loc_map_i) (Store.lookup store_i li) in
            let store_i1 = Store.update store_j lj v in
            let loc_list_i1 = lj :: loc_list_i in
            (store_i1, loc_list_i1, loc_map_i1))
        (store, [], loc_map)
        loc_array in
      (Val_array_any (Array.of_list (List.rev loc_list_n)), store_n, loc_map_n)
  | v -> (v, store, loc_map)

(******************************* Native Methods *******************************)
let get_f c f =
  match (string_of_ty c, string_of_id f) with

| ("Any", "Any") -> fun so args store env ->
    (so, store)

| ("Any", "equals") -> fun so args store env ->
    let b = (so = List.nth args 0) in
    (Val_boolean b, store)

| ("Any", "clone") -> fun so args store env ->
    let (v, store2, _) = clone store [] so in
    (v, store2)

| ("Any", "abort") -> fun so args store env ->
    error Error.Abort

| ("IO", "out_string") -> fun so args store env ->
    let v = List.nth args 0 in
    (match v with
    | Val_string s -> print_string s
    | Val_nil -> error Error.Null_dispatch
    | _ -> no_rule_applies ());
    (Val_unit, store)

| ("IO", "in_string") -> fun so args store env ->
    (Val_string (read_line ()), store)

| ("Int", "to_string") -> fun so args store env ->
    let i = match so with Val_int i -> i | _ -> no_rule_applies () in
    (Val_string (string_of_int i), store)

| ("String", "length") -> fun so args store env ->
    let i = match so with
    | Val_string s -> String.length s
    | _ -> no_rule_applies () in
    (Val_int i, store)

| ("String", "concat") -> fun so args store env ->
    let v = List.nth args 0 in
    let s = match (so, v) with
    | (Val_string s1, Val_string s2) -> s1 ^ s2
    | _ -> no_rule_applies () in
    (Val_string s, store)

| ("String", "substring") -> fun so args store env ->
    let start = List.nth args 0 in
    let len = List.nth args 1 in
    let s = match (so, start, len) with
    | (Val_string s, Val_int start, Val_int len) ->
        (try String.sub s start len
        with Invalid_argument "String.sub" -> error Error.Out_of_range)
    | _ -> no_rule_applies () in
    (Val_string s, store)

| ("String", "to_int") -> fun so args store env ->
    let i = match so with
    | Val_string s -> int_of_string s
    | _ -> no_rule_applies () in
    (Val_int i, store)

| ("ArrayAny", "ArrayAny") -> fun so args store env ->
    let (loc_list, store2) = match List.nth args 0 with
    | Val_int len when len < 0 -> error Error.Out_of_range
    | Val_int len ->
        let l = Store.newloc_multi store len in
        (l, Store.update_multi store l (List.rev_map (fun _ -> Val_nil) l))
    | _ -> no_rule_applies () in
    (Val_array_any (Array.of_list loc_list), store2)

| ("ArrayAny", "length") -> fun so args store env ->
    let i = match so with
    | Val_array_any loc_array -> Array.length loc_array
    | _ -> no_rule_applies () in
    (Val_int i, store)

| ("ArrayAny", "get") -> fun so args store env ->
    let i = List.nth args 0 in
    let v = match (so, i) with
    | (Val_array_any loc_array, Val_int i) ->
        (try
          Store.lookup store loc_array.(i)
        with Invalid_argument "index out of bounds" ->
          error Error.Out_of_range)
    | _ -> no_rule_applies () in
    (v, store)

| ("ArrayAny", "set") -> fun so args store env ->
    let i = List.nth args 0 in
    let v = List.nth args 1 in
    let store2 = match (so, i) with
    | (Val_array_any loc_array, Val_int i) ->
        (try
          Store.update store loc_array.(i) v
        with Invalid_argument "index out of bounds" ->
          error Error.Out_of_range)
    | _ -> no_rule_applies () in
    (Val_unit, store2)

| _ -> fun _ _ _ _ -> no_rule_applies ()

(******************************************************************************)

let mk_method loc override c id formals ty =
  let e = Expr_native (loc, get_f c id, formals, ty) in
  Feat_method (loc, override, id, formals, ty, e)
