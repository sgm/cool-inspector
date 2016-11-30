type location = int

let string_of_location l = "@" ^ string_of_int l

type 'a t =
{
  mutable data : 'a array;
  mutable pos : location;
}

let single_store = ref false

let empty () = { data = [||]; pos = 0 }

let size store = store.pos

let copy store = { store with data = Array.copy store.data }

let lookup store l =
  if l >= store.pos then
    raise Not_found
  else
    try store.data.(l)
    with Invalid_argument "index out of bounds" -> raise Not_found

let update store l v =
  let store = if !single_store then store else copy store in
  if l >= Array.length store.data then
  begin
    let old_data = store.data in
    let new_length = max (l * 3 / 2 + 1) 16 in
    store.data <- Array.make new_length v;
    Array.blit old_data 0 store.data 0 (Array.length old_data)
  end;
  store.data.(l) <- v;
  store.pos <- max (l + 1) store.pos;
  store

let newloc ?(distinct_from = []) store =
  let l_ref = ref store.pos in
  while (List.mem !l_ref distinct_from) do incr l_ref done;
  !l_ref

let update_multi store l_list v_list =
  List.fold_left2 update store l_list v_list

let rec newloc_multi ?(distinct_from = []) store = function
  | 0 -> []
  | i when i > 0 ->
      let l = newloc ~distinct_from store in
      l :: newloc_multi ~distinct_from:(l :: distinct_from) store (i - 1)
  | _ -> invalid_arg "newloc_multi"

let iteri f store =
  Array.iteri (fun i v -> if i < store.pos then f i v) store.data

let iter f store = iteri (fun _ -> f) store
  
let to_list store = Array.to_list (Array.sub store.data 0 store.pos)
