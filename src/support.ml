module String = struct
  (* explode and implode from the OCaml Expert FAQ. *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  let implode l =
    let buf = Buffer.create (List.length l + 1) in
    List.iter (Buffer.add_char buf) l;
    Buffer.contents buf
end

module List = struct
  let make i f =
    let rec loop acc = function
    | j when j < 0 -> acc
    | j -> loop ((f j) :: acc) (j - 1)
    in
    loop [] (i - 1)

  let rec drop n = function
  | _ :: tl when n > 0 -> drop (n - 1) tl
  | l -> l

  let take n l =
    let rec loop n acc = function
    | h :: tl when n > 0 ->
      loop (n - 1) (h :: acc) tl
    | _ -> List.rev acc
    in
    loop n [] l

  let rec findi p l =
    let rec loop n = function
    | [] -> raise Not_found
    | h :: tl -> if p n h then (n, h) else loop (n + 1) tl
    in
    loop 0 l

  let filter_mapi f l =
    let rec loop i acc = function
    | [] -> List.rev acc
    | h :: tl ->
        match f i h with
        | Some x -> loop (i + 1) (x :: acc) tl
        | None -> loop (i + 1) acc tl
    in
    loop 0 [] l

  let filter_map f l = filter_mapi (fun _ x -> f x) l

  let partition_mapi f l =
    let rec loop i a b = function
    | [] -> (List.rev a, List.rev b)
    | h :: tl ->
        match f i h with
        | `A x -> loop (i + 1) (x :: a) b tl
        | `B x -> loop (i + 1) a (x :: b) tl
    in
    loop 0 [] [] l

  let partition_map f l = partition_mapi (fun _ x -> f x) l
end

module Option = struct
  exception No_value
  let may f = function Some v -> f v | None -> ()
  let map f = function Some v -> Some (f v) | None -> None
  let default v = function Some v -> v | None -> v
  let is_some = function Some _ -> true | None -> false
  let is_none = function Some _ -> false | None -> true
  let get = function Some v -> v | None -> raise No_value
  let map_default f v = function Some v' -> f v' | None -> v
end