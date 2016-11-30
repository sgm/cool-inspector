module type S = sig
  type id
  type t

  val empty : unit -> t
  val length : t -> int
  val lookup : t -> id -> Store.location
  val extend_multi : t -> id list -> Store.location list -> t
  val extend : t -> id -> Store.location -> t
  val join : t -> t -> t
  val to_assoc_list : t -> (id * Store.location) list
end

module Make (H : Hashtbl.HashedType) : (S with type id = H.t) = struct
  module T = Hashtbl.Make(H)

  type id = H.t
  type t = Store.location T.t

  let empty () = T.create 16

  let length = T.length

  let lookup = T.find

  let extend_multi env id_list loc_list =
    let new_env = T.copy env in
    List.iter2 (T.replace new_env) id_list loc_list;
    new_env

  let extend env id loc = extend_multi env [id] [loc]

  let join old_env new_env =
    let env = T.copy old_env in
    T.iter (T.replace env) new_env;
    env

  let to_assoc_list env =
    T.fold (fun id loc acc -> (id, loc) :: acc) env []
end
