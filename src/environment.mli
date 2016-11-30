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

module Make (H : Hashtbl.HashedType) : S with type id = H.t