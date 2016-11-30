type location
val string_of_location : location -> string
type 'a t
val single_store : bool ref
val empty : unit -> 'a t
val size : 'a t -> int
val copy : 'a t -> 'a t
val lookup : 'a t -> location -> 'a
val update : 'a t -> location -> 'a -> 'a t
val newloc : ?distinct_from:location list -> 'a t -> location
val update_multi :'a t -> location list -> 'a list -> 'a t
val newloc_multi :
  ?distinct_from:location list -> 'a t -> int -> location list
val iteri : (location -> 'a -> unit) -> 'a t -> unit
val iter : ('a -> unit) -> 'a t -> unit
val to_list : 'a t -> 'a list
