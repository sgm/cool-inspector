module String : sig
  val explode : string -> char list
  val implode : char list -> string
end

module List : sig
  val make : int -> (int -> 'a) -> 'a list
  val drop : int -> 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val findi : (int -> 'a -> bool) -> 'a list -> int * 'a
  val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val partition_mapi :
    (int -> 'a -> [< `A of 'b | `B of 'c ]) -> 'a list -> 'b list * 'c list
  val partition_map :
    ('a -> [< `A of 'b | `B of 'c ]) -> 'a list -> 'b list * 'c list
end

module Option : sig
  exception No_value
  val may : ('a -> unit) -> 'a option -> unit
  val map : ('a -> 'b) -> 'a option -> 'b option
  val default : 'a -> 'a option -> 'a
  val is_some : 'a option -> bool
  val is_none : 'a option -> bool
  val get : 'a option -> 'a
  val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
end
