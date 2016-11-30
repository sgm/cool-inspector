module Token : Camlp4.Sig.Token with
  module Loc = Lexer.Loc and type t = Sig.Token.t

include Camlp4.Sig.Grammar.Static with
  module Loc = Lexer.Loc and module Token := Token

module Dangles : sig
  val of_rule : int -> int -> int -> bool * bool
  val of_terminal :
    int -> int -> string option -> (symbol list * 'a) list ->
    (bool * bool) option
end

val precedence_of_entry :
  string -> ?t:string -> string ->
  int * Camlp4.Sig.Grammar.assoc option * (bool * bool) option
