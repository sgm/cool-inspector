include Camlp4.Sig.Lexer with module Loc = Token.Loc and module Token = Token

val escape : string -> string
val unescape : string -> string
