open Sig.Token
open Format

module Loc = Camlp4.Struct.Loc

type t = Sig.Token.t

module Error = struct
  type t =
    | Illegal_keyword of string

  exception E of t

  let print ppf = function
    | Illegal_keyword kwd ->
        fprintf ppf "Parse error: Illegal keyword \"%s\"" kwd

  let to_string x = asprintf "%a" print x
end
let () = let module M = Camlp4.ErrorHandler.Register(Error) in ()
let error loc err = Loc.raise loc (Error.E err)

let to_string = function
  | KEYWORD s -> sprintf "KEYWORD %S" s
  | ILLEGAL s -> sprintf "ILLEGAL %S" s
  | SYMBOL s  -> sprintf "SYMBOL %S" s
  | TYPE s    -> sprintf "TYPE %S" s
  | ID s      -> sprintf "ID %S" s
  | INTEGER s -> sprintf "INTEGER %S" s
  | STRING s  -> sprintf "STRING %S" s
  | COMMENT s -> sprintf "COMMENT %S" s
  | BLANKS s  -> sprintf "BLANKS %S" s
  | NEWLINE   -> sprintf "NEWLINE"
  | EOI       -> sprintf "EOI"

let print ppf x = Format.pp_print_string ppf (to_string x)

let match_keyword kwd = function
  | KEYWORD kwd' | SYMBOL kwd' when kwd = kwd' -> true
  | _ -> false

let extract_string = function
  | KEYWORD s | ILLEGAL s | SYMBOL s | TYPE s | ID s | INTEGER s
    | STRING s | COMMENT s | BLANKS s -> s
  | tok -> invalid_arg
      ("Cannot extract a string from this token: " ^ to_string tok)


module Filter = struct
  type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
  type t =
    { is_kwd : string -> bool;
      mutable filter : token_filter }

  module StringSet = Set.Make(String)
  let illegal_keywords_set = ref StringSet.empty

  let rec ignore_layout = parser
    | [< '((COMMENT _ | BLANKS _ | NEWLINE), _); s >] -> ignore_layout s
    | [< 'x; s >] -> [< 'x; ignore_layout s >]

  let rec recognize_illegal_keywords =
    let is_illegal kwd = StringSet.mem kwd !illegal_keywords_set in
    parser
    | [< '((KEYWORD kwd | ID kwd | TYPE kwd), _loc) when is_illegal kwd >] ->
        [< '((ILLEGAL kwd), _loc) >]
    | [< 'tok; s >] -> [< 'tok; recognize_illegal_keywords s >]

  let filter strm = recognize_illegal_keywords (ignore_layout strm)

  let mk is_kwd = { is_kwd; filter }

  let filter x strm =
    let rec error_on_illegal = parser
    | [< '((ILLEGAL kwd), loc) >] -> error loc (Error.Illegal_keyword kwd)
    | [< 'tok; s >] -> [< 'tok; error_on_illegal s >] in
    let rec recognize_keywords = parser
    | [< '((ID kwd | TYPE kwd), loc) when x.is_kwd kwd; s >] ->
        [< '(KEYWORD kwd, loc); recognize_keywords s >]
    | [< 'tok; s >] -> [< 'tok; recognize_keywords s >] in
    error_on_illegal (x.filter (recognize_keywords strm))

  let define_filter x f = x.filter <- f x.filter

  let keyword_added _ _ _ = ()
  let keyword_removed _ _ = ()
end

let add_illegal_keyword kwd =
  Filter.illegal_keywords_set :=
    Filter.StringSet.add kwd !Filter.illegal_keywords_set

let add_illegal_keywords = List.iter add_illegal_keyword
