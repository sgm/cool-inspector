{
  module Loc = Token.Loc
  module Token = Token
  open Sig.Token
  open Token

  module Error = struct
    type t =
      | Illegal_character of char
      | Illegal_character_constant of char
      | Unterminated_string
      | Unterminated_comment
      | Invalid_integer_literal of string

    exception E of t

    open Format

    let pr ppf = fprintf ppf "Syntax error: "; fprintf ppf
    let print ppf = function
      | Illegal_character c ->
          pr ppf "Illegal character \"%s\"" (Char.escaped c)
      | Illegal_character_constant c ->
          pr ppf "Illegal character constant \"\\%s\"" (Char.escaped c)
      | Unterminated_string ->
          pr ppf "String not terminated"
      | Unterminated_comment ->
          pr ppf "Comment not terminated"
      | Invalid_integer_literal i ->
          pr ppf "Invalid integer literal \"%s\"" i

    let to_string x = asprintf "%a" print x
  end

  let () = let module M = Camlp4.ErrorHandler.Register(Error) in ()

  type context = {
    mutable start_loc : Loc.t option;
    lexbuf : Lexing.lexbuf;
    buffer : Buffer.t;
  }

  let current_loc ?(from_start = true) ctx =
    let loc = Loc.of_lexbuf ctx.lexbuf in
    match ctx.start_loc with
    | Some start_loc when from_start ->
        ctx.start_loc <- None; Loc.merge start_loc loc
    | _ -> loc

  let set_start_loc ctx = ctx.start_loc <- Some (Loc.of_lexbuf ctx.lexbuf)

  let new_line ctx = Lexing.new_line ctx.lexbuf

  let store_string ctx s = Buffer.add_string ctx.buffer s

  let store_char ctx c = Buffer.add_char ctx.buffer c

  let get_stored_string ctx =
    let s = Buffer.contents ctx.buffer in
    Buffer.reset ctx.buffer;
    s

  let error ?from_start ctx err =
    Loc.raise (current_loc ?from_start ctx) (Error.E err)

  let escape_list = [
    ('0', '\x00'); ('b', '\x08'); ('t', '\x09'); ('n', '\x0A');
    ('r', '\x0D'); ('f', '\x0C'); ('\"', '\"');  ('\\', '\\')
  ]

  let escape s =
    let rec c_list l i =
      if i < 0 then
        l
      else
        try
          let (c, _) = List.find (fun (_, c) -> c = s.[i]) escape_list in
          c_list ('\\' :: c :: l) (i - 1)
        with Not_found ->
          c_list (s.[i] :: l) (i - 1) in
    Support.String.implode (c_list [] (String.length s - 1))

  let unescape s =
    let rec c_list l i =
      if i < 0 then
        l
      else if s.[i] = '\\' then
        match l with
        | [] -> assert false
        | c :: tl ->
            try
              c_list (List.assoc c escape_list :: tl) (i - 1)
            with Not_found ->
              raise (Error.E (Error.Illegal_character_constant c))
      else
        c_list (s.[i] :: l) (i - 1) in
    Support.String.implode (c_list [] (String.length s - 1))

  let unescape_ctx ctx s =
    try unescape s with Error.E e -> error ~from_start:false ctx e
}

(******************************************************************************)

let newline = "\r\n" | ['\r' '\n']
let blank = ['\t' ' ']
let num = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let alphanum = alpha | num | "_"
let integer = '0' | (num # '0') num*
let symbol = "==" | "<=" | "=>" |
  ['(' ')' '{' '}' ',' '.' ':' ';' '=' '<' '!' '+' '-' '*' '/']

rule token ctx = parse
  | eof                      { [EOI] }
  | newline                  { new_line ctx; [NEWLINE] }
  | blank+              as s { [BLANKS s] }
  | lowercase alphanum* as s { [ID s] }
  | uppercase alphanum* as s { [TYPE s] }
  | symbol              as s { [SYMBOL s] }
  | integer             as s { [INTEGER s] }
  | '\"'                     { set_start_loc ctx; string_1 ctx lexbuf }
  | "\"\"\""                 { set_start_loc ctx; string_n ctx lexbuf }
  | "//"                     { set_start_loc ctx; comment_1 ctx lexbuf }
  | "/*"                     { set_start_loc ctx; comment_n ctx lexbuf }
  | _                   as c { error ctx (Error.Illegal_character c) }

and string_1 ctx = parse
  | eof | newline { error ctx Error.Unterminated_string }
  | '\"'          { [STRING (get_stored_string ctx)] }
  | '\\' _ as s   { store_string ctx (unescape_ctx ctx s); string_1 ctx lexbuf }
  | _ as c        { store_char ctx c; string_1 ctx lexbuf }

and string_n ctx = parse
  | eof           { error ctx Error.Unterminated_string }
  | newline as s  { new_line ctx; store_string ctx s; string_n ctx lexbuf }
  | "\"\"\""      { [STRING (get_stored_string ctx)] }
  | _ as c        { store_char ctx c; string_n ctx lexbuf }

and comment_1 ctx = parse
  | eof           { [COMMENT (get_stored_string ctx); EOI] }
  | newline       { new_line ctx; [COMMENT (get_stored_string ctx); NEWLINE] }
  | _ as c        { store_char ctx c; comment_1 ctx lexbuf }

and comment_n ctx = parse
  | eof           { error ctx Error.Unterminated_comment }
  | newline as s  { new_line ctx; store_string ctx s; comment_n ctx lexbuf }
  | "*/"          { [COMMENT (get_stored_string ctx)] }
  | _ as c        { store_char ctx c; comment_n ctx lexbuf }

(******************************************************************************)

{
  let from_context ctx =
    let f l loc = List.map (fun tok -> (tok, loc)) l in
    let rec flatten = parser
    | [< '(h_tok :: t_tok, (h_loc, t_loc)); s >] ->
        [< '(h_tok, h_loc); Stream.of_list (f t_tok t_loc); flatten s >] in
    let next _ =
      let tok_list = token ctx ctx.lexbuf in
      let loc1 = current_loc ctx and loc2 = current_loc ctx in
      Some (tok_list, (loc1, loc2)) in
    flatten (Stream.from next)

  let from_lexbuf lexbuf =
    let ctx = {
      start_loc = None;
      lexbuf    = lexbuf;
      buffer    = Buffer.create 256;
    } in
    from_context ctx

  let setup_loc lexbuf loc =
    let start_pos = Loc.start_pos loc in
    lexbuf.Lexing.lex_abs_pos <- start_pos.Lexing.pos_cnum;
    lexbuf.Lexing.lex_curr_p  <- start_pos

  let from_stream loc strm =
    let lexbuf = Lexing.from_function (fun buff n ->
      try Bytes.set buff 0 (Stream.next strm); 1 with Stream.Failure -> 0) in
    setup_loc lexbuf loc;
    from_lexbuf lexbuf

  let mk () loc strm =
    from_stream loc strm
}
