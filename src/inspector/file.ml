exception Could_not_read_file of string * exn
exception Could_not_write_file of string * exn
exception File_does_not_exist of string
exception No_file_specified

let is_filename filename =
  try not (String.length filename = 0 || Sys.is_directory filename)
  with _ -> true

let with_file filename callback =
  let in_channel = open_in filename in
  try callback in_channel; close_in in_channel
  with exn -> close_in in_channel; raise exn

let read filename =
  if not (Sys.file_exists filename) then raise (File_does_not_exist filename);
  if not (is_filename filename) then raise No_file_specified;
  try
    let file_buf = Buffer.create 1024 in
    let buf = Bytes.create 1024 in
    let len = ref 0 in
    with_file filename
      (fun in_channel ->
        while len := input in_channel buf 0 1024; !len > 0 do
          Buffer.add_subbytes file_buf buf 0 !len
        done);
    Buffer.contents file_buf
  with exn -> raise (Could_not_read_file (filename, exn))

let write filename contents =
  try
    let out_channel = open_out filename in
    try
      Printf.fprintf out_channel "%s" contents;
      close_out out_channel
    with exn -> close_out out_channel; raise exn
  with exn -> raise (Could_not_write_file (filename, exn))

let rec list_from_uris = function
| "" -> []
| s ->
    let cons_if b f l = try if b then (f ()) :: l else l with _ -> l in
    let rec index_of s i = function
    | [] -> (i = 0, max i 1)
    | h :: tl -> index_of s (try min i (String.index s h) with _ -> i) tl in
    let len = String.length s in
    let (is_empty, index) = index_of s len [' '; '\n'; '\r'; '\t'; '\000'] in
    let filename_from_uri () =
      snd (Glib.Convert.filename_from_uri (String.sub s 0 index)) in
    cons_if (not is_empty) filename_from_uri
      (list_from_uris (String.sub s index (len - index)))
