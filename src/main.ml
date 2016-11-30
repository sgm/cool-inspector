let main () =
  let (filename, basic_filename) =
    match Array.length Sys.argv with
    | 0 | 1 -> print_string "No file specified\n"; exit 0
    | 2 -> (Sys.argv.(1), "basic.cool")
    | _ -> (Sys.argv.(1), Sys.argv.(2)) in
  try
    let basic = Parser.parse_file basic_filename in
    let program = Parser.parse_file ~basic filename in
(*
    let options =
      { Printty.default_options with Printty.intermediate_form = false; basic = None } in
    Printty.print_program ~options program;
    print_newline (); print_newline ();
*)
    Type.check_program program;
    let (_result, _store) = Eval.program program in
    ()
  with Camlp4.PreCast.Loc.Exc_located _ as exn ->
    Camlp4.ErrorHandler.print Format.std_formatter exn

let _ = main ()
