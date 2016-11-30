let get_from_list language l =
  let rec starts_with a b i =
    i >= String.length a || i >= String.length b ||
      (String.get a i = String.get b i && starts_with a b (i + 1)) in
  let rec find l = function
  | [] -> raise Not_found
  | [h] -> List.find h l
  | h :: tl -> try List.find h l with _ -> find l tl in
  find l [
    (fun f -> f `_CODE = language);
    (fun f -> f `_WINDOWS_CODE = language);
    (fun f -> f `_ENGLISH_NAME = language);
    (fun f -> f `_LOCALE_NAME = language);
    (fun f -> starts_with (f `_CODE) language 0);
    (fun _ -> true);
  ]

let system ?(ignore_env = false) () =
  let rec try_list =
    let try_f f tl = try f () with _ -> try_list tl in
    function [] -> failwith "try_list" | [h] -> h () | h :: tl -> try_f h tl in
  let try_over_list ?default f l =
    let f () = try_list (List.map (fun v () -> f v) l) in
    match default with None -> f () | Some v -> try f () with _ -> v in
  let locale =
    try
      if ignore_env then raise Not_found;
      let variables = ["LANGUAGE"; "LANG"] in
      let getenv v = match Glib.getenv v with "" -> raise Not_found | s -> s in
      try_over_list getenv variables
    with Not_found ->
      let g_locale = Glib.Main.setlocale `ALL None in
      let system_locale = Glib.Main.setlocale `ALL (Some "") in
      ignore (Glib.Main.setlocale `ALL (Some g_locale));
      system_locale in
  let len = String.length locale in
  let start_index = try_over_list ~default:0 (String.index locale) ['='] in
  let end_index = try_over_list ~default:len (String.index locale) ['.'; ';'] in
  let code = String.sub locale start_index (end_index - start_index) in
  try
    get_from_list code Strings.list `_CODE
  with Not_found ->
    (List.hd Strings.list) `_CODE

let current = ref ""
let get_str str = get_from_list !current Strings.list str
let get_msg msg = get_str (`_MESSAGE msg)
let get_err err = get_str (`_ERROR err)

let get = !current
let set lang =
  current := lang;
  current := get_str `_CODE;
  Glib.setenv "LANGUAGE" !current true;
