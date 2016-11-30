let may_call f arg default = match f with None -> default | Some f -> f arg

let file_chooser ?(multiple = false) ~filters ~parent ~title action_id =
  let action = (action_id :> GtkEnums.file_chooser_action) in
  let dialog =
    GWindow.file_chooser_dialog ~action ~parent ~modal:true ~title () in
  dialog#set_select_multiple multiple;
  dialog#add_select_button_stock (action_id :> GtkStock.id) `APPLY;
  dialog#add_button_stock `CANCEL `CANCEL;
  let add_filter (get_name, patterns) =
    dialog#add_filter (GFile.filter ~name:(get_name ()) ~patterns ()) in
  List.iter add_filter filters;
  dialog

let open_file ~parent ~title ~filters ?multiple ?callback open_fun =
  let dialog = file_chooser ~parent ~filters ~title ?multiple `OPEN in
  let finish opened = dialog#destroy (); may_call callback opened () in
  ignore (dialog#connect#response
    ~callback:(function
    | `DELETE_EVENT | `CANCEL -> finish []
    | `APPLY ->
        let rec open_all opened = function
        | [] -> if opened <> [] then finish (List.rev opened)
        | h :: tl ->
            let callback = function
            | None -> open_all opened tl
            | Some document -> open_all (document :: opened) tl in
            open_fun ~callback h in
        open_all [] dialog#get_filenames));
  dialog

let save_file ~parent ~title ~filters ?callback save_fun =
  let dialog = file_chooser ~parent ~filters ~title `SAVE in
  ignore (GtkSignal.connect_property dialog#as_widget
    ~prop:{ Gobject. name = "filter"; conv = Gobject.Data.gobject_option }
    ~callback:(fun filter ->
      if filter = None then
        dialog#set_filter (List.hd (List.rev dialog#list_filters))));
  let finish saved = dialog#destroy (); may_call callback saved () in
  let filename_updated = ref false in
  let initial_apply = ref false in
  let add_extension_and_apply () =
    let extension =
      try
        let pred (get_name, _) = (get_name () = dialog#filter#name) in
        let (_, patterns) = List.find pred filters in
        let pattern = List.hd patterns in
        let index = String.rindex pattern '.' in
        Some (String.sub pattern index (String.length pattern - index))
      with _ -> None in
    filename_updated := (match (dialog#filename, extension) with
    | (Some filename, Some extension) ->
        (try ignore (Filename.chop_extension filename) with _ ->
          ignore (dialog#set_current_name (filename ^ extension)));
        true
    | (filename, _) -> filename <> None);
    if !filename_updated then
    begin
      dialog#set_do_overwrite_confirmation true;
      dialog#response `APPLY
    end
    else
      initial_apply := false in
  List.iter
    (fun widget ->
      let btn = new GButton.button (GtkButton.Button.cast widget#as_widget) in
      ignore (btn#connect#clicked
        ~callback:(fun () ->
          if !initial_apply then
          begin
            filename_updated := false;
            dialog#set_do_overwrite_confirmation false;
          end;
          initial_apply := not !initial_apply)))
    dialog#action_area#children;
  ignore (dialog#connect#response
    ~callback:(function
    | `DELETE_EVENT | `CANCEL -> finish false
    | `APPLY when not !filename_updated -> add_extension_and_apply ()
    | `APPLY ->
        initial_apply := false;
        match dialog#filename with
        | None -> ()
        | Some filename ->
            let callback saved = if saved then finish true in
            save_fun ~callback filename));
  dialog
