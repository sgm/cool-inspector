let get_str = Lang.get_str
let get_msg = Lang.get_msg
let get_err = Lang.get_err

let default v = function None -> v | Some v -> v
let may f = function None -> () | Some v -> f v
let may_call f arg default = match f with None -> default | Some f -> f arg

let monospace = Pango.Font.from_string "monospace"
let top_window widget = GtkWindow.Window.cast widget#misc#toplevel#as_widget

let error_dialog ?(details = "") parent message =
  let dialog =
    GWindow.message_dialog ~message ~parent ~message_type:`ERROR
      ~title:(get_str `ERROR) ~buttons:GWindow.Buttons.ok ~modal:true () in
  if details <> "" then
  begin
    let expander = GBin.expander ~label:(get_str `DETAILS)
      ~packing:dialog#vbox#pack () in
    ignore (GMisc.label ~text:details ~line_wrap:true ~packing:expander#add ())
  end;
  ignore (dialog#connect#response
    ~callback:(function `DELETE_EVENT | `OK -> dialog#destroy ()));
  dialog#show ()

let open_error parent = function
| File.Could_not_read_file (filename, exn) ->
    let details = Printexc.to_string exn in
    let utf8_filename = Glib.Convert.filename_to_utf8 filename in
    error_dialog ~details parent (get_err (`COULD_NOT_READ_FILE utf8_filename))
| File.File_does_not_exist filename ->
    let utf8_filename = Glib.Convert.filename_to_utf8 filename in
    error_dialog parent (get_err (`FILE_DOES_NOT_EXIST utf8_filename))
| File.No_file_specified ->  ()
| exn ->
    let e = Printexc.to_string exn in
    error_dialog parent (get_err (`GENERIC_ERROR e))

let save_error parent = function
| File.Could_not_write_file (filename, exn) ->
    let details = Printexc.to_string exn in
    let utf8_filename = Glib.Convert.filename_to_utf8 filename in
    error_dialog ~details parent (get_err (`COULD_NOT_WRITE_FILE utf8_filename))
| exn ->
    let e = Printexc.to_string exn in
    error_dialog parent (get_err (`GENERIC_ERROR e))

let all_files_filter = ((fun () -> get_str `ALL_FILES), ["*"])
let cool_file_filters = [
  ((fun () -> get_str `COOL_SOURCE), ["*.cool"]);
  all_files_filter;
]
let pdf_file_filters = [
  ((fun () -> get_str `PDF_FILE), ["*.pdf"]);
  all_files_filter;
]
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

let menu_builder ?accel_group menu =
  let modiffiers = [`CONTROL; `SHIFT; `MOD1] in
  let cmp a b =
    if a = b then 0 else
      let rec find x i = function
      | [] -> raise Not_found
      | h :: tl -> if h = x then i else find x (i + 1) tl in
      find a 0 modiffiers - find b 0 modiffiers in
  let rec combinations = function
  | [] -> [[]]
  | h :: tl ->
      let l = combinations tl in
      List.append (List.map (fun x -> h :: x) l) l in
  let factories =
    List.map
      (fun l -> (l, new GMenu.factory menu ?accel_group ~accel_modi:l))
      (combinations modiffiers) in
  object
    initializer menu#misc#hide ()
    method item ?(modi = [`CONTROL]) ?sensitive ?(disabled = false) text key
                callback =
      let factory = List.assoc (List.sort cmp modi) factories in
      let item = factory#add_item ~key ~callback text in
      if disabled then item#misc#set_sensitive false;
      match sensitive with
      | None -> ()
      | Some get_sensitive ->
          ignore (menu#misc#connect#show
            ~callback:(fun () -> item#misc#set_sensitive (get_sensitive ())));
          ignore (menu#misc#connect#hide
            ~callback:(fun () -> item#misc#set_sensitive (not disabled)))
    method separator =
      let (_, factory) = List.hd factories in
      ignore (factory#add_separator ())
  end

class label_signals obj ~close_button_clicked =
object
  val after = false
  inherit GObj.misc_signals obj
  inherit GUtil.add_ml_signals obj [close_button_clicked#disconnect]
  method close_button_clicked : callback:(unit -> unit) -> GtkSignal.id =
    close_button_clicked#connect ~after
end

class label () =
  let hbox = GPack.hbox () in
  let label = GMisc.label ~packing:hbox#pack () in
  let label_name = label#misc#name in
  let close_button = GBin.event_box ~packing:hbox#pack () in
  let close_image =
    GMisc.image ~pixbuf:Icons.tab_close_gray ~packing:close_button#add () in
object (self)
  inherit GObj.widget hbox#as_widget

  val mutable m_text = ""
  val mutable m_tooltip = ""

  val close_button_clicked = new GUtil.signal ()

  initializer
    label#set_xalign 0.0;
    close_button#misc#set_property "visible-window" (`BOOL false);
    close_button#event#add [`BUTTON_RELEASE];
    ignore (close_button#event#connect#button_release
      ~callback:(fun event ->
        if (GdkEvent.Button.button event) = 1
        then (close_button_clicked#call (); true)
        else false));
    ignore (close_button#event#connect#enter_notify
      ~callback:(fun _ -> close_image#set_pixbuf Icons.tab_close; false));
    ignore (close_button#event#connect#leave_notify
      ~callback:(fun _ ->
        close_image#set_pixbuf Icons.tab_close_gray;
        self#set_text_width ~-1;
        false));

  method text = m_text
  method tooltip = m_tooltip
  method text_width = label#misc#allocation.Gtk.width

  method connect = new label_signals self#as_widget ~close_button_clicked

  method set_text ?tooltip text =
    m_text <- text;
    m_tooltip <- default text tooltip;
    label#set_text (" " ^ text ^ " ");
    label#misc#set_tooltip_text m_tooltip
  method set_saved saved =
    label#misc#set_name (if saved then label_name else "modified-tab-label")
  method set_text_width width =
    label#misc#set_size_request ~width ~height:~-1 ();
end

type inspector_update_data =
[ `PROGRAM of Ast.program
| `BIG_STEP of Ast.program * Eval.tree *
    ((Ast.store, int) Hashtbl.t * (Ast.env, int) Hashtbl.t) option ref
| `ON_EXPORT_TO_PDF of
    (?callback:(exn option-> unit) -> string -> unit) -> unit ]

class base_inspector_signals obj ~idle =
object
  val after = false
  inherit GObj.misc_signals obj
  inherit GUtil.add_ml_signals obj [idle#disconnect]
  method idle : callback:(unit -> unit) -> GtkSignal.id =
    idle#connect ~after
end

class virtual base_inspector ?packing () =
  let event_box = GBin.event_box ~border_width:0 ?packing () in
object (self)
  inherit GBin.event_box (GtkBin.EventBox.cast event_box#as_widget)

  val idle = new GUtil.signal ()

  method update (data : inspector_update_data) = ()
  method clear = ()

  method parent =
    match event_box#misc#parent with
    | None -> failwith "parent"
    | Some widget -> widget

  method inspector_connect = new base_inspector_signals self#as_widget ~idle
end

class inspector_toolbar ?(separator = false) ?packing () =
  let toolbar = GButton.toolbar ?packing () in
object (self)
  val m_tooltips = GData.tooltips ()
  val mutable m_separator = None

  initializer
    toolbar#set_style `ICONS;
    toolbar#set_show_arrow true;
    toolbar#misc#set_can_focus false;
    toolbar#misc#set_size_request ~width:0 ();
    if separator then self#halve ()

  method widget = toolbar
  method has_separator = m_separator <> None

  method halve ?pos () =
    if not self#has_separator then
    begin
      let separator = GButton.separator_tool_item ~draw:false ~expand:true () in
      separator#misc#set_size_request ~width:1 ();
      toolbar#insert ?pos separator;
      m_separator <- Some separator;
    end

  method combine =
    match m_separator with
    | None -> ()
    | Some separator ->
        toolbar#remove separator#coerce;
        m_separator <- None

  method separator_pos =
    match m_separator with
    | None -> None
    | Some separator -> Some (toolbar#get_item_index separator)

  method adjust_pos (side : [> `ABSOLUTE | `LEFT | `RIGHT ] as 'side) pos =
    let separator_pos = default ~-1 (self#separator_pos) in
    match side with
    | `ABSOLUTE -> pos
    | `LEFT ->
        if separator_pos = ~-1 then pos else Some (default separator_pos pos)
    | `RIGHT ->
        match pos with None -> None | Some pos -> Some (separator_pos + pos)

  method set_tooltip :
    'a. string option -> (< coerce : GObj.widget; .. > as 'a) -> unit =
    fun tooltip item -> m_tooltips#set_tip ?text:tooltip item#coerce

  method add_widget ?(side = `LEFT) ?pos ?tooltip widget =
    let pos = self#adjust_pos side pos in
    let tool_item = GButton.tool_item () in
    tool_item#add widget;
    may (fun text -> m_tooltips#set_tip ~text tool_item#coerce) tooltip;
    toolbar#insert ?pos tool_item;
    tool_item

  method add_button ?(side = `LEFT) ?pos ~text ~icon ~callback () =
    let pos = self#adjust_pos side pos in
    let button = GButton.tool_button () in
    button#set_label text;
    button#set_icon_widget icon;
    m_tooltips#set_tip ~text button#coerce;
    ignore (button#connect#clicked callback);
    toolbar#insert ?pos button;
    button

  method insert :
    'a. ?side:'side -> ?pos:int -> (#GButton.tool_item_o as 'a) -> unit =
    fun ?(side = `LEFT) ?pos item ->
      let pos = self#adjust_pos side pos in
      toolbar#insert ?pos item
end

class virtual drawable_inspector ?packing () =
  let vbox = GPack.vbox ?packing () in
  let toolbar = new inspector_toolbar ~separator:true ~packing:vbox#pack () in
  let zoom_list = [4.0; 3.0; 2.0; 1.5; 1.0; 0.75; 0.5; 0.25; 0.1] in
  let string_of_zoom zoom =
    string_of_int (int_of_float (zoom *. 100.0 +. 0.4999)) ^ "%" in
  let zoom_of_string str = Scanf.sscanf str "%d" (fun i -> float i /. 100.0) in
  let strings = List.map string_of_zoom zoom_list in
  let zoom_text_combo = GEdit.combo_box_entry_text ~strings () in
  let scroll = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~shadow_type:`OUT ~packing:(vbox#pack ~expand:true) () in
  let drawing_area = GMisc.drawing_area () in
  let lazy_viewport =
    lazy (new GBin.viewport (GtkBin.Viewport.cast scroll#child#as_widget)) in
object (self)
  inherit base_inspector ?packing () as super

  val m_tooltips = GData.tooltips ()
  val mutable m_cairo_context = None
  val mutable m_panning = false
  val mutable m_scroll_to = None
  val mutable m_zoom = 1.0
  val mutable m_zooming = false
  val mutable m_get_pdf_filename = fun _ -> failwith "export_to_pdf"

  method virtual private drawing_size : Cairo.context -> float * float
  method virtual private repaint :
    ?cr:Cairo.context -> ?bounded:bool -> unit -> unit
  method virtual export_to_pdf : ?zoom:float -> unit -> unit

  initializer
    self#add vbox#coerce;
    ignore (self#toolbar#add_button ~side:`RIGHT ~text:(get_str `EXPORT_TO_PDF)
      ~icon:(Icons.export_to_pdf ()) ~callback:self#export_to_pdf ());
    ignore (self#toolbar#add_button ~side:`RIGHT ~text:(get_str `ZOOM_IN)
      ~icon:(Icons.zoom_in ()) ~callback:self#zoom_in ());
    ignore (self#toolbar#add_button ~side:`RIGHT ~text:(get_str `ZOOM_OUT)
      ~icon:(Icons.zoom_out ()) ~callback:self#zoom_out ());
    let (zoom_combo, _) = zoom_text_combo in
    ignore (self#toolbar#add_widget ~side:`RIGHT
      ~tooltip:(get_str `CURRENT_ZOOM) zoom_combo#coerce);
    let zoom_entry = zoom_combo#entry in
    zoom_entry#set_width_chars 5;
    scroll#add_with_viewport drawing_area#coerce;
    self#viewport#set_shadow_type `NONE;
    self#viewport#event#add
      [`BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `SCROLL];
    (* let redrawing = ref false in
    ignore (drawing_area#event#connect#expose
      ~callback:(fun _ ->
        redrawing := not !redrawing;
        if !redrawing then self#redraw else self#repaint ?cr:m_cairo_context ();
        false)); *)
    ignore (drawing_area#event#connect#expose
      ~callback:(fun _ -> self#repaint ?cr:m_cairo_context (); false));
    let pan_x = ref 0.0 and pan_y = ref 0.0 in
    ignore (self#viewport#event#connect#button_press
      ~callback:(fun event ->
        idle#call ();
        match GdkEvent.Button.button event with
        | 1 ->
            m_panning <- true;
            pan_x := GdkEvent.Button.x_root event;
            pan_y := GdkEvent.Button.y_root event;
            true
        | _ -> false));
    ignore (self#viewport#event#connect#button_release
      ~callback:(fun event ->
        match GdkEvent.Button.button event with
        | 1 when m_panning -> m_panning <- false; true
        | _ -> false));
    ignore (self#viewport#event#connect#motion_notify
      ~callback:(fun event ->
        if m_panning then
        begin
          let x = GdkEvent.Motion.x_root event in
          let y = GdkEvent.Motion.y_root event in
          self#scroll_to
            (scroll#hadjustment#value +. !pan_x -. x)
            (scroll#vadjustment#value +. !pan_y -. y);
          pan_x := x;
          pan_y := y;
        end;
        m_panning));
    ignore (self#viewport#event#connect#scroll
      ~callback:(fun event ->
        let alloc = scroll#misc#allocation in
        let x = GdkEvent.Scroll.x event -. scroll#hadjustment#value in
        let y = GdkEvent.Scroll.y event -. scroll#vadjustment#value in
        let pos = (x /. float alloc.Gtk.width, y /. float alloc.Gtk.height) in
        (match GdkEvent.Scroll.direction event with
        | `UP -> self#zoom_in ~pos ()
        | `DOWN -> self#zoom_out ~pos ()
        | _ -> ());
        true));
    let rescroll () =
      match m_scroll_to with
      | None -> ()
      | Some (h, v) -> self#scroll_to h v; m_scroll_to <- None in
    ignore (scroll#hadjustment#connect#changed ~callback:rescroll);
    ignore (scroll#vadjustment#connect#changed ~callback:rescroll);
    ignore (zoom_combo#connect#changed
      ~callback:(fun () ->
        if not m_zooming && zoom_combo#active_iter <> None then
        begin
          m_zooming <- true;
          self#set_zoom (List.nth zoom_list zoom_combo#active);
          m_zooming <- false;
          idle#call ();
        end));
    ignore (zoom_entry#connect#activate
      ~callback:(fun () ->
          if not m_zooming then
          begin
            m_zooming <- true;
            (try self#set_zoom (zoom_of_string zoom_entry#text) with _ -> ());
            self#update_zoom_combo_text;
            m_zooming <- false;
          end));
    zoom_entry#event#add [`BUTTON_PRESS; `FOCUS_CHANGE; `KEY_PRESS];
    ignore (zoom_entry#event#connect#button_press
      ~callback:(fun _ ->
        if not (Gobject.Property.get
          zoom_entry#as_widget GtkBase.Widget.P.has_focus) then
        begin
          zoom_entry#misc#grab_focus ();
          zoom_entry#select_region ~start:0 ~stop:~-1;
          true
        end else
          false));
    ignore (zoom_entry#event#connect#focus_out
      ~callback:(fun _ ->
        zoom_entry#set_position ~-1;
        idle#call ();
        false));
    ignore (zoom_entry#event#connect#key_press
      ~callback:(fun event ->
        if GdkEvent.Key.keyval event = GdkKeysyms._Escape then idle#call ();
        false));
    zoom_combo#event#add [`SCROLL];
    ignore (zoom_combo#event#connect#scroll
      ~callback:(fun event ->
        (match GdkEvent.Scroll.direction event with
        | `UP -> self#zoom_in ()
        | `DOWN -> self#zoom_out ()
        | _ -> ());
        true))

  method! update = function
  | `ON_EXPORT_TO_PDF f -> m_get_pdf_filename <- f
  | data -> super#update data

  method! clear =
    super#clear;
    drawing_area#misc#draw None

  method redraw = drawing_area#misc#draw None

  method private update_zoom_combo_text =
    let (zoom_combo, _) = zoom_text_combo in
    let rec get_active n = function
    | [] -> raise Not_found
    | h :: tl -> if h = m_zoom then n else get_active (n + 1) tl in
    (try zoom_combo#set_active (get_active 0 zoom_list)
    with Not_found -> zoom_combo#entry#set_text (string_of_zoom m_zoom));
    idle#call ()

  method private toolbar : inspector_toolbar = toolbar
  method private viewport = Lazy.force lazy_viewport
  method private drawing_area = drawing_area
  method private cairo_context =
    let cr = Cairo_gtk.create drawing_area#misc#window in
    Cairo.select_font_face cr (Pango.Font.to_string monospace);
    cr

  method private erase ?(cr = self#cairo_context) () =
    Cairo.set_source_rgb cr 1.0 1.0 1.0;
    Cairo.paint cr

  method private update_size =
    let (width, height) = self#drawing_size self#cairo_context in
    self#drawing_area#set_size (int_of_float width) (int_of_float height)

  method private scroll_to x y =
    let hmax = scroll#hadjustment#upper -. scroll#hadjustment#page_size in
    let vmax = scroll#vadjustment#upper -. scroll#vadjustment#page_size in
    scroll#misc#freeze_notify ();
    scroll#hadjustment#set_value (max (min x hmax) 0.0);
    scroll#vadjustment#set_value (max (min y vmax) 0.0);
    scroll#misc#thaw_notify ()

  method set_zoom ?(pos = (0.5, 0.5)) zoom =
    try
      let zoom = min (max zoom 0.1) 10.0 in
      let d = zoom /. m_zoom in
      let (scaled_x, scaled_y) = pos in
      let (width, height) = self#drawing_size self#cairo_context in
      let alloc = scroll#misc#allocation in
      let x = scaled_x *. float alloc.Gtk.width  in
      let y = scaled_y *. float alloc.Gtk.height in
      let start_x = 0.5 *. max (float alloc.Gtk.width  -. width ) 0.0 in
      let start_y = 0.5 *. max (float alloc.Gtk.height -. height) 0.0 in
      let scroll_h = (x +. scroll#hadjustment#value -. start_x) *. d -. x in
      let scroll_v = (y +. scroll#vadjustment#value -. start_y) *. d -. y in
      m_zoom <- zoom;
      m_scroll_to <- Some (scroll_h, scroll_v);
      if not m_zooming then
      begin
        m_zooming <- true;
        self#update_zoom_combo_text;
        m_zooming <- false;
      end;
      self#update_size
    with _ -> ()

  method zoom_in ?pos () =
    let rec prev_zoom p = function
    | [] -> p
    | h :: tl -> if h > m_zoom then prev_zoom h tl else p in
    self#set_zoom ?pos (prev_zoom m_zoom zoom_list)

  method zoom_out ?pos () =
    let rec next_zoom = function
    | [] -> m_zoom
    | h :: tl -> if h < m_zoom then h else next_zoom tl in
    self#set_zoom ?pos (next_zoom zoom_list)

  method bounds ?(with_zoom = true) () =
    let alloc = scroll#misc#allocation in
    let zoom = if with_zoom then m_zoom else 1.0 in
    let x0 = scroll#hadjustment#value /. zoom in
    let y0 = scroll#vadjustment#value /. zoom in
    let x1 = x0 +. float alloc.Gtk.width  /. zoom in
    let y1 = y0 +. float alloc.Gtk.height /. zoom in
    ((x0, y0), (x1, y1))
end

class ast_inspector ?packing () =
  let methods_text_combo = GEdit.combo_box_text () in
object (self)
  inherit drawable_inspector ?packing () as super

  val mutable m_tree = None
  val mutable m_methods = []

  initializer
    let (methods_combo, _) = methods_text_combo in
    let metrics = methods_combo#misc#pango_context#get_metrics () in
    let char_width = GPango.to_pixels metrics#approx_char_width in
    let methods_combo_width = 25 * char_width in
    methods_combo#misc#set_size_request ~width:methods_combo_width ();
    methods_combo#set_focus_on_click false;
    ignore (self#toolbar#add_widget ~tooltip:(get_str `METHOD_TO_ANALYZE)
      methods_combo#coerce);
    ignore (methods_combo#connect#changed
      ~callback:(fun () ->
        try
          let expr = List.nth m_methods methods_combo#active in
          let f cr =
            Cairo.save cr;
            Cairo.scale cr m_zoom m_zoom;
            let result = Visualization.Ast.generate cr expr in
            Cairo.restore cr;
            result in
          m_tree <- Some (`UNEVALUATED f);
          super#redraw;
          idle#call ()
        with _ -> ()))

  method! update = function
  | `PROGRAM program ->
      self#clear;
      let i = ref 0 and active = ref 0 in
      let (methods_combo, _) = methods_text_combo in
      m_methods <- List.flatten (List.map
        (fun classe ->
          List.map
            (fun m ->
              let class_id = Ast.string_of_ty (Ast.class_type classe) in
              let method_id = Ast.string_of_id (Ast.feature_id m) in
              let label = class_id ^ "." ^ method_id in
              GEdit.text_combo_add methods_text_combo label;
              if label = "Main.Main" then active := !i;
              incr i;
              Ast.method_body m)
            (Ast.class_methods classe))
        (Ast.program_classes program));
      methods_combo#set_active !active;
      self#update_zoom_combo_text
  | data -> super#update data

  method! clear =
    let (_, (list_store, _)) = methods_text_combo in
    list_store#clear ();
    m_tree <- None;
    m_methods <- [];
    super#clear;

  method private drawing_size cairo_context =
    let tree = self#tree cairo_context in
    let width  = m_zoom *. tree.Visualization.Ast.full_width  in
    let height = m_zoom *. tree.Visualization.Ast.full_height in
    (width, height)

  method private repaint ?cr ?(bounded = true) () =
    m_cairo_context <- cr;
    match m_tree with
    | None -> self#erase ?cr (); m_cairo_context <- None
    | Some (`UNEVALUATED f) -> self#update_size
    | Some (`READY (tree', _, f)) ->
        let cr = match m_cairo_context with
        | None -> self#cairo_context
        | Some cr -> cr in
        let tree = if bounded then tree' else f cr in
        Cairo.save cr;
        self#erase ~cr ();
        if bounded then
          let alloc = self#drawing_area#misc#allocation in
          let width  = m_zoom *. tree.Visualization.Ast.full_width  in
          let height = m_zoom *. tree.Visualization.Ast.full_height in
          Cairo.translate cr
            (0.5 *. (float alloc.Gtk.width  -. width))
            (0.5 *. (float alloc.Gtk.height -. height));
        Cairo.scale cr m_zoom m_zoom;
        let bounds = if bounded then Some (self#bounds ()) else None in
        Visualization.Ast.draw cr ?bounds tree;
        Cairo.restore cr;
        m_cairo_context <- None

  method export_to_pdf ?(zoom = 1.0) () =
    let callback ?callback filename =
      let prev_zoom = m_zoom in
      m_zoom <- zoom;
      let exn =
        try
          let cr' = Cairo.create (Cairo.PDF.create filename 1.0 1.0) in
          Cairo.select_font_face cr' (Pango.Font.to_string monospace);
          Cairo.scale cr' zoom zoom;
          let (width, height) = self#drawing_size cr' in
          Cairo.Surface.finish (Cairo.get_target cr');
          let cr = Cairo.create (Cairo.PDF.create filename width height) in
          Cairo.select_font_face cr (Pango.Font.to_string monospace);
          Cairo.scale cr zoom zoom;
          Visualization.Ast.draw cr (self#tree cr);
          Cairo.Surface.finish (Cairo.get_target cr);
          None
        with exn -> Some exn in
      m_zoom <- prev_zoom;
      may_call callback exn () in
    m_get_pdf_filename callback
    
  method private tree cairo_context =
    match m_tree with
    | None -> failwith "ast_inspector#tree"
    | Some (`UNEVALUATED f) ->
        let tree = f cairo_context in
        m_tree <- Some (`READY (tree, m_zoom, f));
        tree
    | Some (`READY (_, zoom, f)) when zoom <> m_zoom ->
        m_tree <- Some (`UNEVALUATED f);
        self#tree cairo_context
    | Some (`READY (tree, _, _)) -> tree
end

let make_cache () = ref None

let extract_cache f cache =
  match !cache with None -> let v = f () in cache := Some v; v | Some v -> v

let eval_cache tree () =
  let store_table = Visualization.BigStep.build_store_table tree in
  let env_table   = Visualization.BigStep.build_env_table   tree in
  (store_table, env_table)

class eval_inspector ?packing () =
object (self)
  inherit drawable_inspector ?packing () as super

  val mutable m_tree = None

  method! update = function
  | `BIG_STEP (program, tree, cache) ->
      self#clear;
      let f cr =
        Cairo.save cr;
        Cairo.scale cr m_zoom m_zoom;
        let (store_table, env_table) = extract_cache (eval_cache tree) cache in
        let result =
          Visualization.BigStep.generate
            cr program store_table env_table tree in
        Cairo.restore cr;
        result in
      m_tree <- Some (`UNEVALUATED f);
      super#redraw;
      self#update_zoom_combo_text;
      idle#call ()
  | data -> super#update data

  method! clear =
    m_tree <- None;
    super#clear;

  method private drawing_size cairo_context =
    let tree = self#tree cairo_context in
    let width  = m_zoom *. tree.Visualization.BigStep.full_width  in
    let height = m_zoom *. tree.Visualization.BigStep.full_height in
    (width, height)

  method private repaint ?cr ?(bounded = true) () =
    m_cairo_context <- cr;
    match m_tree with
    | None -> self#erase ?cr (); m_cairo_context <- None
    | Some (`UNEVALUATED f) -> self#update_size
    | Some (`READY (tree', _, f)) ->
        let cr = match m_cairo_context with
        | None -> self#cairo_context
        | Some cr -> cr in
        let tree = if bounded then tree' else f cr in
        Cairo.save cr;
        self#erase ~cr ();
        if bounded then
          let alloc = self#drawing_area#misc#allocation in
          let width  = m_zoom *. tree.Visualization.BigStep.full_width  in
          let height = m_zoom *. tree.Visualization.BigStep.full_height in
          Cairo.translate cr
            (0.5 *. (float alloc.Gtk.width  -. width))
            (0.5 *. (float alloc.Gtk.height -. height));
        Cairo.scale cr m_zoom m_zoom;
        let bounds = if bounded then Some (self#bounds ()) else None in
        Visualization.BigStep.draw cr ?bounds tree;
        Cairo.restore cr;
        m_cairo_context <- None

  method export_to_pdf ?(zoom = 1.0) () =
    let callback ?callback filename =
      let prev_zoom = m_zoom in
      m_zoom <- zoom;
      let exn =
        try
          let cr' = Cairo.create (Cairo.PDF.create filename 1.0 1.0) in
          Cairo.select_font_face cr' (Pango.Font.to_string monospace);
          Cairo.scale cr' zoom zoom;
          let (width, height) = self#drawing_size cr' in
          Cairo.Surface.finish (Cairo.get_target cr');
          let cr = Cairo.create (Cairo.PDF.create filename width height) in
          Cairo.select_font_face cr (Pango.Font.to_string monospace);
          Cairo.scale cr zoom zoom;
          Visualization.BigStep.draw cr (self#tree cr);
          Cairo.Surface.finish (Cairo.get_target cr);
          None
        with exn -> Some exn in
      m_zoom <- prev_zoom;
      may_call callback exn () in
    m_get_pdf_filename callback

  method private tree cairo_context =
    match m_tree with
    | None -> failwith "ast_inspector#tree"
    | Some (`UNEVALUATED f) ->
        let tree = f cairo_context in
        m_tree <- Some (`READY (tree, m_zoom, f));
        tree
    | Some (`READY (_, zoom, f)) when zoom <> m_zoom ->
        m_tree <- Some (`UNEVALUATED f);
        self#tree cairo_context
    | Some (`READY (tree, _, _)) -> tree
end

class mem_inspector ?packing () =
  let vbox = GPack.vbox ?packing () in
  let toolbar = new inspector_toolbar ~separator:true ~packing:vbox#pack () in
  let kinds_text_combo = GEdit.combo_box_text () in
  let mem_text_combo = GEdit.combo_box_text () in
  let scroll = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~shadow_type:`OUT ~packing:(vbox#pack ~expand:true) () in
  let tree_view = GTree.view () in
  let lazy_viewport =
    lazy (new GBin.viewport (GtkBin.Viewport.cast scroll#child#as_widget)) in
object (self)
  inherit base_inspector ?packing () as super

  val mutable m_envs = [||]
  val mutable m_stores = [||]
  val mutable m_kinds = [||]

  val mutable m_get_env_name   = fun (n, _) -> string_of_int n
  val mutable m_get_store_name = fun (n, _) -> string_of_int n

  val mutable m_prev = []

  val m_tooltips = GData.tooltips ()
  val mutable m_columns_count = 0
  val mutable m_clearing = false
  val mutable m_assoc_text_combo = None

  initializer
    self#add vbox#coerce;
    scroll#add_with_viewport tree_view#coerce;
    self#viewport#set_shadow_type `NONE;

    let (kinds_combo, _) = kinds_text_combo in
    kinds_combo#set_focus_on_click false;
    ignore
      (let tooltip = get_str `KIND_OF_PROGRAM_STATE_TO_ANALYZE in
      toolbar#add_widget ~tooltip kinds_combo#coerce);
    ignore (kinds_combo#connect#changed ~callback:self#populate_mem);

    toolbar#insert (GButton.separator_tool_item ~draw:false ());

    let (mem_combo, _) = mem_text_combo in
    mem_combo#set_focus_on_click false;
    ignore
      (let tooltip = get_str `PROGRAM_STATE_TO_ANALYZE in
      toolbar#add_widget ~tooltip mem_combo#coerce);
    ignore (mem_combo#connect#changed ~callback:self#populate_list);


  method private viewport = Lazy.force lazy_viewport

  method! update = function
  | `BIG_STEP (_, tree, cache) ->
      let (store_table, env_table) = extract_cache (eval_cache tree) cache in
      let array_of_hashtbl table =
        let i = ref 0 in
        let arr = Array.make (Hashtbl.length table) (Obj.magic ()) in
        Hashtbl.iter (fun v n -> arr.(!i) <- (n, v); incr i) table;
        Array.sort (fun (i, _) (j, _) -> compare i j) arr;
        arr in
      m_stores <- array_of_hashtbl store_table;
      m_envs   <- array_of_hashtbl env_table;
      let name f (_, v) = f v in
      m_get_store_name <- name (Visualization.BigStep.store_name store_table);
      m_get_env_name <-   name (Visualization.BigStep.env_name   env_table);
      self#clearing (fun () -> self#remove_assoc_combo);
      m_prev <- [];
      self#populate_kinds ()
  | data -> super#update data

  method private clearing f = m_clearing <- true; f (); m_clearing <- false;

  method private add_assoc_combo =
    match m_assoc_text_combo with
    | None ->
        let assoc_text_combo = GEdit.combo_box_text () in
        let (assoc_combo, _) = assoc_text_combo in
        assoc_combo#set_focus_on_click false;
        let tooltip = get_str `PROGRAM_STATE_TO_ANALYZE in
        let toolbar_item = toolbar#add_widget ~tooltip assoc_combo#coerce in
        ignore (assoc_combo#connect#changed ~callback:self#populate_list);
        m_assoc_text_combo <- Some (assoc_text_combo, toolbar_item);
        assoc_text_combo
    | Some (assoc_text_combo, _) -> assoc_text_combo

  method private remove_assoc_combo =
    match m_assoc_text_combo with
    | None -> ()
    | Some (_, toolbar_item) ->
        toolbar#widget#remove toolbar_item#coerce;
        m_assoc_text_combo <- None

  method private populate_kinds () =
    if not m_clearing then
    begin
      let (kinds_combo, (list_store, _)) = kinds_text_combo in
      let cur_kind = try m_kinds.(kinds_combo#active) with _ -> `NONE in
      self#clearing (fun () -> list_store#clear ());
      let kinds = ref [] in
      let index = ref 0 and active = ref None in
      let add_kind kind label =
        GEdit.text_combo_add kinds_text_combo label;
        if !active = None && cur_kind = kind then active := Some !index;
        kinds := kind :: !kinds;
        incr index in
      if Array.length m_envs   > 0 then add_kind `ENV   (get_str `ENVIRONMENT);
      if Array.length m_stores > 0 then add_kind `STORE (get_str `STORE);
      if Array.length m_envs > 0 && Array.length m_stores > 0 then
        add_kind `ENV_STORE (get_str `ENV_STORE_ASSOC);
      m_kinds <- Array.of_list (List.rev !kinds);
      kinds_combo#set_active (default 0 !active);
    end

  method private populate_mem () =
    if not m_clearing then
    begin
      let (kinds_combo, _) = kinds_text_combo in
      let (_, (list_store, _)) = mem_text_combo in
      self#clearing (fun () -> list_store#clear (); self#remove_assoc_combo);
      let cur_kind = m_kinds.(kinds_combo#active) in
      let active_list = ref [] in
      let fill_combo ?(kind = cur_kind) ?(combo = mem_text_combo) arr f =
        let prev = try Some (List.assoc kind m_prev) with Not_found -> None in
        Array.iteri
          (fun i v ->
            let text = f v in
            GEdit.text_combo_add combo text;
            let activated =
              try let (_, b) = List.assoc kind !active_list in b
              with Not_found -> false in
            if not activated then
              let (c, _) = combo in
              let b = match prev with None -> false | Some s -> text = s in
              let f () = c#set_active (if b then i else 0) in
              active_list :=
                (kind, (f, b)) :: (List.remove_assoc kind !active_list))
          arr in
      (match cur_kind with
      | `ENV   -> fill_combo m_envs   m_get_env_name
      | `STORE -> fill_combo m_stores m_get_store_name
      | `ENV_STORE ->
          let combo = self#add_assoc_combo in
          fill_combo m_envs   m_get_env_name   ~kind:`ENV;
          fill_combo m_stores m_get_store_name ~kind:`STORE ~combo
      | `NONE -> assert false);
      List.iter (fun (_, (f, _)) -> self#clearing f) !active_list;
      self#populate_list ()
    end

  method private populate_list () =
    if not m_clearing then
    begin
      let (kinds_combo, _) = kinds_text_combo in
      let (mem_combo, _) = mem_text_combo in
      let rec remove_columns = function
      | 0 -> m_columns_count <- 0
      | i ->
          ignore (tree_view#remove_column (tree_view#get_column (i - 1)));
          remove_columns (i - 1) in
      remove_columns m_columns_count;

      let column_list = new GTree.column_list in
      let new_column title =
        let title = " " ^ title ^ " " in
        let column = column_list#add Gobject.Data.string in
        let renderer = (GTree.cell_renderer_text [], [("text", column)]) in
        let view_column = GTree.view_column ~title ~renderer () in
        m_columns_count <- tree_view#append_column view_column;
        column in
      let tree_store = lazy (GTree.tree_store column_list) in
      let new_row () = (Lazy.force tree_store)#append () in
      let set_value row column v =
        (Lazy.force tree_store)#set ~row ~column v in

      let remember kind combo =
        match GEdit.text_combo_get_active combo with
        | None -> ()
        | Some text ->
            m_prev <- (kind, text) :: (List.remove_assoc kind m_prev) in

      (match m_kinds.(kinds_combo#active) with
      | `ENV ->
          let id_col  = new_column (get_str `IDENTIFIER) in
          let loc_col = new_column (get_str `LOCATION) in
          let (_, env) = m_envs.(mem_combo#active) in
          remember `ENV mem_text_combo;
          List.iter
            (fun (id, loc) ->
              let row = new_row () in
              set_value row id_col  (Ast.string_of_id id);
              set_value row loc_col (Store.string_of_location loc))
            (Ast.Environment.to_assoc_list env);
      | `STORE ->
          let loc_col = new_column (get_str `LOCATION) in
          let val_col = new_column (get_str `VALUE) in
          let (_, store) = m_stores.(mem_combo#active) in
          remember `STORE mem_text_combo;
          Store.iteri
            (fun loc v ->
              let row = new_row () in
              set_value row loc_col (Store.string_of_location loc);
              set_value row val_col (Printty.string_of_value v))
            store;
      | `ENV_STORE ->
          let id_col  = new_column (get_str `IDENTIFIER) in
          let loc_col = new_column (get_str `LOCATION) in
          let val_col = new_column (get_str `VALUE) in
          let (assoc_text_combo, assoc_combo) =
            match m_assoc_text_combo with
            | None -> assert false
            | Some (((combo, _) as text_combo), _) -> (text_combo, combo) in
          let (_, env) = m_envs.(mem_combo#active) in
          let (_, store) = m_stores.(assoc_combo#active) in
          remember `ENV mem_text_combo;
          remember `STORE assoc_text_combo;
          List.iter
            (fun (id, loc) ->
              let row = new_row () in
              set_value row id_col  (Ast.string_of_id id);
              set_value row loc_col (Store.string_of_location loc);
              try
                let v = Store.lookup store loc in
                set_value row val_col (Printty.string_of_value v)
              with Not_found ->
                set_value row val_col "-")
            (Ast.Environment.to_assoc_list env);
      | `NONE -> assert false);
      tree_view#set_model (Some (Lazy.force tree_store)#coerce);
      idle#call ();
    end
end

class ['a] inspector_bar_signals obj ~toggled ~idle =
object
  val after = false
  inherit GObj.misc_signals obj
  inherit GUtil.add_ml_signals obj [toggled#disconnect; idle#disconnect]
  method toggled : callback:(('a * bool * int) -> unit) -> GtkSignal.id =
    toggled#connect ~after
  method idle : callback:('a -> unit) -> GtkSignal.id =
    idle#connect ~after
end

class inspector_bar ?packing () =
  let hbox = GPack.hbox ?packing () in
  let hbox_pack : GObj.widget -> unit = hbox#pack ~expand:true ~from:`END in
  let as_paned w = new GPack.paned (GtkPack.Paned.cast w#as_widget) in
  let parent_pack ?remove parent =
    try
      let parent = as_paned parent in
      may parent#remove remove;
      (parent#pack2 : GObj.widget -> unit)
    with _ ->
      may hbox#remove remove;
      hbox_pack in
  let toolbar =
    GButton.toolbar ~orientation:`VERTICAL ~packing:(hbox#pack ~from:`END) () in
object (self)
  inherit GObj.widget hbox#as_widget

  val mutable m_inspectors = []
  val mutable m_program = None
  val mutable m_big_step = None
  val mutable m_on_export_to_pdf = None

  val toggled = new GUtil.signal ()
  val idle = new GUtil.signal ()

  initializer
    toolbar#set_show_arrow true;
    toolbar#misc#set_can_focus false;
    let insert_button tooltip icon id =
      let button = toolbar#insert_toggle_button ~tooltip ~icon () in
      button#set_relief `NONE;
      let callback () = if button#active then self#show id else self#hide id in
      ignore (button#connect#toggled ~callback) in
    insert_button (get_str `AST) (Icons.inspector_ast ()) `AST;
 (* insert_button (get_str `TYPE_DERIV) (Icons.inspector_type ()) `TYPE; *)
    insert_button (get_str `BIG_STEP_DERIV) (Icons.inspector_eval ()) `EVAL;
    insert_button (get_str `STORE_AND_ENVS) (Icons.inspector_mem ()) `MEM;
 (* insert_button (get_str `WATCH_LIST) (Icons.inspector_watch ()) `WATCH *)

  method private update_list =
    let add l f = function None -> () | Some v -> l := f v :: !l in
    let l = ref [] in
    add l (fun v -> `PROGRAM v) m_program;
    add l (fun v -> `BIG_STEP v) m_big_step;
    add l (fun v -> `ON_EXPORT_TO_PDF v) m_on_export_to_pdf;
    !l

  method private new_inspector id =
    let (_, widget) as inspector = match id with
    | `AST   -> (0, (new   ast_inspector () :> base_inspector))
 (* | `TYPE  -> (1, (new  type_inspector () :> base_inspector)) *)
    | `EVAL  -> (2, (new  eval_inspector () :> base_inspector))
    | `MEM   -> (3, (new   mem_inspector () :> base_inspector)) in
    List.iter (fun data -> widget#update data) self#update_list;
    ignore (widget#inspector_connect#idle (fun () -> idle#call id));
    inspector

  method show id =
    if not (List.mem_assoc id m_inspectors) then
      let pack ((_, widget) as e) prev tl =
        if prev = None && tl = [] then
          (hbox_pack widget#coerce; (id, e))
        else
          let (to_move, pack_pos) = match (prev, tl) with
          | (Some (_, (_, w)), []) -> (w#coerce, 1)
          | (_, [(_, (_, w))]) -> (w#coerce, 2)
          | (_, (_, (_, w)) :: _) -> (w#parent#coerce, 2)
          | _ -> assert false in
          let packing = parent_pack ~remove:to_move
            (match prev with Some (_, (_, w)) -> w#parent | _ -> hbox#coerce) in
          let paned = GPack.paned `HORIZONTAL ~packing () in
          let p i = if i = pack_pos then to_move else widget#coerce in
          paned#pack1 (p 1) ~resize:true;
          paned#pack2 (p 2);
          (id, e) in
      let rec insert ?prev ((i, _) as e) = function
      | [] -> [pack e prev []]
      | (h :: _) as l when i < fst (snd h) -> pack e prev l :: l
      | h :: tl -> h :: insert ~prev:h e tl in
      m_inspectors <- insert (self#new_inspector id) m_inspectors;
      toggled#call (id, true, List.length m_inspectors)

  method hide id =
    try
      let (_, widget) = List.assoc id m_inspectors in
      (if List.length m_inspectors = 1 then
        hbox#remove widget#coerce
      else
        let paned = as_paned widget#parent in
        paned#remove widget#coerce;
        let other = List.hd paned#children in
        paned#remove other;
        match paned#misc#parent with
        | None -> assert false
        | Some parent -> parent_pack ~remove:paned#coerce parent other);
      m_inspectors <- List.remove_assoc id m_inspectors;
      toggled#call (id, false, List.length m_inspectors)
    with _ -> ()

  method connect = new inspector_bar_signals hbox#as_widget ~toggled ~idle

  method update data =
    List.iter (fun (_, (_, widget)) -> widget#update data) m_inspectors;
    match data with
    | `PROGRAM          v -> m_program          <- Some v
    | `BIG_STEP         v -> m_big_step         <- Some v
    | `ON_EXPORT_TO_PDF v -> m_on_export_to_pdf <- Some v
end

class tabbed_document ?(auto = false) () =
  let hbox = GPack.hbox () in
  let label = new label () in
  let scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~shadow_type:`OUT ~packing:(hbox#pack ~expand:true) () in
  let source_view =
    GSourceView2.source_view ~smart_home_end:`BEFORE ~right_margin_position:80
      ~show_line_marks:true ~show_line_numbers:true ~show_right_margin:true
      ~auto_indent:true ~highlight_current_line:true ~indent_on_tab:true
      ~packing:scroll#add () in
  let inspector_bar = new inspector_bar ~packing:hbox#pack () in
object (self)
  inherit GObj.widget hbox#as_widget

  val mutable m_filename = None
  val mutable m_auto_opened = auto

  initializer
    GtkBase.Widget.modify_font source_view#as_widget monospace;
    source_view#source_buffer#set_highlight_matching_brackets true;
    source_view#source_buffer#begin_not_undoable_action ();
    source_view#source_buffer#set_text (get_msg `HELLO_WORLD_EXAMPLE);
    source_view#source_buffer#set_modified false;
    source_view#source_buffer#end_not_undoable_action ();
    ignore (source_view#source_buffer#connect#modified_changed
      ~callback:(fun () -> m_auto_opened <- false; label#set_saved self#saved));
    ignore (source_view#event#connect#key_press
      ~callback:(fun event ->
        let disabled_shortcuts = [
          (GdkKeysyms._Z, [`CONTROL; `SHIFT], []);
        ] in
        let keyval = GdkEvent.Key.keyval event in
        let key_state = GdkEvent.Key.state event in
        let match_keys (key, state, not_state) =
          keyval = key &&
            List.for_all (fun modi -> List.mem modi key_state) state &&
            not (List.exists (fun modi -> List.mem modi key_state) not_state) in
        List.exists match_keys disabled_shortcuts));
    ignore (source_view#connect#after#populate_popup
      ~callback:(fun menu ->
        let menu = new GMenu.menu menu in
        let remove_icon menu_item =
          try
            let obj = GtkMenu.ImageMenuItem.cast menu_item#as_widget in
            let null_ptr = Obj.magic Gpointer.boxed_null in
            Gobject.set GtkMenu.ImageMenuItem.P.image obj null_ptr
          with Gobject.Cannot_cast (_, _) -> () in
        List.iter remove_icon menu#children));
    ignore (source_view#drag#connect#drop
      ~callback:(fun context ~x ~y ~time ->
        if List.mem "text/uri-list" context#targets then
          let toplevel = source_view#misc#toplevel#as_widget in
          let target = Gdk.Atom.intern "text/uri-list" in
          GtkBase.DnD.get_data toplevel context#context ~target ~time;
          true
        else
          false));
    self#set_filename None;
    let set_panned =
      let paned_ref = ref None in
      fun set ->
        match (set, !paned_ref) with
        | (true, None) ->
            hbox#remove scroll#coerce;
            hbox#remove inspector_bar#coerce;
            let packing = hbox#pack ~expand:true in
            let paned = GPack.paned `HORIZONTAL ~packing () in
            paned#pack1 ~resize:true scroll#coerce;
            paned#pack2 inspector_bar#coerce;
            paned_ref := Some paned
        | (false, Some paned) ->
            paned#remove scroll#coerce;
            paned#remove inspector_bar#coerce;
            hbox#remove paned#coerce;
            hbox#pack ~expand:true scroll#coerce;
            hbox#pack inspector_bar#coerce;
            paned_ref := None
        | _ -> () in
    ignore (inspector_bar#connect#toggled
      ~callback:(fun (_, _, len) -> set_panned (len > 0); self#focus));
    ignore (inspector_bar#connect#idle ~callback:(fun _ -> self#focus));
    let export_to_pdf pdf_callback =
      let dialog = ref (Obj.magic ()) in
      let save_fun ~callback filename =
        let callback =
          function
          | None -> callback true
          | Some exn ->
              open_error (!dialog :> GWindow.window_skel) exn;
              callback false in
        pdf_callback ?callback:(Some callback) filename in
      let parent = new GWindow.window (top_window hbox) in
      dialog :=
        Dialog.save_file ~parent ~title:(get_str `EXPORT_TO_PDF)
          ~filters:pdf_file_filters save_fun;
      (* (match document#filename with
      | None -> !dialog#set_current_name document#label#text
      | Some filename -> ignore (!dialog#set_filename filename)); *)
      !dialog#show () in
    inspector_bar#update (`ON_EXPORT_TO_PDF export_to_pdf)

  method filename = m_filename
  method auto_opened = m_auto_opened
  method saved = not source_view#source_buffer#modified

  method source_view = source_view
  method label = label

  method focus =
    GtkWindow.Window.set_focus (top_window hbox) source_view#as_widget

  method display_name full_path =
    match m_filename with
    | None -> (get_str `NEW_DOCUMENT_TITLE)
    | Some filename ->
        let utf8_filename = Glib.Convert.filename_to_utf8 filename in
        if full_path then utf8_filename else (Filename.basename utf8_filename)

  method set_filename filename =
    m_filename <- filename;
    label#set_text ~tooltip:(self#display_name true) (self#display_name false)

  method add_text ?(endl = "\n") ?(quot = "") ?l ?r text =
    let l = default quot l and r = default quot r in
    let buf = source_view#source_buffer in
    buf#set_text ((buf#get_text ()) ^ l ^ text ^ r ^ endl)

  method load filename =
    let file_contents = File.read filename in
    let source_buffer = source_view#source_buffer in
    source_buffer#begin_not_undoable_action ();
    source_buffer#set_text file_contents;
    source_buffer#place_cursor ~where:source_buffer#start_iter;
    source_buffer#end_not_undoable_action ();
    source_buffer#set_modified false;
    self#set_filename (Some filename)

  method save filename =
    File.write filename (source_view#buffer#get_text ());
    source_view#source_buffer#set_modified false;
    self#set_filename (Some filename)

  method update_visualizations data =
    inspector_bar#update data
end

class notebook ?packing () =
  let documents = new GUtil.memo () in
  let notebook = GPack.notebook ~scrollable:true ~tab_border:0 ?packing () in
object (self)
  inherit GObj.widget notebook#as_widget

  val mutable m_window = None

  initializer
    notebook#misc#set_can_focus false;
    ignore (notebook#connect#switch_page
      ~callback:(fun page -> (self#document page)#focus))

  method private window =
    let gtk_window = top_window notebook in
    match m_window with
    | None ->
        let window = new GWindow.window gtk_window in
        m_window <- Some (gtk_window, window);
        window
    | Some (prev_window, window) when prev_window == gtk_window -> window
    | _ -> m_window <- None; self#window
  method private window_skel = (self#window :> GWindow.window_skel)

  method notebook = notebook
  method document page = documents#find (notebook#get_nth_page page)
  method current_document = self#document notebook#current_page

  method previous_page =
    if notebook#current_page = 0
    then notebook#goto_page (List.length notebook#children - 1)
    else notebook#previous_page ()

  method next_page =
    if notebook#current_page = List.length notebook#children - 1
    then notebook#goto_page 0
    else notebook#next_page ()

  method new_document ?(auto = false) ?callback () =
    let document = new tabbed_document ~auto () in
    documents#add document;
    ignore (document#label#connect#close_button_clicked
      ~callback:(fun () ->
        let page = notebook#page_num document#coerce in
        if page < (List.length notebook#children - 1) then
          (self#document (page + 1))#label#set_text_width
            document#label#text_width;
        self#close_document ~page ()));
    notebook#goto_page
      (notebook#append_page ~tab_label:document#label#coerce document#coerce);
    notebook#set_tab_reorderable document#coerce true;
    document#focus;
    may_call callback document ()

  method open_document ?(error_parent = self#window_skel) ?callback filename =
    let last_document = self#document (List.length notebook#children - 1) in
    self#new_document ()
      ~callback:(fun document ->
        try
          document#load filename;
          if last_document#auto_opened then
            self#close_document
              ~page:(notebook#page_num last_document#coerce) ();
          may_call callback (Some document) ()
        with exn ->
          self#close_document ~page:(notebook#page_num document#coerce) ();
          open_error error_parent exn;
          may_call callback None ())

  method open_dialog ?(title = get_str `OPEN) ?callback () =
    let dialog = ref (Obj.magic ()) in
    let open_fun ~callback filename =
      let error_parent = (!dialog :> GWindow.window_skel) in
      self#open_document ~error_parent ~callback filename in
    dialog :=
      Dialog.open_file ~parent:self#window ~title ~filters:cool_file_filters
        ~multiple:true ?callback open_fun;
    !dialog#show ()

  method save_document ?(error_parent = self#window_skel)
                       ?(page = notebook#current_page) ?filename ?callback () =
    let document = self#document page in
    let filename =
      match filename with None -> document#filename | Some _ -> filename in
    match filename with
    | None -> self#save_dialog ~title:(get_str `SAVE) ~page ?callback ()
    | Some filename ->
        try
          document#save filename;
          may_call callback true ()
        with exn ->
          save_error error_parent exn;
          may_call callback false ()

  method save_dialog ?(title = get_str `SAVE_AS) ?(page = notebook#current_page)
                     ?callback () =
    let document = self#document page in
    let dialog = ref (Obj.magic ()) in
    let save_fun ~callback filename =
      let error_parent = (!dialog :> GWindow.window_skel) in
      self#save_document ~error_parent ~filename ~page ~callback () in
    dialog :=
      Dialog.save_file ~parent:self#window ~title ~filters:cool_file_filters
        ?callback save_fun;
    (match document#filename with
    | None -> !dialog#set_current_name document#label#text
    | Some filename -> ignore (!dialog#set_filename filename));
    !dialog#show ()

  method close_document ?(force = false) ?(page = notebook#current_page)
                        ?callback () =
    let document = self#document page in
    if force || document#saved then
    begin
      notebook#remove_page page;
      documents#remove document#coerce;
      if List.length notebook#children = 0 then self#new_document ~auto:true ();
      may_call callback true ()
    end else
      self#close_dialog ~page ?callback ()

  method close_dialog ?(page = notebook#current_page) ?(close = true)
                      ?callback () =
    let document = self#document page in
    let current_page = notebook#current_page in
    let callback closed =
      if closed && close then
        self#close_document ~force:true ~page ()
          ~callback:(fun closed ->
            if closed && current_page <> page then
              notebook#goto_page
                (current_page - if current_page > page then 1 else 0);
            may_call callback closed ())
      else
        may_call callback closed () in
    let message = get_msg (`SAVE_ON_CLOSE document#label#text) in
    let dialog =
      GWindow.message_dialog ~message ~parent:self#window
        ~message_type:`QUESTION ~title:(get_str `SAVE)
        ~buttons:GWindow.Buttons.yes_no ~modal:true () in
    let cancel =
      GButton.button ~stock:`CANCEL ~packing:dialog#action_area#pack () in
    cancel#misc#set_can_default true;
    ignore (dialog#connect#response
      ~callback:(function
      | `DELETE_EVENT -> dialog#destroy (); callback false
      | `YES -> dialog#destroy (); self#save_document ~page ~callback ()
      | `NO -> dialog#destroy (); callback true));
    ignore (cancel#connect#clicked
      ~callback:(fun () -> dialog#response `DELETE_EVENT));
    notebook#goto_page page;
    dialog#show ()

  method quit ?(msg_only = false) ?callback () =
    let finish ok = may_call callback ok () in
    let rec save_check = function
    | [] -> finish true; if not msg_only then GMain.Main.quit ()
    | h :: tl ->
        if (documents#find h)#saved then
          save_check tl
        else
          let page = notebook#page_num h in
          let callback ok = if ok then save_check tl else finish false in
          self#close_dialog ~close:false ~page ~callback () in
    save_check notebook#children
end

let start () =
  ignore (Glib.Main.setlocale `ALL (Some ""));
  Lang.set (Lang.system ());
  let rc_style =
    "style 'modified-tab-label'                                            " ^
    "{                                                                     " ^
    "  fg[NORMAL] = {1.0, 0.0, 0.0}                                        " ^
    "  fg[ACTIVE] = {1.0, 0.0, 0.0}                                        " ^
    "}                                                                     " ^
    "widget '*.modified-tab-label' style : application 'modified-tab-label'" in
  GtkMain.Rc.parse_string rc_style;

  let main_window =
    GWindow.window ~width:640 ~height:480 ~allow_shrink:true () in
  GtkSignal.user_handler := (fun exn ->
    let e = Printexc.to_string exn in
    error_dialog main_window (get_err (`GENERIC_ERROR e)));

  let main_vbox = GPack.vbox ~packing:main_window#add () in

  let menu_bar = GMenu.menu_bar ~packing:main_vbox#pack () in

  let toolbar = GButton.toolbar ~style:`ICONS ~packing:main_vbox#pack () in
  toolbar#set_show_arrow true;
  toolbar#misc#set_can_focus false;

  let main_paned =
    GPack.paned `VERTICAL ~packing:(main_vbox#pack ~expand:true) () in

  let notebook = new notebook ~packing:(main_paned#pack1 ~resize:true) () in
  let cur_buf () = notebook#current_document#source_view#source_buffer in

  ignore (main_window#event#connect#key_press
    ~callback:(fun event ->
      let k = GdkEvent.Key.keyval event in
      let m = GdkEvent.Key.state event in

      let k_tab = (k = GdkKeysyms._Tab) in
      let k_shift_tab = (k = GdkKeysyms._ISO_Left_Tab) in
      let m_ctrl = List.mem `CONTROL m in
      let m_shift = List.mem `SHIFT m in
      let shift_tab = (k_tab && m_shift) || k_shift_tab in

      if m_ctrl && shift_tab then (notebook#previous_page; true) else
      if m_ctrl && k_tab then (notebook#next_page; true) else
      if k = GdkKeysyms._Return && m_ctrl && List.mem `MOD1 m then
        try
          (cur_buf ())#set_text (Icons.string_code ((cur_buf ())#get_text ()));
          true
        with _ -> false
      else false));

  let update_window_title page =
    let label_text = (notebook#document page)#label#text in
    main_window#set_title (get_str (`APP_TITLE_WITH_FILE label_text)) in
  ignore (notebook#notebook#connect#switch_page ~callback:update_window_title);
  let on_save _ = update_window_title notebook#notebook#current_page in

  (* Commands *)
  let open_file filename = notebook#open_document ~callback:on_save filename in

  let file_new () = notebook#new_document () in
  let file_open () = notebook#open_dialog ~callback:on_save () in
  let file_save () = notebook#save_document ~callback:on_save () in
  let file_save_as () = notebook#save_dialog ~callback:on_save () in
  let file_close () = notebook#close_document () in
  let file_quit () = notebook#quit () in

  let edit_undo () = (cur_buf ())#undo () in
  let edit_redo () = (cur_buf ())#redo () in
  let edit_cut () = (cur_buf ())#cut_clipboard GMain.clipboard in
  let edit_copy () = (cur_buf ())#copy_clipboard GMain.clipboard in
  let edit_paste () = (cur_buf ())#paste_clipboard GMain.clipboard in
  let edit_delete () = ignore ((cur_buf ())#delete_selection ()) in
  let edit_select_all () =
    let buf = cur_buf () in buf#select_range buf#start_iter buf#end_iter in

  let run_compile_and_run () =
    try
      let update = notebook#current_document#update_visualizations in
      let filename = notebook#current_document#label#text in
      let basic = Parser.parse_file "basic.cool" in
      let code = (cur_buf ())#get_text () in

      let program = Parser.parse_string ~basic ~filename code in
      update (`PROGRAM program);

      Type.check_program program;

      let (eval_tree, error) = Eval.Rec.program program in
      update (`BIG_STEP (program, eval_tree, make_cache ()));
      match error with None -> () | Some exn -> raise exn
    with
    (* | Camlp4.PreCast.Loc.Exc_located _ as exn -> *)
    | exn ->
        let message = Camlp4.ErrorHandler.to_string exn in
        error_dialog main_window message in

  (* Menu bar *)
  let build_menu_bar () =
    let module K = GdkKeysyms in

    let factory = new GMenu.factory menu_bar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu (get_str `FILE) in
    let edit_menu = factory#add_submenu (get_str `EDIT) in
    let view_menu = factory#add_submenu (get_str `VIEW) in
    let run_menu = factory#add_submenu (get_str `RUN) in

    (* File menu *)
    let add = menu_builder ~accel_group file_menu in
    add#item (get_str `NEW) K._N file_new;
    add#item (get_str `OPEN) K._O file_open;
    add#item (get_str `SAVE) K._S file_save;
    add#item (get_str `SAVE_AS) ~modi:[`CONTROL; `MOD1] K._S file_save_as;
    add#item (get_str `CLOSE) K._F4 file_close;
    add#separator;
    add#item (get_str `QUIT) ~modi:[`MOD1] K._F4 file_quit;

    (* Edit menu *)
    let add = menu_builder ~accel_group edit_menu in
    add#item (get_str `UNDO) K._Z edit_undo
      ~sensitive:(fun () -> (cur_buf ())#can_undo);
    add#item (get_str `REDO) K._Y edit_redo
      ~sensitive:(fun () -> (cur_buf ())#can_redo);
    add#separator;
    add#item (get_str `CUT) K._X edit_cut
      ~sensitive:(fun () -> (cur_buf ())#has_selection);
    add#item (get_str `COPY) K._C edit_copy
      ~sensitive:(fun () -> (cur_buf ())#has_selection);
    add#item (get_str `PASTE) K._V edit_paste
      ~sensitive:(fun () -> GMain.clipboard#text <> None);
    add#item (get_str `DELETE) ~modi:[] K._Delete edit_delete
      ~sensitive:(fun () -> (cur_buf ())#has_selection) ~disabled:true;
    add#separator;
    add#item (get_str `SELECT_ALL) K._A edit_select_all;

    (* View menu *)
    let _ = menu_builder ~accel_group view_menu in

    (* Run menu *)
    let add = menu_builder ~accel_group run_menu in
    add#item (get_str `RUN) K._F5 run_compile_and_run;

    main_window#add_accel_group accel_group in
  build_menu_bar ();

  (* Toolbar *)
  let build_toolbar () =
    let add_button tooltip icon callback =
      ignore (toolbar#insert_button ~tooltip ~icon ~callback ()) in

    add_button (get_str `NEW) Icons.icon_new file_new;
    add_button (get_str `OPEN) Icons.icon_open file_open;
    add_button (get_str `SAVE) Icons.icon_save file_save;
    toolbar#insert_space ();
    add_button (get_str `RUN) Icons.icon_play run_compile_and_run in
  build_toolbar ();

  main_window#drag#dest_set ~actions:[`COPY]
    [{ Gtk. target = "text/uri-list"; flags = []; info = 0 }];
  ignore (main_window#drag#connect#data_received
    ~callback:(fun context ~x ~y data ~info ~time ->
      let filenames = File.list_from_uris data#data in
      List.iter open_file filenames;
      context#finish ~success:true ~del:false ~time));

  ignore (main_window#event#connect#delete
    ~callback:(fun _ -> file_quit (); true));
  notebook#new_document ~auto:true ();
  Array.iteri (fun i filename -> if i > 0 then open_file filename) Sys.argv;
  main_window#show ();
  GMain.Main.main ()

let _ = start ()
