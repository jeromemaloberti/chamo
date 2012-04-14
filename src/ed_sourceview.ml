(*********************************************************************************)
(*                Chamo                                                          *)
(*                                                                               *)
(*    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Source views *)

let _ = Ed_sourceview_rc.read ();;
let _ = Ed_sourceview_rc.write ();;
Gtksv_utils.set_source_style_scheme (Gtksv_utils.read_style_scheme_selection());;

let factory_name = Ed_sourceview_rc.factory_name

let get_att name l =
  try Some (List.assoc name l)
  with Not_found -> None

let get_att_f ?default f name l =
  match get_att name l with
    None -> default
  | Some s -> Some (f s)
;;

(** {2 Languages} *)

let language_manager = Gtksv_utils.source_language_manager

let lang_of_filename filename =
  try
    let (_,mime) =
      List.find
        (fun (re,_) ->
           let re = Str.regexp re in
           Str.string_match re filename 0
        )
        Ed_sourceview_rc.filename_language_patterns#get
    in
    language_manager#guess_language ~content_type: mime ()
  with
    Not_found ->
      None

(** {2 Utilities} *)

let utf8_of_filename ?(full=false) f =
  Glib.Convert.filename_to_utf8 (if full then f else Filename.basename f)

type location =
  Linechar of int * int
    | Linechars of int * (int * int)
    | Char of int
    | Chars of int * int

let location_of_string s =
  try
    let f a b c = Linechars (a, (b, c)) in
    Some (Scanf.sscanf s "%d,%d-%d" f)
  with
    _ ->
      try let f a b = Linechar(a,b) in Some (Scanf.sscanf s "%d,%d" f)
      with _ ->
          try let f a b = Chars(a,b) in Some (Scanf.sscanf s "%d-%d" f)
          with _ ->
              try Some (Char(Cam_misc.my_int_of_string s))
              with _ -> None
;;
let string_of_location = function
  Linechar (l, c) -> Printf.sprintf "%d,%d" l c
| Linechars (l, (c1,c2)) -> Printf.sprintf "%d,%d-%d" l c1 c2
| Char c -> string_of_int c
| Chars (c1,c2) -> Printf.sprintf "%d-%d" c1 c2
;;

let string_of_line_char (l,c) = Printf.sprintf "%d,%d" l c

let location_of_iter iter =
  let char = iter#offset in
  let line_start = (iter#set_line_offset 0)#offset in
  (iter#line, char - line_start)
;;
let line_char_of_location b = function
  None -> (0,0)
| Some (Linechar (l,c)) -> (l,c)
| Some (Linechars (l,(c,_))) -> (l,c)
| Some (Char c)
| Some (Chars (c,_)) ->
    let it = b#get_iter (`OFFSET c) in
    (location_of_iter it)

(** {2 Bookmarks} *)

let bookmarks = Ed_bookmarks.create_from_cf_wrappers
  ~desc: "Sourceviews bookmarks"
  (Config_file.tuple3_wrappers Config_file.string_wrappers
   Config_file.int_wrappers Config_file.int_wrappers);;
let _ = Ed_bookmarks.load bookmarks Ed_sourceview_rc.bookmarks_rc_file;;
let store_bookmarks () =
  Ed_bookmarks.store bookmarks Ed_sourceview_rc.bookmarks_rc_file;;

(** {2 Saving and loading open buffers} *)

let open_buffers_file =
  ref (Ed_config.local_dir_rc_file (factory_name^".buffers"))

let xml_of_file f =
  let atts = ("file", f#filename) :: f#attributes in
  Xml.Element ("file", atts, [])

let xml_of_file_list l =
  Xml.Element ("list", [], List.map xml_of_file l)

let file_of_xml = function
  Xml.Element ("file", atts, _) ->
    begin
      match List.partition (fun (s,_) -> s = "file") atts with
        ((_,filename) :: _), others ->
          Some (filename, others)
      | _ -> None
    end
| _ -> None

let file_list_of_xml = function
  Xml.Element ("list", _, l) ->
    List.rev
      (List.fold_left
       (fun acc xml ->
          match file_of_xml xml with
            None -> acc
          | Some f -> f :: acc
       )
         []
         l
      )
| _ -> []

let read_open_buffers_file f =
  Ed_misc.read_xml_file f file_list_of_xml

let write_open_buffers_file file buffers  =
  let xml = xml_of_file_list buffers in
  let s = Xml.to_string_fmt xml in
  Ed_misc.file_of_string ~file s
;;

(** {2 Recently used buffers} *)

let buffer_name_history = ref []
let remove_buffer_from_history name =
  buffer_name_history := List.filter ((<>) name) !buffer_name_history
let make_buffer_first_in_history name =
  remove_buffer_from_history name;
  buffer_name_history := name :: !buffer_name_history
;;
let rename_buffer_in_history oldname newname =
  buffer_name_history := List.map
    (fun s ->  if s = oldname then newname else s)
    !buffer_name_history

(** {2 The history of pastable text} *)

let pastable_history = Ed_minibuffer.history ()

  (** {2 The buffer, using GtkSourceView.source_buffer} *)

class my_buffer () =
  let buffer = GSourceView2.source_buffer () in
  object(self)
    inherit GSourceView2.source_buffer buffer#as_source_buffer

    method private filter_out_sig view_id =
      let rec iter = function
        [] -> []
      | (vid,sid) :: q ->
          if vid = view_id then
            (
             GtkSignal.disconnect buffer#as_source_buffer sid;
             q
            )
          else
            (vid,sid) :: (iter q)
      in
      iter

    val mutable modified_changed_signal_ids : (int * GtkSignal.id) list = []
    method remove_modified_changed view_id =
      modified_changed_signal_ids <- self#filter_out_sig view_id modified_changed_signal_ids

    method connect_modified_changed view_id cb =
      self#remove_modified_changed view_id;
      let sid = buffer#connect#modified_changed cb in
      modified_changed_signal_ids <- (view_id, sid) :: modified_changed_signal_ids

    val mutable cursor_moved_signal_ids : (int * GtkSignal.id) list = []
    method remove_cursor_moved view_id =
      cursor_moved_signal_ids <- self#filter_out_sig view_id cursor_moved_signal_ids

    method connect_cursor_moved view_id cb =
      self#remove_cursor_moved view_id;
      let sid = buffer#connect#mark_set
        (fun it _ -> if it#equal (buffer#get_iter `INSERT) then cb ())
      in
      cursor_moved_signal_ids <- (view_id, sid) :: cursor_moved_signal_ids

    val mutable insert_text_signal_ids : (int * GtkSignal.id) list = []
    method remove_insert_text view_id =
      insert_text_signal_ids <- self#filter_out_sig view_id insert_text_signal_ids
    method connect_insert_text view_id cb =
      self#remove_insert_text view_id;
      let sid = buffer#connect#insert_text cb in
      insert_text_signal_ids <- (view_id, sid) :: insert_text_signal_ids

    val mutable delete_range_signal_ids : (int * GtkSignal.id) list = []
    method remove_delete_range view_id =
      delete_range_signal_ids <- self#filter_out_sig view_id delete_range_signal_ids
    method connect_delete_range view_id cb =
      self#remove_delete_range view_id;
      let sid = buffer#connect#delete_range cb in
      delete_range_signal_ids <- (view_id, sid) :: delete_range_signal_ids

    method remove_view_callbacks view_id =
      self#remove_modified_changed view_id;
      self#remove_cursor_moved view_id;
      self#remove_insert_text view_id;
      self#remove_delete_range view_id

    method set_syntax_mode lang = buffer#set_language lang
    method syntax_mode = buffer#language

    method private pcre_offset_tuple_to_char_indices text (start,stop) =
      let len1 = Cam_misc.utf8_string_length (String.sub text 0 start) in
      (len1, len1 + Cam_misc.utf8_string_length (String.sub text start (stop-start)))

    method private re_search_backward re text =
      let res = Pcre.exec_all ~rex: re text in
      let len = Array.length res in
      if len > 0 then
        try Pcre.get_substring_ofs res.(len-1) 0
        with Invalid_argument _ -> raise Not_found
      else
        raise Not_found

    method private re_search_forward re text =
      let res = Pcre.exec ~rex: re text in
      try Pcre.get_substring_ofs res 0
      with Invalid_argument _ -> raise Not_found

    method re_search forward ?(start=buffer#start_iter) ?(stop=buffer#end_iter) re =
      try
        (* warning: if we search backward, we must start on charcrater back that the
           one indicated, because Str starts with the given start position, not before the
           start position; that is different from the [GSourceView2.iter_backward_search] function
           and the way gtk handles this in general, so we make this hack to act the same way.
           *)
        let (text) = buffer#get_text ~start ~stop () in
        let f = if forward then self#re_search_forward else self#re_search_backward in
        let offset = start#offset in
(*
        prerr_endline (Printf.sprintf "offset=%d" offset);
*)
        let (char_start, char_end) = self#pcre_offset_tuple_to_char_indices text (f re text) in
        let (char_start, char_end) = (char_start + offset, char_end + offset) in
        (*        prerr_endline (Printf.sprintf "found: start=%d end=%d" char_start char_end);*)
        let start = buffer#get_iter (`OFFSET char_start) in
        let stop = buffer#get_iter (`OFFSET char_end) in
        Some (start, stop)
      with
        Not_found ->
          None


    method get_iter = function
    | `LINECHAR (l,c) ->
        let line = max 0 (min (buffer#line_count - 1) l) in
        let it1 = buffer#get_iter (`LINECHAR (line,0)) in
        let chars = it1#chars_in_line -
          (if line = buffer#line_count - 1 then 0 else 1)
        in
        let c = max 0 (min c chars) in
        buffer#get_iter (`LINECHAR (line,c))
    | loc -> buffer#get_iter loc
  end

(** {2 Modes} *)

class type mode =
  object
    method name : string
    method key_bindings : (Okey.keyhit_state * string) list
    method menus : (string * GToolbox.menu_entry list) list

    method to_display : string -> string
    method from_display : string -> string
    method set_to_display : (string -> string) -> unit
    method set_from_display : (string -> string) -> unit
    method word_re : string
  end

class empty_mode : mode =
  object
    val mutable to_display = fun s -> s
    val mutable from_display = fun s -> s
    method name = "empty mode"
    method key_bindings = []
    method menus = []
    method to_display s = to_display s
    method from_display s = from_display s
    method set_to_display f = to_display <- f
    method set_from_display f = from_display <- f
    method word_re = "[a-zA-Z0-9]+"
  end

let available_modes = Hashtbl.create 37
let register_mode ?(replace=false) m =
  try
    ignore(Hashtbl.find available_modes m#name);
    if replace then
      Hashtbl.replace available_modes m#name m
    else
      failwith (Printf.sprintf "Mode %s already registered." m#name)
  with
    Not_found ->
      Hashtbl.add available_modes m#name m

let get_mode name =
  try Hashtbl.find available_modes name
  with Not_found -> failwith (Printf.sprintf "Mode %s unknown." name)

let available_mode_names () =
  Hashtbl.fold (fun name _ acc -> name :: acc) available_modes []
;;

(** {2 Associating [buffered_files] and [modes]} *)

let mode_name_of_filename filename =
  try
    let (_,mode_name) =
      List.find
        (fun (re,_) ->
           let re = Str.regexp re in
           Str.string_match re filename 0
        )
        Ed_sourceview_rc.filename_mode_patterns#get
    in
    Some mode_name
  with
    Not_found ->
      None

let mode_of_filename file =
  match mode_name_of_filename file with
    None -> None
  | Some name ->
      try Some (get_mode name)
      with Failure s ->
          Ed_misc.error_message s;
          None
;;

(** {2 A buffer associated to a file} *)

exception Newer_file_exists of string

class buffered_file ?(attributes=[]) ?loc ~name ~filename buffer =
  let loc =
    match loc with
      Some x -> x
    | None ->
        match get_att "location" attributes with
          None -> (0,0)
        | Some s -> line_char_of_location buffer (location_of_string s)
  in
  let enc =
    match get_att "encoding" attributes with
      None -> Some Ed_core_rc.encoding#get
    | Some "" -> None
    | Some s -> Some s
  in
  let mode =
    try
      match get_att "mode" attributes with
        None -> mode_of_filename filename
      | Some m -> Some (get_mode m)
    with Failure s -> Ed_misc.error_message s; None
  in
  let stxmode =
    match get_att "stxmode" attributes with
      None -> lang_of_filename filename
    | Some s -> Gtksv_utils.source_language_by_name s
  in
  object(self)
    val mutable name : string = name
    method name = name
    method set_name s = name <- s

    val mutable filename : string = filename
    method filename = filename
    method set_filename f = filename <- f

    val mutable source_marks = []
    method source_marks = source_marks
    method set_source_marks l = source_marks <- l
    method add_source_mark sm = source_marks <- sm :: source_marks

    method update_source_marks =
      let bookmarks = List.filter
        (fun (bm_name, (f,_,_)) -> Ed_misc.safe_same_files filename f)
        (Ed_bookmarks.list bookmarks)
      in
      buffer#remove_source_marks
        ~start: buffer#start_iter
        ~stop: buffer#end_iter ();
      List.iter
        (fun (bm,(_,l,c)) ->
           try
(*             prerr_endline (Printf.sprintf
              "adding bookmark %s on file %s at line %d and char %d"
                bm filename l c);
                *)
             let sm = buffer#create_source_mark ~name: bm ~category: "bookmark"
               (buffer#get_iter (`LINECHAR (l,c)))
             in
             self#add_source_mark (bm, sm)
           with
             _ -> ()
        )
        bookmarks

(*
    method private update_source_marks_in_bookmarks =
      List.iter
        (fun (name, sm) -> Ed_bookmarks.remove bookmarks name)
        self#source_marks;
      let f (name, sm) =
        let it = buffer#get_iter_at_mark sm#coerce in
        if sm#get_line > 0 then
          (
           let bm = (filename, it#line, it#line_index) in
           Ed_bookmarks.set bookmarks name bm
          )
      in
      let markers = buffer#get_markers_in_region
        ~start: buffer#start_iter ~stop: buffer#end_iter
      in
      List.iter f markers;
      store_bookmarks();
      List.iter buffer#delete_marker self#source_marks;
      self#set_source_marks [];
      self#update_source_marks
*)

    val buffer : my_buffer = buffer
    method buffer = buffer

    method attributes =
      [ "location", string_of_line_char self#location ;
        "encoding", (match self#encoding with None -> "" | Some s -> s) ;
        "mode", (match self#mode with None -> "" | Some m -> m#name) ;
        "stxmode", (match self#syntax_mode with None -> "" | Some s -> s#name) ;
      ]

    val mutable date = None
    method date = date
    method set_date d = date <- d

    val mutable location = loc
    method location = location
    method set_location (l,c) = location <- (l,c)

    val mutable encoding : string option = enc
    method encoding = encoding
    method set_encoding e = encoding <- e

    method of_utf8 s =
      match encoding with
        None -> Ed_misc.of_utf8 s
      | Some coding -> Ed_misc.of_utf8 ~coding s
    method to_utf8 s =
      match encoding with
        None -> Ed_misc.to_utf8 s
      | Some coding -> Ed_misc.to_utf8 ~coding s

    val mutable mode = (mode : mode option)
    method mode = mode
    method set_mode m =
      match mode with
        None -> mode <- m
      | Some m2 ->
          let s = m2#from_display (self#buffer#get_text ()) in
          mode <- m;
          self#buffer#set_text (self#mode_to_display s);
          self#buffer#set_modified false

    method mode_key_bindings =
      match mode with
        None -> []
      | Some m -> m#key_bindings
    method mode_menus =
      match mode with
        None -> []
      | Some m -> m#menus
    method mode_name =
      match mode with
        None -> None
      | Some m -> Some m#name

    method set_syntax_mode lang = buffer#set_syntax_mode lang
    method syntax_mode = buffer#syntax_mode

    method mode_from_display s =
      match mode with
        None -> s
      | Some m -> m#from_display s

    method mode_to_display s =
      match mode with
        None -> s
      | Some m -> m#to_display s

    method load_file filename =
      if Sys.file_exists filename then
        begin
          (* FIXME: handle errors occuring while opening file *)
          let text =
            try self#mode_to_display
              (self#to_utf8 (Ed_misc.string_of_file filename))
            with _ -> ""
          in
          self#buffer#begin_not_undoable_action ();
          self#buffer#set_text text;
          self#buffer#end_not_undoable_action ();
          self#buffer#set_modified false;
        end;
      self#update_date;
      self#update_source_marks

    method newer_file_exists =
      let d = Ed_misc.mod_date_of_file filename in
      match date with
        None -> true
      | Some d2 -> d2 < d

    method write_file ?(fail_if_newer=false) () =
      if self#newer_file_exists && fail_if_newer then
        raise (Newer_file_exists filename);
      let utf8 = buffer#get_text () in
      let s = self#of_utf8 (self#mode_from_display utf8) in
      Ed_misc.file_of_string ~file: filename s;
      buffer#set_modified false;
      self#update_date
(*      self#update_source_marks_in_bookmarks*)

    method update_date =
      date <- Some (Ed_misc.mod_date_of_file filename)

    method select_location loc =
      let (l,c) = line_char_of_location buffer (Some loc) in
      self#set_location (l,c);
      buffer#place_cursor ~where: (buffer#get_iter (`LINECHAR (l,c)));
      let (start,stop) =
        match loc with
        | Linechar (l,c) ->
            let n = (buffer#get_iter (`LINECHAR (l,c)))#offset in
            (n, n)
        | Linechars (l,(start,stop)) ->
            let it = buffer#get_iter (`LINECHAR (l,c)) in
            let start2 = it#offset in
            let stop = (it#forward_cursor_positions (stop-start))#offset in
            (start2, stop)
        | Chars (start,stop) -> (start,stop)
        | Char start -> (start,start)
      in
      buffer#select_range
        (buffer#get_iter (`OFFSET start))
        (buffer#get_iter (`OFFSET stop))

    method range_from_range_in_file ~left ~right =
      let from_display = self#mode_from_display (buffer#get_text ()) in
      let (left, right) =
        let left =
          Cam_misc.utf8_string_length
            (self#mode_to_display
             (String.sub from_display 0 left))
        in
        let right =
          Cam_misc.utf8_string_length
            (self#mode_to_display
             (String.sub from_display 0 right))
        in
        (left, right)
      in
      (left, right)

    method line_offset_from_line_in_file line =
      let char_offset = Cam_misc.char_of_line filename line in
      let from_display = self#mode_from_display (buffer#get_text ()) in
      Cam_misc.utf8_string_length
        (self#mode_to_display (String.sub from_display 0 char_offset))

    method select_range_in_file ~left ~right =
      let (left, right) = self#range_from_range_in_file ~left ~right in
      let start = buffer#get_iter (`OFFSET left) in
      let stop = buffer#get_iter (`OFFSET right) in
      buffer#select_range start stop

    initializer
      self#set_syntax_mode stxmode;
      self#load_file filename;

      let on_marker_update iter =
        prerr_endline "marker_update";
        List.iter
          (fun (bm, m) ->
             let it = buffer#get_iter_at_mark m#coerce in
             prerr_endline
               (Printf.sprintf "marker %s on line %d"
                bm it#line
               )
          )
          self#source_marks
      in
      ignore(buffer#connect#source_mark_updated on_marker_update);


  end

(** {2 The views} *)

class sourceview ?(attributes=[]) (topwin : Ed_view.topwin)
  f_on_destroy f_set_active f_dup
  (f_file_rename : string -> string -> unit) (file : buffered_file) =
  let vbox = GPack.vbox () in
  let wscroll = GBin.scrolled_window
    ~packing: (vbox#pack ~expand: true ~fill: true ~padding: 0)
      ~border_width: 0
      ~vpolicy: `AUTOMATIC ~hpolicy: `AUTOMATIC () in
  let show_line_numbers =
    get_att_f Ed_misc.bool_of_string "line_numbers" attributes = Some true
  in
  let show_line_marks =
    get_att_f Ed_misc.bool_of_string "line_markers" attributes = Some true
  in
  let wrap_mode =
    get_att_f ~default: Ed_sourceview_rc.default_wrap_mode#get
      Ed_sourceview_rc.wrap_mode_of_string "wrap_mode" attributes
  in
  let source_view =
    GSourceView2.source_view
      ~source_buffer: (file#buffer :> GSourceView2.source_buffer)
      ~editable: true
      ~auto_indent:true
      ~insert_spaces_instead_of_tabs:true ~tab_width:2
      ~show_line_numbers
      ~show_line_marks
      ?wrap_mode
      ~smart_home_end: `ALWAYS
      ~packing: wscroll#add
      ()
  in
  let hbox_state = GPack.hbox ~packing: vbox#pack () in
  let add_state fopt =
    let evbox = GBin.event_box ~packing: hbox_state#pack () in
    let wl = GMisc.label ~packing: evbox#add ~xpad: 5 () in
    begin
      match fopt with
        None -> ()
      | Some f ->
          ignore
            (evbox#event#connect#button_press
             (fun ev ->
                match GdkEvent.get_type ev with
                  `BUTTON_PRESS when GdkEvent.Button.button ev = 1 ->
                    f (); true
                | _ -> false
             )
            )
    end;
    wl
  in
  let on_stx_click () =
    Ed_commands.eval_command (factory_name^"_popup_syntax_mode_choice")
  in
  let on_mode_click () =
    Ed_commands.eval_command (factory_name^"_popup_mode_choice")
  in
  let wl_modified = add_state None in
  let wl_file = add_state None in
  let wl_loc = add_state None in
  let wl_stx_mode = add_state (Some on_stx_click) in
  let wl_mode = add_state (Some on_mode_click) in
  let wl_encoding = add_state None in
  let ref_on_destroy = ref (fun () -> ()) in
  object(self)
    inherit Ed_view.dyn_label
    inherit Ed_view.dyn_destroyable
      (fun () -> !ref_on_destroy () ; source_view#destroy ();vbox#destroy();)

    method minibuffer = topwin#minibuffer

    val mutable file = file

    method source_view = source_view
    method source_buffer = file#buffer

    method box = vbox#coerce

    method private write_file =
      let rec do_write ~fail_if_newer =
        try
          file#write_file ~fail_if_newer ();
          let msg = Printf.sprintf "Wrote %s"
            (utf8_of_filename ~full: true file#filename)
          in
          Ed_misc.display_message msg
        with
          Newer_file_exists _ ->
            let do_it () = do_write ~fail_if_newer: false in
            Ed_misc.confirm self#minibuffer
              (Printf.sprintf "%s was edited since last visited; write anyway ?"
               (utf8_of_filename ~full: true file#filename))
              do_it
        | Failure s
        | Sys_error s
        | Glib.Convert.Error (_,s) ->
            Ed_misc.error_message (Ed_misc.to_utf8 s)
      in
      do_write ~fail_if_newer: true

    method do_save =
      self#write_file

    method save =
      let f () =
        if self#buffer_modified then
          self#do_save
        else
          Ed_misc.set_active_action_message "(No changes need to be saved)"
      in
      Some f
    method save_as =
      let f () =
        let save newname =
          let do_it () =
            try
              f_file_rename file#filename newname;
              self#write_file ;
            with
              Failure s -> Ed_misc.error_message (Ed_misc.to_utf8 s)
          in
          if Sys.file_exists newname then
            Ed_misc.confirm self#minibuffer
              (Printf.sprintf "Overwrite %s ?" (utf8_of_filename ~full: true newname))
              do_it
          else
            do_it ()
        in
        Ed_misc.select_file
          self#minibuffer
          ~title: (Printf.sprintf "Save %s as ..." (utf8_of_filename file#filename))
          ((Filename.dirname file#filename)^"/")
          save
      in
      Some f

    method paste = Some (fun () -> Ed_commands.eval_command (factory_name^"_paste"))
    method copy = Some (fun () -> Ed_commands.eval_command (factory_name^"_copy"))
    method cut = Some (fun () -> Ed_commands.eval_command (factory_name^"_cut"))

    method close = vbox#destroy ()

    method kind = factory_name

    val mutable my_location = (0,0)
    method set_my_location (l,c) =
      (*      prerr_endline (Printf.sprintf "set_my_location(%d,%d)" l c);*)
      my_location <- (l,c);
      file#set_location (l,c);
      self#display_location

    method attributes =
      [ "location", string_of_line_char my_location ;
        "line_numbers", (Ed_misc.string_of_bool source_view#show_line_numbers) ;
        "line_markers", (Ed_misc.string_of_bool source_view#show_line_marks) ;
        "wrap_mode", (Ed_sourceview_rc.string_of_wrap_mode source_view#wrap_mode) ;
      ]

    method file = file
    method filename = file#filename
    method buffer_name = file#name
    method buffer_modified = file#buffer#modified

    method set_location (l,c) =
      (*      prerr_endline (Printf.sprintf "set_location(%d,%d)" l c);*)
      (* to avoid a fatal gtk error if the offset on line is bigger than
         the maximum offset of the line, we minimize the column by
         (the number of chars on the line) - 1. *)
      let b = file#buffer in
      let current_loc = self#location_in_buffer in
      if current_loc = (l,c) then
        begin
          (*          prerr_endline (Printf.sprintf "current_loc = (l,c)");*)
          self#update_my_location
        end
      else
        begin
          let it = b#get_iter (`LINECHAR (l,c)) in
          self#place_cursor it
        end;
      source_view#scroll_to_mark `INSERT

    method select_location loc =
      self#set_location (line_char_of_location file#buffer (Some loc));
      file#select_location loc
    method select_location_opt = function
      None -> ()
    | Some loc -> self#select_location loc

    method has_focus =
      let b = source_view#misc#get_flag `HAS_FOCUS in
      (*
         if b then
         prerr_endline (Printf.sprintf "view %d: I have the focus!" (Oo.id self));
      *)
      b

    method location_in_buffer =
      let b = file#buffer in
      let iter = b#get_iter `INSERT in
      location_of_iter iter

    method current_line =
      fst self#location_in_buffer

    method update_my_location =
      self#set_my_location self#location_in_buffer

    method select_range_in_file ?(jump: [`Left|`Right]option) ~left ~right () =
      begin
        match jump with
          None -> ()
        | Some `Left ->
            self#set_location
              (location_of_iter (file#buffer#get_iter (`OFFSET left)))
        | Some `Right ->
            self#set_location
              (location_of_iter (file#buffer#get_iter (`OFFSET right)))
      end;
      file#select_range_in_file ~left ~right


    method on_cursor_moved =
      if self#has_focus then
        self#update_my_location
      else
        ()

    val mutable on_focus_in = fun () -> ()
    method set_on_focus_in (f: unit -> unit) =
      on_focus_in <-
        (fun _ ->
           f_set_active self;
           self#set_location my_location; f ();
        )

    method grab_focus =
      source_view#misc#grab_focus ();
      source_view#scroll_to_mark `INSERT;
      f_set_active self;
      ()

    method my_set_label =
      self#set_label (Printf.sprintf "%s%s" (utf8_of_filename file#name)
       (if source_view#buffer#modified then " *" else ""))

    method set_file ?(focus_in=false) (f : buffered_file) =
      file#buffer#remove_view_callbacks (Oo.id self);
      file <- f;
      source_view#set_buffer (f#buffer :> GText.buffer);
      self#set_location file#location;
      source_view#scroll_to_mark `INSERT;
      self#connect_buffer_events;
      self#my_set_label;
      self#display_state;
      if focus_in then on_focus_in ()

    method dup : Ed_view.topwin -> Ed_view.gui_view option = fun topwin ->
      Some (f_dup file topwin)

    method display_state =
      self#display_modified;
      self#display_buffer_name ;
      self#display_encoding ;
      self#display_location ;
      self#display_stx_mode ;
      self#display_mode

    method display_buffer_name =
      wl_file#set_text (utf8_of_filename file#name)

    method display_modified =
      wl_modified#set_text (if source_view#buffer#modified then "*" else "")

    method display_encoding =
      let enc =
        match file#encoding with
          None -> "default encoding"
        | Some s -> s
      in
      wl_encoding#set_text (Printf.sprintf " %s " (Ed_misc.to_utf8 enc))

    method display_location =
      let (line,char) = my_location in
      wl_loc#set_text (Printf.sprintf "L%d-C%d" (line+1) (char+1))

    method display_stx_mode =
      let lang =
        match file#buffer#language with
          None -> "[no highlight]"
        | Some lang -> Printf.sprintf "[%s]" lang#name
      in
      wl_stx_mode#set_text (Ed_misc.to_utf8 lang)

    method display_mode =
      let mode =
        match file#mode_name with
          None -> "(no mode)"
        | Some name -> Printf.sprintf "(%s)" name
      in
      wl_mode#set_text (Ed_misc.to_utf8 mode)

    method connect_buffer_events =
      ignore(file#buffer#connect_modified_changed
       (Oo.id self)
         (fun () -> self#display_modified; self#my_set_label));
      ignore(file#buffer#connect_cursor_moved
       (Oo.id self)
         (fun () -> self#on_cursor_moved));

    method key_bindings =
      file#mode_key_bindings @
        Ed_sourceview_rc.key_bindings#get

    method bookmarks_menus =
      let l = Ed_bookmarks.list bookmarks in
      let l = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) l in
      let entries =
        List.map
          (fun (name, (file,line,col)) ->
            let com () =
              Ed_commands.eval_command (Printf.sprintf "open_file %s" (Filename.quote file));
              if self#filename = file then
                self#set_location (line,col)
            in
            let label =
               Printf.sprintf "%s (%s, %d, %d)" name (Filename.basename file) line col
             in
            `I (Cam_misc.escape_menu_label (Ed_misc.to_utf8 label), com)
          )
          l
      in
      [ "Bookmarks", entries]

    method menus : (string * GToolbox.menu_entry list) list =
      let com com () = Ed_commands.eval_command (Printf.sprintf "%s_%s" factory_name com) in
      (* FIXME: do some kind of merging between the sourceview menus
         and the mode menus *)
      [
        "Search",
        [ `I ("Search forward", com "search") ;
          `I ("Search backward", com "search_backward") ;
          `S ;
          `I ("Search regexp forward", com "search_re") ;
          `I ("Search regexp backward", com "search_re_backward") ;
          `S ;
          `I ("Query/replace", com "query_replace") ;
        ]
      ] @
        self#bookmarks_menus @
        file#mode_menus

    method update_menus = topwin#update_menus

    method beginning_of_line =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      let (l,_) = location_of_iter it in
      self#set_location (l,0)
    method end_of_line =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      let (l,_) = location_of_iter it in
      self#set_location (l,max_int)

    method set_scroll_on_change =
      file#buffer#connect_delete_range (Oo.id self)
        (fun ~start ~stop -> self#place_cursor start);
      file#buffer#connect_insert_text (Oo.id self)
        (fun it _ -> self#place_cursor it)

    method unset_scroll_on_change =
      file#buffer#remove_delete_range (Oo.id self);
      file#buffer#remove_insert_text (Oo.id self)

    method undo =
      let b = file#buffer in
      if b#can_undo then
        begin
          self#set_scroll_on_change;
          b#undo ();
          self#unset_scroll_on_change;
        end
    method redo =
      let b = file#buffer in
      if b#can_redo then
        begin
          self#set_scroll_on_change;
          b#redo ();
          self#unset_scroll_on_change;
        end

    method place_cursor ?(scroll=true) where =
      file#buffer#place_cursor ~where;
      if scroll then ignore(source_view#scroll_to_iter where);
      self#update_my_location
    method forward_word =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      self#place_cursor it#forward_word_end
    method backward_word =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      self#place_cursor it#backward_word_start
    method forward_line =
      let (l,c) = my_location in
      self#set_location (l+1,c)
    method backward_line =
      let (l,c) = my_location in
      self#set_location (l-1,c)
    method forward_char =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      self#place_cursor it#forward_cursor_position
    method backward_char =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      self#place_cursor it#backward_cursor_position

    method cut_to_selection ?(concat : [`APPEND | `PREPEND] option) ~start ~stop () =
      let b = file#buffer in
      let text = b#get_text ~start ~stop () in
      b#begin_user_action ();
      begin
        match concat with
          None ->
            pastable_history#add text;
            GMain.selection#set_text text;
        | Some p ->
            let sel =
              match GMain.selection#text with
                None -> ""
              | Some s -> s
            in
            let text =
              match p with
                `PREPEND -> text^sel
              | `APPEND -> sel^text
            in
            pastable_history#add text;
            GMain.selection#set_text text;
      end;
      b#delete ~start ~stop;
      self#update_my_location;
      b#end_user_action ();

    method kill_line ~append =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      let eol =
        if it#ends_line then
          it#forward_line
        else
          it#forward_to_line_end
      in
      let concat = if append then Some `APPEND else None in
      self#cut_to_selection ?concat ~start: it ~stop: eol ()

    method kill_word ?concat forward =
      let b = file#buffer in
      let it = b#get_iter `INSERT in
      let (start,stop) =
        if forward then
          (it, it#forward_word_end)
        else
          (it#backward_word_start, it)
      in
      self#cut_to_selection ?concat ~start ~stop ()

    method insert text =
      file#buffer#insert text;
      self#update_my_location

    method delete_char forward =
      let b = file#buffer in
      let start = b#get_iter `INSERT in
      let stop =
        if forward then start#forward_char else start#backward_char
      in
      if start#equal stop then
        ()
      else
        (
         b#begin_user_action ();
         b#delete ~start ~stop;
         b#end_user_action ()
        )

    method transpose_chars =
      let b = file#buffer in
      let insert = b#get_iter `INSERT in
      if insert#is_end or insert#is_start then
        ()
      else
        let stop = insert#backward_char in
        let c = b#get_text ~start: insert ~stop () in
        b#begin_user_action ();
      b#delete ~start: insert ~stop;
      let iter = insert#forward_char in
      b#insert ~iter c;
      self#place_cursor iter;
      b#end_user_action ()

    method transpose_lines =
      let b = file#buffer in
      let insert = b#get_iter `INSERT in
      let line = insert#line in
      if line = 0 then
        ()
      else
        (
         let (line1_start, line1_stop) =
           ((if insert#starts_line then insert else insert#backward_line#forward_line),
            (if insert#is_end then insert else insert#forward_line))
         in
         let prevline_start = line1_start#backward_line in
         (*
           prerr_endline (Printf.sprintf "prevline_start:%s" (string_of_location (location_of_iter prevline_start)));
            *)
         let prevline_stop =
           (* do not cut the line with the ending '\n' if we are at the end of the buffer,
              to prevent adding a new blank line *)
           if line1_stop#is_end or line1_start#equal line1_stop then
             prevline_start#forward_to_line_end
           else
             line1_start
         in
         (*
            prerr_endline (Printf.sprintf "prevline_stop:%s" (string_of_location (location_of_iter prevline_stop)));
            *)
         let prev_line = b#get_text ~start: prevline_start ~stop: prevline_stop () in
         let prev_line =
           if line1_stop#is_end or line1_start#equal line1_stop then
             "\n"^prev_line
           else
             prev_line
         in
         (*
            let sline = b#get_text ~start: line1_start ~stop: line1_stop () in
            prerr_endline (Printf.sprintf "prev_line=\"%s\"" (self#of_utf8 prev_line));
            prerr_endline (Printf.sprintf "line=\"%s\"" (self#of_utf8 sline));
            *)
         b#begin_user_action ();
         self#place_cursor line1_stop;
         b#delete ~start: prevline_start ~stop: line1_start;
         b#insert prev_line;
         self#goto_line (line + 1);
         b#end_user_action ()
        )

    method transpose_words =
      let b = file#buffer in
      let insert = b#get_iter `INSERT in
      (*
         prerr_endline (Printf.sprintf "insert iter starts word: %b" insert#starts_word);
         prerr_endline (Printf.sprintf "insert iter inside word: %b" insert#inside_word);
         prerr_endline (Printf.sprintf "insert#forward_word_end is_end: %b" insert#forward_word_end#is_end);
         prerr_endline (Printf.sprintf "insert#backward_word_start is_start: %b" insert#backward_word_start#is_start);
      *)
      (* let's say we swap a right and left words *)
      try
        let right_word_start =
          if insert#starts_word then
            insert
          else
            (* a forward_word_start function would have been great, thanks Gtk! *)
            let it =
              let itend =
                if insert#inside_word then insert#forward_word_end else insert
              in
              let itend2 = itend#forward_word_end in
              if itend2#equal itend or not itend2#ends_word then
                raise Not_found
              else
                itend2#backward_word_start
            in
            if it#is_end or not it#starts_word then
              raise Not_found
            else
              it
        in
        let right_word_end = right_word_start#forward_word_end in
        let left_word_start =
          let it = right_word_start#backward_word_start in
          if it#equal right_word_start or not it#starts_word then
            raise Not_found
          else
            it
        in
        let left_word_end = left_word_start#forward_word_end in

        let rw_start_offset = right_word_start#offset in
        let rw_end_offset = right_word_end#offset in
        let rw_size = rw_end_offset - rw_start_offset in
        let lw_start_offset = left_word_start#offset in
        let lw_end_offset = left_word_end#offset in
        let lw_size = lw_end_offset - lw_start_offset in

        let rw = b#get_text ~start: right_word_start ~stop: right_word_end () in
        let lw = b#get_text ~start: left_word_start ~stop: left_word_end () in

        b#begin_user_action ();

        b#delete ~start: right_word_start ~stop: right_word_end;

        let left_word_start = b#get_iter (`OFFSET lw_start_offset) in
        let left_word_end = b#get_iter (`OFFSET lw_end_offset) in
        b#delete ~start: left_word_start ~stop: left_word_end;
        let iter = b#get_iter (`OFFSET lw_start_offset) in
        b#insert ~iter rw;
        let iter = b#get_iter (`OFFSET (rw_start_offset - lw_size + rw_size)) in
        let ins_offset = iter#offset in
        b#insert ~iter lw;
        self#place_cursor (b#get_iter (`OFFSET (ins_offset + lw_size)));
        b#end_user_action ()
      with
        Not_found ->
          (* we don't have two words to transpose *)
          ()

    method goto_line n =
      let m = max 0 (min n (file#buffer#line_count - 1)) in
      let where = file#buffer#get_iter (`LINE m) in
      self#place_cursor where

    method goto_char n =
      let m = max 0 (min n (file#buffer#char_count -1)) in
      let where = file#buffer#get_iter (`OFFSET m) in
      self#place_cursor where

    method reload =
      let g () =
        let f () = file#load_file file#filename in
        if file#buffer#modified then
          Ed_misc.confirm self#minibuffer
            "Buffer was modified; revert anyway ?" f
        else
          f ()
      in
      Some g

    method set_syntax_mode lang =
      file#set_syntax_mode lang;
      self#display_stx_mode

    method set_mode mode =
      file#set_mode mode;
      self#display_mode

    method set_encoding e =
      file#set_encoding e;
      self#display_encoding

    method switch_line_numbers ?v () =
      let v = match v with
          None -> not source_view#show_line_numbers
        | Some v -> v
      in
      source_view#set_show_line_numbers v

    method switch_line_markers ?v () =
      let v = match v with
          None -> not source_view#show_line_marks
        | Some v -> v
      in
      source_view#set_show_line_marks v

    method set_wrap_mode m =
      source_view#set_wrap_mode m

    initializer
      self#set_location file#location;
      self#set_my_location file#location;
      self#my_set_label;
      self#display_state;
      source_view#scroll_to_mark `INSERT;
      Gtksv_utils.register_source_view source_view;
      Gtksv_utils.apply_sourceview_props source_view (Gtksv_utils.read_sourceview_props ()) ;
      self#connect_buffer_events;
      let add_clipboard_to_pastable_history () =
        match GMain.clipboard#text with
          None | Some "" -> ()
        | Some s -> pastable_history#add s
      in
      ignore(source_view#connect#after#copy_clipboard
       add_clipboard_to_pastable_history);
      ignore(source_view#connect#after#cut_clipboard
       add_clipboard_to_pastable_history);
      ignore(source_view#connect#after#paste_clipboard
       add_clipboard_to_pastable_history);
      ref_on_destroy := (fun () -> f_on_destroy self);
      ignore(source_view#event#connect#focus_in (fun _ -> on_focus_in (); false));

      try
        let pix_bookmark = GdkPixbuf.from_file
          (Filename.concat Cam_installation.pixmaps_dir "bookmark.png")
        in
        source_view#set_mark_category_pixbuf "bookmark" (Some pix_bookmark)
      with
        e -> prerr_endline (Printexc.to_string e)

  end


(** {2 Handling open views and buffers} *)

let views = ref ([] : sourceview list)
let buffers = ref ([] : buffered_file list)
let active_sourceview = ref (None : sourceview option)

let set_active_sourceview o =
  if List.exists (fun v -> Oo.id v = Oo.id o) !views then
    active_sourceview := Some o;
  make_buffer_first_in_history o#buffer_name

let get_fresh_buffer_name name =
  let name_of_n n =
    if n <= 1
    then name
    else Printf.sprintf "%s<%d>" name n
  in
  let rec iter n =
    let name = name_of_n n in
    if List.exists (fun b -> b#name = name) !buffers then
      iter (n+1)
    else
      name
  in
  iter 1

let create_buffer ?(attributes=[]) filename =
  let mes = Printf.sprintf "creating buffer for %s" filename in
  Ed_misc.display_message mes;

  let b = new my_buffer () in
  b#set_max_undo_levels Ed_sourceview_rc.max_undo_levels#get;
  b#place_cursor b#start_iter;
  b#set_highlight_syntax true;
  b#set_highlight_matching_brackets true;
  Gtksv_utils.register_source_buffer (b :> GSourceView2.source_buffer);
  let name = get_fresh_buffer_name (Filename.basename filename) in
  let file = new buffered_file ~attributes ~name ~filename b in
  buffers := file :: !buffers;
  make_buffer_first_in_history file#name;
  file

let get_buffer ?(attributes=[]) filename =
  try
(*    if not (Sys.file_exists filename) then raise Not_found;*)
    let b = List.find
      (fun f -> Ed_misc.safe_same_files f#filename filename)
        !buffers
    in
    let loc =
      match get_att "location" attributes with
        None -> None
      | Some s -> location_of_string s
    in
    (
     match loc with
     | None -> ()
     | Some loc -> b#select_location loc
    );
    b
  with Not_found -> create_buffer ~attributes filename

let get_buffer_by_name name =
  List.find (fun b -> b#name = name) !buffers

let remove_buffer (b : buffered_file) =
  buffers := List.filter (fun b2 -> b#filename <> b2#filename) !buffers;
  remove_buffer_from_history b#name;
  Gtksv_utils.unregister_source_buffer (b#buffer :> GSourceView2.source_buffer)
;;

let on_view_destroy v =
  views := List.filter (fun v2 -> Oo.id v <> Oo.id v2) !views;
  match !active_sourceview with
    Some v2 when Oo.id v = Oo.id v2 ->
      active_sourceview := None
  | Some _
  | None -> ()

let rec create_view ?(attributes=[]) topwin file =
  let v = new sourceview ~attributes topwin on_view_destroy
    set_active_sourceview dup file_rename file
  in
  ignore(v#source_view#connect#destroy (fun () -> on_view_destroy v));
  views := v :: !views;
  v

and dup file topwin =
  (create_view topwin file :> Ed_view.gui_view)

and file_rename oldname newname =
  try
    ignore(List.find (fun b -> Ed_misc.safe_same_files b#filename newname) !buffers);
    let mes = Printf.sprintf "%s is already open. Close it before." newname in
    failwith mes
  with Not_found ->
      (* search on names here, not dev/inode *)
      let views = List.filter (fun v -> v#filename = oldname) !views in
      let b = get_buffer oldname in
      let old_buffer_name = b#name in
      b#set_filename newname;
      b#set_name (get_fresh_buffer_name (Filename.basename newname));
      rename_buffer_in_history old_buffer_name b#name;
      List.iter (fun v -> v#my_set_label; v#display_state) views

let open_file topwin active_view ?(attributes=[]) filename =
  let file = get_buffer ~attributes filename in
  match !active_sourceview with
    None -> `New_view (create_view ~attributes topwin file :> Ed_view.gui_view)
  | Some v ->
      if topwin#contains_view (v :> Ed_view.gui_view) then
        begin
          if v#file#name = file#name then
            match get_att "location" attributes with
              None -> ()
            | Some s -> v#select_location_opt (location_of_string s)
          else
            v#set_file ~focus_in: true file;
          `Use_view (v :> Ed_view.gui_view)
        end
      else
        `New_view (create_view ~attributes topwin file :> Ed_view.gui_view)
;;

(** {2 Factory} *)

class factory : Ed_view.view_factory =
  object
    method name = factory_name
    method open_file = open_file
    method open_hidden =
      Some (fun ?attributes filename -> ignore (get_buffer ?attributes filename))
    method on_start =
      let f () =
        let buffers = read_open_buffers_file !open_buffers_file in
        List.iter
          (fun (f, attributes) ->
            if Sys.file_exists f then
              ignore(get_buffer ~attributes f)
          )
          buffers
      in
      Ed_misc.catch_print_exceptions f ()
    method on_exit =
      Ed_misc.catch_print_exceptions
        (write_open_buffers_file !open_buffers_file) !buffers;
      Ed_misc.catch_print_exceptions
        (Ed_bookmarks.store bookmarks) Ed_sourceview_rc.bookmarks_rc_file
  end


let _ = Ed_view.register_view_factory factory_name (new factory)
;;

(** {2 "Forward-stack" of locations.} *)

let location_stack = Ed_fstack.create ();;


(** {2 Commands} *)

let keep_key_bindings_from_view v l =
  let rec iter acc = function
    [] -> acc
  | (k,com) :: q ->
      if List.mem com l then
        iter ((k, fun () -> Ed_commands.eval_command com) :: acc) q
      else
        iter acc q
  in
  iter [] v#key_bindings

let register_com ~prefix name args ?more f =
  let name = Printf.sprintf "%s_%s" prefix name in
  let f args =
    match !active_sourceview with
      None -> ()
    | Some v -> f v args
  in
  let c = { Ed_commands.com_name = name ;
      com_args = args ;
      com_more_args = more ;
      com_f = f ;
    }
  in
  Ed_commands.register c

let switch_to_buffer (v : sourceview) name =
  try
    let b = get_buffer_by_name name in
    v#set_file ~focus_in: true b
  with Not_found ->
      Ed_misc.error_message
        (Printf.sprintf "No %s buffer %s"
         factory_name (utf8_of_filename name))

let candidate_buffers () =
  let displayed_buffers = List.map (fun o -> o#buffer_name) !views in
  let (last,first) = List.partition
    (fun name -> List.mem name displayed_buffers)
      !buffer_name_history
  in
  first @ last

let switch_buffer_history = Ed_minibuffer.history ()
let switch_buffer v args =
  if Array.length args > 0 then
    let name = args.(0) in
    switch_to_buffer v name
  else
    (* propose list of available buffers *)
    let candidate_buffers = candidate_buffers () in
    let com = Printf.sprintf "%s_switch_buffer" factory_name in
    let f = function
      "" ->
        (
         match candidate_buffers with
           [] -> ()
         | s :: _ ->
             Ed_commands.launch_command
               ~history:false com [| s |]
        )
    | s ->
        Ed_commands.launch_command
          ~history:false com [| s |]
    in
    let title =
      Printf.sprintf "Switch to %s"
        (match candidate_buffers with
           [] -> "" | s :: _ -> "["^(Glib.Convert.filename_to_utf8 s)^"]")
    in
    Ed_misc.select_string
      ~history: switch_buffer_history
      v#minibuffer
      ~title
      ~choices: (List.map Glib.Convert.filename_to_utf8 candidate_buffers)
      ""
      f

let destroy_buffer (v : sourceview) args =
  let f () =
    let bname = v#buffer_name in
    remove_buffer v#file;
    match List.filter (fun name -> name <> bname) (candidate_buffers()) with
      [] ->
        (* no more buffer to replace the destroyed one, destroy all views *)
        List.iter (fun v -> v#destroy) !views
    | first :: _ ->
        let buf =
          try get_buffer_by_name first
          with Not_found -> failwith "Internal error; Please restart to be safe."
        in
        List.iter
          (fun (v:sourceview) ->
             if v#buffer_name = bname then v#set_file ~focus_in: true buf
          )
          !views
  in
  if not v#buffer_modified then
    f ()
  else
    Ed_misc.confirm v#minibuffer
      (Printf.sprintf "Buffer %s modified; destroy anyway ?"
       (utf8_of_filename v#buffer_name))
      f


type search_buffer_function =
  ?wrapped:bool ->
    bool ->
      my_buffer ->
        ?start:GText.iter -> string -> bool * (GText.iter * GText.iter) option

let prev_search = ref None
let rec search_buffer ?(wrapped=false) forward (buffer : my_buffer)
  ?(start=buffer#get_iter `INSERT) s_utf8 =
  let gsearch =
    if forward then
      GSourceView2.iter_forward_search
    else
      GSourceView2.iter_backward_search
  in
  let stop = buffer#end_iter in
  match gsearch start [] ~start ~stop s_utf8 with
    None ->
      if wrapped then
        (wrapped, None)
      else
        let start = if forward then buffer#start_iter else buffer#end_iter in
        search_buffer ~wrapped: true forward buffer ~start s_utf8
  | Some (start,stop) ->
      (wrapped, Some (start, stop))

let rec search =
  let forward = ref true in
  fun (fsearch_buffer : search_buffer_function)
    mes ?(changed=false) _forward (v: sourceview) args ->
      forward := _forward;
    let fixed wrapped = Printf.sprintf "%s%s%s: "
      (if wrapped then "[wrapped] " else "")
        mes
        (if !forward then "" else " backward")
    in
    let mb = v#minibuffer in
    if mb#active then
      (
       let s_utf8 = mb#get_user_text in
       match s_utf8 with
         "" ->
           begin
             match !prev_search with
               None -> ()
             | Some s -> mb#set_user_text s
           end
       | _ ->
           (*           prerr_endline ("search "^(Ed_misc.of_utf8 s_utf8));*)
           let start =
             if changed then
               let (start, stop) = v#file#buffer#selection_bounds in
               Some (if !forward then start else stop)
             else
               None
           in
           match fsearch_buffer !forward v#file#buffer ?start s_utf8 with
             (wrapped, None) ->
               mb#set_text ~fixed: (fixed wrapped) s_utf8
           | (wrapped, Some (start, stop)) ->
               (*
                  prerr_endline (Printf.sprintf "found start=%d,%d" start#line start#line_offset);
                  prerr_endline (Printf.sprintf "and stop=%d,%d" stop#line stop#line_offset);
               *)
               let loc =
                 let it = if !forward then stop else start in
                 location_of_iter it
               in
               v#set_location loc;
               if !forward then
                 v#file#buffer#select_range stop start
               else
                 v#file#buffer#select_range start stop;
               ignore(v#source_view#scroll_to_iter start);
               ignore(v#source_view#scroll_to_iter stop);
               mb#set_text ~fixed: (fixed wrapped) s_utf8
      )
    else
      (
       let on_changed () =
         (* do not do anything if there is nothing to search
            or the searched string has not changed, to prevent
            from moving to next search when changing the fixed text
            of the minibuffer *)
         match mb#get_user_text with
           "" -> ()
         | s ->
             if !prev_search = Some s then
               ()
             else
               (
                prev_search := Some s;
                search fsearch_buffer mes ~changed: true !forward  v args
               )
       in
       mb#clear;
       mb#set_text ~fixed: (fixed false) "";
       mb#set_on_text_changed on_changed;

       mb#set_more_key_bindings
         (keep_key_bindings_from_view v
          [ factory_name^"_search" ;
            factory_name^"_search_backward" ;
            factory_name^"_search_re" ;
            factory_name^"_search_re_backward" ;
          ]
         );
       mb#set_active true
      )

let rec re_search_buffer ?(wrapped=false) forward (buffer: my_buffer) ?start s_utf8 =
  let (start, stop) =
    if forward then
      match start with
        None -> (buffer#get_iter `INSERT, buffer#end_iter)
      | Some i -> (i, buffer#end_iter)
    else
      match start with
        None -> (buffer#start_iter, buffer#get_iter `INSERT)
      | Some i -> (buffer#start_iter, i)
  in
  let gsearch = buffer#re_search forward in
  match gsearch ~start ~stop (Pcre.regexp s_utf8) with
    None ->
      if wrapped then
        (wrapped, None)
      else
        let start = if forward then buffer#start_iter else buffer#end_iter in
        re_search_buffer ~wrapped: true forward buffer ~start s_utf8
  | Some (start,stop) ->
      (wrapped, Some (start, stop))

let replace_history = Ed_minibuffer.history ()

let query_replace_gen
  ?(mes="")
  command_name
  (fsearch_buffer : search_buffer_function)
  (freplace : searched: string -> found:string -> repl:string -> string)
  (v : sourceview) args =
  let mb = v#minibuffer in
  let len = Array.length args in
  if len <= 0 then
    let f s = Ed_commands.launch_command command_name [| s |] in
    let title = Printf.sprintf "Query-replace%s" mes in
    Ed_misc.input_string ~history: replace_history
      mb ~title "" f
  else
    if len = 1 then
      begin
        let title = Ed_misc.to_utf8
          (Printf.sprintf "Query-replace%s %s with" mes args.(0))
        in
        let f s = Ed_commands.launch_command command_name [| args.(0); s |] in
        Ed_misc.input_string ~history: replace_history
          mb ~title "" f
      end
    else
      begin
        let title = Ed_misc.to_utf8
          (Printf.sprintf "Query-replace%s %s with %s (y/n/!)"
           mes args.(0) args.(1))
        in
        let s1_utf8 = Ed_misc.to_utf8 args.(0) in
        let s2_utf8 = Ed_misc.to_utf8 args.(1) in
        let rec iter interactive =
          let b = v#file#buffer in
          let it = b#get_iter `INSERT in
          let start = it in
          match fsearch_buffer true (*=forward*) b ~start s1_utf8 with
            true, _
          | _, None -> mb#set_active false
          | false, Some (start,stop) ->
              if interactive then
                (
                 v#set_location (location_of_iter start);
                 b#select_range start stop;
                 ignore(v#source_view#scroll_to_iter start);
                 ignore(v#source_view#scroll_to_iter stop)
                );
              let replace () =
                v#place_cursor ~scroll: interactive start;
                let found = b#get_text ~start ~stop () in
                b#delete ~start ~stop;
                (*              prerr_endline
                   (Printf.sprintf "searched=%s, found=%s, repl=%s"
                   s1_utf8 found s2_utf8);
                *)
                let new_text = freplace
                  ~searched: s1_utf8 ~found ~repl: s2_utf8
                in
                b#insert new_text
              in
              if interactive then
                (
                 let f_yes () = replace (); iter true in
                 let f_no () =
                   v#place_cursor stop;
                   iter true
                 in
                 let f_bang () = replace (); iter false in
                 mb#clear;
                 mb#set_more_key_bindings
                   [ [[], GdkKeysyms._y], f_yes ;
                     [[], GdkKeysyms._n], f_no ;
                     [[], GdkKeysyms._exclam], f_bang ;
                   ];
                 mb#set_text ~fixed: title "";
                 if not mb#active then (mb#set_active true; mb#wait);
                )
              else
                (replace (); iter interactive)
        in
        iter true
      end

let query_replace = query_replace_gen
  (Printf.sprintf "%s_query_replace" factory_name)
  search_buffer
  (fun ~searched ~found ~repl -> repl)

let re_replace ~searched ~found ~repl =
  let rex = Pcre.regexp searched in
  Pcre.replace_first ~rex ~templ: repl found

let re_query_replace = query_replace_gen
  ~mes: " regexp"
  (Printf.sprintf "%s_query_replace_re" factory_name)
  re_search_buffer
  re_replace

let paste (v: sourceview) args =
  let text =
    let len = Array.length args in
    if len > 0 then
      Some args.(0)
    else
      let selection = GMain.selection#text in
      (*
         prerr_endline (Printf.sprintf "Selection=%s"
         (match selection with None -> "<None>" | Some s -> s));
      *)
      match selection with
        None -> GMain.clipboard#text
      | x -> x
  in
  (*
     prerr_endline (Printf.sprintf "Text=%s"
     (match text with None -> "<None>" | Some s -> s));
  *)
  match text with
    None -> ()
  | Some text ->
      pastable_history#add (Ed_misc.to_utf8 text);
      v#file#buffer#insert text;
      v#update_my_location

let copy (v: sourceview) args =
  v#file#buffer#copy_clipboard GMain.clipboard
let cut (v: sourceview) args =
  v#file#buffer#cut_clipboard GMain.clipboard;
  v#update_my_location

let beginning_of_line (v : sourceview) args = v#beginning_of_line
let end_of_line (v : sourceview) args = v#end_of_line
let undo (v : sourceview) args = v#undo
let redo (v : sourceview) args = v#redo
let forward_word v args = v#forward_word
let backward_word v args = v#backward_word
let forward_line v args = v#forward_line
let backward_line v args = v#backward_line
let forward_char v args = v#forward_char
let backward_char v args = v#backward_char
let kill_line v args =
  v#kill_line ~append: (Ed_commands.same_previous_command ())

let kill_word v args =
  let concat =
    if Ed_commands.same_previous_command () then
      Some `APPEND
    else
      None
  in
  v#kill_word ?concat true

let backward_kill_word v args =
  let concat =
    if Ed_commands.same_previous_command () then
      Some `PREPEND
    else
      None
  in
  v#kill_word ?concat false

let delete_char v args = v#delete_char true
let backward_delete_char v args = v#delete_char false

let transpose_chars v args = v#transpose_chars
let transpose_lines v args = v#transpose_lines
let transpose_words v args = v#transpose_words

let yank_choose v args =
  let mb = v#minibuffer in
  let title = "Choose text to paste (Up/Down to choose):" in
  let on_eval () =
    let s_utf8 = mb#get_user_text in
    paste v [| s_utf8 |];
    mb#set_active false
  in
  mb#clear ;
  mb#set_on_eval on_eval;
  mb#set_text ~fixed: title "";
  mb#set_history pastable_history;
  mb#set_active true

let insert (v:sourceview) args =
  Array.iter v#insert args

let goto_history = Ed_minibuffer.history ()
let goto_line v args =
  let f s =
    let n =
      try Cam_misc.my_int_of_string args.(0)
      with _ -> invalid_arg "Bad line number"
    in
    v#goto_line (n-1)
  in
  Ed_misc.input_command_arg
    v#minibuffer ~history: goto_history
    ~title: "Go to line"
    f (Printf.sprintf "%s_goto_line" factory_name) args

let goto_char v args =
  let f s =
    let n =
      try Cam_misc.my_int_of_string args.(0)
      with _ -> invalid_arg "Bad character number"
    in
    v#goto_char (n-1)
  in
  Ed_misc.input_command_arg
    v#minibuffer ~history: goto_history
    ~title: "Go to char"
    f (Printf.sprintf "%s_goto_char" factory_name) args

let force_save v args = v#do_save

let syntax_mode_history = Ed_minibuffer.history ()
let set_syntax_mode v args =
  let len = Array.length args in
  if len > 0 then
    let name = args.(0) in
    try
      let langs = Gtksv_utils.available_source_languages ~manager: language_manager () in
      let lang = List.find (fun l -> l#name = name)  langs in
      v#set_syntax_mode (Some lang)
    with
      Not_found ->
        Ed_misc.error_message
          (Printf.sprintf "Unknown syntax mode \"%s\"" name)
  else
    let f mode =
      let com = Printf.sprintf "%s_set_syntax_mode %s"
        factory_name (Filename.quote mode)
      in
      Ed_commands.eval_command com
    in
    let languages = List.map
      (fun l -> l#name)
      (Gtksv_utils.available_source_languages ~manager: language_manager ())
    in
    Ed_misc.select_string ~history: syntax_mode_history
      v#minibuffer
      ~title: "Syntax mode"
      ~choices: languages
      ""
      f

let popup_syntax_mode_choice v args =
  let com s =
    Ed_commands.eval_command
      (Printf.sprintf "%s_set_syntax_mode %s"
       factory_name (Filename.quote s))
  in
  let entries = List.map
    (fun l ->
       `I (l#name, (fun () -> com l#name))
    )
      (Gtksv_utils.sort_languages_by_name
       (Gtksv_utils.available_source_languages ~manager: language_manager ()))
  in
  GToolbox.popup_menu
    ~button: 1
    ~time: (Int32.zero)
    ~entries

let mode_history = Ed_minibuffer.history ()
let set_mode v args =
  let len = Array.length args in
  if len > 0 then
    let name = args.(0) in
    try
      match Ed_misc.no_blanks name with
        "" -> v#set_mode None
      | _ -> v#set_mode (Some (get_mode name))
    with
      Failure s->
        Ed_misc.error_message s
  else
    let f mode =
      let com = Printf.sprintf "%s_set_mode %s"
        factory_name (Filename.quote mode)
      in
      Ed_commands.eval_command com
    in
    Ed_misc.select_string ~history: mode_history
      v#minibuffer
      ~title: "Mode"
      ~choices: (available_mode_names ())
      ""
      f

let popup_mode_choice v args =
  let com s =
    Ed_commands.eval_command
      (Printf.sprintf "%s_set_mode %s"
       factory_name (Filename.quote s))
  in
  let entries =
    (`I ("None", fun () -> com "''")) ::
      (List.map
       (fun s -> `I (s, (fun () -> com s)))
         (available_mode_names ()))
  in
  GToolbox.popup_menu
    ~button: 1
    ~time: (Int32.zero)
    ~entries

let switch_line_numbers (view : sourceview) args =
  let v =
    if Array.length args > 0 then
      Some (Ed_misc.bool_of_string args.(0))
    else
      None
  in
  view#switch_line_numbers ?v ()

let switch_line_markers (view : sourceview) args =
  let v =
    if Array.length args > 0 then
      Some (Ed_misc.bool_of_string args.(0))
    else
      None
  in
  view#switch_line_markers ?v ()

let set_wrap_mode (view : sourceview) args =
  let com = Printf.sprintf "%s_set_wrap_mode" factory_name in
  if Array.length args < 1 then
    let f s = Ed_commands.launch_command com [| s |] in
    Ed_misc.select_string view#minibuffer ~title: com
      ~choices: (List.map Ed_sourceview_rc.string_of_wrap_mode [`CHAR;`NONE;`WORD])
      "" f
  else
    let mode = Ed_sourceview_rc.wrap_mode_of_string args.(0) in
    view#set_wrap_mode mode

let insert_utf8 (view : sourceview) args =
  if Array.length args < 1 then
    ()
  else
    try
      let code = int_of_string args.(0) in
      let s = Cam_misc.utf8_char_of_code code in
      view#file#buffer#insert s
    with
      Invalid_argument _ ->
        let mes = Printf.sprintf "insert_utf8: invalid argument (%s)" args.(0) in
        Ed_misc.error_message mes
;;

let set_encoding (view : sourceview) args =
  if Array.length args < 1 then
    let com = Printf.sprintf "%s_set_encoding" factory_name in
    let f s = Ed_commands.launch_command com [| s |] in
    Ed_misc.select_string view#minibuffer ~title: com
      ~choices: Ed_charsets.charsets
      "" f
  else
    view#set_encoding (Some args.(0))
;;

let set_bookmark (view : sourceview) args =
  let com = Printf.sprintf "%s_set_bookmark" factory_name in
  if Array.length args < 1 then
    (
     let f s = Ed_commands.launch_command com [| s |] in
     Ed_misc.input_string view#minibuffer ~title: com
       "" f
    )
  else
    (
     let data =
       let (l,c) = view#location_in_buffer in
       (view#filename, l, c)
     in
     Ed_bookmarks.set bookmarks args.(0) data;
     view#file#update_source_marks;
     view#update_menus
    )
;;

let remove_bookmark (view : sourceview) args =
  let com = Printf.sprintf "%s_remove_bookmark" factory_name in
  if Array.length args < 1 then
    (
     let choices = List.map fst (Ed_bookmarks.list bookmarks) in
     let f s = Ed_commands.launch_command com [| s |] in
     Ed_misc.select_string view#minibuffer ~title: com
       ~choices
       "" f
    )
  else
    (
     Ed_bookmarks.remove bookmarks args.(0);
     view#update_menus
    )
;;

let push_location (view : sourceview) args =
  let loc = (view#filename, view#location_in_buffer) in
  Ed_fstack.push loc location_stack;
  Ed_misc.set_active_action_message "Location pushed"
;;

let pop_location (view : sourceview) args =
  try
    let (file, (line,col)) = Ed_fstack.pop location_stack in
    Ed_commands.eval_command (Printf.sprintf "open_file %s" (Filename.quote file));
    if view#filename = file then
      view#set_location (line,col)
  with
    Ed_fstack.Empty ->
      let m = "Location stack is empty" in
      Ed_hooks.error_message m;
      Ed_misc.set_active_action_message m

;;

let forward_location (view : sourceview) args =
  try
    let (file, (line,col)) = Ed_fstack.forward location_stack in
    Ed_commands.eval_command (Printf.sprintf "open_file %s" (Filename.quote file));
    if view#filename = file then
      view#set_location (line,col)
  with
    Ed_fstack.Empty ->
      let m = "Location stack is forward-empty" in
      Ed_hooks.error_message m;
      Ed_misc.set_active_action_message m
;;



let coms =
  [
    "switch_buffer", [| |], None, switch_buffer ;
    "destroy_buffer", [| |], None, destroy_buffer ;
    "query_replace", [| |], None, query_replace ;
    "query_replace_re", [| |], None, re_query_replace ;
    "search", [| |], None, search search_buffer "search" true ;
    "search_backward", [| |], None, search search_buffer "search" false ;
    "search_re", [| |], None, search re_search_buffer "regexp search" true ;
    "search_re_backward", [| |], None, search re_search_buffer "regexp search" false ;
    "beginning_of_line", [| |], None, beginning_of_line ;
    "end_of_line", [| |], None, end_of_line ;
    "undo", [| |], None, undo ;
    "redo", [| |], None, redo ;
    "forward_word", [| |], None, forward_word ;
    "backward_word", [| |], None, backward_word ;
    "forward_line", [| |], None, forward_line ;
    "backward_line", [| |], None, backward_line ;
    "forward_char", [| |], None, forward_char ;
    "backward_char", [| |], None, backward_char ;
    "paste", [| |], None, paste ;
    "copy", [| |], None, copy ;
    "cut", [| |], None, cut ;
    "kill_line", [| |], None, kill_line ;
    "kill_word", [| |], None, kill_word ;
    "backward_kill_word", [| |], None, backward_kill_word ;
    "yank_choose", [| |], None, yank_choose ;
    "insert", [| |], Some "utf8 strings to insert", insert ;
    "goto_line", [|"line"|], None, goto_line ;
    "goto_char", [|"character"|], None, goto_char ;
    "force_save", [| |], None, force_save ;
    "delete_char", [| |], None, delete_char ;
    "backward_delete_char", [| |], None, backward_delete_char ;
    "transpose_chars", [| |], None, transpose_chars ;
    "transpose_lines", [| |], None, transpose_lines ;
    "transpose_words", [| |], None, transpose_words ;
    "set_syntax_mode", [| "Syntax mode" |], None, set_syntax_mode ;
    "popup_syntax_mode_choice", [| |], None, popup_syntax_mode_choice ;
    "set_mode", [| "Mode" |], None, set_mode ;
    "popup_mode_choice", [| |], None, popup_mode_choice ;
    "switch_line_numbers", [| "optional value" |], None, switch_line_numbers ;
    "switch_line_markers", [| "optional value" |], None, switch_line_markers ;
    "set_wrap_mode", [| "mode" |], None, set_wrap_mode ;
    "insert_utf8", [| "utf8 code" |], None, insert_utf8 ;
    "set_encoding", [| "encoding" |], None, set_encoding ;
    "set_bookmark", [| "name" |], None, set_bookmark ;
    "remove_bookmark", [| "name" |], None, remove_bookmark ;
    "push_location", [| |], None, push_location ;
    "pop_location", [| |], None, pop_location ;
    "forward_location", [| |], None, forward_location ;
  ]

let _ = List.iter
  (fun (name, args, more, f) ->
     register_com ~prefix: factory_name name args ?more f)
    coms