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

(** Misc *)

let map_opt f = function
    None -> None
  | Some v -> Some (f v)


let rec my_int_of_string s =
  let len = String.length s in
  if len <= 0 then invalid_arg "my_int_of_string";
  match s.[0] with
    '+' -> my_int_of_string (String.sub s 1 (len - 1))
  | _ -> int_of_string s


let add_shortcut w l ((mods, k), action) =
  try
    let (c_opt, f) = List.assoc action l in
    Okey.add ?cond: c_opt w ~mods k f
  with
    Not_found ->
      prerr_endline (Ed_messages.error_unknown_action action)
;;

(* from GToolbox *)
let mOk = "Ok"
let mCancel = "Cancel"
let input_widget ~widget ~event ~get_text ~bind_ok ~expand
    ~title ?(ok=mOk) ?(cancel=mCancel) message =
  let retour = ref None in
  let window = GWindow.dialog ~title ~modal:true () in
  ignore (window#connect#destroy ~callback: GMain.Main.quit);
  let main_box = window#vbox in
  let hbox_boutons = window#action_area in

  let vbox_saisie = GPack.vbox ~packing: (main_box#pack ~expand: true) () in

  let _wl_invite = GMisc.label
      ~text: message
      ~packing: (vbox_saisie#pack ~padding: 3)
      ()
  in

  vbox_saisie#pack widget ~expand ~padding: 3;

  let wb_ok = GButton.button ~label: ok
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  wb_ok#grab_default ();
  let wb_cancel = GButton.button ~label: cancel
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  let f_ok () =
    retour := Some (get_text ()) ;
    window#destroy ()
  in
  let f_cancel () =
    retour := None;
    window#destroy ()
  in
  ignore(wb_ok#connect#clicked f_ok);
  ignore(wb_cancel#connect#clicked f_cancel);

  (* the enter key is linked to the ok action *)
  (* the escape key is linked to the cancel action *)
  event#connect#key_press ~callback:
    begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Return && bind_ok then f_ok ();
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then f_cancel ();
      false
    end;

  widget#misc#grab_focus ();
  window#show ();
  GMain.Main.main ();

  !retour
(* /from GToolbox *)

let select_in_list ?ok ?cancel ?(allow_empty=false) ?(value_in_list=true) ~title ~choices message =
  let wc = GEdit.combo
      ~popdown_strings: choices
      ~allow_empty
      ~value_in_list
      ()
  in
  input_widget ~widget:wc#coerce ~event:wc#entry#event
    ~get_text:(fun () -> wc#entry#text) ~bind_ok:true
    ~expand: false
    ~title ?ok ?cancel message

let remove_char s c =
  if s <> "" then
    for i = 0 to (String.length s) - 1 do
      if s.[i] = c then s.[i] <- ' '
    done;
  s

let line_of_char file n =
  try
    let chanin = open_in file in
    let rec iter l m =
      let s_opt =
        try Some (input_line chanin)
        with End_of_file -> None
      in
      match s_opt with
        None -> l
      | Some s ->
          let new_m = m + ((String.length s) + 1) in (* + 1 is for '\n' *)
          if new_m >= n then
            l
          else
            iter (l + 1) new_m
    in
    let l = iter 0 0 in
    close_in chanin ;
    l
  with
    Sys_error s ->
      prerr_endline s ;
      0

let char_of_line file n =
  let ic = open_in file in
  let rec iter acc l =
    if l >= n then
      acc
    else
      match
        try Some (String.length (input_line ic))
        with End_of_file -> None
      with
        None -> acc
      | Some n -> iter (acc+n+1) (l+1) (* acc+n+1 for the newline character *)
  in
  let n = iter 0 0 in
  close_in ic;
  n
;;

let escape_menu_label s = Ed_extern.replace_in_string ~pat: "_" ~subs: "__" ~s



let mod_date_of_file file =
  try (Unix.stat file).Unix.st_mtime
  with _ -> 0.0

let catch_print_exceptions f x =
  try f x
  with
    e ->
      let s =
        match e with
          Failure s
        | Sys_error s -> s
        | Unix.Unix_error (e,s1,s2) ->
            Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2
        | e -> Printexc.to_string e
      in
      Ed_hooks.error_message s



let to_utf8 ?coding s =
  match coding with
    Some charset ->
      Glib.Convert.convert
        ~to_codeset: "UTF-8" ~from_codeset: charset s
  | None ->
      try Glib.Convert.locale_to_utf8 s
      with _ ->
        try
          Glib.Convert.convert
            ~to_codeset: "UTF-8" ~from_codeset: Ed_core_rc.encoding#get s
        with
          _ -> s

let of_utf8 ?coding s =
  match coding with
    Some charset ->
      Glib.Convert.convert
        ~from_codeset: "UTF-8" ~to_codeset: charset s
  | None ->
      try Glib.Convert.locale_from_utf8 s
      with _ ->
        Glib.Convert.convert
          ~from_codeset: "UTF-8" ~to_codeset: Ed_core_rc.encoding#get s
(*
external _gtkevent_set_above_child : [> Gtk.event_box ] Gtk.obj -> bool -> unit =
  "ml_gtk_event_box_set_above_child"

let gtkevent_set_above_child evbox b =
 _gtkevent_set_above_child (Obj.magic evbox#as_widget :> [Gtk.event_box] Gtk.obj) b
*)

(** {2 Getting canonical filenames} *)

let equal_node n1 n2 =
  n1.Unix.st_ino = n2.Unix.st_ino && n1.Unix.st_dev = n2.Unix.st_dev;;

let set_active_state_message msg =
  Ed_commands.launch_command "set_active_state_message" [|msg|]
let set_active_action_message msg =
  Ed_commands.launch_command "set_active_action_message" [|msg|]
let display_message msg =
  Ed_hooks.display_message msg;
  set_active_action_message msg
let warning_message msg =
  Ed_hooks.warning_message msg;
  set_active_action_message msg
let error_message msg =
  Ed_hooks.error_message msg;
  set_active_action_message msg


let fail_if_unix_error f x =
  try f x
  with Unix.Unix_error (e,s1,s2) ->
    failwith ((Unix.error_message e)^": "^s1^" "^s2)

let is_prefix s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  len1 >= len2 && (String.sub s1 0 len2) = s2

let dir_entries ?prefix dir =
(*
  prerr_endline (Printf.sprintf "dir_entries: prefix=%s dir=%s"
		   (match prefix with None -> "" | Some s -> s)
		   dir);
*)
  let d = fail_if_unix_error Unix.opendir dir in
  let rec iter acc =
    let name_opt =
      try Some (Unix.readdir d)
      with End_of_file ->
	Unix.closedir d;
	None
    in
    match name_opt with
      None -> List.rev acc
    | Some name ->
	let acc =
	  match prefix with
	    None -> name::acc
	  | Some s ->
	      if is_prefix name s then
		name :: acc
	      else
		acc
	in
	iter acc
  in
  iter []

let max_common l =
  let pred char n s =
    String.length s >= (n+1) && s.[n] = char
  in
  let in_all c n = List.for_all (pred c n) l in
  match l with
    [] -> None
  | [s] -> Some s
  | h :: q ->
      let len = String.length h in
      let rec iter n =
	if n < len then
	  if in_all h.[n] n then
	    iter (n+1)
	  else
	    n
	else
	  len
      in
      let maxlen = iter 0 in
      Some (String.sub h 0 maxlen)

let select_file_history = Ed_minibuffer.history ()
let select_file (mb : Ed_minibuffer.minibuffer) ~title text f =
  let get_user_text () =
    let s = mb#get_user_text in
    let s = Glib.Convert.filename_from_utf8 s in
    let len = String.length s in
    if len > 0 && s.[0] = '~' then
      Printf.sprintf "%s%s"
        Ed_installation.home
        (String.sub s 1 (len - 1))
    else
      s
  in
  let on_complete () =
    let s = get_user_text () in
    try
      let is_dir =
        try (fail_if_unix_error Unix.stat s).Unix.st_kind = Unix.S_DIR
        with Failure _ -> false
      in
      let len = String.length s in
      let (list, text) =
        if is_dir && s.[len-1] = '/' then
          (
           let entries = dir_entries (Filename.dirname s) in
           (entries, s)
          )
        else
          (
           let dir = Filename.dirname s in
           let prefix = Filename.basename s in
           let entries = dir_entries ~prefix dir in
           match max_common entries with
             None -> (["[no match]"], s)
           | Some s ->
               let s = Filename.concat dir s in
               match entries with
                 [_] ->
                   if is_dir then
                     ([], s^"/")
                   else
                     ([], s)
	       | _ -> (entries, s)
          )
      in
      let (utf8_list, badly_encoded) =
        List.fold_right
          (fun f (acc, n) ->
             try (Glib.Convert.filename_to_utf8 f :: acc, n)
             with _ -> (acc, n+1)
          )
          list
          ([], 0)
      in
      let fixed =
        Printf.sprintf "%s%s: "
          (if badly_encoded > 1 then
             Printf.sprintf "[%d badly encoded entries] " badly_encoded
           else "")
          title
      in
      mb#set_text
        ~list: utf8_list
        ~fixed
        (Glib.Convert.filename_to_utf8 text)
    with
      Failure err ->
        mb#set_text ~list: [to_utf8 err] ~fixed: (title^": ") s
  in
  mb#clear;
  let on_eval () =
    let s = Glib.Convert.filename_from_utf8 (get_user_text ())in
    mb#set_text "";
    mb#set_active false;
    match s with
      "" -> ()
    | _ -> f s
  in
  mb#set_text ~fixed: (title^": ") text;
  mb#set_on_eval on_eval;
  mb#set_on_complete on_complete;
  mb#set_history select_file_history;
  mb#set_active true
(*
  let dir = match dir with None -> None | Some s -> Some (ref s) in
  GToolbox.select_file ~title ?dir ()
*)

let select_string ?history (mb : Ed_minibuffer.minibuffer) ~title ~choices text f =
  let on_complete () =
    let s = of_utf8 mb#get_user_text in
    let entries = List.filter
        (fun choice -> is_prefix choice s)
        choices
    in
    let (list,text) =
      match max_common entries with
        None -> (["[No match]"], s)
      | Some s ->
          match entries with
            [_] -> ([], s)
          | _ -> (entries, s)
    in
    mb#set_text
      ~list: (List.map to_utf8 list)
      ~fixed: (title^": ")
      (to_utf8 text)
  in
  let on_eval () =
    let s = of_utf8 mb#get_user_text in
    mb#set_text "";
    mb#set_active false;
    f s
  in
  mb#clear;
  mb#set_text ~fixed: (title^": ") text;
  (match history with None -> () | Some h -> mb#set_history h);
  mb#set_on_eval on_eval;
  mb#set_on_complete on_complete;
  mb#set_active true

let input_string ?history (mb : Ed_minibuffer.minibuffer) ~title text f =
  let on_complete () = () in
  let on_eval () =
    let s = of_utf8 mb#get_user_text in
    mb#set_text "";
    mb#set_active false;
    f s
  in
  mb#clear;
  mb#set_text ~fixed: (title^": ") text;
  (match history with None -> () | Some h -> mb#set_history h);
  mb#set_on_eval on_eval;
  mb#set_on_complete on_complete;
  mb#set_active true

let input_command_arg mb ?history ~title f com args =
  let ask ?err text =
    let f s =
      let com = Printf.sprintf "%s %s" com s in
      Ed_commands.eval_command com
    in
    let title = Printf.sprintf "%s%s"
        (match err with None -> "" | Some s -> "["^s^"] ")
        title
    in
    input_string ?history mb ~title text f
  in
  let len = Array.length args in
  if len > 0 then
    try f args.(0)
    with Invalid_argument err -> ask ~err args.(0)
  else
    ask ""

let confirm (mb : Ed_minibuffer.minibuffer) text f =
  let g () =
    let s = of_utf8 mb#get_user_text in
    mb#set_text "";
    mb#set_active false;
    if String.length s > 0 then
      match s.[0] with
        'y'|'Y' -> f ()
      | _ -> ()
    else
      ()
  in
  mb#clear ;
  mb#set_text ~fixed: (Printf.sprintf "%s (y/n) " text) "";
  mb#set_on_eval g;
  mb#set_active true

(*  GToolbox.question_box ~title: "Question"
    ~buttons: ["Ok" ; "Cancel"] ~default: 2 text = 1
*)

let choice_in_list f choices =
  let entries =
    List.map
      (fun (utf8, name) ->
        (`I (escape_menu_label utf8, fun () -> f name)))
      choices
  in
  GToolbox.popup_menu
    ~button: 1 ~time: Int32.zero
    ~entries




let string_of_bool = function
  true -> "true"
| false -> "false"

let bool_of_string = function
  "true" -> true
| _ -> false

let date_of_file filename =
  try Some ((Unix.stat filename).Unix.st_mtime)
  with _ -> None

let same_files f1 f2 =
  let f () =
    let st1 = Unix.stat f1
    and st2 = Unix.stat f2 in
    st1.Unix.st_dev = st2.Unix.st_dev &&
    st1.Unix.st_ino = st2.Unix.st_ino
  in
  fail_if_unix_error f ()

let safe_same_files f1 f2 =
  try same_files f1 f2 with _ -> false
