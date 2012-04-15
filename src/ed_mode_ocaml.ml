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

(** OCaml sourceview mode. *)

let _ = Ed_mode_ocaml_rc.read ()
let _ = Ed_mode_ocaml_rc.write ()
let _ = Ed_mode_ocaml_rc.local_read ()
let _ = Ed_mode_ocaml_rc.local_write ()

let mode_name = Ed_mode_ocaml_rc.mode_name

let remove_first_blanks s =
  let len = String.length s in
  try
    let rec first_no_blank p =
      if p < len then
        match s.[p] with
          ' ' | '\t' -> first_no_blank (p+1)
        | _ -> p
      else
        raise Not_found
    in
    let p = first_no_blank 0 in
    String.sub s p (len - p)
  with
    Not_found -> ""

let indent_line (v : Ed_sourceview.sourceview) args =
  let line = v#current_line in
  let b = v#file#buffer in
  let pos = b#get_iter `INSERT in
  let cnum = pos#line_offset in
  let bol = b#get_iter (`LINE line) in
  let eol = bol#forward_to_line_end in

  let code =
    v#file#of_utf8
      (v#file#mode_from_display
       (b#get_text ~start: b#start_iter ~stop: eol ()))
  in
  let indentations =
    match Ed_ocaml_lexer.get_lines_indentation code with
      `Failure (e,(start,stop),l) ->
        let err = Printf.sprintf "chars %d-%d: %s"
          start stop (Ed_ocaml_lexer.report_error e) in
        Ed_misc.set_active_action_message (Ed_misc.to_utf8 err);
        l
    | `Success l ->
        l
  in
  if List.length indentations <= line then
    ()
  else
    match List.rev indentations with
      [] -> ()
    | None :: _ -> ()
    | (Some n) :: _ ->
        let codeline = v#file#of_utf8
          (v#file#mode_from_display
           (b#get_text ~start: bol ~stop: eol ()))
        in
        let len = String.length codeline in
        let new_codeline = remove_first_blanks codeline in
        let len2 = String.length new_codeline in
        let user_pos =
          let beg = len - len2 in
          if cnum < beg then 0 else cnum - beg
        in
        let new_codeline = Printf.sprintf "%s%s"
            (String.make n ' ')
            new_codeline
        in
        if new_codeline <> codeline then
          begin
            b#delete ~start: bol ~stop: eol;
            let pos = b#get_iter (`LINE line) in
            v#place_cursor pos;
            b#insert (v#file#mode_to_display (v#file#to_utf8 new_codeline));
            let pos = b#get_iter (`LINECHAR (line,n+user_pos)) in
            v#place_cursor pos
          end
        else
          ()

let indent_buffer (v : Ed_sourceview.sourceview) args =
  let current_line = v#current_line in
  let b = v#file#buffer in
  let code = v#file#of_utf8
    (v#file#mode_from_display
     (b#get_text ~start: b#start_iter ~stop: b#end_iter ()))
  in
  match Ed_ocaml_lexer.get_lines_indentation code with
    `Failure (e,(start,stop),_) ->
      let err = Printf.sprintf "chars %d-%d: %s"
          start stop (Ed_ocaml_lexer.report_error e) in
      Ed_misc.set_active_action_message (Ed_misc.to_utf8 err)
  | `Success indentations ->
      let lines = Ed_extern.split_string ~keep_empty: true code ['\n'] in
      let nb_lines = List.length lines in
      let nb_indentations = List.length indentations in
      let indentations =
        if nb_indentations < nb_lines then
          indentations @
          (Ed_extern.make_list (nb_lines - nb_indentations) None)
        else
          indentations
      in
      b#delete ~start: b#start_iter ~stop: b#end_iter;
      v#place_cursor b#start_iter;
      List.iter2
        (fun line nopt ->
          let line =
            match nopt with
              None -> line^"\n"
            | Some n ->
                let s = remove_first_blanks line in
                Printf.sprintf "%s%s\n" (String.make n ' ') s
          in
          b#insert (v#file#mode_to_display (v#file#to_utf8 line))
        )
        lines
        indentations;
      v#place_cursor (b#get_iter (`LINE current_line));
      let message =
        Printf.sprintf "%d lines indented / %d lines in buffer"
          nb_indentations nb_lines
      in
      Ed_misc.set_active_action_message (Ed_misc.to_utf8 message)

let switch_file (v:Ed_sourceview.sourceview) args =
  let f = v#file#filename in
  try
    let filename2 =
      let ext =
	if Filename.check_suffix f ".ml" then
          "mli"
	else if Filename.check_suffix f ".mli" then
          "ml"
	else
	  raise Not_found
      in
      Printf.sprintf "%s.%s" (Filename.chop_extension f) ext
    in
    let com = Printf.sprintf "open_file %s" (Filename.quote filename2) in
    Ed_commands.eval_command com
  with
    Not_found -> ()

let __alpha__ = 1;;

let annot_file_build_dir_max_parent_level = ref 3;;

let find_annot_file file =
  let annot_file =
    try
      if Filename.check_suffix file ".ml" then
        (Filename.chop_extension file)^".annot"
      else
        raise Not_found
    with _ -> failwith "File has no .ml extension"
  in
  if Sys.file_exists annot_file then
    annot_file
  else
    begin
      let annot_file = Filename.basename annot_file in
      let rec iter n up_path down_path =
        if n < !annot_file_build_dir_max_parent_level then
          let dir = Filename.concat
            (Filename.concat up_path "_build")
            down_path
          in
          let f = Filename.concat dir annot_file in
          if Sys.file_exists f then
            f
          else
            iter (n+1) (Filename.dirname up_path)
              (Filename.concat (Filename.basename up_path) down_path)
        else
          failwith (Printf.sprintf "No %s file found." annot_file)
      in
      iter 0 (Filename.dirname file) ""
    end
;;

let load_annot_tree ml_file =
  let annot_file = find_annot_file ml_file in
  match Ed_misc.date_of_file ml_file, Ed_misc.date_of_file annot_file with
    None, _ -> failwith ("Could not access "^ml_file)
  | Some _, None -> failwith ("Could not access "^annot_file)
  | Some d1, Some d2 ->
      if d1 > d2 then
        failwith
          (Printf.sprintf "Source was modified since %s was created" annot_file)
      else
        let annot_string = Ed_extern.string_of_file annot_file in
        match Ed_annot.build_tree annot_string with
          None -> failwith "No tree built"
        | Some t -> t
;;

let display_annot ?(id_jump=false) ?(copy=false) kind (v:Ed_sourceview.sourceview) args =
  let f = v#file#filename in
  try
    let t = load_annot_tree f in
    let loc_start =
      let (start,_) = v#file#buffer#selection_bounds in
      (* beware of the possible offset between file contents and display *)
      Ed_utf8.utf8_string_length
        (v#file#mode_from_display
         (v#file#buffer#get_text ~start: v#file#buffer#start_iter ~stop: start ()))
    in
    match Ed_annot.search_in_tree kind loc_start t with
      None -> failwith "No annotation found"
    | Some (left,right,k) ->
        let (left, right, message) =
          match k with
            Ed_annot.Type f -> (left, right, Lazy.force f)
          | Ed_annot.Ident (Ed_annot.Def id)
          | Ed_annot.Ident (Ed_annot.Ext_ref id) -> (left, right, id)
          | Ed_annot.Ident (Ed_annot.Int_ref (id, (start, stop))) ->
              let s = Printf.sprintf "local def: %s" id in
              if id_jump then
                (start, stop, s)
              else
                (left, right, s)
          | Ed_annot.Call `Tail -> (left, right, "tail")
          | Ed_annot.Call `Stack -> (left, right, "stack")
        in
        let message = Ed_misc.to_utf8 message in
        if copy then
          GMain.clipboard#set_text message
        else
          v#select_range_in_file ~jump: `Left ~left ~right ();
        Ed_misc.set_active_action_message  message;
  with
    Not_found ->
      ()
  | Failure s
  | Sys_error s ->
      Ed_misc.set_active_action_message s
;;

let call_display_tag_name = mode_name^"_call";;
let create_call_display_tag (buf : Ed_sourceview.my_buffer) =
  buf#create_tag
    ~name: call_display_tag_name
    [ `FOREGROUND Ed_mode_ocaml_rc.stack_call_fgcolor#get ;
      `BACKGROUND Ed_mode_ocaml_rc.stack_call_bgcolor#get ;
    ]
;;

let show_hide_call_annots (v:Ed_sourceview.sourceview) args =
  let f = v#file#filename in
  try
    let buf = v#file#buffer in
    let tt = new GText.tag_table buf#tag_table in
    match tt#lookup call_display_tag_name with
    | Some t ->
        buf#remove_tag_by_name call_display_tag_name
          ~start: buf#start_iter ~stop: buf#end_iter;
        tt#remove t;
        Ed_misc.set_active_action_message
          (Ed_misc.to_utf8 "Calls on stack hidden")
    | None ->
        ignore(create_call_display_tag buf);
        let t = load_annot_tree f in
        let f acc = function
          (left, right, Some (Ed_annot.Call k)) -> (left, right, k) :: acc
        | _ -> acc
        in
        let calls = Ed_annot.fold f [] t in
        let display_call n (left, right, kind) =
          match kind with
            `Tail -> n
          | `Stack ->
              let (left, right) = v#file#range_from_range_in_file ~left ~right in
              let start = buf#get_iter (`OFFSET left) in
              let stop = buf#get_iter (`OFFSET right) in
              buf#apply_tag_by_name call_display_tag_name ~start ~stop;
              n+1
        in
        let n = List.fold_left display_call 0 calls in
        Ed_misc.set_active_action_message
          (Ed_misc.to_utf8 (Printf.sprintf "%d calls on stack showed" n))
  with
  | Failure s
  | Sys_error s ->
      Ed_misc.set_active_action_message s
;;

let display_type_annot = display_annot `Type;;
let copy_type_annot = display_annot ~copy: true `Type;;
let display_call_annot = display_annot `Call;;
let display_ident_annot = display_annot `Ident;;
let jump_to_local_def = display_annot ~id_jump: true `Ident;;

let expand_external_idents (v:Ed_sourceview.sourceview) args =
  let f = v#file#filename in
  try
    let t = load_annot_tree f in
    let from_pervasives s = Ed_misc.is_prefix s "Pervasives." in
    let f acc (left, right, kind) =
      match kind with
        Some (Ed_annot.Ident (Ed_annot.Ext_ref ext_ident)) ->
          if from_pervasives ext_ident then
            acc
          else
            (left, right, ext_ident) :: acc
      | _ -> acc
    in
    let ext_refs = List.sort
      (fun (l1, _, _) (l2, _, _) -> Pervasives.compare l1 l2)
        ((Ed_annot.fold f [] t))
    in
    let b = v#file#buffer in
    let mb = v#minibuffer in
    let title s1 s2 = Printf.sprintf
      "Replace %s by %s ? (y/n/!)" s1 s2
    in
    let nb_replaced = ref 0 in
    let rec iter interactive offset = function
      [] -> mb#set_active false
    | (left, right, ext_id) :: q ->
        let left = offset + left in
        let right = offset + right in
        let (b_left, b_right) = v#file#range_from_range_in_file ~left ~right in
        let current = b#get_text
          ~start: (b#get_iter (`OFFSET b_left))
            ~stop: (b#get_iter (`OFFSET b_right))
            ()
        in
        if current = ext_id then
          iter interactive offset q
        else
          begin
            let start = b#get_iter (`OFFSET b_left) in
            b#select_range start (b#get_iter (`OFFSET b_right));
            ignore(v#source_view#scroll_to_iter start);
            let replace () =
              v#place_cursor (b#get_iter (`OFFSET b_left));
              b#delete
                ~start: (b#get_iter (`OFFSET b_left))
                ~stop: (b#get_iter (`OFFSET b_right));
              b#insert ext_id;
              incr nb_replaced;
            in
            let new_offset = offset - (right - left) + (String.length ext_id) in
            if interactive then
              (
               let f_yes () = replace (); iter interactive new_offset q in
               let f_no () = iter interactive offset q in
               let f_bang () = replace (); iter false new_offset q in
               mb#clear;
               mb#set_more_key_bindings
                 [ [[], GdkKeysyms._y], f_yes ;
                   [[], GdkKeysyms._n], f_no ;
                   [[], GdkKeysyms._exclam], f_bang ;
                 ];
               let title = title current ext_id in
               mb#set_text ~fixed: title "";
               if not mb#active then (mb#set_active true; mb#wait);
              )
            else
              (replace (); iter interactive new_offset q)
          end
    in
    iter true 0 ext_refs ;
    Ed_misc.set_active_action_message
      (Ed_misc.to_utf8 (Printf.sprintf "%d ident(s) replaced." !nb_replaced))
  with
  | Failure s
  | Sys_error s ->
      Ed_misc.set_active_action_message s
;;

let coms = [
    "indent_line", [| |], None, indent_line ;
    "indent_buffer", [| |], None, indent_buffer ;
    "switch_file", [| |], None, switch_file ;
    "display_type_annot", [| |], None, display_type_annot ;
    "copy_type_annot", [| |], None, copy_type_annot ;
    "display_call_annot", [| |], None, display_call_annot ;
    "display_ident_annot", [| |], None, display_ident_annot ;
    "jump_to_local_def", [| |], None, jump_to_local_def ;
    "show_stack_calls", [| |], None, show_hide_call_annots;
    "expand_ext_idents", [| |], None, expand_external_idents;
    "build", [| "file" |], None, Ed_ocamlbuild.build ;
]

let _ = List.iter
    (fun (name, args, more, f) ->
      Ed_sourceview.register_com
        ~prefix: mode_name name args ?more f)
    coms

class mode =
  object
    inherit Ed_sourceview.empty_mode
    method name = mode_name
    method key_bindings : (Okey.keyhit_state * string) list =
      Ed_mode_ocaml_rc.key_bindings#get
    method menus : (string * GToolbox.menu_entry list) list =
      [
        "OCaml",
          [ `I ("Switch file", fun () -> Ed_commands.eval_command "ocaml_switch_file") ;
            `S ;
            `I ("Indent line", fun () -> Ed_commands.eval_command "ocaml_indent_line") ;
            `I ("Indent buffer", fun () -> Ed_commands.eval_command "ocaml_indent_buffer") ;
            `S ;
            `I ("Run ocamlbuild", fun () -> Ed_commands.eval_command "ocaml_build") ;
            `S ;
            `I ("Display type from .annot file", fun () -> Ed_commands.eval_command "ocaml_display_type_annot") ;
            `I ("Display call kind from .annot file", fun () -> Ed_commands.eval_command "ocaml_display_call_annot") ;
            `I ("Display ident info from .annot file", fun () -> Ed_commands.eval_command "ocaml_display_ident_annot") ;
            `I ("Jump to local def (from .annot file)", fun () -> Ed_commands.eval_command "ocaml_jump_to_local_def") ;
            `I ("Show stack calls (from .annot file)", fun () -> Ed_commands.eval_command "ocaml_show_stack_calls") ;
          ]
      ]

    method word_re = "[a-zA-Z0-9_']+"
  end

let mode = new mode;;
let _ = Ed_sourceview.register_mode mode;;
let _ = Ed_commands.register_after
  { Ed_commands.com_name = Ed_constant.com_on_exit ;
    com_args = [| |] ;
    com_more_args = None ;
    com_f = (fun _ -> Ed_mode_ocaml_rc.local_write ()) ;
  };;
List.iter
  (fun (f, com) -> Hashtbl.add Ed_ocamlbuild.commands f com)
  Ed_mode_ocaml_rc.ocamlbuild_commands#get
;;