(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Evaluating OCaml phrases to control with the editor. *)

let _ = Toploop.set_paths ()
let _ = Toploop.initialize_toplevel_env()

let init_file = Filename.concat Cam_rc.rc_dir "chamo_init.ml"
let local_init_file = ".chamo_init.ml"

let prompt_eval_history = Ed_minibuffer.history ()
let prompt_eval args =
  (* FIXME: todo: show the "eval" window *)
  match !Ed_gui.active_window with
    None -> ()
  | Some w ->
      let f code =
        Ed_commands.launch_command "eval" [| code |]
      in
      let mb = w#minibuffer in
      Ed_misc.input_string
        ~history: prompt_eval_history
        mb ~title: "eval"
        ""
        f

let _ =
  let com = {
      Ed_commands.com_name = "prompt_eval" ;
      com_args = [| |] ;
      com_more_args = None ;
      com_f = prompt_eval ;
    }
  in
  Ed_commands.register com

let eval_file args =
  if Array.length args < 1 then
    match !Ed_gui.active_window with
      None -> ()
    | Some w ->
        let f file =
          Ed_commands.launch_command "eval_file" [| file |]
        in
        let mb = w#minibuffer in
        Ed_misc.select_file
          mb ~title: "eval_file"
          ""
          f
  else
    (
     let buf = Buffer.create 256 in
     let fmt = Format.formatter_of_buffer buf in
     ignore(Toploop.use_file fmt args.(0));
     Ed_commands.launch_command
       "print_ocaml_output" [| (Buffer.contents buf) |]
    )

let _ =
  let com = {
      Ed_commands.com_name = "eval_file" ;
      com_args = [| "File" |];
      com_more_args = None ;
      com_f = eval_file ;
    }
  in
  Ed_commands.register com

let eval_ocaml args =
  let len = Array.length args in
  if len < 1 then
    Ed_commands.eval_command "prompt_eval"
  else
    (
     let code = args.(0) in
     (* use a temporary file to use the Toploop.use_file function
        instead of parsing the phrase, then exectuing it, because
        we don't want to depend on whether we have the compiled
        sources of ocaml (the needed modules to analyse the parse
        exceptions are not installed (Errors, ...).
     *)
     let tmp_file = Filename.temp_file Ed_messages.software "ml" in
     Ed_misc.file_of_string ~file: tmp_file code;
     eval_file [| tmp_file |];
     Ed_misc.safe_remove_file tmp_file;
    )

let _ =
  let com = {
      Ed_commands.com_name = "eval" ;
      com_args = [| "OCaml Code" |];
      com_more_args = None ;
      com_f = eval_ocaml ;
    }
  in
  Ed_commands.register com

let load_file args =
  if Array.length args < 1 then
    match !Ed_gui.active_window with
      None -> ()
    | Some w ->
        let f file =
          Ed_commands.launch_command "load_file" [| file |]
        in
        let mb = w#minibuffer in
        Ed_misc.select_file
          mb ~title: "load_file"
          ""
          f
  else
    (
     let file = args.(0) in
     let code = Printf.sprintf "#load \"%s\";;" file in
     eval_ocaml [| code |]
(* this does not work, because Symtable.Error may be raised
   and we can't print the error message since Bytecomp interface is not
   installed by ocaml
     let buf = Buffer.create 256 in
     let fmt = Format.formatter_of_buffer buf in
     let success = Topdirs.load_file fmt file in
     if success then
       Ed_misc.set_active_action_message
         (Printf.sprintf "Successfully loaded file %s" file)
     else
       Ed_misc.set_active_action_message
         (Printf.sprintf "Failed to load file %s" file)
*)
    )

let _ =
  let com = {
      Ed_commands.com_name = "load_file" ;
      com_args = [| "File" |];
      com_more_args = None ;
      com_f = load_file ;
    }
  in
  Ed_commands.register com

(** {2 Adding command line options} *)

let use file =
  let com = Printf.sprintf "eval_file %s" (Filename.quote file) in
  Cam_args.append_init_command com

let option_use =
  ("--use", Arg.String use, "<file>\n\t\tocaml-evaluate the given file after initialization")

let _ = Cam_args.add_option option_use;;

(** {2 Init} *)

(** Add the cameleon2 lib dir to directories where to look for interfaces. *)

(* temporarily replace printing functions, to avoid having the
   initializations displaying messages *)
let (old_m,old_w) =
  (Ed_hooks.get_display_message (), Ed_hooks.get_warning_message ());;
Ed_hooks.set_display_message (fun ?to_utf8 _ -> ());;
Ed_hooks.set_warning_message (fun ?to_utf8 _ -> ());;

let _ =
  let default_dirs = [
      Cam_installation.lib_dir ;
      Cam_installation.lablgtk2_dir ;
      Cam_installation.pcre_dir ;
    ]
  in
  List.iter
    (fun d ->
       eval_ocaml
         [| Printf.sprintf "#directory \"%s\";;" d |])
    default_dirs
;;
(* Restore the printing functions *)
Ed_hooks.set_display_message old_m;;
Ed_hooks.set_warning_message old_w;;

(** If the init file exists, then add a command to eval it at launch time *)

let _ =
  let init_files = [ init_file ; local_init_file] in
  List.iter
    (fun file -> if Sys.file_exists file then use file)
    init_files