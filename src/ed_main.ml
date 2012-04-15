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

(** Main module of the editor. *)

let ocaml_syntax_mode_installed () =
  let lm = Ed_sourceview.language_manager in
  let l = Gtksv_utils.available_source_languages ~manager: lm () in
  List.exists (fun l -> String.lowercase l#name = "objective caml") l
;;

let _ = Ed_args.parse ()
let main () =
  Ed_gui.on_last_window_close := GMain.Main.quit;
  ignore(Ed_log.get_log_window());
  List.iter Ed_commands.eval_command (!Ed_args.init_commands @ !Ed_args.commands);
  Ed_view.iter_factories
    (fun f -> Ed_misc.catch_print_exceptions (fun () -> f#on_start) ());
  begin
    try
      if Sys.file_exists !Ed_layout.layout_file then
        try
          let layout = Ed_layout.load_layout !Ed_layout.layout_file in
          Ed_layout.create_windows_of_layout layout
        with
          Failure s
        | Sys_error s ->
            prerr_endline s;
            raise Not_found
      else
        raise Not_found
    with
      Not_found ->
        match !Ed_gui.gui_windows with
          [] -> ignore(Ed_gui.create_window())
        | _ -> ()
  end;
  begin
    match !Ed_gui.active_window, !Ed_gui.gui_windows with
      None, w :: _ -> Ed_gui.active_window := Some w
    | _ -> ()
  end;
  let on_file file =
    Ed_commands.launch_command "open_file" [| file |]
  in
  List.iter on_file !Ed_args.remaining;
  if not (ocaml_syntax_mode_installed ()) then
    (
     let m = Printf.sprintf
       "No objective-caml syntax mode found in directories\n%s\nYou should copy %s into one of these directories and restart Chamo to be able to use the objetive caml syntax mode."
         (String.concat "\n" Ed_sourceview.language_manager#search_path)
         (Filename.concat Ed_installation.languages_specs_dir "ocaml.lang")
     in
     Ed_hooks.warning_message (Ed_misc.to_utf8 m);
    );
  GtkThread.set_do_jobs_delay 0.02;
  GtkThread.main ();
  Ed_view.iter_factories
    (fun f -> Ed_misc.catch_print_exceptions (fun () -> f#on_exit) ());
  Ed_commands.eval_command Ed_constant.com_on_exit
;;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let _ = safe_main (Unix.handle_unix_error main)
