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

(* $Id: cam_messages.ml 334 2006-10-06 07:34:42Z zoggy $ *)

(** Using ocamlbuild. *)

let commands = Hashtbl.create 17;;
let history = Ed_minibuffer.history ();;

let default_build_command file =
  let out = Printf.sprintf "%s.byte"
    (Filename.chop_extension (Filename.basename file))
  in
  let wd = Unix.getcwd () in
  prerr_endline file;
  if Ed_misc.is_prefix wd file then
    (
     let len_file = String.length file in
     let len_wd = String.length wd in
     let s = String.sub file (len_wd + 1) (len_file - len_wd - 1) in
     let inc = Filename.dirname s in
     Printf.sprintf "ocamlbuild -I %s %s"
       (Filename.quote inc) (Filename.quote out)
    )
  else if Filename.is_relative file then
      (
       let inc = Filename.dirname file in
       Printf.sprintf "ocamlbuild -I %s %s"
        (Filename.quote inc) (Filename.quote out)
      )
    else
      (
       let d = Filename.dirname file in
       Printf.sprintf "(cd %s && ocamlbuild %s)"
         (Filename.quote d) (Filename.quote out)
      )
;;

let output_name = "ocamlbuild";;
let ocamlbuild_output = ref None;;
let ocamlbuild_output () =
  match !ocamlbuild_output with
    None ->
      let o = new Ed_outputs.text_output
        ~on_destroy: (fun () -> ocamlbuild_output := None)
          output_name
      in
      ocamlbuild_output := Some o ;
      o
  | Some o -> o
;;

let goto_error file line start stop error =
  match !Ed_sourceview.active_sourceview with
    None -> ()
  | Some v ->
      let com = Printf.sprintf "open_file \"%s\" %d,%d" file (line-1) start in
      Ed_commands.eval_command com;
(*      v#set_location ((line-1), start);*)
      let mes = Printf.sprintf "Line %d, chars %d-%d: %s" line start stop error in
      Ed_misc.error_message (Ed_misc.to_utf8 mes);
      let line_offset = Ed_misc.char_of_line file (line-1) in
      v#select_range_in_file ~jump: `Left
        ~left: (line_offset + start)
        ~right: (line_offset + stop)
        ()
;;

type problem =
  { pb_file : string ;
    pb_line : int ;
    pb_start : int ;
    pb_stop : int ;
    pb_kind : [ `Error of string | `Warning of char * string ] ;
  }
;;

let warning_is_error c =
  let to_show = Ed_commands.safe_get_global "warn_error" in
  let len = String.length to_show in
  let res = ref false in
  for i = 0 to len - 1 do
    if to_show.[i] = c || to_show.[i] = 'A' then
      res := true
    else if to_show.[i] = Char.lowercase c || to_show.[i] = 'a' then
        res := false
  done;
  !res
;;


let analyze_ocaml_compilation on_problem text =
  let lines = Ed_extern.split_string text ['\n'] in
  let rec iter = function
    [] | [_] -> ()
  | line1 :: line2 :: q ->
      let f file line start stop =
        let kind =
          try
            let f c =
              let line_len = String.length line2 in
              let warn_len = String.length "Warning X: " in
              let msg = String.sub line2 warn_len (line_len - warn_len) in
              `Warning (c, msg)
            in
            Scanf.sscanf line2 "Warning %c: " f
          with _ -> `Error line2
        in
        let pb =
          { pb_file = file ;
            pb_line = line ;
            pb_start = start ;
            pb_stop = stop ;
            pb_kind = kind ;
          }
        in
        if on_problem pb then iter q else ()
      in
      try Scanf.sscanf line1 "File %S, line %d, characters %d-%d:" f
      with Scanf.Scan_failure _ -> iter (line2 :: q)
  in
  iter lines
;;

let run ?(output=ocamlbuild_output()) command =
  let outputs = Ed_outputs.outputs () in
  let output' =
    try outputs#output_by_name output#name
    with Not_found ->
        outputs#add_output (output :> Ed_outputs.output);
        outputs#output_by_name output#name
  in
  outputs#show output#name;
  let on_end code =
    output'#set_label (Printf.sprintf "%s (ret %d)" output#name code);
    analyze_ocaml_compilation
      (fun pb ->
         let error = match pb.pb_kind with
             `Error s -> Some s
           | `Warning (c, s) ->
               if warning_is_error c then
                 Some s
               else
                 None
         in
         match error with
           None -> true
         | Some msg ->
             goto_error pb.pb_file pb.pb_line pb.pb_start pb.pb_stop msg;
             false
      )
      output#contents
  in
  output#run ~reset: true command on_end
;;

let build (v:Ed_sourceview.sourceview) args =
  let file = v#file#filename in
  let com =
    try Hashtbl.find commands file
    with Not_found -> default_build_command file
  in
  let on_ok com =
    Hashtbl.replace commands file com;
    Ed_mode_ocaml_rc.ocamlbuild_commands#set
      (Hashtbl.fold
       (fun f com acc -> (f,com)::acc) commands []);
    run com
  in
  Ed_misc.input_string ~history
    v#minibuffer
    ~title: "Command" (Ed_misc.to_utf8 com) on_ok
;;


 