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

(** OCamlbuild support, with some useful stuff for ocaml compilation handling.*)

(** The associations between a file and a compilation command. This table
   is used to propose to the user the last compilation command used for
   a given file. *)
val commands : (string, string) Hashtbl.t

(** Generate a default command to launch ocamlbuild on the given ocaml file,
   depending on the current working directory.*)
val default_build_command : string -> string

(** The name of the default ocamlbuild output. *)
val output_name : string

(** Return the ocamlbuild output object (see {!Ed_outputs.text_output}). The output
     is created if it does not exist.*)
val ocamlbuild_output : unit -> Ed_outputs.text_output

(** [goto_error file line start stop message] can be used to position
     the active sourceview on the given [file], on the given [line]
     and select on this line the characters from [start] to [stop].
     The [message] is displayed in the message zone of the view's window.
     This function is used to position the user on a compilation problem. *)
val goto_error : string -> int -> int -> int -> string -> unit

(** To represent a compilation problem. *)
type problem = {
  pb_file : string;
  pb_line : int;
  pb_start : int; (* index of start character of the problem on line *)
  pb_stop : int; (* index of end character on line *)
  pb_kind : [ `Error of string | `Warning of char * string ];
    (** The error/warning message, with a character defining the
       kind of warning.*)
}

(** This function returns [true] if the warning corresponding to the
     given character must be considered as an error. It uses the
     ["warn_error"] global variable to know which warnings must be considered
     as errors. The signification of this variable is the same as the
     [-warn-error] option of [ocamlc]. This variable can be set with
     the command [set_global warn_error Avd]. *)
val warning_is_error : char -> bool

(** [analyze_ocaml_compilation f text] looks, in the ocaml compilation output
     [text], for warnings and errors locations. When one is found, the
     [f] function is called. If this function returns [true], then
     [analyse_ocaml_compilation] continues to look for the next problem,
     and so on. If [f] returns [false], then the analyze is stopped.
*)
val analyze_ocaml_compilation : (problem -> bool) -> string -> unit

(** [run command] runs the given command, displays its output in
     the ocamlbuild output, and analyzes the output with
     the {!analyze_ocaml_compilation} function, using the
     {!warning_is_error} and {!goto_error} functions to eventually
     position the active sourceview on the problem.
     @param output can be used to specify another output object to
     use.
*)
val run : ?output:Ed_outputs.text_output -> string -> unit

(** [build view args] proposes a compilation command which uses ocamlbuild,
     to compile the file edited in the sourceview. The used can modify the
     command before launching it. The {!run} function above is used to
     run the compilation, display the result and eventually "jump" to the
     compilation problem.

     The command used is associated to the edited file, and this association
     is stored in the directory where Chamo was launched, in the file
     {!Ed_mode_ocaml_rc.local_rc_file}. So the command is kept and proposed
     to the user when this function is called, rather than the default
     ocamlbuild command returned by {!default_built_command}.
     *)
val build : Ed_sourceview.sourceview -> 'a -> unit
