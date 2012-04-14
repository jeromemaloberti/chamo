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

(** Handling the configuration file for windows. *)

(** The user's configuration file. *)
val rc_file : string

(** The option for the size of histories in minibuffers. *)
val minibuffer_history_size : Config_file.int_cp

(** The "abort" key binding. This is the binding use to abort
     the current key press sequence. Default is C-g. *)
val abort_binding : (Gdk.Tags.modifier list * Gdk.keysym) Config_file.cp_custom_type

(** The key bindings for the chamo windows. These bindings are always
     accessible whatever the view which has the focus. *)
val window_key_bindings : (Okey.keyhit_state * string) Config_file.list_cp

(** Read the configuration file. *)
val read : unit -> unit

(** Write the configuration file. *)
val write : unit -> unit

(** [trees_for_window bindings] adds the given [bindings] to the ones
     of the windows (that is [window_key_bindings#get]) and return
     a handler tree to set has handler tree of a window.
     See the {!Okey} library for details about using such trees. *)
val trees_for_window :
  (Okey.keyhit_state * string) list -> Okey.handler_tree list

(** [create_add_bindings_commands option name] return two functions
   to add key bindings to the given option, given a key state as
   OCaml value of as a string.
   It also create and register a [add_<name>_key_binding] command
   which can be used to add a key binding to the given option.*)
val create_add_binding_commands :
  (Okey.keyhit_state * string) Config_file.list_cp -> string ->
    (Okey.keyhit_state -> string -> unit) *
    (string -> string -> unit)

(** These two functions add a key binding to the [window_key_bindings] option.
   The first takes a {!Okey.keyhit_state} and the command name, while the
   second one takes a string representing a {!Okey.key_state} and a command name.*)
val add_window_key_binding : Okey.keyhit_state -> string -> unit
val add_window_key_binding_string : string -> string -> unit
