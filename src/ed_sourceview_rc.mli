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

(** Configuration of the "sourceview" view. *)

(** Name of the view. *)
val factory_name : string

(** The user's configuration file. *)
val rc_file : string

(** The function to get the configuration file of a mode, from the mode name. *)
val mode_rc_file : string -> string

(** The function to get the local configuration file of a mode, from the mode name. *)
val local_mode_rc_file : string -> string

(** The bookmarks file. *)
val bookmarks_rc_file : string

(** Key bindings of the view. *)
val key_bindings : (Okey.keyhit_state * string) Config_file.list_cp

(** Associations between regular expressions on filenames and mime type,
     to get the correct language syntax highlighting. *)
val filename_language_patterns : (string * string) Config_file.list_cp

(** Associations between regular expressions in filenames and mode names. *)
val filename_mode_patterns : (string * string) Config_file.list_cp

(** Maximum undo levels. *)
val max_undo_levels : Config_file.int_cp

(** Default wrap mode to use when creating a sourceview, if no indication is given. *)
val default_wrap_mode : [ `CHAR | `NONE | `WORD ] Config_file.cp_custom_type

(** Return a string which can be used to describe the given wrap mode.*)
val string_of_wrap_mode : [< `CHAR | `NONE | `WORD ] -> string

(** Return the wrap mode corresponding to the given string. *)
val wrap_mode_of_string : string -> [> `CHAR | `NONE | `WORD ]

(** Default regexp for words. *)
val default_word_re : string

(** Read the configuration file. *)
val read : unit -> unit

(** Write the configuration file. *)
val write : unit -> unit

(** This function add a key binding for the view, described by a
     combination of key hits and an associated command name. *)
val add_sourceview_key_binding : Okey.keyhit_state -> string -> unit

(** Same as {!add_sourceview_key_binding} but the combination of
     key hits is given as a string. *)
val add_sourceview_key_binding_string : string -> string -> unit

(** This function create, for a key bindings option and a given mode name,
     two functions to a add key binding to the mode, and also register
     a command to add a binding to the mode.
     The command name is called [add_sourceview_mode_<mode name>_key_binding].
     This function uses {!Ed_gui_rc.create_add_binding_commands}.*)
val create_add_sourceview_mode_binding_commands :
  (Okey.keyhit_state * string) Config_file.list_cp ->
  string ->
  (Okey.keyhit_state -> string -> unit) * (string -> string -> unit)
