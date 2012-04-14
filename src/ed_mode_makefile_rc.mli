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

(** Configuration file of the makefile mode. *)

(** The mode name. *)
val mode_name : string

(** The user's configuration file. *)
val rc_file : string

(** The option for the key bindings in the mode. *)
val key_bindings : (Okey.keyhit_state * string) Config_file.list_cp

(** Read the configuration file. *)
val read : unit -> unit

(** Write the configuration file. *)
val write : unit -> unit

(** Same as {!Ed_gui_rc.add_window_key_binding} and
     {!Ed_gui_rc.add_window_key_binding_string} but for the key bindings
     of the mode. *)
val add_sourceview_mode_makefile_key_binding :
  Okey.keyhit_state -> string -> unit
val add_sourceview_mode_makefile_key_binding_string :
  string -> string -> unit
