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

(** Configuration of the minibuffer. *)

(** Name of the minibuffer in command names, etc. *)
val base_name : string

(** The user's configuration file. *)
val rc_file : string

(** Key bindings of the minibuffer. *)
val key_bindings : (Okey.keyhit_state * string) Config_file.list_cp

(** Additional key bindings of the minibuffer when an history is set. *)
val history_key_bindings : (Okey.keyhit_state * string) Config_file.list_cp

(** Key combinations which exits from the minibuffer. *)
val exiting_keys : Okey.keyhit_state Config_file.list_cp

(** Read the configuration file. *)
val read : unit -> unit

(** Write the configuration file. *)
val write : unit -> unit

(** This function add a key binding for the minibuffers, described by a
     combination of key hits and an associated command name. *)
val add_minibuffer_key_binding : Okey.keyhit_state -> string -> unit

(** Same as {!add_minibuffer_key_binding} but the combination of
     key hits is given as a string. *)
val add_minibuffer_key_binding_string : string -> string -> unit
