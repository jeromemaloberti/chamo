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

(* $Id: cam_hooks.mli 758 2011-01-13 07:53:27Z zoggy $ *)

(** Hooks. *)

val set_display_message : (?to_utf8:bool -> string -> unit) -> unit
val set_error_message : (?to_utf8:bool -> string -> unit) -> unit
val set_warning_message : (?to_utf8:bool -> string -> unit) -> unit

val display_message : ?to_utf8:bool -> string -> unit
val error_message : ?to_utf8:bool -> string -> unit
val warning_message : ?to_utf8:bool -> string -> unit

val get_display_message : unit -> (?to_utf8:bool -> string -> unit)
val get_error_message : unit -> (?to_utf8:bool -> string -> unit)
val get_warning_message : unit -> (?to_utf8:bool -> string -> unit)
