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

(* $Id: cam_args.mli 758 2011-01-13 07:53:27Z zoggy $ *)

(** Command line analysis *)

val commands : string list ref

(** Remaining arguments *)
val remaining: string list ref

(** Add a command as it it were passed with [-e] on command line. *)
val append_command : string -> unit

val init_commands : string list ref

(** Add an init command, different from command-line passed commands. *)
val append_init_command : string -> unit

val add_option : (string * Arg.spec * string) -> unit

val parse : unit -> unit
