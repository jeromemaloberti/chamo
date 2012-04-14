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

(* $Id: cam_rc.mli 758 2011-01-13 07:53:27Z zoggy $ *)

(** Handling configuration files *)

(** The directory where personal config files are stored. *)
val rc_dir : string

(** {2 The core configuration} *)

val core_ini : Config_file.group
val save_core : unit -> unit
val load_core : unit -> unit

(** {2 the GUI configuration file} *)

val gui_ini : Config_file.group
val save_gui : unit -> unit
val load_gui : unit -> unit

(** {2 Keeping windows positions and sizes} *)

(** [handle_window win name] *)
val handle_window : GWindow.window -> string -> unit

(** {2 Utils} *)

val add_binding :
  < get : ((Gdk.Tags.modifier list * int) * 'a) list;
    set : ((Gdk.Tags.modifier list * int) * 'a) list -> 'b; .. > ->
  string -> 'a -> 'b
