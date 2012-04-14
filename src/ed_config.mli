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

(** Some configuration constants. *)

(** The glade file describing chamo's windows *)
val glade_file : string

(** The prefix to use for "local" config files,
     that is files not stored in the user's cameleon config directory
     but stored for example in the directory where the editor
     was launched. *)
val local_dir_rc_file_prefix : string

(** Create a "local" config file name with the given suffix. *)
val local_dir_rc_file : string -> string

(** Create a config file with the given suffix, in the user's 
     cameleon config directory. *)
val rc_file : string -> string

(** {2 Convenient function to use configuration files.} *)

(** Wrappers to convert key states to and from a string. *)
val key_state_wrappers :
  (Gdk.Tags.modifier list * int) list Config_file.wrappers

(** Wrappers to convert key bindings to and from a string. *) 
val binding_wrappers :
  ((Gdk.Tags.modifier list * int) list * string) Config_file.wrappers
