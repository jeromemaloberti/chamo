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

(* $Id: cam_hooks.ml 600 2007-11-14 09:14:22Z zoggy $ *)

(** Generic bookmark system. *)

type 'a bookmarks

(** [create_from_cf_wappers wrappers] create a bookmarks structure.
     The given [wrappers] are used to store the data of bookmarks
     in a formatted file.
     @param desc can be used to describe the bookmarks in the file.*)
val create_from_cf_wrappers : ?desc: string -> 'a Config_file.wrappers -> 'a bookmarks

(** Same as {!create_from_cf_wrappers}, the wrappers being built from
     the given [data_of_string] and [string_of_data] functions.*)
val create : ?desc: string -> (string -> 'a) -> ('a -> string) -> 'a bookmarks

(** [get bk name] retrieve the data associated to the given name.
     @raise Not_found if no data is associated to the given name. *)
val get : 'a bookmarks -> string -> 'a

(** [set bk name data] creates or modify a bookmark by associating the given
     [data] to the given [name]. *)
val set : 'a bookmarks -> string -> 'a -> unit

(** [remove bk name] removes the bookmarks with the given [name]. *)
val remove : 'a bookmarks -> string -> unit

(** [list bk] returns all the bookmarks as a list of [(name, data)]. *)
val list : 'a bookmarks -> (string * 'a) list

(** [store bk file] stores the bookmarks [bk] in the given [file].
     @raise Sys_error if an error occurs while storing the file. *)
val store : 'a bookmarks -> string -> unit

(** [load bk file] loads fills the bookmarks structure [bk] from the given [file].
     All previous bookmarks in the structure are removed.
     @raise Sys_error if an error occurs while reading the file. *)
val load : 'a bookmarks -> string -> unit
