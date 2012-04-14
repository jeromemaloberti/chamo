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

(** Multi-contents clipboards.
   The {!Multiclip_gui} module contains material to create graphical interface for these
   clipboards.*)


type multiclip

(** Create a multiclip.
     @param abst_len is the length of the abstract string displayed in the list.*)
val create_multiclip : ?abst_len:int -> unit -> multiclip

(** Add a string to the multiclip.
     @param abs can be used to force the abstract. By default, it is obtained
     by chopping the given string at the abstract length [abst_len] specified
     at the multiclip creation. It another element with the same abstract alreay
     exists, [#1] is appended to the abstract, or [#2] if #1 is already used and
     so on.*)
val add : multiclip -> ?abs:string -> string -> unit

(** The list of elements in the multiclip, as a list of pairs [(abstract, string)]. *)
val elements : multiclip -> (string * string) list

(** Remove an elements, by giving is abstract. *)
val remove : multiclip -> string -> unit


(** {2 Storable multiclips}
     These multiclips can be stored to a file given at creation time. *)

type storable_multiclip

(** [create_storable_multiclip file] creates a new multiclip, stored and read from
     the given [file].
     @param abst_len is the same as in {!create_multiclip}.
     *)
val create_storable_multiclip : ?abst_len:int -> string -> storable_multiclip

(** Reload the contents of the multiclip from its file. *)
val read_multiclip : storable_multiclip -> unit

(** Write the contents of the multiclip into its file. *)
val write_multiclip : storable_multiclip -> unit

(** Get the raw multiclip. *)
val storable_get_multiclip : storable_multiclip -> multiclip

(** Get the file use by the given multiclip. *)
val storable_get_file : storable_multiclip -> string
