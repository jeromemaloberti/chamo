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

(** Expansion of words in sourceviews. *)

type expand_context = {
  buffer : string;
  mutable pos : int;
  searched_rex : Pcre.regexp;
  searched_pattern : string;
  mutable prev_prop : string * int;
  mutable prev_inserted : string option;
  mutable already_proposed : string list;
}
val context : expand_context option ref
val create_context : string -> int -> string -> Pcre.regexp -> expand_context
val search_in_buffer :
  bool ->
  Ed_sourceview.my_buffer ->
  GText.iter -> GText.iter -> Pcre.regexp -> int * string
val get_next_proposition_in_buffer : expand_context -> string -> int * string
val get_next_buffer_in_history : string -> string option
val get_next_proposition : expand_context -> (bool * string) option
val get_pattern :
  Ed_sourceview.sourceview -> GText.iter -> string * Pcre.regexp
val expand : Ed_sourceview.sourceview -> string array -> unit
