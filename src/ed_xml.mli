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

(** *)

type t =
    E of Xmlm.tag * t list
  | D of string
;;

(** Get a string to represent the given xml tree.
  @decl when set to [false], do not output xml heading declaration. Default is [true]
*)
val string_of_xml : ?decl: bool -> t -> string

(** Parse a string to build a XML tree.
  @strip can be used to strip blanks (see Xmlm documentation for details).
     @raise Failure in case of error. *)
val xml_of_string : ?strip: bool -> string -> t

(** [read_xml_file file f] reads the file to get an xml tree
     and applies [f] on the xml tree. *)
val read_xml_file : ?strip: bool -> string -> (t -> 'a) -> 'a

