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

(** UTF8 functions. *)

(** [utf8_index_of_char string n] returns the position of the first byte
     the [n]th character in the given UTF-8 [string].
     @raise Not_found if there is no such character.*)
val utf8_index_of_char : string -> int -> int

(** [utf8_char_of_index string i] returns the number of characters
     found from the beginning of the UTF-8 string to position [i] (included).
     @raise Invalid_argument if the given position is out of the string bounds.*)
val utf8_char_of_index : string -> int -> int

(** [utf8_string_length string] returns the number of utf8 characters in the
     given string. *)
val utf8_string_length : string -> int

(** [utf8_char_of_code code] returns the string representing the UTF-8 character
  with the given [code].
  @raise Failure if the code is out of range. Only 4 bytes UTF-8 is supported by now.
  *)
val utf8_char_of_code : int -> string
