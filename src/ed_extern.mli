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

(** Caml-get imported functions. *)

(*i==v=[String.chop_n_char]=1.0====*)
(** [chop_n_char n s] returns the given string where characters after position n
   are replaced by ["..."].
@@version 1.0
@@cgname String.chop_n_char*)
val chop_n_char : int -> string -> string
(*/i==v=[String.chop_n_char]=1.0====*)

(*i==v=[List.list_remove_doubles]=1.0====*)
(** [list_remove_doubles ?pred l] remove doubles in the given list [l], according
   to the optional equality function [pred]. Default equality function is [(=)].
@author Maxence Guesdon
@version 1.0
@cgname List.list_remove_doubles*)
val list_remove_doubles : ?pred:('k -> 'k -> bool) -> 'k list -> 'k list
(*/i==v=[List.list_remove_doubles]=1.0====*)

(*i==v=[File.subdirs]=0.1====*)
(** [subdirs path] returns the list of subdirectories of the given directory name.
   Returned names are relative to the given path.
@author Maxence Guesdon
@version 0.1
@raise Unix_error if an error occurs.
@cgname File.subdirs*)
val subdirs : string -> string list
(*/i==v=[File.subdirs]=0.1====*)

(*i==v=[String.replace_in_string]=1.0====*)
(** [replace_in_string ~pat ~subs ~s] replaces all occurences of
   pattern [pat] by [subs] in string [s].
@author Maxence Guesdon
@version 1.0
@cgname String.replace_in_string*)
val replace_in_string : pat:string -> subs:string -> s:string -> string
(*/i==v=[String.replace_in_string]=1.0====*)

(*i==v=[File.string_of_file]=1.0====*)
(** [string_of_file filename] returns the content of [filename]
   in the form of one string.
@author Maxence Guesdon
@version 1.0
@raise Sys_error if the file could not be opened.
@cgname File.string_of_file*)
val string_of_file : string -> string
(*/i==v=[File.string_of_file]=1.0====*)

(*i==v=[File.file_of_string]=1.1====*)
(** [file_of_string ~file str] creates a file named
   [filename] whose content is [str].
@author Fabrice Lefessant
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.file_of_string*)
val file_of_string : file:string -> string -> unit
(*/i==v=[File.file_of_string]=1.1====*)

(*i==v=[String.split_string]=1.1====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.1
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.1====*)

(*i==v=[File.safe_remove_file]=1.0====*)
(** Remove the given file, and ignore the error if
   the file does not exist (catch [Sys_error]).
@version 1.0
@cgname File.safe_remove_file*)
val safe_remove_file : string -> unit
(*/i==v=[File.safe_remove_file]=1.0====*)

(*i==v=[List.make_list]=1.0====*)
(** [make_list n ele] builds a list of [n] elements [ele].
@author Maxence Guesdon
@version 1.0
@cgname List.make_list*)
val make_list : int -> 'i -> 'i list
(*/i==v=[List.make_list]=1.0====*)

(*i==v=[String.no_blanks]=1.0====*)
(** [no_blanks s] returns the given string without any blank
   characters, i.e. '\n' '\r' ' ' '\t'.
@version 1.0
@cgname String.no_blanks*)
val no_blanks : string -> string
(*/i==v=[String.no_blanks]=1.0====*)

(*i==v=[Misc.try_finalize]=1.0====*)
(** [try_finalize f x g y] applies [f] to [x] and return
   the result or raises an exception, but in all cases
   [g] is applied to [y] before returning or raising the exception.
@author Didier Rémy
@version 1.0
@cgname Misc.try_finalize*)
val try_finalize : ('l -> 'm) -> 'l -> ('n -> unit) -> 'n -> 'm
(*/i==v=[Misc.try_finalize]=1.0====*)


