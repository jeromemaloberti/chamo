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

(** The "ocamltop" output, to display output of evaluation of ocaml code. *)

(** The name of the default ocamltop output. *)
val output_name : string

(** Class to create ocamltop output objects (with ocaml syntax highlight). *)
class ocamltop_output : ?on_destroy: (unit -> unit)  -> string ->
  object
    inherit Ed_outputs.text_output
  end

(** Return the ocamltop output object. The output is created if it does not exist.*)
val ocamltop_output : unit -> ocamltop_output

(** The original function of the "print_ocaml_output" command. *)
val print_ocaml_output : ?output: Ed_outputs.text_output -> string array -> unit

