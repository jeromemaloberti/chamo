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

open GdkKeysyms

let string_of_mod_key = Configwin.key_to_string

let string_of_state l =
  String.concat " " (List.map string_of_mod_key l)

let latex_of_key_bindings l =
  let b = Buffer.create 256 in
  Buffer.add_string b "\\begin{tabular}{|l|l|}\\hline\n";
  let f (ks, command) =
    Printf.bprintf b "{\\bf %s} & %s \\\\ \\hline\n"
      (string_of_state ks) command
  in
  List.iter f l;
  Buffer.add_string b "\\end{tabular}\n";
  let s = Buffer.contents b in
  Ed_misc.replace_in_string ~pat: "_" ~subs: "\\_" ~s
;;
