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

(* $Id: cam_com_history.ml 758 2011-01-13 07:53:27Z zoggy $ *)

module O = Config_file

let history : string option array = Array.make Ed_constant.com_history_size None

let pos = ref 0
let n = ref 0

let history_option = new O.list_cp O.string_wrappers
    ~group: Ed_rc.core_ini ["commands_history"]
    []
    ""

let get () =
  let rec iter acc nb_read i =
    if nb_read >= !n then
      acc
    else
      let pred_i =
	if i <= 0 then
	  Ed_constant.com_history_size - 1
	else
	  i - 1
      in
      match history.(pred_i) with
	None -> acc
      |	Some e ->
	  iter (e::acc) (nb_read+1) pred_i
  in
  List.rev (iter [] 0 !pos)

let add e =
  history.(!pos) <- Some e;
  n := min (!n+1) Ed_constant.com_history_size ;
  pos := (!pos + 1) mod Ed_constant.com_history_size;
  history_option#set (get ());
  Ed_rc.save_core ()

let init () =
  List.iter add (List.rev history_option#get)
