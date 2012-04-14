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

module CF = Config_file

let base_name = "minibuffer"
let rc_file = Ed_config.rc_file base_name
let mode_rc_file = Ed_config.rc_file base_name

let group = new CF.group

let default_key_bindings  = [
    [[], GdkKeysyms._Return], base_name ^ "_eval" ;
    [[`MOD1], GdkKeysyms._Return], base_name ^ "_insert \"\n\"" ;
    [[], GdkKeysyms._Tab], base_name ^ "_complete";
  ]
;;
let default_history_key_bindings = [
  [[], GdkKeysyms._Up], base_name ^ "_history_previous" ;
  [[], GdkKeysyms._Down], base_name ^ "_history_next" ;
  ]
;;
let default_exiting_keys : Okey.keyhit_state list = [
    [[], GdkKeysyms._Up];
    [[], GdkKeysyms._Down];
    [[], GdkKeysyms._Escape];
  ]
;;
let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"

let history_key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["history_key_bindings"] default_history_key_bindings "History key bindings"

let exiting_keys = new CF.list_cp Ed_config.key_state_wrappers ~group
    ["exiting_keys"] default_exiting_keys
    "Combinations of keys to exit from the minibuffer"

let read () = group#read rc_file
let write () = group#write rc_file

let (add_minibuffer_key_binding, add_minibuffer_key_binding_string) =
  Ed_gui_rc.create_add_binding_commands key_bindings base_name

