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

module CF = Config_file

let factory_name = "multiclip"
let rc_file = Ed_config.rc_file factory_name

let group = new CF.group

let default_key_bindings = [
    [[`CONTROL], GdkKeysyms._m ; [`CONTROL], GdkKeysyms._c], factory_name^"_copy_selection" ;
    [[`CONTROL], GdkKeysyms._m ; [`MOD1], GdkKeysyms._c], factory_name^"_copy" ;
    [[`CONTROL], GdkKeysyms._m ; [`CONTROL], GdkKeysyms._x], factory_name^"_remove" ;
];;

let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"
;;

let read () = group#read rc_file;;
let write () = group#write rc_file;;

let (add_multiclip_key_binding, add_multiclip_key_binding_string) =
  Ed_gui_rc.create_add_binding_commands key_bindings factory_name