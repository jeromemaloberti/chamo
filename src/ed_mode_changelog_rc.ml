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

let mode_name = "changelog"
let rc_file = Ed_sourceview_rc.mode_rc_file mode_name

let group = new CF.group

let default_key_bindings  = [
  [[], GdkKeysyms._Tab], Ed_sourceview_rc.factory_name^"_insert \"\t\"" ;
  [[`CONTROL], GdkKeysyms._x; [], GdkKeysyms._a],
  mode_name^"_new_day_entry" ;
]

let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"

let read () = group#read rc_file
let write () = group#write rc_file

let (add_sourceview_mode_changelog_key_binding,
   add_sourceview_mode_changelog_key_binding_string) =
  Ed_sourceview_rc.create_add_sourceview_mode_binding_commands
    key_bindings mode_name