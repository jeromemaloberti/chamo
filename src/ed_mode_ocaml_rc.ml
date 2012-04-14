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

let mode_name = "ocaml"
let includes_global_var = mode_name^"_includes"

let rc_file = Ed_sourceview_rc.mode_rc_file mode_name
let local_rc_file = Ed_sourceview_rc.local_mode_rc_file mode_name

let group = new CF.group
let x = 1 - 1
let default_key_bindings  = [
    [[], GdkKeysyms._Tab], mode_name^"_indent_line" ;
    [[`CONTROL], GdkKeysyms._x ; [`CONTROL], GdkKeysyms._Tab], mode_name^"_indent_buffer";
    [[`CONTROL], GdkKeysyms._x ; [`CONTROL], GdkKeysyms._a], mode_name^"_switch_file";
    [[`CONTROL], GdkKeysyms._o ; [`CONTROL], GdkKeysyms._c], mode_name^"_build";
    [[`MOD1], GdkKeysyms._t], mode_name^"_display_type_annot";
    [[`CONTROL; `MOD1], GdkKeysyms._t], mode_name^"_copy_type_annot";
    [[`MOD1], GdkKeysyms._c], mode_name^"_display_call_annot";
    [[`MOD1], GdkKeysyms._i], mode_name^"_display_ident_annot";
    [[`MOD1], GdkKeysyms._j], mode_name^"_jump_to_local_def";
    [[`CONTROL;`MOD1], GdkKeysyms._c], mode_name^"_show_stack_calls";
    [[`CONTROL;`MOD1], GdkKeysyms._x], mode_name^"_expand_ext_idents";
]

let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"

let stack_call_bgcolor =
  new CF.string_cp ~group
    ["stack_call_colors";"background"] "red"
    "Background color of non-tail calls"
;;
let stack_call_fgcolor =
  new CF.string_cp ~group
    ["stack_call_colors";"foreground"] "yellow"
    "Foreground color of non-tail calls"
;;



let read () = group#read rc_file
let write () = group#write rc_file

let (add_sourceview_mode_ocaml_key_binding,
   add_sourceview_mode_ocaml_key_binding_string) =
  Ed_sourceview_rc.create_add_sourceview_mode_binding_commands
    key_bindings mode_name;;

let local_group = new CF.group

let ocamlbuild_commands = new CF.list_cp
  (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers) ~group: local_group
    ["ocamlbuild_commands"] [] "ocamlbuild commands associated to edited files";;

let local_read () = local_group#read local_rc_file;;
let local_write () =
  match ocamlbuild_commands#get with
  [] -> (try Sys.remove local_rc_file with _ -> ())
  | _ -> local_group#write local_rc_file
;;

 