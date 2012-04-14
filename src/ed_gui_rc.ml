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
open GdkKeysyms

let rc_file = Ed_config.rc_file "gui"

let group = new CF.group

let default_minibuffer_history_size = 50

let default_abort_binding = ([`CONTROL], _g)

let default_window_key_bindings  = [
  [[`CONTROL], _x; [`CONTROL], _c], "close_active_window" ;
  [[`CONTROL], _n], "new_window" ;
  [[`CONTROL], _x ; [`CONTROL], _s], "save_active_view" ;
  [[`CONTROL], _x ; [`CONTROL], _w], "save_active_view_as" ;
  [[`CONTROL], _x ; [`CONTROL], _f], "open_file" ;
  [[`CONTROL], _b], "cycle_view" ;
  [[`CONTROL], _Tab], "cycle_tab" ;
  [[`CONTROL;`SHIFT], _Tab], "rev_cycle_tab" ;
  [[`CONTROL], _x; [`CONTROL], _t], "new_tab" ;
  [[`CONTROL], _v], "split_vertically" ;
  [[`CONTROL], _x; [`SHIFT], _3], "split_vertically" ;
  [[`CONTROL], _h], "split_horizontally" ;
  [[`CONTROL], _x; [`SHIFT], _2], "split_horizontally" ;
  [[`CONTROL], _x ; [`SHIFT], _0], "destroy_active_view" ;
  [[`CONTROL], _x ; [`CONTROL], _x; [], _l], "print_key_bindings" ;
  [[`CONTROL], GdkKeysyms._y], "paste" ;
  [[`CONTROL], GdkKeysyms._c], "copy" ;
  [[`CONTROL], GdkKeysyms._w], "cut" ;
  [[`MOD1], GdkKeysyms._x], "prompt_command" ;
]

let minibuffer_history_size =
  new CF.int_cp ~group ["minibuffer_history_size"]
    default_minibuffer_history_size
    "The size of histories in minibuffer"

let abort_binding =
  new CF.cp_custom_type Configwin.key_cp_wrapper ~group
    ["abort_binding"] default_abort_binding
    "The key combination to use to reset the key stroke state"

let window_key_bindings =
  new CF.list_cp Ed_config.binding_wrappers ~group
    ["window_key_bindings"] default_window_key_bindings
    "Common key bindings for windows"

let read () = group#read rc_file
let write () = group#write rc_file

let _ = read () ; write ()

let trees_for_window additional =
  Okey.trees_of_list
    (List.map (fun (x,s) -> (x, fun () -> Ed_commands.eval_command s))
       (window_key_bindings#get @ additional))

let create_add_binding_commands 
  (option:(Okey.keyhit_state * string) Config_file.list_cp)
    name =
  let f_add = fun key_state command ->
    let l = (key_state, command) :: option#get in
    option#set l
  in
  let f_add_string = fun key_state_string ->
    let key_state = Ed_config.key_state_wrappers.Config_file.of_raw
      (Config_file.Raw.of_string key_state_string)
    in
    f_add key_state
  in
  let com_name = Printf.sprintf "add_%s_key_binding" name in
  let f_com args =
    let len = Array.length args in
    if len < 2 then
      failwith (Printf.sprintf "Usage: %s <list of keys> <command>" com_name);
    f_add_string args.(0) args.(1)
  in
  let com =
    { Ed_commands.com_name = com_name;
      com_args = [| "list of keys" ; "command" |] ;
      com_more_args = None ;
      com_f = f_com ;
    } 
  in
  Ed_commands.register com;
  (f_add, f_add_string)
    
let (add_window_key_binding, add_window_key_binding_string) =
  create_add_binding_commands window_key_bindings "window"

  
