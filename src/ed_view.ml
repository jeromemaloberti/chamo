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

(** Views *)

let _ = Ed_view_rc.read ()
let _ = Ed_view_rc.write ()

class type gui_view =
  object
    method box : GObj.widget
    method save : (unit -> unit) option
    method save_as : (unit -> unit) option
    method close : unit

    method reload : (unit -> unit) option

    method paste : (unit -> unit) option
    method copy : (unit -> unit) option
    method cut : (unit -> unit) option

    method kind : string
    method filename : string
    method attributes : (string * string) list

    method set_on_label_change : (string -> unit) -> unit
    method label : string

    method set_on_focus_in : (unit -> unit) -> unit
    method grab_focus : unit

    method dup : topwin -> gui_view option

    method destroy : unit
    method set_on_destroy : (unit -> unit) -> unit

    method key_bindings : (Okey.keyhit_state * string) list

    method menus : (string * GToolbox.menu_entry list) list
  end

and topwin =
  object
    method set_active_view : gui_view option -> unit
    method active_view : gui_view option
    method minibuffer : Ed_minibuffer.minibuffer
    method contains_view : gui_view -> bool
    method update_menus : unit
  end

class virtual dyn_label =
  object
    val mutable label = ""
    method label = label
    val mutable on_label_change = fun _ -> ()
    method set_on_label_change f = on_label_change <- f
    method set_label s =
      label <- s; on_label_change s
  end

class virtual dyn_destroyable (f_destroy : unit -> unit) =
  object(self)
    val mutable on_destroy = (fun () -> ())
    method destroy =
      on_destroy();
      f_destroy ()
    method set_on_destroy f = on_destroy <- f
  end

class type view_factory =
  object
    method name : string
    method open_file :
        topwin -> gui_view option -> ?attributes:(string*string) list -> string ->
          [`Use_view of gui_view | `New_view of gui_view]
    method open_hidden : (?attributes:(string*string) list -> string -> unit) option
    method on_start : unit
    method on_exit : unit
  end

let factories : (string, view_factory) Hashtbl.t = Hashtbl.create 13

let register_view_factory name o =
  try
    ignore(Hashtbl.find factories name);
    failwith (Printf.sprintf "Factory %s already registered" name)
  with
    Not_found ->
      Hashtbl.add factories name o

let get_factory = Hashtbl.find factories

let iter_factories f =
  Hashtbl.iter (fun _ fac -> f fac) factories

let on_factory name f =
  let o =
    try get_factory name
    with Not_found -> failwith (Printf.sprintf "View factory %s not found" name)
  in
  f o

let factory_open_file ~factory topwin active_view ?attributes file =
  let f o = o#open_file topwin active_view ?attributes file in
  on_factory factory f

let factory_open_hidden ~factory ?attributes file =
  let f o =
    match o#open_hidden with None -> () | Some f -> f ?attributes file
  in
  on_factory factory f

let set_default_factory_name s =
  Ed_view_rc.default_view#set s

let factory_of_filename filename =
  try
    let res =
      List.find
        (fun (re,_) ->
          let re = Str.regexp re in
          Str.string_match re filename 0
        )
        Ed_view_rc.filename_view_patterns#get
    in
    snd res
  with
    Not_found ->
      Ed_view_rc.default_view#get
