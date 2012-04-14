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

let _ = Tdl_gui_rc.read ()
let _ = Tdl_gui_rc.write ()
let _ = Ed_tdl_rc.read ()
let _ = Ed_tdl_rc.write ()

class view (topwin : Ed_view.topwin) f_set_active_view f_on_destroy (file : string) =
  let vbox = GPack.vbox () in
  let hbox_state = GPack.hbox () in
  let add_state () = GMisc.label ~packing: hbox_state#pack ~xpad: 5 () in
  let wl_modified = add_state () in
  let wl_file = add_state () in
  let v = new Tdl_gui.file_view file in
  let ref_on_destroy = ref (fun () -> ()) in
  object(self)
    inherit Ed_view.dyn_label
    inherit Ed_view.dyn_destroyable (fun () -> !ref_on_destroy () ; vbox#destroy())

    method vbox = vbox
    method box = vbox#coerce

    method save : (unit -> unit) option = Some (fun () -> v#save)
    method save_as : (unit -> unit) option = None

    method close = ()

    method reload = Some (fun () -> v#reload)

    method paste : (unit -> unit) option = Some (fun () -> v#paste)
    method copy : (unit -> unit) option = Some (fun () -> v#copy)
    method cut : (unit -> unit) option = Some (fun () -> v#cut)

    method kind = Ed_tdl_rc.factory_name
    method filename = file
    method attributes : (string * string) list = []

    method set_on_focus_in (f : unit -> unit) =
      ignore(v#tree_view#event#connect#focus_in
               (fun _ -> f_set_active_view self; f (); false))
    method grab_focus = v#tree_view#misc#grab_focus ()

    method dup : Ed_view.topwin -> Ed_view.gui_view option = fun _ -> None

    method key_bindings : (Okey.keyhit_state * string) list =
      Ed_tdl_rc.key_bindings#get

    method menus : (string * GToolbox.menu_entry list) list =
      let com com () =
        Ed_commands.eval_command (Printf.sprintf "%s_%s" Ed_tdl_rc.factory_name com)
      in
      [
        "Todo",
        [
          `I ("Add item", com "add_item") ;
          `I ("Add group", com "add_group") ;
          `I ("Edit selected", com "edit_selected") ;
        ]
      ]

    method display_state =
      self#display_modified;
      self#display_filename

    method display_filename =
      wl_file#set_text (Glib.Convert.filename_to_utf8 (Filename.basename file))

    method display_modified =
      wl_modified#set_text (if v#modified then "*" else "")

    method my_set_label =
      let f = Glib.Convert.filename_to_utf8 (Filename.basename file) in
      self#set_label (Printf.sprintf "%s%s" f
                        (if v#modified then " *" else ""))

    method add_item = v#add_item
    method add_group = v#add_group
    method edit_selected = v#edit_selected

    initializer
      vbox#pack ~expand: true ~fill: true v#box#coerce;
      vbox#pack ~expand: false ~fill: true hbox_state#coerce;
      self#display_state;
      self#my_set_label;
      v#set_on_modified_changed (fun () -> self#display_modified; self#my_set_label);
      ref_on_destroy := (fun () -> f_on_destroy self);
  end

let views = ref ([] : view list)
let active_view = ref None

let set_active_view o =
  if List.exists (fun v -> Oo.id v = Oo.id o) !views then
    active_view := Some o

let on_view_destroy v =
  views := List.filter (fun v2 -> Oo.id v <> Oo.id v2) !views;
  match !active_view with
    Some v2 when Oo.id v = Oo.id v2 ->
      active_view := None
  | Some _
  | None -> ()

let create_view topwin file =
  let v = new view topwin set_active_view on_view_destroy file in
  views := v :: !views;
  v

let open_file topwin active_view ?attributes filename =
  try
    let v = List.find (fun v -> v#filename = filename) !views in
    `Use_view (v :> Ed_view.gui_view)
  with Not_found ->
    `New_view (create_view topwin filename :> Ed_view.gui_view)

(** {2 Factory} *)

class factory : Ed_view.view_factory =
  object
    method name = Ed_tdl_rc.factory_name
    method open_file = open_file
    method open_hidden = None
    method on_start = ()
    method on_exit = ()
  end

let _ = Ed_view.register_view_factory Ed_tdl_rc.factory_name (new factory)

(** {2 Commands} *)

let on_active_view f () =
  match !active_view with
    None -> prerr_endline "no active tdl view!"
  | Some o -> f o

let unit_coms_on_active_view =
  [
    "add_item",  (fun v -> v#add_item) ;
    "add_group",  (fun v -> v#add_group) ;
    "edit_selected",  (fun v -> v#edit_selected) ;
  ]

let _ = List.iter
    (fun (name, f) ->
      Ed_commands.register
        (Ed_commands.unit_com (Printf.sprintf "%s_%s" Ed_tdl_rc.factory_name name) (on_active_view f)))
    unit_coms_on_active_view
