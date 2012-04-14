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

open Odoc_info
open Module
open Class
open Value
open Type
open Exception

let _ = Ed_odoc_rc.read ()
let _ = Ed_odoc_rc.write ()

type loc_info = Odoc_info.location
type kind = [
    `Module
  | `Module_type
  | `Class
  | `Class_type
  | `Value of bool
  | `Method
  | `Att
  | `Type
  | `Exception
]
type data = string * kind

type tree =
  {
    roots : data list;
    get_loc : data -> loc_info ;
    get_subs : data -> data list ;
  }

let fill_data modules =
  let data : (data, data list) Hashtbl.t = Hashtbl.create 1223 in
  let data_locs : (data, loc_info) Hashtbl.t = Hashtbl.create 1223 in
  let add_loc = Hashtbl.add data_locs in
  let rec iter_mod_eles acc father = function
    [] -> List.rev acc
  | h :: q ->
      let d =
        match h with
          Element_module m ->
            let d = (m.m_name, `Module) in
            add_loc d m.m_loc;
            let subs = iter_mod_eles [] d (module_elements m) in
            Hashtbl.add data d subs;
            Some d
        | Element_module_type m ->
            let d = (m.mt_name, `Module_type) in
            add_loc d m.mt_loc;
            let subs = iter_mod_eles [] d (module_type_elements m) in
            Hashtbl.add data d subs;
            Some d
        | Element_included_module _ ->
            None
        | Element_class c ->
            let d = (c.cl_name, `Class) in
            add_loc d c.cl_loc;
            let subs = iter_class_eles [] d (class_elements c) in
            Hashtbl.add data d subs;
            Some d
        | Element_class_type c ->
            let d = (c.clt_name, `Class_type) in
            add_loc d c.clt_loc;
            let subs = iter_class_eles [] d (class_type_elements c) in
            Hashtbl.add data d subs;
            Some d
        | Element_value v ->
            let d = (v.val_name, `Value (v.val_parameters <> [])) in
            add_loc d v.val_loc ;
            Some d
        | Element_exception e ->
            let d = (e.ex_name, `Exception) in
            add_loc d e.ex_loc ;
            Some d
        | Element_type t ->
            let d = (t.ty_name, `Type) in
            add_loc d t.ty_loc ;
            Some d
        | Element_module_comment _ ->
            None
      in
      match d with
        None -> iter_mod_eles acc father q
      | Some d -> iter_mod_eles (d::acc) father q
  and iter_class_eles acc father = function
        [] -> List.rev acc
    | h :: q ->
        let d =
          match h with
            Class_attribute a ->
              let d = (a.att_value.val_name, `Att) in
              add_loc d a.att_value.val_loc ;
              Some d
          | Class_method m ->
              let d = (m.met_value.val_name, `Method) in
              add_loc d m.met_value.val_loc ;
              Some d
          | Class_comment _ -> None
        in
        match d with
          None -> iter_class_eles acc father q
        | Some d -> iter_class_eles (d::acc) father q
  in
  let add_root m =
    let d = (m.m_name, `Module) in
    add_loc d m.m_loc;
    let subs = iter_mod_eles [] d (module_elements m) in
    Hashtbl.add data d subs;
    d
  in
  let roots = List.map add_root modules in
  let get_subs = function
    (_, `Value _)
  | (_, `Type)
  | (_, `Exception)
  | (_, `Att)
  | (_, `Method) -> []
  | d -> Hashtbl.find data d
  in
  { roots = roots ;
    get_loc = Hashtbl.find data_locs ;
    get_subs = get_subs;
  }

let pix_size = 16
let create_pix =
  let f file =
    GdkPixbuf.from_file_at_size file ~width: pix_size ~height: pix_size
  in
  fun cp ->
    try f cp#get
    with e -> prerr_endline (Printexc.to_string e); f cp#get_default

class view (topwin : Ed_view.topwin) (file : string) =
  let pix_file = create_pix Ed_odoc_rc.pix_file in
  let pix_comp = create_pix Ed_odoc_rc.pix_comp in
  let pix_other = create_pix Ed_odoc_rc.pix_other in
  let pix_type = create_pix Ed_odoc_rc.pix_type in
  let pix_fun = create_pix Ed_odoc_rc.pix_fun in
  let pix_val = create_pix Ed_odoc_rc.pix_value in
  let data = ref (fill_data (Odoc_info.load_modules file)) in
  let vbox = GPack.vbox () in
  let f_children d = !data.get_subs d in
  let f_roots () = !data.roots in
  let f_contents ele =
    let (pix, s) =
      match ele with
        (name,`Module) ->
          let pix =
            if Name.simple name = name
            then pix_file else pix_comp
          in
          (pix, name)
      | (name,`Module_type) -> (pix_type, name)
      | (name,`Class) -> (pix_comp, name)
      | (name,`Class_type) -> (pix_type, name)
      | (name,`Value has_params) ->
          let pix = if has_params then pix_fun else pix_val in
          (pix, name)
      | (name,`Method) -> (pix_fun, name)
      | (name,`Att) -> (pix_val, name)
      | (name,`Exception) -> (pix_other, name)
      | (name,`Type) -> (pix_type, name)
    in
    [`Pixmap (Some pix); `String (Ed_misc.to_utf8 (Odoc_name.simple s))]
  in
  let tree =
    let open_file file char =
      let com = Printf.sprintf "open_file %s %d" (Filename.quote file) char in
      Ed_commands.eval_command com
    in
    let open_location loc =
      match loc.loc_impl with
      | Some (file, char) -> open_file file char
      | None ->
          match loc.loc_inter with
            None -> ()
          | Some (file, char) -> open_file file char
    in
    let location_of_data d = !data.get_loc d in
    object(tree)
      inherit [data] Gmytree.tree_edit ~f_roots ~f_children ~f_contents [`Pixmap None ; `String ""]
      method on_double_click data =
        open_location (location_of_data data)
      method menu =
        match selection with
          None -> []
        | Some data ->
            let loc = location_of_data data in
            (
             match loc.loc_impl with
               None -> []
             | Some (file,n) ->
                 [`I ("Implementation", (fun () -> open_file file n))]
            ) @
            (
             match loc.loc_inter with
               None -> []
             | Some (file,n) ->
                 [`I ("Interface", (fun () -> open_file file n))]
            )
    end
  in
  object(self)
    inherit Ed_view.dyn_label
    inherit Ed_view.dyn_destroyable (fun () -> vbox#destroy())

    method vbox = vbox
    method box = vbox#coerce

    method save : (unit -> unit) option = None
    method save_as : (unit -> unit) option = None

    method close = ()

    method reload =
      let f () =
        data := fill_data (Odoc_info.load_modules file);
        tree#update
      in
      Some f

    method paste : (unit -> unit) option = None
    method copy : (unit -> unit) option = None
    method cut : (unit -> unit) option = None

    method kind = Ed_odoc_rc.factory_name
    method filename = file
    method attributes : (string * string) list = []

    method set_on_focus_in (f : unit -> unit) =
      ignore(tree#view#event#connect#focus_in (fun _ -> f (); false))
    method grab_focus = tree#view#misc#grab_focus ()

    method dup : Ed_view.topwin -> Ed_view.gui_view option = fun _ -> None

    method key_bindings : (Okey.keyhit_state * string) list =
      Ed_odoc_rc.key_bindings#get

    method menus : (string * GToolbox.menu_entry list) list = []

    initializer
      vbox#pack ~expand: true ~fill: true tree#box#coerce;
      self#set_label (Glib.Convert.filename_to_utf8 (Filename.basename file))

  end

let create_view topwin file =
  new view topwin file

let open_file topwin active_view ?attributes filename =
  `New_view (create_view topwin filename :> Ed_view.gui_view)

(** {2 Factory} *)

class factory : Ed_view.view_factory =
  object
    method name = Ed_odoc_rc.factory_name
    method open_file = open_file
    method open_hidden = None
    method on_start = ()
    method on_exit = ()
  end


let _ = Ed_view.register_view_factory Ed_odoc_rc.factory_name (new factory)
