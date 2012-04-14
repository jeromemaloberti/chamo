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

(** Multi-contents clipboards in chamo. *)

let _ = Ed_multiclip_rc.read ()
let _ = Ed_multiclip_rc.write ()

let factory_name = Ed_multiclip_rc.factory_name

class view (topwin : Ed_view.topwin) (file : string)
  f_on_destroy f_set_active =
  let st_clip = Multiclip.create_storable_multiclip file in
  let clipbox = new Multiclip_gui.multiclip_box ~auto_read_write: true st_clip in
  let vbox = GPack.vbox () in
  let wlabel = GMisc.label ~xalign: 0.0 ~xpad: 5 () in
  let ref_on_destroy = ref (fun () -> ()) in
  object(self)
    inherit Ed_view.dyn_label
    inherit Ed_view.dyn_destroyable
      (fun () -> !ref_on_destroy(); vbox#destroy())

    method vbox = vbox
    method box = vbox#coerce

    method save : (unit -> unit) option = None
    method save_as : (unit -> unit) option = None

    method close = ()

    method reload = Some (fun () -> clipbox#reload)

    method paste : (unit -> unit) option = None
    method copy : (unit -> unit) option = None
    method cut : (unit -> unit) option = None

    method kind = factory_name
    method filename = file
    method attributes : (string * string) list = []

    method set_on_focus_in (f : unit -> unit) =
      ignore
        (
         clipbox#view#event#connect#focus_in
         (fun _ -> f_set_active self; f (); false)
        )
    method grab_focus = clipbox#view#misc#grab_focus ()

    method dup : Ed_view.topwin -> Ed_view.gui_view option = fun _ -> None

    method key_bindings : (Okey.keyhit_state * string) list =
      Ed_multiclip_rc.key_bindings#get

    method menus : (string * GToolbox.menu_entry list) list = []

    method minibuffer = topwin#minibuffer
    method storable_multiclip = st_clip
    method clipbox = clipbox
    method add = clipbox#add
    method remove = clipbox#remove

    initializer
      vbox#pack ~expand: true ~fill: true clipbox#box;
      vbox#pack ~expand: false ~fill: true wlabel#coerce;
      let label = Glib.Convert.filename_to_utf8 (Filename.basename file) in
      self#set_label label;
      wlabel#set_text label;
      ref_on_destroy := (fun () -> f_on_destroy self);

  end
;;

let last_active_multiclip = ref (None : view option);;

let on_multiclip_destroy o =
  match !last_active_multiclip with
  | Some o2 when Oo.id o = Oo.id o2 -> last_active_multiclip := None
  | Some _ | None -> ()
;;

let create_view topwin file =
  new view topwin file
    on_multiclip_destroy
    (fun o -> last_active_multiclip := Some o)
;;

let open_file topwin active_view ?attributes filename =
  `New_view (create_view topwin filename :> Ed_view.gui_view)
;;

(** {2 Factory} *)

class factory : Ed_view.view_factory =
  object
    method name = factory_name
    method open_file = open_file
    method open_hidden = None
    method on_start = ()
    method on_exit = ()
  end


let _ = Ed_view.register_view_factory factory_name (new factory)

(** {2 Commands} *)

let copy_history = Ed_minibuffer.history ();;

let register_com ~prefix name args ?more f =
  let name = Printf.sprintf "%s_%s" prefix name in
  let f args =
    match !last_active_multiclip with
      None -> ()
    | Some mc -> f mc args
  in
  let c = {
      Ed_commands.com_name = name ;
      com_args = args ;
      com_more_args = more ;
      com_f = f ;
    }
  in
  Ed_commands.register c
;;

let rec multiclip_copy (mc : view) args =
  if Array.length args > 0 then
    mc#add args.(0)
  else
    begin
      let f = function
        "" -> ()
      | s ->
          Ed_commands.launch_command
            (Printf.sprintf "%s_copy" factory_name) [| s |]
      in
      let title = "Add to last active multiclip" in
      Ed_misc.input_string
        ~history: copy_history
        mc#minibuffer
        ~title
        ""
        f
    end
;;

let multiclip_copy_selection _ args =
  let text =
    match GMain.selection#text with
      None -> GMain.clipboard#text
    | x -> x
  in
  match text with
    None -> ()
  | Some text ->
      let com = Printf.sprintf "%s_copy" factory_name in
      Ed_commands.launch_command com [| text |]
;;

let multiclip_remove mc args =
  if Array.length args > 0 then
    Array.iter mc#remove args
  else
    begin
      let title = "Remove an entry from last active multiclip" in
      let choices =
        List.map fst
          (Multiclip.elements (Multiclip.storable_get_multiclip mc#storable_multiclip))
      in
      let f s = Ed_commands.launch_command
        (Printf.sprintf "%s_remove" factory_name) [| s |]
      in
      Ed_misc.select_string
        mc#minibuffer
        ~title
        ~choices
        ""
        f
    end
;;

let multiclip_sourceview_paste mc args =
  match mc#clipbox#selection with
    [] -> ()
  | (_,text) :: _ ->
      Ed_commands.launch_command "sourceview_paste" [| text |]
;;

let coms =
  [
    "copy", [| "text" |], None, multiclip_copy ;
    "copy_selection", [| |], None, multiclip_copy_selection ;
    "remove", [| |], Some "list of abstracts", multiclip_remove ;
    "sourceview_paste", [| |], None, multiclip_sourceview_paste ;
  ];;

let _ = List.iter
  (fun (name, args, more, f) ->
     register_com ~prefix: factory_name name args ?more f)
    coms