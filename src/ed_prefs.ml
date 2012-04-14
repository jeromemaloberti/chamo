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

let style_scheme_box () =
  let hb = GPack.hbox () in
  let style_box = new Gtksv_utils.source_style_scheme_box () in
  hb#pack ~expand: true ~fill: true style_box#box;
  (Configwin.custom hb
   (fun () -> Gtksv_utils.store_style_scheme_selection style_box#scheme) true,
   (fun () -> Gtksv_utils.apply_source_style_scheme_to_registered_buffers
      (Gtksv_utils.source_style_scheme()))
  )


let source_view_props_box () =
  let hb = GPack.hbox () in
  let p = Gtksv_utils.read_sourceview_props () in
  let box = new Gtksv_utils.sourceview_props_box
      Gtksv_utils.apply_sourceview_props_to_registered
  in
  box#set_props (Some p);
  hb#pack ~expand: true ~fill: true box#box;
  (Configwin.custom hb
     (fun () ->
       match box#props with
         None -> ()
       | Some p -> Gtksv_utils.store_sourceview_props p
     ) true,
   (fun () ->
     Gtksv_utils.apply_sourceview_props_to_registered
       (Gtksv_utils.read_sourceview_props ()))
  )

let edit_preferences () =
  let (param_scheme, f_restore_scheme) = style_scheme_box () in
  let (param_svprops, f_restore_svprops) = source_view_props_box () in
  let sections =
    [
      Configwin.Section ("Source views", [param_svprops]) ;
      Configwin.Section ("Style scheme", [param_scheme]) ;
    ]
  in
  match Configwin.get Ed_messages.preferences sections with
    Configwin.Return_ok ->(* save_options () ;*)()
  | _ -> f_restore_scheme (); f_restore_svprops ()
