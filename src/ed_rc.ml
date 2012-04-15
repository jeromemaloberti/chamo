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

(* $Id: cam_rc.ml 758 2011-01-13 07:53:27Z zoggy $ *)

module O = Config_file

let rc_dir = Ed_installation.rc_dir

(** {2 Core options} *)

let rc_core = Filename.concat rc_dir "core.ini"
let core_ini = new O.group
let save_core () = core_ini#write rc_core
let load_core () = core_ini#read rc_core

(** {2 Gui options} *)

let rc_gui = Filename.concat rc_dir "gui.ini"
let gui_ini = new O.group
let save_gui () = gui_ini#write rc_gui
let load_gui () = gui_ini#read rc_gui

(** {3 Keeping windows positions and sizes} *)

let value_to_geom v =
  match v with
    O.Raw.Tuple [O.Raw.String name ; O.Raw.Tuple [O.Raw.Int w ; O.Raw.Int h; O.Raw.Int x ; O.Raw.Int y ]] ->
      (name, (w,h,x,y))
  | _ ->
      prerr_endline "value_to_geom";
      raise Not_found

let geom_to_value (name, (w,h,x,y)) =
  O.Raw.Tuple
    [ O.Raw.String name ;
      O.Raw.Tuple [ O.Raw.Int w;
                    O.Raw.Int h;
                    O.Raw.Int x;
		    O.Raw.Int y;
		  ]
    ]

let geom_cp_wrapper =
  { O.to_raw = geom_to_value ;
    O.of_raw = value_to_geom ;
  }

let windows = new O.list_cp geom_cp_wrapper
    ~group: gui_ini ["windows"] [] ""

let set_window_info name (w,h,x,y) =
  let rec iter = function
      [] -> [name, (w,h,x,y)]
    | ((s,_) as t) :: q ->
	if name = s then
	  (name, (w,h,x,y)) :: q
	else
	  t :: (iter q)
  in
  windows#set (iter windows#get)

let get_window_info name = List.assoc name windows#get

let move_offset = ref None

let handle_window (win : GWindow.window) name =
  win#show ();
  let (x,y) = Gdk.Window.get_position win#misc#window in
  let (w,h) = Gdk.Drawable.get_size win#misc#window in

  ignore (win#event#connect#configure
	    (fun _ ->
	      let (x,y) = Gdk.Window.get_position win#misc#window in
	      let (w,h) = Gdk.Drawable.get_size win#misc#window in
	      set_window_info name (w,h,x,y);
	      save_gui ();
	      false
	    )
	 );
  let (width,height,x,y) =
    try get_window_info name
    with Not_found -> (w,h,x,y)
  in
  let (offset_x, offset_y) =
    match !move_offset with
      None ->
	let (x,y) = Ed_gtk_misc.get_wm_window_position_offset () in
	move_offset := Some (x,y);
	(x,y)
    | Some (x,y) -> (x,y)
  in
  win#move ~x: (x-offset_x) ~y: (y-offset_y);
  win#resize ~width ~height


(** {2 Utils} *)

let add_binding map binding action =
  map#set ((Configwin.string_to_key binding, action) :: map#get)
