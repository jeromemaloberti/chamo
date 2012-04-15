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

(** *)


let treat_gtk_events () =
  while Glib.Main.pending () do
    ignore (Glib.Main.iteration false)
  done

let get_wm_window_position_offset () =
  let win = GWindow.window ~width: 0 ~height: 0 () in
  win#show ();
  let (x,y) = Gdk.Window.get_position win#misc#window in
  win#move ~x ~y;
  treat_gtk_events ();
  let (x2,y2) = Gdk.Window.get_position win#misc#window in
  win#destroy ();
  (x2 - x, y2 - y)

