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

(** Storing and loading layout of windows. *)

(** The file used to store the layout. *)
val layout_file : string ref

(** Representing a view. *)
type layout_view = {
  lv_kind : string;
  lv_file : string;
  lv_atts : (string * string) list;
}

(** Representing windows and boxes used to split windows. *)
type layout_contents =
    [ `Notebook of layout_notebook
    | `Paned of layout_paned
    | `View of layout_view ]
and layout_paned = {
  lp_orientation : [ `HORIZONTAL | `VERTICAL ];
  lp_position : int;
  lp_children : layout_contents * layout_contents;
}
and layout_notebook = { ln_tabs : layout_contents list; }
type layout_window = {
  lw_x : int;
  lw_y : int;
  lw_w : int;
  lw_h : int;
  lw_contents : layout_contents option;
}

type layout = layout_window list

(** [store_layout file layout] stores the given [layout] in the given [file]. *)
val store_layout : string -> layout -> unit

(** [load_layout file] loads the layout description from the given [file]. *)
val load_layout : string -> layout_window list

(** Create a layout description from the given list of windows. *)
val layout_of_windows : Ed_gui.gui_windows -> layout

(** Create the windows and their contents from a given layout description. *)
val create_windows_of_layout : layout -> unit
