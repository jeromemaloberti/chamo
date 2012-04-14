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

(** The tdl view, to edit todo lists in the format handled by the {!Tdl} library.*)

class view :
  Ed_view.topwin ->
    ('a -> 'b) ->
    ('a -> unit) ->
    string ->
    object ('a)
      inherit Ed_view.dyn_label
      inherit Ed_view.dyn_destroyable
      method add_group : unit
      method add_item : unit
      method attributes : (string * string) list
      method box : GObj.widget
      method close : unit
      method copy : (unit -> unit) option
      method cut : (unit -> unit) option
      method display_filename : unit
      method display_modified : unit
      method display_state : unit
      method dup : Ed_view.topwin -> Ed_view.gui_view option
      method edit_selected : unit
      method filename : string
      method grab_focus : unit
      method key_bindings : (Okey.keyhit_state * string) list
      method kind : string
      method menus : (string * GToolbox.menu_entry list) list
      method my_set_label : unit
      method paste : (unit -> unit) option
      method reload : (unit -> unit) option
      method save : (unit -> unit) option
      method save_as : (unit -> unit) option
      method set_on_focus_in : (unit -> unit) -> unit
      method vbox : GPack.box
    end

val create_view : Ed_view.topwin -> string -> view
val open_file :
  Ed_view.topwin ->
    Ed_view.gui_view -> ?attributes:(string*string) list ->
    string ->
    [> `New_view of Ed_view.gui_view | `Use_view of Ed_view.gui_view ]

class factory : Ed_view.view_factory
