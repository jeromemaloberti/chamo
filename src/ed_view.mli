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

(** Views *)

class type gui_view =
  object
    method box : GObj.widget
    method close : unit
    method destroy : unit
    method dup : topwin -> gui_view option

    method reload : (unit -> unit) option

    method kind : string
    method filename : string
    method attributes : (string * string) list

    method save : (unit -> unit) option
    method save_as : (unit -> unit) option

    method paste : (unit -> unit) option
    method copy : (unit -> unit) option
    method cut : (unit -> unit) option

    method set_on_destroy : (unit -> unit) -> unit

    method grab_focus : unit
    method set_on_focus_in : (unit -> unit) -> unit

    method label : string

    (** The label given to the function should be in UTF-8. *)
    method set_on_label_change : (string -> unit) -> unit

    method key_bindings : (Okey.keyhit_state * string) list

    (** The menus to add when this view is activated. The label
       should already be in UTF-8. *)
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

class virtual dyn_label :
  object
    val mutable label : string
    val mutable on_label_change : string -> unit
    method label : string
    method set_label : string -> unit
    method set_on_label_change : (string -> unit) -> unit
  end
class virtual dyn_destroyable :
  (unit -> unit) ->
  object
    val mutable on_destroy : unit -> unit
    method destroy : unit
    method set_on_destroy : (unit -> unit) -> unit
  end

class type view_factory =
  object
    method name : string
    method open_file :
      topwin -> gui_view option ->
        ?attributes:(string*string) list ->
          string -> [ `New_view of gui_view | `Use_view of gui_view ]
    method open_hidden : (?attributes:(string*string) list -> string -> unit) option
    method on_start : unit
    method on_exit : unit
  end

val register_view_factory : string -> view_factory -> unit
val get_factory : string -> view_factory
val iter_factories : (view_factory -> unit) -> unit
val on_factory : string -> (view_factory -> 'a) -> 'a
val factory_open_file :
  factory:string ->
    topwin ->
    gui_view option ->
    ?attributes:(string*string) list ->
    string -> [ `New_view of gui_view | `Use_view of gui_view ]
val factory_open_hidden :
  factory:string -> ?attributes:(string*string) list -> string -> unit

(** {2 Associations between filenames and factories} *)

(** Set the name of the default factory to use to open files. *)
val set_default_factory_name : string -> unit

(** Return the name of the view factory to use to open the given filename.
   @raise Failure if no factory is found and no default factory name is set.
*)
val factory_of_filename : string -> string
