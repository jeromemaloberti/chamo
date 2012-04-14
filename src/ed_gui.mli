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

(** The main GUI module, defining windows and boxes used to split
   the windows.

   More elements can be showed in interface if needed.
*)

(** The icon to associate to a window if it could be built. *)
val window_pixbuf : GdkPixbuf.pixbuf option

class gui_window :
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int ->
  unit ->
  object
    val mutable active_view : Ed_view.gui_view option
    val mutable contents :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    val hbox_state : GPack.box
    val hboxmenus : GPack.box
    val item_log_window : GMenu.image_menu_item
    val item_reload : GMenu.image_menu_item
    val item_save : GMenu.image_menu_item
    val item_save_as : GMenu.image_menu_item
    val main : GWindow.window
    val menuEdit : GMenu.menu_item
    val toplevel : GWindow.window
    val vbox : GPack.box
    val viewmenubar : GMenu.menu_shell
    val wl_keystate : GEdit.entry
    method active_view : Ed_view.gui_view option
    method add_view : Ed_view.gui_view -> unit
    method add_view_in_active_view_container : Ed_view.gui_view -> unit
    method ask_open_file : unit
    method bind : name:string -> callback:(unit -> unit) -> unit
    method check_widgets : unit -> unit
    method close : unit
    method contains_view : Ed_view.gui_view -> bool
    method contents :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    method copy : unit
    method cut : unit
    method cycle_tab : bool -> unit
    method cycle_view : unit
    method destroy_active_view : unit
    method display_keyhit_state :
      after_handler:bool -> Okey.keyhit_state -> unit
    method error_message : string -> unit
    method get_active_view_container :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `Window of gui_window ] option
    method hbox_state : GPack.box
    method hboxmenus : GPack.box
    method height : int
    method item_log_window : GMenu.image_menu_item
    method item_reload : GMenu.image_menu_item
    method item_save : GMenu.image_menu_item
    method item_save_as : GMenu.image_menu_item
    method main : GWindow.window
    method menuEdit : GMenu.menu_item
    method menuitem11_menu : GMenu.menu
    method menuitem4_menu : GMenu.menu
    method menuitem6_menu : GMenu.menu
    method minibuffer : Ed_minibuffer.minibuffer
    method new_tab : unit
    method on_about : unit -> unit
    method on_close : unit -> unit
    method on_destroy_active_view : unit -> unit
    method on_minibuffer_active_change : bool -> unit
    method on_new_tab : unit -> unit
    method on_new_window : unit -> unit
    method on_open_file : unit -> unit
    method on_split_active_view : [ `HORIZONTAL | `VERTICAL ] -> unit -> unit
    method on_store_layout : unit -> unit
    method private on_view_destroy : unit -> unit
    method open_file : ?attributes:(string * string) list -> string -> unit
    method paste : unit
    method print_key_bindings : unit
    method reload_active_view : unit
    method reparent : GObj.widget -> unit
    method save_active_view : unit
    method save_active_view_as : unit
    method set_action_message : string -> unit
    method set_active_view : Ed_view.gui_view option -> unit
    method set_contents :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> unit
    method set_state_message : string -> unit
    method set_title : string -> unit
    method set_view_interface : Ed_view.gui_view option -> unit
    method split_active_view : Gtk.Tags.orientation -> unit
    method toplevel : GWindow.window
    method update_menus : unit
    method vbox : GPack.box
    method viewmenubar : GMenu.menu_shell
    method widget_opt_of_contents_opt :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> GObj.widget option
    method width : int
    method window : GWindow.window
    method wl_keystate : GEdit.entry
    method x : int
    method xml : Glade.glade_xml Gtk.obj
    method y : int
  end
and gui_paned :
  Ed_view.topwin ->
  Gtk.Tags.orientation ->
  unit ->
  object
    val mutable child1 :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    val mutable child2 :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    val mutable label : string
    val mutable on_destroy :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> unit
    val mutable on_label_change : string -> unit
    method add_view : Ed_view.gui_view -> unit
    method child1 :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    method child2 :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option
    method find_view_container :
      Ed_view.gui_view ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `Window of gui_window ] option
    method grab_focus : unit
    method label : string
    method new_tab :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] -> unit
    method on_child_destroy :
      int ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> unit
    method on_child_label_change : unit
    method on_child_view_destroy : int -> unit
    method orientation : Gtk.Tags.orientation
    method paned : GPack.paned
    method position : int
    method set_children_views : Ed_view.gui_view -> Ed_view.gui_view -> unit
    method set_label : string -> unit
    method set_on_destroy :
      ([ `Notebook of gui_notebook
       | `Paned of gui_paned
       | `View of Ed_view.gui_view ] option -> unit) ->
      unit
    method set_on_label_change : (string -> unit) -> unit
    method set_one_child :
      int ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] -> unit
    method set_position : int -> unit
    method split_active_view : Gtk.Tags.orientation -> unit
  end
and gui_notebook :
  Ed_view.topwin ->
  unit ->
  object
    val mutable label : string
    val mutable on_destroy :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> unit
    val mutable on_label_change : string -> unit
    val mutable tabs :
      (GMisc.label *
       [ `Notebook of gui_notebook
       | `Paned of gui_paned
       | `View of Ed_view.gui_view ])
      list
    method add_tab :
      int option ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] -> unit
    method add_view : Ed_view.gui_view -> unit
    method cycle_tab : bool -> unit
    method destroy : unit
    method find_view_container :
      Ed_view.gui_view ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `Window of gui_window ] option
    method goto_page : int -> unit
    method grab_focus : unit
    method label : string
    method notebook : GPack.notebook
    method on_switch_page : int -> unit
    method on_tab_destroy :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] ->
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] option -> unit
    method on_view_destroy : Ed_view.gui_view -> unit -> unit
    method set_label : string -> unit
    method set_on_destroy :
      ([ `Notebook of gui_notebook
       | `Paned of gui_paned
       | `View of Ed_view.gui_view ] option -> unit) ->
      unit
    method set_on_label_change : (string -> unit) -> unit
    method set_tab_label :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] -> string -> unit
    method split_active_view : Gtk.Tags.orientation -> unit
    method tab_of_contents :
      [ `Notebook of gui_notebook
      | `Paned of gui_paned
      | `View of Ed_view.gui_view ] -> int option
    method tabs :
      (GMisc.label *
       [ `Notebook of gui_notebook
       | `Paned of gui_paned
       | `View of Ed_view.gui_view ])
      list
  end

(** This function inits a view with the given window. *)
val init_view : Ed_view.topwin -> Ed_view.gui_view -> unit

type gui_windows = gui_window list
val gui_windows : gui_windows ref
val active_window : gui_window option ref

(** The default function does nothing. the referenced function
     is called when the last gui window is dsetroyed. *)
val on_last_window_close : (unit -> unit) ref

val create_window :
  ?x:int -> ?y:int -> ?width:int -> ?height:int -> unit -> gui_window

(** Code of the "in_new_window" command which creates a new window
    and use it as active window when evaluating the command represented
    by the given arguments: first argument is the command name, following
    ones are the arguments of the command to launch. *)
val in_new_window : string array -> unit

(** This function displays the traditionnal "about" window. *)
val show_about_dialog : unit -> unit

(** This function and {!on_active_window_args} can be used to apply
     a function to the active window, if any.
     They are useful when creating commands to register. *)
val on_active_window : (gui_window -> unit) -> unit -> unit

val on_active_window_args :
  (gui_window -> string array -> unit) -> string array -> unit

(** The history associated to the "prompt_command" command. *)
val prompt_command_history : Ed_minibuffer.minibuffer_history

(** This function uses the active window's minibuffer to prompt
     the user for a command. *)
val prompt_command : gui_window -> unit

