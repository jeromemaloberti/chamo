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

(* $Id: cam_constant.ml 758 2011-01-13 07:53:27Z zoggy $ *)

let com_history_size = 50
let log_max_size = 3000

let com_quit = "quit"
let com_log_window = "log_window"
let com_new_project = "new_project"

let com_new_file = "new_file"

let com_iconify = "iconify"
let com_deiconify = "deiconify"

(** Update the files view. *)
let com_update_files_view = "update_files_view"

(** Refresh view *)
let com_refresh_view = "refresh_view"

(** Close view *)
let com_close_view = "close_view"

(** Edit the selected files with their associated editor(s). @command edit *)
let com_edit = "edit"

(** Edit the selected files with an editor chossen among the vailable ones.
   @command edit_with *)
let com_edit_with = "edit_with"

(** Quit Cameleon. @command quit *)
let com_quit = "quit"

(** Open menu configuration box. *)
let com_configure_menus = "configure_menus"

(** Open doc sources configuration box. *)
let com_configure_doc_sources = "configure_doc_sources"

(** Open button bar configuration box. *)
let com_configure_bbar = "configure_bbar"

(** Open plugins configuration box. *)
let com_configure_plugins = "configure_plugins"

(** Open file types rules configuration box. *)
let com_configure_ft_rules = "configure_file_types_rules"

(** Open file types handlers configuration box. *)
let com_configure_ft_handlers = "configure_file_types_handlers"

(** Open common keyboard shortcuts configuration box. *)
let com_configure_common_keyboard_shortcuts =
  "configure_common_keyboard_shortcuts"

(** Open docbrowser keyboard shortcuts configuration box. *)
let com_configure_docbrowser_keyboard_shortcuts =
  "configure_docbrowser_keyboard_shortcuts"

(** Make the user select a loaded plug-in and reload it.
   @command reload_plugin *)
let com_reload_plugin = "reload_plugin"

(** Display or update the box with the list of top modules whose documentation
   is available.@command display_modules_box *)
let com_display_doc_box = "display_modules_box"

(** Display the "about..." box. @command about_box *)
let com_about_box = "about_box"

(** Display the list of available internal commands, with
   their description. @command list_commands *)
let com_list_commands = "list_commands"

(** Prompt the used for a command using the cameleon minibuffer. *)
let com_prompt_command = "cam_prompt_command"

(** Command called on exit. Functions can be registered after this
     command so that they are executed at exit. *)
let com_on_exit = "on_exit";;
