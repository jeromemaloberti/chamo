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

module CF = Config_file

let factory_name = "sourceview"
let rc_file = Ed_config.rc_file factory_name

let mode_rc_file mode_name =
  Ed_config.rc_file (Printf.sprintf "%s.mode.%s" factory_name mode_name)
let local_mode_rc_file mode_name =
  Ed_config.local_dir_rc_file (Printf.sprintf "%s.mode.%s" factory_name mode_name)

let bookmarks_rc_file =
  Ed_config.local_dir_rc_file (Printf.sprintf "%s.bookmarks" factory_name)

let string_of_wrap_mode = function
  `NONE -> "none"
| `CHAR -> "char"
| `WORD -> "word"

let wrap_mode_of_string = function
  "char" -> `CHAR
| "word" -> `WORD
| _ -> `NONE

let default_word_re = "[a-zA-Z0-9]+"

let group = new CF.group

let default_key_bindings  = [
    [[`CONTROL], GdkKeysyms._x ; [], GdkKeysyms._b], factory_name^"_switch_buffer" ;
    [[`CONTROL], GdkKeysyms._x ; [], GdkKeysyms._k], factory_name^"_destroy_buffer" ;
    [[`MOD1;`SHIFT], GdkKeysyms._percent], factory_name^"_query_replace" ;
    [[`CONTROL;`MOD1;`SHIFT], GdkKeysyms._percent], factory_name^"_query_replace_re" ;
    [[`CONTROL], GdkKeysyms._s], factory_name^"_search" ;
    [[`CONTROL], GdkKeysyms._r], factory_name^"_search_backward" ;
    [[`CONTROL;`MOD1], GdkKeysyms._s], factory_name^"_search_re" ;
    [[`CONTROL;`MOD1], GdkKeysyms._r], factory_name^"_search_re_backward" ;
    [[],GdkKeysyms._Escape; [`SHIFT], GdkKeysyms._slash], factory_name^"_expand";
    [[`SHIFT],GdkKeysyms._Escape; [`SHIFT], GdkKeysyms._slash], factory_name^"_expand";
    [[`CONTROL], GdkKeysyms._z], factory_name^"_undo" ;
    [[`CONTROL;`MOD1], GdkKeysyms._z], factory_name^"_redo" ;
    [[`CONTROL], GdkKeysyms._a], factory_name^"_beginning_of_line" ;
    [[`CONTROL], GdkKeysyms._e], factory_name^"_end_of_line" ;
    [[`CONTROL], GdkKeysyms._k], factory_name^"_kill_line" ;
    [[`CONTROL], GdkKeysyms._Delete], factory_name^"_kill_word" ;
    [[`CONTROL], GdkKeysyms._BackSpace], factory_name^"_backward_kill_word" ;
    [[`CONTROL], GdkKeysyms._Left], factory_name^"_backward_word" ;
    [[`CONTROL], GdkKeysyms._Right], factory_name^"_forward_word" ;
    [[], GdkKeysyms._Up], factory_name^"_backward_line" ;
    [[], GdkKeysyms._Down], factory_name^"_forward_line" ;
    [[], GdkKeysyms._Left], factory_name^"_backward_char" ;
    [[], GdkKeysyms._Right], factory_name^"_forward_char" ;
    [[], GdkKeysyms._Escape ; [], GdkKeysyms._y], factory_name^"_yank_choose" ;
    [[`MOD1], GdkKeysyms._g], factory_name^"_goto_line" ;
    [[`CONTROL], GdkKeysyms._d], factory_name^"_delete_char" ;
    [[], GdkKeysyms._BackSpace], factory_name^"_backward_delete_char" ;
    [[`CONTROL], GdkKeysyms._t], factory_name^"_transpose_chars" ;
    [[`CONTROL], GdkKeysyms._x; [`CONTROL], GdkKeysyms._l], factory_name^"_transpose_lines" ;
    [[], GdkKeysyms._Escape; [], GdkKeysyms._t], factory_name^"_transpose_words" ;
    [[`CONTROL], GdkKeysyms._x ; [`CONTROL], GdkKeysyms._b], factory_name^"_set_bookmark" ;
    [[`CONTROL], GdkKeysyms._l ; [], GdkKeysyms._p], factory_name^"_push_location" ;
    [[`CONTROL], GdkKeysyms._l ; [], GdkKeysyms._o], factory_name^"_pop_location" ;
    [[`CONTROL], GdkKeysyms._l ; [], GdkKeysyms._f], factory_name^"_forward_location" ;
    [[`CONTROL], GdkKeysyms._m ; [`CONTROL], GdkKeysyms._v], "multiclip_"^factory_name^"_paste" ;

  ]


let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"

let default_filename_language_patterns =
  [
    ".*\\.ml[iyl]?$", "text/x-ocaml" ;
    ".*\\.c$",        "text/x-c" ;
    ".*\\.cpp$",      "text/x-cpp" ;
    ".*\\.c#$",       "text/x-csharp" ;
    ".*\\.tex$",      "text/x-tex" ;
    ".*\\.htm[l]?$",  "text/html" ;
    ".*\\.xml$",      "text/xml" ;
    ".*Makefile\\(\\.in\\)?$", "text/x-makefile" ;
    ".*ChangeLog$",   "text/x-changelog" ;
    ".*\\.sh\\(\\.in\\)?$",   "text/x-shellscript" ;
    ".*\\configure\\(\\.in\\)?$",   "text/x-shellscript" ;
  ]

let filename_language_patterns =
  new CF.list_cp ~group
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers)
    ["language_mime_from_filename_patterns"]
    default_filename_language_patterns
    ("Associations between regular expressions on filename and the "^
     "mime type used to get the language highlight to use in buffer")

let default_filename_mode_patterns =
  [
    ".*\\.ml[iyl]?$", "ocaml" ;
    ".*Makefile\\(\\.in\\)?$", "makefile" ;
    ".*ChangeLog$", "changelog";
  ]

let filename_mode_patterns =
  new CF.list_cp ~group
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers)
    ["mode_from_filename_patterns"]
    default_filename_mode_patterns
    ("Associations between regular expressions on filename and the "^
     "name of the sourceview mode to use in buffer")

let default_max_undo_levels = 200
let max_undo_levels = new CF.int_cp ~group
    ["max_undo_levels"]
    default_max_undo_levels
    "Maximum undo levels in each sourceview buffer"

let default_default_wrap_mode = `CHAR
let wrap_mode_wrappers =
  { Config_file.to_raw = (fun m -> Config_file.Raw.String (string_of_wrap_mode m)) ;
    of_raw =
      (function
           Config_file.Raw.String s -> wrap_mode_of_string s
       | _ -> default_default_wrap_mode) ;
  }
let default_wrap_mode = new
  Config_file.cp_custom_type wrap_mode_wrappers ~group
    ["default_wrap_mode"] default_default_wrap_mode
    "Default wrap mode to use when creating a sourceview (char, word, or none)"

let read () = group#read rc_file
let write () = group#write rc_file

let (add_sourceview_key_binding, add_sourceview_key_binding_string) =
  Ed_gui_rc.create_add_binding_commands key_bindings factory_name

let create_add_sourceview_mode_binding_commands option mode_name =
  Ed_gui_rc.create_add_binding_commands option
    (Printf.sprintf "%s_mode_%s" factory_name mode_name)