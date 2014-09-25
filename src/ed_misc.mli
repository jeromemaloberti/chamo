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

(** Misc functions. *)

val map_opt : ('a -> 'b) -> 'a option -> 'b option



(** Same as [int_of_string] but support strings beginning with '+'. *)
val my_int_of_string : string -> int

val add_shortcut :
  < connect : < destroy : callback:(unit -> unit) -> GtkSignal.id; .. >;
    event : GObj.event_ops; get_oid : int; .. > ->
  (string * ((unit -> bool) option * (unit -> unit))) list ->
  (Okey.modifier list * Gdk.keysym) * string -> unit


val select_in_list :
    ?ok:string -> ?cancel:string ->
      ?allow_empty: bool ->
	?value_in_list: bool ->
	  title:string ->
	  choices: string list ->
	    string -> string option

(** [remove_char s c] returns a copy of the given string [s] without character [c]. *)
val remove_char : string -> char -> string

(** [line_of_char file char] gets the line number (0-based)
   in a file from a character number. *)
val line_of_char : string -> int -> int

(** [char_of_line file line] returns the offset of the first character
     of the given [line] of the give [file]. Line number are zero-based,
     as the returned character number. *)
val char_of_line : string -> int -> int

(** [escape_menu_label string] returns a the string where all '_' have
   been escaped to be displayed correctly in Lablgtk menus.*)
val escape_menu_label : string -> string

(** [mod_date_of_file file] returns the Unix last modification date of the given file,
   or 0.0 if the date could not be obtained. *)
val mod_date_of_file : string -> float

val string_of_bool : bool -> string
val bool_of_string : string -> bool

(** [date_of_file file] returns the unix date of the last modification
     of the file, is the file can be accessed. *)
val date_of_file : string -> float option

(** [catch_print_exception f v] applies [f] to [v] and
     catch and print any raised exception to stderr. *)
val catch_print_exceptions : ('a -> unit) -> 'a -> unit

(** Encode the given string to UTF-8, using the default charset
  {!Ed_core_rc.default_encoding} or the given [coding].*)
val to_utf8 : ?coding:string -> string -> string

(** Decode the given string from UTF-8, using the default charset
  {!Ed_core_rc.default_encoding} or the given [coding].*)
val of_utf8 : ?coding:string -> string -> string

(** Return whether the two given filename identifies the same file
     (same device and inode).
     @raise Failure if information about a file cannot be accessed. *)
val same_files : string -> string -> bool

(** Same as {!same_files} but returns [false] instead of raising an exception
     if information about one file could not be retrieved. *)
val safe_same_files : string -> string -> bool

(** This function calls the "set_active_state_message" command with
     the given utf-8 string. *)
val set_active_state_message : string -> unit

(** This function calls the "set_active_action_message" command with
     the given utf-8 string. *)
val set_active_action_message : string -> unit

(** This function displays the given message with {!Ed_hooks.display_message}
    and calls the "set_active_action_message" command with the given utf-8 string. *)
val display_message : string -> unit

(** This function displays the given message with {!Ed_hooks.warning_message}
    and calls the "set_active_action_message" command with the given utf-8 string. *)
val warning_message : string -> unit

(** This function displays the given message with {!Ed_hooks.error_message}
    and calls the "set_active_action_message" command with the given utf-8 string. *)
val error_message : string -> unit

(** [fail_if_unix_error f x] applies [f] to [x] and catches only exception
     [Unix.Unix_error]. If such an exception is raised by [f], create
     a message string and raise [Failure message]. *)
val fail_if_unix_error : ('a -> 'b) -> 'a -> 'b

(** [is_prefix s1 s2] returns whether [s2] is a prefix of [s1]. *)
val is_prefix : string -> string -> bool

(** [dir_entries dir] return the list of entries in directory [dir].
     @param prefix can be used to filter only entries with the given
     prefix. *)
val dir_entries : ?prefix:string -> string -> string list

(** [max_common strings] return the longest string common to all
     the given strings or None if [strings] is the empty list. *)
val max_common : string list -> string option

(** {2 Input functions}

     The strings given in parameter to the following input functions
     must be utf-8 encoded; the strings passed to the given callback function
     is decoded from utf-8 before. *)

(** The minibuffer history used in function {!select_file}. *)
val select_file_history : Ed_minibuffer.minibuffer_history

(** [select_file mb ~title default_text f] makes the user select a file
     in the minibuffer and calls [f] with the selected file when
     user validates. *)
val select_file :
  Ed_minibuffer.minibuffer ->
  title:string -> string -> (string -> unit) -> unit

(** [select_string mb ~title ~choice default_text f] makes the user
       choose a string among the given choices and then calls [f]
       with the choices when the user validates.
       @param a minibuffer history can be given to use.*)
val select_string :
  ?history:Ed_minibuffer.minibuffer_history ->
  Ed_minibuffer.minibuffer ->
  title:string -> choices:string list -> string -> (string -> unit) -> unit

(** [input_string mb ~title default_text f] makes the user enter a
       text and calls [f] on that text when the user validates. *)
val input_string :
  ?history:Ed_minibuffer.minibuffer_history ->
  Ed_minibuffer.minibuffer ->
  title:string -> string -> (string -> unit) -> unit

(** FIXME: explain this *)
val input_command_arg :
  Ed_minibuffer.minibuffer ->
  ?history:Ed_minibuffer.minibuffer_history ->
  title:string -> (string -> unit) -> string -> string array -> unit

(** [confirm mb question f] makes the user answer to the given question
       and calls [f] if the response is "yes" *)
val confirm : Ed_minibuffer.minibuffer -> string -> (unit -> unit) -> unit

(** [choice_in_list list f] pops up a menu to make the user select
     an entry in the menu. Then [f] is called on the data associated
     to the entry. *)
val choice_in_list : ('a -> unit) -> (string * 'a) list -> unit

