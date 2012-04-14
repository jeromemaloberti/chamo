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

(** The sourceview view, to edit any text file. *)

(** [get_att name pairs] return the value associated to the given name in the
   given list of pairs, or None if no value is associated to the name. *)
val get_att : string -> (string * string) list -> string option

(** Same as {!get_att} but apply the given function on the value associated to
     the name.
     @param default can be used to set a default value to return when no value
     is associated to the given name.
     *)
val get_att_f :
  ?default:'a -> (string -> 'a) -> string -> (string * string) list -> 'a option

(** The language manager. It is the same as {!Gtksv_utils.language_manager},
       to share preferences between all views using this languages manager. *)
val language_manager : GSourceView2.source_language_manager

(** Return the language associated to the given filename, according to
     the {!Ed_sourceview_rc.filename_language_patterns} option. *)
val lang_of_filename : string -> GSourceView2.source_language option

(** Return an UTF-8 string from the given filename.
     @param full can be used to get the complete filename [true] instead of
     the basename [false]. Default is [false]. *)
val utf8_of_filename : ?full:bool -> string -> string

type location =
  Linechar of int * int
    | Linechars of int * (int * int)
    | Char of int
    | Chars of int * int

(** Parse a location string. Can be of form [n] to indicate character number [n],
     or [l,c] to indicate line [l], character [c]. *)
val location_of_string : string -> location option

(** Create a string to represent the given location. *)
val string_of_location : location -> string

(** Create a string to represent the given (line, character) location. *)
val string_of_line_char : int * int -> string

(** Return the (line, character) position of the given [GText.iter].*)
val location_of_iter : GText.iter -> (int * int)

(** The name of the file where to load and store the list of open buffers. *)
val open_buffers_file : string ref

val read_open_buffers_file : string -> (string * (string * string) list) list
val write_open_buffers_file :
  string -> < filename : string; attributes : (string * string) list; .. > list -> unit

(** The history of buffers. First element is the most recently used.
     The list contains the names of buffers.*)
val buffer_name_history : string list ref
val remove_buffer_from_history : string -> unit
val make_buffer_first_in_history : string -> unit

(** An history of pastable text, uasble in a minibuffer. *)
val pastable_history : Ed_minibuffer.minibuffer_history

(** {2 Buffers}
     A buffer is a [GSourceview.source_buffer] with some additional methods. *)
class my_buffer :
  unit ->
    object
      inherit GSourceView2.source_buffer

      val mutable cursor_moved_signal_ids : (int * GtkSignal.id) list
      val mutable delete_range_signal_ids : (int * GtkSignal.id) list
      val mutable insert_text_signal_ids : (int * GtkSignal.id) list
      val mutable modified_changed_signal_ids : (int * GtkSignal.id) list

      method set_syntax_mode : GSourceView2.source_language option -> unit
      method syntax_mode : GSourceView2.source_language option

      method connect_cursor_moved : int -> (unit -> unit) -> unit
      method connect_delete_range : int -> (start:GText.iter -> stop:GText.iter -> unit) -> unit
      method connect_insert_text : int -> (GText.iter -> string -> unit) -> unit
      method connect_modified_changed : int -> (unit -> unit) -> unit
      method remove_cursor_moved : int -> unit
      method remove_delete_range : int -> unit
      method remove_insert_text : int -> unit
      method remove_modified_changed : int -> unit
      method remove_view_callbacks : int -> unit

      (** [re_search forward ?start ?stop re] searches form the given regular expression and
           returns the start and stop iters of the matched string.
           Regexp are searched using Pcre, so UTF-8 strings are handled.
           @param start can be used as start bound of the search.
           @param stop can be used as end bound of the search.
           @param forward indicates if search if forward [true] of backward [false].*)
      method re_search : bool -> ?start: GText.iter -> ?stop: GText.iter ->
        Pcre.regexp -> (GText.iter * GText.iter) option

      method place_cursor : where: GText.iter -> unit
      method get_iter : GText.position -> GText.iter
  end

(** {2 Modes} *)

class type mode =
  object
    method key_bindings : (Okey.keyhit_state * string) list
    method menus : (string * GToolbox.menu_entry list) list
    method name : string

    (** These methods are used to define replacements of some parts
         of the contents of the file (in utf8) and the string displayed (in utf8).
    *)

    method to_display : string -> string
    method from_display : string -> string
    method set_to_display : (string -> string) -> unit
    method set_from_display : (string -> string) -> unit

    (** Definition of a regexp describing characters of words. *)
    method word_re : string
  end

class empty_mode : mode

(** Register a mode.
   @param replace can be used to indicate that if there is already
   a mode with the same name, it should be replaced. Default is [false].
   @raise Failure if a mode with the same name already exists.
   *)
val register_mode : ?replace:bool -> mode -> unit

(** Get the mode with the given name.
     @raise Not_found if no such mode was registered. *)
val get_mode : string -> mode

(** The list of registered modes. *)
val available_mode_names : unit -> string list

(** {2 Buffered files}
     Buffered files are buffered associated to a file. *)

class buffered_file :
  ?attributes: (string * string) list ->
    ?loc:int * int ->
    name:string ->
    filename:string ->
    my_buffer ->
    object
      val buffer : my_buffer

        (* modification date of the file when it was loaded or saved in this buffer *)
      val mutable date : float option
      val mutable encoding : string option
      val mutable filename : string
      val mutable location : int * int
      val mutable mode : mode option
      val mutable name : string
      val mutable source_marks : (string * GSourceView2.source_mark) list

      method add_source_mark : string * GSourceView2.source_mark -> unit
      method attributes : (string * string) list
      method buffer : my_buffer

      method date : float option
      method encoding : string option
      method filename : string

      (** Return the character offset in the buffer corresponding
           to the first character of the given line in the file. *)
      method line_offset_from_line_in_file : int -> int
      method location : int * int
      method mode : mode option
      method mode_key_bindings : (Okey.keyhit_state * string) list
      method mode_menus : (string * GToolbox.menu_entry list) list
      method mode_name : string option
      method mode_from_display : string -> string
      method mode_to_display : string -> string
      method name : string
      method set_date : float option -> unit
      method set_encoding : string option -> unit
      method set_filename : string -> unit
      method of_utf8 : string -> string
      method to_utf8 : string -> string
      method select_location : location -> unit
      method set_location : int * int -> unit

      (** From a range (in character offsets) in the given file,
           return the corresponding range in the buffer. *)
      method range_from_range_in_file : left: int -> right: int -> (int * int)

      (** Select the given range in the buffer. The left and right
           parameters are character offsets in the file.
           *)
      method select_range_in_file : left: int -> right: int -> unit
      method set_mode : mode option -> unit
      method set_name : string -> unit
      method set_source_marks : (string * GSourceView2.source_mark) list -> unit
      method set_syntax_mode : GSourceView2.source_language option -> unit
      method source_marks : (string * GSourceView2.source_mark) list
      method syntax_mode : GSourceView2.source_language option

      (** update date with the last modification date of the file *)
      method update_date : unit

      (** update the markers according to the displayed file and the bookmarks *)
      method update_source_marks : unit

      method load_file : string -> unit

      (** Return whether the file was modified since last loaded or written
           by chamo. *)
      method newer_file_exists : bool

      (** @raise Newer_file_exists [filename] if the file on disk has been
           written since chamo loaded or wrote it.
           @param fail_if_newer indicates whether to raise [Newer_file_exists]. Default if [false].
           *)
      method write_file : ?fail_if_newer: bool -> unit -> unit
    end

(** {2 Sourceview views}
   TODO: methods acting on buffer contents should be moved to
   {!buffered_file} or {!my_buffer} classes.
*)

class sourceview :
  ?attributes:(string * string) list ->
  Ed_view.topwin ->
  ('a -> unit) ->
  ('a -> 'b) ->
  (buffered_file -> Ed_view.topwin -> Ed_view.gui_view) ->
  (string -> string -> unit) ->
  buffered_file ->
    object ('a)
      inherit Ed_view.dyn_label
      inherit Ed_view.dyn_destroyable
      val mutable file : buffered_file
      val mutable my_location : int * int
      val mutable on_focus_in : unit -> unit
      method attributes : (string * string) list
      method backward_char : unit
      method backward_line : unit
      method backward_word : unit
      method beginning_of_line : unit
      method bookmarks_menus : (string * GToolbox.menu_entry list) list
      method box : GObj.widget
      method buffer_modified : bool
      method buffer_name : string
      method close : unit
      method connect_buffer_events : unit
      method copy : (unit -> unit) option
      method current_line : int
      method cut : (unit -> unit) option
      method cut_to_selection :
        ?concat:[ `APPEND | `PREPEND ] ->
          start:GText.iter -> stop:GText.iter -> unit -> unit
      method delete_char : bool -> unit
      method display_buffer_name : unit
      method display_encoding : unit
      method display_location : unit
      method display_mode : unit
      method display_modified : unit
      method display_state : unit
      method display_stx_mode : unit
      method do_save : unit
      method dup : Ed_view.topwin -> Ed_view.gui_view option
      method end_of_line : unit
      method file : buffered_file
      method filename : string
      method forward_char : unit
      method forward_line : unit
      method forward_word : unit
      method goto_char : int -> unit
      method goto_line : int -> unit
      method grab_focus : unit
      method has_focus : bool
      method insert : string -> unit
      method key_bindings : (Okey.keyhit_state * string) list
      method kill_line : append:bool -> unit
      method kill_word : ?concat:[ `APPEND | `PREPEND ] -> bool -> unit
      method kind : string
      method location_in_buffer : int * int
      method menus : (string * GToolbox.menu_entry list) list
      method minibuffer : Ed_minibuffer.minibuffer
      method my_set_label : unit
      method on_cursor_moved : unit
      method paste : (unit -> unit) option
      method place_cursor : ?scroll: bool -> GText.iter -> unit
      method redo : unit
      method reload : (unit -> unit) option
      method save : (unit -> unit) option
      method save_as : (unit -> unit) option
      method select_location : location -> unit
      method select_location_opt : location option -> unit

        (** Select the given range in the buffer. The left and right
           parameters are character offsets in the file.
           @parameter jump can be used to indicate that the insert
           cursor must be positioned to left or right, or not positioned.
           *)
      method select_range_in_file :
        ?jump: [`Left|`Right] -> left: int -> right: int -> unit -> unit
      method set_file : ?focus_in:bool -> buffered_file -> unit
      method set_label : string -> unit
      method set_location : int * int -> unit
      method set_mode : mode option -> unit
      method set_my_location : int * int -> unit
      method set_on_focus_in : (unit -> unit) -> unit
      method set_scroll_on_change : unit
      method set_encoding : string option -> unit
      method set_syntax_mode : GSourceView2.source_language option -> unit
      method set_wrap_mode : Gtk.Tags.wrap_mode -> unit
      method source_buffer : my_buffer
      method source_view : GSourceView2.source_view
      method switch_line_markers : ?v:bool -> unit -> unit
      method switch_line_numbers : ?v:bool -> unit -> unit
      method transpose_chars : unit
      method transpose_lines : unit
      method transpose_words : unit
      method undo : unit
      method unset_scroll_on_change : unit
      method update_menus : unit
      method update_my_location : unit
      method private write_file : unit
    end

(** {2 Associating [buffered_files] and [modes]} *)

(** Use the {!Ed_sourceview_rc.filename_mode_patterns} option to
   get the associated mode name from the given filename. *)
val mode_name_of_filename : string -> string option

(** Get the mode to use from the given filename, using
     {!mode_name_of_filename}. *)
val mode_of_filename : string -> mode option

(** {2 Management of views} *)

val views : sourceview list ref
val buffers : buffered_file list ref
val active_sourceview : sourceview option ref
val set_active_sourceview : sourceview -> unit
val get_fresh_buffer_name : string -> string
val create_buffer :
  ?attributes:(string * string) list -> string -> buffered_file
val get_buffer :
  ?attributes:(string * string) list -> string -> buffered_file
val get_buffer_by_name : string -> buffered_file
val remove_buffer : buffered_file -> unit
val on_view_destroy : sourceview -> unit
val create_view :
  ?attributes:(string * string) list ->
  Ed_view.topwin -> buffered_file -> sourceview
val dup : buffered_file -> Ed_view.topwin -> Ed_view.gui_view
val file_rename : string -> string -> unit
val open_file :
  Ed_view.topwin ->
  Ed_view.gui_view ->
  ?attributes:(string * string) list ->
  string ->
  [> `New_view of Ed_view.gui_view | `Use_view of Ed_view.gui_view ]

class factory : Ed_view.view_factory

(** {2 Commands} *)

(** [register_com ~prefix name args ?more f] creates and register a command with
     name [prefix_name], arguments names [args] and command body [f].
     @param f takes the active sourceview and the regular command arguments.
     @param more can be used to describe the remaining parameters of the command.
*)
val register_com :
  prefix:string ->
  string ->
  string array ->
  ?more:string -> (sourceview -> string array -> unit) -> unit

val switch_to_buffer : sourceview -> string -> unit
val candidate_buffers : unit -> string list
val switch_buffer_history : Ed_minibuffer.minibuffer_history
val switch_buffer : sourceview -> string array -> unit
val destroy_buffer : sourceview -> 'a -> unit
val prev_search : string option ref

type search_buffer_function =
  ?wrapped:bool ->
    bool ->
    my_buffer ->
    ?start:GText.iter -> string -> bool * (GText.iter * GText.iter) option

val search_buffer : search_buffer_function
val re_search_buffer : search_buffer_function

val search :
  search_buffer_function -> string ->
    ?changed:bool -> bool -> sourceview -> string array -> unit

val replace_history : Ed_minibuffer.minibuffer_history

(** [query_replace_gen ?mes command_name fsearch_buffer freplace sourceview args] *)
val query_replace_gen :
  ?mes: string -> string ->
    search_buffer_function ->
    (searched: string -> found:string -> repl:string -> string) ->
    sourceview -> string array -> unit

val query_replace : sourceview -> string array -> unit
val re_query_replace : sourceview -> string array -> unit

val paste : sourceview -> string array -> unit
val copy : sourceview -> string array -> unit
val cut : sourceview -> string array -> unit
val beginning_of_line : sourceview -> string array -> unit
val end_of_line : sourceview -> string array -> unit
val undo : sourceview -> string array -> unit
val redo : sourceview -> string array -> unit
val forward_word : sourceview -> string array -> unit
val backward_word : sourceview -> string array -> unit
val forward_line : sourceview -> string array -> unit
val backward_line : sourceview -> string array -> unit
val forward_char : sourceview -> string array -> unit
val backward_char : sourceview -> string array -> unit
val kill_line : sourceview -> string array -> unit
val kill_word : sourceview -> string array -> unit
val backward_kill_word : sourceview -> string array -> unit
val delete_char : sourceview -> string array -> unit
val backward_delete_char : sourceview -> string array -> unit
val transpose_chars : sourceview -> string array -> unit
val transpose_lines : sourceview -> string array -> unit
val transpose_words : sourceview -> string array -> unit
val yank_choose : sourceview -> string array -> unit
val insert : sourceview -> string array -> unit
val goto_history : Ed_minibuffer.minibuffer_history
val goto_line : sourceview -> string array -> unit
val goto_char : sourceview -> string array -> unit
val force_save : sourceview -> string array -> unit
val syntax_mode_history : Ed_minibuffer.minibuffer_history
val set_syntax_mode : sourceview -> string array -> unit
val popup_syntax_mode_choice : sourceview -> string array -> unit
val mode_history : Ed_minibuffer.minibuffer_history
val set_encoding : sourceview -> string array -> unit
val set_mode : sourceview -> string array -> unit
val popup_mode_choice : sourceview -> string array -> unit
val switch_line_numbers : sourceview -> string array -> unit
val switch_line_markers : sourceview -> string array -> unit
val set_wrap_mode : sourceview -> string array -> unit

