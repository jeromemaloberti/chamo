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

(** Sourceview mode for Objective-Caml files. *)

(** Remove the ehading blanks of a given string. *)
val remove_first_blanks : string -> string

(** Indent the current line of the given sourceview. This is the code
     of the "ocaml_indent_line" command. *)
val indent_line : Ed_sourceview.sourceview -> string array -> unit

(** Indent the whole buffer of the given sourceview.
     This is the code of the "ocaml_indent_buffer" command. *)
val indent_buffer : Ed_sourceview.sourceview -> string array -> unit

(** Open the interface (respectively the implementation) of the
  ocaml implementation (resp. interface) file in the given sourceview.
  Does nothing if the file does not have ".ml" or ".mli" extension.
  This is the code of the "ocaml_switch_file" command. *)
val switch_file : Ed_sourceview.sourceview -> string array-> unit

(** This variable indicates how many levels up to search for a
     [_build] directory to when looking for a [.annot] file. *)
val annot_file_build_dir_max_parent_level : int ref;;

(** If the file in the given sourceview is a ".ml", then lookup
     for the corresponding ".annot" file and search in it for
     type annotation corresponding to the current position in the buffer.
     If an annotation is found, highlight the part of the source code
     corresponding to the type annotation and display the type annotation
     in the minibuffer of the sourceview's window.
     If an errors occurs (no type annotation found or ".ml" file modified
     since the creation of the ".annot" file, for example), a message
     is displayed in the minibuffer.
     This is the code of the "ocaml_display_type_annot" command.
*)
val display_type_annot : Ed_sourceview.sourceview -> string array -> unit

(** Same as {!display_type_annot} but also copy the type in the
     main clipboard.*)
val copy_type_annot : Ed_sourceview.sourceview -> string array -> unit

(** Same as {!display_type_annot} but display call annotation. *)
val display_call_annot : Ed_sourceview.sourceview -> string array -> unit

(** Same as {!display_type_annot} but display ident annotation. *)
val display_ident_annot : Ed_sourceview.sourceview -> string array -> unit

(** Same as {!display_ident_annot} but in case of a local ident (that is,
     defined in the file), jump to the ident definition.*)
val jump_to_local_def : Ed_sourceview.sourceview -> string array -> unit


(** The mode.*)
val mode : Ed_sourceview.mode