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

(** Gui for multi-contents clipboards. See module {!Multiclip}. *)

(** A box to display a storable clipboard.
   @param auto_read_write If set to [true], the contents of the multiclip
   is
   - read at initialization of the object and before adding or removing
   an element,
   - written after adding or removing an elements, and after reading in
   the initializer (so that the file is created).
   *)
class multiclip_box :
  ?auto_read_write:bool ->
  Multiclip.storable_multiclip ->
  object
    inherit [(string * string)] Gmylist.plist
    method add : ?abs:string -> string -> unit
    method content : (string * string) list
    method reload : unit
    method remove : string -> unit
    method update : unit
  end

(** A window to display a stoable clipboard. Not finished yet. *)
class multiclip_window : Multiclip.storable_multiclip ->
  object method window : GWindow.window end
