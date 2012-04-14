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

(** Minibuffers. *)

(** To use an history in minibuffer. *)
class minibuffer_history : unit ->
  object
    method add : string -> unit

    (** reinitialize the position used by the [get_next] and [get_previous] methods. *)
    method init_pos : unit
    method get_next : string option
    method get_previous : string option
  end
val history : unit -> minibuffer_history

(** Max height of the minibuffer. *)
val max_size : int

(** The minibuffers. See code of {!Ed_misc.input_string} or
  {!Ed_misc.select_string} for an example of using the minibuffer. *)
class minibuffer :
  unit ->
  object
    val mutable active : bool
    val mutable editable_from : int
    val mutable history : minibuffer_history option
    val mutable ignore_text_changed : bool
    val mutable more_key_bindings : (Okey.keyhit_state * (unit -> unit)) list
    val mutable on_active_change : bool -> unit
    val mutable on_complete : unit -> unit
    val mutable on_eval : unit -> unit
    val mutable on_text_changed : unit -> unit
    method active : bool
    method box : GObj.widget
    method clear : unit
    method complete : unit
    method eval : unit
    method eval_custom_key_binding : string -> unit
    method exit : unit -> unit
    method get_user_text : string
    method history_key_bindings : (Okey.keyhit_state * string) list
    method history_next : unit
    method history_previous : unit
    method insert : string -> unit
    method key_bindings : (Okey.keyhit_state * string) list
    method more_key_bindings : (Okey.keyhit_state * string) list
    method on_text_changed : unit
    method set_active : bool -> unit
    method set_history : minibuffer_history -> unit
    method set_more_key_bindings : (Okey.keyhit_state * (unit -> unit)) list -> unit
    method set_on_active_change : (bool -> unit) -> unit
    method set_on_eval : (unit -> unit) -> unit
    method set_on_complete : (unit -> unit) -> unit
    method set_on_text_changed : (unit -> unit) -> unit
    method set_size : unit
    method set_text : ?list:string list -> ?fixed:string -> string -> unit
    method set_user_text : string -> unit
    method string_of_list : string list -> string
    method wait : unit
  end
