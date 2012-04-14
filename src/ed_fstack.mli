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

(* $Id: cam_hooks.ml 600 2007-11-14 09:14:22Z zoggy $ *)

(** A stack with "forward" ability, that is "pop" does not remove top-most
   element which remains accessible with the "forward" operation. *)

type 'a t

exception Empty

(** [create ()] creates an empty stack. *)
val create : unit -> 'a t

val push : 'a -> 'a t -> unit

(** @raise Empty if the stack is empty. *)
val pop : 'a t -> 'a

(** [forward stack] is the contrary of pop. Since [pop] does not remove elements,
  [forward] can be used to return the state before the previous [pop]. Two calls
  to [forward] make the stack returns to the state before the two previous [pop].
  There must not occur any [push] after [pop], or else [forward] will raise [Empty].
  Indeed, [push] makes the stack "forget" the states before the previous [pop].
  Think of this stack as the "back" and "forward" buttons of a web browser: [pop]
  is the "back" button, [forward] is the "forward" button, and [push] is either
  the click on a link or typing a new page url. *)
val forward : 'a t -> 'a

(** [top s] returns the topmost element in stack [s],
   or raises [Empty] if the stack is empty. *)
val top : 'a t -> 'a

(** Discard all elements from a stack. *)
val clear : 'a t -> unit

(** Return [true] if the given stack is empty, [false] otherwise. *)
val is_empty : 'a t -> bool

(** Return [true] if a forward operation on the given stack
     will return data, [false] otherwise. *)
val can_forward : 'a t -> bool

(** Return the number of elements in a stack. *)
val length : 'a t -> int

(** Return the number of "forward" elements in a stack. *)
val forward_length : 'a t -> int

