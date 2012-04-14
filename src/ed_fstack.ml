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

type 'a t = {
  mutable data : 'a list ;
  mutable f_data : 'a list ; (** forward data *)
  }
;;

exception Empty;;

let create () = { data = [] ; f_data = [] };;
let push x t =
  t.data <- x :: t.data ;
  t.f_data <- []
;;

let pop t =
  match t.data with
    [] -> raise Empty
  | x :: q ->
      t.data <- q;
      t.f_data <- x :: t.f_data;
      x
;;

let forward t =
  match t.f_data with
    [] -> raise Empty
  | x :: q ->
      t.data <- x :: t.data ;
      t.f_data <- q;
      x
;;

let clear t = t.data <- [] ; t.f_data <- [];;

let is_empty t = t.data = [];;
let can_forward t = t.f_data <> [];;
let top t =
  match t.data with
    [] -> raise Empty
  | x :: _ -> x
;;
let length t = List.length t.data;;
let forward_length t = List.length t.f_data;; 