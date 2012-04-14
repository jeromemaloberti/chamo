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

(* $Id: cam_hooks.ml 758 2011-01-13 07:53:27Z zoggy $ *)

let f_display_message = ref (fun ?(to_utf8=false) s -> prerr_endline s)
let f_warning_message = ref (fun ?(to_utf8=false) s -> prerr_endline s)
let f_error_message = ref (fun ?(to_utf8=false) s -> prerr_endline s)

let set_display_message f = f_display_message := f
let set_error_message f = f_error_message := f
let set_warning_message f = f_warning_message := f

(* FIXME: should these function take utf8 ? I think so *)
let display_message ?to_utf8 s = !f_display_message ?to_utf8 s
let error_message ?to_utf8 s = !f_error_message ?to_utf8 s
let warning_message ?to_utf8 s = !f_warning_message ?to_utf8 s

let get_display_message () = !f_display_message
let get_error_message () = !f_error_message
let get_warning_message () = !f_warning_message