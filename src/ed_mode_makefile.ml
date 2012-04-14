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

(** Makefile sourceview mode. *)

let _ = Ed_mode_makefile_rc.read ()
let _ = Ed_mode_makefile_rc.write ()

let mode_name = Ed_mode_makefile_rc.mode_name

let coms = [
]

let _ = List.iter
    (fun (name, args, more, f) ->
      Ed_sourceview.register_com
        ~prefix: mode_name name args ?more f)
    coms

class mode =
  object
    inherit Ed_sourceview.empty_mode
    method name = mode_name
    method key_bindings : (Okey.keyhit_state * string) list =
      Ed_mode_makefile_rc.key_bindings#get
    method menus : (string * GToolbox.menu_entry list) list = []
  end

let mode = new mode
let _ = Ed_sourceview.register_mode mode