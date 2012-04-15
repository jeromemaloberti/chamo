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

let factory_name = "odoc"
let rc_file = Ed_config.rc_file factory_name

let group = new CF.group

let default_key_bindings  = [
]

let key_bindings = new CF.list_cp Ed_config.binding_wrappers ~group
    ["key_bindings"] default_key_bindings "Key bindings"

let create_pix name def help =
  new CF.filename_cp ~group ["pixmap";name] def
    ("Pixmap file used to represent "^help)

let pix_file = create_pix "file"
    (Filename.concat Ed_installation.pixmap_dir "file_component.png")
    "files"
let pix_comp = create_pix "component"
    (Filename.concat Ed_installation.pixmap_dir "component.png")
    "classes and modules"
let pix_other = create_pix "other"
    (Filename.concat Ed_installation.pixmap_dir "run.png")
    "other stuff"
let pix_type = create_pix "type"
    (Filename.concat Ed_installation.pixmap_dir "type.png")
    "types"
let pix_fun = create_pix "function"
    (Filename.concat Ed_installation.pixmap_dir "fun.png")
    "functions and methods"
let pix_value = create_pix "value"
    (Filename.concat Ed_installation.pixmap_dir "value.png")
    "non functional values"


let read () = group#read rc_file
let write () = group#write rc_file
