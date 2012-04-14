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

module CF = Config_file

let rc_file = Ed_config.rc_file "views"

let group = new CF.group

let default_filename_view_patterns =
  [
    ".*\\.odoc$", "odoc" ;
    ".*\\.tdl$",  "tdl" ;
    ".*\\.todo$", "tdl" ;
    ".*\\.mclip$", "multiclip";
  ]

let filename_view_patterns =
  new CF.list_cp ~group
    (CF.tuple2_wrappers CF.string_wrappers CF.string_wrappers)
    ["view_from_filename_patterns"]
    default_filename_view_patterns
    ("Associations between regular expressions on filename and the "^
     "view to use to open the file")

let default_default_view = "sourceview"
let default_view = new CF.string_cp ~group
  ["default_view"] default_default_view
    "Default view used to when opening files when no pattern on filename matched."

let read () = group#read rc_file
let write () = group#write rc_file
