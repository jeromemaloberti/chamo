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

(** Some predefined messages *)

let software = "Chamo"
let software_author = "Maxence Guesdon"
let software_author_mail = "Maxence.Guesdon@inria.fr"
let software_copyright =
  "Copyright 2004-2010 Institut National de Recherche en \n"^
  "Informatique et en Automatique. All rights reserved.\n"^
  "This software is distributed under the terms of the\n"^
  "GNU Library General Public License version 2.\n"^
  "(see file LICENSE in the distribution)"

let software_about =
  software_author^"\n"^
  software_author_mail^"\n\n"^
  software_copyright

let about = "About "^software^" ..."
let preferences = "Preferences"
