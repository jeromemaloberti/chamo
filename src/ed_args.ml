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

(* $Id: cam_args.ml 758 2011-01-13 07:53:27Z zoggy $ *)

let commands = ref []

let append_command s =
  commands := !commands @ [s]

let init_commands = ref []

let append_init_command s =
  init_commands := !init_commands @ [s]

let set_locale s = ignore(Glib.Main.setlocale `CTYPE (Some s))

let remaining = ref []

let options = ref
  [
    "-e", Arg.String append_command,
    "<command>\texecute command after intialization" ;

    "--locale", Arg.String set_locale,
    "<locale>\n\t\tset locale, for example fr_FR" ;

  ]

let add_option o = options := !options @ [o]

let parse () =
  Arg.parse !options
    (fun s -> remaining := s :: !remaining)
    Ed_messages.usage;
  remaining := List.rev !remaining

