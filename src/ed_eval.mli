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

(** OCaml code evaluation. 
   This module is present only in the bytecode version of chamo and
   defines commands to evaluate OCaml code in the editor itself,
   to modify is behaviour, add commands, etc... 
   
   This module also adds a command line option [--run] taking
   a filename whose contents must be evaluated at launch time.
*)

(** The user's init file evaluated at launch time. This file
   should contain OCaml source code. *)
val init_file : string

(** This is the code of the default "prompt_eval" command.
  It uses the active window's minibuffer to ask the user
  for some code to evaluate.
*)
val prompt_eval : string array -> unit

(** This is the code of the default "eval_file" command.
     It tries to evaluate the file whose name is in first
     parameter. If there is no argument, then the active
     window's minibuffer is used to make the user select a file. *)
val eval_file : string array -> unit

(** This is the code of the default "load_file" command.
     It tries to load the ocaml bytecode file whose name is in first
     parameter. If there is no argument, then the active
     window's minibuffer is used to make the user select a file. *)
val load_file : string array -> unit

(** Code of the default "eval" command. It evaluates the OCaml source
     code given as first argument. If no argument is given, then
     the "prompt_eval" command is called. *)
val eval_ocaml : string array -> unit

