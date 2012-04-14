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

(* $Id: cam_commands.mli 758 2011-01-13 07:53:27Z zoggy $ *)

(** Commands management *)

type command = string array -> unit

type command_desc =
    {
      (** Name of the command *)
      com_name : string ;

      (** Description of arguments *)
      com_args : string array ;

      (** Description of arguments who are optional or can appear
	 a various number of times. *)
      com_more_args : string option ;

      (** Function executing the command *)
      com_f : command ;
    }

(** [register command] registers the given command.
     @table can be used to specify a different command table than the default one.
     @replace is [false] by default and means that when registering a command,
     if a command with the same name is alerady register, a [Failure] exception
     is raised. If set to [true], the given command replaces the previous one (if any).
*) 
val register :
  ?table: (string, command_desc) Hashtbl.t -> 
    ?replace: bool -> command_desc -> unit

(** [register_before command] replace the previous command with the same name
       (if any) by a function calling the given command (with the same name) then
       the function assciated to the previous command. The same arguments are passed to
       the two functions. If the first command fails, the second is not called.
       If there is no previous command registered with the same name, the function
       is similar to {!register}.
*)
val register_before : ?table: (string, command_desc) Hashtbl.t -> command_desc -> unit

(** Same as {!register_before} but the function of the given command is called after
     the previous command registered with the same name (if any). *)
val register_after : ?table: (string, command_desc) Hashtbl.t -> command_desc -> unit
           
(** Get the command with the given name.
     @param table can be used to specify an alternative command table.
     @raise Not_found if the command was not found. *)            
val get_com : ?table: (string, command_desc) Hashtbl.t -> string -> command_desc

(** Same as {!get_com} but raise a Failure exception with a comprehensive message
     when the command is not found. *)
val get_com_or_fail : ?table: (string, command_desc) Hashtbl.t -> string -> command_desc
 
val string_to_argv : string -> string array
val argv_to_string : string array -> string

val launch_command :
    ?history: bool -> ?table:(string, command_desc) Hashtbl.t -> string -> string array -> unit

(** Return [true] if is currently executed command is the same as the previous command one.
   This can be used in commands to have a different behaviour when the same command
   is triggered mutliple time. *)
val same_previous_command : unit -> bool

val ask_launch_command :
    ?history: bool ->
      ?table:(string, command_desc) Hashtbl.t ->
        ?width:int -> string -> string array -> unit
val eval_command :
    ?history: bool ->
      ?table:(string, command_desc) Hashtbl.t -> string -> unit

val available_command_names :
    ?table:(string, command_desc) Hashtbl.t -> unit -> string list

(** Create a simple command with a function taking no argument. *)
val unit_com : string -> (unit -> unit) -> command_desc

(** Convenient function to create a command. *)
val create_com :
  string ->
  ?more:string ->
  string array -> command -> command_desc

(** {2 Global variables} *)

(** [set_global name value] associates the given [value] to the given [name].*)
val set_global : string -> string -> unit

(** [get_global name] returns the value associatd to [name].
     @raise Not_found if the variable has no value.*)
val get_global : string -> string

(** [safe_get_global name] works as {!get_global} but returns an empty
     string [""] if no value is associated to the given [name].*)
val safe_get_global : string -> string
