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

(** A window to display various output boxes with tabs. *)

(** The class of objects which can be added to an "outputs" window. *)
class type output =
  object
    method box : GObj.widget
    method label : string
    method name : string
    method on_destroy : unit
    method set_label : string -> unit
  end

(** An object to represent an "outputs" window. It consists of a notebook
   where "output" object can be added in tabs. *)
class outputs : ?on_destroy:(unit -> unit) -> unit ->
  object
    (** To add a new output to the window. It an output with the same
       name alrady exists in the window, then a [Failure] exception is raised. *)
    method add_output : output -> unit

    (** Get the output object by the given name.
         Note that the object returned is not the same as the object given
         to the [add_output] method, be rather an object of class type
         [output] but with some methods redefined.
         @raise Not_found if no output of this name is in the window. *)
    method output_by_name : string -> output

    (** Call this method to present the window to the used, and make
         the notebook show the page with the output of the given name. *)
    method show : string -> unit

    (** Methods inherited from the glade-generated file. *)

    method bind : name:string -> callback:(unit -> unit) -> unit
    method check_widgets : unit -> unit
    method notebook : GPack.notebook
    method private output_pos : string -> int
    method outputs : GWindow.window
    method reparent : GObj.widget -> unit
    method toplevel : GWindow.window
    method xml : Glade.glade_xml Gtk.obj
  end

(** This function returns the outputs window, creating if it does not exist. *)
val outputs : unit -> outputs

(** [watch_and_insert ic insert] watches on input channel [ic] and uses
     the given [insert] when text is available and read from the channel.
     @param on_end can be used to specify a function called when the channel is closed.
     @return the watch id
     *)
val watch_and_insert : ?on_end: (unit -> unit) -> in_channel -> (string -> unit) -> GMain.Io.id

(** [run_and_read_in_buffer command insert f] runs a command and display its
     output with the [insert] function.
     The function [f] is called when the command exits, and
     takes in parameter the exit code (or signal number if the command was
     killed or stopped). *)
val run_and_read_in_buffer : string -> (string -> unit) -> (int -> unit) -> unit

(** This class defines an output to show command executions (typically
     compilation commands) in a [GSourceView2.source_view widget].
     @param on_destroy can be used to given a function to execute when
     the widget is destroyed.*)
class text_output :
  ?on_destroy:(unit -> unit) ->
  string ->
  object
    val mutable label : string
    method box : GObj.widget
    method contents : string
    method insert : string -> unit
    method label : string
    method name : string
    method on_destroy : unit

    method view : GSourceView2.source_view

    (** Clear the source_buffer. *)
    method reset : unit

    (** [run command f] runs the given command and display its output
    (both stdout and stderr) in the source_buffer, in a different thread
    so that the application is not stalled while the command runs.
    The use of a mutex ensures that two command executions don't output
    to the buffer at the same time (i.e. the second command waits for the end
    of the first one).
    @param reset indicate whether to clear the buffer before executing
    the command. Default is [false].*)
    method run : string -> ?reset:bool -> (int -> unit) -> unit

    method set_label : string -> unit
  end

(** This class inherits from {!text_output} to display the output of a
   command given at the creation of an instance; The [run text f] method sends
   the given [text] on the command stdin. The [f] function is then called,
   with 0 as parameter if output was ok, orelse 1.
   On the destruction of the output, the channels used to communicate
   with the command are closed and the command is waited.
   *)
class interactive_output :
  ?on_destroy: (unit -> unit) ->
  name: string -> command: string ->
    object
      inherit text_output
    end