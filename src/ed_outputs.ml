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

(** A window to display various output boxes with tabs. *)

class type output =
  object
    method name : string
    method label : string
    method set_label : string -> unit
    method box : GObj.widget
    method on_destroy : unit
  end;;

class outputs ?(on_destroy=fun() -> ()) () =
  object(self)
    inherit Ed_gui_base.outputs ~file: Ed_config.glade_file ()

    val mutable pages = ([] : output list)

    method output_by_name name =
      List.find (fun o -> o#name = name) pages

    method private output_pos name =
      let rec iter n = function
        [] -> raise Not_found
      | h :: q -> if h#name = name then n else iter (n+1) q
      in
      iter 0 pages

    method add_output (o : output) =
      try
        ignore(self#output_by_name o#name);
        failwith (Printf.sprintf "Output \"%s\" already present." o#name)
      with
        Not_found ->
          let evt = GBin.event_box () in
          let tab = new Ed_gui_base.outputs_note_tab ~file: Ed_config.glade_file () in
          tab#reparent evt#coerce;
          tab#wlabel#set_text o#label;
          let (o : output) = object
              method name = o#name
              method label = o#label
              method box = o#box
              method set_label s = o#set_label s; tab#wlabel#set_text s
              method on_destroy = o#on_destroy
            end
          in
          ignore(o#box#misc#connect#destroy (fun _ -> o#on_destroy));
          ignore(tab#wb_close#connect#clicked
           (fun () ->
              let n = self#output_pos o#name in
              notebook#remove_page n;
              o#box#destroy ();
              pages <- List.filter (fun o2 -> o2#name <> o#name) pages;
              match pages with
                [] -> toplevel#misc#hide()
              | _ -> ()
           )
          );
          ignore(notebook#append_page ~tab_label: evt#coerce o#box);
          pages <- pages @ [o]

    method show name =
      outputs#present();
      let n = self#output_pos name in
      notebook#goto_page n

    initializer
      ignore(toplevel#connect#destroy on_destroy);
      outputs#show()
  end
;;

let outputs_window = ref None;;
let outputs () =
  match !outputs_window with
    None ->
      let o = new outputs
        ~on_destroy: (fun () -> outputs_window := None)
          ()
      in
      outputs_window := Some o; o
  | Some o -> o
;;

let watch_and_insert ?(on_end=fun() -> ()) ic insert =
  let gchan = GMain.Io.channel_of_descr (Unix.descr_of_in_channel ic) in
  let buf_size = 512 in
  let buf = Bytes.create buf_size in
  let rec f_read l =
    try
      if List.mem `IN l then
        begin
          let n = GMain.Io.read gchan ~buf ~pos: 0 ~len: buf_size in
          (
           (
            try insert (Bytes.sub buf 0 n)
            with _ -> ()
           );
           (n < buf_size) || (f_read l)
          )
        end
      else
        if List.mem `HUP l then
          (
           on_end ();
           false
          )
        else
          true
    with
      e -> prerr_endline (Printexc.to_string e); true
  in
  GMain.Io.add_watch
    ~prio: 0
    ~cond: [ `IN ; `HUP ]
    ~callback: f_read gchan
;;

let run_and_read_in_buffer command insert on_end =
  let com = Printf.sprintf "%s 2>&1" command in
  let ic = Unix.open_process_in com in
  let on_end () =
    let ret =
      match Unix.close_process_in ic with
        Unix.WEXITED n
      | Unix.WSIGNALED n
      | Unix.WSTOPPED n -> n
    in
    on_end ret
  in
  ignore(watch_and_insert ~on_end ic insert)
;;



class text_output ?(on_destroy=fun () -> ()) (name : string) =
  let wscroll = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC () in
  let view = GSourceView2.source_view
    ~wrap_mode: `CHAR
    ~editable: false
      ~packing: wscroll#add ()
  in
  let mutex = Mutex.create () in
  object(self)
    method name = name
    val mutable label = name
    method label = label
    method set_label s = label <- s
    method box = wscroll#coerce
    method view = view

    method reset =
      let b = view#buffer in
      b#delete ~start: (b#get_iter `START) ~stop: (b#get_iter `END)

    method insert text =
      view#buffer#insert text;
      Ed_gtk_misc.treat_gtk_events(); (* needed to that the scroll_to_iter works *)
      ignore(view#scroll_to_iter (view#buffer#get_iter `END))

    method run command ?(reset=false) (f_on_end : int -> unit) =
      let thread_run () =
        let on_end code =
          Mutex.unlock mutex;
          ignore(self#insert "");
          f_on_end code;
        in
        Mutex.lock mutex;
        if reset then self#reset;
        run_and_read_in_buffer command self#insert on_end;
      in
      ignore(Thread.create thread_run ())

    method contents = view#buffer#get_text ()

    method on_destroy = on_destroy ()

    initializer
      Gtksv_utils.register_source_view view;
      Gtksv_utils.register_source_buffer view#source_buffer;
      Gtksv_utils.apply_sourceview_props view
        (Gtksv_utils.read_sourceview_props ()) ;
   end;;

class interactive_output ?(on_destroy=fun() -> ()) ~name ~command =
  let command = Printf.sprintf "%s 2>&1" command in
  let (ic, oc) = Unix.open_process command in
  let destroyed = ref false in
  let id_watch = ref None in
  let on_destroy () =
    on_destroy ();
    (
     match !id_watch with
       None -> ()
     | Some id -> GMain.Io.remove id
    );
    if not !destroyed then
      (
       ignore(Unix.close_process (ic, oc)) ;
       destroyed := true;
      )
  in
  object(self)
    inherit text_output ~on_destroy name

    method run com ?(reset=false) f =
      (
       match !id_watch with
         Some _ -> ()
       | None ->
           id_watch := Some
             (
              watch_and_insert
                ~on_end: self#view#destroy
                ic self#insert
            )
      );
      if reset then self#reset;
      try output_string oc com; flush oc; f 0
      with _ -> f 1

  end;;

