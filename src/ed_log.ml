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

(* $Id: cam_log.ml 758 2011-01-13 07:53:27Z zoggy $ *)

class box () =
  let wscroll = GBin.scrolled_window
      ~hpolicy: `AUTOMATIC
      ~vpolicy: `AUTOMATIC () in
  let tag_table = GText.tag_table () in
  let buffer = GText.buffer ~tag_table () in
  let _view = GText.view
      ~cursor_visible: false
      ~wrap_mode: `CHAR
      ~buffer ~editable: false ~packing: wscroll#add () in
  let new_tag props =
    let t = GText.tag () in
    t#set_properties props;
    tag_table#add t#as_tag;
    t
  in
  let fixed_font = new_tag [`FONT "fixed"] in

  object(self)
    method box = wscroll#coerce
    method print color =
      let c = new_tag [`FOREGROUND color] in
      fun s ->
        let s = s^"\n" in
        buffer#insert
          ~iter: buffer#end_iter
          ~tags: [fixed_font ; c]
          (Glib.Convert.locale_to_utf8 s);
        let l = buffer#char_count in
        if l > Ed_constant.log_max_size then
          (
           buffer#delete
             ~start: buffer#start_iter
             ~stop: (buffer#get_iter_at_char (max (Ed_constant.log_max_size / 2) (String.length s)));
           buffer#insert ~iter: buffer#start_iter "...\n"
          )
  end

let log_window () =
  let window = GWindow.window ~kind: `TOPLEVEL ~show: false
      ~width: 500 ~height: 600 ()
      ~title: "Cameleon log"
  in
  ignore (window#event#connect#delete (fun _ -> window#misc#hide (); true));
  let vbox = GPack.vbox ~packing: window#add () in
  let v = new box () in
  ignore(vbox#pack ~expand: true v#box);
  let wb_close = GButton.button
      ~label: Ed_messages.close
      ~packing: (vbox#pack ~expand: false)
      ()
  in
  ignore (wb_close#connect#clicked window#misc#hide);
  let p_fun f ?(to_utf8=false) s =
    let s =
      (* FIXME: convert from locale or from another charset ?
         The problem is that we can't use Ed_misc from here.
         This problem could be resolved by strongly typing utf8 strings.
         *)
      if to_utf8 then
        try Glib.Convert.locale_to_utf8 s
        with _ -> s
      else
        s
    in
    f s
  in
  Ed_hooks.set_display_message (p_fun (v#print "Black"));
  Ed_hooks.set_warning_message (p_fun (v#print "Orange"));
  Ed_hooks.set_error_message (p_fun (v#print "Red"));
  window

let the_log_window = ref None

let get_log_window () =
  match !the_log_window with
  | Some w -> w
  | None ->
      let w = log_window () in
      the_log_window := Some w;
      w

let show_log_window () =
  let w = get_log_window () in
  w#show ()

let hide_log_window () =
  let w = get_log_window () in
  w#misc#hide ()

let _ =
  Ed_commands.register
    { Ed_commands.com_name = Ed_constant.com_log_window ;
      com_args = [| |] ;
      com_more_args = None ;
      com_f = (fun _ -> show_log_window ()) ;
    } ;
