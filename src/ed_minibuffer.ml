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

Ed_minibuffer_rc.read ();;
Ed_minibuffer_rc.write ();;

class minibuffer_history () =
  let history_size = Ed_gui_rc.minibuffer_history_size#get in
  let t = Array.create history_size "" in
  object
    val mutable len = 0
    val mutable pos = 0
    val mutable insert_pos = 0

    method init_pos = pos <- insert_pos

    method get_next =
      if len > 0 then
        if pos + 1 >= len then
          None
        else
          (pos <- pos + 1; Some t.(pos))
      else
        None

    method get_previous =
      if len > 0 then
        let new_pos = pos - 1 in
        if new_pos >= 0 && new_pos < len then
          (pos <- new_pos; Some t.(pos))
        else
          None
      else
        None

    method add s =
      let insert =
        (
	 if len > 0 then
           let p = insert_pos - 1 in
           let p = if p < 0 then len - 1 else p in
           t.(p) <> s
	 else
	   true
        )
      in
      if insert then
        (
         t.(insert_pos) <- s;
         if len >= history_size then () else len <- len + 1;
         insert_pos <- (insert_pos + 1) mod history_size
        )
      else
        ()
  end

let history () = new minibuffer_history ()

let max_size = 25

let get_size_chars widget =
  let (w,h) = Gdk.Drawable.get_size widget#misc#window in
  let metrics =
    (widget#misc#pango_context : GPango.context)#get_metrics () in
  let width = w / (GPango.to_pixels metrics#approx_digit_width)
  and height = h / (GPango.to_pixels (metrics#ascent+metrics#descent)) in
  (width,height)

let fill_string size s =
  let m = size - Cam_misc.utf8_string_length s in
  Printf.sprintf "%s%s" s (String.make m ' ')

open GdkKeysyms

(* remove the combinations of keys used to exit which could mask the given list of
       key bindings. *)
let remove_used_exiting_keys l =
  let rec pred0 l1 l2 =
    match l1, l2 with
    | [], _ -> true
    | _, [] -> false
    | h1 :: q1, h2 ::q2 -> pred0 q1 q2
  in
  let pred def =
    List.for_all
      (fun (state,_) -> pred0 def state)
      l
  in
  List.filter pred Ed_minibuffer_rc.exiting_keys#get

class minibuffer () =
  let tag_table = GText.tag_table () in
  let tag_not_editable = GText.tag () in
  let _ = tag_not_editable#set_properties [`EDITABLE false ; `WEIGHT `BOLD] in
  let _ = tag_table#add tag_not_editable#as_tag in
  let tag_list = GText.tag () in
  let _ = tag_list#set_properties [`EDITABLE false ; `WEIGHT `NORMAL] in
  let _ = tag_table#add tag_list#as_tag in
  let buffer = GSourceView2.source_buffer ~tag_table () in
  let wscroll = GBin.scrolled_window
      ~height: 20
      ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC () in
  let view = GSourceView2.source_view ~source_buffer: buffer ~packing: wscroll#add () in
  let _ = view#set_wrap_mode `CHAR in
  let _ = Gtksv_utils.register_source_buffer buffer in
  let _ = Gtksv_utils.register_source_view view in
  let _ = Gtksv_utils.apply_sourceview_props view
      (Gtksv_utils.read_sourceview_props ())
  in
  let _ = view#set_editable false in
  let _ = view#set_cursor_visible false in
  object(self)
    method box = wscroll#coerce

    (** number of times the minibuffer is waitied to become inactive *)
    val mutable nb_waits = 0

    val mutable on_complete = fun () -> ()
    method set_on_complete f = on_complete <- f
    method complete = on_complete ()

    val mutable ignore_text_changed = false
    val mutable on_text_changed = fun () -> ()
    method set_on_text_changed f = on_text_changed <- f
    method on_text_changed =
      if ignore_text_changed then
        ()
      else
        on_text_changed ()

    val mutable more_key_bindings =
      ([] : (Okey.keyhit_state * (unit -> unit)) list)
    method set_more_key_bindings l =
      more_key_bindings <- l
    method more_key_bindings =
      List.map
        (fun (ks, _) ->
           let com = Printf.sprintf "%s_eval_custom_key_binding \"%s\""
             Ed_minibuffer_rc.base_name (Ed_keymaps.string_of_state ks)
           in
           (ks, com)
        )
        more_key_bindings

    method eval_custom_key_binding s =
      try
        let (_,f) = List.find
          (fun (ks, _) -> Ed_keymaps.string_of_state ks = s)
          more_key_bindings
        in
        f ()
      with Not_found -> ()

    val mutable history = (None : minibuffer_history option)
    method set_history h =
      history <- Some h;
      h#init_pos

    val mutable on_eval = fun () -> ()
    method eval =
      begin
        match history with
          None -> ()
        | Some h ->
            let s = self#get_user_text in
            h#add s
      end;
      on_eval ()

    method set_on_eval f = on_eval <- f

    val mutable on_active_change = fun (_ : bool) -> ()
    method set_on_active_change f = on_active_change <- f

    val mutable active = false
    method active = active

    (** Change the active state. If the new state is [true],
       [on_active_change] is called, else it is called only
       if the new state if different from the current state
       (i.e. we switch from "active" to "not active").
       This is so because of the [set_active_view] method
       of [Ed_gui.gui_window] which set the minibuffer state
       to "not active" when a view get the focus, and the
       function called when the minibuffer's state changes
       make the last view get the focus when the minibuffer
       is not active.
    *)
    method set_active b =
      if b or (active <> b) then
        begin
          view#set_editable b;
          view#set_cursor_visible b;
          active <- b;
          if b then
            view#misc#grab_focus ()
          else
            (
             self#clear;
             for i = 1 to nb_waits do GMain.quit () done;
             nb_waits <- 0
            );
          on_active_change b
        end

    method clear =
      on_eval <- (fun () -> ());
      on_complete <- (fun () -> ());
      on_text_changed <- (fun () -> ());
      more_key_bindings <- [];
      history <- None;
      self#set_text ""

    method wait =
      nb_waits <- nb_waits + 1;
      GMain.main ()

    (* position from which the text is editable *)
    val mutable editable_from = 0

    method set_size =
      let height =
        if buffer#line_count <= 1 then
          1
        else
          max 1 (min max_size (buffer#line_count + 1))
      in
      wscroll#misc#set_size_chars ~height ()

    method string_of_list l =
      let l = List.sort compare l in
      let (w,_) = get_size_chars view in
      let max = List.fold_left
          (fun acc s -> max acc (Cam_misc.utf8_string_length s))
          0
          l
      in
      let max = max + 3 in
      let nb = w / max in
      let nb = if nb <= 0 then 1 else nb in
      let b = Buffer.create 256 in
      let rec iter m = function
          [] ->
            if m <> 0 then Buffer.add_char b '\n';
            Buffer.contents b
        | s :: q ->
            Buffer.add_string b (fill_string max s);
            let m = (m + 1) mod nb in
            if m = 0 then Buffer.add_char b '\n';
            iter m q
      in
      iter 0 l

    method set_text ?(list=[]) ?(fixed="") (s:string) =
      ignore_text_changed <- true;
      buffer#set_text "";
      buffer#insert ~iter: buffer#start_iter ~tags: [tag_list] (self#string_of_list list);
      buffer#insert ~iter: buffer#end_iter ~tags: [tag_not_editable] fixed;
      let nb_chars = buffer#end_iter#offset in
      editable_from <- nb_chars;
      buffer#insert ~iter: buffer#end_iter s;
      self#set_size;
      ignore_text_changed <- false;
      self#on_text_changed

    method set_user_text s =
      ignore_text_changed <- true;
      let it = buffer#get_iter (`OFFSET editable_from) in
      buffer#delete ~start: it ~stop: buffer#end_iter;
      buffer#insert ~iter: buffer#end_iter s;
      ignore_text_changed <- false;
      self#set_size;
      self#on_text_changed

    method get_user_text =
      let start =
        if editable_from = 0
        then buffer#start_iter
        else buffer#get_iter (`OFFSET editable_from)
      in
      buffer#get_text ~start ~stop: buffer#end_iter ()

    method history_key_bindings =
      match history with
        None -> []
      | Some _ -> Ed_minibuffer_rc.history_key_bindings#get

    method history_previous =
      match history with
        None -> ()
      | Some h ->
          match h#get_previous with
            None -> ()
          | Some s -> self#set_user_text s

    method history_next =
      match history with
        None -> ()
      | Some h ->
          match h#get_next with
            None -> ()
          | Some s -> self#set_user_text s

    method insert s_utf8 =
      if active then
        let it = buffer#get_iter `INSERT in
        let p = it#offset in
        if p >= editable_from then
          buffer#insert s_utf8
        else
          ()
      else
        ()

    method exit () = self#set_active false

    method key_bindings : (Okey.keyhit_state * string) list =
      let l =
        Ed_minibuffer_rc.key_bindings#get @
          self#more_key_bindings @
          self#history_key_bindings
      in
      let exiting_keys = remove_used_exiting_keys l in
      let exiting_key_bindings =
        List.map (fun combs -> (combs,Ed_minibuffer_rc.base_name ^"_exit")) exiting_keys
      in
      l @ exiting_key_bindings

    initializer
      ignore(buffer#connect#changed (fun () -> self#on_text_changed));
      ignore(view#connect#destroy (fun () -> Gtksv_utils.unregister_source_buffer buffer));
(*      ignore(view#event#connect#after#focus_out (fun _ -> self#exit (); false));*)
  end
