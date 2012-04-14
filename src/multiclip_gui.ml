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

module M = Multiclip

let targets = [
  { Gtk.target = "STRING"; flags = []; info = 0};
  { Gtk.target = "text/plain"; flags = []; info = 0};
  { Gtk.target = "text/uri-list"; flags = []; info = 2};
]
;;

class multiclip_box ?(auto_read_write=false) st_clip =
  let columns = [
      None, Gmylist.String (fun (abs,_) -> abs)
    ]
  in
  object(self)
    inherit [(string * string)] Gmylist.plist `SINGLE columns false as plist
    method content =
      List.sort Pervasives.compare
        (M.elements (M.storable_get_multiclip st_clip))

    method add ?abs contents =
      if auto_read_write then M.read_multiclip st_clip;
      M.add (M.storable_get_multiclip st_clip) ?abs contents;
      if auto_read_write then M.write_multiclip st_clip;
      self#update

    method remove s =
      if auto_read_write then M.read_multiclip st_clip;
      M.remove (M.storable_get_multiclip st_clip) s;
      if auto_read_write then M.write_multiclip st_clip;
      self#update

    method update = self#update_data (self#content)

    method reload =
      M.read_multiclip st_clip;
      self#update

    initializer
      if auto_read_write then
        (
         M.read_multiclip st_clip;
         M.write_multiclip st_clip
        );
      self#update_data (self#content);
      let data_get _ sel ~info ~time =
        match self#selection with
          (_, text) :: _ -> sel#return ?typ: None ?format: None text
        | [] -> ()
      in
      let drop context ~x ~y ~time =
        match context#targets with
        | [] -> false
        | d :: _ -> view#drag#get_data ~target:d ~time context ; false
      in
      let data_received context ~x ~y data ~info ~time =
        if data#format = 8 then
          begin
            self#add data#data;
            context#finish ~success:true ~del:false ~time
          end
        else
          context#finish ~success:false ~del:false ~time
      in
      view#drag#source_set targets
        ~modi:[`BUTTON1 ] ~actions:[`COPY ];
      ignore(self#view#drag#connect#data_get ~callback: data_get);

      view#drag#dest_set targets ~actions:[`COPY;`MOVE];
      ignore(view#drag#connect#drop ~callback:drop);
      ignore(view#drag#connect#data_received ~callback:data_received);
  end;;

class multiclip_window st_clip =
  let box = new multiclip_box st_clip in
  let win = GWindow.window ~title: (Filename.basename (M.storable_get_file st_clip)) () in
  let () = win#add box#box in
  object(self)
    method window = win
    initializer
      win#show()
  end