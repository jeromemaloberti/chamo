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

open Xml

let layout_file = ref (Ed_config.local_dir_rc_file "layout.xml")

type  layout_view = {
    lv_kind : string ;
    lv_file : string ;
    lv_atts : (string * string) list
  }

type layout_contents = [
    `View of layout_view
  | `Paned of layout_paned
  | `Notebook of layout_notebook
  ]

and layout_paned = {
    lp_orientation : [`HORIZONTAL | `VERTICAL] ;
    lp_position : int ;
    lp_children : layout_contents * layout_contents ;
  }

and layout_notebook = {
    ln_tabs : layout_contents list ;
  }


type layout_window = {
    lw_x : int ;
    lw_y : int ;
    lw_w : int ;
    lw_h : int ;
    lw_contents : layout_contents option ;
  }

type layout = layout_window list

let xml_of_layout_view lv =
  let atts =
    (("", "kind"), lv.lv_kind) ::
      (("", "file"), lv.lv_file) ::
      (List.map (fun (a, v) -> ("",a), v) lv.lv_atts)
  in
  Ed_xml.E ((("", "view"), atts), [])

let rec xml_of_layout_contents = function
    `View v -> xml_of_layout_view v
  | `Paned p -> xml_of_layout_paned p
  | `Notebook n -> xml_of_layout_notebook n

and xml_of_layout_paned lp =
  let atts =
    [ ("", "orientation"), (match lp.lp_orientation with `VERTICAL -> "vertical" | `HORIZONTAL -> "horizontal") ;
      ("", "position"), string_of_int lp.lp_position ;
    ]
  in
  let children =
    let (c1, c2) = lp.lp_children in
    [ xml_of_layout_contents c1 ;
      xml_of_layout_contents c2 ;
    ]
  in
  Ed_xml.E ((("","paned"), atts), children)

and xml_of_layout_notebook ln =
  let children = List.map xml_of_layout_contents ln.ln_tabs in
  Ed_xml.E((("","notebook"), []), children)

let xml_of_layout_window lw =
  let atts =
    [ ("", "x"), string_of_int lw.lw_x ;
      ("", "y"), string_of_int lw.lw_y ;
      ("", "width"), string_of_int lw.lw_w ;
      ("", "height"), string_of_int lw.lw_h ;
    ]
  in
  let children =
    match lw.lw_contents with
      None -> []
    | Some c -> [xml_of_layout_contents c]
  in
  Ed_xml.E((("","window"), atts), children)

let xml_of_layout wins =
  Ed_xml.E((("","layout"), []), List.map xml_of_layout_window wins)

let store_layout file wins =
  let xml = xml_of_layout wins in
  let s = Ed_xml.string_of_xml xml in
  Ed_extern.file_of_string ~file s


let map_opt f = function
    None -> None
  | Some v -> Some (f v)

let string_opt_att name l =
  try Some (snd (List.find (fun ((_,n),_) -> n = name) l))
  with Not_found -> None
;;

let string_att name l =
  match string_opt_att name l with
    None -> failwith ("Attribute "^name^" not found")
  | Some s -> s

let int_att name l =
  let v = string_att name l in
  try int_of_string v
  with Invalid_argument _ ->
    failwith ("Bad value for attribute "^name^": "^v)

let remove_common_view_atts atts =
  let atts = List.map (fun ((_,a),v) -> (a, v)) atts in
  List.filter (fun (s,_) -> s <> "kind" && s <> "file") atts
;;

let rec layout_contents_of_xml = function
    Ed_xml.E ((("","view"),atts),_) ->
      `View { lv_kind = string_att "kind" atts ;
              lv_file = string_att "file" atts ;
              lv_atts = remove_common_view_atts atts ;
            }
  | Ed_xml.E ((("","paned"),atts),l) ->
      let o =
        match string_att "orientation" atts with
          "vertical" -> `VERTICAL
        | "horizontal" -> `HORIZONTAL
        | s -> failwith ("Invalid orientation: "^s)
      in
      let p = int_att "position" atts in
      let children =
        match l with
          e1 :: e2 :: _ ->
            (layout_contents_of_xml e1,
             layout_contents_of_xml e2)
        | _ -> failwith "Invalid children for paned"
      in
      `Paned { lp_orientation = o;
               lp_position = p;
               lp_children = children ;
             }

  | Ed_xml.E ((("","notebook"),_),l) ->
      `Notebook { ln_tabs = List.map layout_contents_of_xml l}
  | _ -> failwith "Invalid contents layout"

let layout_window_of_xml = function
    Ed_xml.E((("","window"),atts),l) ->
      let c =
        match l with
          [] -> None
        | c :: _ -> Some (layout_contents_of_xml c)
      in
      { lw_x = int_att "x" atts ;
        lw_y = int_att "y" atts ;
        lw_w = int_att "width" atts ;
        lw_h = int_att "height" atts ;
        lw_contents = c ;
      }
  | _ -> failwith "Invalid window layout"

let layout_of_xml = function
    Ed_xml.E ((("","layout"),_),l) ->
      List.map layout_window_of_xml l
  | _ -> failwith "Invalid layout"

let load_layout file =
  Ed_xml.read_xml_file file layout_of_xml

let rec layout_of_contents = function
    `View v ->
      `View { lv_kind = v#kind; lv_file = v#filename ; lv_atts = v#attributes }
  | `Paned p ->
      begin
        match p#child1, p#child2 with
        | None, None -> failwith "Bad paned layout"
        | None, Some x
        | Some x, None -> layout_of_contents x
        | Some a, Some b ->
            `Paned { lp_orientation = p#orientation ;
                     lp_position = p#position ;
                     lp_children = (layout_of_contents a, layout_of_contents b) ;
                   }
      end
  | `Notebook n ->
      `Notebook { ln_tabs = List.map (fun (_,c) -> layout_of_contents c) n#tabs }

let layout_of_window w =
  { lw_x = w#x ;
    lw_y = w#y ;
    lw_w = w#width ;
    lw_h = w#height ;
    lw_contents =
      match w#contents with
        None -> None
      | Some c -> Some (layout_of_contents c);
  }

let layout_of_windows = List.map layout_of_window

let rec contents_of_layout topwin = function
    `View lv ->
      begin
        let factory = lv.lv_kind in
        let v =
          let attributes = lv.lv_atts in
          match Ed_view.factory_open_file topwin ~factory None ~attributes lv.lv_file with
            `New_view v | `Use_view v -> v
        in
        Ed_gui.init_view topwin v;
        `View v
      end
  | `Paned lp ->
      let c1 = contents_of_layout topwin (fst lp.lp_children) in
      let c2 = contents_of_layout topwin (snd lp.lp_children) in
      let gp = new Ed_gui.gui_paned topwin lp.lp_orientation () in
      gp#set_one_child 1 c1;
      gp#set_one_child 2 c2;
      gp#set_position lp.lp_position;
      `Paned gp
  | `Notebook ln ->
      let l = List.map (contents_of_layout topwin) ln.ln_tabs in
      let gn = new Ed_gui.gui_notebook topwin () in
      List.iter (gn#add_tab None) l;
      `Notebook gn

let create_window_of_layout lw =
  let w = Ed_gui.create_window
      ~x: lw.lw_x ~y: lw.lw_y
      ~width: lw.lw_w ~height: lw.lw_h
      ()
  in
  let topwin = (w :> Ed_view.topwin) in
  match lw.lw_contents with
    None -> ()
  | Some lc ->
      let c = contents_of_layout topwin lc in
      match c with
        `View v ->
          w#add_view v;
          Ed_gui.init_view topwin v;
          v#grab_focus
      | `Paned _
      | `Notebook _ -> w#set_contents (Some c)


let create_windows_of_layout = List.iter create_window_of_layout

let _ =
  let f args =
    let file =
      if Array.length args > 0 then
        args.(1)
      else
        !layout_file
    in
    let layout = layout_of_windows !Ed_gui.gui_windows in
    store_layout file layout
  in
  let com =
    { Ed_commands.com_name = "store_layout" ;
      Ed_commands.com_args = [| "file" |] ;
      Ed_commands.com_more_args = None ;
      Ed_commands.com_f = f;
    }
  in
  Ed_commands.register com
