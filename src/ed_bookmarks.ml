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

(* $Id: cam_hooks.ml 600 2007-11-14 09:14:22Z zoggy $ *)

module CF = Config_file

type 'a bookmarks = {
  bk_table : (string, 'a) Hashtbl.t;
  bk_cf_group : CF.group ;
  bk_cf_cp : (string * 'a) list CF.cp ;
  }
;;

let create_from_cf_wrappers ?(desc="") wrappers =
  let group = new CF.group in
  let tuple2 = CF.tuple2_wrappers CF.string_wrappers wrappers in
  let cp = new CF.list_cp tuple2 ~group ["bookmarks"] [] desc in
  {
    bk_table = Hashtbl.create 101;
    bk_cf_group = group ;
    bk_cf_cp = cp ;
  }
;;


let create ?desc data_of_string string_of_data =
  let wrappers = {
      CF.to_raw = (fun a -> CF.string_wrappers.CF.to_raw (string_of_data a)) ;
      CF.of_raw = (fun s -> data_of_string (CF.string_wrappers.CF.of_raw s));
    }
  in
  create_from_cf_wrappers ?desc wrappers
;;

let get bk name = Hashtbl.find bk.bk_table name;;
let set bk name data = Hashtbl.replace bk.bk_table name data;;
let remove bk name = Hashtbl.remove bk.bk_table name;;

let list bk =
  Hashtbl.fold (fun name data acc -> (name, data) :: acc) bk.bk_table []
;;

let store bk file =
  let l = list bk in
  match l with
    [] -> (try Unix.unlink file with _ -> ())
  | _ ->
      bk.bk_cf_cp#set l;
      bk.bk_cf_group#write file
;;

let load bk file =
  Hashtbl.clear bk.bk_table;
  bk.bk_cf_group#read file;
  let f (name, data) = set bk name data in
  List.iter f bk.bk_cf_cp#get
;;

