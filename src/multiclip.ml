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

type multiclip = {
  abst_len : int ;
  elts : (string, string) Hashtbl.t ;
  }

let default_abst_len = 80;;

let create_multiclip ?(abst_len=default_abst_len) () =
  { abst_len = abst_len ;
    elts = Hashtbl.create 111 ;
  }
;;
(*c==v=[String.chop_n_char]=1.0====*)
let chop_n_char n s =
  let len = String.length s in
  if len <= n +1 || n < 0 then
    s
  else
    Printf.sprintf "%s..." (String.sub s 0 (n+1))
(*/c==v=[String.chop_n_char]=1.0====*)

let find_free_abstract t prefix =
  let rec iter n =
    let s = Printf.sprintf "%s%s"
      prefix
      (if n = 0 then "" else Printf.sprintf "#%d" n)
    in
    match
      try ignore(Hashtbl.find t.elts s); None
      with Not_found -> Some s
    with
      Some s -> s
    | None -> iter (n + 1)
  in
  iter 0
;;
(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)

let replace_blanks s =
  let l = split_string s ['\n';'\r';'\t';' '] in
  String.concat " " l
;;

let add t ?abs contents =
  let abstract =
    match abs with
      None ->
        let prefix = replace_blanks (chop_n_char t.abst_len contents) in
        find_free_abstract t prefix
    | Some s -> replace_blanks s
  in
  Hashtbl.replace t.elts abstract contents
;;

let elements t = Hashtbl.fold (fun abstract elt acc -> (abstract, elt) :: acc) t.elts [] ;;
let remove t abstract = Hashtbl.remove t.elts abstract ;;

type storable_multiclip =
  { mutable clip : multiclip ;
    file : string ;
    op_group : Config_file.group ;
    op_abst_len : Config_file.int_cp ;
    op_elts : (string * string) Config_file.list_cp ;
  }

let create_storable_multiclip ?(abst_len=default_abst_len) file =
  let group = new Config_file.group in
  let op_elts = new Config_file.list_cp
    (Config_file.tuple2_wrappers
     Config_file.string_wrappers
       Config_file.string_wrappers)
      ["elements"]
      ~group
      []
      ""
  in
  let op_abst_len = new Config_file.int_cp ~group ["abstract_length"] abst_len "" in
  let clip = create_multiclip ~abst_len () in
  { clip = clip ;
    file = file ;
    op_group = group ;
    op_abst_len = op_abst_len ;
    op_elts = op_elts ;
  }

let read_multiclip t =
  t.op_group#read t.file;
  let clip = create_multiclip ~abst_len: t.op_abst_len#get () in
  List.iter
    (fun (abs, contents) -> add clip ~abs contents)
    t.op_elts#get;
  t.clip <- clip
;;

let write_multiclip t =
  t.op_elts#set (elements t.clip);
  t.op_abst_len#set t.clip.abst_len ;
  t.op_group#write t.file
;;

let storable_get_multiclip t = t.clip;;
let storable_get_file t = t.file;;


