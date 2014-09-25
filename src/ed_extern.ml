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

(** *)

(*c==v=[String.chop_n_char]=1.0====*)
let chop_n_char n s =
  let len = String.length s in
  if len <= n +1 || n < 0 then
    s
  else
    Printf.sprintf "%s..." (String.sub s 0 (n+1))
(*/c==v=[String.chop_n_char]=1.0====*)

(*c==v=[List.list_remove_doubles]=1.0====*)
let list_remove_doubles ?(pred=(=)) l =
  List.fold_left
    (fun acc e -> if List.exists (pred e) acc then acc else e :: acc)
    []
    (List.rev l)
(*/c==v=[List.list_remove_doubles]=1.0====*)

(*c==v=[File.subdirs]=0.2====*)
let subdirs path =
  let d = Unix.opendir path in
  let rec iter acc =
    let file =
      try Some (Unix.readdir d)
      with End_of_file -> Unix.closedir d; None
    in
    match file with
    | None -> List.rev acc
    | Some s when
          s = Filename.current_dir_name ||
          s = Filename.parent_dir_name -> iter acc
    | Some file ->
        let complete_f = Filename.concat path file in
	match
	  try Some (Unix.stat complete_f).Unix.st_kind
	  with _ -> None
	with
	  Some Unix.S_DIR -> iter (complete_f :: acc)
	| None | Some _ -> iter acc
  in
  iter []
(*/c==v=[File.subdirs]=0.2====*)

(*c==v=[String.replace_in_string]=1.0====*)
let replace_in_string ~pat ~subs ~s =
  let len_pat = String.length pat in
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter pos =
    if pos >= len then
      ()
    else
      if pos + len_pat > len then
        Buffer.add_string b (String.sub s pos (len - pos))
      else
        if String.sub s pos len_pat = pat then
          (
           Buffer.add_string b subs;
           iter (pos+len_pat)
          )
        else
          (
           Buffer.add_char b s.[pos];
           iter (pos+1);
          )
  in
  iter 0;
  Buffer.contents b
(*/c==v=[String.replace_in_string]=1.0====*)

(*c==v=[File.string_of_file]=1.1====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_subbytes buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.1====*)
(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

(*c==v=[Misc.try_finalize]=1.0====*)
let try_finalize f x finally y =
  let res =
    try f x
    with exn -> finally y; raise exn
  in
  finally y;
  res
(*/c==v=[Misc.try_finalize]=1.0====*)

(*c==v=[String.no_blanks]=1.0====*)
let no_blanks s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      ' ' | '\n' | '\t' | '\r' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
(*/c==v=[String.no_blanks]=1.0====*)

(*c==v=[String.split_string]=1.2====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> if keep_empty then [""] else []
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
(*/c==v=[String.split_string]=1.2====*)

(*c==v=[List.make_list]=1.0====*)
let make_list n ele =
  let rec f acc n =
    if n > 0 then f (ele :: acc) (n-1) else acc
  in
  f [] n
(*/c==v=[List.make_list]=1.0====*)

(*c==v=[File.safe_remove_file]=1.0====*)
let safe_remove_file file =
  try Sys.remove file
  with Sys_error _ -> ()
(*/c==v=[File.safe_remove_file]=1.0====*)

