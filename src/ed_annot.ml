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

(** Getting information in ocaml-generated .annot files. *)

let filename_re = "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\""
let number_re = "\\([0-9]*\\)"
let position_re = Printf.sprintf "%s %s %s %s"
  filename_re number_re number_re number_re
let s_location_re = Printf.sprintf "^%s %s" position_re position_re

let location_re = Str.regexp s_location_re

type loc = int * int
  (** absolute position of start and end of type annotation in the .annot file *)

type ident_kind =
  | Int_ref of string * loc
  | Ext_ref of string
  | Def of string
;;

type annot_kind =
  | Type of string Lazy.t
  | Ident of ident_kind
  | Call of [`Tail | `Stack]

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

let ext_ref_re = "ext_ref \\(.*$\\)";;
let int_ref_re = Printf.sprintf "int_ref \\([a-zA-Z0-9'_.]+\\) %s %s" position_re position_re;;
let def_re = "def \\([a-zA-Z0-9'_.]+\\)"

let ident_info_of_string s =
  try
    try
      let re = Str.regexp int_ref_re in
      ignore(Str.search_forward re s 0);
      let start = int_of_string (Str.matched_group 6 s) in
      let stop = int_of_string (Str.matched_group 11 s) in
      let name = Str.matched_group 1 s in
      Int_ref (name, (start, stop))
    with
      Not_found ->
        try
          let re = Str.regexp ext_ref_re in
          ignore (Str.search_forward re s 0);
          Ext_ref (Str.matched_group 1 s)
        with
          Not_found ->
            try
              let re = Str.regexp def_re in
              ignore (Str.search_forward re s 0);
              Def (Str.matched_group 1 s)
            with Not_found -> Def ""
  with
  e -> Def (Printexc.to_string e)
;;

let annot_kind_of_string ~start ~stop annot_string = function
  "type" ->
    Type
      (Lazy.lazy_from_fun
       (fun () -> String.sub annot_string start (stop-start))
      )
| "ident" ->
    Ident (ident_info_of_string (String.sub annot_string start (stop-start)))
| "call" ->
    begin
      let k =
        match no_blanks (String.sub annot_string start (stop-start)) with
          "tail" -> `Tail
        | "stack" -> `Stack
        | s -> failwith ("Unknown call value \""^s^"\"")
      in
      Call k
    end
| s -> failwith ("Unknown annotation kind "^s)
;;

let annot_re = Str.regexp "^\\([a-z]+\\)(\n\  \\(\\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)"


type tree = {
    t_pos_left : int ;
    t_pos_right : int ;
    t_kind : annot_kind option ;
    t_children : tree list;
  }

let add_node acc ~left ~right ~kind =
  match acc with
    [] ->
      let t =
        { t_pos_left = left;
          t_pos_right = right;
          t_kind = Some kind ;
          t_children = [] ;
        }
      in
      [ t ]
  | l ->
      let rec find_children acc = function
        [] -> (List.rev acc, [])
      | h :: q ->
          if h.t_pos_right < left then
            (* no more children *)
            (List.rev acc, h ::q)
          else
            find_children (h::acc) q
      in
      let (children, others) = find_children [] l in
      let t =
        { t_pos_left = left ;
          t_pos_right = right ;
          t_kind = Some kind;
          t_children = children ;
        }
      in
      t :: others

let cut_by_locations annot_string =
  let rec iter acc pos =
    match
      try Some (Str.search_forward location_re annot_string pos)
      with Not_found -> None
    with
      None -> List.rev acc
    | Some start ->
        let stop = Str.match_end () in
        let left = int_of_string (Str.matched_group 5 annot_string) in
        let right = int_of_string (Str.matched_group 10 annot_string) in
        iter ((start, stop, left, right) :: acc) stop
  in
  iter [] 0
;;

let get_annots annot_string trees ~left ~right pos_start pos_end =
  let rec iter trees pos =
    match
      try Some (Str.search_forward annot_re annot_string pos)
      with Not_found -> None
    with
      None -> List.rev trees
    | Some pos ->
        match pos_end with
          Some p when p < pos -> trees
        | _ ->
            let start = Str.group_beginning 2 in
            let stop = Str.group_end 2 in
            let newp = Str.match_end () in
            let kind = annot_kind_of_string
              ~start ~stop annot_string
              (Str.matched_group 1 annot_string)
            in
            let new_trees = add_node trees ~left ~right ~kind in
            iter new_trees newp
  in
  iter trees pos_start
;;

let build_tree annot_string =
  let locs = cut_by_locations annot_string in
  let rec iter trees = function
    [] -> trees
  | [(_,stop,left,right)] ->
      get_annots annot_string trees ~left ~right stop None
  | (_,pos_start,left,right)::(((pos_end,_,_,_) :: _) as q) ->
      let t = get_annots annot_string
        trees ~left ~right pos_start (Some pos_end)
      in
      iter t q
  in
  (** the list of trees is supposed to be sorted, left-most first, and inner first
     because the list of annotation is ordered that way in the .annot file *)
  match iter [] locs with
    [t] -> Some t
  | [] -> None
  | l ->
      let t = {
          t_pos_left = (List.hd l).t_pos_left ;
          t_pos_right = (List.hd (List.rev l)).t_pos_right ;
          t_kind = None ;
          t_children = l;
        }
      in
      Some t

let search_in_tree kind =
  let pred pos t =
    t.t_pos_left <= pos && pos <= t.t_pos_right
  in
  let get_t pos l =
    try Some (List.find (pred pos) l)
    with Not_found -> None
  in
  let rec iter fallback pos tree =
    if pred pos tree then
      let fb =
        match tree.t_kind with
          None -> fallback
        | Some (Type _ as t) when kind = `Type ->
            Some (tree.t_pos_left, tree.t_pos_right, t)
        | Some (Ident _ as t) when kind = `Ident ->
            Some (tree.t_pos_left, tree.t_pos_right, t)
        | Some ((Call _) as t) when kind = `Call ->
            Some (tree.t_pos_left, tree.t_pos_right, t)
        | Some _ -> fallback
      in
      match get_t pos tree.t_children with
        None -> fb
      | Some t -> iter fb pos t
    else
      fallback
  in
  iter None
;;

let rec fold f acc tree =
  List.fold_left (fold f)
    (f acc (tree.t_pos_left, tree.t_pos_right, tree.t_kind))
    tree.t_children
;;