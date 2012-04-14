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

let fatal mes =
  prerr_endline mes;
  exit 1

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

(*c==v=[String.split_string]=1.0====*)
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
(*/c==v=[String.split_string]=1.0====*)


let remove_first_blanks s =
  let len = String.length s in
  try
    let rec first_no_blank p =
      if p < len then
        match s.[p] with
          ' ' | '\t' -> first_no_blank (p+1)
        | _ -> p
      else
        raise Not_found
    in
    let p = first_no_blank 0 in
    String.sub s p (len - p)
  with
    Not_found -> ""
;;
let main () =
  let file =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      fatal (Printf.sprintf
               "usage: %s <file>\nwill output the result of indentation of ocaml mode"
               Sys.argv.(0))
  in
  let code = string_of_file file in
  match Ed_ocaml_lexer.get_lines_indentation code with
    `Success line_indentations ->
      let lines = split_string ~keep_empty: true code ['\n'] in
      prerr_endline (Printf.sprintf "%d lines, %d indentations"
                       (List.length lines) (List.length line_indentations));
      List.iter2
        (fun line nopt ->
          match nopt with
            None -> print_endline line
          | Some n ->
              let s = remove_first_blanks line in
              Printf.printf "%s%s\n" (String.make n ' ') s
        )
        lines
        line_indentations
  | `Failure (e,(start,stop),_) ->
      prerr_endline (Printf.sprintf "chars %d-%d: %s" start stop (Ed_ocaml_lexer.report_error e))
;;


let g = fun x ->
  x
let f = fun x -> fun y -> fun z ->
  x + y

let f =
  fun x -> fun y -> fun z ->
    x + y

let g =
  fun x ->
    fun y -> fun z ->
      fun t ->
        x + y

let h =
  function
      1 -> "toto"
    | 2 -> "tutu"
    | _ -> "boo"

let _ =
  if true then
    (
     let _x = 3 in
     prerr_endline "bla bla";
     "toto"
    )
  else
    let _x = 1 in
    "coucou"

let _ =
  if true then
     let _x = 3 in (() ;
     prerr_endline "bla bla"; "foo")
  else
    let _x = 1 in
    "coucou"

let _ =
  if true then
    prerr_endline "coucou";
  prerr_endline "coucou";
  let _x = 1 in prerr_endline "coucou" ;
  if true then
    if false then
      prerr_endline "true/false"
    else
      prerr_endline "false/false"


let x : int = g
    1
    2
    3
    4

let f = function
    "" ->
      prerr_endline "coucou";
      let x = 1 in print_int x
  | _ ->
      prerr_endline "coucou"

let f x y
    z t =
  let u = x + y in
  print_int u;
  ()


let _ = main ()
