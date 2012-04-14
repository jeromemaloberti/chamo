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

(* $Id: cam_commands.ml 758 2011-01-13 07:53:27Z zoggy $ *)

type command = string array -> unit

type command_desc =
    { com_name : string ;
      com_args : string array ;
      com_more_args : string option ;
      com_f : command ;
    }

let commands = Hashtbl.create 937

let get_com ?(table=commands) name = Hashtbl.find table name

let get_com_or_fail ?table com =
  try get_com ?table com
  with Not_found ->
    failwith (Printf.sprintf "Command %s not available." com)

let register ?(table=commands) ?(replace=false) com =
  try
    ignore (Hashtbl.find table com.com_name);
    if replace then
      Hashtbl.replace table com.com_name com
    else
      failwith (Printf.sprintf "Command %s already registered." com.com_name)
  with
    Not_found ->
      Hashtbl.add table com.com_name com

let register_before ?table com =
  try
    let prev = get_com ?table com.com_name in
    let new_com =
      { com with
        com_f = (fun args -> com.com_f args; prev.com_f args) ;
      }
    in
    register ?table ~replace: true new_com
  with
    Not_found -> register ?table com

let register_after ?table com =
  try
    let prev = get_com ?table com.com_name in
    let new_com =
      { com with
        com_f = (fun args -> prev.com_f args; com.com_f args) ;
      }
    in
    register ?table ~replace: true new_com
  with
    Not_found -> register ?table com

let unit_com name f =
  { com_name = name ;
    com_args = [| |] ;
    com_more_args = None ;
    com_f = (fun _ -> f ()) ;
  }

let create_com name ?more args f =
  { com_name = name ;
    com_args = args ;
    com_more_args = more ;
    com_f = f ;
  }


let string_of_char c = String.make 1 c
let concat char string =
  string_of_char char ^ string

let rec parse_squote = parser
    | [< ''\'' >] -> ""
    | [< ''\\'; 'c; word = parse_squote >] ->
        concat c word
    | [< 'c; word = parse_squote >] ->
        concat c word
    | [< >] -> failwith "squote"

let rec parse_dquote = parser
    | [< ''"' >] -> ""
    | [< ''\\'; 'c; word = parse_dquote >] ->
        concat c word
    | [< 'c; word = parse_dquote >] ->
        concat c word
    | [< >] -> failwith "dquote"

let rec parse_noquote = parser
  | [< '' ' >] -> ""
  | [< ''\\'; 'c; word = parse_noquote >] ->
      concat c word
  | [< ''"'; subword = parse_dquote; word = parse_noquote >] ->
      subword ^ word
  | [< ''\''; subword = parse_squote; word = parse_noquote >] ->
      subword ^ word
  | [< 'c; word = parse_noquote >] ->
      concat c word
  | [< >] -> ""


let rec parse_words = parser
    [< '' '; words = parse_words >] ->
      words
  | [< ''"'; word = parse_dquote; words = parse_words >] ->
      word :: words
  | [< ''\''; word = parse_squote; words = parse_words >] ->
      word :: words
  | [< ''\\'; 'c; word = parse_noquote; words = parse_words >] ->
      concat c word :: words
  | [< 'c; word = parse_noquote; words = parse_words >] ->
      concat c word :: words
    | [< >] -> []


let list_of_string s =
  parse_words (Stream.of_string s)

let string_to_argv s = Array.of_list (list_of_string s)
let argv_to_string a =
  String.concat " " (Array.to_list (Array.map Filename.quote a))

let prev_command = ref ""
let same_previous_command = ref false
let launch_command ?(history=true) ?table com args =
  try
    let com = get_com_or_fail ?table com in
    let s_com = Printf.sprintf "%s %s" com.com_name (argv_to_string args) in
    try
      same_previous_command := s_com = !prev_command;
      if history then Ed_com_history.add s_com;
      Ed_hooks.display_message ~to_utf8: true
        (Printf.sprintf "Executing %s" s_com);
      com.com_f (Array.copy args);
      prev_command := s_com
    with
      e ->
        prev_command := s_com;
        raise e
  with
  | e ->
      let err =
        match e with
          Failure s | Sys_error s -> s
        | Invalid_argument s ->
            Printf.sprintf "Invalid_argument(\"%s\")" s
        | e -> Printexc.to_string e
      in
      Ed_hooks.error_message
	(Printf.sprintf "command %s: %s" com err)
let same_previous_command () = !same_previous_command

let ask_launch_command ?history ?table ?(width=300) com args =
  try
    let com = get_com_or_fail ?table com in
    let args =
      let lenv = Array.length args in
      let lend = Array.length com.com_args in
      let len = Array.length com.com_args +
	  (if com.com_more_args = None then 0 else 1)
      in
      Array.init len
	(fun i ->
	  if i >= lend then
	    let v =
	      let d = lenv - lend in
	      if d > 0 then
		let l = Array.to_list
		    (Array.map Filename.quote
		       (Array.sub args i d))
		in
		String.concat " " l
	      else
		""
	    in
	    match com.com_more_args with
	      None -> assert false
	    | Some s -> (s, v)
	  else
	    let v =
	      if i < lenv then
		args.(i)
	      else
		""
	    in
	    (com.com_args.(i), v)
      )
    in
    let f i (label, v) =
      Configwin.string ~f: (fun s -> args.(i) <- (label, s)) label v
    in
    let params = Array.to_list
	(Array.mapi f args)
    in
    match Configwin.simple_get com.com_name params with
      Configwin.Return_cancel -> ()
    | Configwin.Return_ok
    | Configwin.Return_apply ->
	let args =
	  let len = Array.length args in
	  let args = Array.map snd args in
	  match com.com_more_args with
	    None -> args
	  | Some _ ->
	      let s_args =
		(String.concat " "
		   (Array.to_list
		      (Array.map Filename.quote
			 (Array.sub args 0 (len - 1))))
		) ^ " " ^
		args.(len - 1)
	      in
	      string_to_argv s_args
	in
	launch_command ?history ?table com.com_name args
  with
    Failure s ->
      Ed_hooks.error_message s

let _external_command args =
  let len = Array.length args in
  if len < 1 then
    ()
  else
    let name = args.(0) in
    let params = Array.to_list
	(Array.map Filename.quote (Array.sub args 1 (len - 1)))
    in
    let com = Printf.sprintf "%s %s &"
	name
	(String.concat " " params)
    in
    ignore (Sys.command com)

let _ = register
    { com_name = "external" ;
      com_args = [| "program" |] ;
      com_more_args = Some "arguments" ;
      com_f = _external_command ;
    }

let _ask_command args =
  let len = Array.length args in
  if len < 1 then
    ()
  else
    let name = args.(0) in
    let params = Array.sub args 1 (len - 1) in
    ask_launch_command name params

let _ = register
    { com_name = "ask" ;
      com_args = [| |] ;
      com_more_args = Some "command and args" ;
      com_f = _ask_command ;
    }

let _system args =
  let len = Array.length args in
  if len < 1 then
    ()
  else
    let com = String.concat " "
	(Array.to_list (Array.map Filename.quote args))
    in
    ignore (Sys.command (com^" &"))

let _ = register
    { com_name = "system" ;
      com_args = [| |] ;
      com_more_args = Some "command and arguments" ;
      com_f = _system ;
    }

let eval_command ?history ?table com =
  let args = string_to_argv com in
  let len = Array.length args in
  if len < 1 then
    ()
  else
    let name = args.(0) in
    let params = Array.sub args 1 (len - 1) in
    launch_command ?history ?table name params

let _eval args =
  let com = String.concat " " (Array.to_list (Array.map Filename.quote args)) in
  eval_command com

let _ = register
    { com_name = "command" ;
      com_args = [| |] ;
      com_more_args = Some "command and arguments" ;
      com_f = _eval ;
    }

let available_command_names ?(table=commands) () =
  List.sort
    compare
    (Hashtbl.fold (fun k c acc -> k :: acc) table [])

(** {2 Global variables} *)

let global_variables = Hashtbl.create 691

let set_global = Hashtbl.replace global_variables
let get_global = Hashtbl.find global_variables
let safe_get_global name =
  try get_global name
  with Not_found -> ""

let _ =
  let f args =
    if Array.length args < 2
    then ()
    else set_global args.(0) args.(1)
  in
  register (create_com "set_global" [| "name" ; "value" |] f)