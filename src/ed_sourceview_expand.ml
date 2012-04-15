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

type expand_context =
  { buffer : string ;
    mutable pos : int ; (* position by character index *)
    searched_rex : Pcre.regexp ;
    searched_pattern : string ;
    mutable prev_prop : string * int ;
    (* previous proposition, buffer * pos *)
    mutable prev_inserted : string option ;
    (* optional string inserted
       (so that we can remove it when we propose another one *)
    mutable already_proposed : string list ;
  }

let context = ref (None : expand_context option)

let create_context buffer pos pat rex =
  { buffer = buffer ;
    pos = pos ;
    searched_rex = rex ;
    searched_pattern = pat ;
    prev_prop = (buffer, pos) ;
    prev_inserted = None ;
    already_proposed = [];
  }

let search_in_buffer forward (buffer : Ed_sourceview.my_buffer) start stop rex =
  match buffer#re_search forward ~start ~stop rex with
    None -> raise Not_found
  | Some (start, stop) ->
    let pos = start#offset in
    let text = buffer#get_text ~start ~stop () in
    (pos, text)

let rec get_next_proposition_in_buffer c buffer =
  let b = (Ed_sourceview.get_buffer_by_name buffer)#buffer in
  let rex = c.searched_rex in
  let (_,prevpos) = c.prev_prop in
  if buffer = c.buffer then
    if prevpos > c.pos then
      search_in_buffer true b (b#get_iter (`OFFSET (prevpos+1))) b#end_iter rex
    else
      try search_in_buffer false b b#start_iter (b#get_iter (`OFFSET prevpos)) rex
      with Not_found ->
          search_in_buffer true b (b#get_iter (`OFFSET c.pos)) b#end_iter rex
  else
    search_in_buffer true b (b#get_iter (`OFFSET (prevpos+1))) b#end_iter rex

let get_next_buffer_in_history b =
  let rec iter = function
    [] | [_] -> None
  | name :: h :: q ->
      if name = b then Some h else iter (h :: q)
  in
  iter !Ed_sourceview.buffer_name_history

let rec get_next_proposition c =
(*
  let msg = Printf.sprintf "get_next_proposition:\nbuffer=%s, pos=%d, searched=%s, prev_prop=(%s, %d)\nalready_proposed=%s"
    c.buffer c.pos c.searched_pattern (fst c.prev_prop) (snd c.prev_prop)
    (String.concat ";" c.already_proposed)
  in
  prerr_endline msg;
*)
  let buf = fst c.prev_prop in
  let res =
    try Some (get_next_proposition_in_buffer c buf)
    with Not_found ->
        None
  in
  match res with
    None ->
      begin
        match get_next_buffer_in_history buf with
        | Some buf ->
            c.prev_prop <- (buf, 0);
            get_next_proposition c
        | None ->
            match c.already_proposed with
              [] -> None
            | l ->
                c.already_proposed <- [List.hd (List.rev l)];
                c.prev_prop <- (c.buffer, c.pos) ;
                Some (true, List.hd (List.rev l))
      end
  | Some (pos, text) ->
     c.prev_prop <- (buf, pos);
     if List.mem text c.already_proposed then
       get_next_proposition c
     else
       begin
         c.already_proposed <- text :: c.already_proposed;
        Some (false, text)
       end

let get_pattern (v : Ed_sourceview.sourceview) it =
  let stop = it in
  let start = it#backward_word_start in
  let word = v#file#buffer#get_text ~start ~stop () in
  let qword = Pcre.quote word in
  let re =
    match v#file#mode with
      None -> Ed_sourceview_rc.default_word_re
    | Some m -> m#word_re
  in
  (word, Pcre.regexp (Printf.sprintf "%s%s" qword re))

let expand (v : Ed_sourceview.sourceview) args =
  let f = v#file in
  let b = f#buffer in
  let cur_iter = b#get_iter `INSERT in
  let pos = cur_iter#offset in
  let ctx =
    match !context with
      None ->
        let (pat,rex) = get_pattern v cur_iter in
        let c = create_context f#name pos pat rex in
        context := Some c;
        c
    | Some c ->
        if (not (Ed_commands.same_previous_command ())) or
          c.buffer <> f#name or c.pos <> pos
        then
          (
           let (pat, rex) = get_pattern v cur_iter in
           let c = create_context f#name pos pat rex in
           context := Some c;
           c
          )
        else
          c
  in
  match get_next_proposition ctx with
    None ->
      Ed_misc.warning_message "No expansion found."
  | Some (cycle, s) ->
      if cycle then
        Ed_misc.warning_message "No more expansion found, restarting from beginning.";
      (* eventually remove the previous string inserted *)
      let insert_iter =
        let s_to_remove =
          match ctx.prev_inserted with
            None -> ctx.searched_pattern
          | Some s -> s
        in
        (* we're supposed to be at the end of the (previously inserted) text *)
        let stop = cur_iter in
        let len = Ed_misc.utf8_string_length s_to_remove in
        let start = stop#backward_chars len in
        b#delete ~start ~stop;
        b#get_iter (`OFFSET (pos - len))
      in
      b#place_cursor ~where: insert_iter;
      b#insert s;
      let pos = (b#get_iter `INSERT)#offset in
      ctx.pos <- pos;
      ctx.prev_inserted <- Some s
;;
Ed_sourceview.register_com
  ~prefix: Ed_sourceview_rc.factory_name "expand" [||] expand;;
      