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

{
open Lexing

let prerr_endline _ = ()

type token =
    AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0
  | INFIXOP1
  | INFIXOP2
  | INFIXOP3
  | INFIXOP4
  | INHERIT
  | INITIALIZER
  | INT | INT32 | INT64 | NATIVEINT
  | LABEL of string
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETGREATER
  | LBRACKETLESS
  | LESS
  | LESSMINUS
  | LET
  | LIDENT
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of string
  | OR
  | PARSER
  | PLUS
  | PREFIXOP
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING
  | STRUCT
  | SUBTRACTIVE
  | THEN
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
  | COMMENT
  | EOL
  | EOFCOMMENT
  | EOFSTRING
  | ERROR
(* for lexers *)
  | RULE
  | PARSE

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Keyword_as_label of string
  | Literal_overflow of string
  | End_with_no_begin of string
;;

(** start and stop positions in characters *)
type location = int * int

let curr_loc lexbuf =
  (lexbuf.Lexing.lex_start_p.Lexing.pos_cnum,
   lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum)

exception Error of error * location

(* The table of keywords *)

let token_kw =
  [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3 ;
    "land", INFIXOP3 ;
    "lor", INFIXOP3 ;
    "lxor", INFIXOP3 ;
    "lsl", INFIXOP4 ;
    "lsr", INFIXOP4 ;
    "asr", INFIXOP4 ;
    ]

let keyword_table = Hashtbl.create 149
let _ = List.iter
    (fun (k,v) -> Hashtbl.add keyword_table k v)
    token_kw

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
(*  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) && not (in_comment ())
  then raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                    curr_loc lexbuf))
  else Char.chr c
*)
  'x'
let char_for_hexadecimal_code lexbuf i =
(*
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)
*)
  'x'

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(* Error report *)

let report_error = function
  | Illegal_character c ->
      Printf.sprintf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      Printf.sprintf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment ->
      Printf.sprintf "Comment not terminated"
  | Unterminated_string ->
      Printf.sprintf "String literal not terminated"
  | Keyword_as_label kwd ->
      Printf.sprintf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      Printf.sprintf "Integer literal exceeds the range of representable integers of type %s" ty
  | End_with_no_begin s ->
      Printf.sprintf "The closing '%s' has no matching open equivalent" s
;;


(* Block stack *)

let blocks = ref []

type cst_indent = {
    ind_newline : int ;
    ind_bracket : int ;
    ind_brace : int ;
    ind_parent : int ;
    ind_let : int ;
    ind_begin : int ;
    ind_match : int ;
    ind_comment : int ;
    ind_if : int ;
    ind_fun : int ;
    ind_struct : int ;
    ind_object : int ;
    ind_class : int ;
    ind_module : int ;
    ind_type : int ;
    ind_exception : int ;
    ind_loop : int;
    ind_field : int ;
    ind_val : int;
  }

let default_indent = {
  ind_newline = 2 ;
  ind_bracket = 2 ;
  ind_brace = 2 ;
  ind_parent = 1 ;
  ind_let = 2 ;
  ind_begin = 2 ;
  ind_match = 2 ;
  ind_comment = 3 ;
  ind_if = 2 ;
  ind_fun = 2 ;
  ind_struct = 2 ;
  ind_object = 2 ;
  ind_class = 2 ;
  ind_module = 2 ;
  ind_type = 2 ;
  ind_exception = 2 ;
  ind_loop = 2;
  ind_field = 2 ;
  ind_val = 2;
}
let cst_indent = ref default_indent

(* indentation for each line, in revert-order;
   Each time we encounter the first token of a line, we compute its
   indentation and add it to the list. [None] means that we should not
   touch the line (orelse this could change the blanks/tabs on the line).*)
let line_indentations = ref ([] : int option list)

let next_token_is_first = ref true
let next_line_is_more_indented = ref 0

(** Ad this indentation to the line_indentation list if we're on the
   first token of the line *)
let if_first_token_on_line n =
  if !next_token_is_first then
    (
     line_indentations := (Some n) :: !line_indentations;
     next_token_is_first := false
    )

let if_first_token_on_line_set_none () =
  if !next_token_is_first then
    (
     line_indentations := None :: !line_indentations;
     next_token_is_first := false
    )

let cur_indent = ref 0

let begin_comment_indentation = ref (!cur_indent, !next_line_is_more_indented)

let set_indent ?(touch_next_line=true) n =
  cur_indent := n ;
  if touch_next_line=true then
    next_line_is_more_indented := 0
let inc_indent ?(touch_next_line=true) n =
  cur_indent := !cur_indent + n;
  if touch_next_line=true then
    next_line_is_more_indented := 0
let dec_indent ?(touch_next_line=true) n =
  cur_indent := !cur_indent - n;
  if touch_next_line=true then
    next_line_is_more_indented := 0

let string_of_token =
  let l = List.map (fun (a,b) -> (b,a)) token_kw in
  fun t ->
    try List.assoc t l
    with Not_found -> "<other>"

let prerr_blocks_stack () =
  prerr_endline "Block stack is: ";
  List.iter
    (fun (tok,vext,vin) ->
      prerr_endline (Printf.sprintf "%s (%d, %+d)" (string_of_token tok) vext vin))
    !blocks

let last_popped = ref None
let rec pop tokens s lexbuf =
  match !blocks with
    [] ->
      prerr_blocks_stack ();
      raise (Error (End_with_no_begin s, curr_loc lexbuf))
  | (tok, vext, vin) :: q ->
      blocks := q;
      last_popped := Some (tok,vext,vin);
      if List.mem tok tokens then
        (
         prerr_endline (Printf.sprintf "popped %s (%d;%d)" (string_of_token tok) vext vin);
         (vext, vin)
        )
      else
        match tok with
          MATCH | TRY | FUN | FUNCTION | FUNCTOR | MINUSGREATER | MODULE
        | WITH | METHOD | VAL | ELSE | THEN | INCLUDE | LET
        | EXCEPTION | TYPE | INITIALIZER ->
             (* we can pop these ones and try with the next token
                because these contructions don't have an explicit end; *)
            pop tokens s lexbuf
        | _ ->
             (* we could jump the bad token to try to indent anyway
                but the result would be wrong *)
            prerr_blocks_stack ();
            raise (Error (End_with_no_begin s, curr_loc lexbuf))

let last_begin_let_indent () =
  try
    let (_,v,off) = List.find (fun (t,_,_) -> t=LET) !blocks in
    Some (v, off)
  with Not_found -> None

let last_and_associated_indent () =
  try
    let (t,vext,vin) =
      List.find
        (fun (t,_,_) -> t=LET or t=MODULE or t=CLASS or t=TYPE)
        !blocks
    in
    Some (vext,vin)
  with Not_found -> None

let last_begin_module_indent () =
  try
    let (_,v,off) = List.find (fun (t,_,_) -> t=MODULE) !blocks in
    Some (v, off)
  with Not_found -> None

let last_begin_struct_sig_indent () =
  try
    let (_,v,off) = List.find (fun (t,_,_) -> t=STRUCT or t=SIG) !blocks in
    Some (v,off)
  with Not_found -> None

let last_begin_object_indent () =
  try
    let (_,v,off) = List.find (fun (t,_,_) -> t=OBJECT) !blocks in
    Some (v,off)
  with Not_found -> None

let last_begin_sig_object_indent () =
  try
    let (_,v,off) = List.find (fun (t,_,_) -> t=OBJECT or t=SIG) !blocks in
    Some (v,off)
  with Not_found -> None

let push tok vext vin = blocks := (tok, vext, vin) :: !blocks

let push_if_different tok v off =
  match !blocks with
    (t,_,_) :: _ when t = tok -> false
  | _ -> push tok v off; true

let begin_tokens_of_token = function
    END -> [ BEGIN ; OBJECT ; SIG ; STRUCT ]
  | IN -> [ LET ]
  | DONE -> [ DO ]
  | DO -> [ WHILE ; FOR ]
  | RPAREN -> [LPAREN]
  | RBRACE -> [LBRACE]
  | RBRACKET -> [LBRACKET; LBRACKETLESS; LBRACKETGREATER]
  | BARRBRACKET -> [LBRACKETBAR]
  | GREATERRBRACE -> [LBRACELESS]
  | GREATERRBRACKET -> [LBRACKETLESS]
  | WITH -> [MATCH;TRY;LBRACE]
  | THEN -> [IF]
  | ELSE -> [THEN]
  | OBJECT ->  [CLASS]
  | STRUCT | SIG -> [MODULE]
  | _ -> assert false

let last_block_indent () =
  match !blocks with
    (_,n,off) :: _ -> (n, off)
  | _ -> (0, 0)

let last_block_inner_indent () =
  let (n,off) = last_block_indent () in n + off

let nl_info_stack = Stack.create ()
let push_nl_info () =
  Stack.push !next_line_is_more_indented nl_info_stack
let pop_nl_info () =
  try next_line_is_more_indented := Stack.pop nl_info_stack
  with Stack.Empty -> ()

let on_par_open token lexbuf =
  let cst = match token with
    LPAREN -> !cst_indent.ind_parent
  | LBRACE -> !cst_indent.ind_brace
  | LBRACKET
  | LBRACKETLESS
  | LBRACKETGREATER -> !cst_indent.ind_bracket
  | _ -> !cst_indent.ind_parent
  in
  if_first_token_on_line !cur_indent;
  (* keep the indentation of this block *)
  push_nl_info ();
  push token !cur_indent cst;
  (* then increment current indentation for lines in the parenthesis *)
(*
  let pos_on_line =
    let loc = lexbuf.Lexing.lex_start_p in
    loc.Lexing.pos_cnum - loc.Lexing.pos_bol
  in
*)
  inc_indent cst

let on_par_close lexbuf token s =
  set_indent ~touch_next_line: false
    (fst (pop (begin_tokens_of_token token) s lexbuf));
(*  next_line_is_more_indented := 1;*)
  if_first_token_on_line !cur_indent;
  pop_nl_info ()

let rec on_keyword lexbuf = function
    AND ->
      (
       match last_and_associated_indent () with
         Some (n, off) ->
           set_indent (n+off);
           if_first_token_on_line n
       | None -> if_first_token_on_line !cur_indent
      )
  | AS
  | ASSERT -> if_first_token_on_line !cur_indent
  | BAR ->
      (* can be associated to a WITH, a LBRACKET or a FUNCTION *)
      (
       match !blocks with
         (WITH,n,_) :: _ ->
           if_first_token_on_line n;
       | (LBRACKET,n,_) :: _ ->
           if_first_token_on_line n;
       | (FUNCTION,n,_) :: _ ->
           if_first_token_on_line n;
       | (ELSE,_,_) :: q
       | (THEN,_,_) :: q ->
           (* A bar is the end of a then or else if we're in a match/try *)
           blocks := q;
           on_keyword lexbuf BAR
       | _ ->
           if_first_token_on_line !cur_indent
      )
  | (BEGIN | STRUCT | SIG | OBJECT) as token ->
      if token = BEGIN then push_nl_info ();
      if_first_token_on_line !cur_indent;
      let cst =
        match token with
          BEGIN -> !cst_indent.ind_begin
        | STRUCT | SIG -> !cst_indent.ind_struct
        | OBJECT -> !cst_indent.ind_object
        | _ -> assert false
      in
      push token !cur_indent cst;
      inc_indent cst
  | (CLASS | MODULE | CONSTRAINT | INCLUDE) as token ->
      (
       let ind =
         match token with
           CLASS -> !cst_indent.ind_class
         | _ -> !cst_indent.ind_module
       in
       match last_begin_struct_sig_indent () with
         None ->
           if_first_token_on_line 0;
           push token 0 ind;
           set_indent ind
       | Some (n, off) ->
           let n = n + off in
           if_first_token_on_line n;
           push token n ind;
           set_indent (n + ind);
      );
      prerr_endline "CLASS";
      prerr_blocks_stack()
  | DO ->
      (
       set_indent (fst (pop (begin_tokens_of_token DO) "do" lexbuf)) ;
       if_first_token_on_line !cur_indent;
       push DO !cur_indent !cst_indent.ind_loop;
       inc_indent !cst_indent.ind_loop
      )
  | DONE ->
      set_indent (fst (pop (begin_tokens_of_token DONE) "done" lexbuf));
      if_first_token_on_line !cur_indent
  | DOWNTO ->
      if_first_token_on_line !cur_indent
  | THEN ->
      set_indent (fst (pop (begin_tokens_of_token THEN) "then" lexbuf));
      push THEN !cur_indent !cst_indent.ind_if;
      if_first_token_on_line !cur_indent;
      inc_indent !cst_indent.ind_if
  | ELSE ->
      set_indent (fst (pop (begin_tokens_of_token ELSE) "else" lexbuf));
      push ELSE !cur_indent !cst_indent.ind_if;
      if_first_token_on_line !cur_indent;
      inc_indent !cst_indent.ind_if;
  | END ->
      (
       (* if it is the end of a sig, a struct or an object, then
          we must pop also the previous module or class *)
       prerr_blocks_stack();
       let (t,n,off) =
         ignore(pop (begin_tokens_of_token END) "end" lexbuf);
         match !last_popped with
           None -> assert false
         | Some info -> info
       in
       if_first_token_on_line n;
       match t with
         OBJECT | SIG | STRUCT ->
           set_indent (fst (last_block_indent ()))
       | _ ->
           set_indent ~touch_next_line: false n;
           pop_nl_info ();
      )

  | (EXCEPTION | EXTERNAL) as token ->
      (
       match last_begin_struct_sig_indent () with
         None ->
           if_first_token_on_line 0;
           set_indent !cst_indent.ind_exception
       | Some (n, off) ->
           let n = n + off in
           if_first_token_on_line n;
           push token n !cst_indent.ind_exception;
           set_indent (n + !cst_indent.ind_exception);
      )
  | FALSE | TRUE ->
      if_first_token_on_line !cur_indent
  | FOR ->
      if_first_token_on_line !cur_indent;
      push FOR !cur_indent 0;
  | FUN ->
      if !next_token_is_first then
        (
         if_first_token_on_line !cur_indent;
         push FUN !cur_indent !cst_indent.ind_fun;
         inc_indent !cst_indent.ind_fun
        )
      else
        (
         (* to prevent incrementing various times for sequences
            of fun ... -> fun ... -> on a same line *)
         match !blocks with
           (LET, n, _) :: _
         | (METHOD, n, _) :: _
         | (VAL, n, _) :: _ ->
             ()
         | _ ->
             if push_if_different FUN !cur_indent !cst_indent.ind_fun then
               inc_indent !cst_indent.ind_fun
        )
  | FUNCTION ->
      if !next_token_is_first then
        (
         if_first_token_on_line !cur_indent;
         push FUNCTION !cur_indent !cst_indent.ind_fun;
         inc_indent !cst_indent.ind_fun
        )
      else
        (
         match !blocks with
           (LET, n, _) :: _
         | (METHOD, n, _) :: _
         | (VAL, n, _) :: _ ->
             push FUNCTION n (2 * !cst_indent.ind_fun);
             set_indent (n + !cst_indent.ind_fun)
         | _ ->
             push FUNCTION !cur_indent (2 * !cst_indent.ind_fun);
             inc_indent (2 * !cst_indent.ind_fun)
        )
  | FUNCTOR ->
      if_first_token_on_line !cur_indent;
      (* to prevent incrementing various times for sequences
         of functor ... -> functor ... -> *)
      if push_if_different FUNCTOR !cur_indent !cst_indent.ind_fun then
        inc_indent !cst_indent.ind_fun
  | IF ->
      let p = last_block_inner_indent () in
      if_first_token_on_line p;
      push IF p !cst_indent.ind_if;
      set_indent (p + !cst_indent.ind_if)
  | IN ->
      set_indent (fst (pop (begin_tokens_of_token IN) "in" lexbuf));
      if_first_token_on_line !cur_indent;
  | (INHERIT | INITIALIZER | METHOD) as token  ->
      (
       match last_begin_object_indent () with
         None ->
           if_first_token_on_line !cur_indent;
           if token = METHOD or token = INITIALIZER then
             push token !cur_indent !cst_indent.ind_field;
           inc_indent !cst_indent.ind_field;
       | Some (n,off) ->
           if_first_token_on_line (n+off);
           if token = METHOD or token = INITIALIZER then
             push token (n + off) !cst_indent.ind_field;
           set_indent (n + off + !cst_indent.ind_field);
      )
  | VIRTUAL ->
      if_first_token_on_line !cur_indent
  | VAL ->
       (* same heuristic as for LET *)
      if !next_token_is_first then
        (
         let loc = lexbuf.Lexing.lex_start_p in
         let pos_on_line = loc.Lexing.pos_cnum - loc.Lexing.pos_bol in
         let (top_align,off) =
           match last_begin_sig_object_indent () with
             None -> (0, 0)
           | Some (n, off) -> (n, off)
          in
         let pos =
           if top_align + off >= pos_on_line then
             top_align + off
            else
             !cur_indent
         in
         if_first_token_on_line pos;
         push VAL pos !cst_indent.ind_val;
         set_indent (pos + !cst_indent.ind_val)
        )
      else
        (
         push VAL !cur_indent !cst_indent.ind_val;
         inc_indent !cst_indent.ind_val
        )
  | LAZY ->
      if_first_token_on_line !cur_indent

  | LET ->
      (* heuristic: if the let is at the beginning of a line
         with at most n blanks before, with n being the current
         struct indentation, then consider it is a "top let" and
         align it on the other elements of the struct *)
      if !next_token_is_first then
        (
         let loc = lexbuf.Lexing.lex_start_p in
         let pos_on_line = loc.Lexing.pos_cnum - loc.Lexing.pos_bol in
         let (top_align, off) =
           match last_begin_struct_sig_indent () with
             None -> (0, 0)
           | Some (n, off) -> (n, off)
         in
         let pos =
           if top_align + off >= pos_on_line then
             top_align + off
           else
             last_block_inner_indent ()
         in
         if_first_token_on_line pos;
         push LET pos !cst_indent.ind_let;
         set_indent (pos + !cst_indent.ind_let)
        )
      else
        (
         push LET !cur_indent !cst_indent.ind_let;
         inc_indent !cst_indent.ind_let
        )
  | (MATCH | TRY) as token ->
      if_first_token_on_line !cur_indent;
      push token !cur_indent !cst_indent.ind_match;
      inc_indent !cst_indent.ind_match
  | MINUSGREATER ->
      (
       if_first_token_on_line !cur_indent;
       (* can be associated to FUN, WITH and FUNCTOR ...
          or nothing. *)
       try
         match !blocks with
           (FUN,_,_) :: _
         | (FUNCTOR,_,_) :: _ ->
             ()
         | (FUNCTION,n,_) :: _ ->
             set_indent (n + 2 * !cst_indent.ind_fun)
         | (WITH,n,_) :: _ ->
             set_indent (n + 2 * !cst_indent.ind_match)
         | _ ->
             ()
       with
         Not_found ->
           inc_indent !cst_indent.ind_fun
      )
  | MUTABLE ->
      if_first_token_on_line !cur_indent;
  | NEW ->
      if_first_token_on_line !cur_indent;
  | OF | OPEN | OR | PRIVATE | REC | TO ->
      if_first_token_on_line !cur_indent;
  | TYPE ->
      (
       match !blocks with
         (MODULE,_,_) :: _
       | (CLASS,_,_) :: _ ->
           (* a module/class type, indentation has already been done
              when we encounterd MODULE/CLASS; do nothing *)
           prerr_endline "nothing to be done for type";
           prerr_blocks_stack ()
       | _ ->
           match last_begin_struct_sig_indent () with
             None ->
               if_first_token_on_line 0;
               set_indent !cst_indent.ind_type
           | Some (n, off) ->
               if_first_token_on_line (n + off);
               push TYPE (n + off) !cst_indent.ind_type;
               set_indent (n + off + !cst_indent.ind_type)
      )
  | WHEN ->
      if_first_token_on_line !cur_indent;
  | WHILE ->
      if_first_token_on_line !cur_indent;
      push WHILE !cur_indent !cst_indent.ind_loop;
      inc_indent !cst_indent.ind_loop
  | WITH ->
      (
       let (n,_) = pop (begin_tokens_of_token WITH) "with" lexbuf in
       match !last_popped with
         Some (LBRACE,n,off) ->
           set_indent (n + off);
           if_first_token_on_line !cur_indent;
           push LBRACE n off
       | Some (MATCH,n,off) | Some (TRY,n,off) ->
           set_indent n;
           if_first_token_on_line !cur_indent;
           push WITH n (2 * !cst_indent.ind_match);
           inc_indent !cst_indent.ind_match
       | _ ->
           if_first_token_on_line n
      )
  | INFIXOP0 | INFIXOP1 | INFIXOP2 | INFIXOP3 | INFIXOP4 ->
      if_first_token_on_line !cur_indent

  | SEMI ->
      (
       match !blocks with
         (t,n,off) :: q ->
           let indent =
             match t with
               WITH | FUNCTION -> n + off
             | ELSE | THEN ->
                 blocks := q;
                 n
             | _ -> n + off
           in
           set_indent indent;
           if_first_token_on_line !cur_indent;
       | _ ->
           if_first_token_on_line !cur_indent;
      )
  | COMMA ->
      (
       match !blocks with
         (t,n,off) :: q ->
           let indent = n + off in
           set_indent indent;
           if_first_token_on_line !cur_indent;
       | _ ->
           if_first_token_on_line !cur_indent;
      )
  | _ ->
      if_first_token_on_line !cur_indent
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        (* increment only if this is not an empty line *)
        if (not !next_token_is_first) then
          if !next_line_is_more_indented > 0 &&
            (!cur_indent <= last_block_inner_indent ())
          then
            cur_indent := !cur_indent + !cst_indent.ind_newline
          else
            incr next_line_is_more_indented;
        if_first_token_on_line 0;
        next_token_is_first := true;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "_"
      { if_first_token_on_line !cur_indent; token lexbuf }
  | "~" { if_first_token_on_line !cur_indent; token lexbuf }
  | "~" lowercase identchar * ':'
      { if_first_token_on_line !cur_indent;
        let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, curr_loc lexbuf));
        token lexbuf }
  | "?"  { if_first_token_on_line !cur_indent; token lexbuf }
  | "??" { if_first_token_on_line !cur_indent; token lexbuf }
  | "?" lowercase identchar * ':'
      {
        let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, curr_loc lexbuf));
        if_first_token_on_line !cur_indent;
        token lexbuf }
  | lowercase identchar *
      {
        let s = Lexing.lexeme lexbuf in
        try
          let kw = Hashtbl.find keyword_table s in
          on_keyword lexbuf kw;
          token lexbuf
        with Not_found ->
          if_first_token_on_line !cur_indent;
          token lexbuf
      }
  | uppercase identchar *
      {
        if_first_token_on_line !cur_indent;
        token lexbuf}       (* No capitalized keywords *)
  | int_literal
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | float_literal
      { if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | int_literal "l"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | int_literal "L"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | int_literal "n"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | "\""
      {
        if_first_token_on_line !cur_indent;
        string lexbuf;
        token lexbuf
      }
  | "'" newline "'"
      {
        if_first_token_on_line !cur_indent;
        next_token_is_first := true;
        if_first_token_on_line 0;
        update_loc lexbuf None 1 false 1;
        token lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      {
        if_first_token_on_line !cur_indent;
        token lexbuf
      }
  | "'\\" _
      {
        if_first_token_on_line !cur_indent;
        let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, curr_loc lexbuf))
      }
  | "(*"
      {
        begin_comment_indentation := (!cur_indent, !next_line_is_more_indented);
        if_first_token_on_line !cur_indent;
        comment lexbuf;
        token lexbuf }
  | "(*)"
      {
        if_first_token_on_line !cur_indent;
        comment lexbuf;
        token lexbuf
      }
  | "*)"
      {
        if_first_token_on_line !cur_indent;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        token lexbuf
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      {
        if_first_token_on_line 0;
        update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | "#"  { if_first_token_on_line !cur_indent; token lexbuf }
  | "&"  { if_first_token_on_line !cur_indent; token lexbuf }
  | "&&" { if_first_token_on_line !cur_indent; token lexbuf }
  | "`"  { if_first_token_on_line !cur_indent; token lexbuf }
  | "'"  { if_first_token_on_line !cur_indent; token lexbuf }
  | "("  {
           on_par_open LPAREN lexbuf;
           token lexbuf
         }
  | ")"  {
           on_par_close lexbuf RPAREN ")";
           token lexbuf }
  | "*"  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ","  { on_keyword lexbuf COMMA;
           token lexbuf }
  | "->" { if_first_token_on_line !cur_indent;
           on_keyword lexbuf MINUSGREATER;
           token lexbuf }
  | "."  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ".." { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ":"  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "::" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ":=" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ":>" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | ";"  { on_keyword lexbuf SEMI;
           token lexbuf }
  | ";;" {
           set_indent 0;
           if_first_token_on_line !cur_indent;
           token lexbuf }
  | "<"  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "<-" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "="  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "["  { on_par_open LBRACKET lexbuf;
           token lexbuf }
  | "[|" { on_par_open LBRACKETBAR lexbuf ;
           token lexbuf }
  | "[<" { on_par_open LBRACKETLESS lexbuf ;
           token lexbuf }
  | "[>" { on_par_open LBRACKETGREATER lexbuf;
           token lexbuf }
  | "]"  { on_par_close lexbuf RBRACKET "]";
           token lexbuf }
  | "{"  { on_par_open LBRACE lexbuf ;
           token lexbuf }
  | "{<" { on_par_open LBRACELESS lexbuf ;
           token lexbuf }
  | "|"  {
           on_keyword lexbuf BAR;
           token lexbuf }
  | "||" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "|]" { on_par_close lexbuf BARRBRACKET "|]";
           token lexbuf }
  | ">"  { token lexbuf }
  | ">]" { on_par_close lexbuf GREATERRBRACKET ">]";
           token lexbuf }
  | "}"  { on_par_close lexbuf RBRACE "}";
           token lexbuf }
  | ">}" { on_par_close lexbuf GREATERRBRACE ">}";
           token lexbuf }
  | "!=" { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "+"  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "-"  { if_first_token_on_line !cur_indent;
           token lexbuf }
  | "-." { if_first_token_on_line !cur_indent;
           token lexbuf }

  | "!" symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | ['~' '?'] symbolchar +
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | ['@' '^'] symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | ['+' '-'] symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | "**" symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | ['*' '/' '%'] symbolchar *
            { if_first_token_on_line !cur_indent;
              token lexbuf }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     curr_loc lexbuf))
      }

and comment = parse
  | "*)"
      {
        if_first_token_on_line !cur_indent;
        let indent, next_indented = !begin_comment_indentation in
        cur_indent := indent;
        next_line_is_more_indented := next_indented - 1
          (* usually, non-debug comments end a line *)
      }
  | newline
      {
        if_first_token_on_line 0;
        next_token_is_first := true;
        update_loc lexbuf None 1 false 1;
        comment lexbuf
      }
  | eof
      { raise (Error (Unterminated_comment, curr_loc lexbuf)) }
  | _
      {
        if_first_token_on_line (!cur_indent + !cst_indent.ind_comment);
        comment lexbuf }

and string = parse
  | "\\\""
      { if_first_token_on_line_set_none () ;
        string lexbuf
      }
  | '"'
      { if_first_token_on_line_set_none () }
  | newline
      {
        if_first_token_on_line_set_none () ;
        next_token_is_first := true;
        update_loc lexbuf None 1 false 1;
        string lexbuf
      }
  | eof
      { raise (Error (Unterminated_string, curr_loc lexbuf)) }
  | _
      {
        if_first_token_on_line_set_none ();
        string lexbuf }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }

{
let get_lines_indentation ?(indent_spec=default_indent) s =
  let lexbuf = Lexing.from_string s in
  blocks := [];
  line_indentations := [];
  next_token_is_first := true;
  set_indent 0;
  cst_indent := indent_spec;
  try ignore(token lexbuf);
      `Success (List.rev !line_indentations);
  with Error (e,loc) -> `Failure (e,loc,(List.rev !line_indentations))

}
