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

let output_name = "ocamltop";;
class ocamltop_output ?(on_destroy=fun()->()) name =
  object(self)
    inherit Ed_outputs.text_output ~on_destroy name as super

    initializer
      let lang = Gtksv_utils.source_language_manager#guess_language
         ~content_type: "text/x-ocaml"  ()
      in
      super#view#source_buffer#set_language lang;
      super#view#source_buffer#set_highlight_syntax true;
      super#view#source_buffer#set_highlight_matching_brackets true;
      ignore(super#view#connect#destroy
       (fun () -> Gtksv_utils.unregister_source_buffer super#view#source_buffer));
  end;;

let ocamltop_output = ref None;;
let ocamltop_output () =
  match !ocamltop_output with
    None ->
      let o = new ocamltop_output
        ~on_destroy: (fun () -> ocamltop_output := None)
          output_name
      in
      ocamltop_output := Some o ;
      o
  | Some o -> o
;;

let print_ocaml_output ?(output=ocamltop_output()) args =
  let outputs = Ed_outputs.outputs () in
  begin
    try ignore(outputs#output_by_name output#name)
    with Not_found ->
        outputs#add_output (output :> Ed_outputs.output);
  end;
  outputs#show output#name;
  ignore(output#insert (if Array.length args < 1 then "" else args.(0)))
;;

Ed_commands.register
  (Ed_commands.create_com "print_ocaml_output" [|"string"|]
   (print_ocaml_output ?output: None));;
