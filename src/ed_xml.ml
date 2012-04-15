(** Utilities to use XML files. *)

type t =
    E of Xmlm.tag * t list
  | D of string

let string_of_xml ?decl tree =
  try
    let b = Buffer.create 256 in
    let output = Xmlm.make_output ?decl (`Buffer b) in
    let frag = function
    | E (tag, childs) -> `El (tag, childs)
    | D d -> `Data d
    in
    Xmlm.output_doc_tree frag output (None, tree);
     Buffer.contents b
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
;;

let xml_of_string s =
  try
    let input = Xmlm.make_input ~enc: (Some `UTF_8) (`String (0, s)) in
    let el tag childs = E (tag, childs)  in
    let data d = D d in
    let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s\n"
        line col (Xmlm.error_message error)
      in
      failwith msg
  | Invalid_argument e ->
      let msg = Printf.sprintf "%s:\n%s" e s in
      failwith msg
;;

let read_xml_file file f =
  let error s = failwith (Printf.sprintf "File %s: %s" file s) in
  try
    f (xml_of_string (Ed_extern.string_of_file file))
  with
    Failure s -> error s
;;
