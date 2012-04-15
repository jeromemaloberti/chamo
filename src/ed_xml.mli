(** *)

type t =
    E of Xmlm.tag * t list
  | D of string
;;

(** Get a string to represent the given xml tree.
  @decl when set to [false], do not output xml heading declaration. Default is [true]
*)
val string_of_xml : ?decl: bool -> t -> string

(** Parse a string to build a XML tree.
     @raise Failure in case of error. *)
val xml_of_string : string -> t

(** [read_xml_file file f] reads the file to get an xml tree
     and applies [f] on the xml tree. *)
val read_xml_file : string -> (t -> 'a) -> 'a

