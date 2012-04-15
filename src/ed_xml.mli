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
  @strip can be used to strip blanks (see Xmlm documentation for details).
     @raise Failure in case of error. *)
val xml_of_string : ?strip: bool -> string -> t

(** [read_xml_file file f] reads the file to get an xml tree
     and applies [f] on the xml tree. *)
val read_xml_file : ?strip: bool -> string -> (t -> 'a) -> 'a

