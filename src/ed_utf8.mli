(** UTF8 functions. *)

(** [utf8_index_of_char string n] returns the position of the first byte
     the [n]th character in the given UTF-8 [string].
     @raise Not_found if there is no such character.*)
val utf8_index_of_char : string -> int -> int

(** [utf8_char_of_index string i] returns the number of characters
     found from the beginning of the UTF-8 string to position [i] (included).
     @raise Invalid_argument if the given position is out of the string bounds.*)
val utf8_char_of_index : string -> int -> int

(** [utf8_string_length string] returns the number of utf8 characters in the
     given string. *)
val utf8_string_length : string -> int

(** [utf8_char_of_code code] returns the string representing the UTF-8 character
  with the given [code].
  @raise Failure if the code is out of range. Only 4 bytes UTF-8 is supported by now.
  *)
val utf8_char_of_code : int -> string
