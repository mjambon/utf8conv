(**
   Conversion of ISO-8859-1/Windows-1252 text into UTF-8.

   This is a minimalistic module that does not require any configuration file
   and does not depend on any third-party library.

   @param pos position of the beginning of the input substring.
              The default is 0, the beginning of the string.
   @param len length of the input substring.
              The default length extends until the end of the string.
*)

val is_ascii : ?pos: int -> ?len: int -> string -> bool
  (** Returns true if and only if the given string or substring
      contains only 7-bit characters. *)

val is_iso88591 : ?pos: int -> ?len: int -> string -> bool
  (** This function returns true if and only if the given string or substring
      contains only legal ISO-8859-1-encoded characters defined as the union
      of all 7-bit characters (0-127)
      and all 8-bit printable characters (160-255).

      ISO-8859-1 is the default encoding assumed by HTTP 1.1.
      However, it is often confused with Windows-1252 which is a superset
      of ISO-8859-1. Most HTTP user agents assume Windows-1252 as the
      default encoding because the encoding of any ISO-8859-1 character is
      also valid under Windows-1252.

      See also [is_windows1252].
  *)

val is_windows1252 : ?pos: int -> ?len: int -> string -> bool
  (** This function returns true if and only if the given string or substring
      contains only legal Windows-1252-encoded characters defined as the union
      of all 7-bit characters (0-127)
      and all 8-bit printable characters (160-255).

      Windows-1252 is also known as CP-1252. It adds a few printable
      characters to ISO-8859-1 in the 128-159 range.

      See also [is_iso88591].
  *)

val utf8_of_windows1252 :
  ?pos: int ->
  ?len: int ->
  ?undefined: (char -> string) ->
  string -> string
  (** Converts an ASCII, ISO-8859-1 or Windows-1252 string or substring
      into a UTF-8 string.

      @param undefined function for converting invalid bytes. The default
                       is to raise a [Failure] exception with a useful
                       error message.
  *)

val escape :
  ?pos: int ->
  ?len: int ->
  ?noquotes: bool ->
  string -> string
  (** Produces a valid OCaml string literal where non-printable and non-ASCII
      bytes are escaped using the hexadecimal notation
      except for the usual ['\n'], ['\t'] and ['\r'].
      [escape s] can be used as a substitute for [Printf.sprintf "%S" s]
      which uses the decimal notation in its escape sequences.

      @param noquotes omit leading and trailing double-quotes. Default: false.
  *)
