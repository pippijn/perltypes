type sv

external init : string array -> unit = "ml_Perl_init"
let init = init Sys.argv

external sv_of_char	: char		-> sv		= "ml_Perl_sv_of_char"
external sv_of_bool	: bool		-> sv		= "ml_Perl_sv_of_bool"
external sv_of_string	: string	-> sv		= "ml_Perl_sv_of_string"
external sv_of_float	: float		-> sv		= "ml_Perl_sv_of_float"
external sv_of_int	: int		-> sv		= "ml_Perl_sv_of_int"
external sv_of_int32	: int32		-> sv		= "ml_Perl_sv_of_int32"
external sv_of_int64	: int64		-> sv		= "ml_Perl_sv_of_int64"

external char_of_sv	: sv		-> char		= "ml_Perl_char_of_sv"
external bool_of_sv	: sv		-> bool		= "ml_Perl_bool_of_sv"
external string_of_sv	: sv		-> string	= "ml_Perl_string_of_sv"
external float_of_sv	: sv		-> float	= "ml_Perl_float_of_sv"
external int_of_sv	: sv		-> int		= "ml_Perl_int_of_sv"
external int32_of_sv	: sv		-> int32	= "ml_Perl_int32_of_sv"
external int64_of_sv	: sv		-> int64	= "ml_Perl_int64_of_sv"


module Types : sig

  type 'a typ
  type 'a fn

  val int : int typ
  val string : string typ
  val float : float typ

  val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn

end = struct

  type _ typ = [
    | `Int
    | `Float
    | `String
  ]

  type _ fn =
    | Returns : 'a typ -> 'a fn
    | Function : 'a typ * 'b fn -> ('a -> 'b) fn

  let int = `Int
  let float = `Float
  let string = `String

  let ( @-> ) a b = Function (a, b)

  let returning a = Returns (a)

end
