type 'a sv

external init : string array -> unit = "ml_Perl_init"
external fini : unit -> unit = "ml_Perl_fini"

let init = init Sys.argv
let fini = fun () ->
  (* Make sure all SVs are killed before destroying the
   * Perl interpreter. *)
  Gc.full_major ();
  fini ();
;;

external undef		: unit		-> unit sv	= "ml_Perl_undef"
let undef = undef ()

external sv_of_value	: 'a		-> 'a sv	= "ml_Perl_sv_of_value"
external sv_of_char	: char		-> char sv	= "ml_Perl_sv_of_char"
external sv_of_bool	: bool		-> bool sv	= "ml_Perl_sv_of_bool"
external sv_of_string	: string	-> string sv	= "ml_Perl_sv_of_string"
external sv_of_float	: float		-> float sv	= "ml_Perl_sv_of_float"
external sv_of_int	: int		-> int sv	= "ml_Perl_sv_of_int"
external sv_of_nativeint: nativeint	-> nativeint sv	= "ml_Perl_sv_of_nativeint"
external sv_of_int32	: int32		-> int32 sv	= "ml_Perl_sv_of_int32"
external sv_of_int64	: int64		-> int64 sv	= "ml_Perl_sv_of_int64"

external value_of_sv	: 'a sv		-> 'a		= "ml_Perl_value_of_sv"
external char_of_sv	: char sv	-> char		= "ml_Perl_char_of_sv"
external bool_of_sv	: bool sv	-> bool		= "ml_Perl_bool_of_sv"
external string_of_sv	: string sv	-> string	= "ml_Perl_string_of_sv"
external float_of_sv	: float sv	-> float	= "ml_Perl_float_of_sv"
external int_of_sv	: int sv	-> int		= "ml_Perl_int_of_sv"
external nativeint_of_sv: nativeint sv	-> nativeint	= "ml_Perl_nativeint_of_sv"
external int32_of_sv	: int32 sv	-> int32	= "ml_Perl_int32_of_sv"
external int64_of_sv	: int64 sv	-> int64	= "ml_Perl_int64_of_sv"


external call : string -> 'a sv list -> 'b sv list = "ml_Perl_call"

external sv_of_fun1 : ('a sv -> 'b sv) -> ('a -> 'b) sv = "ml_Perl_sv_of_fun1"


module Types : sig

  type 'a fn

  val int : int fn
  val string : string fn
  val float : float fn

  val ( @-> ) : 'a fn -> 'b fn -> ('a -> 'b) fn

  (*val foreign : string -> ('a -> 'b) fn -> 'a -> 'b*)

end = struct

  type _ fn =
    | Function : 'a fn * 'b fn -> ('a -> 'b) fn
    | Int
    | Float
    | String

  let int = Int
  let float = Float
  let string = String

  let ( @-> ) a b = Function (a, b)

  (*external foreign : string -> ('a -> 'b) fn -> 'a -> 'b = "ml_Perl_invoke"*)

end


(*let say = Types.(foreign "say" (string @-> int))*)

let say msg =
  match call "say" [sv_of_string msg] with
  | results -> List.map string_of_sv results


let test_invoke fn =
  let fn =
    sv_of_fun1 (fun s ->
      sv_of_string (fn (string_of_sv s))
    )
  in

  List.map string_of_sv (call "test_invoke" [fn]);
