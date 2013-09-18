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


module Foreign : sig

  type 'a typ
  type 'a fn

  val int : int typ
  val string : string typ
  val float : float typ

  val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
  val returns : 'a typ -> 'a fn

  val ( @* ) : 'a typ -> 'b typ -> ('a * 'b) typ

  val foreign : string -> ('a -> 'b) fn -> 'a -> 'b

end = struct

  type _ typ =
    | Tuple : 'a typ * 'b typ -> ('a * 'b) typ
    | Int : int typ
    | Float : float typ
    | String : string typ

  type _ fn =
    | Returns : 'a typ -> 'a fn
    | Function : 'a typ * 'b fn -> ('a -> 'b) fn

  let int = Int
  let float = Float
  let string = String

  let ( @-> ) a b = Function (a, b)
  let returns a = Returns (a)

  let ( @* ) a b = Tuple (a, b)

  let rec print_typ : type a. unit -> a typ -> string = fun () -> function
    | Tuple (a, b) ->
        Printf.sprintf "Tuple (%a, %a)"
          print_typ a print_typ b
    | Int -> "Int"
    | Float -> "Float"
    | String -> "String"

  let rec print_fn : type a. unit -> a fn -> string = fun () -> function
    | Returns (a) ->
        Printf.sprintf "Returns (%a)"
          print_typ a
    | Function (a, b) ->
        Printf.sprintf "Function (%a, %a)"
          print_typ a print_fn b


  let rec to_sv : type a. a typ -> a -> a sv = function
    | Tuple (a, b) -> failwith "Tuple"
    | Int -> sv_of_int
    | Float -> sv_of_float
    | String -> sv_of_string


  let sv_to : type a. a typ -> a sv -> a = function
    | Tuple (a, b) -> failwith "Tuple"
    | Int -> int_of_sv
    | Float -> float_of_sv
    | String -> string_of_sv


  let unsafe_dump_sp : type sp. sp -> string list =
  fun sp ->
    let rec unsafe_dump args sp =
      if Obj.magic sp == () then
        args
      else
        let (sv, sp) = Obj.magic sp in
        unsafe_dump (string_of_sv (Obj.magic sv) :: args) sp
    in
    unsafe_dump [] sp


  let rec invoke : type a sp. sp -> string -> a typ -> a =
  fun sp name ret ->
    Printf.printf "call %s (%s)\n"
      name (String.concat ", " (unsafe_dump_sp sp));

    (* call function here, convert return value to ocaml value *)
    match call name (List.rev (Obj.magic sp)) with
    | [result] ->
        sv_to ret result
    | _ -> assert false


  let rec foreign : type a sp. sp -> string -> a fn -> a =
  fun sp name -> function
    | Returns (ret) ->
        invoke sp name ret
    | Function (arg, ret) ->
        fun a ->
          let sp = (to_sv arg a, sp) in
          foreign sp name ret

  let foreign : type a b. string -> (a -> b) fn -> a -> b =
  fun name signature ->
    foreign () name signature

end


let say = Foreign.(foreign "say" (string @-> int @-> returns int));;
Printf.printf "return %d\n" (say "hello" 3);;

let say msg =
  match call "stuff" [sv_of_string msg] with
  | results -> List.map string_of_sv results


let test_invoke fn =
  let fn =
    sv_of_fun1 (fun s ->
      sv_of_string (fn (string_of_sv s))
    )
  in

  List.map string_of_sv (call "test_invoke" [fn]);
