type 'a sv

external init : string array -> unit	= "ml_Perl_init"
external fini : unit -> unit		= "ml_Perl_fini"

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

external sv_of_fun1 : int -> ('a sv -> 'b sv) -> ('a -> 'b) sv = "ml_Perl_sv_of_fun"
external sv_of_fun2 : int -> ('a sv -> 'b sv -> 'c sv) -> ('a -> 'b -> 'c) sv = "ml_Perl_sv_of_fun"


module Foreign : sig

  type 'a fn

  val char	: char fn
  val bool	: bool fn
  val string	: string fn
  val float	: float fn
  val int	: int fn
  val nativeint	: nativeint fn
  val int32	: int32 fn
  val int64	: int64 fn

  val ( @-> ) : 'a fn -> 'b fn -> ('a -> 'b) fn
  val ( @* ) : 'a fn -> 'b fn -> ('a * 'b) fn

  val foreign : string -> ('a -> 'b) fn -> 'a -> 'b

end = struct

  type _ fn =
    | Function : 'a fn * 'b fn -> ('a -> 'b) fn
    | Tuple : 'a fn * 'b fn -> ('a * 'b) fn
    | Primitive : ('a -> 'a sv) * ('a sv -> 'a) -> 'a fn

  type _ sp =
    | Nil : unit sp
    | Cons : 'a sv * 'b sp -> ('a -> 'b) sp

  let char	= Primitive (sv_of_char,	char_of_sv	)
  let bool	= Primitive (sv_of_bool,	bool_of_sv	)
  let string	= Primitive (sv_of_string,	string_of_sv	)
  let float	= Primitive (sv_of_float,	float_of_sv	)
  let int	= Primitive (sv_of_int,		int_of_sv	)
  let nativeint	= Primitive (sv_of_nativeint,	nativeint_of_sv	)
  let int32	= Primitive (sv_of_int32,	int32_of_sv	)
  let int64	= Primitive (sv_of_int64,	int64_of_sv	)

  let ( @-> ) a b = Function (a, b)
  let ( @* ) a b = Tuple (a, b)

  let rec print_fn : type a. unit -> a fn -> string = fun () -> function
    | Function (a, b) ->
        Printf.sprintf "Function (%a, %a)"
          print_fn a print_fn b
    | Tuple (a, b) ->
        Printf.sprintf "Tuple (%a, %a)"
          print_fn a print_fn b
    | Primitive _ -> "Primitive"


  let sv_to : type a. a fn -> a sv -> a = function
    | Function (a, b) -> failwith "Function"
    | Tuple (a, b) -> failwith "Tuple"
    | Primitive (to_sv, sv_to) -> sv_to


  let rec to_sv : type a. a fn -> a -> a sv = function
    | Function (arg1, Function (arg2, ret)) ->
        fun fn ->
          sv_of_fun2 2 (fun a1 a2 ->
            to_sv ret (fn (sv_to arg1 a1) (sv_to arg2 a2))
          )
    | Function (arg1, ret) ->
        fun fn ->
          sv_of_fun1 1 (fun a1 ->
            to_sv ret (fn (sv_to arg1 a1))
          )
    | Tuple (a, b) -> failwith "Tuple"
    | Primitive (to_sv, sv_to) -> to_sv


  external call : string -> 'a sp -> 'b sp = "ml_Perl_call"

  let invoke sp name ret =
    let unsafe_dump_sp sp =
      List.rev_map string_of_sv (Obj.magic sp)
    in

    Printf.printf "call %s (%s)\n"
      name (String.concat ", " (unsafe_dump_sp sp));
    flush stdout;

    (* Call function here, convert return value to OCaml value. *)
    match call name sp with
    | Cons (result, Nil) ->
        sv_to ret result
    | sp ->
        failwith (
          Printf.sprintf "call unexpectedly returned (%s)"
            (String.concat ", " (unsafe_dump_sp sp))
        )


  let rec foreign : type a b. b sp -> string -> a fn -> a =
  fun sp name -> function
    | Function (arg, ret) ->
        fun a ->
          let sp = Cons (to_sv arg a, sp) in
          foreign sp name ret
    | ret ->
        invoke sp name ret

  let foreign name signature =
    foreign Nil name signature

end

(* old stuff *)
let stuff msg =
  match call "stuff" [sv_of_string msg] with
  | results -> List.map string_of_sv results


(* new stuff using Foreign *)
let say =
  Foreign.(foreign "say" (string @-> float @-> int @-> int))

let test_invoke1 =
  Foreign.(foreign "test_invoke1" ((string @-> string) @-> string))

let test_invoke2 =
  Foreign.(foreign "test_invoke2" ((string @-> string @-> string) @-> string))


let () =
  Printf.printf "say returned %d\n"
    (say "hello" 123.456 3);

  Printf.printf "closure call returned: %s\n"
    (test_invoke1 (fun x ->
      "hello " ^ x
    ));

  begin try
    Printf.printf "closure call returned: %s\n"
      (test_invoke1 (fun x ->
        failwith ("thrown from closure: hello " ^ x)
      ));
    assert false;
  with Failure msg ->
    print_endline ("Failure: " ^ msg);
  end;

  Printf.printf "closure call returned: %s\n"
    (test_invoke2 (fun x y ->
      "hello " ^ x ^ ", " ^ y
    ));
;;
