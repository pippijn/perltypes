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

external undef		: unit		-> 'a sv	= "ml_Perl_undef"

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

  val make_type : ('a -> 'a sv) -> ('a sv -> 'a) -> 'a fn

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
    | Function  : 'a fn * 'b fn -> ('a -> 'b) fn
    | Tuple     : 'a fn * 'b fn -> ('a * 'b) fn
    | Primitive : ('a -> 'a sv) * ('a sv -> 'a) -> 'a fn

  type _ sp =
    | Nil : unit sp
    | Cons : 'a sv * 'b sp -> ('a -> 'b) sp


  let make_type to_sv sv_to =
    Primitive (to_sv, sv_to)

  let char	= make_type sv_of_char		char_of_sv
  let bool	= make_type sv_of_bool		bool_of_sv
  let string	= make_type sv_of_string	string_of_sv
  let float	= make_type sv_of_float		float_of_sv
  let int	= make_type sv_of_int		int_of_sv
  let nativeint	= make_type sv_of_nativeint	nativeint_of_sv
  let int32	= make_type sv_of_int32		int32_of_sv
  let int64	= make_type sv_of_int64		int64_of_sv

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



(********************************************************************
 *
 *                              TEST CODE
 *
 ********************************************************************)


(* old stuff not supported by new stuff yet (tuple returns) *)
let stuff msg =
  List.map string_of_sv
    (call "stuff" [sv_of_string msg])


(* new stuff using Foreign *)
let say =
  Foreign.(foreign "say" (string @-> float @-> int @-> int))

let make_invoke1 typ =
  Foreign.(foreign "test_invoke1" ((typ @-> typ) @-> typ @-> typ))

let test_invoke1 = make_invoke1 Foreign.string

let test_invoke2 =
  Foreign.(foreign "test_invoke2" ((string @-> string @-> string) @-> string))


let stress_test () =
  print_endline "starting stress-test";

  let continue = ref true in
  let i = ref 0 in

  let invoke_char = make_invoke1 Foreign.char in
  let invoke_bool = make_invoke1 Foreign.bool in
  let invoke_string = make_invoke1 Foreign.string in
  let invoke_float = make_invoke1 Foreign.float in
  let invoke_int = make_invoke1 Foreign.int in
  let invoke_nativeint = make_invoke1 Foreign.nativeint in
  let invoke_int32 = make_invoke1 Foreign.int32 in
  let invoke_int64 = make_invoke1 Foreign.int64 in

  let n64 = Nativeint.of_int 64 in
  let n65 = Nativeint.of_int 65 in

  let start = Unix.gettimeofday () in
  while !continue do
    (*Gc.compact ();*)

    let n = 1250 in

    let local_start = Unix.gettimeofday () in
    for j = 1 to n do
      assert (invoke_char Char.uppercase 'a' = 'A');
      assert (invoke_bool not true = false);
      assert (invoke_string (fun x -> "hello " ^ x) "world" = "hello world");
      assert (invoke_float ((+.) 64.0) 64.0 = 128.0);
      assert (invoke_int ((+) 64) 64 = 128);
      assert (invoke_nativeint Nativeint.succ n64 = n65);
      assert (invoke_int32 Int32.succ 64l = 65l);
      assert (invoke_int64 Int64.succ 64L = 65L);
    done;

    incr i;

    let finish = Unix.gettimeofday () in

    let call_count = !i * n * 8 in
    let total_time = finish -. start in

    Printf.printf "\r[+%.06fs, %.06fs] %d calls (avg: %.06fs)"
      (finish -. local_start)
      total_time
      call_count
      (total_time /. float call_count);
    flush stdout;

    continue := total_time < 60.0;
  done;
;;


let test () =
  for i = 0 to 100 do
    assert (int_of_sv (sv_of_int 200) = 200);
    assert (string_of_sv (sv_of_string "hello") = "hello");

    let foo = (1, "Hello") in
    let foosv = sv_of_value foo in
    Gc.compact ();
    assert (value_of_sv foosv == foo);

    ignore (sv_of_fun1 1 (fun x -> x));
  done;

  List.iter (Printf.printf "got: %s\n")
    (stuff "hello");

  Printf.printf "say returned %d\n"
    (say "hello" 123.456 3);

  Printf.printf "closure call (1) returned: %s\n"
    (test_invoke1 (fun x ->
      "hello " ^ x
    ) "world");

  begin try
    Printf.printf "closure call (1) returned: %s\n"
      (test_invoke1 (fun x ->
        failwith ("thrown from closure: hello " ^ x)
      ) "world");
    assert false;
  with Failure msg ->
    print_endline ("Failure: " ^ msg);
  end;

  Printf.printf "closure call (2) returned: %s\n"
    (test_invoke2 (fun x y ->
      "hello " ^ x ^ ", " ^ y
    ));

  stress_test ();
;;
