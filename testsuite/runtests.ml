open_out "testsuite.rst";;

let test () =
  let open Perl in

  assert (int_of_sv (sv_of_int 200) = 200);
  assert (string_of_sv (sv_of_string "hello") = "hello");

  let foo = (1, "Hello") in
  let foosv = sv_of_value foo in
  Gc.compact ();
  assert (value_of_sv foosv == foo);

  List.iter (Printf.printf "got: %s\n")
    (stuff "hello");
;;


let () =
  match Sys.argv with
  | [|_; "-depend"|] -> ()
  | _ ->
      test ();
      Perl.fini ();
;;
