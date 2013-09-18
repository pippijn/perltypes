open_out "testsuite.rst";;

let test () =
  let open Perl in

  assert (int_of_sv (sv_of_int 200) = 200);
  assert (string_of_sv (sv_of_string "hello") = "hello");

  let foo = (1, "Hello") in
  assert (value_of_sv (sv_of_value foo) == foo);

  List.iter (Printf.printf "got: %s\n")
    (say "hello");

  List.iter (Printf.printf "got: %s\n")
    (test_invoke (fun s ->
      print_endline ("in closure: " ^ s);
      "hello " ^ s
    ));
;;


let () =
  match Sys.argv with
  | [|_; "-depend"|] -> ()
  | _ ->
      test ();
      Perl.fini ();
;;
