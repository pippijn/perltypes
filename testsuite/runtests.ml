open_out "testsuite.rst";;

Perl.init;;

let test () =
  let open Perl in
  assert (int_of_sv (sv_of_int 200) = 200);
  assert (string_of_sv (sv_of_string "hello") = "hello");
;;


let () =
  match Sys.argv with
  | [|_; "-depend"|] -> ()
  | _ -> test ()
