open_out "testsuite.rst";;

let () =
  match Sys.argv with
  | [|_; "-depend"|] -> ()
  | _ ->
      Perl.test ();
      Perl.fini ();
;;
