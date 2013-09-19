PerlTypes
=========

These are some bindings of OCaml and Perl in the style of `ctypes`.


Example
-------

Perl code:

```perl
sub say {
  print "$_: $_[0] and $_[1]\n"
     for 1 .. $_[2];
  300
}

sub stuff {
  print @_, "\n";
  (1, 2, '3 apples', @_)
}

sub test_invoke1 {
  $_[0]->('world')
}

sub test_invoke2 {
  $_[0]->('world')->('hehe')
}
```

OCaml code:

```ocaml
(* new stuff using Foreign *)
let say =
  Perl.Foreign.(foreign "say" (string @-> float @-> int @-> int))

let test_invoke1 =
  Perl.Foreign.(foreign "test_invoke1" ((string @-> string) @-> string))

let test_invoke2 =
  Perl.Foreign.(foreign "test_invoke2" ((string @-> string @-> string) @-> string))


let () =
  Printf.printf "say returned %d\n"
    (say "hello" 123.456 3);

  Printf.printf "closure call returned: %s\n"
    (test_invoke1 (fun x ->
      "hello " ^ x
    ));

  Printf.printf "closure call returned: %s\n"
    (test_invoke2 (fun x y ->
      "hello " ^ x ^ ", " ^ y
    ));
;;
```

Output:

```
call say (hello, 123.456, 3)
1: hello and 123.456
2: hello and 123.456
3: hello and 123.456
say returned 300
call test_invoke1 (CODE(0x906fe8))
closure call returned: hello world
call test_invoke2 (CODE(0x92dee0))
hello
closure call returned: hello world, hehe
```
