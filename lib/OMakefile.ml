install Library ".DEFAULT" [
  (* Target *)
  Name		"perl";
  Description	"OCaml Perl interface";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Interpreter";
    "Perl";
  ];

  Sources [
    "ml_Interpreter.cpp";
    "ml_Perl.cpp";
  ];

  Var ("OM_CXXFLAGS", "$(shell $(PERL) -MExtUtils::Embed -e ccopts)");
  Var ("OM_LDFLAGS",  "$(shell $(PERL) -MExtUtils::Embed -e ldopts)");

]
