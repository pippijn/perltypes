install Library ".DEFAULT" [
  (* Target *)
  Name		"perltypes";
  Description	"OCaml Perl interface";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Perl";
  ];

  (* C/C++ Sources *)
  Sources [
    "ml_Perl.cpp";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "perl.ml",		"-syntax camlp4o";
  ];

  Var ("OM_CXXFLAGS", "$(shell $(PERL) -MExtUtils::Embed -e ccopts)");
  Var ("OM_LDFLAGS",  "$(shell $(PERL) -MExtUtils::Embed -e ldopts)");

]
