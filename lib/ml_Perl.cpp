#include <vector>
#include <type_traits>

#include <EXTERN.h>
#include <XSUB.h>
#include <perl.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

struct stash
{
  char const *const pkg;
  HV *hv;
};

static PerlInterpreter *my_perl;
static stash exn_stash = { "OCaml::Exception" };
static stash fun_stash = { "OCaml::Closure" };
static stash val_stash = { "OCaml::Value" };


static int indent = 0;

struct trace
{
  trace (char const *func, int line)
    : func (func), line (line)
  {
    printf ("%*s>> %s (%d)\n", indent++, "", func, line);
  }

  ~trace ()
  {
    printf ("%*s<< %s (%d)\n", --indent, "", func, line);
  }

  char const *func;
  int line;
};

#define TRACE trace _t (__func__, __LINE__)


// Creates a function declaration of a function returning a reference to an
// array of N chars. Its return value size is therefore N, which can be used
// in the array_size macro to ensure that it returns a compile-time constant.
template<typename T, size_t N>
char (&array_size (T const(&)[N]))[N];

#define array_size(array) (sizeof array_size (array))


template<typename T>
struct value_info
{
  static custom_operations ops;

  struct default_operations
  {
    static constexpr void (*finalize) (value vsv) = nullptr;
  };

  struct operations;
};

template<typename T>
custom_operations value_info<T>::ops = {
  const_cast<char *> (value_info<T>::operations::identifier),
  value_info<T>::operations::finalize,
};


template<typename T>
static T &
value_cast (value v)
{
  return *static_cast<T *> (Data_custom_val (v));
}

template<typename T>
static value
make_value (T const &v)
{
  CAMLparam0 ();
  CAMLlocal1 (val);

  val = alloc_custom (&value_info<T>::ops, sizeof v, 0, 1);
  value_cast<T> (val) = v;

  CAMLreturn (val);
}

#define SV_val value_cast<SV *>


template<>
struct value_info<SV *>::operations
  : default_operations
{
  static constexpr char const *identifier = "Perl object";

  static void finalize (value vsv) {
    if (!my_perl)
      failwith ("OCaml code used object, but Perl interpreter is already destroyed");
    SvREFCNT_dec (SV_val (vsv));
  }
};


/*************************************************************
 * :: Storing OCaml values into Perl SV.
 *************************************************************/

static MGVTBL value_vtbl = {
  /* get */ 0,
  /* set */ 0,
  /* len */ 0,
  /* clear */ 0,
  /* free */ [] (pTHX_ SV *sv, MAGIC *mg) {
    value *root = reinterpret_cast<value *> (mg->mg_ptr);
    caml_remove_generational_global_root (root);
    delete root;

    return 0;
  },
  /* copy */ 0,
  /* dup */ 0,
  /* local */ 0,
};

static SV *
sv_of_value (value val, HV *stash = val_stash.hv)
{
  if (val == Val_unit)
    return &PL_sv_undef;

  SV *self = newSV (0);

  /*printf ("SV created: %p\n", self);*/

  value *root = new value (val);
  caml_register_generational_global_root (root);

  sv_magicext (self, 0, PERL_MAGIC_ext, &value_vtbl,
               reinterpret_cast<char const *> (root), 0);
  if (!SvROK (self))
    self = newRV_noinc (self);
  if (stash)
    self = sv_bless (self, stash);

  /*printf ("RV created: %p -> %s\n", self, sv_pv (self));*/

  return self;
}


/*************************************************************
 * :: Getting stored OCaml values back out of a Perl SV.
 *************************************************************/

static value &
sv_camlroot (SV *sv)
{
  if (sv == &PL_sv_undef)
    // XXX: this function should use caml_failwith
    // when the callee is ocaml code
    croak ("null pointer");

  if (!SvROK (sv))
    croak ("no reference");

  if (   !sv_isa (sv, val_stash.pkg)
      && !sv_isa (sv, fun_stash.pkg)
      && !sv_isa (sv, exn_stash.pkg))
    croak ("expected OCaml value, got '%s'",
           sv_pv (sv));

  sv = SvRV (sv);

  value *root = nullptr;
  // very important shortcut
  if (SvMAGIC (sv) && SvMAGIC (sv)->mg_type == PERL_MAGIC_ext)
    return *reinterpret_cast<value *> (SvMAGIC (sv)->mg_ptr);

  else if (MAGIC *mg = mg_find (sv, PERL_MAGIC_ext))
    return *reinterpret_cast<value *> (mg->mg_ptr);

  croak ("perl code used object, but OCaml object is already destroyed");
}




/*************************************************************
 * :: Call named Perl subroutine.
 *************************************************************/

template<int Flags = G_ARRAY, typename FillArgs, typename OnSuccess>
static value
perl_call (char const *name,
           FillArgs fill_args,
           OnSuccess on_success)
{
  dSP;
  ENTER;
  SAVETMPS;
  PUSHMARK (SP);

  SP = fill_args (SP);

  PUTBACK;
  int count = call_pv (name, Flags | G_EVAL);
  SPAGAIN;

  value retval;
  if (!SvTRUE (ERRSV))
    retval = on_success (SP, count);

  PUTBACK;
  FREETMPS;
  LEAVE;

  if (SvTRUE (ERRSV))
    {
      if (sv_isa (ERRSV, exn_stash.pkg))
        {
          /*printf ("ERRSV = %p = %s\n", ERRSV, sv_pv (ERRSV));*/
          value exn = sv_camlroot (ERRSV);
          caml_raise (exn);
        }
      failwith (sv_pv (ERRSV));
    }

  return retval;
}


/*************************************************************
 * :: Invoke OCaml functional closure from Perl.
 *************************************************************/

static void
invoke_closure (pTHX_ CV *cv)
{
  dVAR;
  dXSARGS;

  if (items < 1)
    croak_xs_usage (cv, "clos, ...");

  SV *clos = ST (0);
  if (!sv_isa (clos, fun_stash.pkg))
    croak ("invoke_closure: expected OCaml functional value, got '%s'",
           sv_pv (clos));

  std::vector<value> args (items - 1);
  for (int i = 1; i < items; i++)
    args[i - 1] = make_value (newSVsv (ST (i)));

  value result = caml_callbackN_exn (sv_camlroot (clos), args.size (), &args[0]);

  if (Is_exception_result (result))
    {
      SV *errsv = sv_of_value (Extract_exception (result), exn_stash.hv);
      croak_sv (sv_2mortal (errsv));
    }

  ST (0) = SV_val (result);
  XSRETURN (1);
}


/*************************************************************
 * :: Perl interpreter initialisation.
 *************************************************************/

extern "C" void boot_DynaLoader (pTHX_ CV *cv);

static void
xs_init (pTHX)
{
  dXSUB_SYS;

  exn_stash.hv = gv_stashpvn (exn_stash.pkg, strlen (exn_stash.pkg), true);
  fun_stash.hv = gv_stashpvn (fun_stash.pkg, strlen (fun_stash.pkg), true);
  val_stash.hv = gv_stashpvn (val_stash.pkg, strlen (val_stash.pkg), true);

  newXS ("DynaLoader::boot_DynaLoader", boot_DynaLoader, __FILE__);
  newXSproto ("OCaml::invoke_closure", invoke_closure, __FILE__, "$@");
}


#define export extern "C" CAMLprim

export value
ml_Perl_init (value vargv)
{
  assert (!my_perl);
  if (my_perl)
    return Val_unit;

  // Allocate and initialise perl interpreter.
  int argc = caml_array_length (vargv);
  std::vector<char *> args (argc + 1);
  for (int i = 0; i < argc; i++)
    args[i] = String_val (Field (vargv, i));
  char **argv = &args[0];

  PERL_SYS_INIT (&argc, &argv);
  my_perl = perl_alloc ();
  perl_construct (my_perl);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;


  // Start interpreter.
  char const *embedding[] = {
    argv[0],
    "-e", "package OCaml;",
    "-e", "",
    "-e", "use common::sense;",
    "-e", "",
    "-e", "sub make_closure {",
    "-e", "  my ($argc, $clos) = @_;",
    "-e", "  sub {",
    "-e", "    die \"expected $argc args, got \" . scalar @_",
    "-e", "       if @_ != $argc;",
    "-e", "    invoke_closure $clos, @_",
    "-e", "  }",
    "-e", "}",
    "-e", "",
    "-e", "package main;",
    "-e", "",
    "-e", "use common::sense;",
    "-e", "use Data::Dumper;",
    /*"-e", "use Test::LeakTrace::Script;",*/
    "-e", "",
    "-e", "sub say {",
    "-e", "  print \"$_: $_[0] and $_[1]\\n\" for (1 .. $_[2]);",
    "-e", "  300",
    "-e", "}",
    "-e", "",
    "-e", "sub stuff {",
    "-e", "  print @_, \"\\n\";",
    "-e", "  (1, 2, '3 apples', @_)",
    "-e", "}",
    "-e", "",
    "-e", "sub test_invoke1 {",
    "-e", "  $_[0]->($_[1])",
    "-e", "}",
    "-e", "",
    "-e", "sub test_invoke2 {",
    "-e", "  $_[0]->('world', 'hehe')",
    "-e", "}",
    0
  };

  if (perl_parse (my_perl, xs_init, array_size (embedding) - 1,
                  const_cast<char **> (embedding), nullptr)
      || perl_run (my_perl))
    failwith ("unable to initialise perl interpreter");

  if (SvTRUE (ERRSV))
    failwith (sv_pv (ERRSV));

  return Val_unit;
}

export value
ml_Perl_fini ()
{
  assert (my_perl);
  if (!my_perl)
    return Val_unit;

  perl_destruct (my_perl);
  perl_free (my_perl);
  PERL_SYS_TERM ();

  val_stash.hv = nullptr;
  fun_stash.hv = nullptr;
  exn_stash.hv = nullptr;
  my_perl = nullptr;

  return Val_unit;
}


/*************************************************************
 * :: Perl's undef scalar, 'a sv on the OCaml side.
 *************************************************************/

export value
ml_Perl_undef ()
{
  assert (my_perl);
  return make_value (&PL_sv_undef);
}


/*************************************************************
 * :: Conversion: Perl -> OCaml
 *************************************************************/

export value
ml_Perl_value_of_sv (value val)
{
  assert (my_perl);
  return sv_camlroot (SV_val (val));
}

export value
ml_Perl_char_of_sv (value val)
{
  assert (my_perl);
  return Val_int (sv_pv (SV_val (val))[0]);
}

export value
ml_Perl_bool_of_sv (value val)
{
  assert (my_perl);
  return Val_bool (SvIV (SV_val (val)));
}

export value
ml_Perl_string_of_sv (value val)
{
  assert (my_perl);
  CAMLparam1 (val);
  CAMLlocal1 (strval);

  STRLEN len;
  char const *str = SvPV (SV_val (val), len);

  strval = caml_alloc_string (len);
  memcpy (String_val (strval), str, len);

  CAMLreturn (strval);
}

export value
ml_Perl_float_of_sv (value val)
{
  assert (my_perl);
  return caml_copy_double (SvNV (SV_val (val)));
}

export value
ml_Perl_int_of_sv (value val)
{
  assert (my_perl);
  return Val_int (SvIV (SV_val (val)));
}

export value
ml_Perl_nativeint_of_sv (value val)
{
  assert (my_perl);
  return caml_copy_nativeint (SvIV (SV_val (val)));
}

export value
ml_Perl_int32_of_sv (value val)
{
  assert (my_perl);
  return caml_copy_int32 (SvIV (SV_val (val)));
}

export value
ml_Perl_int64_of_sv (value val)
{
  assert (my_perl);
  return caml_copy_int64 (SvIV (SV_val (val)));
}


/*************************************************************
 * :: Conversion: OCaml -> Perl
 *************************************************************/

export value
ml_Perl_sv_of_value (value val)
{
  assert (my_perl);
  return make_value (sv_of_value (val));
}

export value
ml_Perl_sv_of_char (value val)
{
  assert (my_perl);
  char str[] = { (char) Int_val (val), '\0' };
  return make_value (newSVpv (str, 1));
}

export value
ml_Perl_sv_of_bool (value val)
{
  assert (my_perl);
  return make_value (newSViv (Bool_val (val)));
}

export value
ml_Perl_sv_of_string (value val)
{
  assert (my_perl);
  CAMLparam1 (val);
  CAMLlocal1 (retval);

  retval = make_value (newSVpv (String_val (val), caml_string_length (val)));

  CAMLreturn (retval);
}

export value
ml_Perl_sv_of_float (value val)
{
  assert (my_perl);
  return make_value (newSVnv (Double_val (val)));
}

export value
ml_Perl_sv_of_int (value val)
{
  assert (my_perl);
  return make_value (newSViv (Int_val (val)));
}

export value
ml_Perl_sv_of_nativeint (value val)
{
  assert (my_perl);
  return make_value (newSViv (Nativeint_val (val)));
}

export value
ml_Perl_sv_of_int32 (value val)
{
  assert (my_perl);
  return make_value (newSViv (Int32_val (val)));
}

export value
ml_Perl_sv_of_int64 (value val)
{
  assert (my_perl);
  return make_value (newSViv (Int64_val (val)));
}


/*************************************************************
 * :: Calling Perl code from OCaml.
 *************************************************************/

static mlsize_t
list_length (value list)
{
  mlsize_t length = 0;
  while (list != Val_emptylist)
    {
      length++;
      list = Field (list, 1);
    }
  return length;
}

export value
ml_Perl_call (value name, value args)
{
  assert (my_perl);
  CAMLparam2 (name, args);
  CAMLlocal2 (retval, cons);

  CAMLreturn (perl_call (String_val (name),

    // fill_args
    [&args] (SV **sp) {
      mlsize_t const argc = list_length (args);

      EXTEND (sp, argc);
      sp += argc;
      while (args != Val_emptylist)
        {
          *sp-- = SV_val (Field (args, 0));
          args = Field (args, 1);
        }
      return sp + argc;
    },

    // on_success
    [&retval, &cons] (SV **sp, int count) {
      retval = Val_emptylist;
      while (count--)
        {
          cons = caml_alloc (2, 0);
          Store_field (cons, 0, make_value (newSVsv (POPs)));
          Store_field (cons, 1, retval);
          retval = cons;
        }

      return retval;
    }

  ));
}


/*************************************************************
 * :: Create Perl object for OCaml functional value.
 *************************************************************/

export value
ml_Perl_sv_of_fun (value argc, value closure)
{
  assert (my_perl);
  CAMLparam1 (closure);

  CAMLreturn (perl_call<G_SCALAR> ("OCaml::make_closure",

    // fill_args
    [argc, &closure] (SV **sp) {
      XPUSHs (sv_2mortal (newSViv (Int_val (argc))));
      XPUSHs (sv_2mortal (sv_of_value (closure, fun_stash.hv)));
      return sp;
    },

    // on_success
    [] (SV **sp, int count) {
      return make_value (newSVsv (POPs));
    }

  ));
}


// vim:ft=xs
