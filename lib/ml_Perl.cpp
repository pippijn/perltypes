#include <vector>

#include <EXTERN.h>
#include <XSUB.h>
#include <perl.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

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
  value val = alloc_custom (&value_info<T>::ops, sizeof v, 0, 1);
  value_cast<T> (val) = v;
  return val;
}


#define SV_val value_cast<SV *>

static PerlInterpreter *my_perl;

template<>
struct value_info<SV *>::operations
  : default_operations
{
  static constexpr char const *identifier = "Perl object";

  static void finalize (value vsv) {
    SV *sv = SV_val (vsv);
    SvREFCNT_dec (sv);
  }
};


extern "C" value
ml_Perl_init (value vargv)
{
  if (my_perl)
    return Val_unit;

  // Allocate and initialise perl interpreter.
  int argc = caml_array_length (vargv);
  std::vector<char *> args (argc + 1);
  for (int i = 0; i < argc; i++)
    args[i] = strdup (String_val (Field (vargv, i)));
  char **argv = &args[0];

  PERL_SYS_INIT (&argc, &argv);
  my_perl = perl_alloc ();
  perl_construct (my_perl);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;


  // Start interpreter.
  char const *embedding[] = {
    argv[0],
    "-e", "use common::sense;",
    0
  };

  if (perl_parse (my_perl, nullptr, array_size (embedding) - 1, const_cast<char **> (embedding), nullptr)
      || perl_run (my_perl))
    failwith ("unable to initialise perl interpreter");

  if (SvTRUE (ERRSV))
    failwith (SvPV_nolen (ERRSV));

  return Val_unit;
}

/*************************************************************
 * :: OCaml -> Perl
 *************************************************************/

extern "C" value
ml_Perl_sv_of_char (value val)
{
  return make_value (newSViv (Int_val (val)));
}

extern "C" value
ml_Perl_sv_of_bool (value val)
{
  return make_value (newSViv (Int_val (val)));
}

extern "C" value
ml_Perl_sv_of_string (value val)
{
  return make_value (newSVpv (String_val (val), caml_string_length (val)));
}

extern "C" value
ml_Perl_sv_of_float (value val)
{
  return make_value (newSVnv (Double_val (val)));
}

extern "C" value
ml_Perl_sv_of_int (value val)
{
  return make_value (newSViv (Int_val (val)));
}

extern "C" value
ml_Perl_sv_of_int32 (value val)
{
  return make_value (newSViv (Int32_val (val)));
}

extern "C" value
ml_Perl_sv_of_int64 (value val)
{
  return make_value (newSViv (Int64_val (val)));
}

/*************************************************************
 * :: Perl -> OCaml
 *************************************************************/

extern "C" value
ml_Perl_char_of_sv (value val)
{
  return Val_int (SvIV (SV_val (val)));
}

extern "C" value
ml_Perl_bool_of_sv (value val)
{
  return Val_int (SvIV (SV_val (val)));
}

extern "C" value
ml_Perl_string_of_sv (value val)
{
  STRLEN len;
  char const *str = SvPV (SV_val (val), len);

  value strval = caml_alloc_string (len);
  memcpy (String_val (strval), str, len);
  return strval;
}

extern "C" value
ml_Perl_float_of_sv (value val)
{
  return caml_copy_double (SvNV (SV_val (val)));
}

extern "C" value
ml_Perl_int_of_sv (value val)
{
  return Val_int (SvIV (SV_val (val)));
}

extern "C" value
ml_Perl_int32_of_sv (value val)
{
  return caml_copy_int32 (SvIV (SV_val (val)));
}

extern "C" value
ml_Perl_int64_of_sv (value val)
{
  return caml_copy_int64 (SvIV (SV_val (val)));
}
