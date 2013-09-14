#include <EXTERN.h>
#include <XSUB.h>
#include <perl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


#define Interpreter_val(v) (*((PerlInterpreter **) Data_custom_val(v)))


static struct custom_operations interpreter_ops = {
  (char *)"PerlInterpreter",
  [] (value vmy_perl) {
    PerlInterpreter *my_perl = Interpreter_val (vmy_perl);

    perl_destruct (my_perl);
    perl_free (my_perl);
  },

  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
};


value
ml_Perl_Interpreter_create (value argv)
{
  PerlInterpreter *my_perl;

  //PERL_SYS_INIT3 (&args.argc, &args.argv, &args.env);
  perl_construct (my_perl);
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;

  value vmy_perl = alloc_custom (&interpreter_ops, sizeof my_perl, 0, 1);
  Interpreter_val (vmy_perl) = my_perl;

  return vmy_perl;
}
