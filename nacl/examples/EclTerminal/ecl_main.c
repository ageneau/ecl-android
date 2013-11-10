#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void main_lib_ASDF();
extern ECL_CPP_TAG void main_lib_ECL_CDB();
extern ECL_CPP_TAG void main_lib_ECL_HELP();
extern ECL_CPP_TAG void main_lib_NACLPSYSTEM();


#ifdef __cplusplus
extern "C"
#endif
ECL_DLLEXPORT
void init_ECL_PROGRAM(cl_object cblock)
{
  main_lib_ECL_HELP();
  main_lib_ASDF();
  // main_lib_NACLPSYSTEM();
}

int
ecl_main(int argc, char **argv)
{
  char *ecl = "ecl";
  setenv("ECLDIR", "/lib/ecl-13.5.1/", 1);

  // ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGINT, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGILL, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0);
  // ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0);
  // ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0);
  // ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0);
  //fprintf(stderr,"ecl_main\n");
  cl_boot(1, &ecl);
  //  cl_boot(argc, argv);
  //fprintf(stderr,"cl_boot\n");

  ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ecl_init_module(OBJNULL, init_ECL_PROGRAM);
    {
      const char *lisp_code = "(SI:TOP-LEVEL T) ";
      cl_object output;
      si_select_package(ecl_make_simple_base_string("CL-USER", 7));
      output = si_safe_eval(2, ecl_read_from_cstring(lisp_code), ECL_NIL);
    }
  } ECL_CATCH_ALL_END;
  si_exit(0);

  fprintf(stderr,"exit\n");
}


