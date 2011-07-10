#include <stdlib.h>
#include <ecl/ecl.h>
#include "ecl_boot.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void main_lib_SOCKETS();
// extern ECL_CPP_TAG void main_lib_BYTECMP();
// extern ECL_CPP_TAG void main_lib_SERVE_EVENT();
// extern ECL_CPP_TAG void main_lib_ECLFFI();
// extern ECL_CPP_TAG void main_lib_PROFILE();
extern ECL_CPP_TAG void main_lib_ALEXANDRIA();
extern ECL_CPP_TAG void main_lib_TRIVIAL_FEATURES();
extern ECL_CPP_TAG void main_lib_TRIVIAL_GARBAGE();
extern ECL_CPP_TAG void main_lib_BORDEAUX_THREADS();
extern ECL_CPP_TAG void main_lib_BABEL();
extern ECL_CPP_TAG void main_lib_CFFI();
extern ECL_CPP_TAG void main_lib_CLPJ();
extern ECL_CPP_TAG void main_lib_CLPJ_MONO_CROSS();
// extern ECL_CPP_TAG void main_lib_TEST();

extern void loadLispFromAssets(char* fn);


int ecl_boot(const char *root_dir)
{
  char *ecl = "ecl";
  setenv("ECLDIR", "", 1);

  ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0);
  ecl_set_option(ECL_OPT_TRAP_SIGINT, 0);
  ecl_set_option(ECL_OPT_TRAP_SIGILL, 0);
  ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0);
  ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0);
  ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0);
  ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0);

  cl_boot(1, &ecl);
  // fprintf(stderr,"loading SOCKETS\n");
  // main_lib_SOCKETS();
  // // main_lib_BYTECMP();
  // // main_lib_SERVE_EVENT();
  // // main_lib_ECLFFI();
  // // main_lib_PROFILE();
  // fprintf(stderr,"loading ALEXANDRIA\n");
  // main_lib_ALEXANDRIA();
  // fprintf(stderr,"loading TRIVIAL_FEATURES\n");
  // main_lib_TRIVIAL_FEATURES();
  // fprintf(stderr,"loading TRIVIAL_GARBAGE\n");
  // main_lib_TRIVIAL_GARBAGE();
  // fprintf(stderr,"loading BORDEAUX_THREADS\n");
  // main_lib_BORDEAUX_THREADS();
  // fprintf(stderr,"loading BABEL\n");
  // main_lib_BABEL();
  // fprintf(stderr,"loading CFFI\n");
  // main_lib_CFFI();
  fprintf(stderr,"loading CLPJ\n");
  main_lib_CLPJ_MONO_CROSS();
  //  main_lib_CLPJ();
  //  main_lib_TEST();
  fprintf(stderr,"ALL LOADED\n");

  // read_VV(OBJNULL, init_ECL_PROGRAM);
  char tmp[2048];
  sprintf(tmp, "(setq *default-pathname-defaults* #p\"%s/\")", root_dir);
  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
  ecl_toplevel(root_dir);

#if 0
  const char *lisp_code = "(SI:TOP-LEVEL)";
  si_select_package(make_simple_base_string("CL-USER"));
  cl_object output = si_safe_eval(3, c_string_to_object(lisp_code), Cnil, OBJNULL);
  cl_shutdown();
  if (FIXNUMP(output))
    return fix(output);
  if (Null(output) || (output == OBJNULL))
    return 1;
#endif
  return 0;
}

void ecl_toplevel(const char *home)
{
  char tmp[512];

  CL_CATCH_ALL_BEGIN(ecl_process_env()) 
  {
    sprintf(tmp, "(load \"%s\")","init.lsp");
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
  } CL_CATCH_ALL_END;

  fflush(stdout);
}
