#include <stdlib.h>
#include <ecl/ecl.h>
#include "ecl_boot.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern void loadLispFromAssets(char* fn);

// extern ECL_CPP_TAG void init_lib_SERVE_EVENT(cl_object);
// extern ECL_CPP_TAG void init_lib_SOCKETS(cl_object);
// extern ECL_CPP_TAG void init_lib_PROFILE(cl_object);
// extern ECL_CPP_TAG void init_lib_BYTECMP(cl_object);
// extern ECL_CPP_TAG void init_lib_ECLFFI(cl_object);

#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0

#ifdef __cplusplus
extern "C"
#endif

void init_ECL_PROGRAM(cl_object cblock)
{
  static cl_object Cblock;
  if (!FIXNUMP(cblock)) {
    Cblock = cblock;
    cblock->cblock.data_text = compiler_data_text;
    cblock->cblock.data_text_size = compiler_data_text_size;
#ifndef ECL_DYNAMIC_VV
    cblock->cblock.data = VV;
#endif
    cblock->cblock.data_size = VM;
    return;
  }
#if defined(ECL_DYNAMIC_VV) && defined(ECL_SHARED_DATA)
  VV = Cblock->cblock.data;
#endif
	
  {
	// cl_object current, next = Cblock;
    // current = read_VV(OBJNULL, init_lib_SOCKETS); current->cblock.next = next; next = current;
    // current = read_VV(OBJNULL, init_lib_BYTECMP); current->cblock.next = next; next = current;
    // current = read_VV(OBJNULL, init_lib_SERVE_EVENT); current->cblock.next = next; next = current;
    // current = read_VV(OBJNULL, init_lib_ECLFFI); current->cblock.next = next; next = current;
	// current = read_VV(OBJNULL, init_lib_PROFILE); current->cblock.next = next; next = current; 

	// Cblock->cblock.next = current;
  }
}

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
  read_VV(OBJNULL, init_ECL_PROGRAM);
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

    sprintf(tmp, "(load \"%s\")","init.lsp");
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);

	fflush(stdout);
}
