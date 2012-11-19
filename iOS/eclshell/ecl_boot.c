#include <stdlib.h>
#include <ecl/ecl.h>
#include "lisp_registry.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void main_lib_ASDF();
extern ECL_CPP_TAG void main_lib_SOCKETS();
extern ECL_CPP_TAG void main_lib_ECL_HELP();
extern ECL_CPP_TAG void main_lib_IPHONEPSYSTEM();


void ecl_toplevel(const char *home)
{
    char tmp[512];
    sprintf(tmp, "(load \"%s/%s\")", home, "init.lisp");
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
}


int ecl_boot(const char *root_dir)
{
    int argc = 1;
    char *argv[256];
    argv[0] = "ecl";
	GC_allow_register_threads();
    GC_register_my_thread((const struct GC_stack_base *)argv);
    GC_stackbottom = (void*)(argv+255);
    setenv("ECLDIR", "", 1);
    cl_boot(argc, argv);
    main_lib_ECL_HELP();
    main_lib_ASDF();
    main_lib_SOCKETS();
    main_lib_IPHONEPSYSTEM();

    si_select_package(ecl_make_simple_base_string("CL-USER", 7));
    char tmp[2048];
    sprintf(tmp, "(setq *default-pathnames-defaults* #p\"%s\")", root_dir);
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
    init_callbacks_registry();
    ecl_toplevel(root_dir);
    return 0;
}

