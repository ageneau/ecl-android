#include <stdlib.h>
#include <ecl/ecl.h>
#include "ecl_boot.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void main_lib_ASDF();
extern ECL_CPP_TAG void main_lib_SOCKETS();
extern ECL_CPP_TAG void main_lib_ECLFFI();
extern ECL_CPP_TAG void main_lib_ECL_HELP();
extern ECL_CPP_TAG void main_lib_APP_BUNDLE_MONO_CROSS();
extern ECL_CPP_TAG void main_lib_OKRA_MONO_CROSS();
extern ECL_CPP_TAG void main_lib_ALEXANDRIA_MONO_CROSS();
extern ECL_CPP_TAG void main_lib_BABEL_MONO_CROSS();
extern ECL_CPP_TAG void main_lib_TRIVIAL_FEATURES_MONO_CROSS();

#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0

#ifdef __cplusplus
extern "C"
#endif


cl_object ecl_callbacks = Cnil;
cl_object Xecl_callbacksX = Cnil;
static void init_callbacks_registry()
{
  int internp;
  Xecl_callbacksX = ecl_intern(make_simple_base_string("*ECL-CALLBACKS*"),
                               ecl_find_package_nolock(ecl_make_keyword("SI")),
                               &internp);
  ecl_defvar(Xecl_callbacksX, ecl_callbacks);
  ecl_register_root(&ecl_callbacks);
}

void add_cb(cl_object fun)
{
    if (Cnil != fun && FALSE == ecl_member_eq(fun, ecl_callbacks)) {
        ecl_callbacks = ecl_cons(fun, ecl_callbacks);
        cl_set(Xecl_callbacksX, ecl_callbacks);
    }
}

void remove_cb(cl_object fun)
{
    if (Cnil == fun) return;
    if (ecl_member_eq(fun, ecl_callbacks)) {
        ecl_callbacks = ecl_remove_eq(fun, ecl_callbacks);
        cl_set(Xecl_callbacksX, ecl_callbacks);
    }
}


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
//    main_lib_OKRA_MONO_CROSS();
    main_lib_APP_BUNDLE_MONO_CROSS();
//    main_lib_ALEXANDRIA_MONO_CROSS();
//    main_lib_BABEL_MONO_CROSS();
//    main_lib_TRIVIAL_FEATURES_MONO_CROSS();

    si_select_package(ecl_make_simple_base_string("CL-USER", 7));
    char tmp[2048];
    sprintf(tmp, "(setq *default-pathnames-defaults* #p\"%s\")", root_dir);
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
    init_callbacks_registry();
    ecl_toplevel(root_dir);
    return 0;
}

