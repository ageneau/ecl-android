#include <stdlib.h>
#include <ecl/ecl.h>
#include "ecl_boot.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void init_lib_SERVE_EVENT(cl_object);
extern ECL_CPP_TAG void init_lib_SOCKETS(cl_object);
extern ECL_CPP_TAG void init_lib_PROFILE(cl_object);
extern ECL_CPP_TAG void init_lib_BYTECMP(cl_object);
extern ECL_CPP_TAG void init_lib_ECLFFI(cl_object);

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
        cl_object current, next = Cblock;
        current = read_VV(OBJNULL, init_lib_SOCKETS);       current->cblock.next = next; next = current;
        current = read_VV(OBJNULL, init_lib_BYTECMP);       current->cblock.next = next; next = current;
        current = read_VV(OBJNULL, init_lib_SERVE_EVENT);   current->cblock.next = next; next = current;
        current = read_VV(OBJNULL, init_lib_ECLFFI);        current->cblock.next = next; next = current;
	    //current = read_VV(OBJNULL, init_lib_PROFILE);       current->cblock.next = next; next = current; 

	Cblock->cblock.next = current;
    }
}

void ecl_toplevel(const char *home)
{
    char tmp[512];
    sprintf(tmp, "(load \"%s/%s\")", home, "init.lisp");
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
}

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

int ecl_boot(const char *root_dir)
{
    int argc = 1;
    char *argv[256];
    argv[0] = "ecl";
	GC_allow_register_threads();
    GC_register_my_thread(argv);
    GC_stackbottom = (void*)(argv+255);
    setenv("ECLDIR", "", 1);
    cl_boot(argc, argv);
    read_VV(OBJNULL, init_ECL_PROGRAM);
    char tmp[2048];
    sprintf(tmp, "(setq *default-pathnames-defaults* #p\"%s\")", root_dir);
    si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
    init_callbacks_registry();
    ecl_toplevel(root_dir);
    return 0;
}
