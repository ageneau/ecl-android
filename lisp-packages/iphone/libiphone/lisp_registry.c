#include <stdlib.h>
#include <ecl/ecl.h>
#include "lisp_registry.h"


cl_object ecl_callbacks = Cnil;
cl_object Xecl_callbacksX = Cnil;
void init_callbacks_registry()
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
