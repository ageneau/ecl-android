#ifndef _LISP_REGISTRY_H_
#define _LISP_REGISTRY_H_

#ifdef __cplusplus
extern "C" {
#endif

void init_callbacks_registry();
void add_cb(cl_object fun);
void remove_cb(cl_object fun);
#define register_cb(h, val) remove_cb(h); h = val; add_cb(h);

#ifdef __cplusplus
}
#endif

#endif
