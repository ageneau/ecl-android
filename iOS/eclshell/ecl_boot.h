#ifndef _ECL_BOOT_H_
#define _ECL_BOOT_H_

#ifdef __cplusplus
extern "C" {
#endif
    
int ecl_boot(const char *root_dir);
void ecl_toplevel(const char *home);

void add_cb(cl_object fun);
void remove_cb(cl_object fun);
#define register_cb(h, val) remove_cb(h); h = val; add_cb(h);

#ifdef __cplusplus
}
#endif

#endif
