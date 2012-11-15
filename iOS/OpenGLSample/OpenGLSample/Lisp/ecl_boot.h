#ifndef _ECL_BOOT_H_
#define _ECL_BOOT_H_

#ifdef __cplusplus
extern "C" {
#endif
    
int ecl_boot(const char *root_dir);
void ecl_toplevel(const char *home);

#ifdef __cplusplus
}
#endif

#endif
