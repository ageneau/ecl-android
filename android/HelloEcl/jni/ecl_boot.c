#include <stdlib.h>
#include <ecl/ecl.h>
#if ANDROID
#include <android/log.h>
#endif

#if ANDROID
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, "native-activity", __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, "native-activity", __VA_ARGS__))
#else
#define LOGI(...)
#define LOGW(...)
#define LOGE(...)
#endif

#include "ecl_boot.h"

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

extern ECL_CPP_TAG void main_lib_ASDF();
extern ECL_CPP_TAG void main_lib_SOCKETS();
extern ECL_CPP_TAG void main_lib_SB_BSD_SOCKETS();
extern ECL_CPP_TAG void main_lib_SERVE_EVENT();
extern ECL_CPP_TAG void main_lib_ECL_CDB();
extern ECL_CPP_TAG void main_lib_ECL_HELP();


extern void loadLispFromAssets(char* fn);


int ecl_boot(const char *root_dir)
{
  char *ecl = "ecl";
  char tmp[2048];

  sprintf(tmp, "%s/", root_dir);
  setenv("ECLDIR", tmp, 1);

  // ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGINT, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGILL, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0);
  // ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0);
  // ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0);
  // ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0);

  cl_boot(1, &ecl);

  main_lib_ECL_HELP();
  main_lib_ASDF();
  main_lib_SOCKETS();

  si_safe_eval(3, c_string_to_object("(format t \"ECL_BOOT, features = ~A ~%\" *features*)"), Cnil, OBJNULL);
  si_safe_eval(3, c_string_to_object("(format t \"(truename SYS:): ~A)\" (truename \"SYS:\"))"), Cnil, OBJNULL);

  LOGI("ALL LOADED\n");

  ecl_toplevel(root_dir);

  return 0;
}

void ecl_toplevel(const char *home)
{
  char tmp[2048];

  LOGI("START TOP LEVEL\n");

  CL_CATCH_ALL_BEGIN(ecl_process_env()) 
  {
	  sprintf(tmp, "(setq *default-pathname-defaults* #p\"%s/\")", home);
	  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);

	  si_select_package(ecl_make_simple_base_string("CL-USER", 7));
	  
	  si_safe_eval(3, c_string_to_object("(load \"init\")"), Cnil, OBJNULL);

  } CL_CATCH_ALL_END;

  LOGI("EXIT TOP LEVEL\n");
}
