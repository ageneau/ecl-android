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
  char tmp[2048];

  char *ecl = "ecl";
  setenv("ECLDIR", root_dir, 1);

  // ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGINT, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGILL, 0);
  // ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0);
  // ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0);
  // ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0);
  // ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0);

  cl_boot(1, &ecl);

  si_safe_eval(3, c_string_to_object("(format t \"ECL_BOOT, features = ~A ~%\" *features*)"), Cnil, OBJNULL);
  si_safe_eval(3, c_string_to_object("(format t \"(truename SYS:): ~A)\" (truename \"SYS:\"))"), Cnil, OBJNULL);


  LOGI("ALL LOADED\n");

  sprintf(tmp, "(setq *default-pathname-defaults* #p\"%s/\")", root_dir);
  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);

  return 0;
}

void ecl_toplevel(const char *home)
{
  char tmp[512];

  LOGI("START TOP LEVEL\n");

  CL_CATCH_ALL_BEGIN(ecl_process_env()) 
  {
	  si_select_package(ecl_make_simple_base_string("CL-USER", 7));

	  sprintf(tmp, "(print \"EXECUTING ecl_toplvl\")");
	  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
	  
	  sprintf(tmp, "(load \"%s\")","toplevel");
	  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
	  
	  sprintf(tmp, "(print \"EXITING ecl_toplvl\")");
	  si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);

  } CL_CATCH_ALL_END;

  LOGI("EXIT TOP LEVEL\n");
}
