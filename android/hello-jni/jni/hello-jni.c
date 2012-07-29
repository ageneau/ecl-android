#include <assert.h>
#if ANDROID
#include <android/log.h>
#endif
#include <string.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>

#include <ecl/ecl.h>
#include "ecl_boot.h"

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


JNIEXPORT void JNICALL 
Java_com_example_hellojni_HelloJni_startECL( JNIEnv* env,
											 jobject thiz )
{
	const char *lisp_dir = "/sdcard/ecl/";

	LOGI("Path is: %s\n",lisp_dir);
	ecl_boot(lisp_dir);
	LOGI("INIT ECL DONE");	
}


JNIEXPORT jstring JNICALL
Java_com_example_hellojni_HelloJni_eclExec (JNIEnv * env, jobject obj, jstring str)
{
	jstring ret;

	char* cmd = (*env)->GetStringUTFChars(env, str, NULL);
	cl_object result = si_safe_eval(3, c_string_to_object(cmd), Cnil, OBJNULL); 

	if(result)
	{
		cl_object out = si_coerce_to_base_string(cl_princ_to_string(result));
		ret = (*env)->NewStringUTF(env, (const char*) out->base_string.self);
	}
	else
	{
		ret = (*env)->NewStringUTF(env, "ERROR in eval"); 
	}

	return ret;
}

