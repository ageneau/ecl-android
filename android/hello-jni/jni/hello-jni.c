#include <assert.h>
#if 0
#include <android/log.h>
#endif
#include <string.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>

#include <ecl/ecl.h>
#include "ecl_boot.h"

#if 0
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, "native-activity", __VA_ARGS__))
#else
#define LOGI(...)
#define LOGW(...)
#define LOGE(...)
#endif

void
Java_com_example_hellojni_HelloJni_startECL( JNIEnv* env,
											 jobject thiz )
{
	LOGI("INIT ECL");
	jclass cls = (*env)->GetObjectClass(env, thiz);
	assert(cls);
	jmethodID mid = (*env)->GetStaticMethodID(env, cls, "getResourcesPath", "()Ljava/lang/String;");
	assert(mid);

	jstring file = (*env)->CallStaticObjectMethod(env, cls, mid);
	const char *lisp_dir = (*env)->GetStringUTFChars(env, file, NULL);

	LOGI("Path is: %s\n",lisp_dir);

	ecl_boot(lisp_dir);
	LOGI("INIT ECL DONE");	
}
