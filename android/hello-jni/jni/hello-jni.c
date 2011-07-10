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
#else
#define LOGI(...)
#define LOGW(...)
#define LOGE(...)
#endif

#define MAX_STRING_SIZE 4096


extern JavaVM *cached_jvm;

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	jclass cls;
	cached_jvm = jvm;  /* cache the JavaVM pointer */

	LOGI("JNI_OnLoad(JavaVM *jvm, void *reserved)");

	if ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_2)) {
		return JNI_ERR; /* JNI version not supported */
	}

	return JNI_VERSION_1_2;
}

JNIEXPORT void JNICALL 
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

JNIEXPORT jstring JNICALL
Java_com_example_hellojni_HelloJni_eclExec (JNIEnv * env, jobject obj, jstring str)
{
	jstring ret;

	char* cmd = (*env)->GetStringUTFChars(env, str, NULL);
	cl_object result = si_safe_eval(3, c_string_to_object(cmd), Cnil, OBJNULL); 

	if(result)
	{
		cl_object out = si_coerce_to_base_string(cl_princ_to_string(result));
		ret = (*env)->NewStringUTF(env, out->base_string.self);
	}
	else
	{
		ret = (*env)->NewStringUTF(env, "ERROR in eval"); 
	}

	return ret;
}
