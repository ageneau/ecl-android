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
#define ECL_TAG "ecl-native"
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, ECL_TAG, __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, ECL_TAG, __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, ECL_TAG, __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, ECL_TAG, __VA_ARGS__))
#else
#define LOGI(...)
#define LOGW(...)
#define LOGE(...)
#endif

#define jni_ecl_read_from_string(x) si_string_to_object(1, x)

/*
 * Class:     org_lisp_ecl_EmbeddedCommonLisp
 * Method:    start
 * Signature: ()V
 */
JNIEXPORT void JNICALL
Java_org_lisp_ecl_EmbeddedCommonLisp_start(JNIEnv *env, jobject this, 
					   jstring path) {

  const char *lisp_dir = (*env)->GetStringUTFChars(env, path, NULL);  
  LOGI("ECL starting: *default-pathname-defaults* to: %s\n", lisp_dir);
  ecl_boot(lisp_dir);
  LOGI("ECL started.");	
};

/* This was fun to make UTF8 work across Java-C-Lisp boundaries.
   -evrim, 2014. */
cl_object java_string_to_ecl_string(JNIEnv *env, jstring str) {
  const jchar *txt = (*env)->GetStringChars(env, str, NULL);
  jsize len = (*env)->GetStringLength(env, str);
  cl_object ecl_txt = ecl_alloc_simple_extended_string(len);
  cl_index i;

  for (i=0;i<len;i++) {
    ecl_txt->string.self[i] = txt[i];
  };
  
  (*env)->ReleaseStringChars(env, str, txt);

  return ecl_txt;
}


jstring ecl_object_to_java_string(JNIEnv *env, cl_object o) {
  jstring ret;
  if (ECL_EXTENDED_STRING_P(o)) {
    LOGI("ecl->java extended string of fillp: %d, dim: %d",
	 o->string.fillp,
	 o->string.dim);
    
    jsize len = o->string.fillp;
    jchar *arr = malloc(sizeof(jchar)*(len+1));
    cl_index i;
    for (i=0; i<len; i++) {
      arr[i] = o->string.self[i];
    }
    arr[len] = 0;
    ret = (*env)->NewString(env, arr, len);
    free(arr);
  } else if (ECL_STRINGP(o)) {
    LOGI("ecl->java base string of len %d: %s",
	 o->base_string.dim,
	 o->base_string.self);

    ret = (*env)->NewStringUTF(env, 
			       (const char*)o->base_string.self);
  } else {
    LOGI("ecl->java not a string, coercing");
    return ecl_object_to_java_string(env, cl_princ_to_string(o));
  }

  return ret;
}

/*
 * Class:     org_lisp_ecl_EmbeddedCommonLisp
 * Method:    exec
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_org_lisp_ecl_EmbeddedCommonLisp_exec(JNIEnv *env, jobject this, jstring str) {
  jstring ret;
  cl_object txt = java_string_to_ecl_string(env, str);
  cl_object result = si_safe_eval(3, jni_ecl_read_from_string(txt), Cnil, OBJNULL);
  
  if (result) {
    ret = ecl_object_to_java_string(env, result);
  } else {
    ret = (*env)->NewStringUTF(env, "ERROR in eval");
  }
  
  return ret;
};

#undef jni_ecl_read_from_string

/*
 * Class:     org_lisp_ecl_EmbeddedCommonLisp
 * Method:    exec_
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_org_lisp_ecl_EmbeddedCommonLisp_exec_(JNIEnv *env, jobject this, jstring str) {
  jstring ret;
  const char *cmd = (*env)->GetStringUTFChars(env, str, NULL);
  cl_object result = si_safe_eval(3, c_string_to_object(cmd),
				  Cnil, OBJNULL);

  if (result) {
    cl_object out = si_coerce_to_base_string(cl_princ_to_string(result));
    ret = (*env)->NewStringUTF(env, (const char*) out->base_string.self);
  } else {
    ret = (*env)->NewStringUTF(env, "ERROR in eval");
  }
  return ret;
};
