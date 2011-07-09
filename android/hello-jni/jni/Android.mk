# Copyright (C) 2009 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
BASE_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE    := hello-jni
LOCAL_PATH := $(BASE_PATH)
LOCAL_SRC_FILES := hello-jni.c ecl_boot.c
LOCAL_CFLAGS += -I/opt/ecl/android/include -I/opt/gmp/android/include
LOCAL_CFLAGS += -g -Wall -DPLATFORM_ANDROID
LOCAL_LDLIBS := -L/opt/ecl/android/lib -lecl -L/opt/gmp/android/lib -lgmp -leclgc -L/opt/ecl/android/lib/ecl-11.1.1 -lsockets -lbytecmp 
LOCAL_LDLIBS += -llog

include $(BUILD_SHARED_LIBRARY)

