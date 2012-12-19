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
ECL_HOME := ../../local-install/android
LOCAL_MODULE    := hello-jni
LOCAL_PATH := $(BASE_PATH)
LOCAL_SRC_FILES := hello-jni.c ecl_boot.c
LOCAL_CFLAGS += -I$(ECL_HOME)/include
LOCAL_CFLAGS += -g -Wall -DPLATFORM_ANDROID
LOCAL_LDLIBS := -L$(ECL_HOME)/lib -lecl -lgmp -L$(ECL_HOME)/lib/ecl-12.7.1 -lasdf -lsockets -lsb-bsd-sockets -lserve-event -lecl-help -lecl-cdb -lgc -latomic_ops
LOCAL_LDLIBS += -llog

include $(BUILD_SHARED_LIBRARY)

