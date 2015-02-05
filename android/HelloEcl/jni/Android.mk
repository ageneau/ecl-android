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

ifeq ($(TARGET_ARCH),x86)
PLATFORM := androidx86
else
PLATFORM := android
endif

ECL_HOME := ../../local-install/$(PLATFORM)
ECL_VER := $(shell basename $(ECL_HOME)/lib/ecl-* |cut -d "-" -f2)

LOCAL_MODULE 	:= ecl_android
LOCAL_PATH 	:= $(BASE_PATH)
LOCAL_SRC_FILES := org_lisp_ecl_EmbeddedCommonLisp.c ecl_boot.c
LOCAL_CFLAGS 	+= -I$(ECL_HOME)/include
LOCAL_CFLAGS 	+= -g -Wall -DANDROID
LOCAL_LDLIBS 	:= -L$(ECL_HOME)/lib 
LOCAL_LDLIBS	+= -L$(ECL_HOME)/lib/ecl-$(ECL_VER)
LOCAL_LDLIBS	+= -lecl -lasdf -lgmp -lsockets -llog
LOCAL_LDLIBS 	+= -lsb-bsd-sockets -lserve-event -lecl-help 
LOCAL_LDLIBS	+= -lecl-cdb -lgc -latomic_ops

include $(BUILD_SHARED_LIBRARY)
