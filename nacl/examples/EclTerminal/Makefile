# Copyright (c) 2012 The Native Client Authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

#
# GNU Make based build file.  For details on GNU Make see:
#   http://www.gnu.org/software/make/manual/make.html
#

#
# Get pepper directory for toolchain and includes.
#
# If NACL_SDK_ROOT is not set, then assume it can be found a two directories up,
# from the default example directory location.
#
PROJECT:=ecl_terminal

THIS_MAKEFILE:=$(abspath $(lastword $(MAKEFILE_LIST)))
NACL_SDK_ROOT?=$(abspath $(dir $(THIS_MAKEFILE))../..)
CHROME_PATH?=Undefined


NACL_SDK_VER:=$(shell basename "$(NACL_SDK_ROOT)" | cut -d '_' -f2)
NACL_SDK_VERSION_GT_25:=$(shell expr $(NACL_SDK_VER) \>= 25)

#
# Defaults
#
#NACL_WARNINGS:=-Wno-long-long -Wall -Wswitch-enum -Werror -pedantic
NACL_WARNINGS:=-Wno-long-long -Wall -Wswitch-enum

#
# Project Settings
#
VALID_TOOLCHAINS:=newlib glibc linux pnacl
TOOLCHAIN?=glibc


ECL_ROOT_INSTALL_DIR:=../../../local-install

ECL=$(ECL_ROOT_INSTALL_DIR)/host/bin/ecl

ECL_ROOT32=$(ECL_ROOT_INSTALL_DIR)/nacl32
ECL_ROOT64=$(ECL_ROOT_INSTALL_DIR)/nacl64
ECL_ROOTPNACL=$(ECL_ROOT_INSTALL_DIR)/pnacl

ECL_CFLAGS=-I$(ECL_ROOT32)/include
ECL_VER := $(shell basename $(ECL_ROOT32)/lib/ecl-* |cut -d "-" -f2)

ECL_LIBS32=-L$(ECL_ROOT32)/lib -lecl -lgmp -lgc -L$(ECL_ROOT32)/lib/ecl-$(ECL_VER) -lasdf -lecl-cdb -lecl-help -lnacl-mounts
ECL_LIBS64=-L$(ECL_ROOT64)/lib -lecl -lgmp -lgc -L$(ECL_ROOT64)/lib/ecl-$(ECL_VER) -lasdf -lecl-cdb -lecl-help -lnacl-mounts
ECL_LIBSPNACL=-L$(ECL_ROOTPNACL)/lib -lecl -lgmp -lgc -L$(ECL_ROOTPNACL)/lib/ecl-$(ECL_VER) -lasdf -lecl-cdb -lecl-help -lnacl-mounts

#
# Default target
#
all:

#
# Alias for standard commands
#
CP:=python $(NACL_SDK_ROOT)/tools/oshelpers.py cp
MKDIR:=python $(NACL_SDK_ROOT)/tools/oshelpers.py mkdir
MV:=python $(NACL_SDK_ROOT)/tools/oshelpers.py mv
RM:=python $(NACL_SDK_ROOT)/tools/oshelpers.py rm


#
# Verify we selected a valid toolchain for this example
#
ifeq (,$(findstring $(TOOLCHAIN),$(VALID_TOOLCHAINS)))
$(warning Availbile choices are: $(VALID_TOOLCHAINS))
$(error Can not use TOOLCHAIN=$(TOOLCHAIN) on this example.)
endif


#
# Compute path to requested NaCl Toolchain
#
OSNAME:=$(shell python $(NACL_SDK_ROOT)/tools/getos.py)
TC_PATH:=$(abspath $(NACL_SDK_ROOT)/toolchain)


#
# Verify we have a valid NACL_SDK_ROOT by looking for the toolchain directory
#
ifeq (,$(wildcard $(TC_PATH)))
$(warning No valid NACL_SDK_ROOT at $(NACL_SDK_ROOT))
ifeq ($(origin NACL_SDK_ROOT), 'file')
$(error Override the default value via enviornment variable, or command-line.)
else
$(error Fix the NACL_SDK_ROOT specified in the environment or command-line.)
endif
endif


#
# Disable DOS PATH warning when using Cygwin based NaCl tools on Windows
#
CYGWIN ?= nodosfilewarning
export CYGWIN


#
# Defaults for TOOLS
#

NEWLIB_CC?=$(TC_PATH)/$(OSNAME)_x86_newlib/bin/i686-nacl-gcc -c
NEWLIB_CXX?=$(TC_PATH)/$(OSNAME)_x86_newlib/bin/i686-nacl-g++ -c -std=gnu++98
NEWLIB_LINK?=$(TC_PATH)/$(OSNAME)_x86_newlib/bin/i686-nacl-g++ -Wl,-as-needed
NEWLIB_LIB?=$(TC_PATH)/$(OSNAME)_x86_newlib/bin/i686-nacl-ar r
NEWLIB_DUMP?=$(TC_PATH)/$(OSNAME)_x86_newlib/x86_64-nacl/bin/objdump
NEWLIB_CXXFLAGS?=-MMD -pthread $(NACL_WARNINGS) -idirafter $(NACL_SDK_ROOT)/include $(ECL_CFLAGS)
NEWLIB_LDFLAGS?=-pthread
NEWLIB_LIBS32=$(ECL_LIBS32) -lppapi_cpp -lppapi -lpthread
NEWLIB_LIBS64=$(ECL_LIBS64) -lppapi_cpp -lppapi -lpthread

GLIBC_CC?=$(TC_PATH)/$(OSNAME)_x86_glibc/bin/i686-nacl-gcc -c
GLIBC_CXX?=$(TC_PATH)/$(OSNAME)_x86_glibc/bin/i686-nacl-g++ -c -std=gnu++98
GLIBC_LINK?=$(TC_PATH)/$(OSNAME)_x86_glibc/bin/i686-nacl-g++ -Wl,-as-needed
GLIBC_LIB?=$(TC_PATH)/$(OSNAME)_x86_glibc/bin/i686-nacl-ar r
GLIBC_DUMP?=$(TC_PATH)/$(OSNAME)_x86_glibc/x86_64-nacl/bin/objdump
GLIBC_PATHS:=-L $(TC_PATH)/$(OSNAME)_x86_glibc/x86_64-nacl/lib32
GLIBC_PATHS+=-L $(TC_PATH)/$(OSNAME)_x86_glibc/x86_64-nacl/lib
GLIBC_CXXFLAGS?=-MMD -pthread $(NACL_WARNINGS) -idirafter $(NACL_SDK_ROOT)/include $(ECL_CFLAGS)
GLIBC_LDFLAGS?=-pthread
GLIBC_LIBS32=$(ECL_LIBS32) -lppapi_cpp -lppapi -lpthread
GLIBC_LIBS64=$(ECL_LIBS64) -lppapi_cpp -lppapi -lpthread

PNACL_CC?=$(TC_PATH)/$(OSNAME)_x86_pnacl/newlib/bin/pnacl-clang -c
PNACL_CXX?=$(TC_PATH)/$(OSNAME)_x86_pnacl/newlib/bin/pnacl-clang++ -c
PNACL_LINK?=$(TC_PATH)/$(OSNAME)_x86_pnacl/newlib/bin/pnacl-clang++
PNACL_LIB?=$(TC_PATH)/$(OSNAME)_x86_pnacl/newlib/bin/pnacl-ar r
PNACL_CXXFLAGS?=-MMD -pthread $(NACL_WARNINGS) -idirafter $(NACL_SDK_ROOT)/include $(ECL_CFLAGS)
PNACL_LDFLAGS?=-pthread
PNACL_LIBS=$(ECL_LIBSPNACL) -lppapi_cpp -lppapi -lpthread
TRANSLATE:=$(TC_PATH)/$(OSNAME)_x86_pnacl/newlib/bin/pnacl-translate

LINUX_CC?=gcc -c
LINUX_CXX?=g++ -c -std=gnu++98
LINUX_LINK?=g++ -shared -Wl,-as-needed
LINUX_LIB?=ar r
LINUX_CXXFLAGS=-I$(NACL_SDK_ROOT)/include -I$(NACL_SDK_ROOT)/include/linux
LINUX_LIBS=-lppapi_cpp -lppapi -lpthread


#
# NMF Manifiest generation
#
# Use the python script create_nmf to scan the binaries for dependencies using
# objdump.  Pass in the (-L) paths to the default library toolchains so that we
# can find those libraries and have it automatically copy the files (-s) to
# the target directory for us.
NMF:=python $(NACL_SDK_ROOT)/tools/create_nmf.py


#
# Verify we can find the Chrome executable if we need to launch it.
#
.PHONY: CHECK_FOR_CHROME
CHECK_FOR_CHROME:
ifeq (,$(wildcard $(CHROME_PATH)))
	$(warning No valid Chrome found at CHROME_PATH=$(CHROME_PATH))
	$(error Set CHROME_PATH via an environment variable, or command-line.)
else
	$(warning Using chrome at: $(CHROME_PATH))
endif

#
# Per target object lists
#
PROJECT_OBJS:=ecl_main nacl_module naclmsg

#
# Rules for newlib toolchain
#
newlib:
	$(MKDIR) newlib
newlib/Debug: | newlib
	$(MKDIR) newlib/Debug
newlib/Release: | newlib
	$(MKDIR) newlib/Release

# Include header dependency files.
-include newlib/Debug/*.d
-include newlib/Release/*.d

PPAPI_DEBUG:=$(abspath newlib/Debug/$(PROJECT)_<ARCH>.nexe);application/x-ppapi-debug
newlib/Debug/%_x86_32.o : %.cc $(THIS_MAKE) | newlib/Debug
	$(NEWLIB_CXX) -o $@ $< -g -O0 -m32 $(NEWLIB_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_NEWLIB_DEBUG_x86_32_O:=$(patsubst %,newlib/Debug/%_x86_32.o,$(PROJECT_OBJS))
newlib/Debug/$(PROJECT)_x86_32.nexe : $(PROJECT_NEWLIB_DEBUG_x86_32_O)
	$(NEWLIB_LINK) -o $@ $^ -g -m32 $(NEWLIB_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_32_newlib/Debug $(NEWLIB_LIBS32)

newlib/Debug/%_x86_64.o : %.cc $(THIS_MAKE) | newlib/Debug
	$(NEWLIB_CXX) -o $@ $< -g -O0 -m64 $(NEWLIB_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_NEWLIB_DEBUG_x86_64_O:=$(patsubst %,newlib/Debug/%_x86_64.o,$(PROJECT_OBJS))
newlib/Debug/$(PROJECT)_x86_64.nexe : $(PROJECT_NEWLIB_DEBUG_x86_64_O)
	$(NEWLIB_LINK) -o $@ $^ -g -m64 $(NEWLIB_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_64_newlib/Debug $(NEWLIB_LIBS64)


#ALL_TARGETS+=newlib/Debug/$(PROJECT).nmf
newlib/Debug/$(PROJECT).nmf : newlib/Debug/$(PROJECT)_x86_32.nexe newlib/Debug/$(PROJECT)_x86_64.nexe
	$(NMF) -D $(NEWLIB_DUMP) -o $@ $^ -t newlib -s newlib/Debug

PPAPI_RELEASE:=$(abspath newlib/Release/$(PROJECT)_x86_64.nexe);application/x-ppapi-release
newlib/Release/%_x86_32.o : %.cc $(THIS_MAKE) | newlib/Release
	$(NEWLIB_CXX) -o $@ $< -O2 -m32 $(NEWLIB_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_NEWLIB_RELEASE_x86_32_O:=$(patsubst %,newlib/Release/%_x86_32.o,$(PROJECT_OBJS))
newlib/Release/$(PROJECT)_x86_32.nexe : $(PROJECT_NEWLIB_RELEASE_x86_32_O)
	$(NEWLIB_LINK) -o $@ $^ -m32 $(NEWLIB_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_32_newlib/Release $(NEWLIB_LIBS32)

newlib/Release/%_x86_64.o : %.cc $(THIS_MAKE) | newlib/Release
	$(NEWLIB_CXX) -o $@ $< -O2 -m64 $(NEWLIB_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_NEWLIB_RELEASE_x86_64_O:=$(patsubst %,newlib/Release/%_x86_64.o,$(PROJECT_OBJS))
newlib/Release/$(PROJECT)_x86_64.nexe : $(PROJECT_NEWLIB_RELEASE_x86_64_O)
	$(NEWLIB_LINK) -o $@ $^ -m64 $(NEWLIB_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_64_newlib/Release $(NEWLIB_LIBS64)


#ALL_TARGETS+=newlib/Release/$(PROJECT).nmf
newlib/Release/$(PROJECT).nmf : newlib/Release/$(PROJECT)_x86_32.nexe newlib/Release/$(PROJECT)_x86_64.nexe
	$(NMF) -D $(NEWLIB_DUMP) -o $@ $^ -t newlib -s newlib/Release


#
# Rules for glibc toolchain
#
glibc:
	$(MKDIR) glibc
glibc/Debug: | glibc
	$(MKDIR) glibc/Debug
glibc/Release: | glibc
	$(MKDIR) glibc/Release

# Include header dependency files.
-include glibc/Debug/*.d
-include glibc/Release/*.d

PPAPI_DEBUG:=$(abspath glibc/Debug/$(PROJECT)_<ARCH>.nexe);application/x-ppapi-debug
glibc/Debug/%_x86_32.o : %.cc $(THIS_MAKE) | glibc/Debug
	$(GLIBC_CXX) -o $@ $< -g -O0 -m32 $(GLIBC_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_GLIBC_DEBUG_x86_32_O:=$(patsubst %,glibc/Debug/%_x86_32.o,$(PROJECT_OBJS))
glibc/Debug/$(PROJECT)_x86_32.nexe : $(PROJECT_GLIBC_DEBUG_x86_32_O)
	$(GLIBC_LINK) -o $@ $^ -g -m32 $(GLIBC_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_32_glibc/Debug $(GLIBC_LIBS32)

glibc/Debug/%_x86_64.o : %.cc $(THIS_MAKE) | glibc/Debug
	$(GLIBC_CXX) -o $@ $< -g -O0 -m64 $(GLIBC_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_GLIBC_DEBUG_x86_64_O:=$(patsubst %,glibc/Debug/%_x86_64.o,$(PROJECT_OBJS))
glibc/Debug/$(PROJECT)_x86_64.nexe : $(PROJECT_GLIBC_DEBUG_x86_64_O)
	$(GLIBC_LINK) -o $@ $^ -g -m64 $(GLIBC_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_64_glibc/Debug $(GLIBC_LIBS64)


ALL_TARGETS+=glibc/Debug/$(PROJECT).nmf
glibc/Debug/$(PROJECT).nmf : glibc/Debug/$(PROJECT)_x86_32.nexe glibc/Debug/$(PROJECT)_x86_64.nexe
	$(NMF) -D $(GLIBC_DUMP) -o $@ $(GLIBC_PATHS) $^ -t glibc -s glibc/Debug $(GLIBC_REMAP)

PPAPI_RELEASE:=$(abspath glibc/Release/$(PROJECT)_x86_64.nexe);application/x-ppapi-release
glibc/Release/%_x86_32.o : %.cc $(THIS_MAKE) | glibc/Release
	$(GLIBC_CXX) -o $@ $< -O2 -m32 $(GLIBC_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_GLIBC_RELEASE_x86_32_O:=$(patsubst %,glibc/Release/%_x86_32.o,$(PROJECT_OBJS))
glibc/Release/$(PROJECT)_x86_32.nexe : $(PROJECT_GLIBC_RELEASE_x86_32_O)
	$(GLIBC_LINK) -o $@ $^ -m32 $(GLIBC_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_32_glibc/Release $(GLIBC_LIBS32)

glibc/Release/%_x86_64.o : %.cc $(THIS_MAKE) | glibc/Release
	$(GLIBC_CXX) -o $@ $< -O2 -m64 $(GLIBC_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_GLIBC_RELEASE_x86_64_O:=$(patsubst %,glibc/Release/%_x86_64.o,$(PROJECT_OBJS))
glibc/Release/$(PROJECT)_x86_64.nexe : $(PROJECT_GLIBC_RELEASE_x86_64_O)
	$(GLIBC_LINK) -o $@ $^ -m64 $(GLIBC_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_x86_64_glibc/Release $(GLIBC_LIBS64)


ALL_TARGETS+=glibc/Release/$(PROJECT).nmf
glibc/Release/$(PROJECT).nmf : glibc/Release/$(PROJECT)_x86_32.nexe glibc/Release/$(PROJECT)_x86_64.nexe
	$(NMF) -D $(GLIBC_DUMP) -o $@ $(GLIBC_PATHS) $^ -t glibc -s glibc/Release $(GLIBC_REMAP)


#
# Rules for linux toolchain
#
linux:
	$(MKDIR) linux
linux/Debug: | linux
	$(MKDIR) linux/Debug
linux/Release: | linux
	$(MKDIR) linux/Release

# Include header dependency files.
-include linux/Debug/*.d
-include linux/Release/*.d

PPAPI_DEBUG:=$(abspath linux/Debug/lib$(PROJECT).so);application/x-ppapi-debug
linux/Debug/%_.o : %.cc $(THIS_MAKE) | linux/Debug
	$(LINUX_CXX) -o $@ $< -g -O0  -fPIC $(LINUX_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_LINUX_DEBUG__O:=$(patsubst %,linux/Debug/%_.o,$(PROJECT_OBJS))
linux/Debug/lib$(PROJECT).so : $(PROJECT_LINUX_DEBUG__O)
	$(LINUX_LINK) -o $@ $^ -g  -shared $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_host/Debug $(LINUX_LIBS)


#ALL_TARGETS+=linux/Debug/$(PROJECT).nmf
linux/Debug/$(PROJECT).nmf : linux/Debug/lib$(PROJECT).so | linux/Debug
	echo {} > $@

PPAPI_RELEASE:=$(abspath linux/Release/lib$(PROJECT).so);application/x-ppapi-release
linux/Release/%_.o : %.cc $(THIS_MAKE) | linux/Release
	$(LINUX_CXX) -o $@ $< -O2  -fPIC $(LINUX_CXXFLAGS) $(PROJECT_CXXFLAGS)  


PROJECT_LINUX_RELEASE__O:=$(patsubst %,linux/Release/%_.o,$(PROJECT_OBJS))
linux/Release/lib$(PROJECT).so : $(PROJECT_LINUX_RELEASE__O)
	$(LINUX_LINK) -o $@ $^  -shared $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/$(OSNAME)_host/Release $(LINUX_LIBS)


#ALL_TARGETS+=linux/Release/$(PROJECT).nmf
linux/Release/$(PROJECT).nmf : linux/Release/lib$(PROJECT).so | linux/Release
	echo {} > $@

#
# Rules for pnacl toolchain
#
pnacl:
	$(MKDIR) pnacl
pnacl/Debug: | pnacl
	$(MKDIR) pnacl/Debug
pnacl/Release: | pnacl
	$(MKDIR) pnacl/Release

# Include header dependency files.
-include pnacl/Debug/*.d
-include pnacl/Release/*.d

PPAPI_DEBUG:=$(abspath pnacl/Debug/$(PROJECT).pexe);application/x-ppapi-debug
pnacl/Debug/%.o : %.cc $(THIS_MAKE) | pnacl/Debug
	$(PNACL_CXX) -o $@ $< -g -O0  $(PNACL_CXXFLAGS) $(PROJECT_CCFLAGS)  


PROJECT_PNACL_DEBUG_O:=$(patsubst %,pnacl/Debug/%.o,$(PROJECT_OBJS))
pnacl/Debug/$(PROJECT).pexe : $(PROJECT_PNACL_DEBUG_O)
	$(PNACL_LINK) -o $@ $^ -g $(PNACL_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/pnacl/Debug $(PNACL_LIBS)

pnacl/Debug/$(PROJECT)_x86_32.nexe : pnacl/Debug/$(PROJECT).pexe
	$(TRANSLATE) -arch x86-32 $< -o $@

pnacl/Debug/$(PROJECT)_x86_64.nexe : pnacl/Debug/$(PROJECT).pexe
	$(TRANSLATE) -arch x86-64 $< -o $@

pnacl/Debug/$(PROJECT)_arm.nexe : pnacl/Debug/$(PROJECT).pexe
	$(TRANSLATE) -arch arm $< -o $@


ifeq "$(NACL_SDK_VERSION_GT_25)" "1"
ALL_TARGETS+=pnacl/Debug/$(PROJECT).nmf
endif

pnacl/Debug/$(PROJECT).nmf : pnacl/Debug/$(PROJECT)_x86_32.nexe pnacl/Debug/$(PROJECT)_x86_64.nexe pnacl/Debug/$(PROJECT)_arm.nexe
	$(NMF) -o $@ $^ -s pnacl/Debug

PPAPI_RELEASE:=$(abspath pnacl/Release/$(PROJECT).pexe);application/x-ppapi-release
pnacl/Release/$(PROJECT).o : $(PROJECT).c $(THIS_MAKE) | pnacl/Release
	$(PNACL_CC) -o $@ $< -O2  $(PNACL_CCFLAGS) $(PROJECT_CCFLAGS)  


PROJECT_PNACL_RELEASE_O:=$(patsubst %,pnacl/Release/%.o,$(PROJECT_OBJS))
pnacl/Release/$(PROJECT).pexe : $(PROJECT_PNACL_RELEASE_O)
	$(PNACL_LINK) -o $@ $^ $(PNACL_LDFLAGS) $(PROJECT_LDFLAGS) -L$(NACL_SDK_ROOT)/lib/pnacl/Release -lppapi -lpthread

pnacl/Release/$(PROJECT)_x86_32.nexe : pnacl/Release/$(PROJECT).pexe
	$(TRANSLATE) -arch x86-32 $< -o $@

pnacl/Release/$(PROJECT)_x86_64.nexe : pnacl/Release/$(PROJECT).pexe
	$(TRANSLATE) -arch x86-64 $< -o $@

pnacl/Release/$(PROJECT)_arm.nexe : pnacl/Release/$(PROJECT).pexe
	$(TRANSLATE) -arch arm $< -o $@

ifeq "$(NACL_SDK_VERSION_GT_25)" "1"
ALL_TARGETS+=pnacl/Release/$(PROJECT).nmf
endif

pnacl/Release/$(PROJECT).nmf : pnacl/Release/$(PROJECT)_x86_32.nexe pnacl/Release/$(PROJECT)_x86_64.nexe pnacl/Release/$(PROJECT)_arm.nexe
	$(NMF) -o $@ $^ -s pnacl/Release


#
# Target to remove temporary files
#
.PHONY: clean
clean:
	$(RM) -fr newlib/Debug
	$(RM) -fr newlib/Release
	$(RM) -fr glibc/Debug
	$(RM) -fr glibc/Release
	$(RM) -fr linux/Debug
	$(RM) -fr linux/Release


all: $(ALL_TARGETS)




RUN: all
	python ../httpd.py

CONFIG?=Debug
PAGE?=index_$(TOOLCHAIN)_$(CONFIG).html

LAUNCH: CHECK_FOR_CHROME all
ifeq (,$(wildcard $(PAGE)))
	$(warning No valid HTML page found at $(PAGE))
	$(error Make sure TOOLCHAIN and CONFIG are properly set)
endif
	$(CHROME_PATH) $(NEXE_ARGS) --register-pepper-plugins="$(PPAPI_DEBUG),$(PPAPI_RELEASE)" localhost:5103/$(PAGE)


