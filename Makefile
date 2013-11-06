ECL_INSTALL_ROOT_DIR=./local-install

TARGETS:=host64 host hostnothreads host64nothreads
TARGET_LIST:=

ifeq ($(UNAME),Darwin)
TARGETS+=iPhoneOS iPhoneSimulator
endif

ifneq ($(ANDROID_NDK_ROOT),)
TARGETS+=android androidx86
endif

ifneq ($(NACL_SDK_ROOT),)
TARGETS+=nacl_x86_32_glibc nacl_x86_64_glibc nacl_x86_32_newlib nacl_x86_64_newlib
NACL_SDK_VER:=$(shell basename "$(NACL_SDK_ROOT)" | cut -d '_' -f2)
NACL_SDK_VERSION_GT_25:=$(shell expr $(NACL_SDK_VER) \>= 25)
ifeq "$(NACL_SDK_VERSION_GT_25)" "1"
TARGETS+=pnacl
endif
endif

TARGETS_ECL:=$(TARGETS:=.ecl)
TARGETS_GMP:=$(TARGETS:=.gmp)
TARGETS_ATOMIC:=$(TARGETS:=.atomic)
TARGETS_BDWGC:=$(TARGETS:=.bdwgc)

UNAME:=$(shell uname)


all: $(TARGETS_ECL) $(TARGETS_GMP) $(TARGETS_ATOMIC) $(TARGETS_GMP)

update: update-modules patch-ecl patch-mpir patch-atomic patch-bdwgc patch-cffi copy-slime
	echo "ECL directory patched for android/iOS/NaCL"

update-modules:
	git submodule init
	git submodule update

patch-ecl:
	cd ecl && git clean -dxf && git checkout HEAD . && for i in ../patches/ecl/*.patch; do patch -p1 < $$i; done
	cp config.sub config.guess ecl/src/
	cp config.sub config.guess ecl/src/gmp/
	chmod +x ecl/src/config.* ecl/src/gmp/config.*

patch-cffi:
	cd lisp-packages/cffi && git clean -dxf && git checkout HEAD . && for i in ../../patches/cffi/*.patch; do patch -p1 < $$i; done

patch-mpir:
	cd mpir && git clean -dxf && git checkout HEAD . && for i in ../patches/mpir/*.patch; do patch -p1 < $$i; done

patch-bdwgc:
	cd bdwgc && git clean -dxf && git checkout HEAD . && for i in ../patches/bdwgc/*.patch; do patch -p1 < $$i; done
	cp -f config.sub config.guess nacl/libgc/

patch-atomic:
	cd libatomic_ops && git clean -dxf && git checkout HEAD . && for i in ../patches/libatomic_ops/*.patch; do patch -p1 < $$i; done

copy-slime:
	-mkdir -p android/HelloEcl/assets/lisp/slime/contrib/
	cp slime/*.lisp android/HelloEcl/assets/lisp/slime/
	cp slime/*.asd android/HelloEcl/assets/lisp/slime/
	cp slime/contrib/*.lisp android/HelloEcl/assets/lisp/slime/contrib/


iPhoneSimulator: iPhoneSimulator.ecl

iPhoneOS: iPhoneOS.ecl

ios: iPhoneUniversal.ecl

nacl: nacl_x86_32_glibc.ecl nacl_x86_64_glibc.ecl

nacl_x86_32_glibc.ecl nacl_x86_64_glibc.ecl nacl_x86_32_newlib.ecl nacl_x86_64_newlib.ecl pnacl: hostnothreads.ecl

iPhoneUniversal.ecl: iPhoneOS iPhoneSimulator
	-rm -rf $(ECL_INSTALL_ROOT_DIR)/iPhoneUniversal
	./bin/iphone_universal $(ECL_INSTALL_ROOT_DIR)
	touch $@

android: android.ecl


androidx86: androidx86.ecl


iPhoneOS.ecl iPhoneSimulator.ecl android.ecl androidx86.ecl : host.ecl


%.ecl: %.gmp %.bdwgc
	-rm -rf build/$(@:.ecl=)/ecl
	./configure ecl $(@:.ecl=)
	cd build/$(@:.ecl=)/ecl && make all install
	touch $@

%.gmp:
	-rm -rf build/$(@:.gmp=)/mpir
	./configure mpir $(@:.gmp=)
	cd build/$(@:.gmp=)/mpir && make all install

	touch $@

%.atomic:
	-rm -rf build/$(@:.atomic=)/libatomic_ops
	./configure libatomic_ops $(@:.atomic=)
	cd build/$(@:.atomic=)/libatomic_ops && make all install

	touch $@

%.bdwgc: %.atomic
	-rm -rf build/$(@:.bdwgc=)/bdwgc
	./configure bdwgc $(@:.bdwgc=)
	cd build/$(@:.bdwgc=)/bdwgc && make all install

	touch $@

nacl_x86_32_glibc.atomic nacl_x86_64_glibc.atomic nacl_x86_32_newlib.atomic nacl_x86_64_newlib.atomic pnacl.atomic:
	touch $@

clean:
	rm -f *.ecl *.gmp *.atomic *.bdwgc


.PHONY: all update-modules patch-ecl patch-mpir patch-cffi copy-slime clean update
