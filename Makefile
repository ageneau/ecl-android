ECL_INSTALL_ROOT_DIR=./install

TARGETS=host64 host iPhoneOS iPhoneSimulator android androidx86
TARGETS_ECL=$(TARGETS:=.ecl)
TARGETS_GMP=$(TARGETS:=.gmp)


all: $(TARGETS_ECL) $(TARGETS_GMP)

update: update-modules patch-ecl patch-mpir patch-bdwgc patch-cffi copy-slime
	echo "ECL directory patched for android"

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
	cp config.sub config.guess mpir/yasm/config/
	chmod +x mpir/yasm/config/config.*

patch-bdwgc:
	cd bdwgc && git clean -dxf && git checkout HEAD . && for i in ../patches/bdwgc/*.patch; do patch -p1 < $$i; done

copy-slime:
	-mkdir -p android/hello-jni/assets/lisp/slime/contrib/
	cp slime/*.lisp android/hello-jni/assets/lisp/slime/
	cp slime/contrib/*.lisp android/hello-jni/assets/lisp/slime/contrib/


iPhoneSimulator: iPhoneSimulator.ecl

iPhoneOS: iPhoneOS.ecl

ios: iPhoneUniversal.ecl

iPhoneUniversal.ecl: iPhoneOS iPhoneSimulator
	-rm -rf $(ECL_INSTALL_ROOT_DIR)/iPhoneUniversal
	./iPhone_universal
	touch $@

android: android.ecl

androidx86: androidx86.ecl

iPhoneOS.ecl iPhoneSimulator.ecl android.ecl androidx86.ecl : host.ecl

android.ecl: android.gmp android.bdwgc

androidx86.ecl: androidx86.gmp androidx86.bdwgc

iPhoneOS.ecl: iPhoneOS.gmp iPhoneOS.bdwgc

iPhoneSimulator.ecl: iPhoneSimulator.gmp iPhoneSimulator.bdwgc


%.ecl: %.gmp
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


clean:
	rm -f *.ecl *.gmp *.atomic *.bdwgc


.PHONY: all update-modules patch-ecl patch-mpir patch-cffi copy-slime clean update
