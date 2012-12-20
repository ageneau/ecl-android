ECL_INSTALL_ROOT_DIR=./local-install

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

patch-bdwgc:
	cd bdwgc && git clean -dxf && git checkout HEAD . && for i in ../patches/bdwgc/*.patch; do patch -p1 < $$i; done

copy-slime:
	-mkdir -p android/HelloEcl/assets/lisp/slime/contrib/
	cp slime/*.lisp android/HelloEcl/assets/lisp/slime/
	cp slime/*.asd android/HelloEcl/assets/lisp/slime/
	cp slime/contrib/*.lisp android/HelloEcl/assets/lisp/slime/contrib/


iPhoneSimulator: iPhoneSimulator.ecl

iPhoneOS: iPhoneOS.ecl

ios: iPhoneUniversal.ecl

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


clean:
	rm -f *.ecl *.gmp *.atomic *.bdwgc


.PHONY: all update-modules patch-ecl patch-mpir patch-cffi copy-slime clean update
