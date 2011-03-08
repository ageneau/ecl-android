ECL_GIT=git://ecls.git.sourceforge.net/gitroot/ecls/ecl
ECL_REV=5fcd784bb9d4990e342bfa065c0ce28c18787e3d
BDWGC_CVSROOT=:pserver:anonymous@bdwgc.cvs.sourceforge.net:/cvsroot/bdwgc
BDWGC_REV=2011/02/22
SLIME_CVSROOT=:pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot
SLIME_REV=2010/01/29

all: clone-ecl checkout-bdwgc replace-bdwgc patch-ecl checkout-slime patch-slime copy-slime
	echo "ECL directory patched for android"

clone-ecl:
	git clone $(ECL_GIT)
	cd ecl && git checkout $(ECL_REV) -b ecl-android

checkout-bdwgc:
	cvs -d "$(BDWGC_CVSROOT)" co -D "$(BDWGC_REV)" bdwgc

replace-bdwgc:
	rm -rf ./ecl/src/gc
	cp -r ./bdwgc ./ecl/src/gc

checkout-slime:
	cvs -d "$(SLIME_CVSROOT)" co -D "$(SLIME_REV)" slime

patch-slime:
	cd slime && patch -p0 < ../patches/swank-ecl-patches.txt

copy-slime:
	-mkdir -p hello-jni/assets/lisp/slime/contrib/
	cp slime/*.lisp hello-jni/assets/lisp/slime/
	cp slime/contrib/*.lisp hello-jni/assets/lisp/slime/contrib/

patch-ecl:
	cd ecl && patch -p1 < ../patches/android-ecl-gc-patch
	cd ecl && patch -p1 < ../patches/build-scripts
	cd ecl && patch -p1 < ../patches/ecl-android
	chmod +x ecl/configure_cross ecl/configure_gmp_cross
