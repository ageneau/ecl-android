ECL_GIT=git://ecls.git.sourceforge.net/gitroot/ecls/ecl
ECL_REV=0df960178922fb0dba752356e59d6194975f333e
BDWGC_CVSROOT=:pserver:anonymous@bdwgc.cvs.sourceforge.net:/cvsroot/bdwgc
BDWGC_REV=2011/02/22

all: clone-ecl checkout-bdwgc replace-bdwgc patch-ecl
	echo "ECL directory patched for android"

clone-ecl:
	git clone $(ECL_GIT)
	cd ecl && git checkout $(ECL_REV) -b ecl-android

checkout-bdwgc:
	cvs -d "$(BDWGC_CVSROOT)" co -D "$(BDWGC_REV)" bdwgc

replace-bdwgc:
	rm -rf ./ecl/src/gc
	cp -r ./bdwgc ./ecl/src/gc

patch-ecl:
	cd ecl && patch -p1 < ../patches/android-ecl-gc-patch
	cd ecl && patch -p1 < ../patches/build-scripts
	cd ecl && patch -p1 < ../patches/ecl-android
	chmod +x ecl/configure_cross ecl/configure_gmp_cross
