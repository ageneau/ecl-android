ECL_GIT=git://git.code.sf.net/p/ecls/ecl
ECL_REV=afafaae325b0427e5e144eb653f99f6e3e16d4c0
SLIME_GIT=git://common-lisp.net/projects/mirror/slime.git
SLIME_REV=847d102f3984cf241e11c8057b4d176f91c122dc

all: clone-ecl patch-ecl checkout-slime copy-slime
	echo "ECL directory patched for android"

clone-ecl:
	git clone $(ECL_GIT) ecl
	cd ecl && git checkout $(ECL_REV) -b mobile

patch-ecl:
	cd ecl && for i in ../patches/ecl/*.patch; do patch -p1 < $$i; done
	chmod +x ecl/configure_cross ecl/configure_gmp_cross

checkout-slime:
	git clone $(SLIME_GIT) slime
	cd slime && git checkout $(SLIME_REV) -b mobile

copy-slime:
	-mkdir -p android/hello-jni/assets/lisp/slime/contrib/
	cp slime/*.lisp android/hello-jni/assets/lisp/slime/
	cp slime/contrib/*.lisp android/hello-jni/assets/lisp/slime/contrib/
