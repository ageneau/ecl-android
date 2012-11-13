ECL_GIT=git://git.code.sf.net/p/ecls/ecl
ECL_REV=b970c5b206f2590531bab24acadfff165ccb2f92
SLIME_GIT=git://common-lisp.net/projects/mirror/slime.git
SLIME_REV=c92708b2df6b92e8dbc78829fd45ceb62303e994

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
