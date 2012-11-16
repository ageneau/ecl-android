all: update-modules patch-ecl copy-slime
	echo "ECL directory patched for android"

update-modules:
	git submodule init
	git submodule update

patch-ecl:
	cd ecl && for i in ../patches/ecl/*.patch; do patch -p1 < $$i; done
	chmod +x ecl/configure_cross ecl/configure_gmp_cross

copy-slime:
	-mkdir -p android/hello-jni/assets/lisp/slime/contrib/
	cp slime/*.lisp android/hello-jni/assets/lisp/slime/
	cp slime/contrib/*.lisp android/hello-jni/assets/lisp/slime/contrib/
