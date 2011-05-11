#ECL_GIT=git://ecls.git.sourceforge.net/gitroot/ecls/ecl
#ECL_REV=f0ace1a75932b4fd824268d797e3d4393049a62d
ECL_GIT=git://github.com/ageneau/ecl-mirror.git
ECL_REV=fec62ac6b23e75ad9d46
SLIME_CVSROOT=:pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot
SLIME_REV=2010/01/29

all: clone-ecl checkout-slime patch-slime copy-slime
	echo "ECL directory patched for android"

clone-ecl:
	git clone $(ECL_GIT) ecl
	cd ecl && git checkout $(ECL_REV) -b mobile

checkout-slime:
	cvs -d "$(SLIME_CVSROOT)" co -D "$(SLIME_REV)" slime

patch-slime:
	cd slime && patch -p0 < ../patches/swank-ecl-patches.txt

copy-slime:
	-mkdir -p hello-jni/assets/lisp/slime/contrib/
	cp slime/*.lisp hello-jni/assets/lisp/slime/
	cp slime/contrib/*.lisp hello-jni/assets/lisp/slime/contrib/
