#! /bin/bash

usage()
{
    echo "Usage: source $(basename $BASH_SOURCE) platform"
    echo ""
    echo " platform    -- one of: nacl32, nacl64"
}

[ $# == 1 ] || { usage; return 1; }

platform=$1

case $platform in
    nacl32)
	CFLAGS="-m32"
	LDFLAGS="-m32"
	host=i686-nacl
	arch=i686
	cross_compiler_platform=hostnothreads
	;;
    nacl64)
	CFLAGS="-m64"
	LDFLAGS="-m64"
	host=x86_64-nacl
	arch=x86_64
	cross_compiler_platform=hostnothreads
	;;
    pnacl)
	host=pnacl
	arch=pnacl
	cross_compiler_platform=hostnothreads
	;;
esac


if [[ ! -d "${NACL_SDK_ROOT}" ]]; then
    echo "Can not find NACL SDK in ${NACL_SDK_ROOT}." >& 2
    echo "Please set NACL_SDK_ROOT to your NACL install." >& 2
    return 1
fi

host_os=$(uname -s | sed -e 's/Linux/linux/;s/Darwin/mac/')

case "${host_os}" in
    "linux")
	toolchain_dir="linux_x86"
	;;
    "mac")
	toolchain_dir="mac_x86"
	;;
    *)
	echo "Host platform ${host_os} is not supported" >& 2
	return 1
esac

case $platform in
    nacl*)
	#NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${toolchain_dir}_newlib
	NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${toolchain_dir}_glibc

	export OBJCOPY=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-objcopy)
	export STRIP=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-strip)
	export AR=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-ar)
	export RANLIB=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-ranlib)
	export AS=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-as)
	export CC=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-gcc)
	export CXX=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-g++)
	export CPP=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-cpp)
	export NM=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-nm)
	export GPROF=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-gprof)
	export READEFLF=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-readelf)
	export OBJDUMP=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-objdump)
	;;
    pnacl)
	NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${toolchain_dir}_pnacl/newlib

	export PNACL_DIS=$(echo ${NACL_TOOLCHAIN}/bin/${arch}-dis)
	export OBJCOPY=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-objcopy)
	export STRIP=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-strip)
	export AR=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-ar)
	export RANLIB=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-ranlib)
	export AS=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-as)
	export CC=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-clang)
	export CXX=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-clang++)
	export CPP="$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-clang) -E"
	export NM=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-nm)
	export GPROF=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-gprof)
	export READEFLF=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-readelf)
	export OBJDUMP=$(echo ${NACL_TOOLCHAIN}/bin/${arch}*-objdump)
	;;
esac
