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
    nacl_x86_32*)
	CFLAGS="-m32 -std=c99 -D_XOPEN_SOURCE=500  -D_POSIX_C_SOURCE=199506L"
	LDFLAGS="-m32"
	host=i686-nacl
	arch=i686
	cross_compiler_platform=hostnothreads
	;;
    nacl_x86_64*)
	CFLAGS="-m64 -std=c99 -D_XOPEN_SOURCE=500  -D_POSIX_C_SOURCE=199506L"
	LDFLAGS="-m64"
	host=x86_64-nacl
	arch=x86_64
	cross_compiler_platform=hostnothreads
	;;
    pnacl*)
	CFLAGS="-std=c99 -D_XOPEN_SOURCE=500  -D_POSIX_C_SOURCE=199506L"
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

NACL_SDK_VERSION=$(basename "$NACL_SDK_ROOT" | cut -d '_' -f2)

host_os=$(uname -s | sed -e 's/Linux/linux/;s/Darwin/mac/')

case "${host_os}" in
    "linux"|"mac")
	;;
    *)
	echo "Host platform ${host_os} is not supported" >& 2
	return 1
esac

case $platform in
    nacl*newlib)
	NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${host_os}_x86_newlib
    ;;
    nacl*glibc)
	NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${host_os}_x86_glibc
    ;;
    pnacl*)
        if [[ $NACL_SDK_VERSION -lt 31 ]]; then
	    NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${host_os}_x86_pnacl/newlib
        else
/opt/nacl/pepper_31/toolchain/mac_pnacl/
	    NACL_TOOLCHAIN=$NACL_SDK_ROOT/toolchain/${host_os}_pnacl
        fi
    ;;
esac
case $platform in
    nacl*)
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
    pnacl*)
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
