#! /bin/bash


usage()
{
    echo "Usage: source $(basename $BASH_SOURCE) platform"
    echo ""
    echo " platform    -- one of: android, androidx86"
}

[ $# == 1 ] || { usage; return 1; }

platform=$1
export force_cross_compiling=yes

case $platform in
    android)
	host=arm-linux-androideabi
	target=arm-linux-androideabi
	arch=arm
	toolchain_arch="arm-linux-androideabi"
	android_target=android-4
	;;
    androidx86)
	host=i686-android-linux
	target=i686-android-linux
	arch=x86
	toolchain_arch="x86"
	# X86 and MIPS ABIs are only supported at API 9 or higher.
	android_target=android-9
	;;
esac

if [[ ! -d "${ANDROID_NDK_ROOT}" ]]; then
    echo "Can not find Android NDK in ${ANDROID_NDK_ROOT}." >& 2
    echo "Please set ANDROID_NDK_ROOT to your NDK install." >& 2
    echo "e.g:" >& 2
    echo "export ANDROID_NDK_ROOT=/opt/android/android-ndk-r8b" >& 2
    return 1
fi

host_os=$(uname -s | sed -e 's/Linux/linux/;s/Darwin/mac/')
host_platform=$(uname -m)

case "${host_os}" in
  "linux")
        case "${host_platform}" in
            "x86_64")
                toolchain_dir="linux-x86_64"
                ;;
            *)
                toolchain_dir="linux-x86"
                ;;
        esac
        ;;
  "mac")
    toolchain_dir="darwin-x86"
    ;;
  *)
    echo "Host platform ${host_os} is not supported" >& 2
    return 1
esac

toolchain_version="4.6"

toolchain_target=$(basename \
    ${ANDROID_NDK_ROOT}/toolchains/${toolchain_arch}-${toolchain_version})
toolchain_path="${ANDROID_NDK_ROOT}/toolchains/${toolchain_target}"\
"/prebuilt/${toolchain_dir}/bin/"
sysroot="${ANDROID_NDK_ROOT}/platforms/${android_target}/arch-${arch}"

export ANDROID_TOOLCHAIN=${ANDROID_TOOLCHAIN:-${toolchain_path}}

if [[ ! -d "${ANDROID_TOOLCHAIN}" ]]; then
    echo "Can not find Android toolchain in ${ANDROID_TOOLCHAIN}." >& 2
    echo "The NDK version might be wrong." >& 2
    return 1
fi
  
export OBJCOPY=$(echo ${ANDROID_TOOLCHAIN}/*-objcopy)
export STRIP=$(echo ${ANDROID_TOOLCHAIN}/*-strip)
export AR=$(echo ${ANDROID_TOOLCHAIN}/*-ar)
export RANLIB=$(echo ${ANDROID_TOOLCHAIN}/*-ranlib)
export AS=$(echo ${ANDROID_TOOLCHAIN}/*-as)
export CC=$(echo ${ANDROID_TOOLCHAIN}/*-gcc)
export CXX=$(echo ${ANDROID_TOOLCHAIN}/*-g++)
export CPP=$(echo ${ANDROID_TOOLCHAIN}/*-cpp)
export NM=$(echo ${ANDROID_TOOLCHAIN}/*-nm)
export GPROF=$(echo ${ANDROID_TOOLCHAIN}/*-gprof)
export READEFLF=$(echo ${ANDROID_TOOLCHAIN}/*-readelf)
export OBJDUMP=$(echo ${ANDROID_TOOLCHAIN}/*-objdump)
export CPPFLAGS="--sysroot=${sysroot}"
export LDFLAGS="--sysroot=${sysroot}"
