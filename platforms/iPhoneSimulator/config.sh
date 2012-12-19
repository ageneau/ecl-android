#! /bin/bash

sdk=iphonesimulator
sdk_ver=$(xcodebuild -version -sdk ${sdk} SDKVersion)
echo "using SDK version: ${sdk_ver}"

export force_cross_compiling=yes
host=i686-apple-darwin
target=i686-apple-darwin
export SDKROOT=$(xcodebuild -version -sdk ${sdk} Path)
#export CC=$(xcodebuild -sdk ${sdk} -find clang)
export CC=$(xcodebuild -sdk ${sdk} -find gcc)
export CXX="$CC"
export CPP=$(xcodebuild -sdk ${sdk} -find cpp)
export AR=$(xcodebuild -sdk ${sdk} -find ar)
export STRIP=$(xcodebuild -sdk ${sdk} -find strip)
export RANLIB=$(xcodebuild -sdk ${sdk} -find ranlib)
export NM=$(xcodebuild -sdk ${sdk} -find nm)
export CFLAGS="-g -arch i386 -isysroot $SDKROOT -DAPPLE -DIPHONE -D__IPHONE_OS_VERSION_MIN_REQUIRED=30000 -DIPHONE_SIMULATOR -DIPHONE_SDK_VER=${sdk_ver} -DIGNORE_DYNAMIC_LOADING -mmacosx-version-min=10.6 -fmessage-length=0 -fasm-blocks"
export LDFLAGS="-arch i386 -isysroot $SDKROOT"
