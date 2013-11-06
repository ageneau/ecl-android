#! /bin/bash

sdk=iphonesimulator
sdk_ver=$(xcodebuild -version -sdk ${sdk} SDKVersion)
echo "using SDK version: ${sdk_ver}"

export force_cross_compiling=yes
host=i686-apple-darwin
target=i686-apple-darwin
IOS_SDKROOT=$(xcodebuild -version -sdk ${sdk} Path)
#export CC=$(xcodebuild -sdk ${sdk} -find clang)
export CC=$(xcodebuild -sdk ${sdk} -find gcc)
export CXX=$(xcodebuild -sdk ${sdk} -find g++)
export CPP=$(xcodebuild -sdk ${sdk} -find cpp)
export AR=$(xcodebuild -sdk ${sdk} -find ar)
export STRIP=$(xcodebuild -sdk ${sdk} -find strip)
export RANLIB=$(xcodebuild -sdk ${sdk} -find ranlib)
export NM=$(xcodebuild -sdk ${sdk} -find nm)
export CFLAGS="-g -arch i386 -mios-simulator-version-min=5.0 -isysroot $IOS_SDKROOT -DAPPLE -DIPHONE -D__IPHONE_OS_VERSION_MIN_REQUIRED=30000 -DIPHONE_SIMULATOR -DIPHONE_SDK_VER=${sdk_ver} -DIGNORE_DYNAMIC_LOADING -fmessage-length=0 -fasm-blocks"
export LDFLAGS="-arch i386 -mios-simulator-version-min=5.0 -isysroot $IOS_SDKROOT"
