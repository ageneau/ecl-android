#! /bin/bash

sdk=iphoneos
sdk_ver=$(xcodebuild -version -sdk ${sdk} SDKVersion)
echo "using SDK version: ${sdk_ver}"

export force_cross_compiling=yes
host=arm-apple-darwin
target=arm-apple-darwin

IOS_SDKROOT=$(xcodebuild -version -sdk ${sdk} Path)
#export CC=$(xcodebuild -sdk ${sdk} -find clang)
export CC=$(xcodebuild -sdk ${sdk} -find gcc)
export CXX="$CC"
export CPP=$(xcodebuild -sdk ${sdk} -find cpp)
export AR=$(xcodebuild -sdk ${sdk} -find ar)
export STRIP=$(xcodebuild -sdk ${sdk} -find strip)
export RANLIB=$(xcodebuild -sdk ${sdk} -find ranlib)
export NM=$(xcodebuild -sdk ${sdk} -find nm)
export CFLAGS="-g -arch armv7s -mios-version-min=5.0 -isysroot $IOS_SDKROOT -DAPPLE -DIPHONE"
export LDFLAGS="-arch armv7s -mios-version-min=5.0 -isysroot $IOS_SDKROOT"
