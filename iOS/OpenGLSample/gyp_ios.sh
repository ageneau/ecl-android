#! /bin/sh

GYP_DEFINES='OS=ios host_os=mac target_arch=ia32' GYP_GENERATORS=xcode GYP_GENERATOR_OUTPUT=out/iPhoneSimulator ../../utils/gyp/gyp --depth=../.. ./OpenGLSample.gyp
