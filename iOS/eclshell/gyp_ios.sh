#! /bin/sh

# export GYP_GENERATOR_OUTPUT=out/iPhoneSimulator
GYP_DEFINES='OS=ios host_os=mac target_arch=ia32' GYP_GENERATORS='xcode' ../../utils/gyp/gyp ./eclshell.gyp
