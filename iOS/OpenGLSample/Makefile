# This makefile is invoked by Xcode to rebuild libiphone
# We can't invoke ecl directly in the script because it hangs the build process
# (not sure why)

IOS_SDK_PATH:=$(shell xcodebuild -version -sdk iphonesimulator Path)

all: libopenglsample.a

libopenglsample.a:
	ecl -norc -shell build
	mv libopenglsample.system_ios_universal.a $@

ltdl-symbol-table.oS.c:
	../../utils/ltdl/gen_sym.sh $(IOS_SDK_PATH)/System/Library/Frameworks/OpenGLES.framework/OpenGLES
	sed -i -e 's/{"OpenGLES", (void \*) 0},/{"libOpenGLES.dylib", (void *) 0},/' $@

.PHONY: clean libopenglsample.a

clean:
	-rm -rf libopenglsample*.a cache
