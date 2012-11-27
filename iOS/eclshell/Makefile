# This makefile is invoked by Xcode to rebuild libiphone
# We can't invoke ecl directly in the script because it hangs the build process
# (not sure why)

ECL=/opt/ecl/host/bin/ecl

all: libiphone_ios_universal.a

libiphone_ios_universal.a:
	$(ECL) -norc -shell build
	mv libiphone.system_ios_universal.a libiphone_ios_universal.a

.PHONY: clean

clean:
	-rm -rf libiphone*.a cache
