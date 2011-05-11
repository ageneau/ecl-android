ANDROID_ROOT=/opt/android/android-ndk-r5
SYSROOT=$(ANDROID_ROOT)/platforms/android-9/arch-arm

CC=$(ANDROID_ROOT)/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin/arm-linux-androideabi-gcc
CFLAGS=--sysroot=$(SYSROOT) -DANDROID  -DPLATFORM_ANDROID -I/opt/gmp/android/include -I/opt/ecl/android/include -I/opt/ecl/android/include/ecl -DNDK_DEBUG=1 -g
LDFLAGS=--sysroot=$(SYSROOT) -g
LIBS=-L/opt/ecl/android/lib -lecl -L/opt/gmp/android/lib -lgmp -leclgc -llog -landroid -L/opt/ecl/android/lib/ecl-11.1.1 -lsockets -lbytecmp
GDB=/opt/android/android-ndk-r5/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin/arm-linux-androideabi-gdb

SRC=main.c
OBJ=${SRC:.c=.o}



all: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) $(LIBS) -o ecl

.c.o:
	$(CC) $(CFLAGS) -c $<

upload:
	adb push ./ecl /data/tmp/

run:
	adb shell /data/tmp/ecl

debug:
	adb forward tcp:2000 tcp:2000
	adb shell /data/tmp/gdbserver host:2000 /data/tmp/ecl
#	adb shell /data/tmp/gdbserver host:2000 /data/tmp/slime/ecl

gdb:
	$(GDB) -x gdb.setup

clean:
	-rm -f $(OBJ) ./ecl
