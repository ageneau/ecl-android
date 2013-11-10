#!/bin/bash
# Copyright (c) 2013 The Native Client Authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

source pkg_info
source $NACL_PORTS/src/build_tools/common.sh

BuildStep() {
  # The sample is built using the NaCl SDK common.mk system.
  # We override $(OUTBASE) to force the build system to put
  # all its artifacts in ${NACL_PACKAGES_REPOSITORY} rather
  # than alongside the Makefile.
  export OUTBASE=${NACL_PACKAGES_REPOSITORY}/${PACKAGE_DIR}
  export NACLPORTS_INCLUDE
  export NACLPORTS_PREFIX
  export NACL_PACKAGES_PUBLISH
  export NACL_SRC
  export CFLAGS=${NACLPORTS_CFLAGS}
  export CXXFLAGS=${NACLPORTS_CXXFLAGS}
  export LDFLAGS=${NACLPORTS_LDFLAGS}

  MakeDir ${OUTBASE}
  if [ "${NACL_GLIBC}" = "1" ]; then
    MAKEFLAGS+=" TOOLCHAIN=glibc"
  elif [ "${NACL_ARCH}" = "pnacl" ]; then
    MAKEFLAGS+=" TOOLCHAIN=pnacl"
  else
    MAKEFLAGS+=" TOOLCHAIN=newlib"
  fi
  if [ "${NACL_DEBUG}" = "1" ]; then
    MAKEFLAGS+=" CONFIG=Debug"
  else
    MAKEFLAGS+=" CONFIG=Release"
  fi
  if [ "${NACL_ARCH}" != "pnacl" ]; then
    MAKEFLAGS+=" NACL_ARCH=${NACL_ARCH_ALT}"
  fi
  MAKEFLAGS+=" V=1"
  export MAKEFLAGS
  ChangeDir ${START_DIR}
  DefaultBuildStep
}

PackageInstall() {
  PreInstallStep
  BuildStep
  InstallStep
}

PackageInstall
exit 0
