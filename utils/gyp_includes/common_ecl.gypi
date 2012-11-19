{
  'includes': [ './common.gypi' ],

  'variables': {
    'GMP_INSTALL_ROOT_DIR': '/opt/gmp',
    'ECL_INSTALL_ROOT_DIR': '/opt/ecl',
    'SLIME_ROOT_DIR' : '../../slime',
    'ECL_VER': '12.7.1',
    'ecl_platform': 'iPhoneUniversal',
    'LIB_EXT': '.a',

    'ECL_LIBRARIES': [
      '-lasdf',
      '-ldeflate',
      '-ldefsystem',
      '-lecl-cdb',
      '-lecl-curl',
      '-lecl-help',
      '-lecl-quicklisp',
      '-lprofile',
      '-lql-minitar',
      '-lrt',
      '-lsb-bsd-sockets',
      '-lserve-event',
      '-lsockets',
      '-lecl',
      '-leclatomic',
      '-leclffi',
      '-leclgc',
      '-lgmp',
      '-lltdl',
    ],
    'ECL_INCLUDE_DIRS': [
      '<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/include',
      '<(GMP_INSTALL_ROOT_DIR)/<(ecl_platform)/include',
    ],
    'ECL_LDFLAGS': [
      '-L<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/lib',
      '-L<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/lib/ecl-<(ECL_VER)',
      '-L<(GMP_INSTALL_ROOT_DIR)/<(ecl_platform)/lib',
    ],

    'mac_bundle_resources': [
      '<(ECL_INSTALL_ROOT_DIR)/iPhoneOS/lib/ecl-<(ECL_VER)/help.doc',
    ],
    
    'ECL_HOST': '<(ECL_INSTALL_ROOT_DIR)/host/bin/ecl',

    'GEN_SYM': '<(DEPTH)/utils/ltdl/gen_sym.sh',
  },
}
