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
      '-leclgc',
      '-lgmp',
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
    
    'ECL_HOST': '<(ECL_INSTALL_ROOT_DIR)/host/bin/ecl',

    'GEN_SYM': '<(DEPTH)/utils/ltdl/gen_sym.sh',
  },
}
