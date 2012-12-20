{
  'includes': [ './common.gypi' ],

  'variables': {
    'ECL_INSTALL_ROOT_DIR': '../../local-install',
    'SLIME_ROOT_DIR' : '../../slime',
    'ecl_platform': 'iPhoneUniversal',
    'ECL_VER': '<!(basename <(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/lib/ecl-* |cut -d "-" -f2)',
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
      '-latomic_ops',
      '-lgc',
      '-lgmp',
    ],

    'ECL_INCLUDE_DIRS': [
      '<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/include',
    ],

    'ECL_LDFLAGS': [
      '-L<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/lib',
      '-L<(ECL_INSTALL_ROOT_DIR)/<(ecl_platform)/lib/ecl-<(ECL_VER)',
    ],
    
    'ECL_HOST': '<(ECL_INSTALL_ROOT_DIR)/host/bin/ecl',
  },
}
