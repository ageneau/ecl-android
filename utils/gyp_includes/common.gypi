{
  'variables' : {
    'ios_sdk%': '',
    'ios_sdk_path%': '',
    'ios_deployment_target%': '4.3',
    'ios_signing' : 1,

    'conditions': [
      ['OS=="ios"', {
          # See http://gcc.gnu.org/onlinedocs/gcc-4.4.2/gcc/Optimize-Options.html
          'mac_release_optimization%': 's', # Use -Os unless overridden
          'mac_debug_optimization%': '0',   # Use -O0 unless overridden
          'ios_sdk_path%': '<!(xcodebuild -version -sdk iphoneos Path)',
          'iossim_sdk_path%': '<!(xcodebuild -version -sdk iphonesimulator Path)',
          }, {
          # See http://gcc.gnu.org/onlinedocs/gcc-4.4.2/gcc/Optimize-Options.html
          'mac_release_optimization%': '3', # Use -O3 unless overridden
          'mac_debug_optimization%': '0',   # Use -O0 unless overridden
        }],
    ],
  },

  'conditions': [
    ['OS=="ios"', {
        'target_defaults': {
          'xcode_settings': {
            'ALWAYS_SEARCH_USER_PATHS': 'NO',
            # Don't link in libarclite_macosx.a, see http://crbug.com/156530.
            'CLANG_LINK_OBJC_RUNTIME': 'NO',          # -fno-objc-link-runtime
            'GCC_C_LANGUAGE_STANDARD': 'c99',         # -std=c99
            'GCC_CW_ASM_SYNTAX': 'NO',                # No -fasm-blocks
            'GCC_ENABLE_CPP_EXCEPTIONS': 'NO',        # -fno-exceptions
            'GCC_ENABLE_CPP_RTTI': 'NO',              # -fno-rtti
            'GCC_ENABLE_PASCAL_STRINGS': 'NO',        # No -mpascal-strings
            'GCC_INLINES_ARE_PRIVATE_EXTERN': 'YES',
            'GCC_OBJC_CALL_CXX_CDTORS': 'YES',        # -fobjc-call-cxx-cdtors
            'GCC_SYMBOLS_PRIVATE_EXTERN': 'YES',      # -fvisibility=hidden
            'GCC_THREADSAFE_STATICS': 'NO',           # -fno-threadsafe-statics
#            'GCC_TREAT_WARNINGS_AS_ERRORS': 'YES',    # -Werror
#            'GCC_VERSION': '4.2',
            'GCC_WARN_ABOUT_MISSING_NEWLINE': 'YES',  # -Wnewline-eof
            'USE_HEADERMAP': 'NO',
            'CLANG_ENABLE_OBJC_ARC': 'YES',

            'conditions': [
              ['ios_sdk_path==""', {
                  'SDKROOT': 'iphoneos<(ios_sdk)',  # -isysroot
                  }, {
                  'SDKROOT': '<(ios_sdk_path)',  # -isysroot
                }],
            ],

            # Just build armv7 since iOS 4.3+ only supports armv7.
            'ARCHS': '$(ARCHS_UNIVERSAL_IPHONE_OS)',
            'IPHONEOS_DEPLOYMENT_TARGET': '<(ios_deployment_target)',
            # Target both iPhone and iPad.
            'TARGETED_DEVICE_FAMILY': '1,2',
          },
          'target_conditions': [
            ['_type=="executable"', {
                'configurations': {
                  'Release_Base': {
                    'xcode_settings': {
                      'DEPLOYMENT_POSTPROCESSING': 'YES',
                      'STRIP_INSTALLED_PRODUCT': 'YES',
                    },
                  },
                },
                'xcode_settings': {
                  'conditions': [
                    ['ios_signing', {
                        # iOS SDK wants everything for device signed.
                        'CODE_SIGN_IDENTITY[sdk=iphoneos*]': 'iPhone Developer',
                        }, {
                        'CODE_SIGNING_REQUIRED': 'NO',
                        'CODE_SIGN_IDENTITY[sdk=iphoneos*]': '',
                      }],
                  ],
                },
              }],
            ],  # target_conditions
        },
      },
   ],
  ],
}
