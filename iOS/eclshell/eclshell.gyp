{
  'includes': [ '../../utils/gyp_includes/common_ecl.gypi' ],

  'variables' : {
    'project_root': '.',
    'project_name': 'eclshell',
  },

  'targets': [
    {
      'target_name': 'All',
      'type': 'executable',
      'mac_bundle': 1,
      'product_name': '<(project_name)',

      'dependencies': [
				'../../lisp-packages/iphone/libiphone.gyp:*',
      ],

      'include_dirs': [
        '<(ECL_INCLUDE_DIRS)',
        '<(project_root)',
      ],
      'sources': [
        '<(project_root)/Classes/eclshellAppDelegate.h',
        '<(project_root)/Classes/eclshellAppDelegate.m',
        '<(project_root)/ecl_boot.h',
        '<(project_root)/ecl_boot.c',
        '<(project_root)/main.m',
        '<(project_root)/<(project_name)-Prefix.pch',
      ],
      'mac_bundle_resources': [
        '<(project_root)/Icon.png',
        '<(project_root)/MainWindow.xib',
        '<(project_root)/init.lisp',
        '<(project_root)/help.doc',
        '<(SLIME_ROOT_DIR)',
      ],
      'link_settings' : {
        'libraries' : [
          '$(SDKROOT)/System/Library/Frameworks/Foundation.framework',
          '$(SDKROOT)/System/Library/Frameworks/UIKit.framework',
          '$(SDKROOT)/System/Library/Frameworks/CoreGraphics.framework',

          # These framework are added here because the iphone package depends on them.
          # You can remove that dependency by removing the GLKit/OpenGLES stuff from lisp-packages/iphone
          '$(SDKROOT)/System/Library/Frameworks/OpenGLES.framework',
          '$(SDKROOT)/System/Library/Frameworks/GLKit.framework',
          '<(INTERMEDIATE_DIR)/libiphone_ios_universal.a',
        ],
      },
      'xcode_settings': {
        'INFOPLIST_FILE': '<(project_root)/<(project_name)-Info.plist',
        'GCC_PREFIX_HEADER': '<(project_root)/<(project_name)-Prefix.pch',
        'GCC_PRECOMPILE_PREFIX_HEADER': 'YES',
        'CLANG_ENABLE_OBJC_ARC': 'NO',
        'OTHER_LDFLAGS' : [
          '<@(ECL_LDFLAGS)',
          '<@(ECL_LIBRARIES)',
          '-L<(project_root)',
        ],
      },
      'actions': [
        {
          'action_name': 'genlibiphone',
          'inputs': [
          ],
          'outputs': [
            'libiphone_ios_universal.a',
          ],
          'action': [
            'make', 
          ],
        },
      ],

      'copies': [
        {
          'destination': '<(INTERMEDIATE_DIR)/',
          'files': [
            'libiphone_ios_universal.a',
          ],
        },
      ],

    },
  ],
}

# Local Variables:
# tab-width:2
# indent-tabs-mode:nil
# End:
# vim: set expandtab tabstop=2 shiftwidth=2:
