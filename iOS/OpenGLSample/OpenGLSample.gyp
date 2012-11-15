{
  'includes': [ '../../utils/gyp_includes/common_ecl.gypi' ],

  'variables' : {
    'project_root': 'OpenGLSample',
    'project_name': 'OpenGLSample',
  },

  'targets': [
    {
      'target_name': 'All',
      'type': 'executable',
      'mac_bundle': 1,
      'product_name': '<(project_name)',

      'dependencies': [
      ],
      'include_dirs': [
        '<(ECL_INCLUDE_DIRS)',
        '<(project_root)/Lisp',
      ],
      'sources': [
        '<(project_root)/AppDelegate.h',
        '<(project_root)/AppDelegate.m',
        '<(project_root)/main.m',
        '<(project_root)/ViewController.h',
        '<(project_root)/ViewController.m',
        '<(project_root)/Lisp/ecl_boot.c',
        '<(project_root)/Lisp/ecl_boot.h',
        '<(project_root)/<(project_name)-Prefix.pch',
      ],
      'mac_bundle_resources': [
        '<(project_root)/Default-568h@2x.png',
        '<(project_root)/Default.png',
        '<(project_root)/Default@2x.png',
        '<(project_root)/en.lproj/InfoPlist.strings',
        '<(project_root)/en.lproj/MainStoryboard_iPad.storyboard',
        '<(project_root)/en.lproj/MainStoryboard_iPhone.storyboard',
        '<(project_root)/Shaders/Shader.fsh',
        '<(project_root)/Shaders/Shader.vsh',
        '<(project_root)/Resources/init.lisp',
        '<(project_root)/Resources/help.doc',
        '<(project_root)/Resources/slime',
#        '<(project_root)/<(project_name)-Info.plist',
      ],
      'link_settings' : {
        'libraries' : [
          '$(SDKROOT)/System/Library/Frameworks/Foundation.framework',
          '$(SDKROOT)/System/Library/Frameworks/UIKit.framework',
          '$(SDKROOT)/System/Library/Frameworks/OpenGLES.framework',
          '$(SDKROOT)/System/Library/Frameworks/GLKit.framework',
        ],
      },
      'xcode_settings': {
        'INFOPLIST_FILE': '<(project_root)/<(project_name)-Info.plist',
        'GCC_PREFIX_HEADER': '<(project_root)/<(project_name)-Prefix.pch',
        'GCC_PRECOMPILE_PREFIX_HEADER': 'YES',
        'OTHER_LDFLAGS' : [
          '<@(ECL_LDFLAGS)',
          '<@(ECL_LIBRARIES)',
          '-lapp-bundle-mono-cross',
#          '-lalexandria-mono-cross',
#          '-lbabel-mono-cross',
#          '-ltrivial-features-mono-cross',
        ],
      },
    },
  ],
}

# Local Variables:
# tab-width:2
# indent-tabs-mode:nil
# End:
# vim: set expandtab tabstop=2 shiftwidth=2:
