{
  'variables' : {
    'project_root': 'libiphone',
    'project_name': 'libiphone',
  },

  'includes': [ '../../utils/gyp_includes/common_ecl.gypi' ],

  'targets': [
    {
      'target_name': 'All',
      'type': 'static_library',
      'product_name': '<(project_name)',

      'direct_dependent_settings': {
        'include_dirs': [
          '<(project_root)',
        ],
      },

      'include_dirs': [
        '<(ECL_INCLUDE_DIRS)',

      ],

      'sources': [
        '<(project_root)/UIButtonCB.h',
        '<(project_root)/UIButtonCB.m',
        '<(project_root)/LispGLKViewController.h',
        '<(project_root)/LispGLKViewController.m',
        '<(project_root)/lisp_registry.c',
        '<(project_root)/lisp_registry.h',
      ],

      'xcode_settings': {
        'CLANG_ENABLE_OBJC_ARC': 'NO',
      },
    },
  ],
}

# Local Variables:
# tab-width:2
# indent-tabs-mode:nil
# End:
# vim: set expandtab tabstop=2 shiftwidth=2:
