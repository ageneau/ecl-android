//
//  UIButtonCB.h
//
//

#import "UIButtonCB.h"
#import "ecl/ecl.h"
#import "lisp_registry.h"

@implementation UIButtonCB

- (id) initWithFrame: (CGRect) aFrame
{
    [super initWithFrame: aFrame];
    clickHandler = Cnil;
    [self addTarget: self action: @selector(clicked:) forControlEvents: UIControlEventTouchUpInside];
    return self;
}

- (void) setClickHandler: (cl_object) fun
{
    register_cb(clickHandler, fun);
}

- (void) clicked: (id) sender
{
    if (Cnil != clickHandler) {
        cl_funcall(2, clickHandler, ecl_foreign_data_ref_elt(&sender,ECL_FFI_POINTER_VOID));
    }
}

- (void) dealloc
{
    remove_cb(clickHandler);
    clickHandler = Cnil;
    [super dealloc];
}

@end
