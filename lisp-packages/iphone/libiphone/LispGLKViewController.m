//
//  LispGLKViewController.m
//  OpenGLSample
//
//  Created by Sylvain Ageneau on 11/14/12.
//  Copyright (c) 2012 Self. All rights reserved.
//

#import "LispGLKViewController.h"
#import "ecl/ecl.h"
#import "lisp_registry.h"


@interface LispGLKViewController () {
}

@property (strong, nonatomic) EAGLContext *context;

- (void)setupGL;
- (void)tearDownGL;

@end

@implementation LispGLKViewController

- (id)initWithNibName:(NSString *)nibName bundle:(NSBundle *)nibBundle
{
	setupGLHandler = Cnil;
	tearDownGLHandler = Cnil;
    drawInRectHandler = Cnil;
    updateHandler = Cnil;
    [super initWithNibName:nibName bundle:nibBundle];
}


- (void) setSetupGLHandler: (cl_object) fun
{
    register_cb(setupGLHandler, fun);
}

- (void) setTearDownGLHandler: (cl_object) fun
{
    register_cb(tearDownGLHandler, fun);
}

- (void) setUpdateHandler: (cl_object) fun
{
    register_cb(updateHandler, fun);
}

- (void) setRedrawHandler: (cl_object) fun
{
    register_cb(drawInRectHandler, fun);
}


- (void)viewDidLoad
{
    [super viewDidLoad];
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];

    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    view.drawableDepthFormat = GLKViewDrawableDepthFormat24;
    
    [self setupGL];
}

- (void)dealloc
{
    [super dealloc];
    
    [self tearDownGL];
    
    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];

    if ([self isViewLoaded] && ([[self view] window] == nil)) {
        self.view = nil;
        
        [self tearDownGL];
        
        if ([EAGLContext currentContext] == self.context) {
            [EAGLContext setCurrentContext:nil];
        }
        self.context = nil;
    }

    // Dispose of any resources that can be recreated.
}

- (void)setupGL
{
    [EAGLContext setCurrentContext:self.context];
    
    if (Cnil != setupGLHandler) {
        cl_funcall(1, setupGLHandler);
    }
}

- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
    
    if (Cnil != tearDownGLHandler) {
		cl_funcall(1, tearDownGLHandler);
	}
}

#pragma mark - GLKView and GLKViewController delegate methods

- (void)update
{
	if (Cnil != updateHandler) {
		cl_funcall(1, updateHandler);
	}
}

- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
	if (Cnil != drawInRectHandler) {
		cl_funcall(6,
		           drawInRectHandler, 
		           ecl_foreign_data_ref_elt(&view,ECL_FFI_POINTER_VOID),
		           ecl_make_single_float(rect.origin.x),
                   ecl_make_single_float(rect.origin.y),
                   ecl_make_single_float(rect.size.width),
                   ecl_make_single_float(rect.size.height));
	}
}


@end
