//
//  LispGLKViewController.h
//  OpenGLSample
//
//  Created by Sylvain Ageneau on 11/14/12.
//  Copyright (c) 2012 Self. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <GLKit/GLKit.h>
#import "ecl/ecl.h"

@interface LispGLKViewController : GLKViewController {
cl_object setupGLHandler;
cl_object tearDownGLHandler;
cl_object updateHandler;
cl_object drawInRectHandler;
}

- (void) setSetupGLHandler: (cl_object) fun;
- (void) setTearDownGLHandler: (cl_object) fun;
- (void) setUpdateHandler: (cl_object) fun;
- (void) setRedrawHandler: (cl_object) fun;

@end
