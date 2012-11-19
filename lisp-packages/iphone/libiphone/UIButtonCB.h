//
//  UIButtonCB.h
//
//

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import "ecl/ecl.h"

@interface UIButtonCB : UIButton
{
    cl_object clickHandler;
}

- (void) setClickHandler: (cl_object) fun;

@end
