//
//  eclshellAppDelegate.m
//  eclshell
//
//

#import "eclshellAppDelegate.h"
#import "ecl/ecl.h"
#import "ecl/gc/gc.h"
#import "ecl_boot.h"

@implementation eclshellAppDelegate

@synthesize window, eclReplThread;

- (void) postLoadInitialize
{
    const char *curdir = [[[NSBundle mainBundle] resourcePath] UTF8String];
    chdir(curdir);
    ecl_boot([[[NSBundle mainBundle] resourcePath] UTF8String]);
}

- (void) applicationDidFinishLaunching:(UIApplication *) application
{
    [self performSelector: @selector(postLoadInitialize) withObject: nil afterDelay: 0.1f];
    // Override point for customization after application launch
    [window makeKeyAndVisible];
}

- (void) dealloc
{
    [window release];
    [super dealloc];
}


@end
