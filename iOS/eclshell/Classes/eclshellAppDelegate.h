//
//  eclshellAppDelegate.h
//  eclshell
//

#import <UIKit/UIKit.h>

@interface eclshellAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    NSThread *eclReplThread;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) NSThread *eclReplThread;

@end

