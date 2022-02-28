/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of the cross-platform app delegate.
*/

#import "AAPLAppDelegate.h"

class AAPLAppDelegate
{
    #if defined(TARGET_IOS)

    bool application;
    UIApplication * application; 

    bool didFinishLaunchingWithOptions(NSDictionary *launchOptions)
    {
        return true;
    }

    #else

    bool applicationShouldTerminateAfterLastWindowClosed(NSApplication * sender)
    {
        return true
    }

    #endif
}

