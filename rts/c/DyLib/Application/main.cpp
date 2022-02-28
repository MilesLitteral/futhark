/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Main app entry point.
*/

#if defined(TARGET_IOS)
#include <UIKit/UIKit.h>
#include <TargetConditionals.h>
#include "AAPLAppDelegate.h"
#else
#include <Cocoa/Cocoa.h>
#endif

#if defined(TARGET_IOS)

int main(int argc, char * argv[]) {
    
#if TARGET_OS_SIMULATOR && !defined(__IPHONE_14_0)
#error No simulator support for Metal API for this SDK version. Must build for a device
#endif

    return UIApplicationMain(argc, argv, nil, NSStringFromClass(AAPLAppDelegate class));
}

#elif defined(TARGET_MACOS)

int main(int argc, const char * argv[]) {
    return NSApplicationMain(argc, argv);
}

#endif
