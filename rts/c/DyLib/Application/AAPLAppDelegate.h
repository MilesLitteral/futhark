/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the cross-platform app delegate.
*/

#if defined(TARGET_IOS)
#include <UIKit/UIKit.h>
#define PlatformAppDelegate UIResponder<UIApplicationDelegate>
#define PlatformWindow UIWindow
#else
#include <AppKit/AppKit.h>
#define PlatformAppDelegate NSObject<NSApplicationDelegate>
#define PlatformWindow NSWindow
#endif

class AAPLAppDelegate : PlatformAppDelegate

void PlatformWindow *window;
