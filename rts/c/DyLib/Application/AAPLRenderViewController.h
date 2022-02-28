/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the cross-platform Metal rendering view controller.
*/

#if defined(TARGET_IOS)
#include <UIKit/UIKit.h>
#define PlatformViewController UIViewController
#else
#include <AppKit/AppKit.h>
#define PlatformViewController NSViewController
#endif
#include <Metal/MetalKit.h>

class AAPLRenderViewController : PlatformViewController

