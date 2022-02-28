/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the cross-platform text editing view controller.
*/

#if defined(TARGET_IOS)
#include <UIKit/UIKit.h>
#define PlatformViewController UIViewController
#else
#include <AppKit/AppKit.h>;
#define PlatformViewController NSViewController
#endif

#include <Metal/MetalKit.h>
#include "AAPLRenderer.h"

class AAPLEditViewController : PlatformViewController;

#if defined(TARGET_IOS)
@property (weak, nonatomic) IBOutlet UITextView *textView;
#else
@property (unsafe_unretained) IBOutlet NSTextView *textView;
#endif

@property (atomic) AAPLRenderer *renderer;

