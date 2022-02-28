/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the renderer class that performs Metal setup and per-frame rendering.
*/

#ifndef AAPLRenderer_h
#define AAPLRenderer_h

#include <Metal/MetalKit.h>
#include <../../../mtlpp.hpp>

// Platform independent renderer class
class AAPLRenderer : ns::NSObject<mtlpp::ViewDelegate>

void initWithMetalKitView(nonnull mtlpp::View *mtkView);
void compileDylibWithString(NSString *_Nonnull programString);

#endif /* AAPLRenderer_h */
