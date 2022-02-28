/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of the renderer class that performs Metal setup and per-frame rendering.
*/

#include <simd/simd.h>
#include <MetalKit/MetalKit.h>

#include "AAPLRenderer.h"
#include "AAPLMathUtilities.h"

// Include header shared between C code here, which executes Metal API commands, and .metal files
#include "AAPLShaderTypes.h"
#include "mtlpp.hpp"

// The max number of command buffers in flight
static const int AAPLMaxFramesInFlight = 3;

// Main class performing the rendering
class AAPLRenderer
{
    mtlpp::Device _device = mtlpp::Device::CreateSystemDefaultDevice();
    mtlpp::CommandQueue _commandQueue;
    mtlpp::CommandBuffer _commandBuffer = _commandQueue.CommandBuffer();

    //dispatch_semaphore_t _inFlightSemaphore;
    mtlpp::Buffer _frameDataBuffer = _device.NewBuffer(AAPLMaxFramesInFlight, mtlpp::ResourceOptions::StorageModeManaged);
    int _frameNumber;

    mtlpp::RenderPipelineState _renderPipeline;
    mtlpp::DepthStencilState _depthState;
    mtlpp::Texture _colorMap;
    mtlpp::Buffer _positionBuffer;
    mtlpp::Buffer _texCoordBuffer;
    mtlpp::Buffer _indexBuffer;
    mtlpp::Texture _colorTarget;

    mtlpp::RenderPassDescriptor *_renderPassDescriptor;

    mtlpp::VertexDescriptor *_vertexDescriptor;
    // Projection matrix calculated as a function of view size
    matrix_float4x4 _projectionMatrix;
    float _rotation;

    mtlpp::ComputePipelineState _computePipeline;
    mtlpp::ComputePipelineDescriptor *_baseDescriptor;
    mtlpp::Size _dispatchExecutionSize;
    mtlpp::Size _threadsPerThreadgroup;
    int _threadgroupMemoryLength;


// Initialize with the MetalKit view with the Metal device used to render.  This MetalKit view
// object will also be used to set the pixelFormat and other properties of the drawable
// class initWithMetalKitView
// {
//     initWithMetalKitView(MTKView* mtkView)
//     {
//         _device = mtkView.device;
//         _inFlightSemaphore = dispatch_semaphore_create(AAPLMaxFramesInFlight);
//         mtkView.loadMetal();
//         MTK::loadAssets;
//     }
// };

/// Create the Metal render state objects including shaders and render state pipeline objects
// void loadMetal(MTKView* mtkView)
// {
//     mtkView.setUpView();

//     mtlpp::Library metalLibrary = [self loadMetallib];

//     [self createBuffers];

//     [self createRenderStateWithLibrary:metalLibrary];

//     [self createComputePipelineWithLibrary:metalLibrary];

//     _commandQueue = [_device newCommandQueue];
// }

// Load the Metal library created in the "Build Executable Metal Library" build phase.
// This library includes the functions in AAPLShaders.metal and the UserDylib.metallib
// created in the "Build Dynamic Metal Library" build phase.
mtlpp::Library loadMetallib(mtlpp::Device _device)
{
    ns::Error *error;

    mtlpp::Library library = _device.NewLibrary("AAPLShaders.metallib", error);

    assert(library);
    if(library == NULL){
        printf("Failed to load AAPLShaders dynamic metal library: %@", error);
    }

    return library;
}

// // Set up properties of the view
// void setUpView(MTKView* mtkView)
// {
//     mtkView.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
//     mtkView.sampleCount = 1;
// }

// Create buffers modified each frame to animate the cube
void createBuffers()
{
    for(int i = 0; i < AAPLMaxFramesInFlight; i++)
    {
        _frameDataBuffer[i] = _device.NewBuffer(sizeof(AAPLPerFrameData), mtlpp::ResourceOptions::StorageModeShared);
        _frameDataBuffer[i].label = "FrameDataBuffer";
    }
}

/// Create the render pass descriptor, render pipeline state object, and depth state object to render the cube.
void createRenderStateWithLibrary(mtlpp::Library metallib)
{
    ns::Error *error;

    // Set up render pass descriptor
    {
        _renderPassDescriptor = new mtlpp::RenderPassDescriptor();
        _renderPassDescriptor->GetColorAttachments()[0].SetClearColor(mtlpp::ClearColor(0, 0, 0, 1));
        _renderPassDescriptor->GetColorAttachments()[0].SetLoadAction(mtlpp::LoadAction::Clear); // LoadActionClear);
        _renderPassDescriptor->GetColorAttachments()[0].SetStoreAction(mtlpp::StoreAction::Store);

        _renderPassDescriptor->GetDepthAttachment().SetLoadAction(mtlpp::LoadAction::Clear);
        _renderPassDescriptor->GetDepthAttachment().SetStoreAction(mtlpp::StoreAction::DontCare);
    }

    // Set up render pipeline
    {
        _vertexDescriptor = new mtlpp::VertexDescriptor();
    
        _vertexDescriptor->GetAttributes()[AAPLVertexAttributePosition].SetFormat(mtlpp::VertexFormat::Float3);
        _vertexDescriptor->GetAttributes()[AAPLVertexAttributePosition].SetOffset(0);
        _vertexDescriptor->GetAttributes()[AAPLVertexAttributePosition].bufferIndex = AAPLBufferIndexMeshPositions;

        _vertexDescriptor->GetAttributes()[AAPLVertexAttributeTexcoord].SetFormat(mtlpp::VertexFormat::Float2);
        _vertexDescriptor->GetAttributes()[AAPLVertexAttributeTexcoord].offset = 0;
        _vertexDescriptor->GetAttributes()[AAPLVertexAttributeTexcoord].bufferIndex = AAPLBufferIndexMeshGenerics;

        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stride = 16;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stepRate = 1;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stepFunction = MTLVertexStepFunctionPerVertex;

        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stride = 8;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stepRate = 1;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stepFunction = MTLVertexStepFunctionPerVertex;

        // Load the vertex function from the library
        mtlpp::Function vertexFunction = metallib.NewFunction("vertexShader");

        // Load the fragment function from the library
        mtlpp::Function fragmentFunction = metallib.NewFunction("fragmentShader");

        mtllpp::RenderPipelineDescriptor *pipelineDescriptor =   new mtlpp::RenderPipelineDescriptor();
        pipelineDescriptor.label = "RenderPipeline";
        pipelineDescriptor.sampleCount = 1;
        pipelineDescriptor.vertexFunction = vertexFunction;
        pipelineDescriptor.vertexDescriptor = _vertexDescriptor;
        pipelineDescriptor.fragmentFunction = fragmentFunction;
        pipelineDescriptor.colorAttachments[0].pixelFormat = MTLPixelFormatRGBA8Unorm;
        pipelineDescriptor.depthAttachmentPixelFormat = MTLPixelFormatDepth32Float;

        _renderPipeline = _device.NewRenderPipelineStateWithDescriptor(pipelineDescriptor, error);

        assert(_renderPipeline) 
        if(_renderPipeline == NULL){
            printf("Failed to create render pipeline state: %@", error);
        }
    }

    // Set up depth stencil state
    {
        mtlpp::DepthStencilDescriptor *depthStateDesc = new mtlpp::DepthStencilDescriptor();
        depthStateDesc.depthCompareFunction = mtlpp::CompareFunction(CompareFunction::Less);
        depthStateDesc.depthWriteEnabled = true;
        _depthState = [_device newDepthStencilStateWithDescriptor:depthStateDesc];
    }
}

void createComputePipelineWithLibrary(mtlpp::Library metallib)
{
    ns::Error *error;

    mtlpp::Function kernelFunction = metallib.NewFunction("dylibKernel");
    _baseDescriptor = new mtlpp::ComputePipelineDescriptor();
    _baseDescriptor->SetComputeFunction(kernelFunction);
    mtlpp::ComputePipelineDescriptor *descriptor = new mtlpp::ComputePipelineDescriptor();
    descriptor.computeFunction = _baseDescriptor.computeFunction;
    
    _computePipeline = _device.newComputePipelineState(descriptor, options:MTLPipelineOptionNone, NULL, error);
    assert(_computePipeline)
    if(_computePipeline == NULL){
        printf("Error creating pipeline which links library from source: %@", error);
    }
}

/// Load assets into metal objects
void loadAssets()
{
    // Create a buffer with positions to draw the cube.
        static const vector_float3 cubePositions[] =
        {
            // Front
            { -1, -1,  1 },
            { -1,  1,  1 },
            {  1,  1,  1 },
            {  1, -1,  1 },

            // Top
            { -1,  1,  1 },
            { -1,  1, -1 },
            {  1,  1, -1 },
            {  1,  1,  1 },

            // Right
            {  1, -1,  1 },
            {  1,  1,  1 },
            {  1,  1, -1 },
            {  1, -1, -1 },

            // Back
            { -1,  1, -1 },
            { -1, -1, -1 },
            {  1, -1, -1 },
            {  1,  1, -1 },

            // Bottom
            { -1, -1, -1 },
            { -1, -1,  1 },
            {  1, -1,  1 },
            {  1, -1, -1 },

            // Left
            { -1, -1, -1 },
            { -1,  1, -1 },
            { -1,  1,  1 },
            { -1, -1,  1 }
        };

        _positionBuffer = _device.NewBuffer(cubePositions, sizeof(cubePositions), 0);

    // Create a buffer with texture coordinates to draw the cube.
        static const vector_float2 cubeTexCoords[] =
        {
            // Front
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Top
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Right
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Back
            { 1, 0 },
            { 1, 1 },
            { 0, 1 },
            { 0, 0 },

            // Bottom
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Right
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },
        };

        _texCoordBuffer = _device.NewBuffer(cubeTexCoords, sizeof(cubeTexCoords), 0);

    // Create the index buffer to draw the cube.
        static uint16_t indices[] =
        {
            // Front
             0,  2,  1,  0,  3,  2,

            // Top
             4,  6,  5,  4,  7,  6,

            // Right
             8, 10,  9,  8, 11, 10,

            // Back
            12, 14, 13, 12, 15, 14,

            // Bottom
            16, 18, 17, 16, 19, 18,

            // Left
            20, 22, 21, 20, 23, 22,
        };

        _indexBuffer = _device.newBuffer(indices, sizeof(indices), 0);

    // Load color texture from asset catalog
    {
        MTKTextureLoader* textureLoader = [[MTKTextureLoader alloc] initWithDevice:_device];

        enum *textureLoaderOptions =
        {
            MTKTextureLoaderOptionTextureUsage       : @(MTLTextureUsageShaderRead),
            MTKTextureLoaderOptionTextureStorageMode : @(MTLStorageModePrivate)
        };

        ns::Error *error;

        _colorMap = [textureLoader newTextureWithName:@"ColorMap"
                                          scaleFactor:1.0
                                               bundle:nil
                                              options:textureLoaderOptions
                                                error:&error];

        assert(_colorMap) 
        if(_colorMap == NULL){
            printf(@"Error creating the Metal texture, error: %@.", error);
        }
    }
}

//Update any scene state before encoding rendering commands to our drawable
void updateSceneState()
{
    NSUInteger frameDataBufferIndex = _frameNumber % AAPLMaxFramesInFlight;

    AAPLPerFrameData *frameData = (AAPLPerFrameData*)_frameDataBuffer[frameDataBufferIndex].contents;

    frameData->projectionMatrix = _projectionMatrix;

    vector_float3 rotationAxis = { 1, 1, 0 };
    matrix_float4x4 modelMatrix = matrix4x4_rotation(_rotation, rotationAxis);
    matrix_float4x4 viewMatrix = matrix4x4_translation(0.0, 0.0, -6.0);

    frameData->modelViewMatrix = matrix_multiply(viewMatrix, modelMatrix);

    _rotation += .01;
}

/// Update the 3D projection matrix with the given size
- (void)updateProjectionMatrixWithSize:(CGSize)size
{
    /// Respond to drawable size or orientation changes here
    float aspect = size.width / (float)size.height;
    _projectionMatrix = matrix_perspective_right_hand(65.0f * (M_PI / 180.0f), aspect, 0.1f, 100.0f);
}

/// Create render targets for compute kernel inputs
-(void)createRenderTargetsWithSize:(CGSize)size
{
    MTLTextureDescriptor *renderTargetDesc = [MTLTextureDescriptor new];

    // Set up properties common to both color and depth textures.
    renderTargetDesc.width = size.width;
    renderTargetDesc.height = size.height;
    renderTargetDesc.storageMode = MTLStorageModePrivate;

    // Set up a color render texture target.
    renderTargetDesc.pixelFormat = MTLPixelFormatRGBA8Unorm;
    renderTargetDesc.usage = MTLTextureUsageRenderTarget | MTLTextureUsageShaderRead;
    _colorTarget =  [_device newTextureWithDescriptor:renderTargetDesc];

    // Set up a depth texture target.
    renderTargetDesc.pixelFormat = MTLPixelFormatDepth32Float;
    renderTargetDesc.usage = MTLTextureUsageRenderTarget;
    id<MTLTexture> depthTarget = [_device newTextureWithDescriptor:renderTargetDesc];

    // Set up the render pass descriptor with newly created textures.
    _renderPassDescriptor.colorAttachments[0].texture = _colorTarget;
    _renderPassDescriptor.depthAttachment.texture = depthTarget;
}

/// Called whenever view changes orientation or layout is changed
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Update the aspect ratio and projection matrix since the view orientation or size has changed.
    [self updateProjectionMatrixWithSize:size];
    [self createRenderTargetsWithSize:size];
}

/// Called whenever the view needs to render
- (void)drawInMTKView:(nonnull MTKView*)view
{
    NSUInteger frameDataBufferIndex = _frameNumber % AAPLMaxFramesInFlight;

    dispatch_semaphore_wait(_inFlightSemaphore, DISPATCH_TIME_FOREVER);

    [self updateSceneState];

    // Render cube to offscreen texture
    {
        id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
        commandBuffer.label = [NSString stringWithFormat:@"Render CommandBuffer"];

        id<MTLRenderCommandEncoder> renderEncoder = [commandBuffer
                                                     renderCommandEncoderWithDescriptor:_renderPassDescriptor];
        // Render cube
        renderEncoder.label = @"Render Encoder";
        [renderEncoder pushDebugGroup:@"Render Cube"];

        [renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [renderEncoder setCullMode:MTLCullModeBack];
        [renderEncoder setRenderPipelineState:_renderPipeline];
        [renderEncoder setDepthStencilState:_depthState];

        [renderEncoder setVertexBuffer:_positionBuffer
                                offset:0
                               atIndex:AAPLBufferIndexMeshPositions];

        [renderEncoder setVertexBuffer:_texCoordBuffer
                                offset:0
                               atIndex:AAPLBufferIndexMeshGenerics];

        [renderEncoder setVertexBuffer:_frameDataBuffer[frameDataBufferIndex]
                                offset:0
                               atIndex:AAPLBufferIndexFrameData];

        [renderEncoder setFragmentBuffer:_frameDataBuffer[frameDataBufferIndex]
                                  offset:0
                                 atIndex:AAPLBufferIndexFrameData];

        [renderEncoder setFragmentTexture:_colorMap
                                  atIndex:AAPLTextureIndexColorMap];

        [renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                  indexCount:36
                                   indexType:MTLIndexTypeUInt16
                                 indexBuffer:_indexBuffer
                           indexBufferOffset:0];

        [renderEncoder popDebugGroup];
        [renderEncoder endEncoding];

        [commandBuffer commit];
    }

    // Use compute pipeline from function in dylib to process offscreen texture
    if(_computePipeline && view.currentDrawable)
    {
        id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
        commandBuffer.label = [NSString stringWithFormat:@"Compute CommandBuffer"];

        __block dispatch_semaphore_t block_sema = _inFlightSemaphore;
        [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> buffer)
         {
            dispatch_semaphore_signal(block_sema);
        }];

        id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
        computeEncoder.label = @"Compute Encoder";
        [computeEncoder setComputePipelineState:_computePipeline];
        [computeEncoder setTexture:_colorTarget
                           atIndex:AAPLTextureIndexComputeIn];
        [computeEncoder setTexture:view.currentDrawable.texture
                           atIndex:AAPLTextureIndexComputeOut];
        [computeEncoder dispatchThreads:MTLSizeMake(view.drawableSize.width, view.drawableSize.height, 1)
                  threadsPerThreadgroup:MTLSizeMake(16, 16, 1)];

        [computeEncoder endEncoding];

        [commandBuffer presentDrawable:view.currentDrawable];

        [commandBuffer commit];
    }


    _frameNumber++;
}

/// Compile a dylib with the given program string then create a compute pipeline with the dylib
void compileDylibWithString:(NSString * programString)
{
    NSError *error;

    MTLCompileOptions *options = [MTLCompileOptions new];
    options.libraryType = MTLLibraryTypeDynamic;
    options.installName = [NSString stringWithFormat:@"@executable_path/userCreatedDylib.metallib"];

    mtlpp::Library> lib = [_device newLibraryWithSource:programString
                                               options:options
                                                 error:&error];
    if(!lib && error)
    {
        printf("Error compiling library from source: %@", error);
        return;
    }
    
    id<MTLDynamicLibrary> dynamicLib = [_device newDynamicLibrary:lib
                                                            error:&error];
    if(!dynamicLib && error)
    {
        printf("Error creating dynamic library from source library: %@", error);
        return;
    }
    
    mtlpp::ComputePipelineDescriptor *descriptor = [MTLComputePipelineDescriptor new];
    descriptor.computeFunction = _baseDescriptor.computeFunction;
    descriptor.insertLibraries = @[dynamicLib];
    
    mtlpp::ComputePipelineState previousComputePipeline = _computePipeline;
    _computePipeline = _device.newComputePipelineStateWithDescriptor(descriptor, MTLPipelineOptionNone, NULL, error);
        if(!_computePipeline && error)
        {
            printf("Error creating pipeline library from source library, using previous pipeline: %@", error);
            _computePipeline = previousComputePipeline;
            return;
        }
    }
}

