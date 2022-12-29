/*
 * Copyright 2016-2017 Nikolay Aleksiev. All rights reserved.
 * License: https://github.com/naleksiev/mtlpp/blob/master/LICENSE
 */

#include "ns.hpp"
#include <CoreFoundation/CFBase.h>
#include <Foundation/NSString.h>
#include <Foundation/NSError.h>
#include <Foundation/NSArray.h>
#include <cstring>

namespace ns
{
    Object::Object() :
        m_ptr(nullptr)
    {
    }

    Object::Object(const Handle& handle) :
        m_ptr(handle.ptr)
    {
        if (m_ptr)
            CFRetain(m_ptr);
    }

    Object::Object(const Object& rhs) :
        m_ptr(rhs.m_ptr)
    {
        if (m_ptr)
            CFRetain(m_ptr);
    }

#if MTLPP_CONFIG_RVALUE_REFERENCES
    Object::Object(Object&& rhs) :
        m_ptr(rhs.m_ptr)
    {
        rhs.m_ptr = nullptr;
    }
#endif

    Object::~Object()
    {
        if (m_ptr)
            CFRelease(m_ptr);
    }

    Object& Object::operator=(const Object& rhs)
    {
        if (rhs.m_ptr == m_ptr)
            return *this;
        if (rhs.m_ptr)
            CFRetain(rhs.m_ptr);
        if (m_ptr)
            CFRelease(m_ptr);
        m_ptr = rhs.m_ptr;
        return *this;
    }

#if MTLPP_CONFIG_RVALUE_REFERENCES
    Object& Object::operator=(Object&& rhs)
    {
        if (rhs.m_ptr == m_ptr)
            return *this;
        if (m_ptr)
            CFRelease(m_ptr);
        m_ptr = rhs.m_ptr;
        rhs.m_ptr = nullptr;
        return *this;
    }
#endif

    uint32_t ArrayBase::GetSize() const
    {
        Validate();
        return uint32_t([(__bridge NSArray*)m_ptr count]);
    }

    void* ArrayBase::GetItem(uint32_t index) const
    {
        Validate();
        return (__bridge void*)[(__bridge NSArray*)m_ptr objectAtIndexedSubscript:index];
    }

    String::String(const char* cstr) :
        Object(Handle{ (__bridge void*)[NSString stringWithUTF8String:cstr] })
    {
    }

    const char* String::GetCStr() const
    {
        Validate();
        return [(__bridge NSString*)m_ptr cStringUsingEncoding:NSUTF8StringEncoding];
    }

    uint32_t String::GetLength() const
    {
        Validate();
        return uint32_t([(__bridge NSString*)m_ptr length]);
    }

    Error::Error() :
        Object(Handle{ (__bridge void*)[[NSError alloc] init] })
    {

    }

    String Error::GetDomain() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr domain] };
    }

    uint32_t Error::GetCode() const
    {
        Validate();
        return uint32_t([(__bridge NSError*)m_ptr code]);
    }

    //@property (readonly, copy) NSDictionary *userInfo;

    String Error::GetLocalizedDescription() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr localizedDescription] };
    }

    String Error::GetLocalizedFailureReason() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr localizedFailureReason] };
    }

    String Error::GetLocalizedRecoverySuggestion() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr localizedRecoverySuggestion] };
    }

    String Error::GetLocalizedRecoveryOptions() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr localizedRecoveryOptions] };
    }

    //@property (nullable, readonly, strong) id recoveryAttempter;

    String Error::GetHelpAnchor() const
    {
        Validate();
        return Handle{ (__bridge void*)[(__bridge NSError*)m_ptr helpAnchor] };
    }

    // static URL* fileURLWithPath(const class String* pPath);

    URL::URL()) : Object(Handle{ (__bridge void*)[[NSURL alloc] init] })
    {

    }
    // URL*        init(const class String* pString);
    // URL*        initFileURLWithPath(const class String* pPath);

    // const char* fileSystemRepresentation() const;

    Bundle::Bundle() : Object(Handle{ (__bridge void*)[[NSBundle alloc] init] })
    {

    }
    //     Device(const ns::Handle& handle) : ns::Object(handle) { }

    //     static Device CreateSystemDefaultDevice() MTLPP_AVAILABLE(10_11, 8_0);
    //     static ns::Array<Device> CopyAllDevices() MTLPP_AVAILABLE(10_11, NA);

    //     ns::String GetName() const;
    //     Size       GetMaxThreadsPerThreadgroup() const MTLPP_AVAILABLE(10_11, 9_0);
    //     bool       IsLowPower() const MTLPP_AVAILABLE_MAC(10_11);
    //     bool       IsHeadless() const MTLPP_AVAILABLE_MAC(10_11);
    //     uint64_t   GetRecommendedMaxWorkingSetSize() const MTLPP_AVAILABLE_MAC(10_12);
    //     bool       IsDepth24Stencil8PixelFormatSupported() const MTLPP_AVAILABLE_MAC(10_11);

    //     CommandQueue NewCommandQueue();
    //     CommandQueue NewCommandQueue(uint32_t maxCommandBufferCount);
    //     SizeAndAlign HeapTextureSizeAndAlign(const TextureDescriptor& desc) MTLPP_AVAILABLE(NA, 10_0);
    //     SizeAndAlign HeapBufferSizeAndAlign(uint32_t length, ResourceOptions options) MTLPP_AVAILABLE(NA, 10_0);
    //     Heap NewHeap(const HeapDescriptor& descriptor) MTLPP_AVAILABLE(NA, 10_0);
    //     Buffer NewBuffer(uint32_t length, ResourceOptions options);
    //     Buffer NewBuffer(const void* pointer, uint32_t length, ResourceOptions options);
    //     Buffer NewBuffer(void* pointer, uint32_t length, ResourceOptions options, std::function<void (void* pointer, uint32_t length)> deallocator);
    //     DepthStencilState NewDepthStencilState(const DepthStencilDescriptor& descriptor);
    //     Texture NewTexture(const TextureDescriptor& descriptor);
    //     SamplerState NewSamplerState(const SamplerDescriptor& descriptor);
    //     Library NewDefaultLibrary();
    //     Library NewDefaultLibraryWithBundle(ns::Bundle * bundle, ns::Error ** error) MTLPP_AVAILABLE(10_12, 10_0);
    //     Library NewLibrary(const ns::String& filepath, ns::Error* error);
    //     Library NewLibrary(const char* source, const CompileOptions& options, ns::Error* error);
    //     void NewLibrary(const char* source, const CompileOptions& options, std::function<void(const Library&, const ns::Error&)> completionHandler);
    //     DynamicLibrary* newDynamicLibrary(const Library* library, ns::Error** error);
    //     DynamicLibrary* newDynamicLibrary(const ns::URL* url,     ns::Error** error);
    //     BinaryArchive*  newBinaryArchive(const BinaryArchiveDescriptor* descriptor, ns::Error** error);
    //     RenderPipelineState NewRenderPipelineState(const RenderPipelineDescriptor& descriptor, ns::Error* error);
    //     RenderPipelineState NewRenderPipelineState(const RenderPipelineDescriptor& descriptor, PipelineOption options, RenderPipelineReflection* outReflection, ns::Error* error);
    //     void NewRenderPipelineState(const RenderPipelineDescriptor& descriptor, std::function<void(const RenderPipelineState&, const ns::Error&)> completionHandler);
    //     void NewRenderPipelineState(const RenderPipelineDescriptor& descriptor, PipelineOption options, std::function<void(const RenderPipelineState&, const RenderPipelineReflection&, const ns::Error&)> completionHandler);
    //     ComputePipelineState NewComputePipelineState(const Function& computeFunction, ns::Error* error);
    //     ComputePipelineState NewComputePipelineState(const Function& computeFunction, PipelineOption options, ComputePipelineReflection& outReflection, ns::Error* error);
    //     void NewComputePipelineState(const Function& computeFunction, std::function<void(const ComputePipelineState&, const ns::Error&)> completionHandler);
    //     void NewComputePipelineState(const Function& computeFunction, PipelineOption options, std::function<void(const ComputePipelineState&, const ComputePipelineReflection&, const ns::Error&)> completionHandler);
    //     ComputePipelineState NewComputePipelineState(const ComputePipelineDescriptor& descriptor, PipelineOption options, ComputePipelineReflection* outReflection, ns::Error* error);
    //     void NewComputePipelineState(const ComputePipelineDescriptor& descriptor, PipelineOption options, std::function<void(const ComputePipelineState&, const ComputePipelineReflection&, const ns::Error&)> completionHandler) MTLPP_AVAILABLE(10_11, 9_0);
    //     Fence NewFence() MTLPP_AVAILABLE(NA, 10_0);
    //     bool SupportsFeatureSet(FeatureSet featureSet) const;
    //     bool SupportsTextureSampleCount(uint32_t sampleCount) const MTLPP_AVAILABLE(10_11, 9_0);
}
