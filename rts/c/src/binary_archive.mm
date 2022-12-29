#pragma once

#include "ns.hpp"
#include "mtlpp.hpp" 
#include "binary_archive.hpp"
//#include <Foundation/Foundation.hpp> //maybe?

    
// (id<MTLBinaryArchive>)newBinaryArchiveWithDescriptor:(MTLBinaryArchiveDescriptor *)descriptor 
//                                                  error:(NSError * _Nullable *)error;

    //     // Error
    //     NSError* nsError = NULL;
    //     NSError** nsErrorPtr = error ? &nsError : nullptr;

    //     id<MTLRenderPipelineState> renderPipelineState = [(__bridge id<MTLDevice>)m_ptr newRenderPipelineStateWithDescriptor:(__bridge MTLRenderPipelineDescriptor*)descriptor.GetPtr()
    //                                                                                                                    error:nsErrorPtr];

    //     // Error update
    //     if (error && nsError){
    //         *error = ns::Handle{ (__bridge void*)nsError };
    //     }

    //     return ns::Handle{ (__bridge void*)renderPipelineState };
    // }
    
namespace mtlpp
{

    NSURL BinaryArchiveDescriptor::GetUrl(){
        Validate();
        return [(__bridge id<MTLBinaryArchiveDescriptor>)m_ptr url)];
    }
        
    void mtlpp::BinaryArchiveDescriptor::SetUrl(ns::URL* _url){
        Validate();
        return [(__bridge id<MTLBinaryArchiveDescriptor>)m_ptr)addTileRenderPipelineFunctionsWithDescriptor:(MTLTileRenderPipelineDescriptor *)descriptor.GetPtr() 
                                            error: error];
    }

    ns::String* BinaryArchive::GetLabel(){
        Validate();                       
        return ns::Handle{ (__bridge void*)[(__bridge NSString*)m_ptr label] };
    }

    void BinaryArchive::SetLabel(const ns::String* label);
    {
        Validate();
        [(__bridge id<MTLBinaryArchive>)m_ptr setLabel:(__bridge NSString*)label.GetPtr()];
    }

    bool BinaryArchive::addComputePipelineFunctions(const class ComputePipelineDescriptor* descriptor, ns::Error** error){
        #if MTLPP_IS_AVAILABLE_MAC(10_11)
        return [(__bridge id<MTLBinaryArchive>)m_ptr)addComputePipelineFunctionsWithDescriptor:(MTLTileRenderPipelineDescriptor *)descriptor 
                                            error:(NSError * _Nullable *)error];
        #else
            return false;
        #endif
    };

    bool BinaryArchive::addRenderPipelineFunctions(const class RenderPipelineDescriptor* descriptor, ns::Error** error){
        #if MTLPP_IS_AVAILABLE_MAC(10_11)
        return [(__bridge id<MTLBinaryArchive>)m_ptr)addRenderPipelineFunctionsWithDescriptor:(MTLTileRenderPipelineDescriptor *)descriptor 
                                            error:(NSError * _Nullable *)error];
        #else
            return false;
        #endif
    };

    bool BinaryArchive::addTileRenderPipelineFunctions(const class TileRenderPipelineDescriptor* descriptor, ns::Error** error){
        Validate();
        #if MTLPP_IS_AVAILABLE_MAC(10_11)
        return [(__bridge id<MTLBinaryArchive>)m_ptr)addTileRenderPipelineFunctionsWithDescriptor:(MTLTileRenderPipelineDescriptor *)descriptor 
                                            error:(NSError * _Nullable *)error];
        #else
            return false;
        #endif
    };

    bool BinaryArchive::serializeToURL(const ns::URL* url, ns::Error** error){
        Validate();
        [(__bridge id<MTLBinaryArchive>)m_ptr serializeToURL:(NSURL*)url 
                                                error:(NSError* _Nullable*)error)]
    };

    bool BinaryArchive::addFunction(const class FunctionDescriptor* descriptor, const class Library* library, ns::Error** error){
        Validate();
        #if MTLPP_IS_AVAILABLE_MAC(10_11)
        return [(__bridge id<MTLBinaryArchive>)m_ptr) addFunctionWithDescriptor:(MTLFunctionDescriptor *)descriptor 
                    library:(id<MTLLibrary>)library 
                    error:(NSError * _Nullable *)error];
            #else
            return false;
        #endif
    };
}
