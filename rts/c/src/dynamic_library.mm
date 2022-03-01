/*
 * Copyright 2016-2017 Nikolay Aleksiev. All rights reserved.
 * License: https://github.com/naleksiev/mtlpp/blob/master/LICENSE
 */

#include "library.hpp"
#include "device.hpp"
#include "function_constant_values.hpp"
#include <Metal/MTLLibrary.h>

namespace mtlpp
{

        (id<MTLDevice>)* mtlpp::DynamicLibrary::GetDevice(){
                Validate();
                return [(id<MTLDevice> device)m_ptr device]
        };
        
        void mtlpp::DynamicLibrary::SetDevice(ns::String* _device){
                Validate();
                [(id<MTLDevice> device)m_ptr device.GetPtr()]
        };

        NSString* mtlpp::DynamicLibrary::GetLabel(){
                Validate();
                return [(NSString dylib)m_ptr label)]
        }

        void mtlpp::DynamicLibrary::SetLabel(ns::String* _label){
                Validate();
                [(NSString device)m_ptr label.GetPtr()]
        }

        NSString* mtlpp::DynamicLibrary::GetInstallName(){
                Validate();
                return [(id<MTLDevice> device)m_ptr installName]
        }

        NSString* mtlpp::DynamicLibrary::SetInstallName(ns::String* _installName){
                Validate();
                [(id<MTLDevice> device)m_ptr installName.GetPtr()]
        }

        bool mtlpp::DynamicLibrary::SerializeToURL(const ns::URL* url, ns::Error** error){
             Validate();
             return [(__bridge id<MTLDynamicLibrary>)m_ptr serializeToURL:(NSURL*)url error:error)]
        };
}
