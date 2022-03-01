/*
 * Copyright 2016-2017 Nikolay Aleksiev. All rights reserved.
 * License: https://github.com/naleksiev/mtlpp/blob/master/LICENSE
 */

#pragma once

#include "defines.hpp"
#include "ns.hpp"
#include "argument.hpp"
#include "mtlpp.hpp"
//#include <Foundation/Foundation.hpp> //Maybe?

namespace mtlpp
{
    enum class DynamicLibraryError {
        DynamicLibraryErrorNone = 0,
        DynamicLibraryErrorInvalidFile = 1,
        DynamicLibraryErrorCompilationFailure = 2,
        DynamicLibraryErrorUnresolvedInstallName = 3,
        DynamicLibraryErrorDependencyLoadFailure = 4,
        DynamicLibraryErrorUnsupported = 5,
    };

    class DynamicLibrary : public ns::Object
    {
        private:
            ns::String*    label;
            mtlpp::Device* device;
            ns::String*    installName;

        public:   
            ns::String*     GetLabel();
            void            SetLabel(ns::String* label);
            mtlpp::Device*  GetDevice();
            ns::String*     GetInstallName();
        
            bool          serializeToURL(const ns::URL* url, ns::Error** error);
        
            ns::String* mtlpp::DynamicLibrary::GetLabel();        
            void mtlpp::DynamicLibrary::SetLabel(ns::String* _label);
            mtlpp::Device* mtlpp::DynamicLibrary::device();
            ns::String* mtlpp::DynamicLibrary::installName();
            bool mtlpp::DynamicLibrary::serializeToURL(const ns::URL* url, ns::Error** error);
    };
}
