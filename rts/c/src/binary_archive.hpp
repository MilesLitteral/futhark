#pragma once

#include "ns.hpp"
#include "mtlpp.hpp" 
//#include <Foundation/Foundation.hpp> //maybe?

namespace mtlpp
{
    enum class BinaryArchiveError {
        BinaryArchiveErrorNone = 0,
        BinaryArchiveErrorInvalidFile = 1,
        BinaryArchiveErrorUnexpectedElement = 2,
        BinaryArchiveErrorCompilationFailure = 3,
    };

    class BinaryArchiveDescriptor : public ns::Object
    {
    private:
        ns::URL url;

    public:
        BinaryArchiveDescriptor();
        BinaryArchiveDescriptor(const ns::Handle& handle) : ns::Object(handle) { }

        ns::URL                               GetUrl();
        void                                  SetUrl(ns::URL* url);
    };

    class BinaryArchive : public ns::Object
    {
    private:
        ns::String     label;
        mtlpp::Device* device;

    public:
        ns::String    GetLabel();
        void          SetLabel(ns::String* label);
        ns::URL       GetURL();
        void          SetURL();
        Device*       GetDevice();
        void          SetDevice();

        bool          AddComputePipelineFunctions(ComputePipelineDescriptor* descriptor, ns::Error** error);
        bool          AddRenderPipelineFunctions(RenderPipelineDescriptor* descriptor, ns::Error** error);
        bool          AddTileRenderPipelineFunctions(TileRenderPipelineDescriptor* descriptor, ns::Error** error);
        bool          SerializeToURL(ns::URL* url, ns::Error** error);
        bool          AddFunction(FunctionDescriptor* descriptor, Library* library, ns::Error** error);
    };
}
