
namespace mtlpp
{
//     _MTL_CONST( NS::ErrorDomain, CounterErrorDomain );
//     _MTL_CONST( CommonCounter, CommonCounterTimestamp );
//     _MTL_CONST( CommonCounter, CommonCounterTessellationInputPatches );
//     _MTL_CONST( CommonCounter, CommonCounterVertexInvocations );
//     _MTL_CONST( CommonCounter, CommonCounterPostTessellationVertexInvocations );
//     _MTL_CONST( CommonCounter, CommonCounterClipperInvocations );
//     _MTL_CONST( CommonCounter, CommonCounterClipperPrimitivesOut );
//     _MTL_CONST( CommonCounter, CommonCounterFragmentInvocations );
//     _MTL_CONST( CommonCounter, CommonCounterFragmentsPassed );
//     _MTL_CONST( CommonCounter, CommonCounterComputeKernelInvocations );
//     _MTL_CONST( CommonCounter, CommonCounterTotalCycles );
//     _MTL_CONST( CommonCounter, CommonCounterVertexCycles );
//     _MTL_CONST( CommonCounter, CommonCounterTessellationCycles );
//     _MTL_CONST( CommonCounter, CommonCounterPostTessellationVertexCycles );
//     _MTL_CONST( CommonCounter, CommonCounterFragmentCycles );
//     _MTL_CONST( CommonCounter, CommonCounterRenderTargetWriteCycles );

//     _MTL_CONST( CommonCounterSet, CommonCounterSetTimestamp );
//     _MTL_CONST( CommonCounterSet, CommonCounterSetStageUtilization );
//     _MTL_CONST( CommonCounterSet, CommonCounterSetStatistic );
    
    typedef CommonCounter ns::String*;
    typedef CommonCounterSet ns::String*;

    struct CounterResultTimestamp
    {
        uint64_t timestamp;
    } 

    struct CounterResultStageUtilization
    {
        uint64_t totalCycles;
        uint64_t vertexCycles;
        uint64_t tessellationCycles;
        uint64_t postTessellationVertexCycles;
        uint64_t fragmentCycles;
        uint64_t renderTargetCycles;
    } 

    struct CounterResultStatistic
    {
        uint64_t tessellationInputPatches;
        uint64_t vertexInvocations;
        uint64_t postTessellationVertexInvocations;
        uint64_t clipperInvocations;
        uint64_t clipperPrimitivesOut;
        uint64_t fragmentInvocations;
        uint64_t fragmentsPassed;
        uint64_t computeKernelInvocations;
    } 

    class Counter : public ns::Object
    {
    public:
        ns::String* GetName();
    };

    class CounterSet : public ns::Object
    {
    public:
        ns::String*          GetName();
        ns::Array<Counter>*  GetCounters();
    };

    class CounterSampleBufferDescriptor : public ns::Object
    {
    public:
    
        CounterSampleBufferDescriptor();
        CounterSet*                                 GetCounterSet();
        void                                        SetCounterSet(const CounterSet* counterSet);

        ns::String*                                 GetLabel();
        void                                        SetLabel(const ns::String* label);

        mtlpp::StorageMode                          GetStorageMode();
        void                                        SetStorageMode(const mtlpp::StorageMode storageMode);

        unsigned int                                GetSampleCount();
        void                                        SetSampleCount(const unsigned int sampleCount);
    };

    class CounterSampleBuffer : public ns::Object
    {
    public:
        mtlpp::Device* GetDevice();
        ns::String*    GetLabel();
        ns::UInteger  GetSampleCount();
        ns::Data*     resolveCounterRange(ns::Range range);
    };

    enum class CounterSampleBufferError {
        CounterSampleBufferErrorOutOfMemory = 0,
        CounterSampleBufferErrorInvalid = 1,
    };

    }
}
