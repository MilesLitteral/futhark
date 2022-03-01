

namespace mtlpp
{
    class ComputePassSampleBufferAttachmentDescriptor : public ns::Object
    {
    public:
        ComputePassSampleBufferAttachmentDescriptor();

        CounterSampleBuffer*                                      GetSampleBuffer();
        void                                                      SetSampleBuffer(const CounterSampleBuffer* sampleBuffer);

        NS::UInteger                                              GetStartOfEncoderSampleIndex() const;
        void                                                      SetStartOfEncoderSampleIndex(NS::UInteger startOfEncoderSampleIndex);

        NS::UInteger                                              GetEndOfEncoderSampleIndex() const;
        void                                                      SetEndOfEncoderSampleIndex(NS::UInteger endOfEncoderSampleIndex);
    };

    class ComputePassSampleBufferAttachmentDescriptorArray : public ns::Object
    {
    public:
        ComputePassSampleBufferAttachmentDescriptorArray();
        ComputePassSampleBufferAttachmentDescriptor*                   GetObject(unsigned int attachmentIndex);
        void                                                           SetObject(const ComputePassSampleBufferAttachmentDescriptor* attachment, unsigned int attachmentIndex);
    };

    class ComputePassDescriptor : public ns::Object
    {
    public:
        ComputePassDescriptor();
        ComputePassDescriptor*                                  GetComputePassDescriptor();

        mtlp::CommandBuffer::DispatchType                       GetDispatchType();
        void                                                    SetDispatchType(mtlpp::CommandBuffer::DispatchType dispatchType);

        ComputePassSampleBufferAttachmentDescriptorArray*       SampleBufferAttachments();
    };

}
