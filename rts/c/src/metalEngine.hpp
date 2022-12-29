#include <stddef.h>
#include <string>
#include <filesystem>
#include "../mtlpp.hpp"

const unsigned int arrayLength = 10; //1 << 24;
const unsigned int bufferSize = arrayLength * sizeof(float);

//xcrun -sdk macosx metal -c add.metal -o add.air
//xcrun -sdk macosx metallib add.air -o add.metallib

//xcrun -sdk macosx metal -c operations.metal -o operations.air
//xcrun -sdk macosx metallib operations.air -o operations.metallib

//define metal reflections here 
void mtlAddArrays(const float* inA, const float* inB, float* result, int length);
//end define Metal Reflections

class MetalEngine
    {
    mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();
    mtlpp::ComputePipelineState _mFunctionPSO;
    mtlpp::CommandQueue _mCommandQueue;
    mtlpp::Buffer _mBufferA;
    mtlpp::Buffer _mBufferB;
    mtlpp::Buffer _mBufferResult;
    ns::Error* error; //nullptr

    MetalEngine();
    MetalEngine(mtlpp::Device device);
    MetalEngine(ns::String libraryPath, ns::String mtlFunction, mtlpp::Device device);
    MetalEngine(ns::String mtlFunction, mtlpp::Device device);
    MetalEngine(const char src[], ns::String functionName, mtlpp::Device device);
    void generateRandomFloatData(mtlpp::Buffer buffer);
    void prepareData(mtlpp::Device device);
    void sendComputeCommand(mtlpp::CommandQueue commandQueue);
    void encodeAddCommand(mtlpp::ComputeCommandEncoder computeEncoder);
    void verifyResults();
    void execute();
};
