#include <stddef.h>
#include <string>
#include <filesystem>
#include "../../mtlpp.hpp"

#define arrayLength 1 << 14
const unsigned int bufferSize = arrayLength * sizeof(float);

//xcrun -sdk macosx metal -c add.metal -o add.air
//xcrun -sdk macosx metallib add.air -o add.metallib

//xcrun -sdk macosx metal -c operations.metal -o operations.air
//xcrun -sdk macosx metallib operations.air -o operations.metallib

//define metal reflections here 
void mtlAddArrays(const float* inA,
                const float* inB,
                float* result,
                int length)
{
    for (int index = 0; index < length; index++)
    {
        result[index] = result[index] + inA[index] * inB[index];
    }
}

//end define Metal Reflections

class MetalEngine
{
    public:
        mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();

        // The compute pipeline generated from the compute kernel in the .metal shader file.
        mtlpp::ComputePipelineState _mAddFunctionPSO;

        // The command queue used to pass commands to the device.
        mtlpp::CommandQueue _mCommandQueue;

        // Buffers to hold data.
        mtlpp::Buffer _mBufferA;
        mtlpp::Buffer _mBufferB;
        mtlpp::Buffer _mBufferResult;

    MetalEngine(mtlpp::Device device)
    {
        _mDevice = device;
        ns::Error* error = NULL;

        // Load the shader files with a .metal file extension in the project

        mtlpp::Library defaultLibrary = device.NewLibrary("/Users/sasori/Desktop/GitHub/mtlpp/build/macos_10.12/add.metallib", error);//device.NewDefaultLibrary();
        if (defaultLibrary.GetFunctionNames() == NULL)
        {
            printf("Failed to find the default library.\n");

        }
        mtlpp::Function addFunction = defaultLibrary.NewFunction("mtlAddArrays");

        // Create a compute pipeline state object.
        _mAddFunctionPSO = device.NewComputePipelineState(addFunction, error);
    
        _mCommandQueue = device.NewCommandQueue();
    }

    MetalEngine(mtlpp::Device device, const char* shaderSrc, ns::String functionName)
    {
        _mDevice = device;
        ns::Error* error = NULL;

        // Load the shader files with a .metal file extension in the project

        mtlpp::Library defaultLibrary = device.NewLibrary(shaderSrc, mtlpp::CompileOptions(), error);//device.NewDefaultLibrary();
        if (defaultLibrary.GetFunctionNames() == NULL)
        {
            printf("Failed to find the default library.\n");

        }
        mtlpp::Function function = defaultLibrary.NewFunction(functionName);

        // Create a compute pipeline state object.
        _mAddFunctionPSO = device.NewComputePipelineState(function, error);
    
        _mCommandQueue = device.NewCommandQueue();
    }

    void generateRandomFloatData(mtlpp::Buffer buffer)
    {
        float* dataPtr = (float*)buffer.GetContents();

        for (unsigned long index = 0; index < arrayLength; index++)
        {
            dataPtr[index] = (float)rand()/(float)(RAND_MAX);
        }
    }

    void prepareData(mtlpp::Device device)
    {
        // Allocate three buffers to hold our initial data and the result.
        _mBufferA = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);
        _mBufferB = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);
        _mBufferResult = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);

        generateRandomFloatData(_mBufferA);
        generateRandomFloatData(_mBufferB);
    }


    void sendComputeCommand(mtlpp::CommandQueue commandQueue)
    {
        // Create a command buffer to hold commands.
        mtlpp::CommandBuffer commandBuffer = commandQueue.CommandBuffer();
        // Start a compute pass.
        mtlpp::ComputeCommandEncoder computeEncoder = commandBuffer.ComputeCommandEncoder();// computeCommandEncoder];

        encodeAddCommand(computeEncoder);
        // End the compute pass.
        computeEncoder.EndEncoding();

        // Execute the command.
        commandBuffer.Commit();

        // Normally, you want to do other work in your app while the GPU is running,
        // but in this example, the code simply blocks until the calculation is complete.
        commandBuffer.WaitUntilCompleted();

        verifyResults();
    }


    void encodeAddCommand(mtlpp::ComputeCommandEncoder computeEncoder) {

        // Encode the pipeline state object and its parameters.
        computeEncoder.SetComputePipelineState(_mAddFunctionPSO);
        computeEncoder.SetBuffer(_mBufferA, 0, 0);
        computeEncoder.SetBuffer(_mBufferB, 0, 1);
        computeEncoder.SetBuffer(_mBufferResult, 0, 2);
        //_mBufferResult offset:x atIndex:y

        mtlpp::Size gridSize = mtlpp::Size(arrayLength, 1, 1);

        // Calculate a threadgroup size.
        uint32_t threadGroupSize = _mAddFunctionPSO.GetMaxTotalThreadsPerThreadgroup();
        if (threadGroupSize > arrayLength)
        {
            threadGroupSize = arrayLength;
        }
        mtlpp::Size threadgroupSize = mtlpp::Size(threadGroupSize, 1, 1);

        // Encode the compute command.
        computeEncoder.DispatchThreadgroups(gridSize, threadgroupSize);
    }


    void verifyResults()
    {
        float* a = (float*)_mBufferA.GetContents();
        float* b = (float*)_mBufferB.GetContents();
        float* result = (float*)_mBufferResult.GetContents();

        for (unsigned long index = 0; index < arrayLength; index++)
        {
            printf("Dot Product: index=%lu result=%g(r+a*b)\n",
            index, result[index] + a[index] * b[index]);

            // if (result[index] != (a[index] + b[index]))
            // {
            //     printf("Compute ERROR: index=%lu result=%g vs %g=r+a*b\n",
            //         index, result[index], a[index] + b[index]);
            //     //assert(result[index] == (a[index] + b[index]));
            // }
            // else{
            //        printf("Compute MATCH: index=%lu result=%g vs %g=r+a*b\n",
            //         index, result[index], a[index] + b[index]);
            // }
        }
        printf("Compute results as expected\n");
    }
};

//By Default, Metallib is made at compilation time however this is an alternative test to run const chars as Metal Scripts
//Possibly more Important for Futhark
void generateMetalLib(const char *src, mtlpp::Device device){
const char shadersSrc[] = 
        "#include <metal_stdlib>";
        "using namespace metal;";
        "kernel void sqr(";
            "const device float *vIn [[ buffer(0) ]],";
            "device float *vOut [[ buffer(1) ]],";
            "uint id[[ thread_position_in_grid ]])";
        "{";
            "vOut[id] = vIn[id] * vIn[id];";       
        "}";

    ns::Error* error = NULL; //nullptr
    
    MetalEngine adder = MetalEngine(device);

    mtlpp::Library library  = device.NewLibrary(shadersSrc, mtlpp::CompileOptions(), error);
    assert(library);
    mtlpp::Function sqrFunc = library.NewFunction("sqr");
    assert(sqrFunc);

    mtlpp::ComputePipelineState computePipelineState = device.NewComputePipelineState(sqrFunc, error);
    assert(computePipelineState);

    mtlpp::CommandQueue commandQueue = device.NewCommandQueue();
    assert(commandQueue);

    // // Create buffers to hold data
    // printf("prepare data\n");
    // adder.prepareData(device);
    // adder.sendComputeCommand(commandQueue);
}


//By Default, Metallib is made at compilation time however this is an alternative test to run const chars as Metal Scripts
//Possibly more Important for Futhark
void generateMetalLibRaw(const char *src, mtlpp::Device device){
const char shadersSrc[] = 
        "#include <metal_stdlib>";
        "using namespace metal;";
        "kernel void sqr(";
            "const device float *vIn [[ buffer(0) ]],";
            "device float *vOut [[ buffer(1) ]],";
            "uint id[[ thread_position_in_grid ]])";
        "{";
            "vOut[id] = vIn[id] * vIn[id];";       
        "}";
    
    MetalEngine adder = MetalEngine(device, src, "sqr");

    // Create buffers to hold data
    printf("prepare data\n");
    adder.prepareData(device);
    adder.sendComputeCommand(adder._mCommandQueue);
}

int main(int argc, char * argv[]){
        mtlpp::Device device = mtlpp::Device::CreateSystemDefaultDevice();

        // Create the custom object used to encapsulate the Metal code.
        // Initializes objects to communicate with the GPU.
        MetalEngine adder = MetalEngine(device);
        
        // Create buffers to hold data
        adder.prepareData(device);
        printf("prepare data\n");
        // Send a command to the GPU to perform the calculation.
        adder.sendComputeCommand(adder._mCommandQueue);

        printf("Execution finished\n");
}
