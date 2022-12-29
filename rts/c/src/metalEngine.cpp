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
void mtlAddArrays(const float* inA,
                const float* inB,
                float* result,
                int length)
{
    for (int index = 0; index < length ; index++)
    {
        result[index] = inA[index] + inB[index];
    }
}

//end define Metal Reflections

   class MetalEngine
      {
          public:
              mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();

              // The compute pipeline generated from the compute kernel in the .metal shader file.
              mtlpp::ComputePipelineState _mFunctionPSO;

              // The command queue used to pass commands to the device.
              mtlpp::CommandQueue _mCommandQueue;

              // Buffers to hold data.
              mtlpp::Buffer _mBufferA;
              mtlpp::Buffer _mBufferB;
              mtlpp::Buffer _mBufferResult;

              //Pointer for Any Errors generated
              ns::Error* error; //nullptr

    
          MetalEngine()
          {
                _mDevice = mtlpp::Device::CreateSystemDefaultDevice();
                ns::Error* error = NULL;

                // Load the shader files with a .metal file extension in the project

                mtlpp::Library defaultLibrary = _mDevice.NewLibrary("fut.metallib", error);
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");

                }
                mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                // Create a compute pipeline state object.
                _mFunctionPSO =  _mDevice.NewComputePipelineState(addFunction, error);
            
                _mCommandQueue = _mDevice.NewCommandQueue();
          }
      
          MetalEngine(mtlpp::Device device)
          {
                _mDevice = device;
                ns::Error* error = NULL;

                // Load the shader files with a .metal file extension in the project

                mtlpp::Library defaultLibrary = device.NewLibrary("fut.metallib", error);
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");

                }
                mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                // Create a compute pipeline state object.
                _mFunctionPSO = device.NewComputePipelineState(addFunction, error);
            
                _mCommandQueue = device.NewCommandQueue();
          }
          //Initializes based on a path
          MetalEngine(ns::String libraryPath, ns::String mtlFunction, mtlpp::Device device)
          {
              _mDevice = device;
              //device.NewDefaultLibrary();
              error = NULL;

              // Load the shader files with a .metal file extension in the project
              // .metallib
              mtlpp::Library defaultLibrary = device.NewLibrary(libraryPath, error);
              if (defaultLibrary.GetFunctionNames() == NULL)
              {
                  printf("Failed to find the default library.\n");
              }
              mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

              // Create a compute pipeline state object.
              _mFunctionPSO = device.NewComputePipelineState(Function, error);
              _mCommandQueue = device.NewCommandQueue();
          }

          MetalEngine(ns::String mtlFunction, mtlpp::Device device)
          {
              _mDevice = device;
              //device.NewDefaultLibrary();
              error = NULL;

              // Load the shader files with a .metal file extension in the project
              // .metallib
              mtlpp::Library defaultLibrary = device.NewDefaultLibrary();
              if (defaultLibrary.GetFunctionNames() == NULL)
              {
                  printf("Failed to find the default library.\n");

              }
              //"mtlAddArrays"
              mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

              // Create a compute pipeline state object.
              _mFunctionPSO = device.NewComputePipelineState(Function, error);
          
              _mCommandQueue = device.NewCommandQueue();
          }

          //By Default, Metallib is made at compilation time however this is an 
          //alternative constructor to run const chars as Metal Scripts
          //Possibly more Important for Futhark
          MetalEngine(const char src[], ns::String functionName, mtlpp::Device device){

              _mDevice = device; 

              error = NULL; //nullptr
              mtlpp::Library library  = device.NewLibrary(src, mtlpp::CompileOptions(), error);
              assert(library);
              mtlpp::Function Func = library.NewFunction(functionName);
              assert(Func);

              _mFunctionPSO = device.NewComputePipelineState(Func, error);
              assert(_mFunctionPSO);

              _mCommandQueue = device.NewCommandQueue();
              assert(_mCommandQueue);
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
              computeEncoder.SetComputePipelineState(_mFunctionPSO);
              computeEncoder.SetBuffer(_mBufferA, 0, 0);
              computeEncoder.SetBuffer(_mBufferB, 0, 1);
              computeEncoder.SetBuffer(_mBufferResult, 0, 2);
              //_mBufferResult offset:x atIndex:y

              mtlpp::Size gridSize = mtlpp::Size(arrayLength, 1, 1);

              // Calculate a threadgroup size.
              uint32_t threadGroupSize = _mFunctionPSO.GetMaxTotalThreadsPerThreadgroup();
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
                  if (result[index] != (a[index] + b[index]))
                  {
                      printf("Compute ERROR: index=%lu result=%g vs %g=a+b\n",
                          index, result[index], a[index] + b[index]);
                      //assert(result[index] == (a[index] + b[index]));
                  }
                  else{
                        printf("Compute MATCH: index=%lu result=%g vs %g=a+b\n",
                          index, result[index], a[index] + b[index]);
                  }
              }
              printf("Compute results as expected\n");
          }


          void execute(){
            // Create the custom object used to encapsulate the Metal code.
            // Initializes objects to communicate with the GPU.
            MetalEngine engine = MetalEngine();
            //MetalEngine engineAlt = MetalEngine(argv, device);

            // Create buffers to hold data
            engine.prepareData(engine._mDevice);
            
            // Send a command to the GPU to perform the calculation.
            engine.sendComputeCommand(engine._mCommandQueue);

            printf("Execution finished\n");
          }
};