/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A shader that adds two arrays of floats.
*/

#include <metal_stdlib>
using namespace metal;
/// This is a Metal Shading Language (MSL) function equivalent to the mtlAddArrays() C function, used to perform the calculation on a GPU.
kernel void mtlAddArrays(device const float* inA,
                       device const float* inB,
                       device float* result,
                       uint index [[thread_position_in_grid]])
{
    // the for-loop is replaced with a collection of threads, each of which
    // calls this function.
    result[index] = inA[index] + inB[index];
}
