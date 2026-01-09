unit DW.Macapi.Simd;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

type
  simd_int2 = record
    values: array[0..1] of Integer;
  end align 16;
  Psimd_int2 = ^simd_int2;

  vector_int2 = simd_int2;
  Pvector_int2 = ^vector_int2;

  simd_int4 = record
    values: array[0..3] of Integer;
  end align 16;
  Psimd_int4 = ^simd_int4;

  vector_int4 = simd_int4;
  Pvector_int4 = ^vector_int4;

  simd_uint2 = record
    values: array[0..1] of UInt32; //!!!!
  end align 16;
  Psimd_uint2 = ^simd_uint2;

  vector_uint2 = simd_uint2;
  Pvector_uint2 = ^vector_uint2;

  simd_uint3 = record
    values: array[0..3] of UInt32; //!!!!
  end align 16;
  Psimd_uint3 = ^simd_uint3;

  vector_uint3 = simd_uint3;
  Pvector_uint3 = ^vector_uint3;

  simd_double2 = record
    values: array[0..1] of Double;
  end align 16;
  Psimd_double2 = ^simd_double2;

  vector_double2 = simd_double2;
  Pvector_double2 = ^vector_double2;

  simd_double3 = record
    values: array[0..2] of Double;
  end align 16;
  Psimd_double3 = ^simd_double3;

  vector_double3 = simd_double3;
  Pvector_double3 = ^vector_double3;

  simd_double4 = record
    values: array[0..3] of Double;
  end align 16;
  Psimd_double4 = ^simd_double4;

  vector_double4 = simd_double4;
  Pvector_double4 = ^vector_double4;

  simd_float2 = record
    values: array[0..1] of Single;
  end align 16;
  Psimd_float2 = ^simd_float2;

  vector_float2 = simd_float2;
  Pvector_float2 = ^vector_float2;

  simd_float3 = record
    values: array[0..2] of Single;
  end align 16;
  Psimd_float3 = ^simd_float3;

  vector_float3 = simd_float3;
  Pvector_float3 = ^vector_float3;

  simd_float4 = record
    values: array[0..3] of Single;
  end align 16;
  Psimd_float4 = ^simd_float4;

  vector_float4 = simd_float4;
  Pvector_float4 = ^vector_float4;

  simd_float2x2 = record
    values: array[0..1] of simd_float2;
  end align 16;
  Psimd_float2x2 = ^simd_float2x2;

  matrix_float2x2 = simd_float2x2;
  Pmatrix_float2x2 = ^simd_float2x2;

  simd_float3x3 = record
    values: array[0..2] of simd_float3;
  end align 16;
  Psimd_float3x3 = ^simd_float3x3;

  matrix_float3x3 = simd_float3x3;
  Pmatrix_float3x3 = ^matrix_float3x3;

  simd_float4x3 = record
    values: array[0..2] of simd_float4;
  end align 16;
  Psimd_float4x3 = ^simd_float4x3;

  matrix_float4x3 = simd_float4x3;
  Pmatrix_float4x3 = ^simd_float4x3;

  simd_float4x4 = record
    values: array[0..3] of simd_float4;
  end align 16;
  Psimd_float4x4 = ^simd_float4x4;

  matrix_float4x4 = simd_float4x4;
  Pmatrix_float4x4 = ^matrix_float4x4;

  simd_double4x4 = record
    values: array[0..3] of simd_double4;
  end align 16;
  Psimd_double4x4 = ^simd_double4x4;

  matrix_double4x4 = simd_double4x4;
  Pmatrix_double4x4 = ^matrix_double4x4;

  simd_quatf = record
    values: array of simd_float4;
  end align 16;
  Psimd_quatf = ^simd_quatf;

  simd_quatd = record
    values: array of simd_double4;
  end align 16;
  Psimd_quatd = ^simd_quatd;

implementation

end.
