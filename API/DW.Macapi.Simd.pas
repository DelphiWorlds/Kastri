unit DW.Macapi.Simd;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
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

  simd_quatf = record
    values: array of simd_float4;
  end align 16;
  Psimd_quatf = ^simd_quatf;

implementation

end.
