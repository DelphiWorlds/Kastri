unit DW.iOSapi.MLKitVision;

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

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreMedia, iOSapi.UIKit,
  // DW
  DW.iOSapi.MLKitCommon;

type
  MLKVisionPoint = interface;
  MLKVision3DPoint = interface;
  MLKVisionImage = interface;

  MLKVisionPointClass = interface(NSObjectClass)
    ['{AC901EC1-C145-49F3-A8D4-2951AA5E80E0}']
  end;

  MLKVisionPoint = interface(NSObject)
    ['{483C273E-E7CA-49AB-BABF-FA7334FD086F}']
    function x: CGFloat; cdecl;
    function y: CGFloat; cdecl;
  end;
  TMLKVisionPoint = class(TOCGenericImport<MLKVisionPointClass, MLKVisionPoint>) end;

  MLKVision3DPointClass = interface(MLKVisionPointClass)
    ['{D8265EDF-6A3A-40EE-86EC-5ECA76C248A3}']
  end;

  MLKVision3DPoint = interface(MLKVisionPoint)
    ['{5DC54099-95AD-4269-B97B-B1462E007AB9}']
    function z: CGFloat; cdecl;
  end;
  TMLKVision3DPoint = class(TOCGenericImport<MLKVision3DPointClass, MLKVision3DPoint>) end;

  MLKVisionImageClass = interface(NSObjectClass)
    ['{75814524-B6C4-4BF5-8FDC-A061129C881E}']
  end;

  MLKVisionImage = interface(NSObject)
    ['{766A719B-4865-48FC-B38C-5DF2CA739353}']
    function initWithBuffer(sampleBuffer: CMSampleBufferRef): Pointer; cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
    function orientation: UIImageOrientation; cdecl;
    procedure setOrientation(orientation: UIImageOrientation); cdecl;
  end;
  TMLKVisionImage = class(TOCGenericImport<MLKVisionImageClass, MLKVisionImage>) end;

const
  libMLKitVision = 'MLKitVision';

implementation

procedure MLKitVisionLoader; cdecl; external framework libMLKitVision;

end.