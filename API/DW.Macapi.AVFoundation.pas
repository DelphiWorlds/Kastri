unit DW.Macapi.AVFoundation;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.Foundation, Macapi.CoreFoundation, Macapi.AVFoundation, Macapi.ObjectiveC, Macapi.CoreMedia, Macapi.CocoaTypes, Macapi.Dispatch;

type
  AVPlayerItemOutputClass = interface(NSObjectClass)
    ['{3D89BA95-B673-402D-8046-BEE1763D9FF3}']
  end;

  AVPlayerItemOutput = interface(NSObject)
    ['{D2175E46-20A7-4F62-9D52-714CB52DE422}']
    function itemTimeForCVTimestamp(timestamp: CVTimeStamp): CMTime; cdecl;
    function itemTimeForHostTime(hostTimeInSeconds: CFTimeInterval): CMTime; cdecl;
    function itemTimeForMachAbsoluteTime(machAbsoluteTime: Int64): CMTime; cdecl;
    procedure setSuppressesPlayerRendering(suppressesPlayerRendering: Boolean); cdecl;
    function suppressesPlayerRendering: Boolean; cdecl;
  end;
  TAVPlayerItemOutput = class(TOCGenericImport<AVPlayerItemOutputClass, AVPlayerItemOutput>)
  end;

  AVPlayerItemVideoOutputClass = interface(AVPlayerItemOutputClass)
    ['{99EF1ABB-428A-44FE-B732-3A5CFF735C16}']
  end;

  AVPlayerItemVideoOutput = interface(AVPlayerItemOutput)
    ['{C25F9FB1-E515-4490-88E4-9D734088EF4D}']
    function initWithPixelBufferAttributes(pixelBufferAttributes: NSDictionary): Pointer { instancetype }; cdecl;
    function initWithOutputSettings(outputSettings: NSDictionary): Pointer { instancetype }; cdecl;
    function hasNewPixelBufferForItemTime(itemTime: CMTime): Pointer; cdecl;
    function copyPixelBufferForItemTime(itemTime: CMTime; itemTimeForDisplay: Pointer): CVPixelBufferRef; cdecl;
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
    procedure requestNotificationOfMediaDataChangeWithAdvanceInterval(interval: NSTimeInterval); cdecl;
    function delegate: Pointer; cdecl;
    function delegateQueue: dispatch_queue_t; cdecl;
  end;
  TAVPlayerItemVideoOutput = class(TOCGenericImport<AVPlayerItemVideoOutputClass, AVPlayerItemVideoOutput>)
  end;

implementation

end.
