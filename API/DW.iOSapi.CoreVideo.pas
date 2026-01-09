unit DW.iOSapi.CoreVideo;

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

uses
  // macOS
  Macapi.CoreFoundation;

function kCVPixelBufferCGImageCompatibilityKey: CFStringRef;
function kCVPixelBufferCGBitmapContextCompatibilityKey: CFStringRef;

implementation

uses
  // iOS
  iOSapi.Foundation, iOSapi.CoreVideo;

function kCVPixelBufferCGImageCompatibilityKey: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libCoreVideo, 'kCVPixelBufferCGImageCompatibilityKey')^);
end;

function kCVPixelBufferCGBitmapContextCompatibilityKey: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libCoreVideo, 'kCVPixelBufferCGBitmapContextCompatibilityKey')^);
end;

end.
