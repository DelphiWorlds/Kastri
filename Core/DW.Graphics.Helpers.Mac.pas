unit DW.Graphics.Helpers.Mac;

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
  {$IF Defined(IOS)}
  // iOS
  iOSapi.CoreVideo, iOSapi.CoreGraphics,
  {$ELSEIF Defined(MACOS)}
  // macOS
  Macapi.CocoaTypes,
  {$IF CompilerVersion > 36}
  DW.Macapi.CoreVideo,
  {$ENDIF}
  {$ENDIF}
  // FMX
  FMX.Graphics;

type
  TBitmapHelper = class helper for TBitmap
  public
    /// <summary>
    ///   Imports from a CGImageRef into this bitmap
    /// </summary>
    function FromCGImageRef(const AImgRef: CGImageRef): Boolean;
    /// <summary>
    ///   Imports from a PixelBuffer into this bitmap
    /// </summary>
    function FromPixelBuffer(const APixelBufferRef: CVPixelBufferRef): Boolean;
    /// <summary>
    ///   Exports from this bitmap to a CGImageRef
    /// </summary>
    function ToCGImageRef(const AWidth, AHeight: Integer): CGImageRef;
    /// <summary>
    ///   Exports from this bitmap to a PixelBuffer
    /// </summary>
    function ToPixelBuffer(const AWidth, AHeight: Integer): CVPixelBufferRef;
  end;

implementation

uses
  // RTL
  System.Types, System.UITypes,
  // macOS
  Macapi.Helpers, Macapi.CoreFoundation, Macapi.ObjectiveC,
  {$IF Defined(IOS)}
  iOSapi.Foundation, DW.iOSapi.CoreVideo,
  {$ELSEIF Defined(MACOS)}
  Macapi.CoreGraphics, Macapi.Foundation,
  {$IF CompilerVersion < 37}
  Macapi.CoreVideo,
  {$ENDIF}
  {$ENDIF}
  // FMX
  FMX.Surfaces, FMX.Types;

var
  GColorSpace: Pointer;

function ColorSpace: Pointer;
begin
  if GColorSpace = nil then
    GColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := GColorSpace;
end;

{ TBitmapHelper }

function TBitmapHelper.FromCGImageRef(const AImgRef: CGImageRef): Boolean;
var
  LCtxRef: CGContextRef;
  LWidth, LHeight: Cardinal;
  LSurface: TBitmapSurface;
begin
  Result := False;
  LWidth := CGImageGetWidth(AImgRef);
  LHeight := CGImageGetHeight(AImgRef);
  LSurface := TBitmapSurface.Create;
  try
    LSurface.SetSize(LWidth, LHeight, TPixelFormat.RGBA);
    LCtxRef := CGBitmapContextCreate(LSurface.Bits, LSurface.Width, LSurface.Height, 8, LSurface.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
    if LCtxRef <> nil then
    try
      CGContextDrawImage(LCtxRef, CGRectFromRect(RectF(0, 0, LSurface.Width, LSurface.Height)), AImgRef);
      Assign(LSurface);
      Result := True;
    finally
      CGContextRelease(LCtxRef);
    end;
  finally
    LSurface.Free;
  end;
end;

function TBitmapHelper.FromPixelBuffer(const APixelBufferRef: CVPixelBufferRef): Boolean;
var
  I, LWidth, LHeight: Integer;
  LBytesPerRow: Integer;
  LBaseAddress: Pointer;
  LAlphaColorLine: Pointer;
  LMap: TBitmapData;
begin
  Result := False;
  if CVPixelBufferLockBaseAddress(APixelBufferRef, 0) = 0 then
  try
    LBytesPerRow := CVPixelBufferGetBytesPerRow(APixelBufferRef);
    LWidth := CVPixelBufferGetWidth(APixelBufferRef);
    LHeight := CVPixelBufferGetHeight(APixelBufferRef);
    if (LWidth <> Width) or (LHeight <> Height) then
      SetSize(LWidth, LHeight);
    LBaseAddress := CVPixelBufferGetBaseAddress(APixelBufferRef);
    if Map(TMapAccess.Write, LMap) then
    try
      GetMem(LAlphaColorLine, LWidth * 4);
      try
        for I := 0 to LHeight - 1 do
        begin
          ScanlineToAlphaColor(@PLongByteArray(LBaseAddress)[LBytesPerRow * I], LAlphaColorLine, LWidth, TPixelFormat.BGRA);
          AlphaColorToScanline(LAlphaColorLine, LMap.GetScanline(I), LWidth, TPixelFormat.RGBA);
        end;
        Result := True;
      finally
        FreeMem(LAlphaColorLine);
      end;
    finally
      Unmap(LMap);
    end;
  finally
    CVPixelBufferUnlockBaseAddress(APixelBufferRef, 0);
  end;
end;

function TBitmapHelper.ToCGImageRef(const AWidth, AHeight: Integer): CGImageRef;
var
  LContext: CGContextRef;
  LBitmapData: TBitmapData;
begin
  Result := nil;
  if Map(TMapAccess.Read, LBitmapData) then
  try
    LContext := CGBitmapContextCreate(LBitmapData.Data, AWidth, AHeight, 8, LBitmapData.Pitch, ColorSpace,
      kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
    try
      Result := CGBitmapContextCreateImage(LContext);
    finally
      CGContextRelease(LContext);
    end;
  finally
    Unmap(LBitmapData);
  end;
end;

function TBitmapHelper.ToPixelBuffer(const AWidth, AHeight: Integer): CVPixelBufferRef;
var
  LOptions: NSMutableDictionary;
  LStatus: CVReturn;
  LBuffer: CVPixelBufferRef;
  LData: Pointer;
  LContext: CGContextRef;
  LImage: CGImageRef;
begin
  LOptions := TNSMutableDictionary.Create;
  {$IF (CompilerVersion < 37) or Defined(OSX)}
  Result := 0;
  LBuffer := 0;
  LOptions.setObject(TNSNumber.OCClass.numberWithBool(True), kCVPixelBufferCGImageCompatibilityKey);
  LOptions.setObject(TNSNumber.OCClass.numberWithBool(True), kCVPixelBufferCGBitmapContextCompatibilityKey);
  {$ELSE}
  LBuffer := nil;
  Result := nil;
  LOptions.setObject(NSObjectToID(TNSNumber.OCClass.numberWithBool(True)), kCVPixelBufferCGImageCompatibilityKey);
  LOptions.setObject(NSObjectToID(TNSNumber.OCClass.numberWithBool(True)), kCVPixelBufferCGBitmapContextCompatibilityKey);
  {$ENDIF}
  LStatus := CVPixelBufferCreate(kCFAllocatorDefault, AWidth, AHeight, kCVPixelFormatType_32ARGB, NSObjectToID(LOptions), @LBuffer);
  {$IF CompilerVersion < 37}
  if (LStatus = kCVReturnSuccess) and (LBuffer <> 0) then
  {$ELSE}
  if (LStatus = kCVReturnSuccess) and (LBuffer <> nil) then
  {$ENDIF}
  begin
    CVPixelBufferLockBaseAddress(LBuffer, 0);
    try
      LData := CVPixelBufferGetBaseAddress(LBuffer);
      if LData <> nil then
      begin
        LContext := CGBitmapContextCreate(LData, AWidth, AHeight, 8, AWidth * 4, ColorSpace, kCGImageAlphaNoneSkipFirst);
        try
          CGContextConcatCTM(LContext, CGAffineTransformMakeRotation(0));
          LImage := ToCGImageRef(AWidth, AHeight);
          try
            CGContextDrawImage(LContext, CGRectMake(0, 0, CGImageGetWidth(LImage), CGImageGetHeight(LImage)), LImage);
          finally
            CGImageRelease(LImage);
          end;
        finally
          CGContextRelease(LContext);
        end;
      end;
    finally
      CVPixelBufferUnlockBaseAddress(LBuffer, 0);
    end;
    Result := LBuffer;
  end;
end;

initialization

finalization
  if GColorSpace <> nil then
    CGColorSpaceRelease(GColorSpace);

end.
