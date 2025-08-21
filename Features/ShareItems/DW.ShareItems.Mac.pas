unit DW.ShareItems.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Mac
  Macapi.ObjectiveC,
  // iOS
  Macapi.AppKit, Macapi.Foundation,
  // FMX
  FMX.Controls,
  // DW
  DW.Macapi.AppKit, DW.ShareItems;

type
  TPlatformShareItems = class;

  NSSharingServicePickerDelegateAbridged = interface(IObjectiveC)
    ['{35E5A733-DF7C-40AA-8A1B-6B7DB4E293D3}']
    [MethodName('sharingServicePicker:didChooseSharingService:')]
    procedure sharingServicePickerDidChooseSharingService(sharingServicePicker: NSSharingServicePicker;
      didChooseSharingService: NSSharingService); cdecl;
  end;

  TSharingServicePickerDelegate = class(TOCLocal, NSSharingServicePickerDelegateAbridged)
  private
    FPlatformShareItems: TPlatformShareItems;
  public
    { NSSharingServicePickerDelegateAbridged }
    [MethodName('sharingServicePicker:didChooseSharingService:')]
    procedure sharingServicePickerDidChooseSharingService(sharingServicePicker: NSSharingServicePicker;
      didChooseSharingService: NSSharingService); cdecl;
  public
    constructor Create(const APlatformShareItems: TPlatformShareItems);
  end;

  TPlatformShareItems = class(TCustomPlatformShareItems)
  private
    FPicker: NSSharingServicePicker;
    FPickerDelegate: TSharingServicePickerDelegate;
  protected
    procedure DoShareCompleted(const AActivity: TShareActivity; const AError: string); override;
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); override;
  public
    constructor Create(const AShareItems: TShareItems); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Types, System.SysUtils,
  // Mac
  Macapi.Helpers, Macapi.CocoaTypes, Macapi.CoreGraphics,
  // FMX
  FMX.Graphics, FMX.Types, FMX.Consts, FMX.Platform.Mac, FMX.Forms;

{ TSharingServicePickerDelegate }

constructor TSharingServicePickerDelegate.Create(const APlatformShareItems: TPlatformShareItems);
begin
  inherited Create;
  FPlatformShareItems := APlatformShareItems;
end;

procedure TSharingServicePickerDelegate.sharingServicePickerDidChooseSharingService(sharingServicePicker: NSSharingServicePicker;
  didChooseSharingService: NSSharingService);
begin
  if NSStrToStr(didChooseSharingService.title).IsEmpty then
    FPlatformShareItems.DoShareCompleted(TShareActivity.None, '')
  else
    FPlatformShareItems.DoShareCompleted(TShareActivity.Unknown, '');
end;

{ TPlatformShareItems }

constructor TPlatformShareItems.Create(const AShareItems: TShareItems);
begin
  inherited;
  FPickerDelegate := TSharingServicePickerDelegate.Create(Self);
end;

destructor TPlatformShareItems.Destroy;
begin
  FPickerDelegate.Free;
  inherited;
end;

procedure TPlatformShareItems.DoShareCompleted(const AActivity: TShareActivity; const AError: string);
begin
  inherited;
end;

function PixelFormatToCGBitmapInfo(const APixelFormat: TPixelFormat): CGBitmapInfo;
begin
  case APixelFormat of
    TPixelFormat.RGB:
      Result := kCGImageAlphaNone or kCGBitmapByteOrder32Big;
    TPixelFormat.RGBA:
      Result := kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big;
    TPixelFormat.BGR:
      Result := kCGImageAlphaNone or kCGBitmapByteOrder32Little;
    TPixelFormat.BGRA:
      Result := kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little;
  else
    raise Exception.CreateRes(@SBitmapFormatUnsupported);
  end;
end;

function BitmapToNSImage(const Bitmap: TBitmap): NSImage;
var
  ImageRef: CGImageRef;
  CtxRef: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  BitmapData: TBitmapData;
  BitmapInfo: CGBitmapInfo;
begin
  if Bitmap.IsEmpty then
    Result := TNSImage.Create
  else
  begin
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      BitmapInfo := PixelFormatToCGBitmapInfo(Bitmap.PixelFormat);
      if Bitmap.Map(TMapAccess.Read, BitmapData) then
      try
        CtxRef := CGBitmapContextCreate(BitmapData.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, BitmapInfo);
        try
          ImageRef := CGBitmapContextCreateImage(CtxRef);
          try
            Result := TNSImage.Alloc;
            Result.initWithCGImage(ImageRef, CGSizeMake(Bitmap.Width, Bitmap.Height));
          finally
            CGImageRelease(ImageRef);
          end;
        finally
          CGContextRelease(CtxRef);
        end;
      finally
        Bitmap.Unmap(BitmapData);
      end;
    finally
      CGColorSpaceRelease(ColorSpace);
    end;
  end;
end;

procedure TPlatformShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
var
  LActivityItems: NSMutableArray;
  LItem: TSharingItem;
  LView: NSView;
  LURL: NSURL;
  LRect: TRectF;
  LNativeRect: NSRect;
begin
  if (AControl <> nil) and (AControl.Root <> nil) and (AControl.Root.GetObject is TCommonCustomForm) then
  begin
    LView := WindowHandleToPlatform(TCommonCustomForm(AControl.Root.GetObject).Handle).View;
    LActivityItems := TNSMutableArray.Create;
    for LItem in Items do
    begin
      if LItem is TSharingItemText then
        LActivityItems.addObject(StringToID(TSharingItemText(LItem).Text))
      else if LItem is TSharingItemFile then
      begin
        LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(TSharingItemFile(LItem).Text)));
        LActivityItems.addObject(TNSitemProvider.Alloc.initWithContentsOfURL(LURL));
      end
      else if LItem is TSharingItemImage then
        LActivityItems.addObject(NSObjectToID(BitmapToNSImage(TSharingItemImage(LItem).Image)))
    end;
    FPicker := nil;
    FPicker := TNSSharingServicePicker.Wrap(TNSSharingServicePicker.Alloc.initWithItems(LActivityItems));
    FPicker.setDelegate(FPickerDelegate.GetObjectID);
    LRect := AControl.AbsoluteRect;
    LNativeRect := MakeNSRect(LRect.Left, LView.Bounds.size.height - LRect.Bottom, LRect.Width, LRect.Height);
    FPicker.showRelativeToRect(LNativeRect, LView, CGRectMinYEdge);
  end;
end;

end.
