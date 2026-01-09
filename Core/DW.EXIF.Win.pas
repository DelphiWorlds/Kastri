unit DW.EXIF.Win;

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


// **** NOTE: This unit is work in progress. ****
//  For example, retrieval of Alititude, Latitude and Longitude is incpmplete, and the values retrieved for Make and Model do not appear to be right

interface

uses
  // DW
  DW.EXIF;

type
  TPlatformEXIF = record
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Win
  Winapi.Windows, Winapi.GDIPAPI, Winapi.ActiveX;

type
  TGdip = record
    class function GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: Double): Boolean; overload; static;
    class function GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: ShortInt): Boolean; overload; static;
    class function GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: string): Boolean; overload; static;
    class function GetPropertyItem(const AImage: GpImage; const APropID: PROPID; out APropItem: PPropertyItem): Boolean; static;
  end;

{ TGdip }

class function TGdip.GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: ShortInt): Boolean;
var
  LPropItem: PPropertyItem;
begin
  Result := False;
  if GetPropertyItem(AImage, APropID, LPropItem) then
  try
    if LPropItem^.type_ = PropertyTagTypeShort then
    begin
      AValue := PShortInt(LPropItem^.value)^;
      Result := True;
    end;
  finally
    FreeMem(LPropItem);
  end;
end;

class function TGdip.GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: Double): Boolean;
var
  LPropItem: PPropertyItem;
begin
  Result := False;
  if GetPropertyItem(AImage, APropID, LPropItem) then
  try
    if LPropItem^.type_ = PropertyTagTypeRational then
    begin
      AValue := PDouble(LPropItem^.value)^;
      Result := True;
    end;
  finally
    FreeMem(LPropItem);
  end;
end;

class function TGdip.GetProperty(const AImage: GpImage; const APropID: PROPID; out AValue: string): Boolean;
var
  LPropItem: PPropertyItem;
  LBytes: TBytes;
begin
  Result := False;
  if GetPropertyItem(AImage, APropID, LPropItem) then
  try
    if LPropItem^.type_ = PropertyTagTypeASCII then
    begin
      SetLength(LBytes, LPropItem^.length);
      Move(LPropItem^.value, LBytes[0], LPropItem^.length);
      AValue := TEncoding.Default.GetString(LBytes);
      Result := True;
    end;
  finally
    FreeMem(LPropItem);
  end;
end;

class function TGdip.GetPropertyItem(const AImage: GpImage; const APropID: PROPID; out APropItem: PPropertyItem): Boolean;
var
  LPropSize: UINT;
  LPropItem: PPropertyItem;
begin
  Result := False;
  if GdipGetPropertyItemSize(AImage, APropID, LPropSize) = Status.Ok then
  begin
    LPropItem := AllocMem(LPropSize);
    Result := GdipGetPropertyItem(AImage, APropID, LPropSize, LPropItem) = Status.Ok;
    if Result then
      APropItem := LPropItem
    else
      FreeMem(LPropItem);
  end;
end;

{ TPlatformEXIF }

//    PropertyTagTypeByte      : Integer =  1;
//    PropertyTagTypeASCII     : Integer =  2;
//    PropertyTagTypeShort     : Integer =  3;
//    PropertyTagTypeLong      : Integer =  4;
//    PropertyTagTypeRational  : Integer =  5;
//    PropertyTagTypeUndefined : Integer =  7;
//    PropertyTagTypeSLONG     : Integer =  9;
//    PropertyTagTypeSRational : Integer = 10;

//    PropertyTagEquipMake               = 271
//    PropertyTagEquipModel              = 272
//    PropertyTagGpsAltitude             = 2
//    PropertyTagGpsLatitude             = 6
//    PropertyTagGpsLongitude            = 4
//    PropertyTagOrientation             = 274

class function TPlatformEXIF.GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean;
var
  LToken: ULONG_PTR;
  LInput: GdiplusStartupInput;
  LImage: GpImage;
  LPropSize: UINT;
  LPropItem: PPropertyItem;
  LStatus: Status;
  LOrientation: ShortInt;
begin
  Result := False;
  LInput.GdiplusVersion := 1;
  LInput.SuppressBackgroundThread := False;
  LInput.SuppressExternalCodecs := False;
  LInput.DebugEventCallback := nil;
  LStatus := GdiplusStartup(LToken, @LInput, nil);
  if LStatus = Status.Ok then
  try
    if GdipLoadImageFromFile(PChar(AFileName), LImage) = Status.Ok then
    try
      TGdip.GetProperty(LImage, PropertyTagOrientation, LOrientation);
      AProperties.Orientation := TEXIFOrientation(LOrientation);
      TGdip.GetProperty(LImage, PropertyTagGpsAltitude, AProperties.Altitude);
      TGdip.GetProperty(LImage, PropertyTagGpsLatitude, AProperties.Latitude);
      TGdip.GetProperty(LImage, PropertyTagGpsLongitude, AProperties.Longitude);
      TGdip.GetProperty(LImage, PropertyTagEquipMake, AProperties.CameraMake);
      TGdip.GetProperty(LImage, PropertyTagEquipModel, AProperties.CameraModel);
      Result := True;
    finally
      GdipFree(LImage);
    end;
  finally
    GdiplusShutdown(LToken);
  end;
end;

end.
