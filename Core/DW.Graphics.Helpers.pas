unit DW.Graphics.Helpers;

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
  // FMX
  FMX.Graphics;

type
  /// <summary>
  ///   Helper class that provides convenience methods that extend the class
  /// </summary>
  TBitmapHelper = class helper for TBitmap
  private
    function GetAsBase64: string;
    function GetAsCompressedBase64: string;
    procedure SetAsBase64(const AValue: string);
    procedure SetAsCompressedBase64(const AValue: string);
  public
    /// <summary>
    ///   Loads an image from a resource compiled into the application
    /// </summary>
    procedure LoadFromResource(const AResourceName: string);
    /// <summary>
    ///   Converts a bitmap to/from a base64 (character) representation
    /// </summary>
    property AsBase64: string read GetAsBase64 write SetAsBase64;
    /// <summary>
    ///   Converts a bitmap to/from a compressed (ZLib) base64 (character) representation
    /// </summary>
    property AsCompressedBase64: string read GetAsCompressedBase64 write SetAsCompressedBase64;
  end;

implementation

uses
  // RTL
  System.Classes, System.SysUtils, System.NetEncoding, System.Types,
  // DW
  DW.Base64.Helpers;

{ TBitmapHelper }

procedure TBitmapHelper.LoadFromResource(const AResourceName: string);
var
  LStream: TStream;
begin
  if FindResource(HInstance, PChar(AResourceName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

function TBitmapHelper.GetAsBase64: string;
var
  LInputStream: TBytesStream;
begin
  Result := '';
  if not IsEmpty then
  begin
    LInputStream := TBytesStream.Create;
    try
      SaveToStream(LInputStream);
      Result := TBase64Helper.Encode(LInputStream);
    finally
      LInputStream.Free;
    end;
  end;
end;

function TBitmapHelper.GetAsCompressedBase64: string;
var
  LInputStream: TBytesStream;
begin
  Result := '';
  if not IsEmpty then
  begin
    LInputStream := TBytesStream.Create;
    try
      SaveToStream(LInputStream);
      Result := TBase64Helper.CompressEncode(LInputStream);
    finally
      LInputStream.Free;
    end;
  end;
end;

procedure TBitmapHelper.SetAsBase64(const AValue: string);
var
  LOutputStream: TBytesStream;
begin
  LOutputStream := TBytesStream.Create;
  try
    TBase64Helper.Decode(AValue, LOutputStream);
    LOutputStream.Position := 0;
    LoadFromStream(LOutputStream);
  finally
    LOutputStream.Free;
  end;
end;

procedure TBitmapHelper.SetAsCompressedBase64(const AValue: string);
var
  LOutputStream: TBytesStream;
begin
  LOutputStream := TBytesStream.Create;
  try
    TBase64Helper.DecodeDecompress(AValue, LOutputStream);
    LOutputStream.Position := 0;
    LoadFromStream(LOutputStream);
  finally
    LOutputStream.Free;
  end;
end;

end.
