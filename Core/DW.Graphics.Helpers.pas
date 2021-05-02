unit DW.Graphics.Helpers;

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
  // FMX
  FMX.Graphics;

type
  /// <summary>
  ///   Helper class that provides convenience methods that extend the class
  /// </summary>
  TBitmapHelper = class helper for TBitmap
  private
    function GetAsBase64: string;
    procedure SetAsBase64(const ASource: string);
  public
    /// <summary>
    ///   Converts a bitmap to/from a base64 (character) representation
    /// </summary>
    property AsBase64: string read GetAsBase64 write SetAsBase64;
  end;

implementation

uses
  // RTL
  System.Classes, System.SysUtils, System.NetEncoding;

{ TBitmapHelper }

function TBitmapHelper.GetAsBase64: string;
var
  LInputStream: TBytesStream;
  LOutputStream: TStringStream;
begin
  Result := '';
  if IsEmpty then
    Exit; // <======
  LInputStream := TBytesStream.Create;
  try
    SaveToStream(LInputStream);
    LInputStream.Position := 0;
    LOutputStream := TStringStream.Create('');
    try
      TNetEncoding.Base64.Encode(LInputStream, LOutputStream);
      Result := LOutputStream.DataString;
    finally
      LOutputStream.Free;
    end;
  finally
    LInputStream.Free;
  end;
end;

procedure TBitmapHelper.SetAsBase64(const ASource: string);
var
  LInputStream: TStringStream;
  LOutputStream: TBytesStream;
begin
  LInputStream := TStringStream.Create(ASource);
  try
    LInputStream.Position := 0;
    LOutputStream := TBytesStream.Create;
    try
      TNetEncoding.Base64.Decode(LInputStream, LOutputStream);
      LOutputStream.Position := 0;
      LoadFromStream(LOutputStream);
    finally
      LOutputStream.Free;
    end;
  finally
    LInputStream.Free;
  end;
end;

end.
