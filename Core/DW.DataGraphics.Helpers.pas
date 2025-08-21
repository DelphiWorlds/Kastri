unit DW.DataGraphics.Helpers;

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
  // Data
  Data.DB,
  // FMX
  FMX.Graphics;

type
  TBlobFieldHelper = class helper for TBlobField
  public
    /// <summary>
    ///   Copies the contents of a blob field to a bitmap.
    /// </summary>
    procedure CopyToBitmap(const ABitmap: TBitmap);
    /// <summary>
    ///   Loads the contents of a blob field from a bitmap.
    /// </summary>
    procedure LoadFromBitmap(const ABitmap: TBitmap);
  end;

implementation

uses
  // RTL
  System.Classes;

{ TBlobFieldHelper }

procedure TBlobFieldHelper.CopyToBitmap(const ABitmap: TBitmap);
var
  LStream: TStream;
begin
  LStream := TMemoryStream.Create;
  try
    SaveToStream(LStream);
    LStream.Position := 0;
    ABitmap.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TBlobFieldHelper.LoadFromBitmap(const ABitmap: TBitmap);
var
  LStream: TStream;
begin
  if ABitmap.Size.IsZero then
    Exit; // <=======
  LStream := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(LStream);
    LStream.Position := 0;
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

end.
