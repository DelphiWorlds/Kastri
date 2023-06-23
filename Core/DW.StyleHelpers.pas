unit DW.StyleHelpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.UITypes, System.JSON,
  // FMX
  FMX.Controls;

type
  TStyleBookHelper = class helper for TStyleBook
  public
    /// <summary>
    ///   Loads a stylebook from a resource compiled (default name: StyleBook) into the application
    /// </summary>
    procedure LoadFromResource(const AResourceName: string = '');
  end;

  TStyleResource = record
  private
    /// <summary>
    ///   Gets a color value that is a string in a format that can be interpreted by StrToUInt64 (e.g.: $FF03C396)
    /// </summary>
    function GetColor(const AName: string; const AJSON: TJSONValue): TAlphaColor;
  public
    ButtonColor: TAlphaColor;
    /// <summary>
    ///   Loads JSON with style information from a resource (default name: Style) compiled into the application
    /// </summary>
    procedure LoadFromResource(const AResourceName: string = '');
    /// <summary>
    ///   Loads style information from a stream
    /// </summary>
    procedure LoadFromStream(const AStream: TStream);
  end;

implementation

uses
  // RTL
  System.Types, System.SysUtils;

const
  cStyleBookResourceNameDefault = 'StyleBook';
  cStyleResourceNameDefault = 'Style';

{ TStyleBookHelper }

procedure TStyleBookHelper.LoadFromResource(const AResourceName: string = '');
var
  LStream: TStream;
  LResourceName: string;
begin
  if LResourceName.IsEmpty then
    LResourceName := cStyleBookResourceNameDefault
  else
    LResourceName := AResourceName;
  if FindResource(HInstance, PChar(LResourceName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, LResourceName, RT_RCDATA);
    try
      LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

{ TStyleResource }

function TStyleResource.GetColor(const AName: string; const AJSON: TJSONValue): TAlphaColor;
var
  LValue: string;
begin
  Result := TAlphaColors.Null;
  if AJSON.TryGetValue(AName, LValue) then
    Result := StrToUInt64Def(LValue, 0);
end;

procedure TStyleResource.LoadFromResource(const AResourceName: string = '');
var
  LStream: TStream;
  LResourceName: string;
begin
  if LResourceName.IsEmpty then
    LResourceName := cStyleResourceNameDefault
  else
    LResourceName := AResourceName;
  if FindResource(HInstance, PChar(LResourceName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, LResourceName, RT_RCDATA);
    try
      LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

procedure TStyleResource.LoadFromStream(const AStream: TStream);
var
  LJSONStream: TStringStream;
  LJSON: TJSONValue;
begin
  AStream.Position := 0;
  LJSONStream := TStringStream.Create;
  try
    LJSONStream.CopyFrom(AStream);
    LJSON := TJSONObject.ParseJSONValue(LJSONStream.DataString);
    if LJSON <> nil then
    try
      ButtonColor := GetColor('ButtonColor', LJSON);
    finally
      LJSON.Free;
    end;
  finally
    LJSONStream.Free;
  end;
end;

end.
