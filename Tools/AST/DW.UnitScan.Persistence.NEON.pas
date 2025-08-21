unit DW.UnitScan.Persistence.NEON;

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
  // DW
  DW.UnitScan;

type
  TSymbolUnitMapsHelper = record helper for TSymbolUnitMaps
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils, System.Rtti,
  // NEON
  Neon.Core.Persistence.JSON;

{ TSymbolUnitMapsHelper }

procedure TSymbolUnitMapsHelper.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    Items := TNeon.JSONToValue<TUnitMaps>(TFile.ReadAllText(AFileName));
end;

procedure TSymbolUnitMapsHelper.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, TNeon.ValueToJSONString(TValue.From(Items)));
end;

end.
