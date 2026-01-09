unit DW.Permissions.Helpers;

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
  System.Permissions, System.Types;

type
  {$IF CompilerVersion < 35}
  TPermissionArray = TArray<string>;
  TPermissionStatusArray = TArray<TPermissionStatus>;
  {$ELSE}
  TPermissionArray = TClassicStringDynArray;
  TPermissionStatusArray = TClassicPermissionStatusDynArray;
  {$ENDIF}

  TPermissionStatusArrayHelper = record helper for TPermissionStatusArray
  public
    function AreAllDenied: Boolean;
    function AreAllGranted: Boolean;
    function IsGranted(const AIndex: Integer): Boolean;
  end;

implementation

{ TPermissionStatusArrayHelper }

function TPermissionStatusArrayHelper.AreAllDenied: Boolean;
var
  LStatus: TPermissionStatus;
begin
  Result := False;
  if Length(Self) > 0 then
  begin
    Result := True;
    for LStatus in Self do
    begin
      if LStatus <> TPermissionStatus.Denied then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TPermissionStatusArrayHelper.AreAllGranted: Boolean;
var
  LStatus: TPermissionStatus;
begin
  Result := True;
  for LStatus in Self do
  begin
    if LStatus <> TPermissionStatus.Granted then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TPermissionStatusArrayHelper.IsGranted(const AIndex: Integer): Boolean;
begin
  Result := (Length(Self) > AIndex) and (Self[AIndex] = TPermissionStatus.Granted);
end;

end.
