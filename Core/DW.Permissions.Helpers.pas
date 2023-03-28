unit DW.Permissions.Helpers;

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
    function AreAllGranted: Boolean;
  end;

implementation

{ TPermissionStatusArrayHelper }

function TPermissionStatusArrayHelper.AreAllGranted: Boolean;
var
  LStatus: TPermissionStatus;
begin
  for LStatus in Self do
  begin
    if LStatus <> TPermissionStatus.Granted then
      Exit(False); // <======
  end;
  Result := True;
end;

end.
