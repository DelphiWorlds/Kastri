unit DW.Permissions.Helpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Permissions;

type
  TGrantResults = TArray<TPermissionStatus>;

  TGrantResultsHelper = record helper for TGrantResults
  public
    function AreAllGranted: Boolean;
  end;

implementation

{ TGrantResultsHelper }

function TGrantResultsHelper.AreAllGranted: Boolean;
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
