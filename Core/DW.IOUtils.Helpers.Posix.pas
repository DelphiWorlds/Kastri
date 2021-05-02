unit DW.IOUtils.Helpers.Posix;

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

type
  TPlatformFile = record
    class function GetFileSize(const AFilename: string): Int64; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Posix
  Posix.SysStat;

{ TPlatformFile }

class function TPlatformFile.GetFileSize(const AFilename: string): Int64;
var
  LStat: _stat;
  LFilename: Pointer;
  LMarshaller: TMarshaller;
  LError: Integer;
begin
  LFilename := LMarshaller.AsAnsi(AFilename, CP_UTF8).ToPointer;
  LError := stat(LFilename, LStat);
  if LError = 0 then
    Result := LStat.st_size
  else
    raise EInOutError.Create(SysErrorMessage(LError));
end;

end.
