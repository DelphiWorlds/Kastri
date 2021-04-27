unit DW.OSDevice.Posix;

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
  // RTL
  System.Classes;

type
  TPosixOSDevice = record
  public
    class procedure GetEnvironmentVars(const AVars: TStrings); static;
  end;

implementation

uses
  // Posix
  Posix.Unistd, Posix.Dlfcn;

{ TPosixOSDevice }

class procedure TPosixOSDevice.GetEnvironmentVars(const AVars: TStrings);
var
  LEnv: PMarshaledAString;
  LVar: string;
begin
  AVars.BeginUpdate;
  try
    AVars.Clear;
    LEnv := environ;
    while LEnv^ <> nil do
    begin
      {$IF CompilerVersion > 33}
      LVar := string(PAnsiChar(LEnv^));
      {$ELSE}
      LVar := UTF8String(MarshaledAString(LEnv^));
      {$ENDIF}
      AVars.Add(LVar);
      Inc(LEnv);
    end;
  finally
    AVars.EndUpdate;
  end;
end;

end.
