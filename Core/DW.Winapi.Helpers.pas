unit DW.Winapi.Helpers;

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
  // RTL
  System.Classes;

type
  TWinapiHelper = record
  public
    class function ExpandEnvironmentVar(var Value: string): Boolean; static;
    class function ExpandPath(const ABaseDir, ARelativeDir: string): string; static;
    /// <summary>
    ///  Expands the variables contained in ASource
    /// </summary>
    class function ExpandVars(const ASource: string): string; static;
    class function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // Windows
  Winapi.Windows, Winapi.ShLwApi;

// "Borrowed" a couple of methods from JVCL for processing of environment variables
procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    if Source <> nil then
    begin
      P := Source;
      while P^ <> #0 do
      begin
        Dest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    Dest.EndUpdate;
  end;
end;

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

{ TWinapiHelper }

// Tweaked version of David Heffernan's answer, here:
//   https://stackoverflow.com/questions/5329472/conversion-between-absolute-and-relative-paths-in-delphi
class function TWinapiHelper.ExpandPath(const ABaseDir, ARelativeDir: string): string;
var
  LBuffer: array [0..MAX_PATH - 1] of Char;
begin
  if PathIsRelative(PChar(ARelativeDir)) then
    Result := IncludeTrailingPathDelimiter(ABaseDir) + ARelativeDir
  else
    Result := ARelativeDir;
  if PathCanonicalize(@LBuffer[0], PChar(Result)) then
    Result := LBuffer;
end;

class function TWinapiHelper.ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;

class function TWinapiHelper.GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    Raw := GetEnvironmentStrings;
    try
      MultiSzToStrings(Vars, Raw);
      Result := True;
    finally
      FreeEnvironmentStrings(Raw);
    end;
    if Expand then
    begin
      for I := 0 to Vars.Count - 1 do
      begin
        Expanded := Vars[I];
        if ExpandEnvironmentVar(Expanded) then
          Vars[I] := Expanded;
      end;
    end;
  finally
    Vars.EndUpdate;
  end;
end;

class function TWinapiHelper.ExpandVars(const ASource: string): string;
var
  LVars: TStrings;
  I: Integer;
begin
  Result := ASource;
  LVars := TStringList.Create;
  try
    GetEnvironmentVars(LVars, True);
    for I := 0 to LVars.Count - 1 do
      Result := StringReplace(Result, '$(' + LVars.Names[i] + ')', LVars.Values[LVars.Names[i]], [rfReplaceAll, rfIgnoreCase]);
  finally
    LVars.Free;
  end;
end;

end.
