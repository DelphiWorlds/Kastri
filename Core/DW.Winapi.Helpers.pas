unit DW.Winapi.Helpers;

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
  System.Classes;

type
  TWinapiHelper = record
  public
    class function ExpandEnvironmentVar(var AValue: string): Boolean; static;
    class function ExpandPath(const ABaseDir, ARelativeDir: string): string; static;
    /// <summary>
    ///  Expands the variables contained in ASource
    /// </summary>
    class function ExpandVars(const ASource: string): string; static;
    class function GetEnvironmentVars(const AVars: TStrings; AExpand: Boolean): Boolean; static;
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

class function TWinapiHelper.ExpandEnvironmentVar(var AValue: string): Boolean;
var
  LLength: Integer;
  LExpanded: string;
begin
  SetLength(LExpanded, 1);
  LLength := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), 0);
  SetLength(LExpanded, LLength);
  Result := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), LLength) <> 0;
  if Result then
  begin
    StrResetLength(LExpanded);
    AValue := LExpanded;
  end;
end;

class function TWinapiHelper.GetEnvironmentVars(const AVars: TStrings; AExpand: Boolean): Boolean;
var
  LRaw: PChar;
  LExpanded: string;
  I: Integer;
begin
  AVars.BeginUpdate;
  try
    AVars.Clear;
    LRaw := GetEnvironmentStrings;
    try
      MultiSzToStrings(AVars, LRaw);
      Result := True;
    finally
      FreeEnvironmentStrings(LRaw);
    end;
    if AExpand then
    begin
      for I := 0 to AVars.Count - 1 do
      begin
        LExpanded := AVars[I];
        if ExpandEnvironmentVar(LExpanded) then
          AVars[I] := LExpanded;
      end;
    end;
  finally
    AVars.EndUpdate;
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
