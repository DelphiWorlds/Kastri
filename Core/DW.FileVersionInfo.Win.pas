unit DW.FileVersionInfo.Win;

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
  System.SysUtils,
  // Windows
  Winapi.Windows;

type
  TFileFlag = (Debug, InfoInferred, Patched, PreRelease, PrivateBuild, SpecialBuild);
  TFileFlags = set of TFileFlag;

  TVersionLanguage = (Arabic, Bulgarian, Catalan, TraditionalChinese, Czech, Danish, German, Greek, USEnglish, CastilianSpanish, Finnish, French,
    Hebrew, Hungarian, Icelandic, Italian, Japanese, Korean, Dutch, NorwegianBokmel, Polish, BrazilianPortuguese, RhaetoRomanic, Romanian, Russian,
    CroatoSerbian, Slovak, Albanian, Swedish, Thai, Turkish, Urdu, Bahasa, SimplifiedChinese, SwissGerman, UKEnglish, MexicanSpanish, BelgianFrench,
    SwissItalian, BelgianDutch, NorwegianNynorsk, Portuguese, SerboCroatian, CanadianFrench, SwissFrench, Unknown
  );

  TVersionCharSet = (ASCII, Japan, Korea, Taiwan, Unicode, EasternEuropean, Cyrillic, Multilingual, Greek, Turkish, Hebrew, Arabic, Unknown);

  TLongVersion = record
    case Integer of
      0:
        (All: array [1..4] of Word);
      1:
        (MS, LS: Longint);
  end;

  TFileVersionInfo = class(TObject)
  private
    FBuffer: Pointer;
    FFileName: TFileName;
    FIsValid: Boolean;
    function GetComments: string;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileFlags: TFileFlags;
    function GetFileVersion: string;
    function GetFileLongVersion: TLongVersion;
    function GetFixedFileInfo: PVSFixedFileInfo;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTrademarks: string;
    function GetOriginalFilename: string;
    function GetPrivateBuild: string;
    function GetProductLongVersion: TLongVersion;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetSpecialBuild: string;
    function GetTranslation: Pointer;
    function GetTranslationString: string;
    function GetVerFileDate: TDateTime;
    function GetVersionCharSet: TVersionCharSet;
    function GetVersionLanguage: TVersionLanguage;
    function GetVersionNum: Longint;
    procedure ReadVersionInfo;
    procedure SetFileName(const Value: TFileName);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function GetVerValue(const VerName: string): string;
    property Comments: string read GetComments;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileLongVersion: TLongVersion read GetFileLongVersion;
    property FileFlags: TFileFlags read GetFileFlags;
    property FileName: TFileName read FFileName write SetFileName;
    property FileVersion: string read GetFileVersion;
    property FixedFileInfo: PVSFixedFileInfo read GetFixedFileInfo;
    property InternalName: string read GetInternalName;
    property IsValid: Boolean read FIsValid;
    property LegalCopyright: string read GetLegalCopyright;
    property LegalTrademarks: string read GetLegalTrademarks;
    property OriginalFilename: string read GetOriginalFilename;
    property PrivateBuild: string read GetPrivateBuild;
    property ProductLongVersion: TLongVersion read GetProductLongVersion;
    property ProductName: string read GetProductName;
    property ProductVersion: string read GetProductVersion;
    property SpecialBuild: string read GetSpecialBuild;
    property Translation: Pointer read GetTranslation;
    property Values[const Name: string]: string read GetVerValue;
    property VerFileDate: TDateTime read GetVerFileDate;
    property VersionCharSet: TVersionCharSet read GetVersionCharSet;
    property VersionLanguage: TVersionLanguage read GetVersionLanguage;
    property VersionNum: Longint read GetVersionNum;
  end;

implementation

uses
  // RTL
  System.IOUtils;

const
  LanguageValues: array[TVersionLanguage] of Word = ($0401, $0402, $0403, $0404, $0405, $0406, $0407, $0408, $0409, $040A, $040B, $040C, $040D,
    $040E, $040F, $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F, $0420, $0421,
    $0804, $0807, $0809, $080A, $080C, $0810, $0813, $0814, $0816, $081A, $0C0C, $100C, $0000);

const
  CharacterSetValues: array [TVersionCharSet] of Integer = (0, 932, 949, 950, 1200, 1250, 1251, 1252, 1253, 1254, 1255, 1256, -1);

function LongVersionToString(const Version: TLongVersion): string;
begin
  Result := Format('%d.%d.%d.%d', [Version.All[2], Version.All[1], Version.All[4], Version.All[3]]);
end;

function StringToLongVersion(const Str: string): TLongVersion;
var
  LSep: Integer;
  LTmp, LFragment: string;
  I: Word;
begin
  LTmp := Str;
  for I := 1 to 4 do
  begin
    LSep := Pos('.', LTmp);
    if LSep = 0 then
      LSep := Pos(',', LTmp);
    if LSep = 0 then
      LFragment := LTmp
    else
    begin
      LFragment := Copy(LTmp, 1, LSep - 1);
      LTmp := Copy(LTmp, LSep + 1, MaxInt);
    end;
    if LFragment = '' then
      Result.All[I] := 0
    else
      Result.All[I] := StrToInt(LFragment);
  end;
  I := Result.All[1];
  Result.All[1] := Result.All[2];
  Result.All[2] := I;
  I := Result.All[3];
  Result.All[3] := Result.All[4];
  Result.All[4] := I;
end;

{ TFileVersionInfo }

constructor TFileVersionInfo.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FBuffer := nil;
  ReadVersionInfo;
end;

destructor TFileVersionInfo.Destroy;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TFileVersionInfo.ReadVersionInfo;
var
  LHandle, LSize: DWORD;
begin
  FIsValid := False;
  LSize := GetFileVersionInfoSize(PChar(FFileName), LHandle);
  if LSize > 0 then
  try
    GetMem(FBuffer, LSize);
    FIsValid := GetFileVersionInfo(PChar(FFileName), LHandle, LSize, FBuffer);
  except
    FIsValid := False;
    raise;
  end;
end;

procedure TFileVersionInfo.SetFileName(const Value: TFileName);
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
  FFileName := Value;
  ReadVersionInfo;
end;

function TFileVersionInfo.GetTranslation: Pointer;
var
  LLen: UINT;
begin
  if IsValid then
    VerQueryValue(FBuffer, '\VarFileInfo\Translation', Result, LLen)
  else
    Result := nil;
end;

function TFileVersionInfo.GetTranslationString: string;
var
  LPtr: Pointer;
begin
  LPtr := GetTranslation;
  if LPtr <> nil then
    Result := IntToHex(MakeLong(HiWord(Longint(LPtr^)), LoWord(Longint(LPtr^))), 8)
  else
    Result := '';
end;

function TFileVersionInfo.GetVersionLanguage: TVersionLanguage;
var
  LPtr: Pointer;
begin
  Result := TVersionLanguage.Unknown;
  LPtr := GetTranslation;
  if LPtr <> nil then
  begin
    for Result := TVersionLanguage.Arabic to TVersionLanguage.Unknown do
      if LoWord(Longint(LPtr^)) = LanguageValues[Result] then
        Break;
  end;
end;

function TFileVersionInfo.GetVersionCharSet: TVersionCharSet;
var
  LPtr: Pointer;
begin
  Result := TVersionCharSet.Unknown;
  LPtr := GetTranslation;
  if LPtr <> nil then
  begin
    for Result := TVersionCharSet.ASCII to TVersionCharSet.Unknown do
      if HiWord(Longint(LPtr^)) = CharacterSetValues[Result] then
       Break;
  end;
end;

function TFileVersionInfo.GetFixedFileInfo: PVSFixedFileInfo;
var
  LLen: UINT;
begin
  if IsValid then
    VerQueryValue(FBuffer, '\', Pointer(Result), LLen)
  else
    Result := nil;
end;

function TFileVersionInfo.GetProductLongVersion: TLongVersion;
begin
  if IsValid then
  begin
    Result.MS := FixedFileInfo^.dwProductVersionMS;
    Result.LS := FixedFileInfo^.dwProductVersionLS;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TFileVersionInfo.GetFileLongVersion: TLongVersion;
begin
  if IsValid then
  begin
    Result.MS := FixedFileInfo^.dwFileVersionMS;
    Result.LS := FixedFileInfo^.dwFileVersionLS;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TFileVersionInfo.GetVersionNum: Longint;
begin
  if IsValid then
    Result := FixedFileInfo^.dwFileVersionMS
  else
    Result := 0;
end;

function TFileVersionInfo.GetVerValue(const VerName: string): string;
var
  LName: array [0..255] of Char;
  LValue: Pointer;
  LLen: UINT;
begin
  Result := '';
  if IsValid then
  begin
    StrPCopy(LName, '\StringFileInfo\' + GetTranslationString + '\' + VerName);
    if VerQueryValue(FBuffer, LName, LValue, LLen) then
      Result := PChar(LValue);
  end;
end;

function TFileVersionInfo.GetComments: string;
begin
  Result := GetVerValue('Comments');
end;

function TFileVersionInfo.GetCompanyName: string;
begin
  Result := GetVerValue('CompanyName');
end;

function TFileVersionInfo.GetFileDescription: string;
begin
  Result := GetVerValue('FileDescription');
end;

function TFileVersionInfo.GetFileFlags: TFileFlags;
var
  LMasked: DWORD;
  LFixedInfo: PVSFixedFileInfo;
begin
  Result := [];
  LFixedInfo := GetFixedFileInfo;
  LMasked := LFixedInfo^.dwFileFlags and LFixedInfo^.dwFileFlagsMask;
  if (LMasked and VS_FF_DEBUG) <> 0 then
    Include(Result, TFileFlag.Debug);
  if (LMasked and VS_FF_INFOINFERRED) <> 0 then
    Include(Result, TFileFlag.InfoInferred);
  if (LMasked and VS_FF_PATCHED) <> 0 then
    Include(Result, TFileFlag.Patched);
  if (LMasked and VS_FF_PRERELEASE) <> 0 then
    Include(Result, TFileFlag.PreRelease);
  if (LMasked and VS_FF_PRIVATEBUILD) <> 0 then
    Include(Result, TFileFlag.PrivateBuild);
  if (LMasked and VS_FF_SPECIALBUILD) <> 0 then
    Include(Result, TFileFlag.SpecialBuild);
end;

function TFileVersionInfo.GetFileVersion: string;
begin
  Result := GetVerValue('FileVersion');
  if (Result = '') and IsValid then
    Result := LongVersionToString(FileLongVersion);
end;

function TFileVersionInfo.GetInternalName: string;
begin
  Result := GetVerValue('InternalName');
end;

function TFileVersionInfo.GetLegalCopyright: string;
begin
  Result := GetVerValue('LegalCopyright');
end;

function TFileVersionInfo.GetLegalTrademarks: string;
begin
  Result := GetVerValue('LegalTrademarks');
end;

function TFileVersionInfo.GetOriginalFilename: string;
begin
  Result := GetVerValue('OriginalFilename');
end;

function TFileVersionInfo.GetProductVersion: string;
begin
  Result := GetVerValue('ProductVersion');
  if (Result = '') and IsValid then
    Result := LongVersionToString(ProductLongVersion);
end;

function TFileVersionInfo.GetProductName: string;
begin
  Result := GetVerValue('ProductName');
end;

function TFileVersionInfo.GetSpecialBuild: string;
begin
  Result := GetVerValue('SpecialBuild');
end;

function TFileVersionInfo.GetPrivateBuild: string;
begin
  Result := GetVerValue('PrivateBuild');
end;

function TFileVersionInfo.GetVerFileDate: TDateTime;
begin
  if FileExists(FileName) then
    Result := TFile.GetCreationTime(FileName)
  else
    Result := 0;
end;

end.
