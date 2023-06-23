unit DW.FileWriter;

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
  /// <summary>
  ///   Specialised TStreamWriter that writes to a file
  /// </summary>
  /// <remarks>
  ///   Set the AutoFlush property (of TStreamWriter) to True for immediate writes
  /// </remarks>
  TFileWriter = class(TStreamWriter)
  private
    FFileName: string;
    FStream: TStream;
  public
    constructor Create(const AFilename: string; AAppend: Boolean); overload; virtual;
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  /// <summary>
  ///   Adds/overrides methods specifically to help with writing log files
  /// </summary>
  TLogWriter = class(TFileWriter)
  private
    FTimestampFormat: string;
    function GetTimestamp: string;
  public
    constructor Create(const AFilename: string; const ATimestampFormat: string = 'mm-dd hh:nn:ss.zzz');
    procedure WriteLine(const Value: string); override;
    procedure WriteLine(const Format: string; Args: array of const); override;
    property TimestampFormat: string read FTimestampFormat write FTimestampFormat;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils;

{ TFileWriter }

constructor TFileWriter.Create(const AFilename: string; AAppend: Boolean);
var
  LShareMode: Word;
begin
  FFilename := AFileName;
  if TOSVersion.Platform <> TOSVersion.TPlatform.pfiOS then
    LShareMode := fmShareDenyWrite
  else
    LShareMode := 0;
  if not TFile.Exists(FFileName) or not AAppend then
    FStream := TFileStream.Create(FFileName, fmCreate or LShareMode)
  else
  begin
    FStream := TFileStream.Create(FFileName, fmOpenWrite or LShareMode);
    FStream.Seek(0, soEnd);
  end;
  inherited Create(FStream);
end;

destructor TFileWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ TLogWriter }

constructor TLogWriter.Create(const AFilename: string; const ATimestampFormat: string = 'mm-dd hh:nn:ss.zzz');
begin
  inherited Create(AFilename, True);
  AutoFlush := True;
  FTimestampFormat := ATimestampFormat;
end;

function TLogWriter.GetTimestamp: string;
begin
  Result := '';
  if not FTimestampFormat.IsEmpty then
  begin
    Result := FormatDateTime(FTimestampFormat, Now) + ': ';
  end;
end;

procedure TLogWriter.WriteLine(const Format: string; Args: array of const);
begin
  inherited WriteLine(GetTimestamp + Format, Args);
end;

procedure TLogWriter.WriteLine(const Value: string);
begin
  inherited WriteLine(GetTimestamp + Value);
end;

end.
