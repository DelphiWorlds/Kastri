unit DW.FileWriter;

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
  /// <summary>
  ///   Specialised TStreamWriter that writes to a file
  /// </summary>
  /// <remarks>
  ///   Set the AutoFlush property (of TStreamWriter) to True for immediate writes
  /// </remarks>
  TFileWriter = class(TStreamWriter)
  private
    FStream: TStream;
  public
    constructor Create(const Filename: string; Append: Boolean = False); overload; virtual;
    destructor Destroy; override;
  end;

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

constructor TFileWriter.Create(const Filename: string; Append: Boolean);
var
  LShareMode: Word;
begin
  if TOSVersion.Platform <> TOSVersion.TPlatform.pfiOS then
    LShareMode := fmShareDenyWrite
  else
    LShareMode := 0;
  if (not TFile.Exists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate or LShareMode)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite or LShareMode);
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
