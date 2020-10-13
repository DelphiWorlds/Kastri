unit DW.FilesSelector;

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
  TFilesSelector = class;

  TCustomPlatformFilesSelector = class(TObject)
  private
    FFileName: string;
    FFiles: TStrings;
    FFileTypes: TStrings;
    FSelector: TFilesSelector;
    FTitle: string;
  protected
    procedure DoComplete(const AOK: Boolean);
    procedure DoSelect; virtual; abstract;
    procedure Select;
    property FileName: string read FFileName write FFileName;
    property Files: TStrings read FFiles;
    property FileTypes: TStrings read FFileTypes;
    property Selector: TFilesSelector read FSelector;
    property Title: string read FTitle write FTitle;
  public
    constructor Create(const ASelector: TFilesSelector); virtual;
    destructor Destroy; override;
  end;

  TSelectorCompleteEvent = procedure(Sender: TObject; const OK: Boolean) of object;

  TFilesSelector = class(TObject)
  private
    FPlatformSelector: TCustomPlatformFilesSelector;
    FOnComplete: TSelectorCompleteEvent;
    function GetFileName: string;
    function GetFiles: TStrings;
    function GetFileTypes: TStrings;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  protected
    procedure DoComplete(const AOK: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Select;
    property FileName: string read GetFileName;
    property Files: TStrings read GetFiles;
    property FileTypes: TStrings read GetFileTypes;
    property Title: string read GetTitle write SetTitle;
    property OnComplete: TSelectorCompleteEvent read FOnComplete write FOnComplete;
  end;

implementation

uses
  // DW
  {$IF Defined(IOS)}
  DW.FilesSelector.iOS;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.FilesSelector.Android;
  {$ENDIF}

{ TCustomPlatformFilesSelector }

constructor TCustomPlatformFilesSelector.Create(const ASelector: TFilesSelector);
begin
  inherited Create;
  FFiles := TStringList.Create;
  FFileTypes := TStringList.Create;
  FSelector := ASelector;
end;

destructor TCustomPlatformFilesSelector.Destroy;
begin
  FFileTypes.Free;
  FFiles.Free;
  inherited;
end;

procedure TCustomPlatformFilesSelector.DoComplete(const AOK: Boolean);
begin
  if AOK and (FFiles.Count > 0) then
    FFileName := FFiles[0];
  FSelector.DoComplete(AOK);
end;

procedure TCustomPlatformFilesSelector.Select;
begin
  FFileName := '';
  FFiles.Clear;
  DoSelect;
end;

{ TFilesSelector }

constructor TFilesSelector.Create;
begin
  inherited;
  FPlatformSelector := TPlatformFilesSelector.Create(Self);
end;

destructor TFilesSelector.Destroy;
begin
  FPlatformSelector.Free;
  inherited;
end;

procedure TFilesSelector.DoComplete(const AOK: Boolean);
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self, AOK);
end;

function TFilesSelector.GetFileName: string;
begin
  Result := FPlatformSelector.FileName;
end;

function TFilesSelector.GetFiles: TStrings;
begin
  Result := FPlatformSelector.Files;
end;

function TFilesSelector.GetFileTypes: TStrings;
begin
  Result := FPlatformSelector.FileTypes;
end;

function TFilesSelector.GetTitle: string;
begin
  Result := FPlatformSelector.Title;
end;

procedure TFilesSelector.Select;
begin
  FPlatformSelector.Select;
end;

procedure TFilesSelector.SetTitle(const Value: string);
begin
  FPlatformSelector.Title := Value;
end;

end.
