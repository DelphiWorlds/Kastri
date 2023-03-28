unit DW.FilesSelector;

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
  TSelectionMode = (Documents, Content);

  TFilesSelector = class;

  TSelectedFile = record
    RawPath: string;
    DecodedPath: string;
    DisplayName: string;
  end;

  TSelectedFiles = TArray<TSelectedFile>;

  TFileKind = (Image, Audio, Movie, Text, Item, Content, SourceCode, PDF, X509Certificate, Key);

  TFileKinds = set of TFileKind;

  TCustomPlatformFilesSelector = class(TObject)
  private
    FActivities: TStrings;
    FFileName: string;
    FSelectedFiles: TSelectedFiles;
    FSelector: TFilesSelector;
    FTitle: string;
    procedure FileTypesChangeHandler(Sender: TObject);
    procedure SetFileKinds(const Value: TFileKinds);
  protected
    FFileKinds: TFileKinds;
    FFileTypes: TStrings;
    procedure AddSelectedFile(const ASelectedFile: TSelectedFile);
    procedure DoComplete(const AOK: Boolean);
    procedure DoSelect(const AMode: TSelectionMode); virtual; abstract;
    procedure FileKindsChanged; virtual;
    procedure FileTypesChanged; virtual;
    procedure Select(const AMode: TSelectionMode);
    property Activities: TStrings read FActivities;
    property FileKinds: TFileKinds read FFileKinds write SetFileKinds;
    property FileName: string read FFileName write FFileName;
    property FileTypes: TStrings read FFileTypes;
    property SelectedFiles: TSelectedFiles read FSelectedFiles;
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
    function GetActivities: TStrings;
    function GetFileKinds: TFileKinds;
    function GetFileTypes: TStrings;
    function GetSelectedFiles: TSelectedFiles;
    function GetTitle: string;
    procedure SetFileKinds(const Value: TFileKinds);
    procedure SetTitle(const Value: string);
  protected
    procedure DoComplete(const AOK: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Presents the file selector
    /// </summary>
    procedure Select(const AMode: TSelectionMode = TSelectionMode.Documents);
    /// <summary>
    ///   List of activities that can handle the file selection based on FileKinds/FileTypes - ANDROID ONLY
    /// </summary>
    property Activities: TStrings read GetActivities;
    /// <summary>
    ///   Set of TFileKind to indicate which kinds of files should be selectable.
    ///   An empty set implies all file types unless the FileTypes property is not empty
    /// </summary>
    /// <remarks>
    ///   NOTE: Altering this property will clear the FileTypes property
    /// </remarks>
    property FileKinds: TFileKinds read GetFileKinds write SetFileKinds;
    /// <summary>
    ///   Allows the use of specific string values to indicate which kinds of files should be selectable.
    ///   The values *must* be valid for the platform which is being targeted, i.e. iOS needs UTIs, Android needs mime types
    ///   An empty list implies all file types unless the FileKinds property is not empty
    /// </summary>
    /// <remarks>
    ///   NOTE: Altering this property will clear the FileKinds property
    /// </remarks>
    property FileTypes: TStrings read GetFileTypes;
    /// <summary>
    ///   List of files that were selected
    /// </summary>
    property SelectedFiles: TSelectedFiles read GetSelectedFiles;
    /// <summary>
    ///   Title for the file selector
    /// </summary>
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
  FActivities := TStringList.Create;
  FFileTypes := TStringList.Create;
  TStringList(FFileTypes).OnChange := FileTypesChangeHandler;
  FSelector := ASelector;
end;

destructor TCustomPlatformFilesSelector.Destroy;
begin
  FFileTypes.Free;
  FActivities.Free;
  inherited;
end;

procedure TCustomPlatformFilesSelector.DoComplete(const AOK: Boolean);
begin
  FSelector.DoComplete(AOK);
end;

procedure TCustomPlatformFilesSelector.FileKindsChanged;
begin
  //
end;

procedure TCustomPlatformFilesSelector.FileTypesChanged;
begin
  //
end;

procedure TCustomPlatformFilesSelector.FileTypesChangeHandler(Sender: TObject);
begin
  FileTypesChanged;
end;

procedure TCustomPlatformFilesSelector.Select(const AMode: TSelectionMode);
begin
  FFileName := '';
  FSelectedFiles := [];
  DoSelect(AMode);
end;

procedure TCustomPlatformFilesSelector.SetFileKinds(const Value: TFileKinds);
begin
  if FFileKinds <> Value then
  begin
    FFileKinds := Value;
    FFileTypes.Clear;
    FileKindsChanged;
  end;
end;

procedure TCustomPlatformFilesSelector.AddSelectedFile(const ASelectedFile: TSelectedFile);
begin
  FSelectedFiles := FSelectedFiles + [ASelectedFile];
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

function TFilesSelector.GetActivities: TStrings;
begin
  Result := FPlatformSelector.Activities;
end;

function TFilesSelector.GetFileKinds: TFileKinds;
begin
  Result := FPlatformSelector.FileKinds;
end;

function TFilesSelector.GetFileTypes: TStrings;
begin
  Result := FPlatformSelector.FileTypes;
end;

function TFilesSelector.GetSelectedFiles: TSelectedFiles;
begin
  Result := FPlatformSelector.SelectedFiles;
end;

function TFilesSelector.GetTitle: string;
begin
  Result := FPlatformSelector.Title;
end;

procedure TFilesSelector.Select(const AMode: TSelectionMode = TSelectionMode.Documents);
begin
  FPlatformSelector.Select(AMode);
end;

procedure TFilesSelector.SetFileKinds(const Value: TFileKinds);
begin
  FPlatformSelector.FileKinds := Value;
end;

procedure TFilesSelector.SetTitle(const Value: string);
begin
  FPlatformSelector.Title := Value;
end;

end.
