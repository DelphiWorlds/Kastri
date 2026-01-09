unit DW.AppUpdate;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

type
  TAppUpdateInfoError = (Success, JavaScript, VersionNotFound, ScriptNotFound);

  TAppUpdateInfo = record
    Available: Boolean;
    AvailableVersionCode: Integer;
    AvailableVersionString: string;
    Error: TAppUpdateInfoError;
    Flexible: Boolean;
    Immediate: Boolean;
    Priority: Integer;
    StalenessDays: Integer;
    TotalBytesToDownload: Int64;
    procedure Reset;
  end;

  TAppUpdateType = (Immediate, Flexible);
  TAppUpdateResult = (Succeeded, Canceled, Failed);

  TAppUpdate = class;

  TCustomPlatformAppUpdate = class(TObject)
  private
    FAppUpdate: TAppUpdate;
    FUpdateURL: string;
  protected
    procedure CheckForUpdate; virtual;
    procedure DoAppUpdateInfo(const AInfo: TAppUpdateInfo);
    procedure DoStartedUpdateFlow(const AStarted: Boolean); virtual;
    procedure DoAppUpdateResult(const AUpdateResult: TAppUpdateResult); virtual;
    procedure StartUpdate(const AUpdateType: TAppUpdateType); virtual;
    property UpdateURL: string read FUpdateURL write FUpdateURL;
  public
    constructor Create(const AAppUpdate: TAppUpdate); virtual;
  end;

  TAppUpdateInfoEvent = procedure(Sender: TObject; const Info: TAppUpdateInfo) of object;
  TAppUpdateStartedFlowEvent = procedure(Sender: TObject; const Started: Boolean) of object;
  TAppUpdateResultEvent = procedure(Sender: TObject; const UpdateResult: TAppUpdateResult) of object;

  TAppUpdate = class(TObject)
  private
    FPlatformAppUpdate: TCustomPlatformAppUpdate;
    FOnAppUpdateInfo: TAppUpdateInfoEvent;
    FOnAppUpdateStartedFlow: TAppUpdateStartedFlowEvent;
    FOnAppUpdateResult: TAppUpdateResultEvent;
    function GetUpdateURL: string;
    procedure SetUpdateURL(const Value: string);
  protected
    procedure DoAppUpdateInfo(const AInfo: TAppUpdateInfo);
    procedure DoStartedUpdateFlow(const AStarted: Boolean); virtual;
    procedure DoAppUpdateResult(const AUpdateResult: TAppUpdateResult); virtual;
  public
    constructor Create;
    procedure CheckForUpdate; virtual;
    procedure StartUpdate(const AUpdateType: TAppUpdateType); virtual;
    property UpdateURL: string read GetUpdateURL write SetUpdateURL;
    property OnAppUpdateInfo: TAppUpdateInfoEvent read FOnAppUpdateInfo write FOnAppUpdateInfo;
    property OnAppUpdateStartedFlow: TAppUpdateStartedFlowEvent read FOnAppUpdateStartedFlow write FOnAppUpdateStartedFlow;
    property OnAppUpdateResult: TAppUpdateResultEvent read FOnAppUpdateResult write FOnAppUpdateResult;
  end;

implementation

{$IF Defined(ANDROID)}
uses
  DW.AppUpdate.Android;
{$ELSE}

type
  TPlatformAppUpdate = class(TCustomPlatformAppUpdate);
{$ENDIF}

{ TAppUpdateInfo }

procedure TAppUpdateInfo.Reset;
begin
  Available := False;
  Flexible := False;
  Immediate := False;
  Priority := -1;
  StalenessDays := 0;
  TotalBytesToDownload := 0;
end;

{ TCustomPlatformAppUpdate }

constructor TCustomPlatformAppUpdate.Create(const AAppUpdate: TAppUpdate);
begin
  inherited Create;
  FAppUpdate := AAppUpdate;
end;

procedure TCustomPlatformAppUpdate.CheckForUpdate;
begin
  //
end;

procedure TCustomPlatformAppUpdate.DoAppUpdateInfo(const AInfo: TAppUpdateInfo);
begin
  FAppUpdate.DoAppUpdateInfo(AInfo);
end;

procedure TCustomPlatformAppUpdate.DoAppUpdateResult(const AUpdateResult: TAppUpdateResult);
begin
  FAppUpdate.DoAppUpdateResult(AUpdateResult);
end;

procedure TCustomPlatformAppUpdate.DoStartedUpdateFlow(const AStarted: Boolean);
begin
  FAppUpdate.DoStartedUpdateFlow(AStarted);
end;

procedure TCustomPlatformAppUpdate.StartUpdate(const AUpdateType: TAppUpdateType);
begin
  //
end;

{ TAppUpdate }

constructor TAppUpdate.Create;
begin
  inherited;
  FPlatformAppUpdate := TPlatformAppUpdate.Create(Self);
end;

procedure TAppUpdate.CheckForUpdate;
begin
  FPlatformAppUpdate.CheckForUpdate;
end;

procedure TAppUpdate.DoAppUpdateInfo(const AInfo: TAppUpdateInfo);
begin
  if Assigned(FOnAppUpdateInfo) then
    FOnAppUpdateInfo(Self, AInfo);
end;

procedure TAppUpdate.DoAppUpdateResult(const AUpdateResult: TAppUpdateResult);
begin
  if Assigned(FOnAppUpdateResult) then
    FOnAppUpdateResult(Self, AUpdateResult);
end;

procedure TAppUpdate.DoStartedUpdateFlow(const AStarted: Boolean);
begin
  if Assigned(FOnAppUpdateStartedFlow) then
    FOnAppUpdateStartedFlow(Self, AStarted);
end;

function TAppUpdate.GetUpdateURL: string;
begin
  Result := FPlatformAppUpdate.UpdateURL;
end;

procedure TAppUpdate.SetUpdateURL(const Value: string);
begin
  FPlatformAppUpdate.UpdateURL := Value;
end;

procedure TAppUpdate.StartUpdate(const AUpdateType: TAppUpdateType);
begin
  FPlatformAppUpdate.StartUpdate(AUpdateType);
end;

end.
