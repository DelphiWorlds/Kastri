unit DW.AssetDelivery.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}


// Based in part on:
//   https://medium.com/mindful-engineering/google-play-asset-delivery-in-android-dcc2059e5a63

interface

{$SCOPEDENUMS ON}

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.Jni.GraphicsContentViewText, Androidapi.JNI,
  // DW
  DW.Androidapi.JNI.PlayCore;

type
  TAssetDelivery = class;

  TAssetPackStateUpdateListener = class(TJavaLocal, JAssetPackStateUpdateListener)
  private
    FAssetDelivery: TAssetDelivery;
  public
    { JStateUpdatedListener }
    procedure onStateUpdate(state: JAssetPackState); cdecl;
  public
    constructor Create(const AAssetDelivery: TAssetDelivery);
  end;

  TAssetPackStateCompleteListener = class(TJavaLocal, JOnCompleteListener)
  private
    FAssetDelivery: TAssetDelivery;
  public
    { JOnCompleteListener }
    procedure onComplete(task: JTask); cdecl;
  public
    constructor Create(const AAssetDelivery: TAssetDelivery);
  end;

  TAssetPackStatus = (Canceled, Completed, Downloading, Failed, NotInstalled, Pending, Transferring, Unknown, WaitingForWifi);

  TAssetPackState = record
    BytesDownloaded: Int64;
    ErrorCode: Integer;
    Name: string;
    Status: TAssetPackStatus;
    TotalBytesToDownload: Int64;
    TransferProgressPercentage: Integer;
    function IsFinished: Boolean;
    function StatusDisplayValue: string;
  end;

  TAssetPackStates = TArray<TAssetPackState>;

  TAssetPackAction = (None, Fetch, Query);
  TAssetPackStateEvent = procedure(Sender: TObject; const AssetPackState: TAssetPackState; const Action: TAssetPackAction) of object;

  TAssetDelivery = class(TObject)
  private
    class function GetAssetPackState(const AAssetPackState: JAssetPackState): TAssetPackState;
    class function GetAssetPackStatus(const AStatus: Integer): TAssetPackStatus;
  private
    FAssetManager: JAssetManager;
    FAssetPackAction: TAssetPackAction;
    FAssetPackManager: JAssetPackManager;
    FStateUpdateListener: JAssetPackStateUpdateListener;
    FStatesCompleteListener: JOnCompleteListener;
    FOnAssetPackStateUpdate: TAssetPackStateEvent;
    function CheckPack(const APackName: string): Boolean;
    procedure DoAssetPackStateUpdate(const AState: TAssetPackState);
  protected
    procedure AssetPackStateUpdate(const AAssetPackState: JAssetPackState);
    procedure AssetPackStatesComplete(const AAssetPackStates: JAssetPackStates);
  public
    constructor Create;
    destructor Destroy; override;
    function Fetch(const APackNames: TArray<string>): Integer;
    function Query(const APackNames: TArray<string>): Integer;
    function GetAssetFilePath(const APackName, AAssetPath: string): string;
    function GetAssets(const APath: string): TArray<string>;
    function GetAssetPackPath(const APackName: string): string;
    property OnAssetPackStateUpdate: TAssetPackStateEvent read FOnAssetPackStateUpdate write FOnAssetPackStateUpdate;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils,
  // Android
  Androidapi.Helpers;

const
  cAssetPackStatusDisplayValues: array[TAssetPackStatus] of string = (
    'Canceled', 'Completed', 'Downloading', 'Failed', 'Not Installed', 'Pending', 'Transferring', 'Unknown', 'Waiting For Wifi'
  );

function StringArrayToJList(const AValues: TArray<string>): JList;
var
  LValue: string;
  LArrayList: JArrayList;
begin
  LArrayList := TJArrayList.Create;
  for LValue in AValues do
    LArrayList.add(StringToJString(LValue));
  Result := TJList.Wrap(LArrayList);
end;

{ TAssetPackState }

function TAssetPackState.IsFinished: Boolean;
begin
  Result := Status in [TAssetPackStatus.Canceled, TAssetPackStatus.Completed, TAssetPackStatus.Failed, TAssetPackStatus.NotInstalled];
end;

function TAssetPackState.StatusDisplayValue: string;
begin
  Result := cAssetPackStatusDisplayValues[Status];
end;

{ TAssetPackStateUpdateListener }

constructor TAssetPackStateUpdateListener.Create(const AAssetDelivery: TAssetDelivery);
begin
  inherited Create;
  FAssetDelivery := AAssetDelivery;
end;

procedure TAssetPackStateUpdateListener.onStateUpdate(state: JAssetPackState);
begin
  FAssetDelivery.AssetPackStateUpdate(state);
end;

{ TAssetPackStateCompleteListener }

constructor TAssetPackStateCompleteListener.Create(const AAssetDelivery: TAssetDelivery);
begin
  inherited Create;
  FAssetDelivery := AAssetDelivery;
end;

procedure TAssetPackStateCompleteListener.onComplete(task: JTask);
begin
  FAssetDelivery.AssetPackStatesComplete(TJAssetPackStates.Wrap(task.getResult));
end;

{ TAssetDelivery }

constructor TAssetDelivery.Create;
begin
  inherited;
  FStateUpdateListener := TAssetPackStateUpdateListener.Create(Self);
  FStatesCompleteListener := TAssetPackStateCompleteListener.Create(Self);
  FAssetPackManager := TJAssetPackManagerFactory.JavaClass.getInstance(TAndroidHelper.Context);
  FAssetPackManager.registerListener(FStateUpdateListener);
  FAssetManager := TAndroidHelper.Context.getAssets;
end;

destructor TAssetDelivery.Destroy;
begin
  FAssetPackManager.unregisterListener(FStateUpdateListener);
  inherited;
end;

procedure TAssetDelivery.AssetPackStatesComplete(const AAssetPackStates: JAssetPackStates);
var
  LIterator: JIterator;
  LKeyObject: JObject;
  LValueObject: JObject;
  LKey: JString;
begin
  LIterator := AAssetPackStates.packStates.entrySet.iterator;
  while LIterator.hasNext do
  begin
    LKeyObject := LIterator.next;
    if LKeyObject <> nil then
    begin
      LKey := LKeyObject.toString;
      LKey := LKey.substring(0, LKey.indexOf(StringToJString('=')));
      LValueObject := AAssetPackStates.packStates.&get(LKey);
      if LValueObject <> nil then
        AssetPackStateUpdate(TJAssetPackState.Wrap(LValueObject));
    end;
  end;
end;

procedure TAssetDelivery.AssetPackStateUpdate(const AAssetPackState: JAssetPackState);
begin
  DoAssetPackStateUpdate(GetAssetPackState(AAssetPackState));
end;

procedure TAssetDelivery.DoAssetPackStateUpdate(const AState: TAssetPackState);
begin
  if Assigned(FOnAssetPackStateUpdate) then
    FOnAssetPackStateUpdate(Self, AState, FAssetPackAction);
end;

function TAssetDelivery.Fetch(const APackNames: TArray<string>): Integer;
var
  LPacks: TArray<string>;
  LPack: string;
begin
  FAssetPackAction := TAssetPackAction.Fetch;
  LPacks := [];
  for LPack in APackNames do
  begin
    if not CheckPack(LPack) then
      LPacks := LPacks + [LPack];
  end;
  Result := Length(LPacks);
  if Result > 0 then
    FAssetPackManager.fetch(StringArrayToJList(LPacks));
end;

function TAssetDelivery.Query(const APackNames: TArray<string>): Integer;
var
  LPacks: TArray<string>;
  LPack: string;
begin
  FAssetPackAction := TAssetPackAction.Query;
  LPacks := [];
  for LPack in APackNames do
  begin
    if not CheckPack(LPack) then
      LPacks := LPacks + [LPack];
  end;
  Result := Length(LPacks);
  if Result > 0 then
    FAssetPackManager.getPackStates(StringArrayToJList(LPacks)).addOnCompleteListener(FStatesCompleteListener);
end;

function TAssetDelivery.CheckPack(const APackName: string): Boolean;
var
  LPackLocation: JAssetPackLocation;
  LState: TAssetPackState;
begin
  Result := False;
  LPackLocation := FAssetPackManager.getPackLocation(StringToJString(APackName));
  if LPackLocation <> nil then
  begin
    LState.Status := TAssetPackStatus.Completed;
    LState.Name := APackName;
    try
      DoAssetPackStateUpdate(LState);
    finally
      Result := True;
    end;
  end;
end;

function TAssetDelivery.GetAssetFilePath(const APackName, AAssetPath: string): string;
var
  LPackLocation: JAssetPackLocation;
begin
  Result := '';
  LPackLocation := FAssetPackManager.getPackLocation(StringToJString(APackName));
  if LPackLocation <> nil then
    Result := TPath.Combine(JStringToString(LPackLocation.assetsPath), AAssetPath);
end;

function TAssetDelivery.GetAssetPackPath(const APackName: string): string;
var
  LPackLocation: JAssetPackLocation;
begin
  Result := '';
  LPackLocation := FAssetPackManager.getPackLocation(StringToJString(APackName));
  if LPackLocation <> nil then
    Result := JStringToString(LPackLocation.assetsPath);
end;

class function TAssetDelivery.GetAssetPackState(const AAssetPackState: JAssetPackState): TAssetPackState;
begin
  Result.BytesDownloaded := AAssetPackState.bytesDownloaded;
  Result.ErrorCode := AAssetPackState.errorCode;
  Result.Name := JStringToString(AAssetPackState.name);
  Result.Status := GetAssetPackStatus(AAssetPackState.status);
  Result.TotalBytesToDownload := AAssetPackState.totalBytesToDownload;
  Result.TransferProgressPercentage := AAssetPackState.TransferProgressPercentage;
end;

class function TAssetDelivery.GetAssetPackStatus(const AStatus: Integer): TAssetPackStatus;
begin
  if AStatus = TJAssetPackStatus.JavaClass.CANCELED then
    Result := TAssetPackStatus.Canceled
  else if AStatus = TJAssetPackStatus.JavaClass.COMPLETED then
    Result := TAssetPackStatus.Completed
  else if AStatus = TJAssetPackStatus.JavaClass.DOWNLOADING then
    Result := TAssetPackStatus.Downloading
  else if AStatus = TJAssetPackStatus.JavaClass.FAILED then
    Result := TAssetPackStatus.Failed
  else if AStatus = TJAssetPackStatus.JavaClass.NOT_INSTALLED then
    Result := TAssetPackStatus.NotInstalled
  else if AStatus = TJAssetPackStatus.JavaClass.PENDING then
    Result := TAssetPackStatus.Pending
  else if AStatus = TJAssetPackStatus.JavaClass.TRANSFERRING then
    Result := TAssetPackStatus.Transferring
  else if AStatus = TJAssetPackStatus.JavaClass.WAITING_FOR_WIFI then
    Result := TAssetPackStatus.WaitingForWifi
  else
    Result := TAssetPackStatus.Unknown;
end;

function TAssetDelivery.GetAssets(const APath: string): TArray<string>;
var
  LList: TJavaObjectArray<JString>;
  I: Integer;
begin
  Result := [];
  LList := FAssetManager.list(StringToJString(APath));
  if LList <> nil then
  try
    for I := 0 to LList.Length - 1 do
      Result := Result + [JStringToString(LList.Items[I])];
  finally
    LList.Free;
  end;
end;

end.
