unit DW.AgeStatus.Android;

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

uses
  // Android
  Androidapi.JNI.JavaTypes,
  // DW
  DW.AgeStatus, DW.PlayTasksEvents.Android, DW.Androidapi.JNI.PlayServices.AgeSignals;

type
  TPlatformAgeStatus = class(TCustomAgeStatus)
  private
    FAgeSignalsAccessRequestEvents: IPlayTasksEvents;
    FCheckAgeSignalsEvents: IPlayTasksEvents;
    procedure AgeSignalsAccessRequestSuccessHandler(const AResult: JAgeSignalsAccessResult; const AHandler: TAgeStatusResultProc);
    procedure CheckAgeSignalsSuccessHandler(const AResult: JAgeSignalsResult; const AHandler: TAgeStatusResultProc);
    procedure DoCheck(const AHandler: TAgeStatusResultProc);
    procedure DoFakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc);
    procedure FakeAgeSignalsAccessRequestSuccessHandler(const ADetails: TFakeAgeStatusDetails; const AResult: JAgeSignalsAccessResult;
      const AHandler: TAgeStatusResultProc);
    function GetAgeSignalsStatusNative(const AStatus: TAgeSignalsStatus): JInteger;
    function GetSignificantChangeStatus(const AStatus: JInteger): TSignificantChangeStatus;
    function GetSignificantChangeStatusNative(const AStatus: TSignificantChangeStatus): JInteger;
    procedure HandleNotShared(const AStatus: Integer; const AHandler: TAgeStatusResultProc);
  public
    procedure Check(const AHandler: TAgeStatusResultProc); override;
    procedure FakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc); override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.App;

function JIntegerToInteger(const AValue: JInteger; const ADefault: Integer = -1): Integer;
begin
  if AValue <> nil then
    Result := AValue.intValue
  else
    Result := ADefault;
end;

function JDateToDateTime(const AValue: JDate; const ADefault: TDateTime = 0): TDateTime;
begin
  if AValue <> nil then
    Result := EncodeDate(AValue.getYear + 1900, AValue.getMonth + 1, AValue.getDay)
  else
    Result := ADefault;
end;

function DateTimeToJDate(const AValue: TDateTime): JDate;
begin
  if AValue > 0 then
    Result := TJDate.JavaClass.init(YearOf(AValue) - 1900, MonthOf(AValue) - 1, DayOf(AValue))
  else
    Result := nil;
end;

{ TPlatformAgeStatus }

procedure TPlatformAgeStatus.Check(const AHandler: TAgeStatusResultProc);
var
  LAgeSignalsManager: JAgeSignalsManager;
  LAccessRequest: JAgeSignalsAccessRequest;
begin
  if FAgeSignalsAccessRequestEvents = nil then
    FAgeSignalsAccessRequestEvents := TPlayTasksEvents.Create;
  LAgeSignalsManager := TJAgeSignalsManagerFactory.JavaClass.create(TAndroidHelper.Context);
  LAccessRequest := TJAgeSignalsAccessRequest.JavaClass.builder
    .setActivity(TAndroidHelper.Activity)
    .build;
  FAgeSignalsAccessRequestEvents.SetTask(LAgeSignalsManager.requestAgeSignalsAccess(LAccessRequest))
    .OnSuccess(
      procedure(const AResult: JObject)
      begin
        AgeSignalsAccessRequestSuccessHandler(TJAgeSignalsAccessResult.Wrap(AResult), AHandler);
      end
    )
    .OnFailure(
      procedure(const AException: JException)
      begin
        AHandler(TAgeStatusResult.Create(TAgeStatusResultKind.AccessFailed, JStringToString(AException.getLocalizedMessage)));
      end
    );
end;

procedure TPlatformAgeStatus.AgeSignalsAccessRequestSuccessHandler(const AResult: JAgeSignalsAccessResult; const AHandler: TAgeStatusResultProc);
var
  LStatus: Integer;
begin
  LStatus := AResult.ageSignalsStatus.intValue;
  if LStatus = TJAgeSignalsStatus.JavaClass.SHARED then
    DoCheck(AHandler)
  else
    HandleNotShared(LStatus, AHandler);
end;

procedure TPlatformAgeStatus.HandleNotShared(const AStatus: Integer; const AHandler: TAgeStatusResultProc);
var
  LKind: TAgeStatusResultKind;
begin
  if AStatus = TJAgeSignalsStatus.JavaClass.NOT_SHARED then
    LKind := TAgeStatusResultKind.StatusNotShared
  else if AStatus = TJAgeSignalsStatus.JavaClass.UNSPECIFIED then
    LKind := TAgeStatusResultKind.StatusUnspecified
  else if AStatus = TJAgeSignalsStatus.JavaClass.VERIFICATION_REQUIRED then
    LKind := TAgeStatusResultKind.StatusVerificationRequired
  else
    LKind := TAgeStatusResultKind.StatusUnspecified;
  AHandler(TAgeStatusResult.Create(LKind));
end;

procedure TPlatformAgeStatus.DoCheck(const AHandler: TAgeStatusResultProc);
var
  LAgeSignalsManager: JAgeSignalsManager;
begin
  if FCheckAgeSignalsEvents = nil then
    FCheckAgeSignalsEvents := TPlayTasksEvents.Create;
  if FAgeSignalsAccessRequestEvents = nil then
    FAgeSignalsAccessRequestEvents := TPlayTasksEvents.Create;
  LAgeSignalsManager := TJAgeSignalsManagerFactory.JavaClass.create(TAndroidHelper.Context);
  FCheckAgeSignalsEvents.SetTask(LAgeSignalsManager.checkAgeSignals(TJAgeSignalsRequest.JavaClass.builder.build))
    .OnSuccess(
      procedure(const AResult: JObject)
      begin
        CheckAgeSignalsSuccessHandler(TJAgeSignalsResult.Wrap(AResult), AHandler);
      end
    )
    .OnFailure(
      procedure(const AException: JException)
      begin
        AHandler(TAgeStatusResult.Create(TAgeStatusResultKind.CheckStatusFailed, JStringToString(AException.getLocalizedMessage)));
      end
    );
end;

function TPlatformAgeStatus.GetAgeSignalsStatusNative(const AStatus: TAgeSignalsStatus): JInteger;
begin
  case AStatus of
    TAgeSignalsStatus.NotShared:
      Result := TJInteger.JavaClass.init(TJAgeSignalsStatus.JavaClass.NOT_SHARED);
    TAgeSignalsStatus.Shared:
      Result := TJInteger.JavaClass.init(TJAgeSignalsStatus.JavaClass.SHARED);
    TAgeSignalsStatus.VerificationRequired:
      Result := TJInteger.JavaClass.init(TJAgeSignalsStatus.JavaClass.VERIFICATION_REQUIRED);
    TAgeSignalsStatus.Unspecified:
      Result := TJInteger.JavaClass.init(TJAgeSignalsStatus.JavaClass.UNSPECIFIED);
  else
    Result := nil;
  end;
end;

function TPlatformAgeStatus.GetSignificantChangeStatus(const AStatus: JInteger): TSignificantChangeStatus;
begin
  Result := TSignificantChangeStatus.None;
  if AStatus <> nil then
  begin
    if AStatus.intValue = TJSignificantChangeStatus.JavaClass.APPROVED then
      Result := TSignificantChangeStatus.Approved
    else if AStatus.intValue = TJSignificantChangeStatus.JavaClass.DECLINED then
      Result := TSignificantChangeStatus.Declined
    else if AStatus.intValue = TJSignificantChangeStatus.JavaClass.PENDING then
      Result := TSignificantChangeStatus.Pending
    else if AStatus.intValue = TJSignificantChangeStatus.JavaClass.UNSPECIFIED then
      Result := TSignificantChangeStatus.Unspecified
  end;
end;

function TPlatformAgeStatus.GetSignificantChangeStatusNative(const AStatus: TSignificantChangeStatus): JInteger;
begin
  case AStatus of
    TSignificantChangeStatus.Approved:
      Result := TJInteger.JavaClass.init(TJSignificantChangeStatus.JavaClass.APPROVED);
    TSignificantChangeStatus.Declined:
      Result := TJInteger.JavaClass.init(TJSignificantChangeStatus.JavaClass.DECLINED);
    TSignificantChangeStatus.Pending:
      Result := TJInteger.JavaClass.init(TJSignificantChangeStatus.JavaClass.PENDING);
    TSignificantChangeStatus.Unspecified:
      Result := TJInteger.JavaClass.init(TJSignificantChangeStatus.JavaClass.UNSPECIFIED);
  else
    Result := nil;
  end;
end;

procedure TPlatformAgeStatus.CheckAgeSignalsSuccessHandler(const AResult: JAgeSignalsResult; const AHandler: TAgeStatusResultProc);
var
  LStatusResult: TAgeStatusResult;
begin
  LStatusResult := TAgeStatusResult.Create(
    JIntegerToInteger(AResult.ageLower),
    JIntegerToInteger(AResult.ageUpper),
    GetSignificantChangeStatus(AResult.significantChangeStatus),
    JDateToDateTime(AResult.significantChangeApprovalDate)
  );
  AHandler(LStatusResult);
end;

procedure TPlatformAgeStatus.FakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc);
var
  LFakeAgeSignalsManager: JFakeAgeSignalsManager;
  LResult: JAgeSignalsAccessResult;
  LAccessRequest: JAgeSignalsAccessRequest;
begin
  if FAgeSignalsAccessRequestEvents = nil then
    FAgeSignalsAccessRequestEvents := TPlayTasksEvents.Create;
  LFakeAgeSignalsManager := TJFakeAgeSignalsManager.JavaClass.init;
  LResult := TJAgeSignalsAccessResult.JavaClass.builder
    .setAgeSignalsStatus(GetAgeSignalsStatusNative(ADetails.AgeSignalsStatus))
    .build;
  LFakeAgeSignalsManager.setNextAgeSignalsAccessResult(LResult);
  LAccessRequest := TJAgeSignalsAccessRequest.JavaClass.builder
    .setActivity(TAndroidHelper.Activity)
    .build;
  FAgeSignalsAccessRequestEvents.SetTask(LFakeAgeSignalsManager.requestAgeSignalsAccess(LAccessRequest))
    .OnSuccess(
      procedure(const AResult: JObject)
      begin
        FakeAgeSignalsAccessRequestSuccessHandler(ADetails, TJAgeSignalsAccessResult.Wrap(AResult), AHandler);
      end
    )
    .OnFailure(
      procedure(const AException: JException)
      begin
        AHandler(TAgeStatusResult.Create(TAgeStatusResultKind.AccessFailed, JStringToString(AException.getLocalizedMessage)));
      end
    );
end;

procedure TPlatformAgeStatus.FakeAgeSignalsAccessRequestSuccessHandler(const ADetails: TFakeAgeStatusDetails; const AResult: JAgeSignalsAccessResult;
  const AHandler: TAgeStatusResultProc);
var
  LStatus: Integer;
begin
  LStatus := AResult.ageSignalsStatus.intValue;
  if LStatus = TJAgeSignalsStatus.JavaClass.SHARED then
    DoFakeCheck(ADetails, AHandler)
  else
    HandleNotShared(LStatus, AHandler);
end;

procedure TPlatformAgeStatus.DoFakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc);
var
  LBuilder: JAgeSignalsResult_Builder;
  LFakeAgeSignalsManager: JFakeAgeSignalsManager;
begin
  if FCheckAgeSignalsEvents = nil then
    FCheckAgeSignalsEvents := TPlayTasksEvents.Create;
  LFakeAgeSignalsManager := TJFakeAgeSignalsManager.JavaClass.init;
  LBuilder := TJAgeSignalsResult.JavaClass.builder
    .setInstallId(StringToJString('12345678'))
    .setSignificantChangeStatus(GetSignificantChangeStatusNative(ADetails.SignificantChangeStatus))
    .setSignificantChangeApprovalDate(DateTimeToJDate(ADetails.SignificantChangeApprovalDate));
  if ADetails.AgeLower > -1 then
    LBuilder.setAgeLower(TJInteger.JavaClass.init(ADetails.AgeLower));
  if ADetails.AgeUpper > -1 then
    LBuilder.setAgeUpper(TJInteger.JavaClass.init(ADetails.AgeUpper));
  LFakeAgeSignalsManager.setNextAgeSignalsResult(LBuilder.build);
  FCheckAgeSignalsEvents.SetTask(LFakeAgeSignalsManager.checkAgeSignals(TJAgeSignalsRequest.JavaClass.builder.build))
    .OnSuccess(
      procedure(const AResult: JObject)
      begin
        CheckAgeSignalsSuccessHandler(TJAgeSignalsResult.Wrap(AResult), AHandler);
      end
    )
    .OnFailure(
      procedure(const AException: JException)
      begin
        AHandler(TAgeStatusResult.Create(TAgeStatusResultKind.CheckStatusFailed, JStringToString(AException.getLocalizedMessage)));
      end
    );
end;

end.
