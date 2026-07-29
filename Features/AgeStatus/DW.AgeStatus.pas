unit DW.AgeStatus;

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
  TAgeStatusResultKind = (Success, AccessFailed, CheckStatusFailed, StatusNotShared, StatusUnspecified, StatusVerificationRequired);

  TSignificantChangeStatus = (None, Unspecified, Approved, Pending, Declined);

  TAgeStatusResult = record
    AgeLower: Integer;
    AgeUpper: Integer;
    Kind: TAgeStatusResultKind;
    SignificantChangeApprovalDate: TDateTime;
    SignificantChangeStatus: TSignificantChangeStatus;
    StatusMessage: string;
    constructor Create(const AKind: TAgeStatusResultKind; const AStatusMessage: string = ''); overload;
    constructor Create(const AAgeLower, AAgeUpper: Integer; const AStatus: TSignificantChangeStatus; const ADate: TDateTime); overload;
  end;

  TAgeStatusResultProc = reference to procedure(const StatusResult: TAgeStatusResult);

  // TierA: User has self declared their age.
  // TierB: User's age is managed by a parent or a guardian.
  // TierC: User's age is assessed by using credit card, email address, selfie assessment, Government ID, or Tax ID.
  // TierD: User's age is checked by using a combination of Government ID and selfie assessment, or Digital ID.

  TAgeRangeSource = (Unspecified, TierA, TierB, TierC, TierD);

  TAgeSignalsStatus = (NotShared, Shared, Unspecified, VerificationRequired);

  TFakeAgeStatusDetails = record
    AgeRangeSource: TAgeRangeSource;
    AgeSignalsStatus: TAgeSignalsStatus;
    AgeLower: Integer;
    AgeUpper: Integer;
    SignificantChangeApprovalDate: TDateTime;
    SignificantChangeStatus: TSignificantChangeStatus;
    constructor Create(const ASignalsStatus: TAgeSignalsStatus; const ARangeSource: TAgeRangeSource; const AAgeLower, AAgeUpper: Integer;
      const AChangeStatus: TSignificantChangeStatus; const ADate: TDateTime);
  end;

  IAgeStatus = interface
    ['{153BAC22-D9EC-4121-9E65-B10830466B77}']
    procedure Check(const AHandler: TAgeStatusResultProc);
    procedure FakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc);
  end;

  TCustomAgeStatus = class(TInterfacedObject, IAgeStatus)
  public
    { IAgeStatus }
    procedure Check(const AHandler: TAgeStatusResultProc); virtual;
    procedure FakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc); virtual;
  end;

var
  AgeStatus: IAgeStatus;

implementation

{$IF Defined(ANDROID)}
uses
  DW.AgeStatus.Android;
{$ELSE}
type
  TPlatformAgeStatus = class(TCustomAgeStatus);
{$ENDIF}

{ TFakeAgeStatusDetails }

constructor TFakeAgeStatusDetails.Create(const ASignalsStatus: TAgeSignalsStatus; const ARangeSource: TAgeRangeSource;
  const AAgeLower, AAgeUpper: Integer; const AChangeStatus: TSignificantChangeStatus; const ADate: TDateTime);
begin
  AgeRangeSource := ARangeSource;
  AgeLower := AAgeLower;
  AgeUpper := AAgeUpper;
  AgeSignalsStatus := ASignalsStatus;
  SignificantChangeStatus := AChangeStatus;
  SignificantChangeApprovalDate := ADate;
end;

{ TAgeStatusResult }

constructor TAgeStatusResult.Create(const AKind: TAgeStatusResultKind; const AStatusMessage: string = '');
begin
  Kind := AKind;
  StatusMessage := AStatusMessage;
  Create(-1, -1, TSignificantChangeStatus.None, 0);
end;

constructor TAgeStatusResult.Create(const AAgeLower, AAgeUpper: Integer; const AStatus: TSignificantChangeStatus; const ADate: TDateTime);
begin
  AgeLower := AAgeLower;
  AgeUpper := AAgeUpper;
  SignificantChangeApprovalDate := ADate;
  SignificantChangeStatus := AStatus;
  if SignificantChangeStatus <> TSignificantChangeStatus.None then
    Kind := TAgeStatusResultKind.Success;
end;

{ TCustomAgeStatus }

procedure TCustomAgeStatus.Check(const AHandler: TAgeStatusResultProc);
begin
  //
end;

procedure TCustomAgeStatus.FakeCheck(const ADetails: TFakeAgeStatusDetails; const AHandler: TAgeStatusResultProc);
begin
  //
end;

initialization
  AgeStatus := TPlatformAgeStatus.Create;

end.
