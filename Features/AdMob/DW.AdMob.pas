unit DW.AdMob;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes, System.Messaging;

const
  cTestAdUnitIdAppOpen = 'ca-app-pub-3940256099942544/3419835294';
  cTestAdUnitIdBanner =	'ca-app-pub-3940256099942544/6300978111';
  cTestAdUnitIdInterstitial	= 'ca-app-pub-3940256099942544/1033173712';
  cTestAdUnitIdInterstitialVideo = 'ca-app-pub-3940256099942544/8691691433';
  cTestAdUnitIdRewarded	= 'ca-app-pub-3940256099942544/5224354917';
  cTestAdUnitIdRewardedInterstitial	= 'ca-app-pub-3940256099942544/5354046379';
  cTestAdUnitIdNativeAdvanced	= 'ca-app-pub-3940256099942544/2247696110';
  cTestAdUnitIdNativeAdvancedVideo = 'ca-app-pub-3940256099942544/1044960115';

type
  TAdError = record
    ErrorCode: Integer;
    Message: string;
  end;

  TAdErrorEvent = procedure(Sender: TObject; const Error: TAdError) of object;

  TAdsStartedMessage = TMessage;

  TDebugGeography = (Disabled, EEA, NotEEA);

  TConsentError = record
    ErrorCode: Integer;
    Message: string;
    Origin: string;
    constructor Create(const AErrorCode: Integer; const AMessage, AOrigin: string);
  end;

  TConsentStatus = (Unknown, Required, NotRequired, Obtained);

  TConsentCompleteEvent = procedure(Sender: TObject; const Status: TConsentStatus) of object;
  TConsentErrorEvent = procedure(Sender: TObject; const Error: TConsentError) of object;

  IAdMob = interface(IInterface)
    ['{0B1932D7-A29D-44D7-AB3E-1AE85CBCC192}']
    function CanRequestAds: Boolean;
    function ConsentStatus: TConsentStatus;
    function GetOnConsentComplete: TConsentCompleteEvent;
    function GetOnConsentError: TConsentErrorEvent;
    function GetShowATTPrompt: Boolean;
    function IsStarted: Boolean;
    procedure RequestConsent(const ABypass: Boolean = False);
    procedure ResetConsent;
    procedure SetDebugGeography(const AGeography: TDebugGeography);
    procedure SetTestDeviceHashedId(const AHashedId: string);
    procedure SetOnConsentComplete(const Value: TConsentCompleteEvent);
    procedure SetOnConsentError(const Value: TConsentErrorEvent);
    procedure SetShowATTPrompt(const Value: Boolean);
    property OnConsentComplete: TConsentCompleteEvent read GetOnConsentComplete write SetOnConsentComplete;
    property OnConsentError: TConsentErrorEvent read GetOnConsentError write SetOnConsentError;
    property ShowATTPrompt: Boolean read GetShowATTPrompt write SetShowATTPrompt;
  end;

  TAdMob = class(TInterfacedObject, IAdMob)
  private
    FDebugGeography: TDebugGeography;
    FIsComplete: Boolean;
    FIsStarted: Boolean;
    FShowATTPrompt: Boolean;
    FTestDeviceHashedId: string;
    FOnConsentComplete: TConsentCompleteEvent;
    FOnConsentError: TConsentErrorEvent;
  protected
    procedure AdsStarted;
    procedure ConsentComplete(const AStatus: TConsentStatus);
    procedure ConsentError(const AError: TConsentError);
    procedure DoRequestConsent; virtual;
    procedure DoResetConsent; virtual;
    function NeedsDebugSettings: Boolean;
    property DebugGeography: TDebugGeography read FDebugGeography;
    property ShowATTPrompt: Boolean read FShowATTPrompt;
    property TestDeviceHashedId: string read FTestDeviceHashedId;
  public
    { IAdMob }
    function CanRequestAds: Boolean; virtual;
    function ConsentStatus: TConsentStatus; virtual;
    function GetOnConsentComplete: TConsentCompleteEvent;
    function GetOnConsentError: TConsentErrorEvent;
    function GetShowATTPrompt: Boolean;
    function IsStarted: Boolean;
    procedure RequestConsent(const ABypass: Boolean = False);
    procedure ResetConsent;
    procedure SetDebugGeography(const AGeography: TDebugGeography);
    procedure SetTestDeviceHashedId(const AHashedId: string);
    procedure SetOnConsentComplete(const Value: TConsentCompleteEvent);
    procedure SetOnConsentError(const Value: TConsentErrorEvent);
    procedure SetShowATTPrompt(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  AdMob: IAdMob;

implementation

uses
  // RTL
  System.SysUtils,
  // FMX
  FMX.Platform,
  // DW
{$IF Defined(ANDROID)}
  DW.AdMob.Android,
{$ENDIF}
{$IF Defined(IOS)}
  DW.AdMob.iOS,
{$ENDIF}
  DW.OSLog;

{ TConsentError }

constructor TConsentError.Create(const AErrorCode: Integer; const AMessage, AOrigin: string);
begin
  ErrorCode := AErrorCode;
  Message := AMessage;
  Origin := AOrigin;
end;

{ TAdMob }

constructor TAdMob.Create;
begin
  inherited Create;
  FShowATTPrompt := True;
end;

destructor TAdMob.Destroy;
begin
  //
  inherited;
end;

procedure TAdMob.DoRequestConsent;
begin
  //
end;

procedure TAdMob.DoResetConsent;
begin
  //
end;

function TAdMob.CanRequestAds: Boolean;
begin
  Result := False;
end;

procedure TAdMob.ConsentComplete(const AStatus: TConsentStatus);
begin
  if not FIsComplete then
  begin
    FIsComplete := True;
    if Assigned(FOnConsentComplete) then
      FOnConsentComplete(Self, AStatus);
  end;
end;

procedure TAdMob.ConsentError(const AError: TConsentError);
begin
  TOSLog.d('Consent error from: %s - %d: %s', [AError.Origin, AError.ErrorCode, AError.Message]);
  if Assigned(FOnConsentError) then
    FOnConsentError(Self, AError);
end;

function TAdMob.ConsentStatus: TConsentStatus;
begin
  Result := TConsentStatus.Unknown;
end;

function TAdMob.GetOnConsentComplete: TConsentCompleteEvent;
begin
  Result := FOnConsentComplete
end;

function TAdMob.GetOnConsentError: TConsentErrorEvent;
begin
  Result := FOnConsentError;
end;

function TAdMob.GetShowATTPrompt: Boolean;
begin
  Result := FShowATTPrompt;
end;

function TAdMob.IsStarted: Boolean;
begin
  Result := FIsStarted;
end;

function TAdMob.NeedsDebugSettings: Boolean;
begin
  Result := (FDebugGeography <> TDebugGeography.Disabled) or not FTestDeviceHashedId.IsEmpty;
end;

procedure TAdMob.AdsStarted;
begin
  FIsStarted := True;
  TMessageManager.DefaultManager.SendMessage(Self, TAdsStartedMessage.Create, True);
  ConsentComplete(ConsentStatus);
end;

procedure TAdMob.RequestConsent(const ABypass: Boolean = False);
begin
  if ABypass then
    AdsStarted
  else
    DoRequestConsent;
end;

procedure TAdMob.ResetConsent;
begin
  FIsComplete := False;
  DoResetConsent;
  FIsStarted := False;
end;

procedure TAdMob.SetDebugGeography(const AGeography: TDebugGeography);
begin
  FDebugGeography := AGeography;
end;

procedure TAdMob.SetOnConsentComplete(const Value: TConsentCompleteEvent);
begin
  FOnConsentComplete := Value;
end;

procedure TAdMob.SetOnConsentError(const Value: TConsentErrorEvent);
begin
  FOnConsentError := Value;
end;

procedure TAdMob.SetShowATTPrompt(const Value: Boolean);
begin
  FShowATTPrompt := Value;
end;

procedure TAdMob.SetTestDeviceHashedId(const AHashedId: string);
begin
  FTestDeviceHashedId := AHashedId;
end;

{$IF not (Defined(IOS) or Defined(ANDROID))}
initialization
  AdMob := TAdMob.Create;
{$ENDIF}

end.
