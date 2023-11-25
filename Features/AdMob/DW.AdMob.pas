unit DW.AdMob;

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
  System.Messaging;

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

  TConsentErrorEvent = procedure(Sender: TObject; const Error: TConsentError) of object;

  IAdMob = interface(IInterface)
    ['{0B1932D7-A29D-44D7-AB3E-1AE85CBCC192}']
    function GetOnConsentError: TConsentErrorEvent;
    function IsStarted: Boolean;
    procedure ResetConsent;
    procedure SetDebugGeography(const AGeography: TDebugGeography);
    procedure SetTestDeviceHashedId(const AHashedId: string);
    procedure SetIsUMPEnabled(const AEnabled: Boolean);
    procedure SetOnConsentError(const Value: TConsentErrorEvent);
    property OnConsentError: TConsentErrorEvent read GetOnConsentError write SetOnConsentError;
  end;

  TAdMob = class(TInterfacedObject, IAdMob)
  private
    FDebugGeography: TDebugGeography;
    FHasLaunched: Boolean;
    FIsStarted: Boolean;
    FIsUMPEnabled: Boolean;
    FTestDeviceHashedId: string;
    FOnConsentError: TConsentErrorEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    procedure AdsStarted;
    procedure ConsentError(const AError: TConsentError);
    function NeedsDebugSettings: Boolean;
    procedure RequestConsent; virtual;
    property DebugGeography: TDebugGeography read FDebugGeography;
    property TestDeviceHashedId: string read FTestDeviceHashedId;
  public
    { IAdMob }
    function GetOnConsentError: TConsentErrorEvent;
    function IsStarted: Boolean;
    procedure ResetConsent; virtual;
    procedure SetDebugGeography(const AGeography: TDebugGeography);
    procedure SetTestDeviceHashedId(const AHashedId: string);
    procedure SetIsUMPEnabled(const AEnabled: Boolean);
    procedure SetOnConsentError(const Value: TConsentErrorEvent);
  public
    constructor Create;
    destructor Destroy; override;
    property OnConsentError: TConsentErrorEvent read GetOnConsentError write SetOnConsentError;
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
  FIsUMPEnabled := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TAdMob.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TAdMob.ConsentError(const AError: TConsentError);
begin
  TOSLog.d('Consent error from: %s - %d: %s', [AError.Origin, AError.ErrorCode, AError.Message]);
  if Assigned(FOnConsentError) then
    FOnConsentError(Self, AError);
end;

function TAdMob.GetOnConsentError: TConsentErrorEvent;
begin
  Result := FOnConsentError;
end;

function TAdMob.IsStarted: Boolean;
begin
  Result := FIsStarted;
end;

function TAdMob.NeedsDebugSettings: Boolean;
begin
  Result := (FDebugGeography <> TDebugGeography.Disabled) or not FTestDeviceHashedId.IsEmpty;
end;

procedure TAdMob.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if not FHasLaunched then
      begin
        FHasLaunched := True;
        if FIsUMPEnabled then
          RequestConsent
        else
          AdsStarted;
      end;
    end;
  end;
end;

procedure TAdMob.AdsStarted;
begin
  FIsStarted := True;
  TMessageManager.DefaultManager.SendMessage(Self, TAdsStartedMessage.Create, True);
end;

procedure TAdMob.RequestConsent;
begin
  //
end;

procedure TAdMob.ResetConsent;
begin
  //
end;

procedure TAdMob.SetDebugGeography(const AGeography: TDebugGeography);
begin
  FDebugGeography := AGeography;
end;

procedure TAdMob.SetIsUMPEnabled(const AEnabled: Boolean);
begin
  FIsUMPEnabled := AEnabled;
end;

procedure TAdMob.SetOnConsentError(const Value: TConsentErrorEvent);
begin
  FOnConsentError := Value;
end;

procedure TAdMob.SetTestDeviceHashedId(const AHashedId: string);
begin
  FTestDeviceHashedId := AHashedId;
end;

{$IF not (Defined(IOS) or Defined(ANDROID))}
// {$IF not (Defined(IOS))}
initialization
  AdMob := TAdMob.Create;
{$ENDIF}

end.
