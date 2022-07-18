unit DW.AdMobAds;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Messaging,
  // DW
  DW.AdMob;

type
  TBaseAd = class;

  TCustomPlatformBaseAd = class(TObject)
  private
    class var FIsWarmStart: Boolean;
  private
    FBaseAd: TBaseAd;
    FTestMode: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  protected
    FAdUnitId: string;
    procedure ApplicationBecameActive; virtual;
    procedure ApplicationEnteredBackground; virtual;
    procedure DoAdFailedToLoad(const AError: TAdError); virtual;
    procedure DoAdLoaded; virtual;
    function GetAdUnitId: string; virtual;
    procedure Load; virtual;
    function IsWarmStart: Boolean;
    property AdUnitId: string read GetAdUnitId write FAdUnitId;
    property TestMode: Boolean read FTestMode write FTestMode;
  public
    constructor Create(const ABaseAd: TBaseAd);
    destructor Destroy; override;
  end;

  TFullScreenAd = class;

  TCustomPlatformFullScreenAd = class(TCustomPlatformBaseAd)
  private
    FFullScreenAd: TFullScreenAd;
  protected
    procedure DoAdDismissedFullScreenContent; virtual;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); virtual;
    procedure DoAdWillPresentFullScreenContent; virtual;
  public
    constructor Create(const AFullScreenAd: TFullScreenAd);
  end;

  TInterstitialAd = class;

  TCustomPlatformInterstitialAd = class(TCustomPlatformFullScreenAd)
  private
    FInterstitialAd: TInterstitialAd;
  protected
    function GetAdUnitId: string; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); virtual;
  end;

  TAdReward = record
    Amount: Integer;
    RewardType: string;
  end;

  TBaseRewardedAd = class;

  TCustomPlatformBaseRewardedAd = class(TCustomPlatformFullScreenAd)
  private
    FBaseRewardedAd: TBaseRewardedAd;
  protected
    procedure DoUserEarnedReward(const AReward: TAdReward); virtual;
  public
    constructor Create(const ABaseRewardedAd: TBaseRewardedAd);
  end;

  TRewardedAd = class;

  TCustomPlatformRewardedAd = class(TCustomPlatformBaseRewardedAd)
  private
    FRewardedAd: TRewardedAd;
  protected
    function GetAdUnitId: string; override;
  public
    constructor Create(const ARewardedAd: TRewardedAd); virtual;
  end;

  TRewardedInterstitialAd = class;

  TCustomPlatformRewardedInterstitialAd = class(TCustomPlatformBaseRewardedAd)
  private
    FRewardedInterstitialAd: TRewardedInterstitialAd;
  protected
    function GetAdUnitId: string; override;
  public
    constructor Create(const ARewardedInterstitialAd: TRewardedInterstitialAd); virtual;
  end;

  TAppOpenAdOrientation = (Portrait, Landscape);

  TAppOpenAd = class;

  TCustomPlatformAppOpenAd = class(TCustomPlatformFullScreenAd)
  private
    FAppOpenAd: TAppOpenAd;
    FOrientation: TAppOpenAdOrientation;
  protected
    function GetAdUnitId: string; override;
    property Orientation: TAppOpenAdOrientation read FOrientation write FOrientation;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); virtual;
  end;

  TBaseAd = class(TObject)
  private
    FOnAdFailedToLoad: TAdErrorEvent;
    FOnAdLoaded: TNotifyEvent;
    function GetAdUnitId: string;
    procedure SetAdUnitId(const Value: string);
    function GetTestMode: Boolean;
    procedure SetTestMode(const Value: Boolean);
  protected
    procedure DoAdFailedToLoad(const AError: TAdError);
    procedure DoAdLoaded;
    function GetPlatformBaseAd: TCustomPlatformBaseAd; virtual; abstract;
    property PlatformBaseAd: TCustomPlatformBaseAd read GetPlatformBaseAd;
  public
    procedure Load;
    property AdUnitId: string read GetAdUnitId write SetAdUnitId;
    property TestMode: Boolean read GetTestMode write SetTestMode;
    property OnAdFailedToLoad: TAdErrorEvent read FOnAdFailedToLoad write FOnAdFailedToLoad;
    property OnAdLoaded: TNotifyEvent read FOnAdLoaded write FOnAdLoaded;
  end;

  TFullScreenAd = class(TBaseAd)
  private
    FOnAdDismissedFullScreenContent: TNotifyEvent;
    FOnAdFailedToShowFullScreenContent: TAdErrorEvent;
    FOnAdWillPresentFullScreenContent: TNotifyEvent;
  protected
    procedure DoAdDismissedFullScreenContent;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError);
    procedure DoAdWillPresentFullScreenContent;
  public
    property OnAdDismissedFullScreenContent: TNotifyEvent read FOnAdDismissedFullScreenContent write FOnAdDismissedFullScreenContent;
    property OnAdFailedToShowFullScreenContent: TAdErrorEvent read FOnAdFailedToShowFullScreenContent write FOnAdFailedToShowFullScreenContent;
    property OnAdWillPresentFullScreenContent: TNotifyEvent read FOnAdWillPresentFullScreenContent write FOnAdWillPresentFullScreenContent;
  end;

  TInterstitialAd = class(TFullScreenAd)
  private
    FPlatformInterstitialAd: TCustomPlatformInterstitialAd;
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAdRewardEvent = procedure(Sender: TObject; const Reward: TAdReward) of object;

  TBaseRewardedAd = class(TFullScreenAd)
  private
    FOnUserEarnedReward: TAdRewardEvent;
  protected
    procedure DoUserEarnedReward(const AReward: TAdReward);
  public
    property OnUserEarnedReward: TAdRewardEvent read FOnUserEarnedReward write FOnUserEarnedReward;
  end;

  TRewardedAd = class(TBaseRewardedAd)
  private
    FPlatformRewardedAd: TCustomPlatformRewardedAd;
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRewardedInterstitialAd = class(TBaseRewardedAd)
  private
    FPlatformRewardedInterstitialAd: TCustomPlatformRewardedInterstitialAd;
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAppOpenAd = class(TFullScreenAd)
  private
    FPlatformAppOpenAd: TCustomPlatformAppOpenAd;
    function GetOrientation: TAppOpenAdOrientation;
    procedure SetOrientation(const Value: TAppOpenAdOrientation);
  protected
    function GetPlatformBaseAd: TCustomPlatformBaseAd; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Orientation: TAppOpenAdOrientation read GetOrientation write SetOrientation;
  end;

implementation

uses
  // DW
  DW.OSLog,
{$IF Defined(ANDROID)}
  DW.AdMobAds.Android,
{$ENDIF}
{$IF Defined(IOS)}
  DW.AdMobAds.iOS,
{$ENDIF}
  // RTL
  System.SysUtils,
  // FMX
  FMX.Platform;

{$IF not Defined(ANDROID) and not Defined(IOS)}
type
  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd);
  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd);
  TPlatformRewardedAd = class(TCustomPlatformRewardedAd);
  TPlatformRewardedInterstitialAd = class(TCustomPlatformRewardedInterstitialAd);
{$ENDIF}

{ TCustomPlatformBaseAd }

constructor TCustomPlatformBaseAd.Create(const ABaseAd: TBaseAd);
begin
  inherited Create;
  FBaseAd := ABaseAd;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformBaseAd.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomPlatformBaseAd.DoAdFailedToLoad(const AError: TAdError);
begin
  FBaseAd.DoAdFailedToLoad(AError);
end;

procedure TCustomPlatformBaseAd.DoAdLoaded;
begin
  FBaseAd.DoAdLoaded;
end;

function TCustomPlatformBaseAd.GetAdUnitId: string;
begin
  Result := FAdUnitId;
end;

function TCustomPlatformBaseAd.IsWarmStart: Boolean;
begin
  Result := FIsWarmStart;
end;

procedure TCustomPlatformBaseAd.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      ApplicationBecameActive;
      FIsWarmStart := True;
    end;
    TApplicationEvent.EnteredBackground:
      ApplicationEnteredBackground;
  end;
end;

procedure TCustomPlatformBaseAd.ApplicationBecameActive;
begin
  //
end;

procedure TCustomPlatformBaseAd.ApplicationEnteredBackground;
begin
  //
end;

procedure TCustomPlatformBaseAd.Load;
begin
  //
end;

{ TCustomPlatformFullScreenAd }

constructor TCustomPlatformFullScreenAd.Create(const AFullScreenAd: TFullScreenAd);
begin
  inherited Create(TBaseAd(AFullScreenAd));
  FFullScreenAd := AFullScreenAd;
end;

procedure TCustomPlatformFullScreenAd.DoAdDismissedFullScreenContent;
begin
  FFullScreenAd.DoAdDismissedFullScreenContent;
end;

procedure TCustomPlatformFullScreenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  FFullScreenAd.DoAdFailedToShowFullScreenContent(AError);
end;

procedure TCustomPlatformFullScreenAd.DoAdWillPresentFullScreenContent;
begin
  FFullScreenAd.DoAdWillPresentFullScreenContent;
end;

{ TCustomPlatformInterstitialAd }

constructor TCustomPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited Create(TFullScreenAd(AInterstitialAd));
  FInterstitialAd := AInterstitialAd;
end;

function TCustomPlatformInterstitialAd.GetAdUnitId: string;
begin
  if TestMode then
    Result := cTestAdUnitIdInterstitial
  else
    Result := FAdUnitId;
end;

{ TCustomPlatformBaseRewardedAd }

constructor TCustomPlatformBaseRewardedAd.Create(const ABaseRewardedAd: TBaseRewardedAd);
begin
  inherited Create(ABaseRewardedAd);
  FBaseRewardedAd := ABaseRewardedAd;
end;

procedure TCustomPlatformBaseRewardedAd.DoUserEarnedReward(const AReward: TAdReward);
begin
  FBaseRewardedAd.DoUserEarnedReward(AReward);
end;

{ TCustomPlatformRewardedAd }

constructor TCustomPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited Create(TBaseRewardedAd(ARewardedAd));
  FRewardedAd := ARewardedAd;
end;

function TCustomPlatformRewardedAd.GetAdUnitId: string;
begin
  if TestMode then
    Result := cTestAdUnitIdRewarded
  else
    Result := FAdUnitId;
end;

{ TCustomPlatformRewardedInterstitialAd }

constructor TCustomPlatformRewardedInterstitialAd.Create(const ARewardedInterstitialAd: TRewardedInterstitialAd);
begin
  inherited Create(TBaseRewardedAd(ARewardedInterstitialAd));
  FRewardedInterstitialAd := ARewardedInterstitialAd;
end;

function TCustomPlatformRewardedInterstitialAd.GetAdUnitId: string;
begin
  if TestMode then
    Result := cTestAdUnitIdRewardedInterstitial
  else
    Result := FAdUnitId;
end;

{ TCustomPlatformAppOpenAd }

constructor TCustomPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited Create(TFullScreenAd(AAppOpenAd));
  FAppOpenAd := AAppOpenAd;
end;

function TCustomPlatformAppOpenAd.GetAdUnitId: string;
begin
  if TestMode then
    Result := cTestAdUnitIdAppOpen
  else
    Result := FAdUnitId;
end;

{ TBaseAd }

procedure TBaseAd.DoAdFailedToLoad(const AError: TAdError);
begin
  TOSLog.d('DoAdFailedToLoad (%s) - %d: %s', [ClassName, AError.ErrorCode, AError.Message]);
  if Assigned(FOnAdFailedToLoad) then
    FOnAdFailedToLoad(Self, AError);
end;

procedure TBaseAd.DoAdLoaded;
begin
  if Assigned(FOnAdLoaded) then
    FOnAdLoaded(Self);
end;

function TBaseAd.GetAdUnitId: string;
begin
  Result := PlatformBaseAd.AdUnitId;
end;

function TBaseAd.GetTestMode: Boolean;
begin
  Result := PlatformBaseAd.TestMode;
end;

procedure TBaseAd.Load;
begin
  if not AdUnitId.IsEmpty then
    PlatformBaseAd.Load;
end;

procedure TBaseAd.SetAdUnitId(const Value: string);
begin
  PlatformBaseAd.AdUnitId := Value;
end;

procedure TBaseAd.SetTestMode(const Value: Boolean);
begin
  PlatformBaseAd.TestMode := Value;
end;

{ TFullScreenAd }

procedure TFullScreenAd.DoAdDismissedFullScreenContent;
begin
  if Assigned(FOnAdDismissedFullScreenContent) then
    FOnAdDismissedFullScreenContent(Self);
end;

procedure TFullScreenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  TOSLog.d('DoAdFailedToShowFullScreenContent (%s) - %d: %s', [ClassName, AError.ErrorCode, AError.Message]);
  if Assigned(FOnAdFailedToShowFullScreenContent) then
    FOnAdFailedToShowFullScreenContent(Self, AError);
end;

procedure TFullScreenAd.DoAdWillPresentFullScreenContent;
begin
  if Assigned(FOnAdWillPresentFullScreenContent) then
    FOnAdWillPresentFullScreenContent(Self);
end;

{ TInterstitialAd }

constructor TInterstitialAd.Create;
begin
  inherited;
  FPlatformInterstitialAd := TPlatformInterstitialAd.Create(Self);
end;

destructor TInterstitialAd.Destroy;
begin
  FPlatformInterstitialAd.Free;
  inherited;
end;

function TInterstitialAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformInterstitialAd;
end;

{ TBaseRewardedAd }

procedure TBaseRewardedAd.DoUserEarnedReward(const AReward: TAdReward);
begin
  if Assigned(FOnUserEarnedReward) then
    FOnUserEarnedReward(Self, AReward);
end;

{ TRewardedAd }

constructor TRewardedAd.Create;
begin
  inherited;
  FPlatformRewardedAd := TPlatformRewardedAd.Create(Self);
end;

destructor TRewardedAd.Destroy;
begin
  FPlatformRewardedAd.Free;
  inherited;
end;

function TRewardedAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformRewardedAd;
end;

{ TRewardedInterstitialAd }

constructor TRewardedInterstitialAd.Create;
begin
  inherited;
  FPlatformRewardedInterstitialAd := TPlatformRewardedInterstitialAd.Create(Self);
end;

destructor TRewardedInterstitialAd.Destroy;
begin
  FPlatformRewardedInterstitialAd.Free;
  inherited;
end;

function TRewardedInterstitialAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformRewardedInterstitialAd;
end;

{ TAppOpenAd }

constructor TAppOpenAd.Create;
begin
  inherited;
  FPlatformAppOpenAd := TPlatformAppOpenAd.Create(Self);
end;

destructor TAppOpenAd.Destroy;
begin
  FPlatformAppOpenAd.Free;
  inherited;
end;

function TAppOpenAd.GetOrientation: TAppOpenAdOrientation;
begin
  Result := FPlatformAppOpenAd.Orientation;
end;

function TAppOpenAd.GetPlatformBaseAd: TCustomPlatformBaseAd;
begin
  Result := FPlatformAppOpenAd;
end;

procedure TAppOpenAd.SetOrientation(const Value: TAppOpenAdOrientation);
begin
  FPlatformAppOpenAd.Orientation := Value;
end;

end.
