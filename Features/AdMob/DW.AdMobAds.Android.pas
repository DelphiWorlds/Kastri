unit DW.AdMobAds.Android;

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
  System.Classes, System.SysUtils,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.AdMob, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os, Androidapi.JNI.App,
  // DW
  DW.Androidapi.JNI.AdMob, DW.AdMob, DW.AdMobAds;

type
  JDWAppOpenAdLoadCallback = interface;
  JDWAppOpenAdLoadCallbackDelegate = interface;
  JDWInterstitialAdCallback = interface;
  JDWInterstitialAdCallbackDelegate = interface;
  JDWRewardedAdLoadCallback = interface;
  JDWRewardedAdLoadCallbackDelegate = interface;
  JDWRewardedInterstitialAdLoadCallback = interface;
  JDWRewardedInterstitialAdLoadCallbackDelegate = interface;

  JDWInterstitialAdCallbackClass = interface(JInterstitialAdLoadCallbackClass)
    ['{5CC7467D-5131-4C6A-BB4E-FE8945F6F5B6}']
    {class} function init(delegate: JDWInterstitialAdCallbackDelegate): JDWInterstitialAdCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWInterstitialAdCallback')]
  JDWInterstitialAdCallback = interface(JInterstitialAdLoadCallback)
    ['{8818EEA7-36A6-4246-8079-CA10E5F141DB}']
  end;
  TJDWInterstitialAdCallback = class(TJavaGenericImport<JDWInterstitialAdCallbackClass, JDWInterstitialAdCallback>) end;

  JDWInterstitialAdCallbackDelegateClass = interface(IJavaClass)
    ['{81A3438F-D0A8-4392-B604-81A47B875FD4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWInterstitialAdCallbackDelegate')]
  JDWInterstitialAdCallbackDelegate = interface(IJavaInstance)
    ['{11DEBF75-98FA-42B8-B9DE-5661D0CEC5AF}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(interstitialAd: JInterstitial_InterstitialAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWInterstitialAdCallbackDelegate = class(TJavaGenericImport<JDWInterstitialAdCallbackDelegateClass, JDWInterstitialAdCallbackDelegate>) end;

  JDWRewardedAdLoadCallbackClass = interface(JRewardedAdLoadCallbackClass)
    ['{5CC7467D-5131-4C6A-BB4E-FE8945F6F5B6}']
    {class} function init(delegate: JDWRewardedAdLoadCallbackDelegate): JDWRewardedAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedAdLoadCallback')]
  JDWRewardedAdLoadCallback = interface(JRewardedAdLoadCallback)
    ['{8818EEA7-36A6-4246-8079-CA10E5F141DB}']
  end;
  TJDWRewardedAdLoadCallback = class(TJavaGenericImport<JDWRewardedAdLoadCallbackClass, JDWRewardedAdLoadCallback>) end;

  JDWRewardedAdLoadCallbackDelegateClass = interface(IJavaClass)
    ['{81A3438F-D0A8-4392-B604-81A47B875FD4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedAdLoadCallbackDelegate')]
  JDWRewardedAdLoadCallbackDelegate = interface(IJavaInstance)
    ['{11DEBF75-98FA-42B8-B9DE-5661D0CEC5AF}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(rewardedAd: JRewardedAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWRewardedAdLoadCallbackDelegate = class(TJavaGenericImport<JDWRewardedAdLoadCallbackDelegateClass, JDWRewardedAdLoadCallbackDelegate>) end;

  JDWRewardedInterstitialAdLoadCallbackClass = interface(JRewardedInterstitialAdLoadCallbackClass)
    ['{06214E8A-0F7C-45C3-A2CB-5016E57E8534}']
    {class} function init(delegate: JDWRewardedInterstitialAdLoadCallbackDelegate): JDWRewardedInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedInterstitialAdLoadCallback')]
  JDWRewardedInterstitialAdLoadCallback = interface(JRewardedInterstitialAdLoadCallback)
    ['{C3369D51-0B53-44E0-B97A-E1BEC17675BE}']
  end;
  TJDWRewardedInterstitialAdLoadCallback = class(TJavaGenericImport<JDWRewardedInterstitialAdLoadCallbackClass,
    JDWRewardedInterstitialAdLoadCallback>) end;

  JDWRewardedInterstitialAdLoadCallbackDelegateClass = interface(IJavaClass)
    ['{71C29500-D2F2-49C9-B240-3F4FDD116F48}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRewardedInterstitialAdLoadCallbackDelegate')]
  JDWRewardedInterstitialAdLoadCallbackDelegate = interface(IJavaInstance)
    ['{F61B31AD-2721-4387-83B2-F649EB779FB4}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(rewardedInterstitialAd: JRewardedInterstitialAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWRewardedInterstitialAdLoadCallbackDelegate = class(TJavaGenericImport<JDWRewardedInterstitialAdLoadCallbackDelegateClass,
    JDWRewardedInterstitialAdLoadCallbackDelegate>) end;

  JDWAppOpenAdLoadCallbackClass = interface(JAppOpenAd_AppOpenAdLoadCallbackClass)
    ['{C6E850B3-3CB4-4FEF-BF18-BC76EFD112B4}']
    {class} function init(delegate: JDWAppOpenAdLoadCallbackDelegate): JDWAppOpenAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAppOpenAdLoadCallback')]
  JDWAppOpenAdLoadCallback = interface(JAppOpenAd_AppOpenAdLoadCallback)
    ['{05C207B7-5D2E-4809-89F5-3867D01A143E}']
  end;
  TJDWAppOpenAdLoadCallback = class(TJavaGenericImport<JDWAppOpenAdLoadCallbackClass, JDWAppOpenAdLoadCallback>) end;

  JDWAppOpenAdLoadCallbackDelegateClass = interface(IJavaClass)
    ['{6DDDEFE6-8FAA-4CE1-A8C0-DA14EEDFE06C}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAppOpenAdLoadCallbackDelegate')]
  JDWAppOpenAdLoadCallbackDelegate = interface(IJavaInstance)
    ['{C98A7860-16ED-4675-9100-75A7524ADC3E}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(ad: JAppOpenAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJDWAppOpenAdLoadCallbackDelegate = class(TJavaGenericImport<JDWAppOpenAdLoadCallbackDelegateClass, JDWAppOpenAdLoadCallbackDelegate>) end;

  TPlatformInterstitialAd = class;

  TInterstitialAdCallbackDelegate = class(TJavaLocal, JDWInterstitialAdCallbackDelegate)
  private
    FPlatformInterstitialAd: TPlatformInterstitialAd;
    FCallback: JDWInterstitialAdCallback;
  protected
    property Callback: JDWInterstitialAdCallback read FCallback;
  public
    { JDWInterstitialAdCallbackDelegate }
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(interstitialAd: JInterstitial_InterstitialAd); cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const APlatformInterstitialAd: TPlatformInterstitialAd);
  end;

  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd)
  private
    FAd: Jinterstitial_InterstitialAd;
    FCallbackDelegate: TInterstitialAdCallbackDelegate;
  protected
    procedure AdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdWillPresentFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); override;
    destructor Destroy; override;
  end;

  TOpenCustomPlatformBaseRewardedAd = class(TCustomPlatformBaseRewardedAd);

  TUserEarnedRewardListener = class(TJavaLocal, JOnUserEarnedRewardListener)
  private
    FPlatformBaseRewardedAd: TOpenCustomPlatformBaseRewardedAd;
  public
    { JOnUserEarnedRewardListener }
    procedure onUserEarnedReward(rewardItem: JRewardItem); cdecl;
  public
    constructor Create(const APlatformBaseRewardedAd: TCustomPlatformBaseRewardedAd);
  end;

  TPlatformRewardedAd = class;

  TRewardedAdLoadCallbackDelegate = class(TJavaLocal, JDWRewardedAdLoadCallbackDelegate)
  private
    FPlatformRewardedAd: TPlatformRewardedAd;
    FCallback: JDWRewardedAdLoadCallback;
  protected
    property Callback: JDWRewardedAdLoadCallback read FCallback;
  public
    { JDWRewardedAdCallbackDelegate }
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdLoaded(rewardedAd: JRewardedAd); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const APlatformRewardedAd: TPlatformRewardedAd);
  end;

  TPlatformRewardedAd = class(TCustomPlatformRewardedAd)
  private
    FAd: JRewardedAd;
    FCallbackDelegate: TRewardedAdLoadCallbackDelegate;
    FUserEarnedRewardListener: JOnUserEarnedRewardListener;
  protected
    procedure AdLoaded(const ARewardedAd: JRewardedAd);
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdWillPresentFullScreenContent; override;
    procedure DoUserEarnedReward(const AReward: TAdReward); override;
    procedure Load; override;
  public
    constructor Create(const ARewardedAd: TRewardedAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedInterstitialAd = class;

  TRewardedInterstitialAdLoadCallbackDelegate = class(TJavaLocal, JDWRewardedInterstitialAdLoadCallbackDelegate)
  private
    FPlatformRewardedInterstitialAd: TPlatformRewardedInterstitialAd;
    FCallback: JDWRewardedInterstitialAdLoadCallback;
  protected
    property Callback: JDWRewardedInterstitialAdLoadCallback read FCallback;
  public
    { JDWRewardedInterstitialAdCallbackDelegate }
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdLoaded(rewardedInterstitialAd: JRewardedInterstitialAd); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const APlatformRewardedInterstitialAd: TPlatformRewardedInterstitialAd);
  end;

  TPlatformRewardedInterstitialAd = class(TCustomPlatformRewardedInterstitialAd)
  private
    FAd: JRewardedInterstitialAd;
    FCallbackDelegate: TRewardedInterstitialAdLoadCallbackDelegate;
    FUserEarnedRewardListener: JOnUserEarnedRewardListener;
    procedure LoadAd;
  protected
    procedure AdLoaded(const ARewardedInterstitialAd: JRewardedInterstitialAd);
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdWillPresentFullScreenContent; override;
    procedure DoUserEarnedReward(const AReward: TAdReward); override;
    procedure Load; override;
  public
    constructor Create(const ARewardedInterstitialAd: TRewardedInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformAppOpenAd = class;

  TAppOpenAdLoadCallbackDelegate = class(TJavaLocal, JDWAppOpenAdLoadCallbackDelegate)
  private
    FAppOpenAd: TPlatformAppOpenAd;
    FCallback: JDWAppOpenAdLoadCallback;
  protected
    property Callback: JDWAppOpenAdLoadCallback read FCallback;
  public
    { JDWAppOpenAdLoadCallbackDelegate }
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdLoaded(ad: JAppOpenAd); cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  public
    constructor Create(const AAppOpenAd: TPlatformAppOpenAd);
  end;

  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd)
  private
    FAd: JAppOpenAd;
    FCallbackDelegate: TAppOpenAdLoadCallbackDelegate;
    function GetOrientation: Integer;
    procedure LoadAd;
    procedure ShowAdIfAvailable(const ACanShow: Boolean);
  protected
    procedure AdLoaded(const AAd: JAppOpenAd);
    procedure ApplicationBecameActive; override;
    procedure ApplicationEnteredBackground; override;
    procedure DoAdDismissedFullScreenContent; override;
    procedure DoAdFailedToLoad(const AError: TAdError); override;
    procedure DoAdFailedToShowFullScreenContent(const AError: TAdError); override;
    procedure DoAdLoaded; override;
    procedure DoAdWillPresentFullScreenContent; override;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.Helpers;

function GetAdError(const adError: JAdError): TAdError;
begin
  Result.ErrorCode := adError.getCode;
  Result.Message := JStringToString(adError.getMessage);
end;

{ TInterstitialAdCallbackDelegate }

constructor TInterstitialAdCallbackDelegate.Create(const APlatformInterstitialAd: TPlatformInterstitialAd);
begin
  inherited Create;
  FPlatformInterstitialAd := APlatformInterstitialAd;
  FCallback := TJDWInterstitialAdCallback.JavaClass.init(Self);
end;

procedure TInterstitialAdCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FPlatformInterstitialAd.DoAdDismissedFullScreenContent;
end;

procedure TInterstitialAdCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
begin
  FPlatformInterstitialAd.DoAdFailedToLoad(GetAdError(loadAdError));
end;

procedure TInterstitialAdCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
begin
  FPlatformInterstitialAd.DoAdFailedToShowFullScreenContent(GetAdError(adError));
end;

procedure TInterstitialAdCallbackDelegate.onAdLoaded(interstitialAd: JInterstitial_InterstitialAd);
begin
  FPlatformInterstitialAd.AdLoaded(interstitialAd);
end;

procedure TInterstitialAdCallbackDelegate.onAdShowedFullScreenContent;
begin
  FPlatformInterstitialAd.DoAdWillPresentFullScreenContent;
end;

{ TPlatformInterstitialAd }

constructor TPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited;
  FCallbackDelegate := TInterstitialAdCallbackDelegate.Create(Self);
end;

destructor TPlatformInterstitialAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformInterstitialAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformInterstitialAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformInterstitialAd.AdLoaded(const AInterstitialAd: JInterstitial_InterstitialAd);
begin
  FAd := AInterstitialAd;
  DoAdLoaded;
  FAd.show(TAndroidHelper.Activity);
end;

procedure TPlatformInterstitialAd.DoAdWillPresentFullScreenContent;
begin
  inherited;
end;

procedure TPlatformInterstitialAd.Load;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJinterstitial_InterstitialAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, FCallbackDelegate.Callback);
  end;
  // else ad already showing
end;

{ TUserEarnedRewardListener }

constructor TUserEarnedRewardListener.Create(const APlatformBaseRewardedAd: TCustomPlatformBaseRewardedAd);
begin
  inherited Create;
  FPlatformBaseRewardedAd := TOpenCustomPlatformBaseRewardedAd(APlatformBaseRewardedAd);
end;

procedure TUserEarnedRewardListener.onUserEarnedReward(rewardItem: JRewardItem);
var
  LReward: TAdReward;
begin
  LReward.Amount := rewardItem.getAmount;
  LReward.RewardType := JStringToString(rewardItem.getType);
  FPlatformBaseRewardedAd.DoUserEarnedReward(LReward);
end;

{ TRewardedAdLoadCallbackDelegate }

constructor TRewardedAdLoadCallbackDelegate.Create(const APlatformRewardedAd: TPlatformRewardedAd);
begin
  inherited Create;
  FPlatformRewardedAd := APlatformRewardedAd;
  FCallback := TJDWRewardedAdLoadCallback.JavaClass.init(Self);
end;

procedure TRewardedAdLoadCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FPlatformRewardedAd.DoAdDismissedFullScreenContent;
end;

procedure TRewardedAdLoadCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
begin
  FPlatformRewardedAd.DoAdFailedToLoad(GetAdError(loadAdError));
end;

procedure TRewardedAdLoadCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
begin
  FPlatformRewardedAd.DoAdFailedToShowFullScreenContent(GetAdError(adError));
end;

procedure TRewardedAdLoadCallbackDelegate.onAdLoaded(rewardedAd: JRewardedAd);
begin
  FPlatformRewardedAd.AdLoaded(rewardedAd);
end;

procedure TRewardedAdLoadCallbackDelegate.onAdShowedFullScreenContent;
begin
  FPlatformRewardedAd.DoAdWillPresentFullScreenContent;
end;

{ TPlatformRewardedAd }

constructor TPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited;
  FCallbackDelegate := TRewardedAdLoadCallbackDelegate.Create(Self);
  FUserEarnedRewardListener := TUserEarnedRewardListener.Create(Self);
end;

destructor TPlatformRewardedAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformRewardedAd.AdLoaded(const ARewardedAd: JRewardedAd);
begin
  FAd := ARewardedAd;
  FAd.show(TAndroidHelper.Activity, FUserEarnedRewardListener);
end;

procedure TPlatformRewardedAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformRewardedAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformRewardedAd.DoAdWillPresentFullScreenContent;
begin
  inherited;
end;

procedure TPlatformRewardedAd.DoUserEarnedReward(const AReward: TAdReward);
begin
  inherited;
end;

procedure TPlatformRewardedAd.Load;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJRewardedAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, FCallbackDelegate.Callback);
  end;
  // else ad already showing
end;

{ TRewardedInterstitialAdLoadCallbackDelegate }

constructor TRewardedInterstitialAdLoadCallbackDelegate.Create(const APlatformRewardedInterstitialAd: TPlatformRewardedInterstitialAd);
begin
  inherited Create;
  FPlatformRewardedInterstitialAd := APlatformRewardedInterstitialAd;
  FCallback := TJDWRewardedInterstitialAdLoadCallback.JavaClass.init(Self);
end;

procedure TRewardedInterstitialAdLoadCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FPlatformRewardedInterstitialAd.DoAdDismissedFullScreenContent;
end;

procedure TRewardedInterstitialAdLoadCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := loadAdError.getCode;
  LError.Message := JStringToString(loadAdError.getMessage);
  FPlatformRewardedInterstitialAd.DoAdFailedToLoad(LError);
end;

procedure TRewardedInterstitialAdLoadCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
begin
  FPlatformRewardedInterstitialAd.DoAdFailedToShowFullScreenContent(GetAdError(adError));
end;

procedure TRewardedInterstitialAdLoadCallbackDelegate.onAdLoaded(rewardedInterstitialAd: JRewardedInterstitialAd);
begin
  FPlatformRewardedInterstitialAd.AdLoaded(rewardedInterstitialAd);
end;

procedure TRewardedInterstitialAdLoadCallbackDelegate.onAdShowedFullScreenContent;
begin
  FPlatformRewardedInterstitialAd.DoAdWillPresentFullScreenContent;
end;

{ TPlatformRewardedInterstitialAd }

constructor TPlatformRewardedInterstitialAd.Create(const ARewardedInterstitialAd: TRewardedInterstitialAd);
begin
  inherited;
  FCallbackDelegate := TRewardedInterstitialAdLoadCallbackDelegate.Create(Self);
  FUserEarnedRewardListener := TUserEarnedRewardListener.Create(Self);
end;

destructor TPlatformRewardedInterstitialAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.AdLoaded(const ARewardedInterstitialAd: JRewardedInterstitialAd);
begin
  FAd := ARewardedInterstitialAd;
  DoAdLoaded;
  FAd.show(TAndroidHelper.Activity, FUserEarnedRewardListener);
end;

procedure TPlatformRewardedInterstitialAd.DoAdDismissedFullScreenContent;
begin
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.DoAdWillPresentFullScreenContent;
begin
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.DoUserEarnedReward(const AReward: TAdReward);
begin
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.Load;
begin
  // TPlatformMobileAds.Start(LoadAd);
  LoadAd;
end;

procedure TPlatformRewardedInterstitialAd.LoadAd;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJRewardedInterstitialAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, FCallbackDelegate.Callback);
  end;
end;

{ TAppOpenAdLoadCallbackDelegate }

constructor TAppOpenAdLoadCallbackDelegate.Create(const AAppOpenAd: TPlatformAppOpenAd);
begin
  inherited Create;
  FAppOpenAd := AAppOpenAd;
  FCallback := TJDWAppOpenAdLoadCallback.JavaClass.init(Self);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdDismissedFullScreenContent;
begin
  FAppOpenAd.DoAdDismissedFullScreenContent;
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdFailedToLoad(loadAdError: JLoadAdError);
begin
  FAppOpenAd.DoAdFailedToLoad(GetAdError(loadAdError));
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdFailedToShowFullScreenContent(adError: JAdError);
begin
  FAppOpenAd.DoAdFailedToShowFullScreenContent(GetAdError(adError));
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdLoaded(ad: JAppOpenAd);
begin
  FAppOpenAd.AdLoaded(ad);
end;

procedure TAppOpenAdLoadCallbackDelegate.onAdShowedFullScreenContent;
begin
  FAppOpenAd.DoAdWillPresentFullScreenContent;
end;

{ TPlatformAppOpenAd }

constructor TPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited;
  FCallbackDelegate := TAppOpenAdLoadCallbackDelegate.Create(Self);
end;

destructor TPlatformAppOpenAd.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformAppOpenAd.AdLoaded(const AAd: JAppOpenAd);
begin
  FAd := AAd;
  DoAdLoaded;
end;

procedure TPlatformAppOpenAd.ApplicationBecameActive;
begin
  ShowAdIfAvailable(IsWarmStart);
end;

procedure TPlatformAppOpenAd.ApplicationEnteredBackground;
begin
  //
end;

procedure TPlatformAppOpenAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdFailedToLoad(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdFailedToShowFullScreenContent(const AError: TAdError);
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdLoaded;
begin
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdWillPresentFullScreenContent;
begin
  inherited;
end;

function TPlatformAppOpenAd.GetOrientation: Integer;
begin
  if Orientation = TAppOpenAdOrientation.Landscape then
    Result := TJAppOpenAd.JavaClass.APP_OPEN_AD_ORIENTATION_LANDSCAPE
  else
    Result := TJAppOpenAd.JavaClass.APP_OPEN_AD_ORIENTATION_PORTRAIT;
end;

procedure TPlatformAppOpenAd.LoadAd;
var
  LRequest: JAdRequest;
  LAdUnitId: JString;
begin
  if FAd = nil then
  begin
    LRequest := TJAdRequest_Builder.JavaClass.init.build;
    LAdUnitId := StringToJString(AdUnitID);
    TJAppOpenAd.JavaClass.load(TAndroidHelper.Context, LAdUnitId, LRequest, GetOrientation, FCallbackDelegate.Callback);
  end;
end;

procedure TPlatformAppOpenAd.ShowAdIfAvailable(const ACanShow: Boolean);
begin
  if (FAd <> nil) and ACanShow then // and load time was less than 4 hours ago??
    FAd.show(TAndroidHelper.Activity)
  else if FAd = nil then
    LoadAd;
end;

end.
