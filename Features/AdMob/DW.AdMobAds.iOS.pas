unit DW.AdMobAds.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation,
  // DW
  DW.iOSapi.GoogleMobileAds, DW.AdMob, DW.AdMobAds;

type
  TOpenCustomPlatformFullScreenAd = class(TCustomPlatformFullScreenAd);

  TFullScreenContentDelegate = class(TOCLocal, GADFullScreenContentDelegate)
  private
    FCustomPlatformFullScreenAd: TOpenCustomPlatformFullScreenAd;
  public
    { GADFullScreenContentDelegate }
    procedure ad(ad: Pointer; didFailToPresentFullScreenContentWithError: NSError); cdecl;
    procedure adDidDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adDidPresentFullScreenContent(ad: Pointer); cdecl;
    procedure adDidRecordClick(ad: Pointer); cdecl;
    procedure adDidRecordImpression(ad: Pointer); cdecl;
    procedure adWillDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adWillPresentFullScreenContent(ad: Pointer); cdecl;
  public
    constructor Create(const ACustomPlatformFullScreenAd: TCustomPlatformFullScreenAd);
  end;

  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd)
  private
    FAd: GADInterstitialAd;
    FDelegate: TFullScreenContentDelegate;
    procedure AdLoadCompletionHandler(interstitialAd: GADInterstitialAd; error: NSError);
    procedure PaidEventHandler(value: GADAdValue);
   protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedAd = class(TCustomPlatformRewardedAd)
  private
    FAd: GADRewardedAd;
    FDelegate: TFullScreenContentDelegate;
    procedure AdLoadCompletionHandler(rewardedAd: GADRewardedAd; error: NSError);
    procedure PaidEventHandler(value: GADAdValue);
    procedure UserDidEarnRewardHandler;
  protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const ARewardedAd: TRewardedAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedInterstitialAd = class(TCustomPlatformRewardedInterstitialAd)
  private
    FAd: GADRewardedInterstitialAd;
    FDelegate: TFullScreenContentDelegate;
    procedure AdLoadCompletionHandler(rewardedInterstitialAd: GADRewardedInterstitialAd; error: NSError);
    procedure LoadAd;
    procedure PaidEventHandler(value: GADAdValue);
    procedure UserDidEarnRewardHandler;
  protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const ARewardedInterstitialAd: TRewardedInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd)
  private
    FAd: GADAppOpenAd;
    FDelegate: TFullScreenContentDelegate;
    procedure AdLoadCompletionHandler(appOpenAd: GADAppOpenAd; error: NSError);
    function GetOrientation: UIInterfaceOrientation;
    procedure LoadAd;
    procedure PaidEventHandler(value: GADAdValue);
    procedure ShowAdIfAvailable(const ACanShow: Boolean);
  protected
    procedure ApplicationBecameActive; override;
    procedure ApplicationEnteredBackground; override;
    procedure DoAdDismissedFullScreenContent; override;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers;

type
  TGADHelper = record
  public
    class function GetRequest: GADRequest; static;
    class function GetAdError(const AError: NSError): TADError; static;
    class function GetAdReward(const AReward: GADAdReward): TADReward; static;
  end;

{ TGADHelper }

class function TGADHelper.GetAdError(const AError: NSError): TADError;
begin
  Result.ErrorCode := AError.code;
  Result.Message := NSStrToStr(AError.localizedDescription);
end;

class function TGADHelper.GetAdReward(const AReward: GADAdReward): TADReward;
begin
  Result.Amount := AReward.amount.intValue;
  Result.RewardType := NSStrToStr(AReward.&type);
end;

class function TGADHelper.GetRequest: GADRequest;
begin
  Result := TGADRequest.Wrap(TGADRequest.OCClass.request);
end;

{ TFullScreenContentDelegate }

constructor TFullScreenContentDelegate.Create(const ACustomPlatformFullScreenAd: TCustomPlatformFullScreenAd);
begin
  inherited Create;
  FCustomPlatformFullScreenAd := TOpenCustomPlatformFullScreenAd(ACustomPlatformFullScreenAd);
end;

procedure TFullScreenContentDelegate.ad(ad: Pointer; didFailToPresentFullScreenContentWithError: NSError);
begin
  TOSLog.d('TFullScreenContentDelegate.ad');
  FCustomPlatformFullScreenAd.DoAdFailedToShowFullScreenContent(TGADHelper.GetAdError(didFailToPresentFullScreenContentWithError));
end;

procedure TFullScreenContentDelegate.adDidDismissFullScreenContent(ad: Pointer);
begin
  TOSLog.d('TFullScreenContentDelegate.adDidDismissFullScreenContent');
  FCustomPlatformFullScreenAd.DoAdDismissedFullScreenContent;
end;

procedure TFullScreenContentDelegate.adDidPresentFullScreenContent(ad: Pointer);
begin
  // Deprecated
end;

procedure TFullScreenContentDelegate.adDidRecordClick(ad: Pointer);
begin
  // Not implemented
end;

procedure TFullScreenContentDelegate.adDidRecordImpression(ad: Pointer);
begin
  // Not implemented
end;

procedure TFullScreenContentDelegate.adWillDismissFullScreenContent(ad: Pointer);
begin
  // Not implemented
end;

procedure TFullScreenContentDelegate.adWillPresentFullScreenContent(ad: Pointer);
begin
  FCustomPlatformFullScreenAd.DoAdWillPresentFullScreenContent;
end;

{ TPlatformInterstitialAd }

constructor TPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited;
  FDelegate := TFullScreenContentDelegate.Create(Self);
end;

destructor TPlatformInterstitialAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformInterstitialAd.AdLoadCompletionHandler(interstitialAd: GADInterstitialAd; error: NSError);
begin
  if error = nil then
  begin
    TOSLog.d('TPlatformInterstitialAd.AdLoadCompletionHandler(ok)');
    FAd := interstitialAd;
    FAd.retain;
    FAd.setFullScreenContentDelegate(FDelegate.GetObjectID);
    FAd.setPaidEventHandler(PaidEventHandler);
    DoAdLoaded;
    FAd.presentFromRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController);
  end
  else
  begin
    TOSLog.d('TPlatformInterstitialAd.AdLoadCompletionHandler(error)');
    DoAdFailedToLoad(TGADHelper.GetAdError(error));
  end;
end;

procedure TPlatformInterstitialAd.DoAdDismissedFullScreenContent;
begin
  TOSLog.d('TPlatformInterstitialAd.DoAdDismissedFullScreenContent');
  FAd.release;
  FAd := nil;
  inherited;
end;

procedure TPlatformInterstitialAd.Load;
begin
  TGADInterstitialAd.OCClass.loadWithAdUnitID(StrToNSStr(AdUnitID), TGADHelper.GetRequest, AdLoadCompletionHandler);
end;

procedure TPlatformInterstitialAd.PaidEventHandler(value: GADAdValue);
begin
  TOSLog.d('TPlatformInterstitialAd.PaidEventHandler');
  // Future implementation
end;

{ TPlatformRewardedAd }

constructor TPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited;
  FDelegate := TFullScreenContentDelegate.Create(Self);
end;

destructor TPlatformRewardedAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformRewardedAd.AdLoadCompletionHandler(rewardedAd: GADRewardedAd; error: NSError);
begin
  if error = nil then
  begin
    FAd := rewardedAd;
    FAd.retain;
    FAd.setFullScreenContentDelegate(FDelegate.GetObjectID);
    FAd.setPaidEventHandler(PaidEventHandler);
    DoAdLoaded;
    FAd.presentFromRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController, UserDidEarnRewardHandler);
  end
  else
    DoAdFailedToLoad(TGADHelper.GetAdError(error));
end;

procedure TPlatformRewardedAd.DoAdDismissedFullScreenContent;
begin
  FAd.release;
  FAd := nil;
  inherited;
end;

procedure TPlatformRewardedAd.Load;
begin
  TGADRewardedAd.OCClass.loadWithAdUnitID(StrToNSStr(AdUnitID), TGADHelper.GetRequest, AdLoadCompletionHandler);
end;

procedure TPlatformRewardedAd.PaidEventHandler(value: GADAdValue);
begin
  // Future implementation
end;

procedure TPlatformRewardedAd.UserDidEarnRewardHandler;
begin
  TOSLog.d('TPlatformRewardedAd.UserDidEarnRewardHandler');
  DoUserEarnedReward(TGADHelper.GetAdReward(FAd.adReward));
end;

{ TPlatformRewardedInterstitialAd }

constructor TPlatformRewardedInterstitialAd.Create(const ARewardedInterstitialAd: TRewardedInterstitialAd);
begin
  inherited;
  FDelegate := TFullScreenContentDelegate.Create(Self);
end;

destructor TPlatformRewardedInterstitialAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.DoAdDismissedFullScreenContent;
begin
  FAd.release;
  FAd := nil;
  inherited;
end;

procedure TPlatformRewardedInterstitialAd.AdLoadCompletionHandler(rewardedInterstitialAd: GADRewardedInterstitialAd; error: NSError);
begin
  if error = nil then
  begin
    FAd := rewardedInterstitialAd;
    FAd.retain;
    FAd.setFullScreenContentDelegate(FDelegate.GetObjectID);
    FAd.setPaidEventHandler(PaidEventHandler);
    DoAdLoaded;
    FAd.presentFromRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController, UserDidEarnRewardHandler);
  end
  else
    DoAdFailedToLoad(TGADHelper.GetAdError(error));
end;

procedure TPlatformRewardedInterstitialAd.Load;
begin
  LoadAd;
end;

procedure TPlatformRewardedInterstitialAd.LoadAd;
begin
  TGADRewardedInterstitialAd.OCClass.loadWithAdUnitID(StrToNSStr(AdUnitID), TGADHelper.GetRequest, AdLoadCompletionHandler);
end;

procedure TPlatformRewardedInterstitialAd.PaidEventHandler(value: GADAdValue);
begin
  // Future implementation
end;

procedure TPlatformRewardedInterstitialAd.UserDidEarnRewardHandler;
begin
  DoUserEarnedReward(TGADHelper.GetAdReward(FAd.adReward));
end;

{ TPlatformAppOpenAd }

constructor TPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited;
  FDelegate := TFullScreenContentDelegate.Create(Self);
end;

destructor TPlatformAppOpenAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformAppOpenAd.DoAdDismissedFullScreenContent;
begin
  FAd := nil;
  inherited;
end;

procedure TPlatformAppOpenAd.ApplicationBecameActive;
begin
  ShowAdIfAvailable(IsWarmStart);
end;

procedure TPlatformAppOpenAd.ApplicationEnteredBackground;
begin
  //
end;

function TPlatformAppOpenAd.GetOrientation: UIInterfaceOrientation;
begin
  if Orientation = TAppOpenAdOrientation.Landscape then
    Result := UIInterfaceOrientationLandscapeLeft
  else
    Result := UIInterfaceOrientationPortrait;
end;

procedure TPlatformAppOpenAd.AdLoadCompletionHandler(appOpenAd: GADAppOpenAd; error: NSError);
begin
  if error = nil then
  begin
    FAd := appOpenAd;
    FAd.retain;
    FAd.setFullScreenContentDelegate(FDelegate.GetObjectID);
    FAd.setPaidEventHandler(PaidEventHandler);
    DoAdLoaded;
  end
  else
    DoAdFailedToLoad(TGADHelper.GetAdError(error));
end;

procedure TPlatformAppOpenAd.LoadAd;
begin
  TGADAppOpenAd.OCClass.loadWithAdUnitID(StrToNSStr(AdUnitID), TGADHelper.GetRequest, GetOrientation, AdLoadCompletionHandler);
end;

procedure TPlatformAppOpenAd.PaidEventHandler(value: GADAdValue);
begin
  // Future implementation
end;

procedure TPlatformAppOpenAd.ShowAdIfAvailable(const ACanShow: Boolean);
begin
  if (FAd <> nil) and ACanShow then // and load time was less than 4 hours ago??
    FAd.presentFromRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController)
  else if FAd = nil then
    LoadAd;
end;

end.
