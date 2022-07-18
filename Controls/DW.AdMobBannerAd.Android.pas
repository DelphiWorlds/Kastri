unit DW.AdMobBannerAd.Android;

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

implementation

uses
  // RTL
  System.Types, System.SysUtils,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.AdMob, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.Helpers,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Util,
  // FMX
  FMX.Presentation.Android, FMX.Controls.Model, FMX.Controls.Presentation, FMX.Presentation.Messages, FMX.Presentation.Factory,
  FMX.Controls, FMX.Types,
  // DW
  DW.Androidapi.JNI.AdMob, DW.AdMob, DW.AdMobBannerAd;

type
  JDWAdListener = interface;
  JDWAdListenerDelegate = interface;

  JDWAdListenerClass = interface(JAdListenerClass)
    ['{33674549-6BA1-412E-89A5-27A16F96B620}']
    {class} function init(delegate: JDWAdListenerDelegate): JDWAdListener; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAdListener')]
  JDWAdListener = interface(JAdListener)
    ['{16A28A07-7667-447C-9D2D-4F54A58063B8}']
  end;
  TJDWAdListener = class(TJavaGenericImport<JDWAdListenerClass, JDWAdListener>) end;

  JDWAdListenerDelegateClass = interface(IJavaClass)
    ['{ED5632F4-BCBB-4EE8-859B-F3D773149AC4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWAdListenerDelegate')]
  JDWAdListenerDelegate = interface(IJavaInstance)
    ['{345A1684-3CEF-4567-B6B9-E3B5C9883AE5}']
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(adError: JAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  end;
  TJDWAdListenerDelegate = class(TJavaGenericImport<JDWAdListenerDelegateClass, JDWAdListenerDelegate>) end;

  TAndroidAdMobBannerAd = class;

  TAdListenerDelegate = class(TJavaLocal, JDWAdListenerDelegate)
  private
    FAdMobBannerAd: TAndroidAdMobBannerAd;
    FListener: JAdListener;
  protected
    property Listener: JAdListener read FListener;
  public
    { JDWAdListener }
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(adError: JAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  public
    constructor Create(const AAdMobBannerAd: TAndroidAdMobBannerAd);
  end;

  TOpenAdMobBannerAd = class(TCustomAdMobBannerAd);

  TAndroidAdMobBannerAd = class(TAndroidNativeView)
  private
    FAdView: JAdView;
    FListenerDelegate: TAdListenerDelegate;
    FNeedsRecreate: Boolean;
    FView: JViewGroup;
    function AdControl: TOpenAdMobBannerAd;
    procedure CreateAdView;
    procedure DestroyAdView;
    function GetAdaptiveAdSize: JAdSize;
    function GetAdSize: JAdSize;
    function GetModel: TCustomAdMobBannerAdModel;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    procedure AdClicked;
    procedure AdClosed;
    procedure AdFailedToLoad(const AAdError: JAdError);
    procedure AdImpression;
    procedure AdLoaded;
    procedure AdOpened;
    procedure MMAdSizeChanged(var AMessage: TDispatchMessage); message MM_BANNER_ADSIZE_CHANGED;
    procedure MMAdUnitIdChanged(var AMessage: TDispatchMessage); message MM_BANNER_ADUNITID_CHANGED;
    procedure MMLoadAd(var AMessage: TDispatchMessage); message MM_BANNER_LOADAD;
    procedure MMTestModeChanged(var AMessage: TDispatchMessage); message MM_BANNER_TESTMODE_CHANGED;
    procedure PMGetAdjustedSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_ADJUST_SIZE;
    procedure PMGetAdjustedType(var AMessage: TDispatchMessageWithValue<TAdjustType>); message PM_GET_ADJUST_TYPE;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomAdMobBannerAdModel read GetModel;
  end;

{ TAdListenerDelegate }

constructor TAdListenerDelegate.Create(const AAdMobBannerAd: TAndroidAdMobBannerAd);
begin
  inherited Create;
  FListener := TJDWAdListener.JavaClass.init(Self);
  FAdMobBannerAd := AAdMobBannerAd;
end;

procedure TAdListenerDelegate.onAdClicked;
begin
  FAdMobBannerAd.AdClicked;
end;

procedure TAdListenerDelegate.onAdClosed;
begin
  FAdMobBannerAd.AdClosed;
end;

procedure TAdListenerDelegate.onAdFailedToLoad(adError: JAdError);
begin
  FAdMobBannerAd.AdFailedToLoad(adError);
end;

procedure TAdListenerDelegate.onAdImpression;
begin
  FAdMobBannerAd.AdImpression;
end;

procedure TAdListenerDelegate.onAdLoaded;
begin
  FAdMobBannerAd.AdLoaded;
end;

procedure TAdListenerDelegate.onAdOpened;
begin
  FAdMobBannerAd.AdOpened;
end;

{ TAndroidAdMobBannerAd }

constructor TAndroidAdMobBannerAd.Create;
begin
  inherited;
  FListenerDelegate := TAdListenerDelegate.Create(Self);
end;

destructor TAndroidAdMobBannerAd.Destroy;
begin
  FListenerDelegate.Free;
  inherited;
end;

procedure TAndroidAdMobBannerAd.CreateAdView;
var
  LLayoutParams: JRelativeLayout_LayoutParams;
  LAdSize: JAdSize;
begin
  LAdSize := GetAdSize;
  LLayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
    TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FAdView := TJAdView.JavaClass.init(TAndroidHelper.Context);
  FAdView.setAdSize(LAdSize);
  FView.addView(FAdView, LLayoutParams);
  if Model.TestMode then
    FAdView.setAdUnitID(StringToJString(cTestAdUnitIdBanner))
  else
    FAdView.setAdUnitID(StringToJString(Model.AdUnitID));
  FAdView.setAdListener(FListenerDelegate.Listener);
  FNeedsRecreate := False;
end;

procedure TAndroidAdMobBannerAd.DestroyAdView;
begin
  if FAdView <> nil then
    FView.removeView(FAdView);
  FAdView := nil;
end;

function TAndroidAdMobBannerAd.CreateView: JView;
begin
  FView := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  Result := FView;
end;

function TAndroidAdMobBannerAd.DefineModelClass: TDataModelClass;
begin
  Result := TCustomAdMobBannerAdModel;
end;

function TAndroidAdMobBannerAd.AdControl: TOpenAdMobBannerAd;
begin
  Result := TOpenAdMobBannerAd(Control);
end;

function TAndroidAdMobBannerAd.GetModel: TCustomAdMobBannerAdModel;
begin
  Result := inherited GetModel<TCustomAdMobBannerAdModel>;
end;

function TAndroidAdMobBannerAd.GetAdaptiveAdSize: JAdSize;
var
  LMetrics: JDisplayMetrics;
  LAdWidth: Integer;
begin
  LMetrics := TJDisplayMetrics.JavaClass.init;
  TAndroidHelper.Activity.getWindowManager.getDefaultDisplay.getMetrics(LMetrics);
  LAdWidth := Trunc(LMetrics.widthPixels / LMetrics.density);
  Result := TJAdSize.JavaClass.getCurrentOrientationAnchoredAdaptiveBannerAdSize(TAndroidHelper.Context, LAdWidth);
end;

function TAndroidAdMobBannerAd.GetAdSize: JAdSize;
begin
  case Model.AdSize of
    TAdMobBannerAdSize.Banner:
      Result := TJAdSize.JavaClass.BANNER;
    TAdMobBannerAdSize.LargeBanner:
      Result := TJAdSize.JavaClass.LARGE_BANNER;
    TAdMobBannerAdSize.MediumRectangle:
      Result := TJAdSize.JavaClass.MEDIUM_RECTANGLE;
    TAdMobBannerAdSize.FullBanner:
      Result := TJAdSize.JavaClass.FULL_BANNER;
    TAdMobBannerAdSize.LeaderBoard:
      Result := TJAdSize.JavaClass.LEADERBOARD;
    TAdMobBannerAdSize.Adaptive:
      Result := GetAdaptiveAdSize;
  else
    Result := TJAdSize.JavaClass.BANNER;
  end;
end;

procedure TAndroidAdMobBannerAd.MMAdSizeChanged(var AMessage: TDispatchMessage);
begin
  FNeedsRecreate := True;
end;

procedure TAndroidAdMobBannerAd.MMAdUnitIdChanged(var AMessage: TDispatchMessage);
begin
  FNeedsRecreate := True;
end;

procedure TAndroidAdMobBannerAd.MMLoadAd(var AMessage: TDispatchMessage);
begin
  if FNeedsRecreate then
    DestroyAdView;
  if FAdView = nil then
    CreateAdView;
  FAdView.loadAd(TJAdRequest_Builder.JavaClass.init.build);
end;

procedure TAndroidAdMobBannerAd.MMTestModeChanged(var AMessage: TDispatchMessage);
begin
  FNeedsRecreate := True;
end;

procedure TAndroidAdMobBannerAd.PMGetAdjustedSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
var
  LAdSize: JAdSize;
begin
  LAdSize := GetAdSize;
  AMessage.Value := TSizeF.Create(LAdSize.getWidth, LAdSize.getHeight);
end;

procedure TAndroidAdMobBannerAd.PMGetAdjustedType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  AMessage.Value := TAdjustType.FixedSize;
end;

procedure TAndroidAdMobBannerAd.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  //
end;

procedure TAndroidAdMobBannerAd.AdClicked;
begin
  AdControl.DoAdClicked;
end;

procedure TAndroidAdMobBannerAd.AdClosed;
begin
  AdControl.DoAdClosed;
end;

procedure TAndroidAdMobBannerAd.AdFailedToLoad(const AAdError: JAdError);
var
  LError: TAdError;
begin
  LError.ErrorCode := AAdError.getCode;
  LError.Message := JStringToString(AAdError.getMessage);
  AdControl.DoAdFailedToLoad(LError);
end;

procedure TAndroidAdMobBannerAd.AdImpression;
begin
  AdControl.DoAdImpression;
end;

procedure TAndroidAdMobBannerAd.AdLoaded;
begin
  AdControl.DoAdLoaded;
end;

procedure TAndroidAdMobBannerAd.AdOpened;
begin
  AdControl.DoAdOpened;
end;

initialization
  TPresentationProxyFactory.Current.Register(TAdMobBannerAd, TControlType.Platform, TAndroidPresentationProxy<TAndroidAdMobBannerAd>);

finalization
  TPresentationProxyFactory.Current.Unregister(TAdMobBannerAd, TControlType.Platform, TAndroidPresentationProxy<TAndroidAdMobBannerAd>);

end.
