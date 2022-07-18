unit DW.AdMobBannerAd.iOS;

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
  System.TypInfo, System.Types,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.Helpers,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model, FMX.Types,
  // DW
  DW.iOSapi.GoogleMobileAds, DW.AdMob, DW.AdMobBannerAd;

type
  IGADBannerView = interface(UIView)
    ['{D46FDA3D-B11A-4B0D-861C-169655ADBEBE}']
  end;

  TiOSAdMobBannerAd = class;

  TGADBannerViewDelegate = class(TOCLocal, GADBannerViewDelegate)
  private
    FBannerAd: TiOSAdMobBannerAd;
  public
    { GADBannerViewDelegate }
    procedure bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError); cdecl;
    procedure bannerViewDidDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordClick(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordImpression(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillPresentScreen(bannerView: GADBannerView); cdecl;
  public
    constructor Create(const ABannerAd: TiOSAdMobBannerAd);
  end;

  TOpenAdMobBannerAd = class(TCustomAdMobBannerAd);

  TiOSAdMobBannerAd = class(TiOSNativeControl)
  private
    FBannerView: GADBannerView;
    FDelegate: TGADBannerViewDelegate;
    function AdControl: TOpenAdMobBannerAd;
    procedure AdUnitIdChanged;
    procedure AdSizeChanged;
    function GetAdSize: GADAdSize;
    function GetModel: TCustomAdMobBannerAdModel;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    function DefineModelClass: TDataModelClass; override;
    procedure AdClicked;
    procedure AdClosed;
    procedure AdFailedToLoad(const AAdError: NSError);
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

{ TGADBannerViewDelegate }

constructor TGADBannerViewDelegate.Create(const ABannerAd: TiOSAdMobBannerAd);
begin
  inherited Create;
  FBannerAd := ABannerAd;
end;

procedure TGADBannerViewDelegate.bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError);
begin
  FBannerAd.AdFailedToLoad(didFailToReceiveAdWithError);
end;

procedure TGADBannerViewDelegate.bannerViewDidDismissScreen(bannerView: GADBannerView);
begin
  FBannerAd.AdClosed;
end;

procedure TGADBannerViewDelegate.bannerViewDidReceiveAd(bannerView: GADBannerView);
begin
  FBannerAd.AdLoaded;
end;

procedure TGADBannerViewDelegate.bannerViewDidRecordClick(bannerView: GADBannerView);
begin
  FBannerAd.AdClicked;
end;

procedure TGADBannerViewDelegate.bannerViewDidRecordImpression(bannerView: GADBannerView);
begin
  FBannerAd.AdImpression;
end;

procedure TGADBannerViewDelegate.bannerViewWillDismissScreen(bannerView: GADBannerView);
begin
  // Not implemented
end;

procedure TGADBannerViewDelegate.bannerViewWillPresentScreen(bannerView: GADBannerView);
begin
  FBannerAd.AdOpened;
end;

{ TiOSAdMobBannerAd }

constructor TiOSAdMobBannerAd.Create;
var
  LConstraint: NSLayoutConstraint;
begin
  inherited;
  FBannerView := TGADBannerView.Create;
  FBannerView.initWithAdSize(GADAdSizeFromCGSize(CGSizeMake(0, 0)));
  FBannerView.setTranslatesAutoresizingMaskIntoConstraints(False);
  FDelegate := TGADBannerViewDelegate.Create(Self);
  FBannerView.setDelegate(FDelegate.GetObjectID);
  FBannerView.setRootViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController);
  View.addSubview(FBannerView);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FBannerView),
    NSLayoutAttributeTop, NSLayoutRelationEqual, NSObjectToID(View), NSLayoutAttributeTop, 1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FBannerView),
    NSLayoutAttributeBottom, NSLayoutRelationEqual, NSObjectToID(View), NSLayoutAttributeBottom, 1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FBannerView),
    NSLayoutAttributeLeft, NSLayoutRelationEqual, NSObjectToID(View), NSLayoutAttributeLeft, 1, 0));
  LConstraint.setActive(True);
  LConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FBannerView),
    NSLayoutAttributeRight, NSLayoutRelationEqual, NSObjectToID(View), NSLayoutAttributeRight, 1, 0));
  LConstraint.setActive(True);
  View.updateConstraints;
end;

destructor TiOSAdMobBannerAd.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TiOSAdMobBannerAd.AdControl: TOpenAdMobBannerAd;
begin
  Result := TOpenAdMobBannerAd(Control);
end;

procedure TiOSAdMobBannerAd.AdClicked;
begin
  AdControl.DoAdClicked;
end;

procedure TiOSAdMobBannerAd.AdClosed;
begin
  AdControl.DoAdClosed;
end;

procedure TiOSAdMobBannerAd.AdFailedToLoad(const AAdError: NSError);
var
  LError: TAdError;
begin
  LError.ErrorCode := AAdError.code;
  LError.Message := NSStrToStr(AAdError.localizedDescription);
  AdControl.DoAdFailedToLoad(LError);
end;

procedure TiOSAdMobBannerAd.AdImpression;
begin
  AdControl.DoAdImpression;
end;

procedure TiOSAdMobBannerAd.AdLoaded;
begin
  AdControl.DoAdLoaded;
end;

procedure TiOSAdMobBannerAd.AdOpened;
begin
  AdControl.DoAdOpened;
end;

function TiOSAdMobBannerAd.DefineModelClass: TDataModelClass;
begin
  Result := TCustomAdMobBannerAdModel;
end;

function TiOSAdMobBannerAd.GetAdSize: GADAdSize;
begin
  case Model.AdSize of
    TAdMobBannerAdSize.Banner:
      Result := kGADAdSizeBanner;
    TAdMobBannerAdSize.FullBanner:
      Result := kGADAdSizeFullBanner;
    TAdMobBannerAdSize.LargeBanner:
      Result := kGADAdSizeLargeBanner;
    TAdMobBannerAdSize.LeaderBoard:
      Result := kGADAdSizeLeaderboard;
    TAdMobBannerAdSize.MediumRectangle:
      Result := kGADAdSizeMediumRectangle;
    TAdMobBannerAdSize.Adaptive:
      Result := GADCurrentOrientationAnchoredAdaptiveBannerAdSizeWithWidth(ParentView.frame.Size.Width);
  else
    Result := kGADAdSizeBanner;
  end;
  Result := GADAdSizeFromCGSize(Result.size);
end;

function TiOSAdMobBannerAd.GetModel: TCustomAdMobBannerAdModel;
begin
  Result := inherited GetModel<TCustomAdMobBannerAdModel>;
end;

function TiOSAdMobBannerAd.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IGADBannerView);
end;

procedure TiOSAdMobBannerAd.AdUnitIdChanged;
begin
  if Model.TestMode then
    FBannerView.setAdUnitID(StrToNSStr(cTestAdUnitIdBanner))
  else
    FBannerView.setAdUnitID(StrToNSStr(Model.AdUnitID));
end;

procedure TiOSAdMobBannerAd.AdSizeChanged;
var
  LAdSize: GADAdSize;
begin
  LAdSize := GetAdSize;
  Control.Size.Size := TSizeF.Create(LAdSize.size.width, LAdSize.size.height);
  View.layoutSubviews;
  FBannerView.setNeedsLayout;
  FBannerView.layoutIfNeeded;
  FBannerView.setAdSize(LAdSize);
end;

procedure TiOSAdMobBannerAd.MMAdSizeChanged(var AMessage: TDispatchMessage);
begin
  AdSizeChanged;
end;

procedure TiOSAdMobBannerAd.MMAdUnitIdChanged(var AMessage: TDispatchMessage);
begin
  AdUnitIdChanged;
end;

procedure TiOSAdMobBannerAd.MMLoadAd(var AMessage: TDispatchMessage);
begin
  FBannerView.loadRequest(TGADRequest.Create);
end;

procedure TiOSAdMobBannerAd.MMTestModeChanged(var AMessage: TDispatchMessage);
begin
  AdUnitIdChanged;
end;

procedure TiOSAdMobBannerAd.PMGetAdjustedSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  AMessage.Value := GetAdSize.size.ToSizeF;
end;

procedure TiOSAdMobBannerAd.PMGetAdjustedType(var AMessage: TDispatchMessageWithValue<TAdjustType>);
begin
  AMessage.Value := TAdjustType.FixedSize;
end;

procedure TiOSAdMobBannerAd.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  //
end;

initialization
  TPresentationProxyFactory.Current.Register(TAdMobBannerAd, TControlType.Platform, TiOSPresentationProxy<TiOSAdMobBannerAd>);

finalization
  TPresentationProxyFactory.Current.Unregister(TAdMobBannerAd, TControlType.Platform, TiOSPresentationProxy<TiOSAdMobBannerAd>);

end.
