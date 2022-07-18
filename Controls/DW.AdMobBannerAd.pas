unit DW.AdMobBannerAd;

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
  System.Classes, System.Types, System.UITypes,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.Controls, FMX.Graphics, FMX.Types,
  // DW
  DW.AdMob;

const
  MM_BANNER_LOADAD = MM_USER + 1;
  MM_BANNER_ADSIZE_CHANGED = MM_USER + 2;
  MM_BANNER_ADUNITID_CHANGED = MM_USER + 3;
  MM_BANNER_TESTMODE_CHANGED = MM_USER + 4;

type
  TAdMobBannerAdSize = (Banner, LargeBanner, MediumRectangle, FullBanner, Leaderboard, Adaptive);

  TCustomAdMobBannerAdModel = class(TDataModel)
  private
    FAdSize: TAdMobBannerAdSize;
    FAdUnitID: string;
    FTestMode: Boolean;
    procedure SetAdSize(const Value: TAdMobBannerAdSize);
    procedure SetAdUnitID(const Value: string);
    procedure SetTestMode(const Value: Boolean);
  public
    procedure LoadAd;
    property AdSize: TAdMobBannerAdSize read FAdSize write SetAdSize;
    property AdUnitID: string read FAdUnitID write SetAdUnitID;
    property TestMode: Boolean read FTestMode write SetTestMode;
  end;

  TCustomAdMobBannerAd = class(TPresentedControl)
  private
    FOnAdClicked: TNotifyEvent;
    FOnAdClosed: TNotifyEvent;
    FOnAdFailedToLoad: TAdErrorEvent;
    FOnAdImpression: TNotifyEvent;
    FOnAdLoaded: TNotifyEvent;
    FOnAdOpened: TNotifyEvent;
    function GetAdSize: TAdMobBannerAdSize;
    function GetAdUnitID: string;
    function GetModel: TCustomAdMobBannerAdModel; overload;
    function GetTestMode: Boolean;
    procedure SetAdSize(const Value: TAdMobBannerAdSize);
    procedure SetAdUnitID(const Value: string);
    procedure SetTestMode(const Value: Boolean);
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure DoAdClicked;
    procedure DoAdClosed;
    procedure DoAdFailedToLoad(const AError: TAdError);
    procedure DoAdImpression;
    procedure DoAdLoaded;
    procedure DoAdOpened;
    procedure Paint; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadAd;
    property AdSize: TAdMobBannerAdSize read GetAdSize write SetAdSize;
    property AdUnitID: string read GetAdUnitID write SetAdUnitID;
    property Model: TCustomAdMobBannerAdModel read GetModel;
    property TestMode: Boolean read GetTestMode write SetTestMode default False;
    property OnAdClicked: TNotifyEvent read FOnAdClicked write FOnAdClicked;
    property OnAdClosed: TNotifyEvent read FOnAdClosed write FOnAdClosed;
    property OnAdFailedToLoad: TAdErrorEvent read FOnAdFailedToLoad write FOnAdFailedToLoad;
    property OnAdImpression: TNotifyEvent read FOnAdImpression write FOnAdImpression;
    property OnAdLoaded: TNotifyEvent read FOnAdLoaded write FOnAdLoaded;
    property OnAdOpened: TNotifyEvent read FOnAdOpened write FOnAdOpened;
  end;

  {$IF CompilerVersion >= 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm)]
  {$ENDIF}
  TAdMobBannerAd = class(TCustomAdMobBannerAd)
  published
    property AdSize;
    property AdUnitID;
    property Align;
    property Anchors;
    property Height;
    property Margins;
    property Position;
    property Size;
    property TestMode;
    property Visible default True;
    property Width;
    property OnAdClicked;
    property OnAdClosed;
    property OnAdFailedToLoad;
    property OnAdImpression;
    property OnAdLoaded;
    property OnAdOpened;
  end;

procedure Register;

implementation

uses
  // DW
  {$IF Defined(IOS)}
  DW.AdMobBannerAd.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.AdMobBannerAd.Android,
  {$ENDIF}
  // RTL
  System.SysUtils, System.IOUtils,
  // FMX
  FMX.Consts;

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TAdMobBannerAd]);
end;

{ TCustomAdMobBannerAdModel }

procedure TCustomAdMobBannerAdModel.LoadAd;
begin
  SendMessage(MM_BANNER_LOADAD);
end;

procedure TCustomAdMobBannerAdModel.SetAdSize(const Value: TAdMobBannerAdSize);
begin
  if Value <> FAdSize then
  begin
    FAdSize := Value;
    SendMessage(MM_BANNER_ADSIZE_CHANGED);
  end;
end;

procedure TCustomAdMobBannerAdModel.SetAdUnitID(const Value: string);
begin
  if Value <> FAdUnitID then
  begin
    FAdUnitID := Value;
    SendMessage(MM_BANNER_ADUNITID_CHANGED);
  end;
end;

procedure TCustomAdMobBannerAdModel.SetTestMode(const Value: Boolean);
begin
  if Value <> FTestMode then
  begin
    FTestMode := Value;
    SendMessage(MM_BANNER_TESTMODE_CHANGED);
  end;
end;

{ TCustomAdMobBannerAd }

constructor TCustomAdMobBannerAd.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
  //
end;

function TCustomAdMobBannerAd.DefineModelClass: TDataModelClass;
begin
  Result := TCustomAdMobBannerAdModel;
end;

function TCustomAdMobBannerAd.GetAdSize: TAdMobBannerAdSize;
begin
  Result := Model.AdSize;
end;

function TCustomAdMobBannerAd.GetAdUnitID: string;
begin
  Result := Model.AdUnitID;
end;

function TCustomAdMobBannerAd.GetModel: TCustomAdMobBannerAdModel;
begin
  Result := inherited GetModel<TCustomAdMobBannerAdModel>;
end;

function TCustomAdMobBannerAd.GetTestMode: Boolean;
begin
  Result := Model.TestMode;
end;

procedure TCustomAdMobBannerAd.LoadAd;
begin
  Model.LoadAd;
end;

procedure TCustomAdMobBannerAd.Paint;
begin
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

function TCustomAdMobBannerAd.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TCustomAdMobBannerAd.SetAdSize(const Value: TAdMobBannerAdSize);
begin
  Model.AdSize := Value;
end;

procedure TCustomAdMobBannerAd.SetAdUnitID(const Value: string);
begin
  Model.AdUnitID := Value;
end;

procedure TCustomAdMobBannerAd.SetTestMode(const Value: Boolean);
begin
  Model.TestMode := Value;
end;

procedure TCustomAdMobBannerAd.DoAdClicked;
begin
  if Assigned(FOnAdClicked) then
    FOnAdClicked(Self);
end;

procedure TCustomAdMobBannerAd.DoAdClosed;
begin
  if Assigned(FOnAdClosed) then
    FOnAdClosed(Self);
end;

procedure TCustomAdMobBannerAd.DoAdFailedToLoad(const AError: TAdError);
begin
  if Assigned(FOnAdFailedToLoad) then
    FOnAdFailedToLoad(Self, AError);
end;

procedure TCustomAdMobBannerAd.DoAdImpression;
begin
  if Assigned(FOnAdImpression) then
    FOnAdImpression(Self);
end;

procedure TCustomAdMobBannerAd.DoAdLoaded;
begin
  if Assigned(FOnAdLoaded) then
    FOnAdLoaded(Self);
end;

procedure TCustomAdMobBannerAd.DoAdOpened;
begin
  if Assigned(FOnAdOpened) then
    FOnAdOpened(Self);
end;

end.
