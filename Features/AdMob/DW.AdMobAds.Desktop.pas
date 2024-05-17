unit DW.AdMobAds.Desktop;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  DW.AdMob, DW.AdMobAds;

type
  TPlatformInterstitialAd = class(TCustomPlatformInterstitialAd)
  private
   protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const AInterstitialAd: TInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedAd = class(TCustomPlatformRewardedAd)
  private
  protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const ARewardedAd: TRewardedAd); override;
    destructor Destroy; override;
  end;

  TPlatformRewardedInterstitialAd = class(TCustomPlatformRewardedInterstitialAd)
  private
  protected
    procedure DoAdDismissedFullScreenContent; override;
    procedure Load; override;
  public
    constructor Create(const ARewardedInterstitialAd: TRewardedInterstitialAd); override;
    destructor Destroy; override;
  end;

  TPlatformAppOpenAd = class(TCustomPlatformAppOpenAd)
  private
  protected
    procedure ApplicationBecameActive; override;
    procedure ApplicationEnteredBackground; override;
    procedure DoAdDismissedFullScreenContent; override;
  public
    constructor Create(const AAppOpenAd: TAppOpenAd); override;
    destructor Destroy; override;
  end;

implementation

{ TPlatformInterstitialAd }

constructor TPlatformInterstitialAd.Create(const AInterstitialAd: TInterstitialAd);
begin
  inherited;

end;

destructor TPlatformInterstitialAd.Destroy;
begin

  inherited;
end;

procedure TPlatformInterstitialAd.DoAdDismissedFullScreenContent;
begin
  inherited;

end;

procedure TPlatformInterstitialAd.Load;
begin
  inherited;

end;

{ TPlatformRewardedAd }

constructor TPlatformRewardedAd.Create(const ARewardedAd: TRewardedAd);
begin
  inherited;

end;

destructor TPlatformRewardedAd.Destroy;
begin

  inherited;
end;

procedure TPlatformRewardedAd.DoAdDismissedFullScreenContent;
begin
  inherited;

end;

procedure TPlatformRewardedAd.Load;
begin
  inherited;

end;

{ TPlatformRewardedInterstitialAd }

constructor TPlatformRewardedInterstitialAd.Create(const ARewardedInterstitialAd: TRewardedInterstitialAd);
begin
  inherited;

end;

destructor TPlatformRewardedInterstitialAd.Destroy;
begin

  inherited;
end;

procedure TPlatformRewardedInterstitialAd.DoAdDismissedFullScreenContent;
begin
  inherited;

end;

procedure TPlatformRewardedInterstitialAd.Load;
begin
  inherited;

end;

{ TPlatformAppOpenAd }

procedure TPlatformAppOpenAd.ApplicationBecameActive;
begin
  inherited;

end;

procedure TPlatformAppOpenAd.ApplicationEnteredBackground;
begin
  inherited;

end;

constructor TPlatformAppOpenAd.Create(const AAppOpenAd: TAppOpenAd);
begin
  inherited;

end;

destructor TPlatformAppOpenAd.Destroy;
begin

  inherited;
end;

procedure TPlatformAppOpenAd.DoAdDismissedFullScreenContent;
begin
  inherited;

end;

end.
