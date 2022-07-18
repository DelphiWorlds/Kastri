unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  DW.AdMob, DW.AdMobBannerAd, DW.AdMobAds;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    ButtonsLayout: TLayout;
    ShowBanner: TButton;
    AdMobBannerAd1: TAdMobBannerAd;
    ShowInterstitial: TButton;
    ShowReward: TButton;
    procedure ShowBannerClick(Sender: TObject);
    procedure AdMobBannerAd1AdClicked(Sender: TObject);
    procedure AdMobBannerAd1AdClosed(Sender: TObject);
    procedure AdMobBannerAd1AdFailedToLoad(Sender: TObject; const Error: TAdError);
    procedure AdMobBannerAd1AdImpression(Sender: TObject);
    procedure AdMobBannerAd1AdLoaded(Sender: TObject);
    procedure AdMobBannerAd1AdOpened(Sender: TObject);
    procedure ShowInterstitialClick(Sender: TObject);
    procedure ShowRewardClick(Sender: TObject);
  private
    FIntAd: TInterstitialAd;
    FRewAd: TRewardedAd;
    // FOpenAd: TAppOpenAd;
    procedure AdDismissedFullScreenContentHandler(Sender: TObject);
    procedure AdFailedToLoadHandler(Sender: TObject; const AError: TAdError);
    procedure AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
    procedure AdLoadedHander(Sender: TObject);
    procedure AdWillPresentFullScreenContentHandler(Sender: TObject);
    function GetErrorMessage(const AEvent: string; const ASender: TObject; const AError: TAdError): string;
    procedure UserEarnedRewardHandler(Sender: TObject; const AReward: TAdReward);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FIntAd := TInterstitialAd.Create;
  FIntAd.TestMode := True;
  FIntAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
  FIntAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
  FIntAd.OnAdWillPresentFullScreenContent := AdWillPresentFullScreenContentHandler;
  FIntAd.OnAdLoaded := AdLoadedHander;
  FIntAd.OnAdFailedToLoad := AdFailedToLoadHandler;

  FRewAd := TRewardedAd.Create;
  FRewAd.TestMode := True;
  FRewAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
  FRewAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
  FRewAd.OnAdWillPresentFullScreenContent := AdWillPresentFullScreenContentHandler;
  // RewardAds only
  FRewAd.OnUserEarnedReward := UserEarnedRewardHandler;

//  FOpenAd := TAppOpenAd.Create;
//  FOpenAd.TestMode := True;
//  FOpenAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
//  FOpenAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
//  FOpenAd.OnAdWillPresentFullScreenContent := AdWillPresentFullScreenContentHandler;
//  FOpenAd.Load;
end;

function TForm1.GetErrorMessage(const AEvent: string; const ASender: TObject; const AError: TAdError): string;
begin
  Result := Format('%s (%s)'#13#10'%d: %s ', [AEvent, ASender.ClassName, AError.ErrorCode, AError.Message]);
end;

procedure TForm1.ShowBannerClick(Sender: TObject);
begin
  AdMobBannerAd1.AdSize := TAdMobBannerAdSize.Adaptive;
  AdMobBannerAd1.LoadAd;
end;

procedure TForm1.ShowInterstitialClick(Sender: TObject);
begin
  FIntAd.Load;
end;

procedure TForm1.ShowRewardClick(Sender: TObject);
begin
  FRewAd.Load;
end;

procedure TForm1.UserEarnedRewardHandler(Sender: TObject; const AReward: TAdReward);
begin
  Memo.Lines.Add('Rewarded Ad Reward - ' + AReward.RewardType + ': ' + AReward.Amount.ToString);
end;

procedure TForm1.AdDismissedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Fullscreen Ad Dismissed '+Sender.ClassName);
end;

procedure TForm1.AdFailedToLoadHandler(Sender: TObject; const AError: TAdError);
begin
  Memo.Lines.Add(GetErrorMessage('Ad Failed To Load', Sender, AError));
end;

procedure TForm1.AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
begin
  Memo.Lines.Add(GetErrorMessage('Fullscreen Ad Failed', Sender, AError));
end;

procedure TForm1.AdLoadedHander(Sender: TObject);
begin
  Memo.Lines.Add('Ad Loaded '+ Sender.ClassName);
end;

procedure TForm1.AdWillPresentFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Fullscreen Ad Will Present ' + Sender.ClassName);
end;

procedure TForm1.AdMobBannerAd1AdClicked(Sender: TObject);
begin
  Memo.Lines.Add('Ad Clicked ' + Sender.ClassName);
end;

procedure TForm1.AdMobBannerAd1AdClosed(Sender: TObject);
begin
  Memo.Lines.Add('Ad Closed ' + Sender.ClassName);
end;

procedure TForm1.AdMobBannerAd1AdFailedToLoad(Sender: TObject; const Error: TAdError);
begin
  Memo.Lines.Add(GetErrorMessage('Ad Failed To Load', Sender, Error));
end;

procedure TForm1.AdMobBannerAd1AdImpression(Sender: TObject);
begin
  Memo.Lines.Add('Ad Impression ' + Sender.ClassName);
end;

procedure TForm1.AdMobBannerAd1AdLoaded(Sender: TObject);
begin
  Memo.Lines.Add('Ad Loaded ' + Sender.ClassName);
end;

procedure TForm1.AdMobBannerAd1AdOpened(Sender: TObject);
begin
  Memo.Lines.Add('Ad Opened ' + Sender.ClassName);
end;

end.
