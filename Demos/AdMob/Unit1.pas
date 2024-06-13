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
    ConsentButton: TButton;
    procedure ShowBannerClick(Sender: TObject);
    procedure AdMobBannerAd1AdClicked(Sender: TObject);
    procedure AdMobBannerAd1AdClosed(Sender: TObject);
    procedure AdMobBannerAd1AdFailedToLoad(Sender: TObject; const Error: TAdError);
    procedure AdMobBannerAd1AdImpression(Sender: TObject);
    procedure AdMobBannerAd1AdLoaded(Sender: TObject);
    procedure AdMobBannerAd1AdOpened(Sender: TObject);
    procedure ShowInterstitialClick(Sender: TObject);
    procedure ShowRewardClick(Sender: TObject);
    procedure ConsentButtonClick(Sender: TObject);
  private
    FIntAd: TInterstitialAd;
    FRewAd: TRewardedAd;
    // FOpenAd: TAppOpenAd;
    procedure AdDismissedFullScreenContentHandler(Sender: TObject);
    procedure AdFailedToLoadHandler(Sender: TObject; const AError: TAdError);
    procedure AdFailedToShowFullScreenContentHandler(Sender: TObject; const AError: TAdError);
    procedure AdLoadedHander(Sender: TObject);
    procedure AdMobConsentErrorHandler(Sender: TObject; const AError: TConsentError);
    procedure AdMobConsentCompleteHandler(Sender: TObject; const AStatus: TConsentStatus);
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

const
  cConsentStatusValues: array[TConsentStatus] of string = ('Unknown', 'Required', 'Not Required', 'Obtained');

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  AdMob.OnConsentComplete := AdMobConsentCompleteHandler;
  AdMob.OnConsentError := AdMobConsentErrorHandler;
  // AdMob.ResetConsent; // <---- Simulates users first install experience
  AdMob.SetDebugGeography(TDebugGeography.EEA); // <---- Simulates geographical area
  {$IF Defined(ANDROID)}
  AdMob.SetTestDeviceHashedId('5E236AB5D9ACD3626D268508544B8259'); // <---- This value is shown in logcat messages on first run
  {$ELSEIF Defined(IOS)}
  AdMob.SetTestDeviceHashedId('2E868554-4228-45B3-BE84-28AE14B4DA31'); // <---- This value is shown in Console messages on first run
  {$ENDIF}

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
  FRewAd.OnAdLoaded := AdLoadedHander;
  FRewAd.OnAdFailedToLoad := AdFailedToLoadHandler;
  // RewardAds only
  FRewAd.OnUserEarnedReward := UserEarnedRewardHandler;

//  FOpenAd := TAppOpenAd.Create;
//  FOpenAd.TestMode := True;
//  FOpenAd.OnAdDismissedFullScreenContent := AdDismissedFullScreenContentHandler;
//  FOpenAd.OnAdFailedToShowFullScreenContent := AdFailedToShowFullScreenContentHandler;
//  FOpenAd.OnAdWillPresentFullScreenContent := AdWillPresentFullScreenContentHandler;
//  FOpenAd.Load;
end;

procedure TForm1.ConsentButtonClick(Sender: TObject);
begin
  // Use RequestConsent(True) if you need to OPT OUT out of UMP
  {$IF CompilerVersion > 35}
  AdMob.RequestConsent;
  {$ELSE}
  // UMP not supported on Delphi 11
  AdMob.RequestConsent(True);
  {$ENDIF}
  // **** NOTE: Ads will not start until RequestConsent has been called ****
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
  ShowInterstitial.Enabled := False;
  FIntAd.Show;
end;

procedure TForm1.ShowRewardClick(Sender: TObject);
begin
  ShowReward.Enabled := False;
  FRewAd.Show;
end;

procedure TForm1.UserEarnedRewardHandler(Sender: TObject; const AReward: TAdReward);
begin
  Memo.Lines.Add('Rewarded Ad Reward - ' + AReward.RewardType + ': ' + AReward.Amount.ToString);
end;

procedure TForm1.AdDismissedFullScreenContentHandler(Sender: TObject);
begin
  Memo.Lines.Add('Fullscreen Ad Dismissed ' + Sender.ClassName);
  // Dismissed current ad - load the next one
  if Sender = FIntAd then
    FIntAd.Load(False)
  else if Sender = FRewAd then
    FRewAd.Load(False);
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
  if Sender = FIntAd then
    ShowInterstitial.Enabled := True
  else if Sender = FRewAd then
    ShowReward.Enabled := True;
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

procedure TForm1.AdMobConsentCompleteHandler(Sender: TObject; const AStatus: TConsentStatus);
begin
  Memo.Lines.Add(Format('Consent complete - Status: %s, CanRequestAds: %s', [cConsentStatusValues[AStatus], BoolToStr(AdMob.CanRequestAds, True)]));
  ShowBanner.Enabled := True;
  // ShowInterstitial.Enabled := True;
  // ShowReward.Enabled := True;
  FIntAd.Load(False);
  FRewAd.Load(False);
end;

procedure TForm1.AdMobConsentErrorHandler(Sender: TObject; const AError: TConsentError);
begin
  Memo.Lines.Add(Format('Consent error from: %s - %d: %s', [AError.Origin, AError.ErrorCode, AError.Message]));
end;

end.
