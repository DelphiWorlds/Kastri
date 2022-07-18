unit DW.AdMob.Android;

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
  System.SysUtils,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.AdMob;

type
  TPlatformMobileAds = class;

  TInitializatonCompleteListener = class(TJavaLocal, JOnInitializationCompleteListener)
  private
    FPlatformMobileAds: TPlatformMobileAds;
  public
    { JOnInitializationCompleteListener }
    procedure onInitializationComplete(initializationStatus: JInitializationStatus); cdecl;
  public
    constructor Create(const APlatformMobileAds: TPlatformMobileAds);
  end;

  TMobileAdsState = (None, Initializing, Initialized);

  TStartProcs = TArray<TProc>;

  TPlatformMobileAds = class(TObject)
  private
    class var FInstance: TPlatformMobileAds;
  private
    FInitializatonCompleteListener: JOnInitializationCompleteListener;
    FStartProcs: TStartProcs;
    FState: TMobileAdsState;
    procedure InternalStart(const AStartProc: TProc);
  protected
    class procedure Start(const AStartProc: TProc);
  protected
    procedure InitializationComplete;
  public
    constructor Create;
  end;

implementation

uses
  Androidapi.Helpers;

{ TInitializatonCompleteListener }

constructor TInitializatonCompleteListener.Create(const APlatformMobileAds: TPlatformMobileAds);
begin
  inherited Create;
  FPlatformMobileAds := APlatformMobileAds;
end;

procedure TInitializatonCompleteListener.onInitializationComplete(initializationStatus: JInitializationStatus);
begin
  FPlatformMobileAds.InitializationComplete;
end;

{ TPlatformMobileAds }

constructor TPlatformMobileAds.Create;
begin
  inherited;
  FInitializatonCompleteListener := TInitializatonCompleteListener.Create(Self);
end;

procedure TPlatformMobileAds.InitializationComplete;
var
  I: Integer;
  LProc: TProc;
begin
  FState := TMobileAdsState.Initialized;
  for I := 0 to High(FStartProcs) do
  begin
    LProc := FStartProcs[I];
    LProc;
  end;
  FStartProcs := [];
end;

procedure TPlatformMobileAds.InternalStart(const AStartProc: TProc);
begin
  if FState <> TMobileAdsState.Initialized then
  begin
    FStartProcs := FStartProcs + [AStartProc];
    if FState = TMobileAdsState.None then
    begin
      FState := TMobileAdsState.Initializing;
      TJMobileAds.JavaClass.initialize(TAndroidHelper.Context, FInitializatonCompleteListener);
    end;
  end
  else
    AStartProc;
end;

class procedure TPlatformMobileAds.Start(const AStartProc: TProc);
begin
  if FInstance = nil then
    FInstance := TPlatformMobileAds.Create;
  FInstance.InternalStart(AStartProc);
end;

end.
