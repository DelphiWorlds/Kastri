unit DW.VideoPlayer.iOS;

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

interface

uses
  // RTL
  System.TypInfo,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.AVFoundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.AVKit, DW.VideoPlayer, DW.Macapi.Helpers;

type
  TPlatformVideoPlayer = class;

  IAVPlayerItemNotifications = interface(IObjectiveC)
    ['{8943DCF8-6D5F-409A-B2D3-330D5DED8065}']
    procedure ItemDidPlayToEndTime; cdecl;
    procedure ItemFailedToPlayToEndTime; cdecl;
    procedure ItemNewAccessLogEntry; cdecl;
    procedure ItemNewErrorLogEntry; cdecl;
    procedure ItemPlaybackStalled; cdecl;
    procedure ItemTimeJumped; cdecl;
  end;

  TAVPlayerItemNotifications = class(TOCLocal, IAVPlayerItemNotifications)
  private
    FPlatformVideoPlayer: TPlatformVideoPlayer;
  public
    { IAVPlayerItemNotifications }
    procedure ItemDidPlayToEndTime; cdecl;
    procedure ItemFailedToPlayToEndTime; cdecl;
    procedure ItemNewAccessLogEntry; cdecl;
    procedure ItemNewErrorLogEntry; cdecl;
    procedure ItemPlaybackStalled; cdecl;
    procedure ItemTimeJumped; cdecl;
  public
    constructor Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
    procedure AddObservers(const APlayerItemID: Pointer);
  end;

  AVPlayerViewControllerDelegateCustom = interface(IObjectiveC)
    ['{34D3FFE0-39FB-48BC-867D-9AFA4BD00052}']
    [MethodName('playerViewController:willEndFullScreenPresentationWithAnimationCoordinator:')]
    procedure playerViewControllerWillEndFullScreenPresentationWithAnimationCoordinator(playerViewController: AVPlayerViewController;
      willEndFullScreenPresentationWithAnimationCoordinator: Pointer); cdecl;
  end;

  TAVPlayerViewControllerDelegate = class(TOCLocal, AVPlayerViewControllerDelegateCustom)
  private
    FPlatformVideoPlayer: TPlatformVideoPlayer;
  public
    { AVPlayerViewControllerDelegateCustom }
    [MethodName('playerViewController:willEndFullScreenPresentationWithAnimationCoordinator:')]
    procedure playerViewControllerWillEndFullScreenPresentationWithAnimationCoordinator(playerViewController: AVPlayerViewController;
      willEndFullScreenPresentationWithAnimationCoordinator: Pointer); cdecl;
  public
    constructor Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
  end;

  IGesturesHandler = interface(NSObject)
    ['{29785A5E-D750-4AA2-A6B7-25D3B02AE0D6}']
    procedure handleTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
  end;

  TTapEvent = procedure(Sender: TObject; const View: UIView) of object;

  TGesturesHandler = class(TOCLocal)
  private
    FTapGestureRecognizer: UITapGestureRecognizer;
    FOnTap: TTapEvent;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { IGestureHandlers }
    procedure handleTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
  public
    constructor Create;
    procedure AddTap(const AView: UIView);
    property OnTap: TTapEvent read FOnTap write FOnTap;
  end;

  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer)
  private
    FController: AVPlayerViewController;
    FControllerDelegate: TAVPlayerViewControllerDelegate;
    FGesturesHandler: TGesturesHandler;
    FPlayer: AVPlayer;
    FPlayerItem: AVPlayerItem;
    FPlayerItemNotifications: TAVPlayerItemNotifications;
    FPlayerLayer: AVPlayerLayer;
    FValueObserver: TKeyValueObserver;
    FView: UIView;
    procedure CheckControllerTimeout;
    procedure CreateAVPlayer(const AURL: string);
    procedure DestroyAVPlayer;
    function GetControllerView: UIView;
    function HasPlayer: Boolean;
    procedure TapHandler(Sender: TObject; const AView: UIView);
    procedure ValueChangeHandler(const AKeyPath: string; const AChange: NSDictionary);
  protected
    procedure ControllerWillEndFullScreenPresentation;
    procedure ItemDidPlayToEndTime;
    procedure ItemFailedToPlayToEndTime;
    procedure ItemNewAccessLogEntry;
    procedure ItemNewErrorLogEntry;
    procedure ItemPlaybackStalled;
    procedure ItemTimeJumped;
    procedure Pause; override;
    procedure Play(const AURL: string = ''); override;
    procedure Prepare(const AURL: string); override;
    procedure ShowController(const Value: Boolean); override;
    procedure Stop; override;
    procedure ViewResized; override;
  public
    constructor Create(const AVideoPlayer: TVideoPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Classes, System.SysUtils, System.DateUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers, iOSapi.CoreMedia, {$IF CompilerVersion > 36} iOSapi.AVFAudio, {$ENDIF}
  // FMX
  FMX.Presentation.iOS,
  FMX.Controls, FMX.Presentation.Factory,
  // DW
  DW.VideoPlayer.View, DW.iOSapi.Helpers, DW.OSTimer;

type
  TiOSVideoPlayerView = class(TiOSNativeView);

function AVLayerVideoGravityResizeAspect: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspect');
end;

function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill');
end;

{$IF CompilerVersion < 37}
function GetName(const AName: string): Pointer;
begin
  Result := StringToID(AName);
end;
{$ELSE}
function GetName(const AName: string): NSString;
begin
  Result := StrToNSStr(AName);
end;
{$ENDIF}

{ TAVPlayerItemNotifications }

constructor TAVPlayerItemNotifications.Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
begin
  inherited Create;
  FPlatformVideoPlayer := APlatformVideoPlayer;
end;

procedure TAVPlayerItemNotifications.AddObservers(const APlayerItemID: Pointer);
var
  LCenter: NSNotificationCenter;
begin
  LCenter := TiOSHelper.DefaultNotificationCenter;
  LCenter.addObserver(GetObjectID, sel_getUid('ItemDidPlayToEndTime'), GetName('AVPlayerItemDidPlayToEndTimeNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), GetName('AVPlayerItemFailedToPlayToEndTimeNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemNewAccessLogEntry'), GetName('AVPlayerItemNewAccessLogEntryNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemNewErrorLogEntry'), GetName('AVPlayerItemNewErrorLogEntryNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemPlaybackStalled'), GetName('AVPlayerItemPlaybackStalledNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemTimeJumped'), GetName('AVPlayerItemTimeJumpedNotification'), APlayerItemID);
end;

procedure TAVPlayerItemNotifications.ItemDidPlayToEndTime;
begin
  TOSLog.d('TAVPlayerItemNotifications.ItemDidPlayToEndTime');
  FPlatformVideoPlayer.ItemDidPlayToEndTime;
end;

procedure TAVPlayerItemNotifications.ItemFailedToPlayToEndTime;
begin
  TOSLog.d('TAVPlayerItemNotifications.ItemFailedToPlayToEndTime');
  FPlatformVideoPlayer.ItemFailedToPlayToEndTime;
end;

procedure TAVPlayerItemNotifications.ItemNewAccessLogEntry;
begin
  FPlatformVideoPlayer.ItemNewAccessLogEntry;
end;

procedure TAVPlayerItemNotifications.ItemNewErrorLogEntry;
begin
  FPlatformVideoPlayer.ItemNewErrorLogEntry;
end;

procedure TAVPlayerItemNotifications.ItemPlaybackStalled;
begin
  TOSLog.d('TAVPlayerItemNotifications.ItemPlaybackStalled');
  FPlatformVideoPlayer.ItemPlaybackStalled;
end;

procedure TAVPlayerItemNotifications.ItemTimeJumped;
begin
  FPlatformVideoPlayer.ItemTimeJumped;
end;

{ TAVPlayerViewControllerDelegate }

constructor TAVPlayerViewControllerDelegate.Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
begin
  inherited Create;
  FPlatformVideoPlayer := APlatformVideoPlayer;
end;

procedure TAVPlayerViewControllerDelegate.playerViewControllerWillEndFullScreenPresentationWithAnimationCoordinator(
  playerViewController: AVPlayerViewController; willEndFullScreenPresentationWithAnimationCoordinator: Pointer);
begin
  FPlatformVideoPlayer.ControllerWillEndFullScreenPresentation;
end;

{ TPlatformVideoPlayer }

constructor TPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
begin
  inherited;
  FValueObserver := TKeyValueObserver.Create(ValueChangeHandler);
  FControllerDelegate := TAVPlayerViewControllerDelegate.Create(Self);
  FController := TAVPlayerViewController.Create;
  FController.setDelegate(FControllerDelegate.GetObjectID);
  FGesturesHandler := TGesturesHandler.Create;
  FGesturesHandler.AddTap(GetControllerView);
  FGesturesHandler.OnTap := TapHandler;
  FPlayerItemNotifications := TAVPlayerItemNotifications.Create(Self);
  FView := TiOSVideoPlayerView(View.Presentation).View;
  FView.setContentMode(UIViewContentModeScaleAspectFit);
  {$IF CompilerVersion < 37}
  TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance).setCategory(AVAudioSessionCategoryPlayback, nil);
  {$ELSE}
  TAVAudioSession.OCClass.sharedInstance.setCategory(AVAudioSessionCategoryPlayback, nil);
  {$ENDIF}
end;

destructor TPlatformVideoPlayer.Destroy;
begin
  FGesturesHandler.Free;
  DestroyAVPlayer;
  FPlayerItemNotifications.Free;
  FControllerDelegate.Free;
  FValueObserver.Free;
  inherited;
end;

procedure TPlatformVideoPlayer.CreateAVPlayer(const AURL: string);
var
  LURL: NSURL;
begin
  DestroyAVPlayer;
  if AURL.StartsWith('http://', True) or AURL.StartsWith('https://', True) or AURL.StartsWith('rtsp://', True) then
    LURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AURL)))
  else
  begin
    {$IF CompilerVersion < 37}
    LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AURL)));
    {$ELSE}
    LURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(AURL));
    {$ENDIF}
  end;
  FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL));
  FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem));
  FPlayerItemNotifications.AddObservers(NSObjectToID(FPlayerItem));
  FValueObserver.Observe(FPlayer, 'rate', NSKeyValueObservingOptionNew or NSKeyValueObservingOptionOld);
  ShowController(UseController);
  {$IF CompilerVersion < 37}
  FPlayerLayer := TAVPlayerLayer.Wrap(TAVPlayerLayer.OCClass.playerLayerWithPlayer(FPlayer));
  {$ELSE}
  FPlayerLayer := TAVPlayerLayer.OCClass.playerLayerWithPlayer(FPlayer);
  {$ENDIF}
  FPlayerLayer.setVideoGravity(AVLayerVideoGravityResizeAspect);
  FView.layer.addSublayer(FPlayerLayer);
  ViewResized;
end;

procedure TPlatformVideoPlayer.DestroyAVPlayer;
begin
  FPlayer := nil;
  FPlayerItem := nil;
  if FPlayerLayer <> nil then
    FPlayerLayer.removeFromSuperlayer;
  FPlayerLayer := nil;
end;

function TPlatformVideoPlayer.GetControllerView: UIView;
begin
  Result := TUIView.Wrap(NSObjectToID(FController.view));
end;

function TPlatformVideoPlayer.HasPlayer: Boolean;
begin
  Result := FPlayer <> nil;
end;

procedure TPlatformVideoPlayer.ShowController(const Value: Boolean);
begin
  if FPlayer <> nil then
  begin
    if Value then
    begin
      FController.setPlayer(FPlayer);
      TiOSHelperEx.SharedApplication.keyWindow.rootViewController.presentViewController(FController, True, nil);
    end
    else
      FController.setPlayer(nil);
  end;
end;

procedure TPlatformVideoPlayer.ControllerWillEndFullScreenPresentation;
begin
  SetPlayerState(TPlayerState.Dismissed);
end;

procedure TPlatformVideoPlayer.ItemDidPlayToEndTime;
const
  NSEC_PER_SEC = 1000000000;
begin
  if NeedsLoop then
  begin
    FPlayer.seekToTime(CMTimeMakeWithSeconds(0, NSEC_PER_SEC));
    FPlayer.play;
  end
  else
    SetPlayerState(TPlayerState.Completed);
end;

procedure TPlatformVideoPlayer.ItemFailedToPlayToEndTime;
begin

end;

procedure TPlatformVideoPlayer.ItemNewAccessLogEntry;
begin

end;

procedure TPlatformVideoPlayer.ItemNewErrorLogEntry;
begin

end;

procedure TPlatformVideoPlayer.ItemPlaybackStalled;
begin

end;

procedure TPlatformVideoPlayer.ItemTimeJumped;
begin

end;

procedure TPlatformVideoPlayer.Play(const AURL: string = '');
begin
  if not AURL.IsEmpty then
    Prepare(AURL);
  if HasPlayer then
  begin
    if FPlayer.currentItem = nil then
      FPlayer.replaceCurrentItemWithPlayerItem(FPlayerItem);
    FPlayer.play;
  end;
end;

procedure TPlatformVideoPlayer.TapHandler(Sender: TObject; const AView: UIView);
begin
  FController.setShowsPlaybackControls(True);
end;

procedure TPlatformVideoPlayer.Prepare(const AURL: string);
begin
  CreateAVPlayer(AURL);
end;

procedure TPlatformVideoPlayer.Pause;
begin
  if HasPlayer then
    FPlayer.pause;
end;

procedure TPlatformVideoPlayer.Stop;
begin
  if HasPlayer then
  begin
    FPlayer.pause;
    FPlayer.replaceCurrentItemWithPlayerItem(nil);
    FPlayerLayer.setPlayer(nil);
    FPlayerLayer.removeFromSuperlayer;
    FPlayerLayer := nil;
  end;
end;

procedure TPlatformVideoPlayer.CheckControllerTimeout;
begin
  if ControllerTimeout > 0 then
  begin
    TOSTimer.FireOnce(ControllerTimeout,
      procedure
      begin
        FController.setShowsPlaybackControls(False);
      end
    );
  end
  else
    FController.setShowsPlaybackControls(False);
end;

procedure TPlatformVideoPlayer.ValueChangeHandler(const AKeyPath: string; const AChange: NSDictionary);
begin
  if AKeyPath.Equals('rate') then
  begin
    if FPlayer.rate > 0 then
    begin
      if PlayerState <> TPlayerState.Playing then
      begin
        CheckControllerTimeout;
        SetPlayerState(TPlayerState.Playing);
      end;
    end
    else if FPlayer.status = AVPlayerStatusReadyToPlay then
      SetPlayerState(TPlayerState.Paused)
    else
      SetPlayerState(TPlayerState.Stopped);
  end;
end;

procedure TPlatformVideoPlayer.ViewResized;
begin
  if FPlayerLayer <> nil then
    FPlayerLayer.setFrame(FView.frame);
end;

{ TGesturesHandler }

constructor TGesturesHandler.Create;
begin
  inherited Create;
  FTapGestureRecognizer := TUITapGestureRecognizer.Wrap(TUITapGestureRecognizer.Alloc.initWithTarget(GetObjectID, sel_getUid('handleTap:')))
end;

function TGesturesHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IGesturesHandler);
end;

procedure TGesturesHandler.AddTap(const AView: UIView);
begin
  AView.addGestureRecognizer(FTapGestureRecognizer);
end;

procedure TGesturesHandler.handleTap(gestureRecognizer: UITapGestureRecognizer);
begin
  if Assigned(FOnTap) then
    FOnTap(Self, gestureRecognizer.view);
end;

initialization
  TPresentationProxyFactory.Current.Register(TVideoPlayerView, TControlType.Platform, TiOSPresentationProxy<TiOSVideoPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TVideoPlayerView, TControlType.Platform, TiOSPresentationProxy<TiOSVideoPlayerView>);

end.
