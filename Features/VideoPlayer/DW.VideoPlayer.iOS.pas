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

  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer)
  private
    FController: AVPlayerViewController;
    FControllerDelegate: TAVPlayerViewControllerDelegate;
    FPlayer: AVPlayer;
    FPlayerItem: AVPlayerItem;
    FPlayerItemNotifications: TAVPlayerItemNotifications;
    FPlayerLayer: AVPlayerLayer;
    FValueObserver: TKeyValueObserver;
    FView: UIView;
    procedure CreateAVPlayer(const AURL: string);
    procedure DestroyAVPlayer;
    function HasPlayer: Boolean;
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
  // RTL
  System.Classes, System.TypInfo, System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers,
  // FMX
  FMX.Controls, FMX.Presentation.iOS, FMX.Presentation.Factory,
  // DW
  DW.VideoPlayer.View, DW.iOSapi.Helpers;

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
  LCenter.addObserver(GetObjectID, sel_getUid('ItemDidPlayToEndTime'), StringToID('AVPlayerItemDidPlayToEndTimeNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemFailedToPlayToEndTime'), StringToID('AVPlayerItemFailedToPlayToEndTimeNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemNewAccessLogEntry'), StringToID('AVPlayerItemNewAccessLogEntryNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemNewErrorLogEntry'), StringToID('AVPlayerItemNewErrorLogEntryNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemPlaybackStalled'), StringToID('AVPlayerItemPlaybackStalledNotification'), APlayerItemID);
  LCenter.addObserver(GetObjectID, sel_getUid('ItemTimeJumped'), StringToID('AVPlayerItemTimeJumpedNotification'), APlayerItemID);
end;

procedure TAVPlayerItemNotifications.ItemDidPlayToEndTime;
begin
  FPlatformVideoPlayer.ItemDidPlayToEndTime;
end;

procedure TAVPlayerItemNotifications.ItemFailedToPlayToEndTime;
begin
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
  FPlayerItemNotifications := TAVPlayerItemNotifications.Create(Self);
  FView := TiOSVideoPlayerView(View.Presentation).View;
  FView.setContentMode(UIViewContentModeScaleAspectFit);
  TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance).setCategory(AVAudioSessionCategoryPlayback, nil);
end;

destructor TPlatformVideoPlayer.Destroy;
begin
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
    LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AURL)));
  FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL));
  FPlayerItemNotifications.AddObservers(NSObjectToID(FPlayerItem));
  FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem));
  FValueObserver.Observe(FPlayer, 'rate', NSKeyValueObservingOptionNew or NSKeyValueObservingOptionOld);
  ShowController(UseController);
  FPlayerLayer := TAVPlayerLayer.Wrap(TAVPlayerLayer.OCClass.playerLayerWithPlayer(FPlayer));
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
begin
  // May need this for looping
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
    FPlayer.play;
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
    FPlayer.pause;
end;

procedure TPlatformVideoPlayer.ValueChangeHandler(const AKeyPath: string; const AChange: NSDictionary);
begin
  if AKeyPath.Equals('rate') then
  begin
    if FPlayer.rate > 0 then
      SetPlayerState(TPlayerState.Playing)
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

initialization
  TPresentationProxyFactory.Current.Register(TVideoPlayerView, TControlType.Platform, TiOSPresentationProxy<TiOSVideoPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TVideoPlayerView, TControlType.Platform, TiOSPresentationProxy<TiOSVideoPlayerView>);

end.
