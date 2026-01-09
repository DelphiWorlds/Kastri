unit DW.VideoPlayer;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Messaging, System.Classes,
  // FMX
  FMX.Controls.Presentation;

type
  TPlayerState = (NoPlayer, Preparing, Prepared, Ready, Playing, Paused, Stopped, Completed, Dismissed, Error);

  TVideoPlayer = class;

  TCustomPlatformVideoPlayer = class(TObject)
  private
    FIsBackground: Boolean;
    FVideoPlayer: TVideoPlayer;
    FPlayerState: TPlayerState;
    FView: TPresentedControl;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ViewResizedHandler(Sender: TObject);
  protected
    FControllerTimeout: Integer;
    FUseController: Boolean;
    procedure BecameActive; virtual;
    procedure EnteredBackground;
    procedure Pause; virtual;
    procedure Play(const AURL: string = ''); virtual;
    procedure Prepare(const AURL: string); virtual;
    procedure SetControllerTimeout(const Value: Integer); virtual;
    procedure SetPlayerState(const Value: TPlayerState);
    procedure SetUseController(const Value: Boolean); virtual;
    procedure ShowController(const Value: Boolean); virtual;
    procedure Stop; virtual;
    procedure ViewResized; virtual;
    procedure ViewTouched;
    procedure WillBecomeInactive; virtual;
    property ControllerTimeout: Integer read FControllerTimeout write SetControllerTimeout;
    property UseController: Boolean read FUseController write SetUseController;
    property View: TPresentedControl read FView;
  public
    constructor Create(const AVideoPlayer: TVideoPlayer); virtual;
    destructor Destroy; override;
  end;

  TPlayerStateChangedEvent = procedure(Sender: TObject; const PlayerState: TPlayerState) of object;

  TVideoPlayer = class(TObject)
  private
    FPlatformVideoPlayer: TCustomPlatformVideoPlayer;
    FOnClick: TNotifyEvent;
    FOnPlayerStateChanged: TPlayerStateChangedEvent;
    function GetView: TPresentedControl;
    function GetControllerTimeout: Integer;
    function GetUseController: Boolean;
    procedure SetControllerTimeout(const Value: Integer); virtual;
    procedure SetUseController(const Value: Boolean);
  protected
    procedure PlayerStateChanged(const APlayerState: TPlayerState);
    procedure ViewTouched;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Pause;
    /// <summary>
    ///   Resumes playback if paused, or if a URL is supplied, prepares an item with the URL and plays it
    /// </summary>
    /// <remarks>
    ///   URL can be a web address or a filename
    /// </remarks>
    procedure Play(const AURL: string = '');
    procedure Prepare(const AURL: string);
//    procedure SeekTo(const AValue: Int64);
    procedure ShowController(const Value: Boolean);
    procedure Stop;
    /// <summary>
    ///   Determines when the player controls are hidden. To prevent them from showing at all, use a value <= 0
    /// </summary>
    property ControllerTimeout: Integer read GetControllerTimeout write SetControllerTimeout;
    property UseController: Boolean read GetUseController write SetUseController;
    property View: TPresentedControl read GetView;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    /// <summary>
    ///   Called when the player state changes
    /// </summary>
    property OnPlayerStateChanged: TPlayerStateChangedEvent read FOnPlayerStateChanged write FOnPlayerStateChanged;
  end;

implementation

uses
  FMX.Platform, FMX.Types,
  {$IF Defined(ANDROID)}
  DW.VideoPlayer.Android,
  {$ENDIF}
  {$IF Defined(IOS)}
  DW.VideoPlayer.iOS,
  // macOS support is currently dependent on EMBT resolving this:
  //   https://quality.embarcadero.com/browse/RSP-38988 (Add support for platform controls for macOS)
  // {$ELSEIF Defined(MACOS)}
  // DW.VideoPlayer.Mac,
  // Windows support is in development
  // {$ELSEIF Defined(MSWINDOWS)}
  // DW.VideoPlayer.Win,
  {$ENDIF}
  DW.VideoPlayer.View;

{$IF not (Defined(ANDROID) or Defined(IOS))}
type
  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer);
{$ENDIF}

{ TCustomPlatformVideoPlayer }

constructor TCustomPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
begin
  inherited Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FUseController := True;
  FVideoPlayer := AVideoPlayer;
  FView := TVideoPlayerView.Create(nil);
  FView.Align := TAlignLayout.Client;
  FView.OnResized := ViewResizedHandler;
end;

destructor TCustomPlatformVideoPlayer.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FView.Free;
  inherited;
end;

procedure TCustomPlatformVideoPlayer.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      BecameActive;
    TApplicationEvent.EnteredBackground:
      EnteredBackground;
    TApplicationEvent.WillBecomeInactive:
      WillBecomeInactive;
  end;
end;

procedure TCustomPlatformVideoPlayer.BecameActive;
begin
  FIsBackground := False;
end;

procedure TCustomPlatformVideoPlayer.EnteredBackground;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.WillBecomeInactive;
begin
  FIsBackground := True;
end;

procedure TCustomPlatformVideoPlayer.Pause;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.Play(const AURL: string = '');
begin
  //
end;

procedure TCustomPlatformVideoPlayer.Prepare(const AURL: string);
begin
  //
end;

procedure TCustomPlatformVideoPlayer.SetControllerTimeout(const Value: Integer);
begin
  FControllerTimeout := Value;
end;

procedure TCustomPlatformVideoPlayer.SetPlayerState(const Value: TPlayerState);
begin
  if Value <> FPlayerState then
  begin
    FPlayerState := Value;
    FVideoPlayer.PlayerStateChanged(FPlayerState);
  end;
end;

procedure TCustomPlatformVideoPlayer.SetUseController(const Value: Boolean);
begin
  FUseController := Value;
end;

procedure TCustomPlatformVideoPlayer.ShowController(const Value: Boolean);
begin
  //
end;

procedure TCustomPlatformVideoPlayer.Stop;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.ViewResized;
begin
  //
end;

procedure TCustomPlatformVideoPlayer.ViewResizedHandler(Sender: TObject);
begin
  ViewResized;
end;

procedure TCustomPlatformVideoPlayer.ViewTouched;
begin
  FVideoPlayer.ViewTouched;
end;

{ TVideoPlayer }

constructor TVideoPlayer.Create;
begin
  inherited;
  FPlatformVideoPlayer := TPlatformVideoPlayer.Create(Self);
end;

destructor TVideoPlayer.Destroy;
begin
  FPlatformVideoPlayer.Free;
  inherited;
end;

function TVideoPlayer.GetControllerTimeout: Integer;
begin
  Result := FPlatformVideoPlayer.ControllerTimeout;
end;

function TVideoPlayer.GetUseController: Boolean;
begin
  Result := FPlatformVideoPlayer.UseController;
end;

function TVideoPlayer.GetView: TPresentedControl;
begin
  Result := FPlatformVideoPlayer.View;
end;

procedure TVideoPlayer.Pause;
begin
  FPlatformVideoPlayer.Pause;
end;

procedure TVideoPlayer.Play(const AURL: string = '');
begin
  FPlatformVideoPlayer.Play(AURL);
end;

procedure TVideoPlayer.PlayerStateChanged(const APlayerState: TPlayerState);
begin
  if Assigned(FOnPlayerStateChanged) then
    FOnPlayerStateChanged(Self, APlayerState);
end;

procedure TVideoPlayer.Prepare(const AURL: string);
begin
  FPlatformVideoPlayer.Prepare(AURL);
end;

procedure TVideoPlayer.SetControllerTimeout(const Value: Integer);
begin
  FPlatformVideoPlayer.ControllerTimeout := Value;
end;

procedure TVideoPlayer.SetUseController(const Value: Boolean);
begin
  FPlatformVideoPlayer.UseController := Value;
end;

procedure TVideoPlayer.ShowController(const Value: Boolean);
begin
  FPlatformVideoPlayer.ShowController(Value);
end;

procedure TVideoPlayer.Stop;
begin
  FPlatformVideoPlayer.Stop;
end;

procedure TVideoPlayer.ViewTouched;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

end.
