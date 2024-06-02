unit VPD.View.Player;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Layouts, FMX.StdCtrls, FMX.Objects,
  DW.VideoPlayer,
  VPD.HostedView;

type
  TPlayerView = class(TForm)
    RootLayout: TLayout;
    PlayerClientLayout: TLayout;
    PlayerLayout: TLayout;
    BackgroundRectangle: TRectangle;
  private
    FPlayer: TVideoPlayer;
    procedure PlayerStateChangedHandler(Sender: TObject; const APlayerState: TPlayerState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GoBack;
    procedure SelectedURL(const AURL: string);
  end;

var
  PlayerView: TPlayerView;

implementation

{$R *.fmx}

uses
  VPD.View.Main;

{ TPlayerView }

constructor TPlayerView.Create(AOwner: TComponent);
begin
  inherited;
  FPlayer := TVideoPlayer.Create;
  FPlayer.View.Visible := False;
  FPlayer.View.Parent := PlayerLayout;
  FPlayer.ControllerTimeout := 1000; // ms. Set to 0 to prevent player controls from showing
  FPlayer.OnPlayerStateChanged := PlayerStateChangedHandler;
  PlayerView := Self;
  Embed(TFmxObject(AOwner));
end;

destructor TPlayerView.Destroy;
begin
  FPlayer.Free;
  inherited;
end;

procedure TPlayerView.GoBack;
begin
  FPlayer.Stop;
  FPlayer.View.Visible := False;
  MainView.GoBack;
end;

procedure TPlayerView.PlayerStateChangedHandler(Sender: TObject; const APlayerState: TPlayerState);
begin
  if APlayerState = TPlayerState.Dismissed then
    GoBack;
end;

procedure TPlayerView.SelectedURL(const AURL: string);
begin
  FPlayer.Prepare(AURL);
  FPlayer.View.Visible := True;
end;

end.
