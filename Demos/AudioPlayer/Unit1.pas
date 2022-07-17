unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.ActnList,
  DW.AudioPlayer;

type
  TAudioPlayers = TArray<TAudioPlayer>;

  TForm1 = class(TForm)
    TracksListBox: TListBox;
    Layout1: TLayout;
    PlayPauseButton: TButton;
    Layout2: TLayout;
    StopButton: TButton;
    ActionList: TActionList;
    PlayAction: TAction;
    PauseAction: TAction;
    StopAction: TAction;
    StopAllButton: TButton;
    StopAllAction: TAction;
    Label1: TLabel;
    procedure PlayPauseButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure StopAllActionUpdate(Sender: TObject);
    procedure PlayActionUpdate(Sender: TObject);
    procedure PlayActionExecute(Sender: TObject);
    procedure PauseActionUpdate(Sender: TObject);
    procedure PauseActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure StopAllActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FPlayingCount: Integer;
    FPlayers: TAudioPlayers;
    procedure AddPlayers;
    function GetSelectedPlayer: TAudioPlayer;
    procedure PlayerAudioStateChangeHandler(Sender: TObject; const AAudioState: TAudioState);
    procedure RemovePlayers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  DW.OSLog,
  DW.IOUtils.Helpers, DW.UIHelper;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  AddPlayers;
end;

destructor TForm1.Destroy;
begin
  RemovePlayers;
  inherited;
end;

procedure TForm1.RemovePlayers;
var
  LPlayer: TAudioPlayer;
begin
  for LPlayer in FPlayers do
    LPlayer.Free;
end;

procedure TForm1.PauseActionExecute(Sender: TObject);
begin
  GetSelectedPlayer.Pause;
end;

procedure TForm1.PauseActionUpdate(Sender: TObject);
var
  LPlayer: TAudioPlayer;
begin
  LPlayer := GetSelectedPlayer;
  PauseAction.Enabled := (LPlayer <> nil) and (LPlayer.AudioState = TAudioState.Playing);
end;

procedure TForm1.PlayActionExecute(Sender: TObject);
begin
  GetSelectedPlayer.Play;
end;

procedure TForm1.PlayActionUpdate(Sender: TObject);
var
  LPlayer: TAudioPlayer;
begin
  LPlayer := GetSelectedPlayer;
  PlayAction.Enabled := LPlayer <> nil;
end;

procedure TForm1.StopActionExecute(Sender: TObject);
begin
  GetSelectedPlayer.Stop;
end;

procedure TForm1.StopActionUpdate(Sender: TObject);
var
  LPlayer: TAudioPlayer;
begin
  LPlayer := GetSelectedPlayer;
  StopAction.Enabled := (LPlayer <> nil) and LPlayer.IsPlaying;
end;

procedure TForm1.StopAllActionExecute(Sender: TObject);
var
  LPlayer: TAudioPlayer;
begin
  for LPlayer in FPlayers do
    LPlayer.Stop;
end;

procedure TForm1.StopAllActionUpdate(Sender: TObject);
begin
  StopAllAction.Enabled := FPlayingCount > 0;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Layout2.Margins.Bottom := TUIHelper.GetOffsetRect.Height;
end;

function TForm1.GetSelectedPlayer: TAudioPlayer;
begin
  Result := nil;
  if TracksListBox.ItemIndex > -1 then
    Result := FPlayers[TracksListBox.ItemIndex];
end;

procedure TForm1.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
var
  LPlayer: TAudioPlayer;
begin
  LPlayer := GetSelectedPlayer;
  if LPlayer <> nil then
  begin
    if LPlayer.IsPlaying then
      PlayPauseButton.Action := PauseAction
    else
      PlayPauseButton.Action := PlayAction;
  end;
end;

procedure TForm1.AddPlayers;
var
  LFileName, LMusicPath: string;
  I: Integer;
  LPlayer: TAudioPlayer;
begin
  {$IF Defined(MSWINDOWS)}
  LMusicPath := TPathHelper.GetDefaultSourcePath('Music');
  {$ELSE}
  LMusicPath := TPathHelper.GetResourcesPath('Music');
  {$ENDIF}
  for LFileName in TDirectory.GetFiles(LMusicPath, '*.mp3', TSearchOption.soTopDirectoryOnly) do
  begin
    LPlayer := TAudioPlayer.Create;
    try
      LPlayer.OnAudioStateChange := PlayerAudioStateChangeHandler;
      LPlayer.LoadFromFile(LFileName);
    except
      LPlayer.Free;
      LPlayer := nil;
    end;
    if LPlayer <> nil then
    begin
      FPlayers := FPlayers + [LPlayer];
      TracksListBox.Items.Add(TPath.GetFileNameWithoutExtension(TPath.GetFileName(LFileName)));
    end;
 end;
  if TracksListBox.Items.Count > 0 then
    TracksListBox.ItemIndex := 0;
end;

procedure TForm1.PlayerAudioStateChangeHandler(Sender: TObject; const AAudioState: TAudioState);
var
  LPlayer: TAudioPlayer;
begin
  LPlayer := TAudioPlayer(Sender);
  case AAudioState of
    TAudioState.Playing:
    begin
      TOSLog.d('Playing: %s', [TPath.GetFileName(LPlayer.FileName)]);
      Inc(FPlayingCount);
      TOSLog.d('Delay: %d', [LPlayer.Delay]);
    end;
    TAudioState.Stopped:
    begin
      TOSLog.d('Stopped: %s', [TPath.GetFileName(LPlayer.FileName)]);
      Dec(FPlayingCount);
    end;
  end;
end;

procedure TForm1.PlayPauseButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if TracksListBox.ItemIndex > -1 then
  begin
    for I := 0 to Length(FPlayers) - 1 do
      FPlayers[I].Stop;
    FPlayers[TracksListBox.ItemIndex].Play;
  end;
end;

end.
