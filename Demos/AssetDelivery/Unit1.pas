unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ListBox, FMX.Media, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.ActnList,
  DW.AssetDelivery.Android;

type
  TForm1 = class(TForm)
    MediaPlayer: TMediaPlayer;
    MediaPlayerControl: TMediaPlayerControl;
    FilesListBox: TListBox;
    ButtonsLayout: TLayout;
    PlayButton: TButton;
    StopButton: TButton;
    ActionList: TActionList;
    PlayAction: TAction;
    StopAction: TAction;
    procedure PlayActionUpdate(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure PlayActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
  private
    FAssetDelivery: TAssetDelivery;
    FAssetPackStates: TAssetPackStates;
    FPackCount: Integer;
    FStarted: Boolean;
    procedure AssetDeliveryAssetPackStateUpdateHandler(Sender: TObject; const AAssetPackState: TAssetPackState; const AAction: TAssetPackAction);
    procedure GetFiles(const APath: string);
    procedure IdleMessageHandler(const Sender: TObject; const M: TMessage);
    procedure UpdateFilesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

const
  cMegabyte = 1048576;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAssetDelivery := TAssetDelivery.Create;
  FAssetDelivery.OnAssetPackStateUpdate := AssetDeliveryAssetPackStateUpdateHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, IdleMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, IdleMessageHandler);
  FAssetDelivery.Free;
  inherited;
end;

procedure TForm1.GetFiles(const APath: string);
var
  LPath: string;
begin
  if not APath.IsEmpty and TDirectory.Exists(APath) then
  begin
    for LPath in TDirectory.GetFiles(APath, '*.*', TSearchOption.soAllDirectories) do
    begin
      FilesListBox.Items.Add(TPath.GetFileName(LPath));
      FilesListBox.ListItems[FilesListBox.Count - 1].TagString := LPath;
    end;
  end;
end;

procedure TForm1.IdleMessageHandler(const Sender: TObject; const M: TMessage);
begin
  if not FStarted then
  begin
    FStarted := True;
    // FPackCount := FAssetDelivery.Fetch(['fastfollow_assetpack', 'ondemand_assetpack']);
    FAssetPackStates := [];
    // Query will request ONLY asset packs that have not already been fully downloaded
    FPackCount := FAssetDelivery.Query(['fastfollow_assetpack', 'ondemand_assetpack']);
  end;
end;

procedure TForm1.AssetDeliveryAssetPackStateUpdateHandler(Sender: TObject; const AAssetPackState: TAssetPackState; const AAction: TAssetPackAction);
begin
  case AAction of
    TAssetPackAction.Fetch:
    begin
      if AAssetPackState.IsFinished then
      begin
        Log.d('Asset pack: ' + AAssetPackState.Name + ' ' + AAssetPackState.StatusDisplayValue);
        Dec(FPackCount);
        if FPackCount = 0 then
          UpdateFilesList;
      end;
    end;
    TAssetPackAction.Query:
    begin
      Log.d('Asset pack: ' + AAssetPackState.Name + ' required space: ' + Round(AAssetPackState.TotalBytesToDownload / cMegabyte).ToString + ' MB');
      FAssetPackStates := FAssetPackStates + [AAssetPackState];
      // **** Use the TotalBytesToDownload value to determine whether the pack might result in using up available space ***
      Dec(FPackCount);
      if FPackCount = 0 then ; // Query has finished - decide which packs to download or inform user of space issues
    end;
  end;
end;

procedure TForm1.PlayActionExecute(Sender: TObject);
begin
  MediaPlayer.FileName := FilesListBox.ListItems[FilesListBox.ItemIndex].TagString;
  MediaPlayer.Play;
end;

procedure TForm1.PlayActionUpdate(Sender: TObject);
begin
  PlayAction.Enabled := (MediaPlayer.State <> TMediaState.Playing) and (FilesListBox.ItemIndex > -1);
end;

procedure TForm1.StopActionExecute(Sender: TObject);
begin
  MediaPlayer.Stop;
end;

procedure TForm1.StopActionUpdate(Sender: TObject);
begin
  StopAction.Enabled := MediaPlayer.State = TMediaState.Playing;
end;

procedure TForm1.UpdateFilesList;
begin
//!!!! Install time packs are just "normal" assets
//  GetFiles(FAssetDelivery.GetAssetPackPath('installtime_assetpack'));
  GetFiles(FAssetDelivery.GetAssetPackPath('fastfollow_assetpack'));
  GetFiles(FAssetDelivery.GetAssetPackPath('ondemand_assetpack'));
end;

end.
