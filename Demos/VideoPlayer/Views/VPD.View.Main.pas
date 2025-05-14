unit VPD.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.TabControl,
  FMX.Edit, System.Actions, FMX.ActnList;

type
  TMainView = class(TForm)
    TabControl: TTabControl;
    RootLayout: TLayout;
    PlayerTab: TTabItem;
    StreamsTab: TTabItem;
    FilesTab: TTabItem;
    MenuTab: TTabItem;
    FilesButton: TButton;
    StreamsButton: TButton;
    URLEdit: TEdit;
    PlayURLButton: TButton;
    ClearURLButton: TClearEditButton;
    ActionList: TActionList;
    PlayURLAction: TAction;
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure StreamsButtonClick(Sender: TObject);
    procedure ClearURLButtonClick(Sender: TObject);
    procedure PlayURLActionExecute(Sender: TObject);
    procedure PlayURLActionUpdate(Sender: TObject);
  private
    FTabStack: TStack<TTabItem>;
    procedure CreateViews;
    procedure SwitchTab(const ATab: TTabItem);
    procedure TabSwitched;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GoBack;
    procedure SelectedFile(const AFileName: string);
    procedure SelectedStream(const ATitle, AURL: string);
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Net.URLClient, System.RegularExpressions,
  FMX.Platform,
  VPD.View.Streams, VPD.View.Files, VPD.View.Player,
  DW.UIHelper;

function IsValidURL(const AURL: string): Boolean;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create('[(http(s)?):\/\/(www\.)?a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)', [roIgnoreCase]);
  Result := LRegEx.Match(AUrl).Success;
end;

{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited;
  {$IF Defined(OSX)}
  ClientHeight := Round(Screen.Height * 0.75);
  ClientWidth := Round(Height * 1.7778); // 16:9
  {$ENDIF}
  FTabStack := TStack<TTabItem>.Create;
  TabControl.ActiveTab := MenuTab;
  CreateViews;
end;

destructor TMainView.Destroy;
begin
  FTabStack.Free;
  inherited;
end;

procedure TMainView.ClearURLButtonClick(Sender: TObject);
begin
  URLEdit.Text := '';
  URLEdit.SetFocus;
end;

procedure TMainView.StreamsButtonClick(Sender: TObject);
begin
  SwitchTab(StreamsTab);
end;

procedure TMainView.FilesButtonClick(Sender: TObject);
begin
  SwitchTab(FilesTab);
end;

procedure TMainView.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    Key := 0;
    if TabControl.ActiveTab = PlayerTab then
      PlayerView.GoBack
    else
      GoBack;
  end;
end;

procedure TMainView.FormResize(Sender: TObject);
begin
  RootLayout.Margins.Left := TUIHelper.GetOffsetRect.Left;
end;

procedure TMainView.SelectedFile(const AFileName: string);
begin
  Caption := TPath.GetFileName(AFileName);
  PlayerView.SelectedURL(AFileName);
  SwitchTab(PlayerTab);
end;

procedure TMainView.SelectedStream(const ATitle, AURL: string);
begin
  Caption := ATitle;
  PlayerView.SelectedURL(AURL);
  SwitchTab(PlayerTab);
end;

procedure TMainView.SwitchTab(const ATab: TTabItem);
begin
  FTabStack.Push(TabControl.ActiveTab);
  TabControl.SetActiveTabWithTransitionAsync(ATab, TTabTransition.Slide, TTabTransitionDirection.Normal, TabSwitched);
end;

procedure TMainView.TabSwitched;
begin
  //
end;

procedure TMainView.GoBack;
begin
  if FTabStack.Count > 0 then
    TabControl.SetActiveTabWithTransitionAsync(FTabStack.Pop, TTabTransition.Slide, TTabTransitionDirection.Reversed, TabSwitched);
end;

procedure TMainView.PlayURLActionExecute(Sender: TObject);
begin
  Caption := TPath.GetFileName(URLEdit.Text);
  PlayerView.SelectedURL(URLEdit.Text);
  SwitchTab(PlayerTab);
end;

procedure TMainView.PlayURLActionUpdate(Sender: TObject);
begin
  PlayURLAction.Enabled := IsValidURL(URLEdit.Text);
end;

procedure TMainView.CreateViews;
begin
  TStreamsView.Create(StreamsTab);
  TFilesView.Create(FilesTab);
  TPlayerView.Create(PlayerTab);
end;

end.
