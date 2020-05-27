unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus, System.ImageList, FMX.ImgList,
  DW.StatusBarMenu.Mac;

type
  TfrmMain = class(TForm)
    ImageList: TImageList;
    StatusBarPopupMenu: TPopupMenu;
    TestMenuItem: TMenuItem;
    StatusBarMenuItem: TMenuItem;
    SeparatorMenuItem1: TMenuItem;
    QuitMenuItem: TMenuItem;
    TestRefreshMenuItem: TMenuItem;
    procedure QuitMenuItemClick(Sender: TObject);
    procedure TestMenuItemClick(Sender: TObject);
    procedure TestRefreshMenuItemClick(Sender: TObject);
  private
    FStatusBarMenu: TStatusBarMenu;
  public
    constructor Create(AOwner: TComponent); override;
    function CanShow: Boolean; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Macapi.AppKit;

function SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FStatusBarMenu := TStatusBarMenu.Create(Self);
  FStatusBarMenu.PopupMenu := StatusBarPopupMenu;
end;

function TfrmMain.CanShow: Boolean;
begin
  Result := False; // Prevent this form from showing
end;

procedure TfrmMain.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.TestMenuItemClick(Sender: TObject);
begin
  // Make sure the app is active
  SharedApplication.activateIgnoringOtherApps(True);
  ShowMessage('Test');
end;

procedure TfrmMain.TestRefreshMenuItemClick(Sender: TObject);
begin
  if TestMenuItem.Text.Equals('Test') then
    TestMenuItem.Text := 'Test B'
  else
    TestMenuItem.Text := 'Test';
  FStatusBarMenu.Refresh;
end;

end.
