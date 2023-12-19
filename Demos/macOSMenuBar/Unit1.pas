unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.ImageList, FMX.ImgList,
  DW.Menus.Mac;

type
  TForm1 = class(TForm)
    ImageList: TImageList;
  private
    FCertsProfilesItem: TPlatformMenuItem;
    FCreateInstallerItem: TPlatformMenuItem;
    FIsActive: Boolean;
    FMenu: TPlatformMenu;
    FMenuImageSize: TSizeF;
    FMessagesItem: TPlatformMenuItem;
    FNotarizeItem: TPlatformMenuItem;
    FOptionsItem: TPlatformMenuItem;
    FPAServersConfigItem: TPlatformMenuItem;
    FPAServersItem: TPlatformMenuItem;
    FPAServersSepItem: TPlatformMenuItem;
    FQuitItem: TPlatformMenuItem;
    FStatusItem: TPlatformStatusItem;
    FStartStopItem: TPlatformMenuItem;
    procedure CertsProfilesItemExecuteHandler(Sender: TObject);
    procedure CreateInstallerItemExecuteHandler(Sender: TObject);
    procedure CreateMenu;
    function GetMenuImage(const AIndex: Integer): TBitmap;
    procedure MessagesItemExecuteHandler(Sender: TObject);
    procedure NotarizeItemExecuteHandler(Sender: TObject);
    procedure OptionsItemExecuteHandler(Sender: TObject);
    procedure PAServersConfigItemClickHandler(Sender: TObject);
    procedure PAServersItemClickHandler(Sender: TObject);
    procedure QuitItemExecuteHandler(Sender: TObject);
    procedure StartStopItemExecuteHandler(Sender: TObject);
    procedure UpdatePAServersMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanShow: Boolean; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Macapi.AppKit;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FMenuImageSize := TSizeF.Create(16, 16);
  CreateMenu;
end;

destructor TForm1.Destroy;
begin
  FStatusItem.Free;
  FMenu.Free;
  inherited;
end;

procedure TForm1.CreateMenu;
begin
  FMenu := TPlatformMenu.Create;
  FStatusItem := TPlatformStatusItem.Create(FMenu.Menu);
  FStatusItem.SetImage(GetMenuImage(0));
  FStartStopItem := FMenu.CreateItem('Start', GetMenuImage(1), StartStopItemExecuteHandler);
  FOptionsItem := FMenu.CreateItem('Options', GetMenuImage(3), OptionsItemExecuteHandler);
  FCertsProfilesItem := FMenu.CreateItem('View Certs & Profiles', GetMenuImage(5), CertsProfilesItemExecuteHandler);
  FMessagesItem := FMenu.CreateItem('View Messages', GetMenuImage(4), MessagesItemExecuteHandler);
  FMenu.CreateSeparator;
  FCreateInstallerItem := FMenu.CreateItem('Create Installer..', CreateInstallerItemExecuteHandler);
  FNotarizeItem := FMenu.CreateItem('Notarize..', NotarizeItemExecuteHandler);
  FMenu.CreateSeparator;
  FPAServersItem := FMenu.CreateItem('PAServers');
  FMenu.CreateSeparator;
  FQuitItem := FMenu.CreateItem('Quit', QuitItemExecuteHandler);
  UpdatePAServersMenu;
end;

function TForm1.CanShow: Boolean;
begin
  // Prevents this form from showing
  Result := False;
end;

function TForm1.GetMenuImage(const AIndex: Integer): TBitmap;
begin
  Result := ImageList.Bitmap(FMenuImageSize, AIndex);
end;

procedure TForm1.UpdatePAServersMenu;
var
  I: Integer;
begin
  FPAServersItem.Clear;
  for I := 22 to 23 do
    FPAServersItem.CreateSubItem('PAServer ' + I.ToString, PAServersItemClickHandler);
  FPAServersSepItem := FPAServersItem.CreateSeparator;
  FPAServersConfigItem := FPAServersItem.CreateSubItem('Configure', PAServersConfigItemClickHandler);
  // In Mosco, the value FPAServersSepItem.Visible would be set depending on whether there were any PAServer subitems created
end;

procedure TForm1.CertsProfilesItemExecuteHandler(Sender: TObject);
begin
  // Show the certs/profiles view
end;

procedure TForm1.CreateInstallerItemExecuteHandler(Sender: TObject);
begin
  // Show the create installer view
end;

procedure TForm1.MessagesItemExecuteHandler(Sender: TObject);
begin
  // Show the messages view
end;

procedure TForm1.NotarizeItemExecuteHandler(Sender: TObject);
begin
  // Show the notarize view
end;

procedure TForm1.OptionsItemExecuteHandler(Sender: TObject);
begin
  // Show options dialog
end;

procedure TForm1.PAServersConfigItemClickHandler(Sender: TObject);
begin
  // Show PAServers config dialog
end;

procedure TForm1.PAServersItemClickHandler(Sender: TObject);
begin
  // Run the selected PAServer
end;

procedure TForm1.QuitItemExecuteHandler(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.StartStopItemExecuteHandler(Sender: TObject);
begin
  FIsActive := not FIsActive;
  if FIsActive then
  begin
    FStartStopItem.Title := 'Stop';
    FStartStopItem.SetImage(GetMenuImage(2));
  end
  else
  begin
    FStartStopItem.Title := 'Start';
    FStartStopItem.SetImage(GetMenuImage(1));
  end;
end;

function SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;

initialization
  // Hides the application from the "Dock"
  //   https://stackoverflow.com/a/9220857/3164070
  SharedApplication.setActivationPolicy(NSApplicationActivationPolicyAccessory);

end.
