unit VPD.View.Files;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.ListView.Helper,
  VPD.HostedView;

type
  TFilesView = class(TForm)
    ListView: TListView;
    RootLayout: TLayout;
    TopLayout: TLayout;
    BackButton: TSpeedButton;
    procedure ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure BackButtonClick(Sender: TObject);
    procedure ListViewResized(Sender: TObject);
  private
    procedure LoadItems;
    procedure UpdateItem(const AFileName: string; const AItem: TListViewItem);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FilesView: TFilesView;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  DW.IOUtils.Helpers,
  VPD.View.Main,
  VPD.ListView.Appearances;

type
  TFileListViewItem = class(TIDListViewItem)
  private
    function GetMaxRes: TListItemText;
    function GetImage: TListItemImage;
    function GetLocation: TListItemText;
    function GetName: TListItemText;
  public
    property Name: TListItemText read GetName;
    property Location: TListItemText read GetLocation;
    property MaxRes: TListItemText read GetMaxRes;
    property Image: TListItemImage read GetImage;
  end;

{ TFileListViewItem }

function TFileListViewItem.GetName: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('Name');
end;

function TFileListViewItem.GetLocation: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('Location');
end;

function TFileListViewItem.GetMaxRes: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('MaxRes');
end;

function TFileListViewItem.GetImage: TListItemImage;
begin
  Result := GetListItemImage('Image');
end;

{ TStreamsView }

constructor TFilesView.Create(AOwner: TComponent);
begin
  inherited;
  FilesView := Self;
  Embed(TFmxObject(AOwner));
  LoadItems;
end;

procedure TFilesView.BackButtonClick(Sender: TObject);
begin
  MainView.GoBack;
end;

procedure TFilesView.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  MainView.SelectedFile(TFileListViewItem(AItem).ID);
end;

procedure TFilesView.ListViewResized(Sender: TObject);
begin
  ListView.StretchObject('Name', 'More');
  ListView.StretchObject('Location', 'More');
end;

procedure TFilesView.LoadItems;
var
  LFileName: string;
begin
  for LFileName in TDirectory.GetFiles(TPathHelper.GetResourcesPath('Videos'), '*.*', TSearchOption.soTopDirectoryOnly) do
    UpdateItem(LFileName, ListView.Items.Add);
end;

procedure TFilesView.UpdateItem(const AFileName: string; const AItem: TListViewItem);
var
  LItem: TFileListViewItem;
begin
  LItem := TFileListViewItem(AItem);
  LItem.ID := AFileName;
  LItem.Name.Text := TPath.GetFileName(AFileName);
end;


end.
