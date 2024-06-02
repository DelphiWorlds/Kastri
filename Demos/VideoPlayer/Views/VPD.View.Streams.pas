unit VPD.View.Streams;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.ListView.Helper,
  VPD.HostedView, VPD.Types;

type
  TStreamsView = class(TForm)
    ListView: TListView;
    RootLayout: TLayout;
    TopLayout: TLayout;
    BackButton: TSpeedButton;
    procedure ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure BackButtonClick(Sender: TObject);
    procedure ListViewResized(Sender: TObject);
  private
    procedure LoadItems;
    procedure UpdateItem(const AHLSStream: THLSStreamItem; const AItem: TListViewItem);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  StreamsView: TStreamsView;

implementation

{$R *.fmx}

uses
  VPD.View.Main,
  DW.IOUtils.Helpers,
  VPD.ListView.Appearances;

type
  THLSStreamListViewItem = class(TIDListViewItem)
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

{ THLSStreamListViewItem }

function THLSStreamListViewItem.GetName: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('Name');
end;

function THLSStreamListViewItem.GetLocation: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('Location');
end;

function THLSStreamListViewItem.GetMaxRes: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('MaxRes');
end;

function THLSStreamListViewItem.GetImage: TListItemImage;
begin
  Result := GetListItemImage('Image');
end;

{ TStreamsView }

constructor TStreamsView.Create(AOwner: TComponent);
begin
  inherited;
  StreamsView := Self;
  {$IF Defined(ANDROID)}
  TopLayout.Visible := False;
  {$ENDIF}
  Embed(TFmxObject(AOwner));
  LoadItems;
end;

procedure TStreamsView.BackButtonClick(Sender: TObject);
begin
  MainView.GoBack;
end;

procedure TStreamsView.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  LHLSItem: THLSStreamListViewItem;
begin
  LHLSItem := THLSStreamListViewItem(AItem);
  MainView.SelectedStream(LHLSItem.Name.Text, LHLSItem.ID);
end;

procedure TStreamsView.ListViewResized(Sender: TObject);
begin
  ListView.StretchObject('Name', 'More');
  ListView.StretchObject('Location', 'More');
end;

procedure TStreamsView.LoadItems;
var
  LStreams: THLSStreams;
  LItem: THLSStreamItem;
begin
  LStreams.LoadFromFile(TPathHelper.GetResourcesFile('Streams.json'));
  for LItem in LStreams.Items do
    UpdateItem(LItem, ListView.Items.Add);
end;

procedure TStreamsView.UpdateItem(const AHLSStream: THLSStreamItem; const AItem: TListViewItem);
var
  LItem: THLSStreamListViewItem;
begin
  LItem := THLSStreamListViewItem(AItem);
  LItem.ID := AHLSStream.URL;
  LItem.Name.Text := AHLSStream.Channel;
  LItem.Location.Text := AHLSStream.Location;
  // LItem.MaxRes.Text := AHLSStream.MaxResolution;
  // LItem.Image.Bitmap.AsCompressedBase64 := AHLSStream.Picture;
end;


end.
