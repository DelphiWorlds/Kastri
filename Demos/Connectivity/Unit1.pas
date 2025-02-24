unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  DW.Connectivity;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
  private
    FConnectivity: TConnectivity;
    procedure ConnectivityChangeHandler(Sender: TObject; const AIsConnected: Boolean);
    procedure DumpIPAddresses;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  if TConnectivity.IsConnectedToInternet then
  begin
    Memo1.Lines.Add('Device is connected to the internet');
    if TConnectivity.IsWifiInternetConnection then
      Memo1.Lines.Add('via Wifi');
  end
  else
    Memo1.Lines.Add('Device is NOT connected to the internet');
  DumpIPAddresses;
  FConnectivity := TConnectivity.Create;
  FConnectivity.OnConnectivityChange := ConnectivityChangeHandler;
end;

destructor TForm1.Destroy;
begin
  FConnectivity.Free;
  inherited;
end;

procedure TForm1.DumpIPAddresses;
var
  LAddress: TIPAddress;
begin
  for LAddress in TConnectivity.GetLocalAddresses do
  begin
    if LAddress.Version = TIPVersion.IPv4 then
      Memo1.Lines.Add(LAddress.IP);
  end;
  for LAddress in TConnectivity.GetLocalAddresses do
  begin
    if LAddress.Version = TIPVersion.IPv6 then
      Memo1.Lines.Add(LAddress.IP);
  end;
end;

procedure TForm1.ConnectivityChangeHandler(Sender: TObject; const AIsConnected: Boolean);
begin
  if AIsConnected then
    Memo1.Lines.Add('Connected!')
  else
    Memo1.Lines.Add('Disconnected!');
end;

end.
