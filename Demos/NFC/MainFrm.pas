unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  DW.NFC;

type
  TfrmMain = class(TForm)
    ButtonsLayout: TLayout;
    StartButton: TButton;
    LogMemo: TMemo;
    procedure StartButtonClick(Sender: TObject);
  private
    FNFCReader: TNFCReader;
    procedure NFCReaderDetectedNDEFsHandler(Sender: TObject; const AMessages: TNFCMessages);
    procedure NFCReaderErrorHandler(Sender: TObject; const AError: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  DW.OSLog;

const
  cNFCMessagePayload = 'Payload - id: %s, payload: %s';

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FNFCReader := TNFCReader.Create;
  FNFCReader.OnDetectedNDEFs := NFCReaderDetectedNDEFsHandler;
  FNFCReader.OnError := NFCReaderErrorHandler;
  FNFCReader.AlertMessage := 'Hold an NFC tag near the phone..';
end;

procedure TfrmMain.NFCReaderDetectedNDEFsHandler(Sender: TObject; const AMessages: TNFCMessages);
var
  I, J: Integer;
  LPayload: TNFCPayload;
begin
  for I := 0 to Length(AMessages) - 1 do
  begin
    for J := 0 to Length(AMessages[I].Payloads) - 1 do
    begin
      LPayload := AMessages[I].Payloads[J];
      LogMemo.Lines.Add(Format(cNFCMessagePayload, [LPayload.Identifier, LPayload.Payload]))
    end;
  end;
end;

procedure TfrmMain.NFCReaderErrorHandler(Sender: TObject; const AError: string);
begin
  if not FNFCReader.IsActive then
    StartButton.Text := 'Start';
  LogMemo.Lines.Add('Error: ' + AError);
end;

procedure TfrmMain.StartButtonClick(Sender: TObject);
begin
  if FNFCReader.IsActive then
  begin
    FNFCReader.EndSession;
    StartButton.Text := 'Start';
  end
  else
  begin
    FNFCReader.BeginSession;
    StartButton.Text := 'Stop';
  end;
end;

end.
