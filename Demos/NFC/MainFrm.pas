unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  DW.NFC;

type
  TfrmMain = class(TForm)
    ButtonsLayout: TLayout;
    StartButton: TButton;
    LogMemo: TMemo;
    procedure StartButtonClick(Sender: TObject);
  private
    FNFCReader: TNFCReader;
    procedure NFCReaderResultHandler(Sender: TObject; const ANFCResult: TNFCResult);
    procedure NFCReaderErrorHandler(Sender: TObject; const AError: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  DW.OSLog;

const
  cNFCTagId = 'Tag ID: %s';
  cNFCMessagePayload = 'Payload - id: %s, payload: %s';

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FNFCReader := TNFCReader.Create;
  FNFCReader.OnResult := NFCReaderResultHandler;
  FNFCReader.OnError := NFCReaderErrorHandler;
  FNFCReader.AlertMessage := 'Hold an NFC tag near the phone..';
end;

procedure TfrmMain.NFCReaderResultHandler(Sender: TObject; const ANFCResult: TNFCResult);
var
  I, J: Integer;
  LPayload: TNFCPayload;
  LTechnology: TNFCTechnology;
begin
  LogMemo.Lines.Add(Format(cNFCTagId, [ANFCResult.TagInfo.ID]));
  if Length(ANFCResult.TagInfo.Technologies) > 0 then
    LogMemo.Lines.Add('Has the following techs:');
  for LTechnology in ANFCResult.TagInfo.Technologies do
    LogMemo.Lines.Add('> ' + GetEnumName(TypeInfo(TNFCTechnologyKind), Ord(LTechnology.Kind)));
  if Length(ANFCResult.Messages) > 0 then
  begin
    LogMemo.Lines.Add('Messages:');
    for I := 0 to Length(ANFCResult.Messages) - 1 do
    begin
      for J := 0 to Length(ANFCResult.Messages[I].Payloads) - 1 do
      begin
        LPayload := ANFCResult.Messages[I].Payloads[J];
        LogMemo.Lines.Add(Format(cNFCMessagePayload, [LPayload.Identifier, LPayload.Payload]));
      end;
    end;
  end
  else
    LogMemo.Lines.Add('No messages');
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
