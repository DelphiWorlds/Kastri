unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.SMS;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    RecipientsMemo: TMemo;
    Label2: TLabel;
    TextMemo: TMemo;
    SendButton: TButton;
    MessagesMemo: TMemo;
    Label3: TLabel;
    procedure SendButtonClick(Sender: TObject);
  private
    FSMS: TSMS;
    procedure SMSPermissionRequestResultHandler(Sender: TObject; const AIsGranted: Boolean);
    procedure SMSMessageResultHandler(Sender: TObject; const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
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
  // SendButton.Enabled := False;
  FSMS := TSMS.Create;
  FSMS.OnMessageResult := SMSMessageResultHandler;
  FSMS.OnPermissionRequestResult := SMSPermissionRequestResultHandler;
  FSMS.UseIntents := True;
  // FSMS.RequestPermission;
end;

destructor TForm1.Destroy;
begin
  FSMS.Free;
  inherited;
end;

procedure TForm1.SMSMessageResultHandler(Sender: TObject; const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
const
  cMessageResultCaptions: array[TMessageResult] of string = ('cancelled', 'failed', 'sent', 'could not send');
begin
  MessagesMemo.Lines.Add(Format('Message to %s: %s', [string.Join(', ', ADestinations), cMessageResultCaptions[AMessageResult]]));
end;

procedure TForm1.SMSPermissionRequestResultHandler(Sender: TObject; const AIsGranted: Boolean);
begin
  SendButton.Enabled := AIsGranted;
end;

procedure TForm1.SendButtonClick(Sender: TObject);
begin
  FSMS.SendTextMessage(TextMemo.Text, RecipientsMemo.Lines.ToStringArray);
end;

end.
