unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure WindowFocusChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FMX.Platform,
  {$IF Defined(ANDROID)} DW.StartUpHook.Android, {$ENDIF} DW.Messaging;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TWindowFocusChangedMessage, WindowFocusChangedMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TWindowFocusChangedMessage, WindowFocusChangedMessageHandler);
  inherited;
end;

procedure TForm1.WindowFocusChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if TWindowFocusChangedMessage(AMsg).Value then
    Memo1.Lines.Add('Gained focus')
  else
    Memo1.Lines.Add('Lost focus');
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      Memo1.Lines.Add('Became Active');
    TApplicationEvent.EnteredBackground:
      Memo1.Lines.Add('Entered background');
  end;
end;

end.
