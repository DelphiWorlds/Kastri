unit FSD.MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TfrmMain = class(TForm)
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure SendCommand(const ACommand: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.Android.Service,
  Androidapi.Helpers,
  FMX.Platform,
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager,
  FSD.Consts;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TLocalServiceConnection.StartService('ForegroundService');
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TfrmMain.Destroy;
begin
  //
  inherited;
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      SendCommand(cServiceCommandAppBecameActive);
    end;
    TApplicationEvent.EnteredBackground:
    begin
      SendCommand(cServiceCommandAppEnteredBackground);
    end;
  end;
end;

procedure TfrmMain.SendCommand(const ACommand: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceCommandAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamCommand), ACommand);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

end.
