unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.Firebase.Analytics;

type
  TForm1 = class(TForm)
    LoginEventButton: TButton;
    procedure LoginEventButtonClick(Sender: TObject);
  private
    FAnalytics: TFirebaseAnalytics;
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
  FAnalytics := TFirebaseAnalytics.Create;
end;

destructor TForm1.Destroy;
begin
  FAnalytics.Free;
  inherited;
end;

procedure TForm1.LoginEventButtonClick(Sender: TObject);
var
  LParams: TEventParams;
begin
  LParams := [];
  FAnalytics.LogEvent(TAnalyticsEvent.Login, LParams);
end;

end.
