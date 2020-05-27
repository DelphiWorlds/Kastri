unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.OSTimer;

type
  TfrmMain = class(TForm)
    ClockLabel: TLabel;
    TopLayout: TLayout;
    StartStopButton: TButton;
    procedure StartStopButtonClick(Sender: TObject);
  private
    FTimer: TOSTimer;
    procedure TimerIntervalHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TOSTimer.Create;
  FTimer.Interval := 50;
  FTimer.OnInterval := TimerIntervalHandler;
  FTimer.Enabled := False;
end;

destructor TfrmMain.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TfrmMain.StartStopButtonClick(Sender: TObject);
begin
  FTimer.Enabled := not FTimer.Enabled;
  if FTimer.Enabled then
    StartStopButton.Text := 'Stop'
  else
    StartStopButton.Text := 'Start';
end;

procedure TfrmMain.TimerIntervalHandler(Sender: TObject);
begin
  ClockLabel.Text := FormatDateTime('hh:nn:ss.zzz', Now);
end;

end.
