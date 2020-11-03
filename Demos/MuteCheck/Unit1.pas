unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  DW.MuteCheck;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    FMuteCheck: TMuteCheck;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FMuteCheck := TMuteCheck.Create;
  // Not required on Android
  FMuteCheck.SoundFileName := TPath.Combine(TPath.GetDocumentsPath, 'mute.mp3');
end;

destructor TForm1.Destroy;
begin
  FMuteCheck.Free;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FMuteCheck.Check(
    procedure(const AIsMuted: Boolean)
    begin
      if AIsMuted then
        Label1.Text := 'Device Muted'
      else
        Label1.Text := 'Device Not Muted';
    end
  );
end;

end.
