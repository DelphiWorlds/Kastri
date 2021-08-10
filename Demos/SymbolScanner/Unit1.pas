unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  DW.SymbolScanner;

type
  TForm1 = class(TForm)
    CodeValueLabel: TLabel;
    CodeValueEdit: TEdit;
    Memo: TMemo;
    LabelTypeLabel: TLabel;
    LabelTypeEdit: TEdit;
  private
    FScanner: TSymbolScanner;
    procedure ScannerDataReceivedHandler(Sender: TObject; const AData, ALabelType: string);
    procedure ScannerStatusChangeHandler(Sender: TObject; const AStatus: TScannerStatus);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FScanner := TSymbolScanner.Create;
  FScanner.OnDataReceived := ScannerDataReceivedHandler;
  FScanner.OnStatusChange := ScannerStatusChangeHandler;
  FScanner.IsActive := True;
end;

procedure TForm1.ScannerDataReceivedHandler(Sender: TObject; const AData, ALabelType: string);
begin
  CodeValueEdit.Text := AData;
  LabelTypeEdit.Text := ALabelType;
end;

procedure TForm1.ScannerStatusChangeHandler(Sender: TObject; const AStatus: TScannerStatus);
begin
  Memo.Lines.Add('Scanner status: ' + cScannerStatusCaptions[AStatus]);
end;

end.
