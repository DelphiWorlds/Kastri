unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.AgeStatus;

type
  TForm1 = class(TForm)
    CheckButton: TButton;
    Memo1: TMemo;
    FakeCheckButton: TButton;
    procedure CheckButtonClick(Sender: TObject);
    procedure FakeCheckButtonClick(Sender: TObject);
  private
    procedure AgeStatusCheckHandler(const AStatusResult: TAgeStatusResult);
    procedure FakeCheckExample2;
    procedure FakeCheckExample3;
    procedure FakeCheckExample4;
    procedure FakeCheckExample5;
    procedure FakeCheckExample6;
    procedure SafeAreaChangedHandler(Sender: TObject; const AInsets: TRectF);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  cAgeStatusResultKindCaptions: array[TAgeStatusResultKind] of string = (
    'Success', 'Access Request Failed', 'Age Status Check Failed', 'Age Status Not Shared', 'Age Status Unspecified', 'Verification Required for Age Status'
  );
  cSignificantChangeStatusCaptions: array[TSignificantChangeStatus] of string = (
    'None', 'Unspecified', 'Approved', 'Pending', 'Declined'
  );

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  {$IF CompilerVersion > 36}
  OnSafeAreaChanged := SafeAreaChangedHandler;
  {$ENDIF}
end;

procedure TForm1.SafeAreaChangedHandler(Sender: TObject; const AInsets: TRectF);
begin
  Padding.Rect := AInsets;
end;

procedure TForm1.AgeStatusCheckHandler(const AStatusResult: TAgeStatusResult);
var
  LDateString: string;
begin
  case AStatusResult.Kind of
    TAgeStatusResultKind.Success:
    begin
      Memo1.Lines.Add(Format('Age range - Lower: %d, Upper: %d', [AStatusResult.AgeLower, AStatusResult.AgeUpper]));
      Memo1.Lines.Add(Format('Significant Change Status: %s', [cSignificantChangeStatusCaptions[AStatusResult.SignificantChangeStatus]]));
      LDateString := '(none)';
      if AStatusResult.SignificantChangeApprovalDate > 0  then
        LDateString := FormatDateTime('dd-MMM-yyyy', AStatusResult.SignificantChangeApprovalDate);
      Memo1.Lines.Add(Format('Significant Change Approval Date: %s', [LDateString]));
    end;
  else
    Memo1.Lines.Add(Format('Age Status Check failed - Kind: %s, Message: %s', [cAgeStatusResultKindCaptions[AStatusResult.Kind], AStatusResult.StatusMessage]));
  end;
end;

procedure TForm1.CheckButtonClick(Sender: TObject);
begin
  AgeStatus.Check(AgeStatusCheckHandler);
end;

procedure TForm1.FakeCheckButtonClick(Sender: TObject);
begin
  FakeCheckExample6;
end;

procedure TForm1.FakeCheckExample2;
begin
  // https://developer.android.com/google/play/age-signals/test-age-signals-api#request-access-age-not-shared
  AgeStatus.FakeCheck(TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.NotShared,
    TAgeRangeSource.Unspecified, // User's age is managed by a parent or a guardian.
    -1,
    -1,
    TSignificantChangeStatus.Unspecified,
    0
  ), AgeStatusCheckHandler);
end;

procedure TForm1.FakeCheckExample3;
begin
  // https://developer.android.com/google/play/age-signals/test-age-signals-api#verified-adult
  AgeStatus.FakeCheck(TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.Shared,
    TAgeRangeSource.TierD, // User's age is checked by using a combination of Government ID and selfie assessment, or Digital ID.
    18,
    -1,
    TSignificantChangeStatus.None,
    0
  ), AgeStatusCheckHandler);
end;

procedure TForm1.FakeCheckExample4;
begin
  // https://developer.android.com/google/play/age-signals/test-age-signals-api#supervised-minor
  AgeStatus.FakeCheck(TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.Shared,
    TAgeRangeSource.TierB, // User's age is managed by a parent or a guardian.
    13,
    15,
    TSignificantChangeStatus.None,
    0
  ), AgeStatusCheckHandler);
end;

procedure TForm1.FakeCheckExample5;
begin
  // https://developer.android.com/google/play/age-signals/test-age-signals-api#supervised-minor-pending-change
  AgeStatus.FakeCheck(TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.Shared,
    TAgeRangeSource.TierB, // User's age is managed by a parent or a guardian.
    13,
    15,
    TSignificantChangeStatus.Pending,
    0
  ), AgeStatusCheckHandler);
end;

procedure TForm1.FakeCheckExample6;
begin
  // https://developer.android.com/google/play/age-signals/test-age-signals-api#supervised-minor-approved-change
  AgeStatus.FakeCheck(TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.Shared,
    TAgeRangeSource.TierB, // User's age is managed by a parent or a guardian.
    13,
    15,
    TSignificantChangeStatus.Approved,
    EncodeDate(2026, 5, 1)
  ), AgeStatusCheckHandler);
end;

end.
