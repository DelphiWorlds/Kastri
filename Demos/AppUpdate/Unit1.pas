unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  DW.AppUpdate;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    ButtonsLayout: TLayout;
    CheckUpdateButton: TButton;
    UpdateButton: TButton;
    procedure CheckUpdateButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    FAppUpdate: TAppUpdate;
    procedure AppUpdateInfoHandler(Sender: TObject; const AInfo: TAppUpdateInfo);
    procedure AppUpdateResultHandler(Sender: TObject; const AUpdateResult: TAppUpdateResult);
    procedure AppUpdateStartedFlowHandler(Sender: TObject; const AStarted: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAppUpdate := TAppUpdate.Create;
  FAppUpdate.OnAppUpdateInfo := AppUpdateInfoHandler;
  FAppUpdate.OnAppUpdateStartedFlow := AppUpdateStartedFlowHandler;
  FAppUpdate.OnAppUpdateResult := AppUpdateResultHandler;
end;

destructor TForm1.Destroy;
begin
  FAppUpdate.Free;
  inherited;
end;

procedure TForm1.AppUpdateInfoHandler(Sender: TObject; const AInfo: TAppUpdateInfo);
begin
  Memo.Lines.Add('Available: ' + BoolToStr(AInfo.Available, True));
  if AInfo.Available then
  begin
    Memo.Lines.Add('Total Bytes: ' + AInfo.TotalBytesToDownload.ToString);
    Memo.Lines.Add('Priority: ' + AInfo.Priority.ToString);
    Memo.Lines.Add('Immediate: ' + BoolToStr(AInfo.Immediate, True));
    Memo.Lines.Add('Flexible: ' + BoolToStr(AInfo.Flexible, True));
    if AInfo.Flexible then
      Memo.Lines.Add('Staleness Days: ' + AInfo.StalenessDays.ToString);
  end;
end;

procedure TForm1.AppUpdateResultHandler(Sender: TObject; const AUpdateResult: TAppUpdateResult);
begin
  Memo.Lines.Add('Update result: ' + GetEnumName(TypeInfo(TAppUpdateResult), Ord(AUpdateResult)));
end;

procedure TForm1.AppUpdateStartedFlowHandler(Sender: TObject; const AStarted: Boolean);
begin
  Memo.Lines.Add('Flow started: ' + BoolToStr(AStarted, True));
end;

procedure TForm1.CheckUpdateButtonClick(Sender: TObject);
begin
  FAppUpdate.CheckForUpdate;
end;

procedure TForm1.UpdateButtonClick(Sender: TObject);
begin
  // NOTE: You should call CheckForUpdate first, and in the response (in this case: AppUpdateInfoHandler)
  //   check what update types are available, and pass the appropriate type to the StartUpdate call
  FAppUpdate.StartUpdate(TAppUpdateType.Immediate);
end;

end.
