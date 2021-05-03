unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects, FMX.TabControl, System.Actions, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions,
  DW.BarcodeReader;

type
  TForm1 = class(TForm)
    BottomLayout: TLayout;
    ScanButton: TButton;
    ScanImage: TImage;
    Memo: TMemo;
    TabControl: TTabControl;
    ImageTab: TTabItem;
    ActionList: TActionList;
    TakePhotoFromCameraAction: TTakePhotoFromCameraAction;
    procedure ScanButtonClick(Sender: TObject);
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
  private
    FReader: TBarcodeReader;
    procedure ReaderBarcodeHandler(Sender: TObject; const ABarcodes: TBarcodes; const AError: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Permissions,
  DW.Consts.Android, DW.Permissions.Helpers;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FReader := TBarcodeReader.Create;
  FReader.Formats := [TBarcodeFormat.All];
  FReader.OnBarcode := ReaderBarcodeHandler;
end;

destructor TForm1.Destroy;
begin
  FReader.Free;
  inherited;
end;

procedure TForm1.ReaderBarcodeHandler(Sender: TObject; const ABarcodes: TBarcodes; const AError: string);
var
  LBarcode: TBarcode;
begin
  Memo.Lines.Clear;
  for LBarcode in ABarcodes do
    Memo.Lines.Add(Format('%s - (%s)', [LBarcode.Value, LBarcode.FormatDescription]));
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if AGrantResults.AreAllGranted then
        TakePhotoFromCameraAction.ExecuteTarget(ScanButton);
    end
  );
end;

procedure TForm1.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
begin
  ScanImage.Bitmap.Assign(Image);
  FReader.Scan(Image);
end;

end.
