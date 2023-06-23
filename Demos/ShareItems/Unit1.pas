unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  DW.ShareItems;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private
    FExcluded: TShareActivities;
    FShareItems: TShareItems;
    procedure Share;
    procedure ShareItemsShareCompletedHandler(Sender: TObject; const AActivity: TShareActivity; const AError: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Permissions,
  DW.Consts.Android, DW.Permissions.Helpers;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // Exclude all but the following - Note: applicable to iOS ONLY - uncomment the next line to test
  // FExcluded := TShareItems.AllShareActivities - [TShareActivity.Message, TShareActivity.Mail, TShareActivity.CopyToPasteboard];
  FShareItems := TShareItems.Create;
  FShareItems.OnShareCompleted := ShareItemsShareCompletedHandler;
end;

destructor TForm1.Destroy;
begin
  FShareItems.Free;
  inherited;
end;

procedure TForm1.ShareItemsShareCompletedHandler(Sender: TObject; const AActivity: TShareActivity; const AError: string);
begin
  // If AActivity is TShareActivity.None, then the user cancelled - except for Android because it does not tell you :-/
  if AActivity = TShareActivity.None then
    ShowMessage('Share cancelled')
  else
    ShowMessage('Share completed');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PermissionsService.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        Share;
    end
  );
end;

procedure TForm1.Share;
var
  LSharedFileName: string;
begin
  FShareItems.Clear;
  LSharedFileName := TPath.Combine('Files', 'Lorem.txt');
  // FShareItems.AddText('Share Test');
  // Uncomment the following line and comment out the next one, to test sharing of a file
  // FShareItems.AddFile(TPath.Combine(TPath.GetDocumentsPath, LSharedFileName));
  FShareItems.AddImage(Image1.Bitmap); // On Android, don't attempt to share an image as well as text
  // FShareItems.AddImage(Image1.Bitmap); // On Android, don't attempt to share an image as well as text
  FShareItems.Share(Button1, FExcluded);
end;

end.
