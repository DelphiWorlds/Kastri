unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  DW.WebChromeClient, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    WebBrowser: TWebBrowser;
    UseCameraCheckBox: TCheckBox;
  private
    FManager: TWebChromeClientManager;
    procedure ManagerFileChooserHandler(Sender: TObject; const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FManager := TWebChromeClientManager.Create(WebBrowser);
  // FileCachePath is a folder where files of photos that are taken are stored. If you do not specify a path, the camera roll area is used
  // Using a value for FileCachePath gives an opportunity to control whether photos that are uploaded can be easily deleted
  FManager.FileCachePath := TPath.Combine(TPath.GetTempPath, 'Cache');
  FManager.OnFileChooser := ManagerFileChooserHandler;
  // https://ps.uci.edu/~franklin/doc/file_upload.html // <---- This URL and the one just below are for demoing the file chooser fix
  // https://west-wind.com/wconnect/wcscripts/fileupload.wwd 
  // https://www.youtube.com/watch?v=QkBvmv8kt4U // <----- This URL is for demo-ing YouTube in full screen mode
  WebBrowser.Navigate('https://www.w3docs.com/tools/code-editor/11957');
end;

procedure TForm1.ManagerFileChooserHandler(Sender: TObject; const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
var
  LMimeType: string;
  LHasImageType: Boolean;
begin
  LHasImageType := False;
  for LMimeType in AMimeTypes do
  begin
    if LMimeType.StartsWith('image/') then
    begin
      LHasImageType := True;
      Break;
    end;
  end;
  if LHasImageType then
  begin
    // NOTE: Set AFileChooserKind to TFileChooserKind.VisualMedia if you want the user to be able to select photos/videos from their gallery ONLY
    if UseCameraCheckBox.IsChecked then
      AFileChooserKind := TFileChooserKind.Camera
    else
      AFileChooserKind := TFileChooserKind.VisualMedia;
  end;

end;

end.
