unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.PDFControl;

type
  TForm1 = class(TForm)
    WebBrowser: TWebBrowser;
    BottomLayout: TLayout;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FPDFControl: TPDFControl;
    procedure LoadBrowser(const AFileName: string);
    procedure LoadPDF;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  {$IF Defined(IOS)}
  Macapi.Helpers,
  iOSapi.WebKit, iOSapi.Foundation,
  {$ENDIF}
  // DW
  DW.IOUtils.Helpers;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  if TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid then
  begin
    WebBrowser.Visible := False;
    FPDFControl := TPDFControl.Create(Self);
    FPDFControl.Align := TAlignLayout.Client;
    FPDFControl.Parent := Self;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadPDF;
end;

procedure TForm1.LoadBrowser(const AFileName: string);
{$IF Defined(IOS)}
var
  LWebView: WKWebView;
  LURL: NSURL;
{$ENDIF}
begin
  {$IF Defined(IOS)}
  if Supports(WebBrowser, WKWebView, LWebView) then
  begin
    LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AFileName)));
    LWebView.loadFileURL(LURL, LURL);
  end;
  {$ELSE}
  WebBrowser.Navigate(AFileName);
  {$ENDIF}
end;

procedure TForm1.LoadPDF;
var
  LFileName: string;
begin
  {$IF Defined(MSWINDOWS)}
  LFileName := TPath.Combine(TPathHelper.GetAppPath, '..\..\FeatureMatrix.pdf');
  {$ELSE}
  LFileName := TPath.Combine(TPath.GetDocumentsPath, 'FeatureMatrix.pdf');
  {$ENDIF}
  if FPDFControl <> nil then
    FPDFControl.LoadPDF(LFileName)
  else
    LoadBrowser(LFileName);
end;

end.
