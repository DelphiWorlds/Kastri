unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  DW.WebChromeClient.Android;

type
  TForm1 = class(TForm)
    WebBrowser: TWebBrowser;
  private
    FManager: TWebChromeClientManager;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.JNI.Webkit,
  DW.Androidapi.JNI.DWWebChromeClient;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FManager := TWebChromeClientManager.Create(WebBrowser);
  WebBrowser.Navigate('https://ps.uci.edu/~franklin/doc/file_upload.html'); // http://lacliniccaonline.com.br/resultados2020/mobile
end;

end.
