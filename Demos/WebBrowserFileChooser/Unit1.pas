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
  // https://ps.uci.edu/~franklin/doc/file_upload.html // <---- This URL and the one just below are for demoing the file chooser fix
  // http://lacliniccaonline.com.br/resultados2020/mobile
  // https://www.youtube.com/watch?v=QkBvmv8kt4U // <----- This URL is for demo-ing YouTube in full screen mode
  WebBrowser.Navigate('https://www.youtube.com/watch?v=QkBvmv8kt4U');
end;

end.
