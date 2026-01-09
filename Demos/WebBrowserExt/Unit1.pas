unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Edit,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ActnList,
  DW.WebBrowserExt;

type
  TForm1 = class(TForm)
    BottomLayout: TLayout;
    ExecJSButton: TButton;
    WebBrowser: TWebBrowser;
    TopLayout: TLayout;
    GoButton: TButton;
    URLEdit: TEdit;
    Memo: TMemo;
    PrintButton: TButton;
    ActionList: TActionList;
    PrintAction: TAction;
    WBExtExamplesButton: TButton;
    procedure ExecJSButtonClick(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
    procedure WebBrowserDidFinishLoad(ASender: TObject);
    procedure PrintActionExecute(Sender: TObject);
    procedure PrintActionUpdate(Sender: TObject);
    procedure WBExtExamplesButtonClick(Sender: TObject);
  private
    FWebBrowserExt: TWebBrowserExt;
    FIsLoggingIn: Boolean;
    FIsPageLoaded: Boolean;
    procedure WebBrowserExtDownloadStartHandler(Sender: TObject; const AUri, AFileExt: string; var AFileName: string);
    procedure WebBrowserExtDownloadStateChangeHandler(Sender: TObject; const AFileName: string; const AState: TDownloadState);
    procedure WebBrowserExtElementClickHandler(Sender: TObject; const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean);
    procedure WebBrowserExtStringMessagePostedHandler(Sender: TObject; const AData: string; var AReply: string);
    procedure WebBrowserShouldStartLoadWithRequestHandler(Sender: TObject; const AURL: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo, System.Permissions,
  System.IOUtils, System.Net.HttpClient, System.Net.URLClient,
  DW.OSLog,
  DW.Consts.Android, DW.Permissions.Helpers,
  DW.IOUtils.Helpers, DW.JavaScript, DW.Printing;

type
  TURIHelper = record helper for TURI
    function FindParamValue(const AName: string; out AValue: string): Boolean;
  end;

{ TURIHelper }

function TURIHelper.FindParamValue(const AName: string; out AValue: string): Boolean;
var
  LParam: TURIParameter;
begin
  Result := False;
  for LParam in Params do
  begin
    if LParam.Name.Equals(AName) then
    begin
      AValue := LParam.Value;
      Result := True;
      Break;
    end;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  WebBrowser.OnShouldStartLoadWithRequest := WebBrowserShouldStartLoadWithRequestHandler;
  FWebBrowserExt := TWebBrowserExt.Create(WebBrowser);
  // *** WARNING ***: Allowing ANY origins (e.g. ['*'] can be a security risk
  // Preference is to allow only origins that are known to be secure
  FWebBrowserExt.SetWebListenerParams('WBExt', ['https://delphiworlds.com']);
  // FWebBrowserExt.SetWebListenerParams('WBExt', ['*']);
  FWebBrowserExt.OnElementClick := WebBrowserExtElementClickHandler;
  FWebBrowserExt.OnDownloadStart := WebBrowserExtDownloadStartHandler;
  FWebBrowserExt.OnDownloadStateChange := WebBrowserExtDownloadStateChangeHandler;
  FWebBrowserExt.OnStringMessagePosted := WebBrowserExtStringMessagePostedHandler;
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

procedure TForm1.WBExtExamplesButtonClick(Sender: TObject);
begin
  FWebBrowserExt.ClearCache;
  URLEdit.Text := 'https://delphiworlds.com/files/Example.html';
  FIsPageLoaded := False;
  WebBrowser.Navigate(URLEdit.Text);
end;

procedure TForm1.WebBrowserShouldStartLoadWithRequestHandler(Sender: TObject; const AURL: string);
var
  LJSEvent: TJSEvent;
begin
  if LJSEvent.ParseEvent(AURL) then
    Memo.Lines.Add(LJSEvent.ToString);
end;

procedure TForm1.WebBrowserDidFinishLoad(ASender: TObject);
begin
  FIsPageLoaded := True;
  if FIsLoggingIn then
  begin
    FIsLoggingIn := False;
    Memo.Lines.Add('Responded from login attempt');
  end;
end;

procedure TForm1.WebBrowserExtDownloadStartHandler(Sender: TObject; const AUri, AFileExt: string; var AFileName: string);
begin
  // Leaving AFileName blank will prevent download
  // Including a folder name in the file name should result in the file being downloaded to that path
  // If just the filename is provided, and DefaultDownloadsFolder is not empty, that folder will be used
  // If just the filename is provided, and DefaultDownloadsFolder IS empty, the default folder for the system will be used
  // On mobile, DefaultDownloadsFolder defaults to: <DocumentsPath>\Downloads

  // The developer can decide:
  //   Which part(s) of the Uri should be used for the filename
  //   What to do if AFileExt ends up being blank (not a valid mime type, or whatever other reason)
  //   How to construct AFileName as a whole.
  Memo.Lines.Add(Format('> WebBrowserExtDownloadStartHandler - AUri: %s, AFileName: %s, AFileExt: %s', [AUri, AFileName, AFileExt]));
  // This variation provides just the filename
  if not AFileName.EndsWith(AFileExt) then
    AFileName := TPath.ChangeExtension(AFileName, AFileExt);
end;

procedure TForm1.WebBrowserExtDownloadStateChangeHandler(Sender: TObject; const AFileName: string; const AState: TDownloadState);
begin
  // Possible values for AState:
  // TDownloadState = (Completed, Failed, Paused, Pending, Running, Unknown);
  Memo.Lines.Add(Format('> WebBrowserExtDownloadStateChangeHandler: %s', [AFileName]));
end;

procedure TForm1.WebBrowserExtElementClickHandler(Sender: TObject; const AHitTestKind: THitTestKind; const AExtra: string;
  var APreventDefault: Boolean);
var
  LURI: TURI;
  LAction: string;
begin
  if AHitTestKind in [THitTestKind.SrcAnchor, THitTestKind.SrcImageAnchor] then
  begin
    Memo.Lines.Add(Format('Clicked on anchor with extra: %s', [AExtra]));
    LURI := TURI.Create(AExtra);
    // Example of how to intercept a URL click to allow your app to download the file, rather than the browser downloading it
    // The HTML in the the example (http://delphiworlds.com/files/Example.html) looks like this:
    //   <a href="http://delphiworlds.com/files/Images.zip?action=download">Download Into App</a>
    if LURI.FindParamValue('action', LAction) and LAction.ToLower.Equals('download') then
    begin
      // Clear the params
      LURI.Params := [];
      Memo.Lines.Add(Format('Download URL is: %s', [LURI.ToString]));
      APreventDefault := False;
    end;
  end;
end;

procedure TForm1.WebBrowserExtStringMessagePostedHandler(Sender: TObject; const AData: string; var AReply: string);
begin
  Memo.Lines.Add('JavaScript message received: '#13#10 + AData);
end;

procedure TForm1.ExecJSButtonClick(Sender: TObject);
begin
  // Placeholder for using the ExecuteJavaScript method of TWebBrowserExt
  FWebBrowserExt.ExecuteJavaScript(Format(cJavaScriptSetInputValueById, ['login', 'test']),
    procedure(const AJavaScriptResult: string; const AErrorCode: Integer)
    begin
      if AErrorCode = 0 then
      begin
        FWebBrowserExt.ExecuteJavaScript(Format(cJavaScriptSetInputValueById, ['senha', 'test']),
          procedure(const AJavaScriptResult: string; const AErrorCode: Integer)
          begin
            FWebBrowserExt.ExecuteJavaScript(Format(cJavaScriptClickButtonWithText, ['Acessar']),
              procedure(const AJavaScriptResult: string; const AErrorCode: Integer)
              begin
                if AErrorCode = 0 then
                  FIsLoggingIn := True
                else
                  Memo.Lines.Add(Format('ErrorCode: %d, %s', [AErrorCode, AJavaScriptResult]));
              end
            );
          end
        );
      end;
    end
  );
end;

procedure TForm1.GoButtonClick(Sender: TObject);
begin
  FIsPageLoaded := False;
  WebBrowser.Navigate(URLEdit.Text);
end;

procedure TForm1.PrintActionExecute(Sender: TObject);
begin
  // Perhaps change 'WebPage' to something more meaningful, related to the URL
  Printing.Print(FWebBrowserExt.GetPrintAdapter('WebPage'));
  // Printing can also be used to print a file, e.g
  //   Printing.Print('SomeFileName.txt');
end;

procedure TForm1.PrintActionUpdate(Sender: TObject);
begin
  PrintAction.Enabled := FIsPageLoaded;
end;

end.
