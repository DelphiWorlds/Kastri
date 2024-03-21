unit DW.JavaScript.WebView2;

interface

uses
  Winapi.WebView2,
  DW.JavaScript;

type
  TJavaScriptResultCallback = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
  private
    FHandler: TJavaScriptResultProc;
  public
    { ICoreWebView2ExecuteScriptCompletedHandler }
    function Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult; stdcall;
  public
    constructor Create(const AHandler: TJavaScriptResultProc);
  end;

implementation

{ TJavaScriptResultCallback }

constructor TJavaScriptResultCallback.Create(const AHandler: TJavaScriptResultProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TJavaScriptResultCallback.Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult;
begin
  Result := S_OK;
  if Assigned(FHandler) then
    FHandler(string(resultObjectAsJson), errorCode);
end;


end.
