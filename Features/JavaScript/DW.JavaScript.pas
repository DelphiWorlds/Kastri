unit DW.JavaScript;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

const
  cJSEventScheme = 'jsevent';
  cJSEventProtocol = cJSEventScheme + ':blank?';

  cJavaScriptClickAtXY = '(function() {'#13#10 +
    'var windowX = %d, windowY = %d;'#13#10 +
    'var x = windowX + (window.scrollX || window.pageXOffset)'#13#10 +
    'var y = windowY + (window.scrollY || window.pageYOffset);'#13#10 +
    'var element = document.elementFromPoint(x, y);'#13#10 +
    'while (element) {'#13#10 +
    '  if ((element.tagName !== "A") && (element.tagName !== "INPUT") && !element.hasAttribute("onclick"))'#13#10 +
    '    element = element.parentElement;'#13#10 +
    '  else'#13#10 +
    '    break;'#13#10 +
    '}'#13#10 +
    'if (element) {'#13#10 +
    '  if (element.tagName !== "INPUT") {'#13#10 +
    '    var clickEvent = new MouseEvent("click", { view: window, bubbles: true, cancelable: true, clientX: x, clientY: y });'#13#10 +
    '    element.dispatchEvent(clickEvent);'#13#10 +
    '  } else'#13#10 +
    '    element.focus();'#13#10 +
    '} else'#13#10 +
    '  console.log("No clickable element found at coordinates:", x, y);'#13#10 +
    '})()';
  cJavaScriptGetInputValueByName = '(function() { return document.getElementsByName("%s")[0].value; })()';
  cJavaScriptGetPageContents = '(function() { return document.getElementsByTagName("html")[0].innerHTML; })()';
  cJavaScriptNullResult = 'null';
  cJavaScriptSetInputValueById = '(function() { document.getElementById("%s").value = "%s"; })()';
  cJavaScriptSetInputValueByName = '(function() { '#13#10 +
    '  var element = document.querySelector("input[name=''%s'']"); '#13#10 +
    '  var inputValue = "%s"; '#13#10 +
    '  if (element && (element.offsetWidth > 0 || element.offsetHeight > 0 || element.getClientRects().length > 0)) {'#13#10 +
    '    element.value = inputValue;'#13#10 +
    '    element.setAttribute("value", inputValue);'#13#10 +
    '    element.focus(); '#13#10 +
    '    var event = new Event("input", { bubbles: true, cancelable: true });'#13#10 +
    '    element.dispatchEvent(event);'#13#10 +
    '    return element.value == inputValue; '#13#10 +
    '  } else return false; '#13#10 +
    '})()';
  cJavaScriptClickButtonWithText = '(function() { '#13#10 +
    '  var buttons = document.getElementsByTagName("button"); '#13#10 +
    '  for (var i = 0; i < buttons.length; i++) {'#13#10 +
    '    if (buttons[i].innerText.trim() === "%s") {'#13#10 +
    '      buttons[i].click();'#13#10 +
    '      return true;'#13#10 +
    '    }'#13#10 +
    '  } return false;'#13#10 +
    '})()';
  cJavaScriptRemoveBlankTargets = '(function() { '#13#10 +
    '  document.querySelectorAll("a").forEach(anchor => { '#13#10 +
    '    anchor.removeAttribute("target"); '#13#10 +
    '  }); '#13#10 +
    '})()';
  // Parameters. Using "about:" works around an issue with TWebBrowser on Android
  cJavaScriptSendEvent = 'var iframe = document.createElement("iframe"); '#13#10 +
    '  iframe.setAttribute("src", "' + cJSEventProtocol + '%s"); '#13#10 +
    '  document.documentElement.appendChild(iframe); '#13#10 +
    '  iframe.parentNode.removeChild(iframe); '#13#10 +
    '  iframe = null;';
  // Element ID, Event name, Parameters
  cJavaScriptAddEventById = '(function() { '#13#10 +
    '  document.getElementById("%s").addEventListener("%s", function() {' +
    '  console.log("Event triggered"); '#13#10 +
    cJavaScriptSendEvent + #13#10 +
    '    '#13#10 +
    '  }); '#13#10 +
    '  console.log("Event added"); '#13#10 +
    '})()';

type
  TJSEventParam = record
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
    function ToString: string;
  end;

  TJSEventParams = TArray<TJSEventParam>;

  TJSEvent = record
    Name: string;
    Params: TJSEventParams;
    function IndexOfParam(const AParamName: string): Integer;
    function TryGetValue(const AParamName: string; out AValue: Double): Boolean; overload;
    function TryGetValue(const AParamName: string; out AValue: Integer): Boolean; overload;
    function TryGetValue(const AParamName: string; out AValue: string): Boolean; overload;
    function ParseEvent(const AURL: string): Boolean;
    function ToString: string;
  end;

  TJavaScriptResultProc = reference to procedure(const JavaScriptResult: string; const ErrorCode: Integer);

function IsTrue(const AJavaScriptResult: string): Boolean;

implementation

uses
  // RTL
  System.SysUtils, System.NetEncoding;

function IsTrue(const AJavaScriptResult: string): Boolean;
begin
  Result := AJavaScriptResult.Equals('true');
end;

{ TJSEventParam }

constructor TJSEventParam.Create(const AName, AValue: string);
begin
  Name := AName.Replace('/', '');
  Value := AValue.Replace('/', '');
end;

function TJSEventParam.ToString: string;
begin
  Result := Format('%s: %s', [Name, Value]);
end;

{ TJSEvent }

function TJSEvent.ToString: string;
var
  LLines: TArray<string>;
  LParam: TJSEventParam;
begin
  LLines := [Format('Name: %s', [Name])];
  if Length(Params) > 0 then
    LLines := LLines + ['Params:'];
  for LParam in Params do
    LLines := LLines + [LParam.ToString];
  Result := string.Join(#13#10, LLines);
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: string): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    AValue := Params[LIndex].Value;
    Result := True;
  end;
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: Double): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    if TryStrToFloat(Params[LIndex].Value, AValue) then
      Result := True;
  end;
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: Integer): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    if TryStrToInt(Params[LIndex].Value, AValue) then
      Result := True;
  end;
end;

function TJSEvent.IndexOfParam(const AParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Params) to High(Params) do
  begin
    if Params[I].Name.Equals(AParamName) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TJSEvent.ParseEvent(const AURL: string): Boolean;
var
  LParts, LArgs: TArray<string>;
  LArg: string;
begin
  Result := False;
  Name := '';
  Params := [];
  if AURL.ToLower.StartsWith(cJSEventProtocol) then
  begin
    LParts := TNetEncoding.URL.Decode(AURL.Substring(cJSEventProtocol.Length)).Split([':']);
    if Length(LParts) > 0 then
    begin
      Name := LParts[0].Replace('/', '');
      if Length(LParts) > 1 then
      begin
        LArgs := LParts[1].Split(['#']);
        for LArg in LArgs do
        begin
          LParts := LArg.Split(['=']);
          if Length(LParts) > 1 then
            Params := Params + [TJSEventParam.Create(LParts[0], LParts[1])];
        end;
      end;
      Result := True;
    end;
  end;
end;

end.
