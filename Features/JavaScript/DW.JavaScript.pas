unit DW.JavaScript;

interface

const
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

const
  cJSEventScheme = 'about'; // This is being used so as to support Android (EMBT's code otherwise interferes)
  cJSEventProtocol = cJSEventScheme + '://';

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
  System.SysUtils;

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
    LParts := AURL.Substring(cJSEventProtocol.Length).Split([':']);
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
