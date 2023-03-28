unit DW.Types;

interface

const
  CRLF = #13#10;

type
  TAuthorizationStatus = (NotDetermined, Restricted, Denied, Authorized);
  TAuthorizationStatusProc = reference to procedure(const Status: TAuthorizationStatus);
  TAuthorizationStatusEvent = procedure(Sender: TObject; const Status: TAuthorizationStatus) of object;

  TAndroidLogLevel = (Info, Debug, Error, Warning, Verbose, Fatal);
  TAndroidLogLevels = set of TAndroidLogLevel;

  TStringArrayItem = record
    Key: string;
    Value: string;
    constructor Create(const APair: string);
  end;

  TStringArray = record
  public
    Items: TArray<string>;
    procedure Add(const AValue: string);
    procedure Clear;
    function Count: Integer;
    function GetItem(const AIndex: Integer): TStringArrayItem;
    function GetKey(const AIndex: Integer): string; overload;
    function GetKey(const AValue: string): string; overload;
    function GetValue(const AIndex: Integer): string; overload;
    function GetValue(const AKey: string): string; overload;
    function IndexOf(const AValue: string): Integer;
    function IndexOfName(const AKey: string): Integer;
    function Reconcile(const AChanges: TStringArray): Boolean;
    procedure SetValue(const AKey: string; const AValue: string);
    function Text(const ASeparator: string = CRLF): string;
    function Values: TArray<string>;
  end;

implementation

uses
  System.SysUtils;

{ TStringArrayItem }

constructor TStringArrayItem.Create(const APair: string);
var
  LEqualsIndex: Integer;
begin
  LEqualsIndex := APair.IndexOf('=');
  Key := APair.Substring(0, LEqualsIndex);
  Value := APair.Substring(LEqualsIndex + 1);
end;

{ TStringArray }

procedure TStringArray.Add(const AValue: string);
begin
  Items := Items + [AValue];
end;

procedure TStringArray.Clear;
begin
  Items := [];
end;

function TStringArray.Count: Integer;
begin
  Result := Length(Items);
end;

function TStringArray.IndexOf(const AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Equals(AValue) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TStringArray.IndexOfName(const AKey: string): Integer;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if (Length(LParts) > 1) and LParts[0].Equals(AKey) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TStringArray.Reconcile(const AChanges: TStringArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AChanges.Count - 1 do
  begin
    // New key does not exist in the old list
    if IndexOfName(AChanges.GetKey(I)) = -1 then
    begin
      Result := True;
      Break;
    end;
  end;
  if not Result then
  begin
    for I := 0 to Count - 1 do
    begin
      // Old key does not exist in the new list
      if AChanges.IndexOfName(GetKey(I)) = -1 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
  if Result then
    Items := Copy(AChanges.Items);
end;

procedure TStringArray.SetValue(const AKey, AValue: string);
var
  I: Integer;
  LItem: TStringArrayItem;
  LIsFound: Boolean;
begin
  LIsFound := False;
  for I := 0 to Count - 1 do
  begin
    LItem := TStringArrayItem.Create(Items[I]);
    if LItem.Key.Equals(AKey) then
    begin
      LIsFound := True;
      Items[I] := Format('%s=%s', [AKey, AValue]);
      Break;
    end;
  end;
  if not LIsFound then
    Add(Format('%s=%s', [AKey, AValue]));
end;

function TStringArray.Text(const ASeparator: string): string;
begin
  Result := string.Join(ASeparator, Items);
end;

function TStringArray.Values: TArray<string>;
var
  I: Integer;
  LItem: TStringArrayItem;
begin
  Result := [];
  for I := 0 to Count - 1 do
  begin
    LItem := TStringArrayItem.Create(Items[I]);
    Result := Result + [LItem.Value];
  end;
end;

function TStringArray.GetItem(const AIndex: Integer): TStringArrayItem;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := TStringArrayItem.Create(Items[AIndex]);
end;

function TStringArray.GetKey(const AValue: string): string;
var
  I: Integer;
  LItem: TStringArrayItem;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    LItem := TStringArrayItem.Create(Items[I]);
    if LItem.Value.Equals(AValue) then
    begin
      Result := LItem.Key;
      Break;
    end;
  end;
end;

function TStringArray.GetValue(const AIndex: Integer): string;
var
  LItem: TStringArrayItem;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LItem := TStringArrayItem.Create(Items[AIndex]);
    Result := LItem.Value;
  end;
end;

function TStringArray.GetKey(const AIndex: Integer): string;
var
  LItem: TStringArrayItem;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LItem := TStringArrayItem.Create(Items[AIndex]);
    Result := LItem.Key;
  end;
end;

function TStringArray.GetValue(const AKey: string): string;
var
  I: Integer;
  LItem: TStringArrayItem;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    LItem := TStringArrayItem.Create(Items[I]);
    if LItem.Key.Equals(AKey) then
    begin
      Result := LItem.Value;
      Break;
    end;
  end;
end;

end.
