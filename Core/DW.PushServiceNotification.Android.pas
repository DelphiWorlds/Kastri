unit DW.PushServiceNotification.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.JSON, System.PushNotification,
  // Android
  Androidapi.JNI.Os;

type
  TAndroidPushServiceNotification = class(TPushServiceNotification)
  private
    FRawData: TJSONObject;
  protected
    function GetDataKey: string; override;
    function GetJson: TJSONObject; override;
    function GetDataObject: TJSONObject; override;
  public
    constructor Create(const ABundle: JBundle); overload;
  end;

implementation

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers;

{ TAndroidPushServiceNotification }

constructor TAndroidPushServiceNotification.Create(const ABundle: JBundle);
var
  LJSONObject: TJSONObject;
  LIterator: JIterator;
  LValue: JString;
  LKey: JString;
begin
  LJSONObject := TJSONObject.Create;
  LIterator := ABundle.KeySet.iterator;
  while LIterator.hasNext do
  begin
    LKey := LIterator.next.toString;
    LValue := ABundle.&get(LKey).ToString;
    LJSONObject.AddPair(JStringToString(LKey), JStringToString(LValue));
  end;
  FRawData := LJSONObject;
end;

function TAndroidPushServiceNotification.GetDataKey: string;
begin
  Result := 'fcm';
end;

function TAndroidPushServiceNotification.GetDataObject: TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := FRawData;
  if FRawData <> nil then
  begin
    LValue := FRawData.Values[GetDataKey];
    if LValue <> nil then
      Result := LValue as TJSONObject;
  end;
end;

function TAndroidPushServiceNotification.GetJson: TJSONObject;
begin
  Result := FRawData;
end;

end.
