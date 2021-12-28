unit DW.Location.Types;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Sensors, System.JSON;

const
  cInvalidLatitude = 91;
  cInvalidLongitude = 181;

type
  TLocationDataFlag = (Accuracy, Altitude, Bearing, Speed);

  TLocationDataFlags = set of TLocationDataFlag;

  TLocationData = record
  private
    function GetLocationJSON: TJSONValue;
  public
    Accuracy: Double;
    Altitude: Double;
    Bearing: Double;
    DateTime: TDateTime;
    Flags: TLocationDataFlags;
    // BearingAccuracy: Double;
    Location: TLocationCoord2D;
    Speed: Double;
    procedure FromJSON(const AValue: string);
    procedure Reset;
    function ToJSON: string;
  end;

  TLocationChangedEvent = procedure(Sender: TObject; const Data: TLocationData) of object;

implementation

{ TLocationData }

procedure TLocationData.Reset;
begin
  Flags := [];
  Location := TLocationCoord2D.Create(cInvalidLatitude, cInvalidLongitude);
  Accuracy := 0;
  Altitude := 0;
  Bearing := 0;
  DateTime := 0;
  Speed := 0;
end;

procedure TLocationData.FromJSON(const AValue: string);
var
  LValue, LLocationValue: TJSONValue;
  LJSON: TJSONObject;
  LLatitude, LLongitude: Double;
begin
  Reset;
  LValue := TJSONObject.ParseJSONValue(AValue);
  if LValue <> nil then
  try
    if LValue is TJSONObject then
    begin
      LJSON := TJSONObject(LValue);
      if LJSON.TryGetValue('Location', LLocationValue) then
      begin
        if LLocationValue.TryGetValue('Latitude', LLatitude) and LLocationValue.TryGetValue('Longitude', LLongitude) then
          Location := TLocationCoord2D.Create(LLatitude, LLongitude);
      end;
      if LJSON.TryGetValue('Accuracy', Accuracy) then
        Include(Flags, TLocationDataFlag.Accuracy);
      if LJSON.TryGetValue('Altitude', Altitude) then
        Include(Flags, TLocationDataFlag.Altitude);
      if LJSON.TryGetValue('Bearing', Bearing) then
        Include(Flags, TLocationDataFlag.Bearing);
      if LJSON.TryGetValue('Speed', Speed) then
        Include(Flags, TLocationDataFlag.Speed);
    end;
  finally
    LValue.Free;
  end;
end;

function TLocationData.GetLocationJSON: TJSONValue;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  LJSON.AddPair('Latitude', TJSONNumber.Create(Location.Latitude));
  LJSON.AddPair('Longitude', TJSONNumber.Create(Location.Longitude));
  Result := LJSON;
end;

function TLocationData.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    if TLocationDataFlag.Accuracy in Flags then
      LJSON.AddPair('Accuracy', TJSONNumber.Create(Accuracy));
    if TLocationDataFlag.Altitude in Flags then
      LJSON.AddPair('Altitude', TJSONNumber.Create(Altitude));
    if TLocationDataFlag.Bearing in Flags then
      LJSON.AddPair('Bearing', TJSONNumber.Create(Bearing));
    if TLocationDataFlag.Speed in Flags then
      LJSON.AddPair('Speed', TJSONNumber.Create(Speed));
    LJSON.AddPair('DateTime', TJSONNumber.Create(DateTime));
    LJSON.AddPair('Location', GetLocationJSON);
    Result := LJSON.ToString;
  finally
    LJSON.Free;
  end;
end;

end.
