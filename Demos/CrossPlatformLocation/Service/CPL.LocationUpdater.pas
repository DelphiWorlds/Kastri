unit CPL.LocationUpdater;

interface

uses
  System.Classes, System.Sensors;

type
  TLocationUpdater = class(TObject)
  private
    FURL: string;
    procedure PostRequest(const ARequest: TStream);
  public
    procedure SendLocation(const ALocation: TLocationCoord2D; const AStatus: Integer);
    property URL: string read FURL write FURL;
  end;

implementation

uses
  System.SysUtils, System.Net.HTTPClient, System.Net.URLClient, System.NetConsts,
  REST.Types,
  DW.OSLog, DW.OSDevice;

const
  // Change this to whatever format is required by your location update service.
  cLocationRequestJSONTemplate = '{"deviceid": "%s", "latitude": "%2.6f", "longitude": "%2.6f", "os": "%S", "device_status": "%d"}';

{ TLocationUpdater }

procedure TLocationUpdater.SendLocation(const ALocation: TLocationCoord2D; const AStatus: Integer);
var
  LStream: TStringStream;
  LJSON: string;
begin
  LJSON := Format(cLocationRequestJSONTemplate, [{'',} TOSDevice.GetUniqueDeviceID, ALocation.Latitude, ALocation.Longitude, 'Android', AStatus]);
  LStream := TStringStream.Create(LJSON);
  try
    try
      PostRequest(LStream);
    except
      on E: Exception do
        TOSLog.e('Exception in SendLocation - %s: %s', [E.ClassName, E.Message], True);
    end;
  finally
    LStream.Free;
  end;
end;

procedure TLocationUpdater.PostRequest(const ARequest: TStream);
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  // Posts the JSON request to the server
  LHTTP := THTTPClient.Create;
  try
    LHTTP.Accept := CONTENTTYPE_APPLICATION_JSON;
    LHTTP.ContentType := CONTENTTYPE_APPLICATION_JSON;
    LResponse := LHTTP.Post(FURL, ARequest);
    if LResponse.StatusCode <> 200 then
      TOSLog.d('Post unsuccessful - Status: %s, Response: %s', [LResponse.StatusText, LResponse.ContentAsString], True);
  finally
    LHTTP.Free;
  end;
end;

end.
