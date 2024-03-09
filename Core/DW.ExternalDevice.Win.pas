unit DW.ExternalDevice.Win;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Win
  Winapi.Windows, Winapi.Messages,
  // DW
  DW.Winapi.ExternalDevice;

type
  TExternalDevice = (Unknown, CD, DVD, Drive, USB, HID, USBRaw, NetworkCard, Android);

  TDeviceChangedEvent = procedure(Sender: TObject; const Device: TExternalDevice; const Inserted: Boolean; const Drive: string) of object;

  /// <summary>
  ///   Watches for devices (eg USB, Network, CD/DVD) being inserted or removed
  /// </summary>
  /// <remarks>
  ///   Used by the Device Lens application to detect when and Android device is plugged in/removed
  /// </remarks>
  TExternalDeviceWatcher = class(TObject)
  private
    FHandle: THandle;
    FOnDeviceChanged: TDeviceChangedEvent;
    function GetDeviceFromGUID(const AGUID: string): TExternalDevice;
    procedure WndProc(var Message: TMessage);
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
  protected
    procedure DoDeviceChanged(const ADevice: TExternalDevice; const AInserted: Boolean; const ADrive: string);
  public
    constructor Create;
    destructor Destroy; override;
    property OnDeviceChanged: TDeviceChangedEvent read FOnDeviceChanged write FOnDeviceChanged;
  end;

const
  cExternalDeviceCaptions: array[TExternalDevice] of string = ('Unknown', 'CD', 'DVD', 'External Drive', 'USB', 'Human Interface Device',
    'USB Raw Device', 'Network Card', 'Android');

implementation

uses
  // RTL
  System.Classes, System.SysUtils;

{ TExternalDeviceWatcher }

constructor TExternalDeviceWatcher.Create;
var
  LDeviceInterface: TDevBroadcastDeviceInterface;
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
  LDeviceInterface.dbcc_size := SizeOf(LDeviceInterface);
  LDeviceInterface.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  RegisterDeviceNotification(FHandle, @LDeviceInterface, 4);
end;

destructor TExternalDeviceWatcher.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited;
end;

procedure TExternalDeviceWatcher.DoDeviceChanged(const ADevice: TExternalDevice; const AInserted: Boolean; const ADrive: string);
begin
  if Assigned(FOnDeviceChanged) then
    FOnDeviceChanged(Self, ADevice, AInserted, ADrive);
end;

function TExternalDeviceWatcher.GetDeviceFromGUID(const AGUID: string): TExternalDevice;
begin
  Result := TExternalDevice.Unknown;
  if SameText(AGUID, SGUIDHumanInterfaceDevice) then
    Result := TExternalDevice.HID
  else if SameText(AGUID, SGUIDUSBRawDevice) then
    Result := TExternalDevice.USBRaw
  else if SameText(AGUID, SGUIDAndroidDevice) then
    Result := TExternalDevice.Android;
end;

procedure TExternalDeviceWatcher.WMDeviceChange(var Msg: TMessage);
var
  LDeviceHeader: TDevBroadcastHdr;
  LDeviceVol: TDevBroadcastVolume;
  LDeviceInterface: TDevBroadcastDeviceInterface;
  LDrive: string;
  LDiscType: Integer;
begin
  case Msg.wParam of
    DBT_DEVICEARRIVAL:
    begin
      LDeviceHeader := PDevBroadcastHdr(Msg.lParam)^;
      case LDeviceHeader.dbch_devicetype of
        DBT_DEVTYP_VOLUME:
        begin
          LDeviceVol := PDevBroadcastVolume(Msg.lParam)^;
          LDrive := GetDriveFromUnitMask(LDeviceVol.dbcv_unitmask);
          LDrive := IncludeTrailingPathDelimiter(LDrive + ':');
          if (LDeviceVol.dbcv_flags and DBTF_MEDIA) <> 0 then
          begin
            LDiscType := GetMediaType(LDrive);
            if LDiscType in DiscTypesDVD then
              DoDeviceChanged(TExternalDevice.DVD, True, LDrive)
            else if LDiscType in DiscTypesCD then
              DoDeviceChanged(TExternalDevice.CD, True, LDrive);
          end
          else
            DoDeviceChanged(TExternalDevice.Drive, True, LDrive);
        end;
        DBT_DEVTYP_DEVICEINTERFACE:  // USB device
        begin
          LDeviceInterface := PDevBroadcastDeviceInterface(Msg.LParam)^;
          DoDeviceChanged(GetDeviceFromGUID(GUIDToString(LDeviceInterface.dbcc_classguid)), True, '');
        end;
      end;
    end;
    DBT_DEVICEREMOVECOMPLETE:
    begin
      LDeviceHeader := PDevBroadcastHdr(Msg.LParam)^;
      case LDeviceHeader.dbch_devicetype of
        DBT_DEVTYP_DEVICEINTERFACE:  // USB device
        begin
          LDeviceInterface := PDevBroadcastDeviceInterface(Msg.LParam)^;
          DoDeviceChanged(GetDeviceFromGUID(GUIDToString(LDeviceInterface.dbcc_classguid)), False, '');
        end;
      end;
    end;
  end;
end;

procedure TExternalDeviceWatcher.WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

end.
