unit DW.Winapi.ExternalDevice;

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
  Winapi.Windows;

const
  SGUIDHumanInterfaceDevice = '{4D1E55B2-F16F-11CF-88CB-001111000030}';
  SGUIDUSBRawDevice = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  SGUIDDiskDevice = '{53f56307-b6bf-11d0-94f2-00a0c91efb8b}';
  SGUIDNetworkCard = '{ad498944-762f-11d0-8dcb-00c04fc3358c}';
  SGUIDAndroidDevice = '{F72FE0D4-CBCB-407d-8814-9ED673D0DD6B}';

  DBT_QUERYCHANGECONFIG = $0017;
  DBT_CONFIGCHANGED = $0018;
  DBT_CONFIGCHANGECANCELED = $0019;
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEQUERYREMOVE = $8001;
  DBT_DEVICEQUERYREMOVEFAILED = $8002;
  DBT_DEVICEREMOVEPENDING = $8003;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVICETYPESPECIFIC = $8005;
  DBT_USERDEFINED = $FFFF;
  DBT_DEVTYP_OEM = 0;
  DBT_DEVTYP_VOLUME = 2;
  DBT_DEVTYP_PORT = 3;
  DBT_DEVTYP_DEVICEINTERFACE = 5;
  DBT_DEVTYP_HANDLE = 6;
  DBTF_MEDIA = 1;
  DBTF_NET = 2;

  FILE_DEVICE_BEEP = $00000001;
  FILE_DEVICE_CD_ROM = $00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM = $00000003;
  FILE_DEVICE_CONTROLLER = $00000004;
  FILE_DEVICE_DATALINK = $00000005;
  FILE_DEVICE_DFS = $00000006;
  FILE_DEVICE_DISK = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM = $00000008;
  FILE_DEVICE_FILE_SYSTEM = $00000009;
  FILE_DEVICE_INPORT_PORT = $0000000a;
  FILE_DEVICE_KEYBOARD = $0000000b;
  FILE_DEVICE_MAILSLOT = $0000000c;
  FILE_DEVICE_MIDI_IN = $0000000d;
  FILE_DEVICE_MIDI_OUT = $0000000e;
  FILE_DEVICE_MOUSE = $0000000f;
  FILE_DEVICE_MULTI_UNC_PROVIDER = $00000010;
  FILE_DEVICE_NAMED_PIPE = $00000011;
  FILE_DEVICE_NETWORK = $00000012;
  FILE_DEVICE_NETWORK_BROWSER = $00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  FILE_DEVICE_NULL = $00000015;
  FILE_DEVICE_PARALLEL_PORT = $00000016;
  FILE_DEVICE_PHYSICAL_NETCARD = $00000017;
  FILE_DEVICE_PRINTER = $00000018;
  FILE_DEVICE_SCANNER = $00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT = $0000001a;
  FILE_DEVICE_SERIAL_PORT = $0000001b;
  FILE_DEVICE_SCREEN = $0000001c;
  FILE_DEVICE_SOUND = $0000001d;
  FILE_DEVICE_STREAMS = $0000001e;
  FILE_DEVICE_TAPE = $0000001f;
  FILE_DEVICE_TAPE_FILE_SYSTEM = $00000020;
  FILE_DEVICE_TRANSPORT = $00000021;
  FILE_DEVICE_UNKNOWN = $00000022;
  FILE_DEVICE_VIDEO = $00000023;
  FILE_DEVICE_VIRTUAL_DISK = $00000024;
  FILE_DEVICE_WAVE_IN = $00000025;
  FILE_DEVICE_WAVE_OUT = $00000026;
  FILE_DEVICE_8042_PORT = $00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR = $00000028;
  FILE_DEVICE_BATTERY = $00000029;
  FILE_DEVICE_BUS_EXTENDER = $0000002a;
  FILE_DEVICE_MODEM = $0000002b;
  FILE_DEVICE_VDM = $0000002c;
  FILE_DEVICE_MASS_STORAGE = $0000002d;
  FILE_DEVICE_SMB = $0000002e;
  FILE_DEVICE_KS = $0000002f;
  FILE_DEVICE_CHANGER = $00000030;
  FILE_DEVICE_SMARTCARD = $00000031;
  FILE_DEVICE_ACPI = $00000032;
  FILE_DEVICE_DVD = $00000033;
  FILE_DEVICE_FULLSCREEN_VIDEO = $00000034;
  FILE_DEVICE_DFS_FILE_SYSTEM = $00000035;
  FILE_DEVICE_DFS_VOLUME = $00000036;

  FILE_ANY_ACCESS = 0;
  METHOD_BUFFERED = 0;
  MEDIA_ERASEABLE = $00000001;
  MEDIA_WRITE_ONCE = $00000002;
  MEDIA_READ_ONLY = $00000004;
  MEDIA_READ_WRITE = $00000008;
  MEDIA_WRITE_PROTECTED = $00000100;
  MEDIA_CURRENTLY_MOUNTED = DWORD($80000000);

  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;
  IOCTL_STORAGE_GET_MEDIA_TYPES_EX = ((IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or ($0301 shl 2) or METHOD_BUFFERED);
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
//  IOCTL_SCSI_PASS_THROUGH_DIRECT = ((IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
//    ($0405 shl 2) or METHOD_BUFFERED);
  IOCTL_SCSI_PASS_THROUGH_DIRECT     = $4D014;

  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';

  DiscTypes: array[0..19] of string = (
    'No Current Profile', 'Non Removable Disk', 'Removable Disk', 'Magneto Optical Erasable',
    'Optical Write Once', 'AS-MO', 'CD-ROM', 'CD-R', 'CD-RW', 'DVD-ROM',
    'DVD-R SequentialRecording', 'DVD-RAM', 'DVD-RW Restricted Overwrite', 'DVD-RW Sequential Recording',
    'DVD+RW', 'DVD+R', 'DDCD-ROM', 'DDCD-R', 'DDCD-RW', 'UNKNOWN');

type
  PDevBroadcastHdr  = ^TDevBroadcastHdr;
  TDevBroadcastHdr = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  DEV_BROADCAST_HDR = TDevBroadcastHdr;

  PDevBroadcastDeviceInterface  = ^DEV_BROADCAST_DEVICEINTERFACE;
  TDevBroadcastDeviceInterface = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: short;
  end;
  DEV_BROADCAST_DEVICEINTERFACE = TDevBroadcastDeviceInterface;

  TUSBInfo = record
    DeviceType: string;
    DriverDesc: string;
    FriendlyName: string;
  end;

  PDevBroadcastVolume = ^TDevBroadcastVolume;
  TDevBroadcastVolume = packed record
     dbcv_size: DWORD;
     dbcv_devicetype: DWORD;
     dbcv_reserved: DWORD;
     dbcv_unitmask: DWORD;
     dbcv_flags: WORD;
  end;
  DEV_BROADCAST_VOLUME = TDevBroadcastVolume;

  STORAGE_MEDIA_TYPE = DWORD;
  STORAGE_BUS_TYPE = DWORD;

  TDMIDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMIRemovableDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMITapeInfo = record
    MediaType: STORAGE_MEDIA_TYPE;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
    CurrentBlockSize: DWORD;
    BusType: STORAGE_BUS_TYPE;
    //
    // Bus specific information describing the medium supported.
    //
    case Integer of {BusSpecificData}
      0:
        ({ScsiInformation}
        MediumType: BYTE;
        DensityCode: BYTE);
    end;

  PDeviceMediaInfo = ^TDeviceMediaInfo;
  TDeviceMediaInfo = record
    case Integer of
      0:
        (DiskInfo: TDMIDiskInfo);
      1:
        (RemovableDiskInfo: TDMIRemovableDiskInfo);
      2:
        (TapeInfo: TDMITapeInfo);
  end;
  _DEVICE_MEDIA_INFO = TDeviceMediaInfo;

  PGetMediaTypes = ^TGetMediaTypes;
  TGetMediaTypes = packed record
    DeviceType: DWord;
    MediaInfoCount: DWord;
    MediaInfo: array[0..1] of _DEVICE_MEDIA_INFO;
  end;
  GET_MEDIA_TYPES = TGetMediaTypes;

  TDeviceConfigHeader = packed record
    DataLength: Cardinal;
    Reserved: Word;
    CurrentProfile: Word;
    FeatureCode: Word;
    Version: Byte;
    AdditionalLength: Byte;
    OtherData: array[0..101] of Byte;
  end;

  PSCSI_PASS_THROUGH = ^SCSI_PASS_THROUGH;
  SCSI_PASS_THROUGH = record
    Length: Word;
    ScsiStatus: Byte;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
    CdbLength: Byte;
    SenseInfoLength: Byte;
    DataIn: Byte;
    DataTransferLength: ULONG;
    TimeOutValue: ULONG;
    DataBufferOffset: ULONG;
    SenseInfoOffset: ULONG;
    Cdb: array[0..15] of Byte;
  end;
  PSCSIPassThrough = ^TSCSIPassThrough;
  TSCSIPassThrough = SCSI_PASS_THROUGH;

  PSCSI_PASS_THROUGH_DIRECT = ^SCSI_PASS_THROUGH_DIRECT;
  SCSI_PASS_THROUGH_DIRECT = record
    Length: Word;
    ScsiStatus: Byte;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
    CdbLength: Byte;
    SenseInfoLength: Byte;
    DataIn: Byte;
    DataTransferLength: ULONG;
    TimeOutValue: ULONG;
    DataBuffer: Pointer;
    SenseInfoOffset: ULONG;
    Cdb: array[0..15] of Byte;
  end;
  PSCSIPassThroughDirect = ^TSCSIPassThroughDirect;
  TSCSIPassThroughDirect = SCSI_PASS_THROUGH_DIRECT;

  PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = ^SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = record
    Spt: TSCSIPassThroughDirect;
    Filler: ULONG;
    SenseBuf: array[0..31] of Byte;
  end;
  PSCSIPassThroughDirectWithBuffer = ^TSCSIPassThroughDirectWithBuffer;
  TSCSIPassThroughDirectWithBuffer = SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;

  TDiscTypes = set of 0..19;

var
  DiscTypesCD: TDiscTypes = [6, 7, 8];
  DiscTypesDVD: TDiscTypes = [9, 10, 11, 12, 13, 14, 15];

function GetDriveFromUnitMask(UnitMask: DWord): string;
function GetMediaType(const Drive: string): Integer;

implementation

uses
  // RTL
  Math;

function GetDriveFromUnitMask(UnitMask: DWord): string;
begin
  Result := Chr(65 + Trunc(Log2(UnitMask) / Log2(2)));
end;

function GetMediaTypeFromHandle(const AHandle: THandle): Integer;
var
  SPTDW: TSCSIPassThroughDirectWithBuffer;
  Size, Returned: LongWord;
  DeviceConfigHeader: TDeviceConfigHeader;
begin
  Result := -1;
  ZeroMemory(@SPTDW, SizeOf(SPTDW));
  Size := SizeOf(SPTDW);
  SPTDW.Spt.Length := SizeOf(TSCSIPassThrough);
  SPTDW.Spt.CdbLength := 10;
  SPTDW.Spt.SenseInfoLength := 32;
  SPTDW.Spt.DataIn := 1; // SCSI_IOCTL_DATA_IN;
  SPTDW.Spt.DataTransferLength := SizeOf(DeviceConfigHeader);
  SPTDW.Spt.TimeOutValue := 120;
  SPTDW.Spt.DataBuffer := @DeviceConfigHeader;
  SPTDW.Spt.SenseInfoOffset := 48;

  SPTDW.Spt.Cdb[0] := $46;
  SPTDW.Spt.Cdb[1] := $02;
  SPTDW.Spt.Cdb[3] := $00;
  SPTDW.Spt.Cdb[7] := HiByte(SizeOf(DeviceConfigHeader));
  SPTDW.Spt.Cdb[8] := LoByte(SizeOf(DeviceConfigHeader));
  if DeviceIoControl(AHandle, IOCTL_SCSI_PASS_THROUGH_DIRECT, @SPTDW, Size, @SPTDW, Size, Returned, nil) then
  begin
    case (((DeviceConfigHeader.CurrentProfile shl 8) and $FF00) or ((DeviceConfigHeader.CurrentProfile shr 8) and $00FF)) of
      $0000:
        Result := 0;
      $0001:
        Result := 1;
      $0002:
        Result := 2;
      $0003:
        Result := 3;
      $0004:
        Result := 4;
      $0005:
        Result := 5;
      $0008:
        Result := 6;
      $0009:
        Result := 7;
      $000A:
        Result := 8;
      $0010:
        Result := 9;
      $0011:
        Result := 10;
      $0012:
        Result := 11;
      $0013:
        Result := 12;
      $0014:
        Result := 13;
      $001A:
        Result := 14;
      $001B:
        Result := 15;
      $0020:
        Result := 16;
      $0021:
        Result := 17;
      $0022:
        Result := 18;
      $FFFF:
        Result := 19;
    else
      Result := -1;
    end;
  end;
end;

function GetMediaType(const Drive: string): Integer;
var
  LHandle: THandle;
  LDrivePath: string;
begin
  Result := -1;
  if Drive <> '' then
  begin
    LDrivePath := '\\.\' + Drive[1] + ':';
    LHandle := CreateFile(PChar(LDrivePath), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if LHandle <> INVALID_HANDLE_VALUE then
    try
      Result := GetMediaTypeFromHandle(LHandle);
    finally
      CloseHandle(LHandle);
    end;
  end;
end;

end.
