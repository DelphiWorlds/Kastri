unit DW.Macapi.MobileDevice;

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


// Based (so far) on:
//   https://github.com/rpetrich/deviceconsole/blob/master/MobileDevice.h
// Error codes:
//   https://github.com/ios-control/ios-deploy/blob/master/src/ios-deploy/errors.h

// Note: In order to compile this unit, you will need to do the following:
//   1. On your Mac, open Finder and either use Shift-Command-G, or click on Go | Go To Folder, and enter /Library/Apple/System/Library/PrivateFrameworks and hit enter
//   2. Copy the MobileDevice.Framework folder and put it in a folder on your PC that can be used with your Delphi project
//   3. In your Delphi project, for the macOS platform, ensure that a path to the folder *above* MobileDevice.Framework is included in the Framework Path value

interface

uses
  // macOS
  Macapi.CoreFoundation, Macapi.Mach;

const
  libMobileDevice = 'MobileDevice';

  ADNCI_MSG_CONNECTED = 1;
  ADNCI_MSG_DISCONNECTED = 2;
  ADNCI_MSG_UNKNOWN = 3;

  MDERR_OK = 0; // ERR_SUCCESS

  AMSVC_AFC = 'com.apple.afc';
  AMSVC_AFC2 = 'com.apple.afc2';
  AMSVC_DEBUG_IMAGE_MOUNT = 'com.apple.mobile.debug_image_mount';
  AMSVC_SIMULATE_LOCATION = 'com.apple.dt.simulatelocation';
  AMSVC_SCREENSHOT = 'com.apple.mobile.screenshotr'; // That's not a typo - it really is screenshotr
  AMSVC_SYNC = 'com.apple.mobilesync';
  AMSVC_SYSLOG_RELAY_STRING = 'com.apple.syslog_relay';

  AMSVC_LOCKTEST = AMSVC_SYNC;

type
  mach_error_t = kern_return_t;
  service_conn_t = Integer;
  Pservice_conn_t = ^service_conn_t;

  am_device = record
    unknown0: array[0..15] of AnsiChar;
    device_id: NativeInt;
    product_id: NativeInt;
    serial: PAnsiChar;
    unknown1: NativeInt;
    unknown2: array[0..3] of AnsiChar;
    lockdown_conn: NativeInt;
    unknown3: array[0..7] of AnsiChar;
  end;
  Pam_device = ^am_device;

  am_device_notification_callback_info = record
    dev: Pam_device;
    msg: NativeInt;
  end;
  Pam_device_notification_callback_info = ^am_device_notification_callback_info;

  am_device_notification_callback = procedure(info: Pam_device_notification_callback_info; unknown: Pointer); cdecl;
  Pam_device_notification_callback = ^am_device_notification_callback;

  am_device_notification = record
    unknown0: UInt32;
    unknown1: UInt32;
    unknown2: UInt32;
    callback: am_device_notification_callback;
    unknown3: UInt32;
  end;
  Pam_device_notification = ^am_device_notification;

  afc_error_t = UInt32;

  afc_connection = record
    handle: UInt32;
    unknown0: UInt32;
    unknown1: Byte;
    padding: array[0..2] of Byte;
    unknown2: UInt32;
    unknown3: UInt32;
    unknown4: UInt32;
    fs_block_size: UInt32;
    sock_block_size: UInt32;
    io_timeout: UInt32;
    afc_lock: Pointer;
    context: UInt32;
  end;
  Pafc_connection = ^afc_connection;

function AMDeviceConnect(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceConnect';
function AMDeviceCopyDeviceIdentifier(device: Pam_device): CFStringRef; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceCopyDeviceIdentifier';
function AMDeviceCopyValue(device: Pam_device; unknown0: NativeInt; key: CFStringRef): CFStringRef; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceCopyValue';
function AMDeviceDisconnect(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceDisconnect';
function AMDeviceIsAtLeastVersionOnPlatform(device: Pam_device; vers: CFDictionaryRef): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceIsAtLeastVersionOnPlatform';
function AMDeviceIsPaired(device: Pam_device): Integer; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceIsPaired';
function AMDeviceMountImage(device: Pam_device; image: CFStringRef; options: CFDictionaryRef; callback: Pointer; cbarg: NativeInt): mach_error_t;
  external framework libMobileDevice name _PU + 'AMDeviceMountImage';
function AMDeviceNotificationSubscribe(callback: Pam_device_notification_callback; unused0: NativeInt; unused1: NativeInt; dn_unknown3: Pointer;
  notification: Pam_device_notification): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceNotificationSubscribe';
function AMDeviceNotificationUnsubscribe(notification: Pam_device_notification): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceNotificationUnsubscribe';
function AMDevicePair(device: Pam_device): Integer; cdecl;
  external framework libMobileDevice name _PU + 'AMDevicePair';
function AMDeviceRelease(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceRelease';
function AMDeviceRetain(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceRetain';
function AMDeviceSecureInstallApplication(unknown0: Integer; device: Pam_device; url: CFURLRef; options: CFDictionaryRef; callback: Pointer; callback_arg: Integer): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceSecureInstallApplication';
function AMDeviceSecureInstallApplicationBundle(device: Pam_device; url: CFURLRef; options: CFDictionaryRef; callback: Pointer; callback_arg: Integer): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceSecureInstallApplication';
function AMDeviceSecureStartService(device: Pam_device; service_name: CFStringRef; unknown: PNativeInt; handle: Pservice_conn_t): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceSecureStartService';
function AMDeviceSecureTransferPath(unknown0: Integer; device: Pam_device; url: CFURLRef; options: CFDictionaryRef; callback: Pointer; callback_arg: Integer): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceSecureTransferPath';
function AMDeviceSecureUninstallApplication(unknown0: Integer; device: Pam_device; bundle_id: CFStringRef; unknown1: Integer; callback: Pointer; callback_arg: Integer): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceSecureUninstallApplication';
function AMDeviceStartService(device: Pam_device; service_name: CFStringRef; handle: Pservice_conn_t; unknown: PNativeInt): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceStartService';
function AMDeviceStartSession(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceStartSession';
function AMDeviceStopSession(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceStopSession';
function AMDeviceUnpair(device: Pam_device): Integer; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceUnpair';
function AMDeviceValidatePairing(device: Pam_device): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDeviceValidatePairing';
function AMDServiceConnectionReceive(handle: Pservice_conn_t; buf: PByte; size: NativeInt): mach_error_t; cdecl;
  external framework libMobileDevice name _PU + 'AMDServiceConnectionReceive';
procedure AMDServiceConnectionInvalidate(handle: Pservice_conn_t); cdecl;
  external framework libMobileDevice name _PU + 'AMDServiceConnectionInvalidate';
function AFCConnectionOpen(handle: service_conn_t; io_timeout: UInt32; conn: Pafc_connection): afc_error_t;
  external framework libMobileDevice name _PU + 'AFCConnectionOpen';
function AFCConnectionClose(conn: afc_connection): afc_error_t;
  external framework libMobileDevice name _PU + 'AFCConnectionClose';

implementation

end.
