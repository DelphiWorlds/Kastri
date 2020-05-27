unit DW.Precompile;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

// A cross-platform way of including units in a project for pre-compiling, specifically for the Kastri project source
// This unit should not be included in a regular application

uses
{$IF Defined(ANDROID)}
  DW.Androidapi.JNI.App,
  DW.Androidapi.JNI.Content,
  DW.Androidapi.JNI.DWCameraHelpers,
  DW.Androidapi.JNI.DWFingerprintAuthenticationCallback,
  DW.Androidapi.JNI.DWFirebaseServiceHelpers,
  DW.Androidapi.JNI.DWMultiBroadcastReceiver,
  DW.Androidapi.JNI.DWTimerTask,
  DW.Androidapi.JNI.DWWebChromeClient,
  DW.Androidapi.JNI.Firebase,
  DW.Androidapi.JNI.Hardware.Camera2,
  DW.Androidapi.JNI.Hardware.Usb,
  DW.Androidapi.JNI.Hardware,
  DW.Androidapi.JNI.Lang,
  DW.Androidapi.JNI.Net,
  DW.Androidapi.JNI.Nfc,
  DW.Androidapi.JNI.Os,
  DW.Androidapi.JNI.Print,
  DW.Androidapi.JNI.Security,
  DW.Androidapi.JNI.SupportV4,
  DW.Androidapi.JNI.Telecom,
  DW.Androidapi.JNI.Util,
  DW.Androidapi.JNI.View,
  DW.Androidapi.JNI.VisionBarcode,
  DW.Androidapi.JNI.Widget.Toast,
  DW.Android.Helpers,
  DW.Android.Service,
  DW.Background.Android,
  DW.Consts.Android,
  DW.Geodetic.Android,
  DW.Graphics.Helpers.Android,
  DW.Location.Android,
  DW.LocationReceiver.Android,
  DW.MediaLibrary.Android,
  DW.MultiReceiver.Android,
  DW.ServiceCommander.Android,
  DW.TimerTask.Android,
  DW.Toast.Android,
  DW.VirtualKeyboard.Android,
  DW.WebChromeClient.Android,
{$ENDIF}

{$IF Defined(LINUX64)}
  DW.Linuxapi.Epoll,
  DW.Linuxapi.Timerfd,
{$ENDIF}

{$IF Defined(MACOS)}
  DW.Graphics.Helpers.Mac,
  DW.Macapi.Dispatch,
  DW.Macapi.Helpers,
  DW.Macapi.ObjCBlocks,
  DW.Macapi.ObjCRuntime,
{$ENDIF}

{$IF Defined(OSX)}
  DW.Macapi.AVFoundation,
  DW.Macapi.AVKit,
  DW.Macapi.AppKit,
  DW.Macapi.Foundation,
  DW.Macapi.FSEvents,
  DW.Macapi.IOKit,
  DW.StatusBarMenu.Mac,
{$ENDIF}

{$IF Defined(IOS)}
  DW.iOSapi.AuthenticationServices,
  DW.iOSapi.AVFoundation,
  DW.iOSapi.Contacts,
  DW.iOSapi.CoreNFC,
  DW.iOSapi.CoreVideo,
  DW.iOSapi.DeviceCheck,
  DW.iOSapi.EventKit,
  DW.iOSapi.FBAudienceNetwork,
  DW.iOSapi.Firebase,
  DW.iOSapi.Foundation,
  DW.iOSapi.Helpers,
  DW.iOSapi.ImageIO,
  DW.iOSapi.MediaPlayer,
  DW.iOSapi.PassKit,
  DW.iOSapi.Photos,
  DW.iOSapi.PushKit,
  DW.iOSapi.Speech,
  DW.iOSapi.SystemConfiguration,
  DW.iOSapi.UIKit,
  DW.iOSapi.UserNotifications,
  DW.Location.iOS,
  DW.Orientation.iOS,
  DW.PushNotification.iOS,
  DW.UserDefaults.iOS,
{$ENDIF}

{$IF Defined(POSIX)}
  DW.IOUtils.Helpers.Posix,
{$ENDIF}

{$IF Defined(MSWINDOWS)}
  DW.FileVersionInfo.Win,
  DW.Swizzler.Win,
  DW.Vcl.DialogService,
  DW.Vcl.FormStates,
  DW.Vcl.ListBoxHelper,
  DW.Winapi.Helpers,
{$ENDIF}

  // Platform agnostic
  DW.Base64.Helpers,
  DW.Classes.Helpers,
  DW.Consts,
  DW.Controls.Helpers,
  DW.DataGraphics.Helpers,
  DW.DialogService,
  DW.ElasticLayout,
  DW.FileWriter,
  DW.Geodetic,
  DW.Graphics.Helpers,
  DW.JsonConfig,
  DW.Location,
  DW.MediaLibrary,
  DW.Messaging,
  DW.OSDevice,
  DW.OSLog,
  DW.OSMetadata,
  DW.OSPower,
  DW.OSTimer,
  DW.Permissions.Helpers,
  DW.REST.Json.Helpers,
  DW.Services,
  DW.SysUtils.Helpers,
  DW.ThreadedTimer,
  DW.Tokenizers,
  DW.Types.Helpers,
  DW.Types,
  DW.UIHelper,
  DW.VKVertScrollbox,
  DW.VirtualKeyboard.Helpers,
  DW.Biometric,
  DW.Connectivity,
  DW.Firebase.Messaging,
  DW.NFC,
  DW.Notifications,
  DW.PushUDP,
  DW.Sensors,
  DW.SpeechRecognition;

end.
