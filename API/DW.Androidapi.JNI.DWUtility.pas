unit DW.Androidapi.JNI.DWUtility;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Media;

type
  JDWUtility = interface;

  JDWUtilityClass = interface(JObjectClass)
    ['{CD282406-0D42-4EEE-B42E-24E79D058B30}']
    {class} procedure crashTest; cdecl;
    {class} function createObjectMap: JMap; cdecl;
    {class} function getScreenBrightness(activity: JActivity): Single; cdecl;
    {class} function imageToBitmap(image: JImage): JBitmap; cdecl;
    {class} function isPackageInstalled(context: JContext; packageName: JString): Boolean; cdecl;
    {class} function jpegImageToBytes(image: JImage): TJavaArray<Byte>; cdecl;
    {class} procedure setScreenBrightness(activity: JActivity; brightness: Single); cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWUtility')]
  JDWUtility = interface(JObject)
    ['{5C9753FA-7746-4DCE-A709-E68F1319CB97}']
  end;
  TJDWUtility = class(TJavaGenericImport<JDWUtilityClass, JDWUtility>) end;

implementation

end.
