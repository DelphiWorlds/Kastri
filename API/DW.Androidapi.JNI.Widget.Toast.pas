unit DW.Androidapi.JNI.Widget.Toast;

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

uses
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;

type
  JToast = interface;

  JToastClass = interface(JObjectClass)
    ['{68CA26BB-66F9-4253-B348-40D4A6841874}']
    {class} function _GetLENGTH_LONG: Integer; cdecl;
    {class} function _GetLENGTH_SHORT: Integer; cdecl;
    {class} function init(context: JContext): JToast; cdecl; overload;
    {class} function makeText(context: JContext; text: JCharSequence; duration: Integer): JToast; cdecl;
    {class} property LENGTH_LONG: Integer read _GetLENGTH_LONG;
    {class} property LENGTH_SHORT: Integer read _GetLENGTH_SHORT;
  end;

  [JavaSignature('android/widget/Toast')]
  JToast = interface(JObject)
    ['{17E599B9-E074-4226-B5B7-BDCE114FDD6A}']
    procedure cancel; cdecl;
    function getDuration: Integer; cdecl;
    function getGravity: Integer; cdecl;
    function getHorizontalMargin: Single; cdecl;
    function getVerticalMargin: Single; cdecl;
    function getView: JView; cdecl;
    function getXOffset: Integer; cdecl;
    function getYOffset: Integer; cdecl;
    procedure setDuration(value: Integer); cdecl;
    procedure setGravity(gravity, xOffset, yOffset: Integer); cdecl;
    procedure setMargin(horizontalMargin, verticalMargin: Single); cdecl;
    procedure setText(text: JCharSequence); cdecl;
    procedure setView(view: JView); cdecl;
    procedure show; cdecl;
  end;
  TJToast = class(TJavaGenericImport<JToastClass, JToast>) end;

implementation

end.

