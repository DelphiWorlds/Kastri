unit DW.Androidapi.JNI.DWMultiBroadcastReceiver;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;

type
  JDWMultiBroadcastReceiver = interface;

  JDWMultiBroadcastReceiverClass = interface(JBroadcastReceiverClass)
    ['{7287006F-964B-4092-ACCB-9306009864A8}']
    {class} function _GetACTION_NOTIFICATION: JString; cdecl;
    {class} function _GetACTION_SERVICE_ALARM: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_ID: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_NAME: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_REPEATINTERVAL: JString; cdecl;
    {class} function init: JDWMultiBroadcastReceiver; cdecl;
    {class} property ACTION_NOTIFICATION: JString read _GetACTION_NOTIFICATION;
    {class} property ACTION_SERVICE_ALARM: JString read _GetACTION_SERVICE_ALARM;
    {class} property EXTRA_NOTIFICATION: JString read _GetEXTRA_NOTIFICATION;
    {class} property EXTRA_NOTIFICATION_ID: JString read _GetEXTRA_NOTIFICATION_ID;
    {class} property EXTRA_NOTIFICATION_NAME: JString read _GetEXTRA_NOTIFICATION_NAME;
    {class} property EXTRA_NOTIFICATION_REPEATINTERVAL: JString read _GetEXTRA_NOTIFICATION_REPEATINTERVAL;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWMultiBroadcastReceiver')]
  JDWMultiBroadcastReceiver = interface(JBroadcastReceiver)
    ['{C2D86CCE-A586-4619-B9B5-C2F456B4C609}']
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;
  TJDWMultiBroadcastReceiver = class(TJavaGenericImport<JDWMultiBroadcastReceiverClass, JDWMultiBroadcastReceiver>)
  public
    class function getClass: Jlang_Class;
  end;

implementation

{ TJDWMultiBroadcastReceiver }

class function TJDWMultiBroadcastReceiver.getClass: Jlang_Class;
begin
  Result := TJDWMultiBroadcastReceiver.JavaClass.init.getClass;
end;

end.

