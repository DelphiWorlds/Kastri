unit DW.Androidapi.JNI.Androidx.LocalBroadcastManager;

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
  JLocalBroadcastManager = interface;

  JLocalBroadcastManagerClass = interface(JObjectClass)
    ['{BDAED328-AD30-4DBF-A084-D9B9AFDAF5D6}']
    {class} function getInstance(context: JContext): JLocalBroadcastManager; cdecl;
  end;

  [JavaSignature('androidx/localbroadcastmanager/content/LocalBroadcastManager')]
  JLocalBroadcastManager = interface(JObject)
    ['{B7024205-4A57-4BC2-ADAF-EE60011FE0F3}']
    procedure registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter); cdecl;
    function sendBroadcast(intent: JIntent): Boolean; cdecl;
    procedure sendBroadcastSync(intent: JIntent); cdecl;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
  end;
  TJLocalBroadcastManager = class(TJavaGenericImport<JLocalBroadcastManagerClass, JLocalBroadcastManager>) end;

implementation

end.

