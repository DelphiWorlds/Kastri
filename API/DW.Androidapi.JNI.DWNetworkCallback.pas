unit DW.Androidapi.JNI.DWNetworkCallback;

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
  Androidapi.JNIBridge, Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText;

type
  JDWNetworkCallback = interface;
  JDWNetworkCallbackDelegate = interface;

  JDWNetworkCallbackClass = interface(JConnectivityManager_NetworkCallbackClass)
    ['{D5B4C0CD-4476-4908-AF1D-5CB1676A778B}']
    {class} function init(context: JContext; delegate: JDWNetworkCallbackDelegate): JDWNetworkCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWNetworkCallback')]
  JDWNetworkCallback = interface(JConnectivityManager_NetworkCallback)
    ['{6219B1FD-3793-4B58-9504-FCF34EB1B632}']
  end;
  TJDWNetworkCallback = class(TJavaGenericImport<JDWNetworkCallbackClass, JDWNetworkCallback>) end;

  JDWNetworkCallbackDelegateClass = interface(IJavaClass)
    ['{4D158A50-0EE9-4296-B3ED-CE89FFDC1BF8}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWNetworkCallbackDelegate')]
  JDWNetworkCallbackDelegate = interface(IJavaInstance)
    ['{F23FBAF9-2FBF-43ED-B30E-DD44B4441F58}']
    procedure onAvailable(network: JNetwork); cdecl;
    procedure onLost(network: JNetwork); cdecl;
    procedure onUnavailable; cdecl;
  end;
  TJDWNetworkCallbackDelegate = class(TJavaGenericImport<JDWNetworkCallbackDelegateClass, JDWNetworkCallbackDelegate>) end;

implementation

end.
