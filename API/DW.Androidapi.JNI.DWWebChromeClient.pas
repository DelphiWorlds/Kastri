unit DW.Androidapi.JNI.DWWebChromeClient;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.WebKit, Androidapi.JNI.GraphicsContentViewText;

type
  JDWWebChromeClient = interface;
  JDWWebChromeClientDelegate = interface;

  JDWWebChromeClientClass = interface(JWebChromeClientClass)
    ['{937DE965-7D1B-439E-8C83-134CC8C3E082}']
    {class} function init(delegate: JDWWebChromeClientDelegate): JDWWebChromeClient; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWWebChromeClient')]
  JDWWebChromeClient = interface(JWebChromeClient)
    ['{30473AA6-1CCA-44AC-AA08-EBD8FA926090}']
    procedure handleFileChooserResult(intent: JIntent; resultCode: Integer); cdecl;
  end;
  TJDWWebChromeClient = class(TJavaGenericImport<JDWWebChromeClientClass, JDWWebChromeClient>) end;

  JDWWebChromeClientDelegateClass = interface(IJavaClass)
    ['{793CF960-BFB7-485E-93D8-8879AA78A14A}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWWebChromeClientDelegate')]
  JDWWebChromeClientDelegate = interface(IJavaInstance)
    ['{C2A5E444-EAA4-4447-919D-F06B6A38C102}']
    function onFileChooserIntent(intent: JIntent): Boolean; cdecl;
  end;
  TJDWWebChromeClientDelegate = class(TJavaGenericImport<JDWWebChromeClientDelegateClass, JDWWebChromeClientDelegate>) end;

implementation

end.
