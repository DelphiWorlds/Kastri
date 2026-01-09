unit DW.Androidapi.JNI.DWMediaProjectionCallback;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Media;

type
  JDWMediaProjectionCallback = interface;
  JDWMediaProjectionCallbackDelegate = interface;

  JDWMediaProjectionCallbackDelegateClass = interface(IJavaClass)
    ['{9CA123C1-AD9E-4858-A50D-8A66FB4CAF14}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWMediaProjectionCallbackDelegate')]
  JDWMediaProjectionCallbackDelegate = interface(IJavaInstance)
    ['{D27CB132-E79F-4F41-B5FE-8F8848FB7BBC}']
    procedure onCapturedContentResize(width: Integer; height: Integer); cdecl;
    procedure onCapturedContentVisibilityChanged(isVisible: Boolean); cdecl;
    procedure onStop; cdecl;
  end;
  TJDWMediaProjectionCallbackDelegate = class(TJavaGenericImport<JDWMediaProjectionCallbackDelegateClass, JDWMediaProjectionCallbackDelegate>) end;

  JDWMediaProjectionCallbackClass = interface(JMediaProjection_CallbackClass)
    ['{ACB33B31-80FF-4188-98C2-92E20D28E16C}']
    {class} function init(delegate: JDWMediaProjectionCallbackDelegate): JDWMediaProjectionCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWMediaProjectionCallback')]
  JDWMediaProjectionCallback = interface(JMediaProjection_Callback)
    ['{8826D00A-BDE3-4D9A-BD86-C091D6B8627F}']
    procedure onCapturedContentResize(width: Integer; height: Integer); cdecl;
    procedure onCapturedContentVisibilityChanged(isVisible: Boolean); cdecl;
    procedure onStop; cdecl;
  end;
  TJDWMediaProjectionCallback = class(TJavaGenericImport<JDWMediaProjectionCallbackClass, JDWMediaProjectionCallback>) end;

implementation

end.
