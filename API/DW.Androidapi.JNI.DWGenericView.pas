unit DW.Androidapi.JNI.DWGenericView;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  JGenericView = interface;
  JGenericViewDelegate = interface;

  JGenericViewDelegateClass = interface(IJavaClass)
    ['{AF4620B1-9D15-4B8D-82F1-A8A2C1F411DB}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGenericViewDelegate')]
  JGenericViewDelegate = interface(IJavaInstance)
    ['{43A4AF5E-4BDB-48E9-9E1F-1F939E4384E4}']
    procedure onMouseWheel(event: JMotionEvent); cdecl;
  end;

  JGenericViewClass = interface(JViewClass)
    ['{9002B46F-C616-4428-8FCA-F86ED28BD55B}']
    {class} function init(context: JContext; delegate: JGenericViewDelegate): JGenericView; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGenericView')]
  JGenericView = interface(JView)
    ['{77A9B1B8-9412-4074-9BBF-FF81F6364174}']
  end;
  TJGenericView = class(TJavaGenericImport<JGenericViewClass, JGenericView>) end;

implementation

end.
