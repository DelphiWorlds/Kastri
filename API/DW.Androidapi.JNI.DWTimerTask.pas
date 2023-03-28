unit DW.Androidapi.JNI.DWTimerTask;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JDWTimerTask = interface;
  JDWTimerTaskDelegate = interface;

  JDWTimerTaskClass = interface(JTimerTaskClass)
    ['{ABD8D248-CDAC-442E-A0E1-CB925C2748DB}']
    {class} function init(delegate: JDWTimerTaskDelegate): JDWTimerTask; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWTimerTask')]
  JDWTimerTask = interface(JTimerTask)
    ['{6746E419-ED13-492D-A256-4D2515E2881D}']
  end;
  TJDWTimerTask = class(TJavaGenericImport<JDWTimerTaskClass, JDWTimerTask>) end;

  JDWTimerTaskDelegateClass = interface(IJavaClass)
    ['{361A0781-2953-4702-8D12-492C38CC9723}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWTimerTaskDelegate')]
  JDWTimerTaskDelegate = interface(IJavaInstance)
    ['{C2A5E444-EAA4-4447-919D-F06B6A38C102}']
    procedure run; cdecl;
  end;
  TJDWTimerTaskDelegate = class(TJavaGenericImport<JDWTimerTaskDelegateClass, JDWTimerTaskDelegate>) end;

implementation

end.
