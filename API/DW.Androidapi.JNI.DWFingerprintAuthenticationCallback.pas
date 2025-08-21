unit DW.Androidapi.JNI.DWFingerprintAuthenticationCallback;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.Java.Security, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.Hardware;

type
  JDWFingerprintAuthenticationCallbackDelegate = interface;
  JDWFingerprintAuthenticationCallback = interface;

  /// <summary>
  ///   TJDWFingerprintAuthenticationCallback is the Delphi import of the DWAuthenticationCallback class
  ///   It implements overrides for the necessary methods for authentication results, and
  ///   forwards them on to a Delegate interface assigned to it. The TDWAuthenticationCallbackDelegate
  ///   is the implementation of this interface on the Delphi side
  /// <summary>
  JDWFingerprintAuthenticationCallbackClass = interface(JFingerprintManager_AuthenticationCallbackClass)
    ['{42688F3E-2982-490D-BC3B-BEDA774C8CC7}']
    {class} function init(delegate: JDWFingerprintAuthenticationCallbackDelegate): JDWFingerprintAuthenticationCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFingerprintAuthenticationCallback')]
  JDWFingerprintAuthenticationCallback = interface(JFingerprintManager_AuthenticationCallback)
    ['{CBB0B24C-3389-4A6D-A550-F22C5DBDC9DD}']
    procedure onAuthenticationError(errMsgId: Integer; errString: JCharSequence); cdecl;
    procedure onAuthenticationFailed; cdecl;
    procedure onAuthenticationHelp(helpMsgId: Integer; helpString: JCharSequence); cdecl;
    procedure onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult); cdecl;
  end;
  TJDWFingerprintAuthenticationCallback = class(TJavaGenericImport<JDWFingerprintAuthenticationCallbackClass, JDWFingerprintAuthenticationCallback>)
  end;

  JDWFingerprintAuthenticationCallbackDelegateClass = interface(IJavaClass)
    ['{4800271A-81D5-4B35-93FD-FD515F085DB8}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFingerprintAuthenticationCallbackDelegate')]
  JDWFingerprintAuthenticationCallbackDelegate = interface(IJavaInstance)
    ['{74B1F0C6-B78F-424B-B7B4-27B090F413A9}']
    procedure onAuthenticationError(errMsgId: Integer; errString: JCharSequence); cdecl;
    procedure onAuthenticationFailed; cdecl;
    procedure onAuthenticationHelp(helpMsgId: Integer; helpString: JCharSequence); cdecl;
    procedure onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult); cdecl;
  end;
  TJDWFingerprintAuthenticationCallbackDelegate = class(TJavaGenericImport<JDWFingerprintAuthenticationCallbackDelegateClass,
    JDWFingerprintAuthenticationCallbackDelegate>)
  end;

implementation

end.

