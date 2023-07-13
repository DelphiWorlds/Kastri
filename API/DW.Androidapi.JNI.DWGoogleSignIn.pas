unit DW.Androidapi.JNI.DWGoogleSignIn;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2021 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.GoogleSignIn;

type
  JDWGoogleSignIn = interface;
  JDWGoogleSignInDelegate = interface;

  JDWGoogleSignInClass = interface(JObjectClass)
    ['{B0146CBE-65C5-4655-B173-017BE49FAE93}']
    {class} function _GetRC_SIGN_IN: Integer; cdecl;
    {class} function init(context: JContext; delegate: JDWGoogleSignInDelegate): JDWGoogleSignIn; cdecl;
    {class} property RC_SIGN_IN: Integer read _GetRC_SIGN_IN;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGoogleSignIn')]
  JDWGoogleSignIn = interface(JObject)
    ['{0393A7A6-EE6B-4EE1-BB24-D2AAB146FE44}']
    // procedure signInSilently; cdecl;
    procedure handleSignInResult(intent: JIntent); cdecl;
    procedure revokeAccess; cdecl;
    procedure setupClient(clientId: JString; scopes: TJavaObjectArray<JString>; requestServerAuthCode: boolean); cdecl;
    procedure signIn; cdecl;
    procedure signOut; cdecl;
    procedure silentSignIn; cdecl;
  end;
  TJDWGoogleSignIn = class(TJavaGenericImport<JDWGoogleSignInClass, JDWGoogleSignIn>) end;

  JDWGoogleSignInDelegateClass = interface(IJavaClass)
    ['{82445839-15D8-4E8B-A740-523930991853}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGoogleSignInDelegate')]
  JDWGoogleSignInDelegate = interface(IJavaInstance)
    ['{F9651A24-E577-4DC8-AF0B-6DDC02E9172D}']
    procedure revokeAccessComplete; cdecl;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl;
    procedure signInComplete(account: JGoogleSignInAccount; statusCode: Integer; statusMessage: JString); cdecl;
    procedure signOutComplete; cdecl;
  end;
  TJDWGoogleSignInDelegate = class(TJavaGenericImport<JDWGoogleSignInDelegateClass, JDWGoogleSignInDelegate>) end;

implementation

end.