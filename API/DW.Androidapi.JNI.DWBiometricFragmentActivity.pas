unit DW.Androidapi.JNI.DWBiometricFragmentActivity;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.Java.Security, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.AndroidX.Activity;

type
  JDWBiometricFragmentActivity = interface;

  JDWBiometricFragmentActivityClass = interface(JFragmentActivityClass)
    ['{FC751FC8-4506-4B2A-BDAC-4A764B21A200}']
    {class} function _GetACTION_AUTHENTICATION: JString; cdecl;
    {class} function _GetAUTHENTICATION_RESULT_ERROR: JString; cdecl;
    {class} function _GetAUTHENTICATION_RESULT_FAILED: JString; cdecl;
    {class} function _GetAUTHENTICATION_RESULT_SUCCESS: JString; cdecl;
    {class} function _GetEXTRA_AUTHENTICATION_RESULT: JString; cdecl;
    {class} function _GetEXTRA_AUTHENTICATION_ERROR_CODE: JString; cdecl;
    {class} function _GetEXTRA_AUTHENTICATION_ERROR_MESSAGE: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_CANCEL_BUTTON_TEXT: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_DESCRIPTION: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_SUBTITLE: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_TITLE: JString; cdecl;
    {class} function _GetEXTRA_PROMPT_CONFIRMATION_REQUIRED: JString; cdecl;
    {class} procedure cancel; cdecl;
    {class} procedure start(context: JContext; intent: JIntent); cdecl;
    {class} procedure stop; cdecl;
    {class} property ACTION_AUTHENTICATION: JString read _GetACTION_AUTHENTICATION;
    {class} property AUTHENTICATION_RESULT_ERROR: JString read _GetAUTHENTICATION_RESULT_ERROR;
    {class} property AUTHENTICATION_RESULT_FAILED: JString read _GetAUTHENTICATION_RESULT_FAILED;
    {class} property AUTHENTICATION_RESULT_SUCCESS: JString read _GetAUTHENTICATION_RESULT_SUCCESS;
    {class} property EXTRA_AUTHENTICATION_RESULT: JString read _GetEXTRA_AUTHENTICATION_RESULT;
    {class} property EXTRA_AUTHENTICATION_ERROR_CODE: JString read _GetEXTRA_AUTHENTICATION_ERROR_CODE;
    {class} property EXTRA_AUTHENTICATION_ERROR_MESSAGE: JString read _GetEXTRA_AUTHENTICATION_ERROR_MESSAGE;
    {class} property EXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL: JString read _GetEXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL;
    {class} property EXTRA_PROMPT_CANCEL_BUTTON_TEXT: JString read _GetEXTRA_PROMPT_CANCEL_BUTTON_TEXT;
    {class} property EXTRA_PROMPT_DESCRIPTION: JString read _GetEXTRA_PROMPT_DESCRIPTION;
    {class} property EXTRA_PROMPT_SUBTITLE: JString read _GetEXTRA_PROMPT_SUBTITLE;
    {class} property EXTRA_PROMPT_TITLE: JString read _GetEXTRA_PROMPT_TITLE;
    {class} property EXTRA_PROMPT_CONFIRMATION_REQUIRED: JString read _GetEXTRA_PROMPT_CONFIRMATION_REQUIRED;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWBiometricFragmentActivity')]
  JDWBiometricFragmentActivity = interface(JFragmentActivity)
    ['{5798D3A5-C761-4963-901F-DD8DE7A1A7F2}']
  end;
  TJDWBiometricFragmentActivity = class(TJavaGenericImport<JDWBiometricFragmentActivityClass, JDWBiometricFragmentActivity>)
  end;

implementation

end.

