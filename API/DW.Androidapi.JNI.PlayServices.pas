unit DW.Androidapi.JNI.PlayServices;

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
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  JScope = interface;
  JCommonStatusCodes = interface;

  JScopeClass = interface(JObjectClass)
    ['{92B24AEE-2F21-4421-BEA0-56D622112E1C}']
    {class} function init(scopeUri: JString): JScope; cdecl;
  end;

  [JavaSignature('com/google/android/gms/common/api/Scope')]
  JScope = interface(JObject)
    ['{B2FA592B-4FAF-4B58-9C57-C9AF95F95FB9}']
    function dD: JString; cdecl;
  end;
  TJScope = class(TJavaGenericImport<JScopeClass, JScope>) end;

  JCommonStatusCodesClass = interface(JObjectClass)
    ['{7E439158-D82B-4B03-9A23-6BBB88D90A29}']
    {class} function _GetDATE_INVALID: Integer; cdecl;
    {class} function _GetDEVELOPER_ERROR: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINTERRUPTED: Integer; cdecl;
    {class} function _GetINVALID_ACCOUNT: Integer; cdecl;
    {class} function _GetLICENSE_CHECK_FAILED: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetRESOLUTION_REQUIRED: Integer; cdecl;
    {class} function _GetSERVICE_DISABLED: Integer; cdecl;
    {class} function _GetSERVICE_INVALID: Integer; cdecl;
    {class} function _GetSERVICE_MISSING: Integer; cdecl;
    {class} function _GetSERVICE_VERSION_UPDATE_REQUIRED: Integer; cdecl;
    {class} function _GetSIGN_IN_REQUIRED: Integer; cdecl;
    {class} function _GetSUCCESS: Integer; cdecl;
    {class} function _GetSUCCESS_CACHE: Integer; cdecl;
    {class} function _GetTIMEOUT: Integer; cdecl;
    {class} function getStatusCodeString(statusCode: Integer): JString; cdecl;
    {class} function init: JCommonStatusCodes; cdecl;
    {class} property DATE_INVALID: Integer read _GetDATE_INVALID;
    {class} property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INTERRUPTED: Integer read _GetINTERRUPTED;
    {class} property INVALID_ACCOUNT: Integer read _GetINVALID_ACCOUNT;
    {class} property LICENSE_CHECK_FAILED: Integer read _GetLICENSE_CHECK_FAILED;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property RESOLUTION_REQUIRED: Integer read _GetRESOLUTION_REQUIRED;
    {class} property SERVICE_DISABLED: Integer read _GetSERVICE_DISABLED;
    {class} property SERVICE_INVALID: Integer read _GetSERVICE_INVALID;
    {class} property SERVICE_MISSING: Integer read _GetSERVICE_MISSING;
    {class} property SERVICE_VERSION_UPDATE_REQUIRED: Integer read _GetSERVICE_VERSION_UPDATE_REQUIRED;
    {class} property SIGN_IN_REQUIRED: Integer read _GetSIGN_IN_REQUIRED;
    {class} property SUCCESS: Integer read _GetSUCCESS;
    {class} property SUCCESS_CACHE: Integer read _GetSUCCESS_CACHE;
    {class} property TIMEOUT: Integer read _GetTIMEOUT;
  end;

  [JavaSignature('com/google/android/gms/common/api/CommonStatusCodes')]
  JCommonStatusCodes = interface(JObject)
    ['{BDB21EAA-F613-4B7F-A956-629E7F1F3CC9}']
  end;
  TJCommonStatusCodes = class(TJavaGenericImport<JCommonStatusCodesClass, JCommonStatusCodes>) end;

implementation

end.
