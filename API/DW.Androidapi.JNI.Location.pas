unit DW.Androidapi.JNI.Location;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Location,
  Androidapi.JNI.PlayServices, Androidapi.JNI.Os;

type
  JLocationResult = interface;

  JLocationResultClass = interface(JAbstractSafeParcelableClass)
    ['{A26ADB0C-854E-4739-B595-49BEBEBA82DA}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function create(locations: JList): JLocationResult; cdecl;
    {class} function extractResult(intent: JIntent): JLocationResult; cdecl;
    {class} function hasResult(intent: JIntent): Boolean; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/location/LocationResult')]
  JLocationResult = interface(JAbstractSafeParcelable)
    ['{A8A58510-0204-46B0-A249-E469B041E1EA}']
    function equals(other: JObject): Boolean; cdecl;
    function getLastLocation: JLocation; cdecl;
    function getLocations: JList; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJLocationResult = class(TJavaGenericImport<JLocationResultClass, JLocationResult>) end;

implementation

end.
