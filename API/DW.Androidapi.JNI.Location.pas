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
  JLocationAvailability = interface;
  JLocationCallback = interface;
  JLocationRequest = interface;
  JLocationResult = interface;

  JLocationAvailabilityClass = interface(JAbstractSafeParcelableClass)
    ['{3EF9395A-52E4-4BE9-93DC-48C4BBF8706B}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function extractLocationAvailability(intent: JIntent): JLocationAvailability; cdecl;
    {class} function hasLocationAvailability(intent: JIntent): Boolean; cdecl;
    {class} function isLocationAvailable: Boolean; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/location/LocationAvailability')]
  JLocationAvailability = interface(JAbstractSafeParcelable)
    ['{69694160-2B2B-43BC-9A69-814408569F14}']
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJLocationAvailability = class(TJavaGenericImport<JLocationAvailabilityClass, JLocationAvailability>) end;

  JLocationCallbackClass = interface(JObjectClass)
    ['{CA9F2969-EA45-4127-A4C7-9BE4E020B74D}']
    {class} function init: JLocationCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/location/LocationCallback')]
  JLocationCallback = interface(JObject)
    ['{3D563A0E-283B-4F10-BAE1-CE2F265CD931}']
    procedure onLocationAvailability(locationAvailability: JLocationAvailability); cdecl;
    procedure onLocationResult(locationResult: JLocationResult); cdecl;
  end;
  TJLocationCallback = class(TJavaGenericImport<JLocationCallbackClass, JLocationCallback>) end;

  JLocationRequestClass = interface(JAbstractSafeParcelableClass)
    ['{05119402-67C2-4B28-9127-852977268AC1}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPRIORITY_BALANCED_POWER_ACCURACY: Integer; cdecl;
    {class} function _GetPRIORITY_HIGH_ACCURACY: Integer; cdecl;
    {class} function _GetPRIORITY_LOW_POWER: Integer; cdecl;
    {class} function _GetPRIORITY_NO_POWER: Integer; cdecl;
    {class} function create: JLocationRequest; cdecl;
    {class} function init: JLocationRequest; cdecl; overload;
    {class} function init(i: Integer; l: Int64; l1: Int64; b: Boolean; l2: Int64; i1: Integer; f: Single; l3: Int64;
      b1: Boolean): JLocationRequest; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property PRIORITY_BALANCED_POWER_ACCURACY: Integer read _GetPRIORITY_BALANCED_POWER_ACCURACY;
    {class} property PRIORITY_HIGH_ACCURACY: Integer read _GetPRIORITY_HIGH_ACCURACY;
    {class} property PRIORITY_LOW_POWER: Integer read _GetPRIORITY_LOW_POWER;
    {class} property PRIORITY_NO_POWER: Integer read _GetPRIORITY_NO_POWER;
  end;

  [JavaSignature('com/google/android/gms/location/LocationRequest')]
  JLocationRequest = interface(JAbstractSafeParcelable)
    ['{C5E84DA4-6E65-4EBC-85C2-E084C59E736B}']
    function equals(object_: JObject): Boolean; cdecl;
    function getExpirationTime: Int64; cdecl;
    function getFastestInterval: Int64; cdecl;
    function getInterval: Int64; cdecl;
    function getMaxWaitTime: Int64; cdecl;
    function getNumUpdates: Integer; cdecl;
    function getPriority: Integer; cdecl;
    function getSmallestDisplacement: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isFastestIntervalExplicitlySet: Boolean; cdecl;
    function isWaitForAccurateLocation: Boolean; cdecl;
    function setExpirationDuration(l: Int64): JLocationRequest; cdecl;
    function setExpirationTime(l: Int64): JLocationRequest; cdecl;
    function setFastestInterval(l: Int64): JLocationRequest; cdecl;
    function setInterval(l: Int64): JLocationRequest; cdecl;
    function setMaxWaitTime(l: Int64): JLocationRequest; cdecl;
    function setNumUpdates(i: Integer): JLocationRequest; cdecl;
    function setPriority(i: Integer): JLocationRequest; cdecl;
    function setSmallestDisplacement(f: Single): JLocationRequest; cdecl;
    function setWaitForAccurateLocation(b: Boolean): JLocationRequest; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJLocationRequest = class(TJavaGenericImport<JLocationRequestClass, JLocationRequest>) end;

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
