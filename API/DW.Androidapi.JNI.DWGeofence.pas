unit DW.Androidapi.JNI.DWGeofence;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.PlayServices.Maps;

type
  JGeofenceIntentReceiver = interface;
  JGeofenceManager = interface;
  JGeofenceManagerDelegate = interface;
  JGeofenceRegions = interface;
  JGeofenceRegions_Region = interface;

  JGeofenceRegionsClass = interface(JObjectClass)
    ['{24C20C2E-071B-4DA0-A2D5-1A06EFB769C5}']
    {class} function getInstance(context: JContext): JGeofenceRegions; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/GeofenceRegions')]
  JGeofenceRegions = interface(JObject)
    ['{112F6DC2-F71C-4080-B29F-92AE9F0D52D9}']
    procedure add(id: JString; latitude: Double; longitude: Double; radius: Double; transitionTypes: Integer); cdecl;
    function toJson: JString; cdecl;
    procedure clear; cdecl;
    function getItems: JHashMap;
    procedure load; cdecl;
    procedure remove(id: JString); cdecl;
  end;
  TJGeofenceRegions = class(TJavaGenericImport<JGeofenceRegionsClass, JGeofenceRegions>) end;

  JGeofenceRegions_RegionClass = interface(JObjectClass)
    ['{C9968F47-B37D-4EFF-9581-7B5802DEB1A4}']
    { class } function init(id: JString; coords: JLatLng; radius: Double; transitionTypes: Integer): JGeofenceRegions_Region; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/GeofenceRegions$Region')]
  JGeofenceRegions_Region = interface(JObject)
    ['{AB232C99-4D17-46E0-BF72-C9C3E3B8176C}']
    function getId: JString; cdecl;
    function getCoords: JLatLng; cdecl;
    function getRadius: Double; cdecl;
    function getTransitionTypes: Integer; cdecl;
  end;
  TJGeofenceRegions_Region = class(TJavaGenericImport<JGeofenceRegions_RegionClass, JGeofenceRegions_Region>) end;

  JGeofenceManagerDelegateClass = interface(IJavaClass)
    ['{AF4620B1-9D15-4B8D-82F1-A8A2C1F411DB}']
  end;

  [JavaSignature('com/delphiworlds/kastri/GeofenceManagerDelegate')]
  JGeofenceManagerDelegate = interface(IJavaInstance)
    ['{43A4AF5E-4BDB-48E9-9E1F-1F939E4384E4}']
    procedure onGeofenceActionComplete(action: Integer; result: Integer; errorMessage: JString); cdecl;
  end;

  JGeofenceManagerClass = interface(JObjectClass)
    ['{9002B46F-C616-4428-8FCA-F86ED28BD55B}']
    {class} function init(context: JContext; delegate: JGeofenceManagerDelegate): JGeofenceManager; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/GeofenceManager')]
  JGeofenceManager = interface(JObject)
    ['{77A9B1B8-9412-4074-9BBF-FF81F6364174}']
    function getDelphiService: JString; cdecl;
    function getIsMonitoring: Boolean; cdecl;
    function getRegions: JGeofenceRegions; cdecl;
    procedure setDelphiService(serviceName: JString); cdecl;
    procedure start; cdecl;
    procedure stop; cdecl;
  end;
  TJGeofenceManager = class(TJavaGenericImport<JGeofenceManagerClass, JGeofenceManager>) end;

  JGeofenceIntentReceiverClass = interface(JBroadcastReceiverClass)
    ['{EED166F3-8368-4D5A-B238-6277F4A10F8D}']
    {class} function _GetACTION_GEOFENCE_TRANSITION: JString; cdecl;
    {class} function _GetEXTRA_TRANSITION_IDS: JString; cdecl;
    {class} function _GetEXTRA_TRANSITION_LATITUDE: JString; cdecl;
    {class} function _GetEXTRA_TRANSITION_LONGITUDE: JString; cdecl;
    {class} function _GetEXTRA_TRANSITION_TYPE: JString; cdecl;
    {class} property ACTION_GEOFENCE_TRANSITION: JString read _GetACTION_GEOFENCE_TRANSITION;
    {class} property EXTRA_TRANSITION_IDS: JString read _GetEXTRA_TRANSITION_IDS;
    {class} property EXTRA_TRANSITION_LATITUDE: JString read _GetEXTRA_TRANSITION_LATITUDE;
    {class} property EXTRA_TRANSITION_LONGITUDE: JString read _GetEXTRA_TRANSITION_LONGITUDE;
    {class} property EXTRA_TRANSITION_TYPE: JString read _GetEXTRA_TRANSITION_TYPE;
  end;

  [JavaSignature('com/delphiworlds/kastri/GeofenceIntentReceiver')]
  JGeofenceIntentReceiver = interface(JBroadcastReceiver)
    ['{4B7FB36A-B6CE-43F1-98BA-F352D927DD90}']
  end;
  TJGeofenceIntentReceiver = class(TJavaGenericImport<JGeofenceIntentReceiverClass, JGeofenceIntentReceiver>) end;

implementation

end.
