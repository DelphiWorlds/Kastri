unit DW.Androidapi.JNI.DWFusedLocation;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Location, AndroidApi.JNI.GraphicsContentViewText;

type
  JDWFusedLocationClient = interface;
  JDWFusedLocationClientDelegate = interface;

  JDWFusedLocationClientClass = interface(JObjectClass)
    ['{159B5068-9D62-4A8F-A3A2-2BA5D9E45657}']
    {class} function init(context: JContext; delegate: JDWFusedLocationClientDelegate): JDWFusedLocationClient; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFusedLocationClient')]
  JDWFusedLocationClient = interface(JObject)
    ['{8C54A1DE-2B00-4C77-B1C5-6D07E1174076}']
    function getFastestInterval: Int64; cdecl;
    function getInterval: Int64; cdecl;
    function getIsActive: Boolean; cdecl;
    function getIsMockMode: Boolean; cdecl;
    function getPriority: Integer; cdecl;
    function getSmallestDisplacement: Single; cdecl;
    procedure requestLastKnownLocation; cdecl;
    procedure setFastestInterval(interval: Int64); cdecl;
    procedure setInterval(interval: Int64); cdecl;
    procedure setMockLocation(latitude: Double; longitude: Double); cdecl;
    procedure setMockMode(isMockMode: Boolean); cdecl;
    procedure setPriority(interval: Integer); cdecl;
    procedure setSmallestDisplacement(value: Single); cdecl;
    procedure startLocationUpdates; cdecl;
    procedure stopLocationUpdates; cdecl;
  end;
  TJDWFusedLocationClient = class(TJavaGenericImport<JDWFusedLocationClientClass, JDWFusedLocationClient>) end;

  JDWFusedLocationClientDelegateClass = interface(IJavaClass)
    ['{D2E8844A-D046-46E3-B5E5-C0A56A7A4D40}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFusedLocationClientDelegate')]
  JDWFusedLocationClientDelegate = interface(IJavaInstance)
    ['{C8A0BF7F-0CC2-4D8E-A680-343345D6D187}']
    procedure onLocation(location: JLocation); cdecl;
    procedure onLocationUpdatesChange(active: Boolean); cdecl;
    procedure onSetMockLocationResult(location: JLocation); cdecl;
    procedure onSetMockModeResult(success: Boolean); cdecl;
  end;
  TJDWFusedLocationClientDelegate = class(TJavaGenericImport<JDWFusedLocationClientDelegateClass, JDWFusedLocationClientDelegate>) end;

implementation

end.
