unit DW.iOSapi.MLKitCommon;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

type
  MLKRemoteModel = interface;
  MLKCustomRemoteModel = interface;
  MLKLocalModel = interface;
  MLKModelManager = interface;
  MLKRemoteModelSource = interface;
  MLKModelDownloadConditions = interface;

  MLKModelDownloadUserInfoKey = NSString;
  TMLKModelManagerBlockMethod1 = procedure(error: NSError) of object;

  MLKRemoteModelClass = interface(NSObjectClass)
    ['{2825EDCD-758B-47D5-95DF-72F3DE5FB5B1}']
  end;

  MLKRemoteModel = interface(NSObject)
    ['{EEC957CE-50C3-4908-991B-4BAD245B76D9}']
    function name: NSString; cdecl;
  end;
  TMLKRemoteModel = class(TOCGenericImport<MLKRemoteModelClass, MLKRemoteModel>) end;

  MLKCustomRemoteModelClass = interface(MLKRemoteModelClass)
    ['{42CD11AD-5506-42CE-AFED-E9E8BDBF7579}']
  end;

  MLKCustomRemoteModel = interface(MLKRemoteModel)
    ['{6403987F-8ECD-4775-A789-0E0AB7DB0A9E}']
    function initWithRemoteModelSource(remoteModelSource: MLKRemoteModelSource): Pointer; cdecl;
  end;
  TMLKCustomRemoteModel = class(TOCGenericImport<MLKCustomRemoteModelClass, MLKCustomRemoteModel>) end;

  MLKLocalModelClass = interface(NSObjectClass)
    ['{15C4E7C0-1091-44E6-AFD5-527E93247737}']
  end;

  MLKLocalModel = interface(NSObject)
    ['{6A8866F9-F2C6-49B5-AFDE-4962DA36EE45}']
    function initWithManifestPath(manifestPath: NSString): Pointer; cdecl;
    function initWithPath(path: NSString): Pointer; cdecl;
    function manifestPath: NSString; cdecl;
    function path: NSString; cdecl;
  end;
  TMLKLocalModel = class(TOCGenericImport<MLKLocalModelClass, MLKLocalModel>) end;

  MLKModelManagerClass = interface(NSObjectClass)
    ['{C95BC728-CD82-4254-A341-8EB59D5D9304}']
    {class} function modelManager: Pointer; cdecl;
  end;

  MLKModelManager = interface(NSObject)
    ['{2964E6DE-A49E-4C8E-B13D-24D46651385F}']
    procedure deleteDownloadedModel(remoteModel: MLKRemoteModel; completion: TMLKModelManagerBlockMethod1); cdecl;
    function downloadModel(remoteModel: MLKRemoteModel; conditions: MLKModelDownloadConditions): NSProgress; cdecl;
    function isModelDownloaded(remoteModel: MLKRemoteModel): Boolean; cdecl;
  end;
  TMLKModelManager = class(TOCGenericImport<MLKModelManagerClass, MLKModelManager>) end;

  MLKRemoteModelSourceClass = interface(NSObjectClass)
    ['{204082B5-B574-4A74-8D6E-4EE3D3D2A60D}']
  end;

  MLKRemoteModelSource = interface(NSObject)
    ['{DF15493D-D6BC-4F1A-809A-C267B5920EC0}']
  end;
  TMLKRemoteModelSource = class(TOCGenericImport<MLKRemoteModelSourceClass, MLKRemoteModelSource>) end;

  MLKModelDownloadConditionsClass = interface(NSObjectClass)
    ['{5C2FA75D-06A9-48AA-9838-349999B4232B}']
  end;

  MLKModelDownloadConditions = interface(NSObject)
    ['{3D3AB2DF-05AE-4360-B48A-766C268C081A}']
    function allowsBackgroundDownloading: Boolean; cdecl;
    function allowsCellularAccess: Boolean; cdecl;
    function initWithAllowsCellularAccess(allowsCellularAccess: Boolean; allowsBackgroundDownloading: Boolean): Pointer; cdecl;
  end;
  TMLKModelDownloadConditions = class(TOCGenericImport<MLKModelDownloadConditionsClass, MLKModelDownloadConditions>) end;

implementation

procedure AccelerateLoader; cdecl; external framework 'Accelerate';
procedure CLangRTLoader; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
procedure GoogleDataTransportLoader; cdecl; external framework 'GoogleDataTransport';
procedure GoogleToolboxForMacLoader; cdecl; external 'libGoogleToolboxForMac.a';
procedure GoogleUtilitiesComponentsLoader; cdecl; external 'libGoogleUtilitiesComponents.a';
// NOTE: When combining Firebase Cloud Messaging with ML Kit, Firebase iOS SDK v10.8.0 MUST be used (as at 22-SEP-2024)
{$IF not Defined(FIREBASE)}
procedure FBLPromisesLoader; cdecl; external 'libPromisesObjC.a';
procedure GoogleUtilitiesLoader; cdecl; external 'libGoogleUtilities.a';
procedure GTMSessionFetcherLoader; cdecl; external framework 'GTMSessionFetcher';
{$ELSE}
procedure FBLPromisesLoader; cdecl; external framework 'FBLPromises';
procedure GTMSessionFetcherLoader; cdecl; external 'libGTMSessionFetcher.a';
{$ENDIF}
procedure MLImageLoader; cdecl; external framework 'MLImage';
procedure MLKitCommonLoader; cdecl; external framework 'MLKitCommon';
procedure nanoPBLoader; cdecl; external framework 'nanoPB';

end.