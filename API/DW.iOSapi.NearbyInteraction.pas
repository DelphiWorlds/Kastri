unit DW.iOSapi.NearbyInteraction;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.Macapi.Simd, DW.iOSapi.Foundation;

const
  NIErrorCodeUnsupportedPlatform = -5889;
  NIErrorCodeInvalidConfiguration = -5888;
  NIErrorCodeSessionFailed = -5887;
  NIErrorCodeResourceUsageTimeout = -5886;
  NIErrorCodeActiveSessionsLimitExceeded = -5885;
  NIErrorCodeUserDidNotAllow = -5884;
  NINearbyObjectRemovalReasonTimeout = 0;
  NINearbyObjectRemovalReasonPeerEnded = 1;

type
  NIDiscoveryToken = interface;
  NIConfiguration = interface;
  NINearbyPeerConfiguration = interface;
  NINearbyAccessoryConfiguration = interface;
  NINearbyObject = interface;
  NISession = interface;
  NISessionDelegate = interface;

  NIErrorCode = NSInteger;
  NINearbyObjectRemovalReason = NSInteger;

  NIDiscoveryTokenClass = interface(NSObjectClass)
    ['{342747FD-BB4D-4B70-AC47-8120CF75003B}']
    {class} function new: Pointer; cdecl;
  end;

  NIDiscoveryToken = interface(NSObject)
    ['{6CB8C43E-9BAA-4F23-AC0C-153938514A76}']
  end;
  TNIDiscoveryToken = class(TOCGenericImport<NIDiscoveryTokenClass, NIDiscoveryToken>) end;

  NIConfigurationClass = interface(NSObjectClass)
    ['{506AB824-EE4A-4E2E-BD78-4A07D6DB14C4}']
    {class} function new: Pointer; cdecl;
  end;

  NIConfiguration = interface(NSObject)
    ['{F2215E36-09E8-498C-8B23-AB3F01C4C9DE}']
  end;
  TNIConfiguration = class(TOCGenericImport<NIConfigurationClass, NIConfiguration>) end;

  NINearbyPeerConfigurationClass = interface(NIConfigurationClass)
    ['{12621855-7E18-4C0F-A9B5-8330E96B0A03}']
    {class} function new: Pointer; cdecl;
  end;

  NINearbyPeerConfiguration = interface(NIConfiguration)
    ['{2C775A35-36A8-4289-98B7-49FB65F9CAA6}']
    function initWithPeerToken(peerToken: NIDiscoveryToken): Pointer; cdecl;
    function peerDiscoveryToken: NIDiscoveryToken; cdecl;
  end;
  TNINearbyPeerConfiguration = class(TOCGenericImport<NINearbyPeerConfigurationClass, NINearbyPeerConfiguration>) end;

  NINearbyAccessoryConfigurationClass = interface(NIConfigurationClass)
    ['{CBEFD3B7-190C-4EA0-9CAB-F70A78A73F14}']
    {class} function new: Pointer; cdecl;
  end;

  NINearbyAccessoryConfiguration = interface(NIConfiguration)
    ['{79CD5773-597A-4CFC-9FB8-8AC85D8D7F66}']
    function accessoryDiscoveryToken: NIDiscoveryToken; cdecl;
    function initWithData(data: NSData; error: PPointer): Pointer; cdecl;
  end;
  TNINearbyAccessoryConfiguration = class(TOCGenericImport<NINearbyAccessoryConfigurationClass, NINearbyAccessoryConfiguration>) end;

  NINearbyObjectClass = interface(NSObjectClass)
    ['{FD4AEC93-0E96-4D3F-A0EB-1C5CFD716EC1}']
    {class} function new: Pointer; cdecl;
  end;

  NINearbyObject = interface(NSObject)
    ['{64BE00FD-7C82-46F4-A499-57BC4DD6BEC2}']
    function direction: simd_float3; cdecl;
    function discoveryToken: NIDiscoveryToken; cdecl;
    function distance: Single; cdecl;
  end;
  TNINearbyObject = class(TOCGenericImport<NINearbyObjectClass, NINearbyObject>) end;

  NISessionClass = interface(NSObjectClass)
    ['{20FBD532-FA49-493E-9F81-82AF3F828018}']
    {class} function isSupported: Boolean; cdecl;
  end;

  NISession = interface(NSObject)
    ['{213FCF83-C51A-45E8-A908-1E45A8A6D64C}']
    function configuration: NIConfiguration; cdecl;
    function delegate: Pointer; cdecl;
    function delegateQueue: dispatch_queue_t; cdecl;
    function discoveryToken: NIDiscoveryToken; cdecl;
    procedure invalidate; cdecl;
    procedure pause; cdecl;
    procedure runWithConfiguration(configuration: NIConfiguration); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDelegateQueue(delegateQueue: dispatch_queue_t); cdecl;
  end;
  TNISession = class(TOCGenericImport<NISessionClass, NISession>) end;

  NISessionDelegate = interface(IObjectiveC)
    ['{3C364B34-5762-46D0-B7B4-8F3D968BE359}']
    procedure session(session: NISession; didInvalidateWithError: NSError); overload; cdecl;
    procedure session(session: NISession; didGenerateShareableConfigurationData: NSData; forObject: NINearbyObject); overload; cdecl;
    procedure session(session: NISession; didRemoveNearbyObjects: NSArray; withReason: NINearbyObjectRemovalReason); overload; cdecl;
    procedure session(session: NISession; didUpdateNearbyObjects: NSArray); overload; cdecl;
    procedure sessionSuspensionEnded(session: NISession); cdecl;
    procedure sessionWasSuspended(session: NISession); cdecl;
  end;

function NIErrorDomain: NSErrorDomain;
// TODO: Exported const NINearbyObjectDistanceNotAvailable: float
// TODO: Exported const NINearbyObjectDirectionNotAvailable: simd_float3

const
  libNearbyInteraction = '/System/Library/Frameworks/NearbyInteraction.framework/NearbyInteraction';

implementation

uses
  Posix.Dlfcn;

var
  NearbyInteractionModule: THandle;

function NIErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libNearbyInteraction, 'NIErrorDomain');
end;

initialization
  NearbyInteractionModule := dlopen(MarshaledAString(libNearbyInteraction), RTLD_LAZY);

finalization
  dlclose(NearbyInteractionModule);

end.