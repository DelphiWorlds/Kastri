unit DW.iOSapi.PushKit;

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
  // macOS
  Macapi.ObjectiveC, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  PKPushCredentials = interface;
  PKPushPayload = interface;
  PKPushRegistry = interface;
  PKPushRegistryDelegate = interface;

  PKPushType = NSString;

  PKPushCredentialsClass = interface(NSObjectClass)
    ['{3E434925-7A0F-47F3-A1C6-0D7AE1C8543B}']
  end;

  PKPushCredentials = interface(NSObject)
    ['{BBACB9D2-98D3-4048-BA6D-AF4E5128FFBA}']
    function &type: PKPushType; cdecl;
    function token: NSData; cdecl;
  end;
  TPKPushCredentials = class(TOCGenericImport<PKPushCredentialsClass, PKPushCredentials>) end;

  PKPushPayloadClass = interface(NSObjectClass)
    ['{F066DA5B-5D3D-43F7-9E5D-8AE3C7C216B2}']
  end;

  PKPushPayload = interface(NSObject)
    ['{1F805F0D-D8EB-4694-A2CB-76504F2B8C37}']
    function &type: PKPushType; cdecl;
    function dictionaryPayload: NSDictionary; cdecl;
  end;
  TPKPushPayload = class(TOCGenericImport<PKPushPayloadClass, PKPushPayload>) end;

  PKPushRegistryClass = interface(NSObjectClass)
    ['{98807B48-7926-4FCF-8901-1F9CCB96C606}']
  end;

  PKPushRegistry = interface(NSObject)
    ['{45145A11-AA0F-4BF5-A2C5-3A66B67D23F9}']
    function delegate: Pointer; cdecl;
    function desiredPushTypes: NSSet; cdecl;
    function initWithQueue(queue: dispatch_queue_t): Pointer; cdecl;
    function pushTokenForType(&type: PKPushType): NSData; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDesiredPushTypes(desiredPushTypes: NSSet); cdecl;
  end;
  TPKPushRegistry = class(TOCGenericImport<PKPushRegistryClass, PKPushRegistry>) end;

  PKPushRegistryDelegate = interface(IObjectiveC)
    ['{15106303-561D-47D6-9E76-C32C5B842E00}']
    procedure pushRegistry(registry: PKPushRegistry; didInvalidatePushTokenForType: PKPushType); overload; cdecl;
    procedure pushRegistry(registry: PKPushRegistry; didReceiveIncomingPushWithPayload: PKPushPayload; forType: PKPushType;
      withCompletionHandler: Pointer); overload; cdecl; // procedure of object
    // API_DEPRECATED_WITH_REPLACEMENT("-pushRegistry:(PKPushRegistry *)registry didReceiveIncomingPushWithPayload:(PKPushPayload *)payload
    //   forType:(PKPushType)type withCompletionHandler:(void(^)(void))completion", ios(8.0, 11.0))
    // procedure pushRegistry(registry: PKPushRegistry; didReceiveIncomingPushWithPayload: PKPushPayload; forType: PKPushType); overload; cdecl;
    procedure pushRegistry(registry: PKPushRegistry; didUpdatePushCredentials: PKPushCredentials; forType: PKPushType); overload; cdecl;
  end;

function PKPushTypeVoIP: PKPushType;
function PKPushTypeComplication: PKPushType;
function PKPushTypeFileProvider: PKPushType;

const
  libPushKit = '/System/Library/Frameworks/PushKit.framework/PushKit';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  PushKitModule: THandle;
{$ENDIF}

function PKPushTypeVoIP: PKPushType;
begin
  Result := CocoaNSStringConst(libPushKit, 'PKPushTypeVoIP');
end;

function PKPushTypeComplication: PKPushType;
begin
  Result := CocoaNSStringConst(libPushKit, 'PKPushTypeComplication');
end;

function PKPushTypeFileProvider: PKPushType;
begin
  Result := CocoaNSStringConst(libPushKit, 'PKPushTypeFileProvider');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  PushKitModule := dlopen(MarshaledAString(libPushKit), RTLD_LAZY);

finalization
  dlclose(PushKitModule)
{$ENDIF}

end.