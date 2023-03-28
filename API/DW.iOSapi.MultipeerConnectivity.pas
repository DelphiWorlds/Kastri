unit DW.iOSapi.MultipeerConnectivity;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  // DW
  DW.iOSapi.Foundation;

const
  MCErrorUnknown = 0;
  MCErrorNotConnected = 1;
  MCErrorInvalidParameter = 2;
  MCErrorUnsupported = 3;
  MCErrorTimedOut = 4;
  MCErrorCancelled = 5;
  MCErrorUnavailable = 6;
  MCSessionSendDataReliable = 0;
  MCSessionSendDataUnreliable = 1;
  MCSessionStateNotConnected = 0;
  MCSessionStateConnecting = 1;
  MCSessionStateConnected = 2;
  MCEncryptionOptional = 0;
  MCEncryptionRequired = 1;
  MCEncryptionNone = 2;

type
  MCPeerID = interface;
  MCSession = interface;
  MCSessionDelegate = interface;
  MCNearbyServiceAdvertiser = interface;
  MCNearbyServiceAdvertiserDelegate = interface;
  MCNearbyServiceBrowser = interface;
  MCNearbyServiceBrowserDelegate = interface;
  MCAdvertiserAssistant = interface;
  MCAdvertiserAssistantDelegate = interface;
  MCBrowserViewController = interface;
  MCBrowserViewControllerDelegate = interface;

  MCErrorCode = NSInteger;
  MCSessionSendDataMode = NSInteger;
  MCSessionState = NSInteger;
  MCEncryptionPreference = NSInteger;
  TMCSessionBlockMethod1 = procedure(error: NSError) of object;
  TMCSessionBlockMethod2 = procedure(connectionData: NSData; error: NSError) of object;

  MCPeerIDClass = interface(NSObjectClass)
    ['{9205C9AA-F4EA-4D89-A496-9206D4648039}']
  end;

  MCPeerID = interface(NSObject)
    ['{B7A0EF2C-FE66-46F4-833C-63B593BB6658}']
    function displayName: NSString; cdecl;
    function initWithDisplayName(myDisplayName: NSString): Pointer; cdecl;
  end;
  TMCPeerID = class(TOCGenericImport<MCPeerIDClass, MCPeerID>) end;

  MCSessionClass = interface(NSObjectClass)
    ['{B7CB4AC3-3203-49F7-A7BB-30CF72920FAF}']
  end;

  MCSession = interface(NSObject)
    ['{7ECF9CCB-8708-4242-A520-68C9749FCAAD}']
    procedure cancelConnectPeer(peerID: MCPeerID); cdecl;
    function connectedPeers: NSArray; cdecl;
    procedure connectPeer(peerID: MCPeerID; withNearbyConnectionData: NSData); cdecl;
    function delegate: Pointer; cdecl;
    procedure disconnect; cdecl;
    function encryptionPreference: MCEncryptionPreference; cdecl;
    function initWithPeer(myPeerID: MCPeerID): Pointer; overload; cdecl;
    function initWithPeer(myPeerID: MCPeerID; securityIdentity: NSArray; encryptionPreference: MCEncryptionPreference): Pointer; overload; cdecl;
    function myPeerID: MCPeerID; cdecl;
    procedure nearbyConnectionDataForPeer(peerID: MCPeerID; withCompletionHandler: TMCSessionBlockMethod2); cdecl;
    function securityIdentity: NSArray; cdecl;
    function sendData(data: NSData; toPeers: NSArray; withMode: MCSessionSendDataMode; error: PPointer): Boolean; cdecl;
    function sendResourceAtURL(resourceURL: NSURL; withName: NSString; toPeer: MCPeerID;
      withCompletionHandler: TMCSessionBlockMethod1): NSProgress; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function startStreamWithName(streamName: NSString; toPeer: MCPeerID; error: PPointer): NSOutputStream; cdecl;
  end;
  TMCSession = class(TOCGenericImport<MCSessionClass, MCSession>) end;

  MCSessionDelegate = interface(IObjectiveC)
    ['{8236D8BA-5109-4247-8652-9EC88B40D6E0}']
    procedure session(session: MCSession; didStartReceivingResourceWithName: NSString; fromPeer: MCPeerID; withProgress: NSProgress); overload; cdecl;
    procedure session(session: MCSession; didFinishReceivingResourceWithName: NSString; fromPeer: MCPeerID; atURL: NSURL;
      withError: NSError); overload; cdecl;
    procedure session(session: MCSession; didReceiveCertificate: NSArray; fromPeer: MCPeerID;
      certificateHandler: Pointer); overload; cdecl;
    procedure session(session: MCSession; peer: MCPeerID; didChangeState: MCSessionState); overload; cdecl;
    procedure session(session: MCSession; didReceiveData: NSData; fromPeer: MCPeerID); overload; cdecl;
    procedure session(session: MCSession; didReceiveStream: NSInputStream; withName: NSString; fromPeer: MCPeerID); overload; cdecl;
  end;

  MCNearbyServiceAdvertiserClass = interface(NSObjectClass)
    ['{A03B028E-22B1-447C-B660-73CD06BAF597}']
  end;

  MCNearbyServiceAdvertiser = interface(NSObject)
    ['{258C417D-0A9B-4D9B-BC30-17E73762CA26}']
    function delegate: Pointer; cdecl;
    function discoveryInfo: NSDictionary; cdecl;
    function initWithPeer(myPeerID: MCPeerID; discoveryInfo: NSDictionary; serviceType: NSString): Pointer; cdecl;
    function myPeerID: MCPeerID; cdecl;
    function serviceType: NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startAdvertisingPeer; cdecl;
    procedure stopAdvertisingPeer; cdecl;
  end;
  TMCNearbyServiceAdvertiser = class(TOCGenericImport<MCNearbyServiceAdvertiserClass, MCNearbyServiceAdvertiser>) end;

  MCNearbyServiceAdvertiserDelegate = interface(IObjectiveC)
    ['{21234B15-63E7-4FE5-9A1C-E12624939E31}']
    procedure advertiser(advertiser: MCNearbyServiceAdvertiser; didReceiveInvitationFromPeer: MCPeerID; withContext: NSData;
      invitationHandler: Pointer); overload; cdecl;
    procedure advertiser(advertiser: MCNearbyServiceAdvertiser; didNotStartAdvertisingPeer: NSError); overload; cdecl;
  end;

  MCNearbyServiceBrowserClass = interface(NSObjectClass)
    ['{B2F73F01-72C0-4D41-ACBD-B2D0F38E51B3}']
  end;

  MCNearbyServiceBrowser = interface(NSObject)
    ['{A87A2710-CD09-4030-8889-8C01F9B09FBF}']
    function delegate: Pointer; cdecl;
    function initWithPeer(myPeerID: MCPeerID; serviceType: NSString): Pointer; cdecl;
    procedure invitePeer(peerID: MCPeerID; toSession: MCSession; withContext: NSData; timeout: NSTimeInterval); cdecl;
    function myPeerID: MCPeerID; cdecl;
    function serviceType: NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startBrowsingForPeers; cdecl;
    procedure stopBrowsingForPeers; cdecl;
  end;
  TMCNearbyServiceBrowser = class(TOCGenericImport<MCNearbyServiceBrowserClass, MCNearbyServiceBrowser>) end;

  MCNearbyServiceBrowserDelegate = interface(IObjectiveC)
    ['{AF8E0F7D-0727-4C75-9B6F-476E166AEB00}']
    procedure browser(browser: MCNearbyServiceBrowser; didNotStartBrowsingForPeers: NSError); overload; cdecl;
    procedure browser(browser: MCNearbyServiceBrowser; lostPeer: MCPeerID); overload; cdecl;
    procedure browser(browser: MCNearbyServiceBrowser; foundPeer: MCPeerID; withDiscoveryInfo: NSDictionary); overload; cdecl;
  end;

  MCAdvertiserAssistantClass = interface(NSObjectClass)
    ['{5437EF68-1658-4368-B979-297653FAD08F}']
  end;

  MCAdvertiserAssistant = interface(NSObject)
    ['{45F559BB-1425-41D6-9324-490ACB38C2A8}']
    function delegate: Pointer; cdecl;
    function discoveryInfo: NSDictionary; cdecl;
    function initWithServiceType(serviceType: NSString; discoveryInfo: NSDictionary; session: MCSession): Pointer; cdecl;
    function serviceType: NSString; cdecl;
    function session: MCSession; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure start; cdecl;
    procedure stop; cdecl;
  end;
  TMCAdvertiserAssistant = class(TOCGenericImport<MCAdvertiserAssistantClass, MCAdvertiserAssistant>) end;

  MCAdvertiserAssistantDelegate = interface(IObjectiveC)
    ['{B8A0D41E-CA32-4FE6-A62D-26C4CA73C38A}']
    procedure advertiserAssistantDidDismissInvitation(advertiserAssistant: MCAdvertiserAssistant); cdecl;
    procedure advertiserAssistantWillPresentInvitation(advertiserAssistant: MCAdvertiserAssistant); cdecl;
  end;

  MCBrowserViewControllerClass = interface(UIViewControllerClass)
    ['{76305D6C-9687-43EA-9C10-1226B69C95D5}']
  end;

  MCBrowserViewController = interface(UIViewController)
    ['{D111E349-3A1F-4715-B786-F96930DC4715}']
    function browser: MCNearbyServiceBrowser; cdecl;
    function delegate: Pointer; cdecl;
    function initWithBrowser(browser: MCNearbyServiceBrowser; session: MCSession): Pointer; cdecl;
    function initWithServiceType(serviceType: NSString; session: MCSession): Pointer; cdecl;
    function maximumNumberOfPeers: NSUInteger; cdecl;
    function minimumNumberOfPeers: NSUInteger; cdecl;
    function session: MCSession; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMaximumNumberOfPeers(maximumNumberOfPeers: NSUInteger); cdecl;
    procedure setMinimumNumberOfPeers(minimumNumberOfPeers: NSUInteger); cdecl;
  end;
  TMCBrowserViewController = class(TOCGenericImport<MCBrowserViewControllerClass, MCBrowserViewController>) end;

  MCBrowserViewControllerDelegate = interface(IObjectiveC)
    ['{48251525-3564-476F-B07E-22EDF8F0567E}']
    function browserViewController(browserViewController: MCBrowserViewController; shouldPresentNearbyPeer: MCPeerID;
      withDiscoveryInfo: NSDictionary): Boolean; cdecl;
    procedure browserViewControllerDidFinish(browserViewController: MCBrowserViewController); cdecl;
    procedure browserViewControllerWasCancelled(browserViewController: MCBrowserViewController); cdecl;
  end;

function MCErrorDomain: NSString;
// TODO: Exported const kMCSessionMinimumNumberOfPeers: NSUInteger
// TODO: Exported const kMCSessionMaximumNumberOfPeers: NSUInteger

const
  libMultipeerConnectivity = '/System/Library/Frameworks/MultipeerConnectivity.framework/MultipeerConnectivity';

implementation

uses
  Posix.Dlfcn;

var
  MultipeerConnectivityModule: THandle;

function MCErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMultipeerConnectivity, 'MCErrorDomain');
end;

initialization
  MultipeerConnectivityModule := dlopen(MarshaledAString(libMultipeerConnectivity), RTLD_LAZY);

finalization
  dlclose(MultipeerConnectivityModule);

end.