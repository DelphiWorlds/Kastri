unit DW.Macapi.MultipeerConnectivity;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  // DW
  DW.Macapi.Foundation;

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
    ['{200525EA-CDF8-44A6-82BC-520684EF5102}']
  end;

  MCPeerID = interface(NSObject)
    ['{F6CF03BB-0D6B-4756-A08C-D1C4CC9A168D}']
    function displayName: NSString; cdecl;
    function initWithDisplayName(myDisplayName: NSString): Pointer; cdecl;
  end;
  TMCPeerID = class(TOCGenericImport<MCPeerIDClass, MCPeerID>) end;

  MCSessionClass = interface(NSObjectClass)
    ['{A23E989D-0165-4F58-BA50-48497A5AA003}']
  end;

  MCSession = interface(NSObject)
    ['{DCBFF385-8EBB-486B-B894-E4D3C1A13C0F}']
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
    function sendResourceAtURL(resourceURL: NSURL; withName: NSString; toPeer: MCPeerID; withCompletionHandler: TMCSessionBlockMethod1): NSProgress; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function startStreamWithName(streamName: NSString; toPeer: MCPeerID; error: PPointer): NSOutputStream; cdecl;
  end;
  TMCSession = class(TOCGenericImport<MCSessionClass, MCSession>) end;

  MCSessionDelegate = interface(IObjectiveC)
    ['{7C0D4BCC-B6E7-4092-A13E-229AAE05F5B7}']
    procedure session(session: MCSession; didStartReceivingResourceWithName: NSString; fromPeer: MCPeerID; withProgress: NSProgress); overload; cdecl;
    procedure session(session: MCSession; didFinishReceivingResourceWithName: NSString; fromPeer: MCPeerID; atURL: NSURL; withError: NSError); overload; cdecl;
    procedure session(session: MCSession; didReceiveCertificate: NSArray; fromPeer: MCPeerID; certificateHandler: Pointer); overload; cdecl;
    procedure session(session: MCSession; peer: MCPeerID; didChangeState: MCSessionState); overload; cdecl;
    procedure session(session: MCSession; didReceiveData: NSData; fromPeer: MCPeerID); overload; cdecl;
    procedure session(session: MCSession; didReceiveStream: NSInputStream; withName: NSString; fromPeer: MCPeerID); overload; cdecl;
  end;

  MCNearbyServiceAdvertiserClass = interface(NSObjectClass)
    ['{CCB2A9F6-2B56-4C2E-B1ED-77CB4C7A70C6}']
  end;

  MCNearbyServiceAdvertiser = interface(NSObject)
    ['{5737C60F-C266-4C3C-AE7F-B0DED14A97AA}']
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
    ['{FC55B646-3462-4F72-A38D-4949CBBBB271}']
    procedure advertiser(advertiser: MCNearbyServiceAdvertiser; didReceiveInvitationFromPeer: MCPeerID; withContext: NSData;
      invitationHandler: Pointer); overload; cdecl;
    procedure advertiser(advertiser: MCNearbyServiceAdvertiser; didNotStartAdvertisingPeer: NSError); overload; cdecl;
  end;

  MCNearbyServiceBrowserClass = interface(NSObjectClass)
    ['{3A74D26E-17DF-431D-BFF8-D9EDA0F80D14}']
  end;

  MCNearbyServiceBrowser = interface(NSObject)
    ['{3ECB5CD4-A143-4B5B-B984-1591F5647DE5}']
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
    ['{1E669826-CFEA-4E83-B6B4-B937ECDF2ADB}']
    procedure browser(browser: MCNearbyServiceBrowser; didNotStartBrowsingForPeers: NSError); overload; cdecl;
    procedure browser(browser: MCNearbyServiceBrowser; lostPeer: MCPeerID); overload; cdecl;
    procedure browser(browser: MCNearbyServiceBrowser; foundPeer: MCPeerID; withDiscoveryInfo: NSDictionary); overload; cdecl;
  end;

  MCAdvertiserAssistantClass = interface(NSObjectClass)
    ['{3B0B5366-5975-4DC9-9C9D-A6374804E2B6}']
  end;

  MCAdvertiserAssistant = interface(NSObject)
    ['{8721C252-1E1D-4C1A-90B2-7AB0CF013CBC}']
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
    ['{BD686AE3-6395-4BE1-B3B6-39FF4D8B414E}']
    procedure advertiserAssistantDidDismissInvitation(advertiserAssistant: MCAdvertiserAssistant); cdecl;
    procedure advertiserAssistantWillPresentInvitation(advertiserAssistant: MCAdvertiserAssistant); cdecl;
  end;

  MCBrowserViewControllerClass = interface(NSViewControllerClass)
    ['{1D7263DE-A7D5-4E40-B966-0CB6A708312B}']
  end;

  MCBrowserViewController = interface(NSViewController)
    ['{8B204A5B-1BCA-4ABB-9F98-1FA016AB9AA8}']
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
    ['{7BD7AEFB-28B2-418F-A737-2E36F5CC59DB}']
    function browserViewController(browserViewController: MCBrowserViewController; shouldPresentNearbyPeer: MCPeerID; withDiscoveryInfo: NSDictionary): Boolean; cdecl;
    procedure browserViewControllerDidFinish(browserViewController: MCBrowserViewController); cdecl;
    procedure browserViewControllerWasCancelled(browserViewController: MCBrowserViewController); cdecl;
  end;

function MCErrorDomain: NSString;
// Exported const kMCSessionMinimumNumberOfPeers has an unsupported type: const NSUInteger
// Exported const kMCSessionMaximumNumberOfPeers has an unsupported type: const NSUInteger

const
  libMultipeerConnectivity = '/System/Library/Frameworks/MultipeerConnectivity.framework/MultipeerConnectivity';

implementation

uses
  System.SysUtils;

var
  MultipeerConnectivityModule: THandle;

function MCErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libMultipeerConnectivity, 'MCErrorDomain');
end;

initialization
  MultipeerConnectivityModule := LoadLibrary(libMultipeerConnectivity);

finalization
  if MultipeerConnectivityModule <> 0 then
    FreeLibrary(MultipeerConnectivityModule);

end.