unit DW.iOSapi.NetworkExtension;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // Posix
  Posix.SysSocket,
  // DW
  DW.iOSapi.Network;

const
  UINT64_MAX = 18446744073709551615;
  NEFilterFlowBytesMax = UINT64_MAX;
  NEAppProxyFlowErrorNotConnected = 1;
  NEAppProxyFlowErrorPeerReset = 2;
  NEAppProxyFlowErrorHostUnreachable = 3;
  NEAppProxyFlowErrorInvalidArgument = 4;
  NEAppProxyFlowErrorAborted = 5;
  NEAppProxyFlowErrorRefused = 6;
  NEAppProxyFlowErrorTimedOut = 7;
  NEAppProxyFlowErrorInternal = 8;
  NEAppProxyFlowErrorDatagramTooLarge = 9;
  NEAppProxyFlowErrorReadAlreadyPending = 10;
  NEProviderStopReasonNone = 0;
  NEProviderStopReasonUserInitiated = 1;
  NEProviderStopReasonProviderFailed = 2;
  NEProviderStopReasonNoNetworkAvailable = 3;
  NEProviderStopReasonUnrecoverableNetworkChange = 4;
  NEProviderStopReasonProviderDisabled = 5;
  NEProviderStopReasonAuthenticationCanceled = 6;
  NEProviderStopReasonConfigurationFailed = 7;
  NEProviderStopReasonIdleTimeout = 8;
  NEProviderStopReasonConfigurationDisabled = 9;
  NEProviderStopReasonConfigurationRemoved = 10;
  NEProviderStopReasonSuperceded = 11;
  NEProviderStopReasonUserLogout = 12;
  NEProviderStopReasonUserSwitch = 13;
  NEProviderStopReasonConnectionFailed = 14;
  NEProviderStopReasonSleep = 15;
  NEProviderStopReasonAppUpdate = 16;
  NETunnelProviderErrorNetworkSettingsInvalid = 1;
  NETunnelProviderErrorNetworkSettingsCanceled = 2;
  NETunnelProviderErrorNetworkSettingsFailed = 3;
  NETunnelProviderRoutingMethodDestinationIP = 1;
  NETunnelProviderRoutingMethodSourceApplication = 2;
  NETunnelProviderRoutingMethodNetworkRule = 3;
  NEVPNErrorConfigurationInvalid = 1;
  NEVPNErrorConfigurationDisabled = 2;
  NEVPNErrorConnectionFailed = 3;
  NEVPNErrorConfigurationStale = 4;
  NEVPNErrorConfigurationReadWriteFailed = 5;
  NEVPNErrorConfigurationUnknown = 6;
  NEDNSProxyManagerErrorConfigurationInvalid = 1;
  NEDNSProxyManagerErrorConfigurationDisabled = 2;
  NEDNSProxyManagerErrorConfigurationStale = 3;
  NEDNSProxyManagerErrorConfigurationCannotBeRemoved = 4;
  NEDNSProtocolCleartext = 1;
  NEDNSProtocolTLS = 2;
  NEDNSProtocolHTTPS = 3;
  NEDNSSettingsManagerErrorConfigurationInvalid = 1;
  NEDNSSettingsManagerErrorConfigurationDisabled = 2;
  NEDNSSettingsManagerErrorConfigurationStale = 3;
  NEDNSSettingsManagerErrorConfigurationCannotBeRemoved = 4;
  NENetworkRuleProtocolAny = 0;
  NENetworkRuleProtocolTCP = 1;
  NENetworkRuleProtocolUDP = 2;
  NETrafficDirectionAny = 0;
  NETrafficDirectionInbound = 1;
  NETrafficDirectionOutbound = 2;
  NEFilterReportFrequencyNone = 0;
  NEFilterReportFrequencyLow = 1;
  NEFilterReportFrequencyMedium = 2;
  NEFilterReportFrequencyHigh = 3;
  NEFilterActionInvalid = 0;
  NEFilterActionAllow = 1;
  NEFilterActionDrop = 2;
  NEFilterActionRemediate = 3;
  NEFilterActionFilterData = 4;
  NEFilterReportEventNewFlow = 1;
  NEFilterReportEventDataDecision = 2;
  NEFilterReportEventFlowClosed = 3;
  NEFilterReportEventStatistics = 4;
  NEFilterDataAttributeHasIPHeader = 1;
  NEFilterManagerErrorConfigurationInvalid = 1;
  NEFilterManagerErrorConfigurationDisabled = 2;
  NEFilterManagerErrorConfigurationStale = 3;
  NEFilterManagerErrorConfigurationCannotBeRemoved = 4;
  NEFilterManagerErrorConfigurationPermissionDenied = 5;
  NEFilterManagerErrorConfigurationInternalError = 6;
  NEFilterManagerGradeFirewall = 1;
  NEFilterManagerGradeInspector = 2;
  NEFilterPacketProviderVerdictAllow = 0;
  NEFilterPacketProviderVerdictDrop = 1;
  NEFilterPacketProviderVerdictDelay = 2;
  kNEHotspotHelperCommandTypeNone = 0;
  kNEHotspotHelperCommandTypeFilterScanList = 1;
  kNEHotspotHelperCommandTypeEvaluate = 2;
  kNEHotspotHelperCommandTypeAuthenticate = 3;
  kNEHotspotHelperCommandTypePresentUI = 4;
  kNEHotspotHelperCommandTypeMaintain = 5;
  kNEHotspotHelperCommandTypeLogoff = 6;
  kNEHotspotHelperResultSuccess = 0;
  kNEHotspotHelperResultFailure = 1;
  kNEHotspotHelperResultUIRequired = 2;
  kNEHotspotHelperResultCommandNotRecognized = 3;
  kNEHotspotHelperResultAuthenticationRequired = 4;
  kNEHotspotHelperResultUnsupportedNetwork = 5;
  kNEHotspotHelperResultTemporaryFailure = 6;
  kNEHotspotHelperConfidenceNone = 0;
  kNEHotspotHelperConfidenceLow = 1;
  kNEHotspotHelperConfidenceHigh = 2;
  NEHotspotConfigurationEAPTypeEAPTLS = 13;
  NEHotspotConfigurationEAPTypeEAPTTLS = 21;
  NEHotspotConfigurationEAPTypeEAPPEAP = 25;
  NEHotspotConfigurationEAPTypeEAPFAST = 43;
  NEHotspotConfigurationEAPTTLSInnerAuthenticationPAP = 0;
  NEHotspotConfigurationEAPTTLSInnerAuthenticationCHAP = 1;
  NEHotspotConfigurationEAPTTLSInnerAuthenticationMSCHAP = 2;
  NEHotspotConfigurationEAPTTLSInnerAuthenticationMSCHAPv2 = 3;
  NEHotspotConfigurationEAPTTLSInnerAuthenticationEAP = 4;
  NEHotspotConfigurationEAPTLSVersion_1_0 = 0;
  NEHotspotConfigurationEAPTLSVersion_1_1 = 1;
  NEHotspotConfigurationEAPTLSVersion_1_2 = 2;
  NEHotspotConfigurationErrorInvalid = 0;
  NEHotspotConfigurationErrorInvalidSSID = 1;
  NEHotspotConfigurationErrorInvalidWPAPassphrase = 2;
  NEHotspotConfigurationErrorInvalidWEPPassphrase = 3;
  NEHotspotConfigurationErrorInvalidEAPSettings = 4;
  NEHotspotConfigurationErrorInvalidHS20Settings = 5;
  NEHotspotConfigurationErrorInvalidHS20DomainName = 6;
  NEHotspotConfigurationErrorUserDenied = 7;
  NEHotspotConfigurationErrorInternal = 8;
  NEHotspotConfigurationErrorPending = 9;
  NEHotspotConfigurationErrorSystemConfiguration = 10;
  NEHotspotConfigurationErrorUnknown = 11;
  NEHotspotConfigurationErrorJoinOnceNotSupported = 12;
  NEHotspotConfigurationErrorAlreadyAssociated = 13;
  NEHotspotConfigurationErrorApplicationIsNotInForeground = 14;
  NEHotspotConfigurationErrorInvalidSSIDPrefix = 15;
  NEOnDemandRuleActionConnect = 1;
  NEOnDemandRuleActionDisconnect = 2;
  NEOnDemandRuleActionEvaluateConnection = 3;
  NEOnDemandRuleActionIgnore = 4;
  NEOnDemandRuleInterfaceTypeAny = 0;
  NEOnDemandRuleInterfaceTypeEthernet = 1;
  NEOnDemandRuleInterfaceTypeWiFi = 2;
  NEOnDemandRuleInterfaceTypeCellular = 3;
  NEEvaluateConnectionRuleActionConnectIfNeeded = 1;
  NEEvaluateConnectionRuleActionNeverConnect = 2;
  NEVPNStatusInvalid = 0;
  NEVPNStatusDisconnected = 1;
  NEVPNStatusConnecting = 2;
  NEVPNStatusConnected = 3;
  NEVPNStatusReasserting = 4;
  NEVPNStatusDisconnecting = 5;
  NEVPNIKEAuthenticationMethodNone = 0;
  NEVPNIKEAuthenticationMethodCertificate = 1;
  NEVPNIKEAuthenticationMethodSharedSecret = 2;
  NEVPNIKEv2EncryptionAlgorithmDES = 1;
  NEVPNIKEv2EncryptionAlgorithm3DES = 2;
  NEVPNIKEv2EncryptionAlgorithmAES128 = 3;
  NEVPNIKEv2EncryptionAlgorithmAES256 = 4;
  NEVPNIKEv2EncryptionAlgorithmAES128GCM = 5;
  NEVPNIKEv2EncryptionAlgorithmAES256GCM = 6;
  NEVPNIKEv2EncryptionAlgorithmChaCha20Poly1305 = 7;
  NEVPNIKEv2IntegrityAlgorithmSHA96 = 1;
  NEVPNIKEv2IntegrityAlgorithmSHA160 = 2;
  NEVPNIKEv2IntegrityAlgorithmSHA256 = 3;
  NEVPNIKEv2IntegrityAlgorithmSHA384 = 4;
  NEVPNIKEv2IntegrityAlgorithmSHA512 = 5;
  NEVPNIKEv2DeadPeerDetectionRateNone = 0;
  NEVPNIKEv2DeadPeerDetectionRateLow = 1;
  NEVPNIKEv2DeadPeerDetectionRateMedium = 2;
  NEVPNIKEv2DeadPeerDetectionRateHigh = 3;
  NEVPNIKEv2DiffieHellmanGroupInvalid = 0;
  NEVPNIKEv2DiffieHellmanGroup1 = 1;
  NEVPNIKEv2DiffieHellmanGroup2 = 2;
  NEVPNIKEv2DiffieHellmanGroup5 = 5;
  NEVPNIKEv2DiffieHellmanGroup14 = 14;
  NEVPNIKEv2DiffieHellmanGroup15 = 15;
  NEVPNIKEv2DiffieHellmanGroup16 = 16;
  NEVPNIKEv2DiffieHellmanGroup17 = 17;
  NEVPNIKEv2DiffieHellmanGroup18 = 18;
  NEVPNIKEv2DiffieHellmanGroup19 = 19;
  NEVPNIKEv2DiffieHellmanGroup20 = 20;
  NEVPNIKEv2DiffieHellmanGroup21 = 21;
  NEVPNIKEv2DiffieHellmanGroup31 = 31;
  NEVPNIKEv2CertificateTypeRSA = 1;
  NEVPNIKEv2CertificateTypeECDSA256 = 2;
  NEVPNIKEv2CertificateTypeECDSA384 = 3;
  NEVPNIKEv2CertificateTypeECDSA521 = 4;
  NEVPNIKEv2CertificateTypeEd25519 = 5;
  NEVPNIKEv2TLSVersionDefault = 0;
  NEVPNIKEv2TLSVersion1_0 = 1;
  NEVPNIKEv2TLSVersion1_1 = 2;
  NEVPNIKEv2TLSVersion1_2 = 3;
  NEAppPushManagerErrorConfigurationInvalid = 1;
  NEAppPushManagerErrorConfigurationNotLoaded = 2;
  NEAppPushManagerErrorInternalError = 3;
  NEAppPushManagerErrorInactiveSession = 4;
  NWPathStatusInvalid = 0;
  NWPathStatusSatisfied = 1;
  NWPathStatusUnsatisfied = 2;
  NWPathStatusSatisfiable = 3;
  NWTCPConnectionStateInvalid = 0;
  NWTCPConnectionStateConnecting = 1;
  NWTCPConnectionStateWaiting = 2;
  NWTCPConnectionStateConnected = 3;
  NWTCPConnectionStateDisconnected = 4;
  NWTCPConnectionStateCancelled = 5;
  NWUDPSessionStateInvalid = 0;
  NWUDPSessionStateWaiting = 1;
  NWUDPSessionStatePreparing = 2;
  NWUDPSessionStateReady = 3;
  NWUDPSessionStateFailed = 4;
  NWUDPSessionStateCancelled = 5;

type
  NEAppProxyFlow = interface;
  NEProvider = interface;
  NETunnelProvider = interface;
  NEAppProxyProvider = interface;
  NEVPNManager = interface;
  NETunnelProviderManager = interface;
  NEAppProxyProviderManager = interface;
  NEAppProxyTCPFlow = interface;
  NEAppProxyUDPFlow = interface;
  NEAppRule = interface;
  NEDNSProxyManager = interface;
  NEDNSProxyProvider = interface;
  NEProxyServer = interface;
  NEProxySettings = interface;
  NEVPNProtocol = interface;
  NEDNSProxyProviderProtocol = interface;
  NEDNSSettings = interface;
  NEDNSOverTLSSettings = interface;
  NEDNSOverHTTPSSettings = interface;
  NEDNSSettingsManager = interface;
  NENetworkRule = interface;
  NEFilterFlow = interface;
  NEFilterBrowserFlow = interface;
  NEFilterSocketFlow = interface;
  NEFilterProvider = interface;
  NEFilterVerdict = interface;
  NEFilterNewFlowVerdict = interface;
  NEFilterControlVerdict = interface;
  NEFilterReport = interface;
  NEFilterControlProvider = interface;
  NEFilterDataProvider = interface;
  NEFilterDataVerdict = interface;
  NEFilterRemediationVerdict = interface;
  NEFilterManager = interface;
  NEFilterPacketContext = interface;
  NEFilterPacketProvider = interface;
  NEFilterProviderConfiguration = interface;
  NEFilterRule = interface;
  NEFilterSettings = interface;
  NEFlowMetaData = interface;
  NEHotspotNetwork = interface;
  NEHotspotHelperCommand = interface;
  NEHotspotHelperResponse = interface;
  NEHotspotHelper = interface;
  NEHotspotHS20Settings = interface;
  NEHotspotEAPSettings = interface;
  NEHotspotConfiguration = interface;
  NEHotspotConfigurationManager = interface;
  NEIPv4Settings = interface;
  NEIPv4Route = interface;
  NEIPv6Settings = interface;
  NEIPv6Route = interface;
  NEOnDemandRule = interface;
  NEOnDemandRuleConnect = interface;
  NEOnDemandRuleDisconnect = interface;
  NEOnDemandRuleIgnore = interface;
  NEOnDemandRuleEvaluateConnection = interface;
  NEEvaluateConnectionRule = interface;
  NEPacket = interface;
  NEPacketTunnelFlow = interface;
  NETunnelNetworkSettings = interface;
  NEPacketTunnelNetworkSettings = interface;
  NEPacketTunnelProvider = interface;
  NETransparentProxyManager = interface;
  NETransparentProxyNetworkSettings = interface;
  NETransparentProxyProvider = interface;
  NEVPNConnection = interface;
  NETunnelProviderSession = interface;
  NETunnelProviderProtocol = interface;
  NEVPNProtocolIPSec = interface;
  NEVPNIKEv2SecurityAssociationParameters = interface;
  NEVPNProtocolIKEv2 = interface;
  NEAppPushManager = interface;
  NEAppPushDelegate = interface;
  NEAppPushProvider = interface;
  NWEndpoint = interface;
  NWHostEndpoint = interface;
  NWBonjourServiceEndpoint = interface;
  NWPath = interface;
  NWTCPConnection = interface;
  NWTCPConnectionAuthenticationDelegate = interface;
  NWUDPSession = interface;
  NWTLSParameters = interface;

  NEAppProxyFlowError = NSInteger;
  NEProviderStopReason = NSInteger;
  NETunnelProviderError = NSInteger;
  NETunnelProviderRoutingMethod = NSInteger;
  NEVPNError = NSInteger;
  NEDNSProxyManagerError = NSInteger;
  NEDNSProtocol = NSInteger;
  NEDNSSettingsManagerError = NSInteger;
  NENetworkRuleProtocol = NSInteger;
  NETrafficDirection = NSInteger;
  NEFilterReportFrequency = NSInteger;
  NEFilterAction = NSInteger;
  NEFilterReportEvent = NSInteger;
  NEFilterDataAttribute = NSInteger;
  NEFilterManagerError = NSInteger;
  NEFilterManagerGrade = NSInteger;
  NEFilterPacketProviderVerdict = NSInteger;
  NSErrorDomain = NSString;

  NEFilterPacketHandler = function(context: NEFilterPacketContext; &interface: nw_interface_t; direction: NETrafficDirection; packetBytes: Pointer;
    packetLength: NativeUInt): NEFilterPacketProviderVerdict of object;
  NEHotspotHelperCommandType = NSInteger;
  NEHotspotHelperResult = NSInteger;
  NEHotspotHelperConfidence = NSInteger;

  NEHotspotHelperHandler = procedure(cmd: NEHotspotHelperCommand) of object;
  NEHotspotConfigurationEAPType = NSInteger;
  NEHotspotConfigurationTTLSInnerAuthenticationType = NSInteger;
  NEHotspotConfigurationEAPTLSVersion = NSInteger;
  NEHotspotConfigurationError = NSInteger;
  NEOnDemandRuleAction = NSInteger;
  NEOnDemandRuleInterfaceType = NSInteger;
  NEEvaluateConnectionRuleAction = NSInteger;
  NEVPNStatus = NSInteger;
  NEVPNIKEAuthenticationMethod = NSInteger;
  NEVPNIKEv2EncryptionAlgorithm = NSInteger;
  NEVPNIKEv2IntegrityAlgorithm = NSInteger;
  NEVPNIKEv2DeadPeerDetectionRate = NSInteger;
  NEVPNIKEv2DiffieHellmanGroup = NSInteger;
  NEVPNIKEv2CertificateType = NSInteger;
  NEVPNIKEv2TLSVersion = NSInteger;
  NEAppPushManagerError = NSInteger;
  NWPathStatus = NSInteger;
  NWTCPConnectionState = NSInteger;
  NWUDPSessionState = NSInteger;
  TNEAppProxyFlowBlockMethod1 = procedure(error: NSError) of object;
  TNEProviderBlockMethod1 = procedure of object;
  TNEProviderBlockMethod2 = procedure(success: Boolean) of object;
  TNETunnelProviderBlockMethod1 = procedure(responseData: NSData) of object;
  TNETunnelProviderBlockMethod2 = procedure(error: NSError) of object;
  TNEAppProxyProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEAppProxyProviderBlockMethod2 = procedure of object;
  TNEVPNManagerBlockMethod1 = procedure(error: NSError) of object;
  TNETunnelProviderManagerBlockMethod1 = procedure(managers: NSArray; error: NSError) of object;
  TNEAppProxyProviderManagerBlockMethod1 = procedure(managers: NSArray; error: NSError) of object;
  TNEAppProxyTCPFlowBlockMethod1 = procedure(data: NSData; error: NSError) of object;
  TNEAppProxyTCPFlowBlockMethod2 = procedure(error: NSError) of object;
  TNEAppProxyUDPFlowBlockMethod1 = procedure(datagrams: NSArray; remoteEndpoints: NSArray; error: NSError) of object;
  TNEAppProxyUDPFlowBlockMethod2 = procedure(error: NSError) of object;
  TNEDNSProxyManagerBlockMethod1 = procedure(error: NSError) of object;
  TNEDNSProxyProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEDNSProxyProviderBlockMethod2 = procedure of object;
  TNEDNSSettingsManagerBlockMethod1 = procedure(error: NSError) of object;
  TNEFilterProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEFilterProviderBlockMethod2 = procedure of object;
  TNEFilterControlProviderBlockMethod1 = procedure(param1: NEFilterControlVerdict) of object;
  TNEFilterDataProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEFilterManagerBlockMethod1 = procedure(error: NSError) of object;
  TNEHotspotNetworkBlockMethod1 = procedure(currentNetwork: NEHotspotNetwork) of object;
  TNEHotspotConfigurationManagerBlockMethod1 = procedure(error: NSError) of object;
  TNEHotspotConfigurationManagerBlockMethod2 = procedure(param1: NSArray) of object;
  TNEPacketTunnelFlowBlockMethod1 = procedure(packets: NSArray; protocols: NSArray) of object;
  TNEPacketTunnelFlowBlockMethod2 = procedure(packets: NSArray) of object;
  TNEPacketTunnelProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEPacketTunnelProviderBlockMethod2 = procedure of object;
  TNETransparentProxyManagerBlockMethod1 = procedure(managers: NSArray; error: NSError) of object;
  TNETunnelProviderSessionBlockMethod1 = procedure(responseData: NSData) of object;
  TNEAppPushManagerBlockMethod1 = procedure(managers: NSArray; error: NSError) of object;
  TNEAppPushManagerBlockMethod2 = procedure(error: NSError) of object;
  TNEAppPushProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEAppPushProviderBlockMethod2 = procedure of object;
  TNWTCPConnectionBlockMethod1 = procedure(data: NSData; error: NSError) of object;
  TNWTCPConnectionBlockMethod2 = procedure(error: NSError) of object;
  TNWTCPConnectionAuthenticationDelegateBlockMethod1 = procedure(identity: SecIdentityRef; certificateChain: NSArray) of object;
  TNWTCPConnectionAuthenticationDelegateBlockMethod2 = procedure(trust: SecTrustRef) of object;
  TNWUDPSessionBlockMethod1 = procedure(datagrams: NSArray; error: NSError) of object;
  TNWUDPSessionBlockMethod2 = procedure(error: NSError) of object;

  NEAppProxyFlowClass = interface(NSObjectClass)
    ['{D60EDEC6-E773-411B-B7D0-4A5B33733D46}']
  end;

  NEAppProxyFlow = interface(NSObject)
    ['{7ADC2C9C-BEE8-497F-8001-A40C7C49BC95}']
    procedure closeReadWithError(error: NSError); cdecl;
    procedure closeWriteWithError(error: NSError); cdecl;
    function isBound: Boolean; cdecl;
    function metaData: NEFlowMetaData; cdecl;
    function networkInterface: nw_interface_t; cdecl;
    procedure openWithLocalEndpoint(localEndpoint: NWHostEndpoint; completionHandler: TNEAppProxyFlowBlockMethod1); cdecl;
    function remoteHostname: NSString; cdecl;
    procedure setMetadata(parameters: nw_parameters_t); cdecl;
    procedure setNetworkInterface(networkInterface: nw_interface_t); cdecl;
  end;
  TNEAppProxyFlow = class(TOCGenericImport<NEAppProxyFlowClass, NEAppProxyFlow>) end;

  NEProviderClass = interface(NSObjectClass)
    ['{739202A5-3D1C-4B55-8851-36F81C0CA60B}']
    {class} procedure startSystemExtensionMode; cdecl;
  end;

  NEProvider = interface(NSObject)
    ['{FF166D99-A3C5-466C-9484-6261BBA80ABB}']
    function createTCPConnectionToEndpoint(remoteEndpoint: NWEndpoint; enableTLS: Boolean; TLSParameters: NWTLSParameters;
      delegate: Pointer): NWTCPConnection; cdecl;
    function createUDPSessionToEndpoint(remoteEndpoint: NWEndpoint; fromEndpoint: NWHostEndpoint): NWUDPSession; cdecl;
    function defaultPath: NWPath; cdecl;
    procedure displayMessage(message: NSString; completionHandler: TNEProviderBlockMethod2); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UILocalNotification", macos(10.12, 10.14), ios(10.0, 12.0))
    procedure sleepWithCompletionHandler(completionHandler: TNEProviderBlockMethod1); cdecl;
    procedure wake; cdecl;
  end;
  TNEProvider = class(TOCGenericImport<NEProviderClass, NEProvider>) end;

  NETunnelProviderClass = interface(NEProviderClass)
    ['{DF158D0B-A9C0-4F8D-87DD-AB4759065DE3}']
  end;

  NETunnelProvider = interface(NEProvider)
    ['{EECA2C05-E773-4927-B569-620737E0E660}']
    function appRules: NSArray; cdecl;
    procedure handleAppMessage(messageData: NSData; completionHandler: TNETunnelProviderBlockMethod1); cdecl;
    function protocolConfiguration: NEVPNProtocol; cdecl;
    function reasserting: Boolean; cdecl;
    function routingMethod: NETunnelProviderRoutingMethod; cdecl;
    procedure setReasserting(reasserting: Boolean); cdecl;
    procedure setTunnelNetworkSettings(tunnelNetworkSettings: NETunnelNetworkSettings; completionHandler: TNETunnelProviderBlockMethod2); cdecl;
  end;
  TNETunnelProvider = class(TOCGenericImport<NETunnelProviderClass, NETunnelProvider>) end;

  NEAppProxyProviderClass = interface(NETunnelProviderClass)
    ['{A6050B56-89A2-4419-A224-C829C58DFD32}']
  end;

  NEAppProxyProvider = interface(NETunnelProvider)
    ['{F8010CEB-6046-4EC7-96CD-D4DAEE09B6A7}']
    procedure cancelProxyWithError(error: NSError); cdecl;
    function handleNewFlow(flow: NEAppProxyFlow): Boolean; cdecl;
    function handleNewUDPFlow(flow: NEAppProxyUDPFlow; initialRemoteEndpoint: NWEndpoint): Boolean; cdecl;
    procedure startProxyWithOptions(options: NSDictionary; completionHandler: TNEAppProxyProviderBlockMethod1); cdecl;
    procedure stopProxyWithReason(reason: NEProviderStopReason; completionHandler: TNEAppProxyProviderBlockMethod2); cdecl;
  end;
  TNEAppProxyProvider = class(TOCGenericImport<NEAppProxyProviderClass, NEAppProxyProvider>) end;

  NEVPNManagerClass = interface(NSObjectClass)
    ['{61FD44A1-8D38-4C51-9D93-301278AD46E0}']
    {class} function sharedManager: NEVPNManager; cdecl;
  end;

  NEVPNManager = interface(NSObject)
    ['{876F5532-BCEB-4615-96D1-736CCBA44CCA}']
    function connection: NEVPNConnection; cdecl;
    function isEnabled: Boolean; cdecl;
    function isOnDemandEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function onDemandRules: NSArray; cdecl;
    function protocol: NEVPNProtocol; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use protocolConfiguration instead", macos(10.11, 10.11), ios(8.0, 9.0))
    function protocolConfiguration: NEVPNProtocol; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setOnDemandEnabled(onDemandEnabled: Boolean); cdecl;
    procedure setOnDemandRules(onDemandRules: NSArray); cdecl;
    procedure setProtocol(protocol: NEVPNProtocol); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use protocolConfiguration instead", macos(10.11, 10.11), ios(8.0, 9.0))
    procedure setProtocolConfiguration(protocolConfiguration: NEVPNProtocol); cdecl;
  end;
  TNEVPNManager = class(TOCGenericImport<NEVPNManagerClass, NEVPNManager>) end;

  NETunnelProviderManagerClass = interface(NEVPNManagerClass)
    ['{C25C7984-D740-4E11-85DC-757F48C5BE62}']
    {class} function forPerAppVPN: Pointer; cdecl;
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNETunnelProviderManagerBlockMethod1); cdecl;
  end;

  NETunnelProviderManager = interface(NEVPNManager)
    ['{A5EBB89F-AA71-4361-B4B7-5675A1BBB2E4}']
    function appRules: NSArray; cdecl;
    function associatedDomains: NSArray; cdecl;
    function calendarDomains: NSArray; cdecl;
    function contactsDomains: NSArray; cdecl;
    function copyAppRules: NSArray; cdecl;
    function excludedDomains: NSArray; cdecl;
    function mailDomains: NSArray; cdecl;
    function routingMethod: NETunnelProviderRoutingMethod; cdecl;
    function safariDomains: NSArray; cdecl;
    procedure setAppRules(appRules: NSArray); cdecl;
    procedure setAssociatedDomains(associatedDomains: NSArray); cdecl;
    procedure setCalendarDomains(calendarDomains: NSArray); cdecl;
    procedure setContactsDomains(contactsDomains: NSArray); cdecl;
    procedure setExcludedDomains(excludedDomains: NSArray); cdecl;
    procedure setMailDomains(mailDomains: NSArray); cdecl;
    procedure setSafariDomains(safariDomains: NSArray); cdecl;
  end;
  TNETunnelProviderManager = class(TOCGenericImport<NETunnelProviderManagerClass, NETunnelProviderManager>) end;

  NEAppProxyProviderManagerClass = interface(NETunnelProviderManagerClass)
    ['{FA08C570-D55E-465B-A32B-CC3A442ADA72}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNEAppProxyProviderManagerBlockMethod1); cdecl;
  end;

  NEAppProxyProviderManager = interface(NETunnelProviderManager)
    ['{AFD3A54D-E26C-4584-897B-240C41AAE412}']
  end;
  TNEAppProxyProviderManager = class(TOCGenericImport<NEAppProxyProviderManagerClass, NEAppProxyProviderManager>) end;

  NEAppProxyTCPFlowClass = interface(NEAppProxyFlowClass)
    ['{86897762-0D07-44E1-A478-58AC180603C8}']
  end;

  NEAppProxyTCPFlow = interface(NEAppProxyFlow)
    ['{C424DD2A-F002-4CBB-8F77-8FACA7E686B7}']
    procedure readDataWithCompletionHandler(completionHandler: TNEAppProxyTCPFlowBlockMethod1); cdecl;
    function remoteEndpoint: NWEndpoint; cdecl;
    procedure writeData(data: NSData; withCompletionHandler: TNEAppProxyTCPFlowBlockMethod2); cdecl;
  end;
  TNEAppProxyTCPFlow = class(TOCGenericImport<NEAppProxyTCPFlowClass, NEAppProxyTCPFlow>) end;

  NEAppProxyUDPFlowClass = interface(NEAppProxyFlowClass)
    ['{8C9EC00F-8438-464A-B968-F58CF7CB35FA}']
  end;

  NEAppProxyUDPFlow = interface(NEAppProxyFlow)
    ['{260532E4-CEE0-41FF-BA51-AF17F93FFE97}']
    function localEndpoint: NWEndpoint; cdecl;
    procedure readDatagramsWithCompletionHandler(completionHandler: TNEAppProxyUDPFlowBlockMethod1); cdecl;
    procedure writeDatagrams(datagrams: NSArray; sentByEndpoints: NSArray; completionHandler: TNEAppProxyUDPFlowBlockMethod2); cdecl;
  end;
  TNEAppProxyUDPFlow = class(TOCGenericImport<NEAppProxyUDPFlowClass, NEAppProxyUDPFlow>) end;

  NEAppRuleClass = interface(NSObjectClass)
    ['{DABFE418-2BD3-4E08-93FD-2C47BF83DA52}']
  end;

  NEAppRule = interface(NSObject)
    ['{D6D36F1C-D2AC-4DBA-ACBA-838030388753}']
    function initWithSigningIdentifier(signingIdentifier: NSString; designatedRequirement: NSString): Pointer; overload; cdecl;
    function initWithSigningIdentifier(signingIdentifier: NSString): Pointer; overload; cdecl;
    function matchDesignatedRequirement: NSString; cdecl;
    function matchDomains: NSArray; cdecl;
    function matchPath: NSString; cdecl;
    function matchSigningIdentifier: NSString; cdecl;
    function matchTools: NSArray; cdecl;
    procedure setMatchDomains(matchDomains: NSArray); cdecl;
    procedure setMatchPath(matchPath: NSString); cdecl;
    procedure setMatchTools(matchTools: NSArray); cdecl;
  end;
  TNEAppRule = class(TOCGenericImport<NEAppRuleClass, NEAppRule>) end;

  NEDNSProxyManagerClass = interface(NSObjectClass)
    ['{331B7A31-C148-4C7A-B5C2-BE4A1B77006D}']
    {class} function sharedManager: NEDNSProxyManager; cdecl;
  end;

  NEDNSProxyManager = interface(NSObject)
    ['{CF35A32B-32FC-4F0C-AC0A-A293B30217EB}']
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEDNSProxyManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function providerProtocol: NEDNSProxyProviderProtocol; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEDNSProxyManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEDNSProxyManagerBlockMethod1); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setProviderProtocol(providerProtocol: NEDNSProxyProviderProtocol); cdecl;
  end;
  TNEDNSProxyManager = class(TOCGenericImport<NEDNSProxyManagerClass, NEDNSProxyManager>) end;

  NEDNSProxyProviderClass = interface(NEProviderClass)
    ['{3F53F886-897F-47AB-92B6-9258E0D87B9B}']
  end;

  NEDNSProxyProvider = interface(NEProvider)
    ['{E10BCB86-DB72-435D-AE18-0AFDCED6B855}']
    procedure cancelProxyWithError(error: NSError); cdecl;
    function handleNewFlow(flow: NEAppProxyFlow): Boolean; cdecl;
    function handleNewUDPFlow(flow: NEAppProxyUDPFlow; initialRemoteEndpoint: NWEndpoint): Boolean; cdecl;
    procedure startProxyWithOptions(options: NSDictionary; completionHandler: TNEDNSProxyProviderBlockMethod1); cdecl;
    procedure stopProxyWithReason(reason: NEProviderStopReason; completionHandler: TNEDNSProxyProviderBlockMethod2); cdecl;
    function systemDNSSettings: NSArray; cdecl;
  end;
  TNEDNSProxyProvider = class(TOCGenericImport<NEDNSProxyProviderClass, NEDNSProxyProvider>) end;

  NEProxyServerClass = interface(NSObjectClass)
    ['{368ECB70-DAEA-4EE4-990B-C82A4C6B3FF1}']
  end;

  NEProxyServer = interface(NSObject)
    ['{4100F823-E2E2-4F2A-8E80-73784B95BC15}']
    function address: NSString; cdecl;
    function authenticationRequired: Boolean; cdecl;
    function initWithAddress(address: NSString; port: NSInteger): Pointer; cdecl;
    function password: NSString; cdecl;
    function port: NSInteger; cdecl;
    procedure setAuthenticationRequired(authenticationRequired: Boolean); cdecl;
    procedure setPassword(password: NSString); cdecl;
    procedure setUsername(username: NSString); cdecl;
    function username: NSString; cdecl;
  end;
  TNEProxyServer = class(TOCGenericImport<NEProxyServerClass, NEProxyServer>) end;

  NEProxySettingsClass = interface(NSObjectClass)
    ['{251FD2C7-3224-4F9E-AE38-AE4E49409A9B}']
  end;

  NEProxySettings = interface(NSObject)
    ['{E61523EE-E0F8-4CEA-8419-6105B7A4D6AF}']
    function autoProxyConfigurationEnabled: Boolean; cdecl;
    function exceptionList: NSArray; cdecl;
    function excludeSimpleHostnames: Boolean; cdecl;
    function HTTPEnabled: Boolean; cdecl;
    function HTTPSEnabled: Boolean; cdecl;
    function HTTPServer: NEProxyServer; cdecl;
    function HTTPSServer: NEProxyServer; cdecl;
    function matchDomains: NSArray; cdecl;
    function proxyAutoConfigurationJavaScript: NSString; cdecl;
    function proxyAutoConfigurationURL: NSURL; cdecl;
    procedure setAutoProxyConfigurationEnabled(autoProxyConfigurationEnabled: Boolean); cdecl;
    procedure setExceptionList(exceptionList: NSArray); cdecl;
    procedure setExcludeSimpleHostnames(excludeSimpleHostnames: Boolean); cdecl;
    procedure setHTTPEnabled(HTTPEnabled: Boolean); cdecl;
    procedure setHTTPSEnabled(HTTPSEnabled: Boolean); cdecl;
    procedure setHTTPServer(HTTPServer: NEProxyServer); cdecl;
    procedure setHTTPSServer(HTTPSServer: NEProxyServer); cdecl;
    procedure setMatchDomains(matchDomains: NSArray); cdecl;
    procedure setProxyAutoConfigurationJavaScript(proxyAutoConfigurationJavaScript: NSString); cdecl;
    procedure setProxyAutoConfigurationURL(proxyAutoConfigurationURL: NSURL); cdecl;
  end;
  TNEProxySettings = class(TOCGenericImport<NEProxySettingsClass, NEProxySettings>) end;

  NEVPNProtocolClass = interface(NSObjectClass)
    ['{BFA47DC9-BDA1-4457-90D1-EF256D65B9AB}']
  end;

  NEVPNProtocol = interface(NSObject)
    ['{E3546296-AE9F-427C-908B-BCE38F132423}']
    function disconnectOnSleep: Boolean; cdecl;
    function enforceRoutes: Boolean; cdecl;
    function excludeLocalNetworks: Boolean; cdecl;
    function identityData: NSData; cdecl;
    function identityDataPassword: NSString; cdecl;
    function identityReference: NSData; cdecl;
    function includeAllNetworks: Boolean; cdecl;
    function passwordReference: NSData; cdecl;
    function proxySettings: NEProxySettings; cdecl;
    function serverAddress: NSString; cdecl;
    procedure setDisconnectOnSleep(disconnectOnSleep: Boolean); cdecl;
    procedure setEnforceRoutes(enforceRoutes: Boolean); cdecl;
    procedure setExcludeLocalNetworks(excludeLocalNetworks: Boolean); cdecl;
    procedure setIdentityData(identityData: NSData); cdecl;
    procedure setIdentityDataPassword(identityDataPassword: NSString); cdecl;
    procedure setIdentityReference(identityReference: NSData); cdecl;
    procedure setIncludeAllNetworks(includeAllNetworks: Boolean); cdecl;
    procedure setPasswordReference(passwordReference: NSData); cdecl;
    procedure setProxySettings(proxySettings: NEProxySettings); cdecl;
    procedure setServerAddress(serverAddress: NSString); cdecl;
    procedure setUsername(username: NSString); cdecl;
    function username: NSString; cdecl;
  end;
  TNEVPNProtocol = class(TOCGenericImport<NEVPNProtocolClass, NEVPNProtocol>) end;

  NEDNSProxyProviderProtocolClass = interface(NEVPNProtocolClass)
    ['{00D44D0A-793D-4B93-9D33-261E75BA7AFC}']
  end;

  NEDNSProxyProviderProtocol = interface(NEVPNProtocol)
    ['{67135D54-4842-4124-A52D-2D1182563EA4}']
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNEDNSProxyProviderProtocol = class(TOCGenericImport<NEDNSProxyProviderProtocolClass, NEDNSProxyProviderProtocol>) end;

  NEDNSSettingsClass = interface(NSObjectClass)
    ['{56D103D0-9E1F-4EE9-AB46-C735EF704323}']
  end;

  NEDNSSettings = interface(NSObject)
    ['{2A905F86-11B1-4A3B-B7B7-1AD9F37EE5A5}']
    function dnsProtocol: NEDNSProtocol; cdecl;
    function domainName: NSString; cdecl;
    function initWithServers(servers: NSArray): Pointer; cdecl;
    function matchDomains: NSArray; cdecl;
    function matchDomainsNoSearch: Boolean; cdecl;
    function searchDomains: NSArray; cdecl;
    function servers: NSArray; cdecl;
    procedure setDomainName(domainName: NSString); cdecl;
    procedure setMatchDomains(matchDomains: NSArray); cdecl;
    procedure setMatchDomainsNoSearch(matchDomainsNoSearch: Boolean); cdecl;
    procedure setSearchDomains(searchDomains: NSArray); cdecl;
  end;
  TNEDNSSettings = class(TOCGenericImport<NEDNSSettingsClass, NEDNSSettings>) end;

  NEDNSOverTLSSettingsClass = interface(NEDNSSettingsClass)
    ['{C21A6950-8FE8-487E-AA4C-0FDDA4FD7101}']
  end;

  NEDNSOverTLSSettings = interface(NEDNSSettings)
    ['{808F55BB-1CC6-49D7-8C02-832574D800CD}']
    function serverName: NSString; cdecl;
    procedure setServerName(serverName: NSString); cdecl;
  end;
  TNEDNSOverTLSSettings = class(TOCGenericImport<NEDNSOverTLSSettingsClass, NEDNSOverTLSSettings>) end;

  NEDNSOverHTTPSSettingsClass = interface(NEDNSSettingsClass)
    ['{9D642CC2-7215-40A5-8F26-C64F2D94CA68}']
  end;

  NEDNSOverHTTPSSettings = interface(NEDNSSettings)
    ['{CCDA7A09-1048-48EB-B1CE-C82B89E8E5C5}']
    function serverURL: NSURL; cdecl;
    procedure setServerURL(serverURL: NSURL); cdecl;
  end;
  TNEDNSOverHTTPSSettings = class(TOCGenericImport<NEDNSOverHTTPSSettingsClass, NEDNSOverHTTPSSettings>) end;

  NEDNSSettingsManagerClass = interface(NSObjectClass)
    ['{C53C62FE-7A0C-4748-B85E-8370C6A666D8}']
    {class} function sharedManager: NEDNSSettingsManager; cdecl;
  end;

  NEDNSSettingsManager = interface(NSObject)
    ['{0FB878D6-A5FD-4E01-B3F7-4FA1E3395566}']
    function dnsSettings: NEDNSSettings; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEDNSSettingsManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function onDemandRules: NSArray; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEDNSSettingsManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEDNSSettingsManagerBlockMethod1); cdecl;
    procedure setDnsSettings(dnsSettings: NEDNSSettings); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setOnDemandRules(onDemandRules: NSArray); cdecl;
  end;
  TNEDNSSettingsManager = class(TOCGenericImport<NEDNSSettingsManagerClass, NEDNSSettingsManager>) end;

  NENetworkRuleClass = interface(NSObjectClass)
    ['{6B5D751F-C4E6-4BD6-93C1-6B3F7F4481AD}']
  end;

  NENetworkRule = interface(NSObject)
    ['{C100346D-3065-47A7-BF55-8D07938F6929}']
    function initWithDestinationHost(hostEndpoint: NWHostEndpoint; protocol: NENetworkRuleProtocol): Pointer; cdecl;
    function initWithDestinationNetwork(networkEndpoint: NWHostEndpoint; prefix: NSUInteger; protocol: NENetworkRuleProtocol): Pointer; cdecl;
    function initWithRemoteNetwork(remoteNetwork: NWHostEndpoint; remotePrefix: NSUInteger; localNetwork: NWHostEndpoint; localPrefix: NSUInteger;
      protocol: NENetworkRuleProtocol; direction: NETrafficDirection): Pointer; cdecl;
    function matchDirection: NETrafficDirection; cdecl;
    function matchLocalNetwork: NWHostEndpoint; cdecl;
    function matchLocalPrefix: NSUInteger; cdecl;
    function matchProtocol: NENetworkRuleProtocol; cdecl;
    function matchRemoteEndpoint: NWHostEndpoint; cdecl;
    function matchRemotePrefix: NSUInteger; cdecl;
  end;
  TNENetworkRule = class(TOCGenericImport<NENetworkRuleClass, NENetworkRule>) end;

  NEFilterFlowClass = interface(NSObjectClass)
    ['{1A5B4841-29C6-4955-BD54-98CB54E3837F}']
  end;

  NEFilterFlow = interface(NSObject)
    ['{B1B04143-B900-4831-93CF-8AF20F889062}']
    function direction: NETrafficDirection; cdecl;
    function identifier: NSUUID; cdecl;
    function sourceAppAuditToken: NSData; cdecl;
    function sourceAppIdentifier: NSString; cdecl;
    function sourceAppUniqueIdentifier: NSData; cdecl;
    function sourceAppVersion: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TNEFilterFlow = class(TOCGenericImport<NEFilterFlowClass, NEFilterFlow>) end;

  NEFilterBrowserFlowClass = interface(NEFilterFlowClass)
    ['{A554F7A6-686F-4B43-B99C-2D9920229381}']
  end;

  NEFilterBrowserFlow = interface(NEFilterFlow)
    ['{A3E08565-1F87-458C-8AC3-2DA237B793F1}']
    function parentURL: NSURL; cdecl;
    function request: NSURLRequest; cdecl;
    function response: NSURLResponse; cdecl;
  end;
  TNEFilterBrowserFlow = class(TOCGenericImport<NEFilterBrowserFlowClass, NEFilterBrowserFlow>) end;

  NEFilterSocketFlowClass = interface(NEFilterFlowClass)
    ['{18FD16D7-01C0-4574-8CBA-64C2853B6C68}']
  end;

  NEFilterSocketFlow = interface(NEFilterFlow)
    ['{E3F91BAD-EC34-4CA5-8805-B361167D2905}']
    function localEndpoint: NWEndpoint; cdecl;
    function remoteEndpoint: NWEndpoint; cdecl;
    function remoteHostname: NSString; cdecl;
    function socketFamily: Integer; cdecl;
    function socketProtocol: Integer; cdecl;
    function socketType: Integer; cdecl;
  end;
  TNEFilterSocketFlow = class(TOCGenericImport<NEFilterSocketFlowClass, NEFilterSocketFlow>) end;

  NEFilterProviderClass = interface(NEProviderClass)
    ['{97189695-6620-4F7A-876A-2A067CC3BBA1}']
  end;

  NEFilterProvider = interface(NEProvider)
    ['{57F58E44-10AC-4D6D-AD73-F57C22C17D64}']
    function filterConfiguration: NEFilterProviderConfiguration; cdecl;
    procedure handleReport(report: NEFilterReport); cdecl;
    procedure startFilterWithCompletionHandler(completionHandler: TNEFilterProviderBlockMethod1); cdecl;
    procedure stopFilterWithReason(reason: NEProviderStopReason; completionHandler: TNEFilterProviderBlockMethod2); cdecl;
  end;
  TNEFilterProvider = class(TOCGenericImport<NEFilterProviderClass, NEFilterProvider>) end;

  NEFilterVerdictClass = interface(NSObjectClass)
    ['{4529231A-027C-4D34-9E4A-E5C9E05C4315}']
  end;

  NEFilterVerdict = interface(NSObject)
    ['{FF5D374F-8AC2-4DE2-A34A-3BF3C26C01E0}']
    procedure setShouldReport(shouldReport: Boolean); cdecl;
    function shouldReport: Boolean; cdecl;
  end;
  TNEFilterVerdict = class(TOCGenericImport<NEFilterVerdictClass, NEFilterVerdict>) end;

  NEFilterNewFlowVerdictClass = interface(NEFilterVerdictClass)
    ['{D9AD6782-1654-43DF-B13A-FF3922CA7495}']
    {class} function allowVerdict: NEFilterNewFlowVerdict; cdecl;
    {class} function dropVerdict: NEFilterNewFlowVerdict; cdecl;
    {class} function filterDataVerdictWithFilterInbound(filterInbound: Boolean; peekInboundBytes: NSUInteger; filterOutbound: Boolean;
      peekOutboundBytes: NSUInteger): NEFilterNewFlowVerdict; cdecl;
    {class} function needRulesVerdict: NEFilterNewFlowVerdict; cdecl;
    {class} function pauseVerdict: NEFilterNewFlowVerdict; cdecl;
    {class} function remediateVerdictWithRemediationURLMapKey(remediationURLMapKey: NSString;
      remediationButtonTextMapKey: NSString): NEFilterNewFlowVerdict; cdecl;
    {class} function URLAppendStringVerdictWithMapKey(urlAppendMapKey: NSString): NEFilterNewFlowVerdict; cdecl;
  end;

  NEFilterNewFlowVerdict = interface(NEFilterVerdict)
    ['{705B0C5E-ADA7-42C8-A658-354C507FFCC9}']
    procedure setStatisticsReportFrequency(statisticsReportFrequency: NEFilterReportFrequency); cdecl;
    function statisticsReportFrequency: NEFilterReportFrequency; cdecl;
  end;
  TNEFilterNewFlowVerdict = class(TOCGenericImport<NEFilterNewFlowVerdictClass, NEFilterNewFlowVerdict>) end;

  NEFilterControlVerdictClass = interface(NEFilterNewFlowVerdictClass)
    ['{D09C411E-1009-4E01-929B-AE2F8ACB011C}']
    {class} function allowVerdictWithUpdateRules(updateRules: Boolean): NEFilterControlVerdict; cdecl;
    {class} function dropVerdictWithUpdateRules(updateRules: Boolean): NEFilterControlVerdict; cdecl;
    {class} function updateRules: NEFilterControlVerdict; cdecl;
  end;

  NEFilterControlVerdict = interface(NEFilterNewFlowVerdict)
    ['{9D06F842-727B-41BC-B664-9CA6A8216172}']
  end;
  TNEFilterControlVerdict = class(TOCGenericImport<NEFilterControlVerdictClass, NEFilterControlVerdict>) end;

  NEFilterReportClass = interface(NSObjectClass)
    ['{608C3B51-4592-4930-81A9-86DC72809B41}']
  end;

  NEFilterReport = interface(NSObject)
    ['{0B91E75F-D565-496C-A986-C94479601737}']
    function action: NEFilterAction; cdecl;
    function bytesInboundCount: NSUInteger; cdecl;
    function bytesOutboundCount: NSUInteger; cdecl;
    function event: NEFilterReportEvent; cdecl;
    function flow: NEFilterFlow; cdecl;
  end;
  TNEFilterReport = class(TOCGenericImport<NEFilterReportClass, NEFilterReport>) end;

  NEFilterControlProviderClass = interface(NEFilterProviderClass)
    ['{17286441-F121-4D3D-A2F6-EB5622F98A7A}']
  end;

  NEFilterControlProvider = interface(NEFilterProvider)
    ['{874CDBDF-530A-49AB-B356-533FCDF62A56}']
    procedure handleNewFlow(flow: NEFilterFlow; completionHandler: TNEFilterControlProviderBlockMethod1); cdecl;
    procedure handleRemediationForFlow(flow: NEFilterFlow; completionHandler: TNEFilterControlProviderBlockMethod1); cdecl;
    procedure notifyRulesChanged; cdecl;
    function remediationMap: NSDictionary; cdecl;
    procedure setRemediationMap(remediationMap: NSDictionary); cdecl;
    procedure setURLAppendStringMap(URLAppendStringMap: NSDictionary); cdecl;
    function URLAppendStringMap: NSDictionary; cdecl;
  end;
  TNEFilterControlProvider = class(TOCGenericImport<NEFilterControlProviderClass, NEFilterControlProvider>) end;

  NEFilterDataProviderClass = interface(NEFilterProviderClass)
    ['{C01899C1-444A-4277-AB8D-DD1DE8680A0F}']
  end;

  NEFilterDataProvider = interface(NEFilterProvider)
    ['{FDEFDC60-1DB2-44A1-807A-FE547F52EAFE}']
    procedure applySettings(settings: NEFilterSettings; completionHandler: TNEFilterDataProviderBlockMethod1); cdecl;
    function handleInboundDataCompleteForFlow(flow: NEFilterFlow): NEFilterDataVerdict; cdecl;
    function handleInboundDataFromFlow(flow: NEFilterFlow; readBytesStartOffset: NSUInteger; readBytes: NSData): NEFilterDataVerdict; cdecl;
    function handleNewFlow(flow: NEFilterFlow): NEFilterNewFlowVerdict; cdecl;
    function handleOutboundDataCompleteForFlow(flow: NEFilterFlow): NEFilterDataVerdict; cdecl;
    function handleOutboundDataFromFlow(flow: NEFilterFlow; readBytesStartOffset: NSUInteger; readBytes: NSData): NEFilterDataVerdict; cdecl;
    function handleRemediationForFlow(flow: NEFilterFlow): NEFilterRemediationVerdict; cdecl;
    procedure handleRulesChanged; cdecl;
    procedure resumeFlow(flow: NEFilterFlow; withVerdict: NEFilterVerdict); cdecl;
    procedure updateFlow(flow: NEFilterSocketFlow; usingVerdict: NEFilterDataVerdict; forDirection: NETrafficDirection); cdecl;
  end;
  TNEFilterDataProvider = class(TOCGenericImport<NEFilterDataProviderClass, NEFilterDataProvider>) end;

  NEFilterDataVerdictClass = interface(NEFilterVerdictClass)
    ['{8B532FF0-EC24-46BB-9A65-1D983CD67990}']
    {class} function allowVerdict: NEFilterDataVerdict; cdecl;
    {class} function dataVerdictWithPassBytes(passBytes: NSUInteger; peekBytes: NSUInteger): NEFilterDataVerdict; cdecl;
    {class} function dropVerdict: NEFilterDataVerdict; cdecl;
    {class} function needRulesVerdict: NEFilterDataVerdict; cdecl;
    {class} function pauseVerdict: NEFilterDataVerdict; cdecl;
    {class} function remediateVerdictWithRemediationURLMapKey(remediationURLMapKey: NSString;
      remediationButtonTextMapKey: NSString): NEFilterDataVerdict; cdecl;
  end;

  NEFilterDataVerdict = interface(NEFilterVerdict)
    ['{073C7A41-5698-4EA4-83D4-ED23B9690DB5}']
    procedure setStatisticsReportFrequency(statisticsReportFrequency: NEFilterReportFrequency); cdecl;
    function statisticsReportFrequency: NEFilterReportFrequency; cdecl;
  end;
  TNEFilterDataVerdict = class(TOCGenericImport<NEFilterDataVerdictClass, NEFilterDataVerdict>) end;

  NEFilterRemediationVerdictClass = interface(NEFilterVerdictClass)
    ['{85F1AE40-9CED-451F-AEB0-6512FF651683}']
    {class} function allowVerdict: NEFilterRemediationVerdict; cdecl;
    {class} function dropVerdict: NEFilterRemediationVerdict; cdecl;
    {class} function needRulesVerdict: NEFilterRemediationVerdict; cdecl;
  end;

  NEFilterRemediationVerdict = interface(NEFilterVerdict)
    ['{D1806B96-C3C7-4BD7-8D0F-A20F2ABFB394}']
  end;
  TNEFilterRemediationVerdict = class(TOCGenericImport<NEFilterRemediationVerdictClass, NEFilterRemediationVerdict>) end;

  NEFilterManagerClass = interface(NSObjectClass)
    ['{DFE0A2B6-D1B6-49AB-A731-29D8F85B6700}']
    {class} function sharedManager: NEFilterManager; cdecl;
  end;

  NEFilterManager = interface(NSObject)
    ['{39C6DF07-43F6-4C75-B9E8-F23584541634}']
    function grade: NEFilterManagerGrade; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEFilterManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function providerConfiguration: NEFilterProviderConfiguration; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEFilterManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEFilterManagerBlockMethod1); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setGrade(grade: NEFilterManagerGrade); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NEFilterProviderConfiguration); cdecl;
  end;
  TNEFilterManager = class(TOCGenericImport<NEFilterManagerClass, NEFilterManager>) end;

  NEFilterPacketContextClass = interface(NSObjectClass)
    ['{C9BFC316-6BE8-4F19-859C-6ADA30038A4C}']
  end;

  NEFilterPacketContext = interface(NSObject)
    ['{074ABD03-D4C0-4534-B74E-3D422ABF4C61}']
  end;
  TNEFilterPacketContext = class(TOCGenericImport<NEFilterPacketContextClass, NEFilterPacketContext>) end;

  NEFilterPacketProviderClass = interface(NEFilterProviderClass)
    ['{F99B325C-ED68-4255-A573-49B72BA63D52}']
  end;

  NEFilterPacketProvider = interface(NEFilterProvider)
    ['{39C10AAA-62AA-4E5E-8B87-FF20E048D115}']
    procedure allowPacket(packet: NEPacket); cdecl;
    function delayCurrentPacket(context: NEFilterPacketContext): NEPacket; cdecl;
    function packetHandler: NEFilterPacketHandler; cdecl;
    procedure setPacketHandler(packetHandler: NEFilterPacketHandler); cdecl;
  end;
  TNEFilterPacketProvider = class(TOCGenericImport<NEFilterPacketProviderClass, NEFilterPacketProvider>) end;

  NEFilterProviderConfigurationClass = interface(NSObjectClass)
    ['{45561ACA-8E7C-412B-8599-8C696A182013}']
  end;

  NEFilterProviderConfiguration = interface(NSObject)
    ['{52F63AA0-9DC0-4EE9-8F1B-F8233BDCA88E}']
    function filterBrowsers: Boolean; cdecl; // API_DEPRECATED("filterBrowsers is not supported on macOS", macos(10.11, 10.15))
    function filterDataProviderBundleIdentifier: NSString; cdecl;
    function filterPacketProviderBundleIdentifier: NSString; cdecl;
    function filterPackets: Boolean; cdecl;
    function filterSockets: Boolean; cdecl;
    function identityReference: NSData; cdecl;
    function organization: NSString; cdecl;
    function passwordReference: NSData; cdecl;
    function serverAddress: NSString; cdecl;
    procedure setFilterBrowsers(filterBrowsers: Boolean); cdecl; // API_DEPRECATED("filterBrowsers is not supported on macOS", macos(10.11, 10.15))
    procedure setFilterDataProviderBundleIdentifier(filterDataProviderBundleIdentifier: NSString); cdecl;
    procedure setFilterPacketProviderBundleIdentifier(filterPacketProviderBundleIdentifier: NSString); cdecl;
    procedure setFilterPackets(filterPackets: Boolean); cdecl;
    procedure setFilterSockets(filterSockets: Boolean); cdecl;
    procedure setIdentityReference(identityReference: NSData); cdecl;
    procedure setOrganization(organization: NSString); cdecl;
    procedure setPasswordReference(passwordReference: NSData); cdecl;
    procedure setServerAddress(serverAddress: NSString); cdecl;
    procedure setUsername(username: NSString); cdecl;
    procedure setVendorConfiguration(vendorConfiguration: NSDictionary); cdecl;
    function username: NSString; cdecl;
    function vendorConfiguration: NSDictionary; cdecl;
  end;
  TNEFilterProviderConfiguration = class(TOCGenericImport<NEFilterProviderConfigurationClass, NEFilterProviderConfiguration>) end;

  NEFilterRuleClass = interface(NSObjectClass)
    ['{C952F6CB-A259-4EC9-B766-B3848E9A2FF8}']
  end;

  NEFilterRule = interface(NSObject)
    ['{CDEDE258-2D0C-485F-941F-EE547EAA147C}']
    function action: NEFilterAction; cdecl;
    function initWithNetworkRule(networkRule: NENetworkRule; action: NEFilterAction): Pointer; cdecl;
    function networkRule: NENetworkRule; cdecl;
  end;
  TNEFilterRule = class(TOCGenericImport<NEFilterRuleClass, NEFilterRule>) end;

  NEFilterSettingsClass = interface(NSObjectClass)
    ['{B0283E3F-CD80-4BE0-8C13-44B976C28F59}']
  end;

  NEFilterSettings = interface(NSObject)
    ['{8E558798-B4A3-48F6-980D-3E6F51097951}']
    function defaultAction: NEFilterAction; cdecl;
    function initWithRules(rules: NSArray; defaultAction: NEFilterAction): Pointer; cdecl;
    function rules: NSArray; cdecl;
  end;
  TNEFilterSettings = class(TOCGenericImport<NEFilterSettingsClass, NEFilterSettings>) end;

  NEFlowMetaDataClass = interface(NSObjectClass)
    ['{ECBBA0DC-2DBD-4E15-968E-4FDE2694E394}']
  end;

  NEFlowMetaData = interface(NSObject)
    ['{B6AB08C0-620E-4FB4-81ED-9BE6D3DE0EA5}']
    function filterFlowIdentifier: NSUUID; cdecl;
    function sourceAppAuditToken: NSData; cdecl;
    function sourceAppSigningIdentifier: NSString; cdecl;
    function sourceAppUniqueIdentifier: NSData; cdecl;
  end;
  TNEFlowMetaData = class(TOCGenericImport<NEFlowMetaDataClass, NEFlowMetaData>) end;

  NEHotspotNetworkClass = interface(NSObjectClass)
    ['{1153040E-B6BA-40D5-8A36-192DD479C5D4}']
    {class} procedure fetchCurrentWithCompletionHandler(completionHandler: TNEHotspotNetworkBlockMethod1); cdecl;
  end;

  NEHotspotNetwork = interface(NSObject)
    ['{05812A0C-0194-4D1F-8671-DBF746D584C9}']
    function BSSID: NSString; cdecl;
    function didAutoJoin: Boolean; cdecl;
    function didJustJoin: Boolean; cdecl;
    function isChosenHelper: Boolean; cdecl;
    function isSecure: Boolean; cdecl;
    procedure setConfidence(confidence: NEHotspotHelperConfidence); cdecl;
    procedure setPassword(password: NSString); cdecl;
    function signalStrength: Double; cdecl;
    function SSID: NSString; cdecl;
  end;
  TNEHotspotNetwork = class(TOCGenericImport<NEHotspotNetworkClass, NEHotspotNetwork>) end;

  NEHotspotHelperCommandClass = interface(NSObjectClass)
    ['{1C49EED3-7D43-4741-A923-2353E0569582}']
  end;

  NEHotspotHelperCommand = interface(NSObject)
    ['{DD9F1BA0-EA93-4584-BB5A-23ADEAE5528C}']
    function commandType: NEHotspotHelperCommandType; cdecl;
    function createResponse(result: NEHotspotHelperResult): NEHotspotHelperResponse; cdecl;
    function createTCPConnection(endpoint: NWEndpoint): NWTCPConnection; cdecl;
    function createUDPSession(endpoint: NWEndpoint): NWUDPSession; cdecl;
    function network: NEHotspotNetwork; cdecl;
    function networkList: NSArray; cdecl;
  end;
  TNEHotspotHelperCommand = class(TOCGenericImport<NEHotspotHelperCommandClass, NEHotspotHelperCommand>) end;

  NEHotspotHelperResponseClass = interface(NSObjectClass)
    ['{FEC0D926-D7CA-46BA-BD09-75B7A25F21D3}']
  end;

  NEHotspotHelperResponse = interface(NSObject)
    ['{9D88D39E-244C-46D5-AE23-085CEAE1FC85}']
    procedure deliver; cdecl;
    procedure setNetwork(network: NEHotspotNetwork); cdecl;
    procedure setNetworkList(networkList: NSArray); cdecl;
  end;
  TNEHotspotHelperResponse = class(TOCGenericImport<NEHotspotHelperResponseClass, NEHotspotHelperResponse>) end;

  NEHotspotHelperClass = interface(NSObjectClass)
    ['{A7B7C2D1-EC08-42AC-AB2A-CACB4087E3A6}']
    {class} function logoff(network: NEHotspotNetwork): Boolean; cdecl;
    {class} function registerWithOptions(options: NSDictionary; queue: dispatch_queue_t; handler: NEHotspotHelperHandler): Boolean; cdecl;
    {class} function supportedNetworkInterfaces: NSArray; cdecl;
  end;

  NEHotspotHelper = interface(NSObject)
    ['{AB1F06CF-95B3-4504-8F73-F4A5AE708E3B}']
  end;
  TNEHotspotHelper = class(TOCGenericImport<NEHotspotHelperClass, NEHotspotHelper>) end;

  NEHotspotHS20SettingsClass = interface(NSObjectClass)
    ['{9D770A4E-7D32-4076-9BFA-E0F9CAE290E6}']
  end;

  NEHotspotHS20Settings = interface(NSObject)
    ['{F9291FEB-14FE-44AE-8FC2-75642771DEB6}']
    function domainName: NSString; cdecl;
    function initWithDomainName(domainName: NSString; roamingEnabled: Boolean): Pointer; cdecl;
    function isRoamingEnabled: Boolean; cdecl;
    function MCCAndMNCs: NSArray; cdecl;
    function naiRealmNames: NSArray; cdecl;
    function roamingConsortiumOIs: NSArray; cdecl;
    procedure setMCCAndMNCs(MCCAndMNCs: NSArray); cdecl;
    procedure setNaiRealmNames(naiRealmNames: NSArray); cdecl;
    procedure setRoamingConsortiumOIs(roamingConsortiumOIs: NSArray); cdecl;
    procedure setRoamingEnabled(roamingEnabled: Boolean); cdecl;
  end;
  TNEHotspotHS20Settings = class(TOCGenericImport<NEHotspotHS20SettingsClass, NEHotspotHS20Settings>) end;

  NEHotspotEAPSettingsClass = interface(NSObjectClass)
    ['{DAE061B7-C032-4BAC-B003-7458C72ED3E9}']
  end;

  NEHotspotEAPSettings = interface(NSObject)
    ['{D474C278-ED42-4CD6-AAD8-D487ABC37A59}']
    function isTLSClientCertificateRequired: Boolean; cdecl;
    function outerIdentity: NSString; cdecl;
    function password: NSString; cdecl;
    function preferredTLSVersion: NEHotspotConfigurationEAPTLSVersion; cdecl;
    function setIdentity(identity: SecIdentityRef): Boolean; cdecl;
    procedure setOuterIdentity(outerIdentity: NSString); cdecl;
    procedure setPassword(password: NSString); cdecl;
    procedure setPreferredTLSVersion(preferredTLSVersion: NEHotspotConfigurationEAPTLSVersion); cdecl;
    procedure setSupportedEAPTypes(supportedEAPTypes: NSArray); cdecl;
    procedure setTlsClientCertificateRequired(tlsClientCertificateRequired: Boolean); cdecl;
    function setTrustedServerCertificates(certificates: NSArray): Boolean; cdecl;
    procedure setTrustedServerNames(trustedServerNames: NSArray); cdecl;
    procedure setTtlsInnerAuthenticationType(ttlsInnerAuthenticationType: NEHotspotConfigurationTTLSInnerAuthenticationType); cdecl;
    procedure setUsername(username: NSString); cdecl;
    function supportedEAPTypes: NSArray; cdecl;
    function trustedServerNames: NSArray; cdecl;
    function ttlsInnerAuthenticationType: NEHotspotConfigurationTTLSInnerAuthenticationType; cdecl;
    function username: NSString; cdecl;
  end;
  TNEHotspotEAPSettings = class(TOCGenericImport<NEHotspotEAPSettingsClass, NEHotspotEAPSettings>) end;

  NEHotspotConfigurationClass = interface(NSObjectClass)
    ['{035B9682-49FF-45A0-937F-230484504BE7}']
  end;

  NEHotspotConfiguration = interface(NSObject)
    ['{A11B3C0D-F548-4ACE-A214-4B40AE8C6943}']
    function hidden: Boolean; cdecl;
    function initWithHS20Settings(hs20Settings: NEHotspotHS20Settings; eapSettings: NEHotspotEAPSettings): Pointer; cdecl;
    function initWithSSID(SSID: NSString; eapSettings: NEHotspotEAPSettings): Pointer; overload; cdecl;
    function initWithSSID(SSID: NSString; passphrase: NSString; isWEP: Boolean): Pointer; overload; cdecl;
    function initWithSSID(SSID: NSString): Pointer; overload; cdecl;
    function initWithSSIDPrefix(SSIDPrefix: NSString; passphrase: NSString; isWEP: Boolean): Pointer; overload; cdecl;
    function initWithSSIDPrefix(SSIDPrefix: NSString): Pointer; overload; cdecl;
    function joinOnce: Boolean; cdecl;
    function lifeTimeInDays: NSNumber; cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setJoinOnce(joinOnce: Boolean); cdecl;
    procedure setLifeTimeInDays(lifeTimeInDays: NSNumber); cdecl;
    function SSID: NSString; cdecl;
    function SSIDPrefix: NSString; cdecl;
  end;
  TNEHotspotConfiguration = class(TOCGenericImport<NEHotspotConfigurationClass, NEHotspotConfiguration>) end;

  NEHotspotConfigurationManagerClass = interface(NSObjectClass)
    ['{4C18E30E-726F-4F66-AFE1-AB776FED2876}']
    {class} function sharedManager: NEHotspotConfigurationManager; cdecl;
  end;

  NEHotspotConfigurationManager = interface(NSObject)
    ['{7F9DF3C9-3BD2-4D5E-A491-973D7A889A96}']
    procedure applyConfiguration(configuration: NEHotspotConfiguration; completionHandler: TNEHotspotConfigurationManagerBlockMethod1); cdecl;
    procedure getConfiguredSSIDsWithCompletionHandler(completionHandler: TNEHotspotConfigurationManagerBlockMethod2); cdecl;
    procedure removeConfigurationForHS20DomainName(domainName: NSString); cdecl;
    procedure removeConfigurationForSSID(SSID: NSString); cdecl;
  end;
  TNEHotspotConfigurationManager = class(TOCGenericImport<NEHotspotConfigurationManagerClass, NEHotspotConfigurationManager>) end;

  NEIPv4SettingsClass = interface(NSObjectClass)
    ['{E5A223C6-B5BE-40B4-A644-41DBB7F82469}']
  end;

  NEIPv4Settings = interface(NSObject)
    ['{AF82AD79-B119-4D7E-9395-016A9BB6E37E}']
    function addresses: NSArray; cdecl;
    function excludedRoutes: NSArray; cdecl;
    function includedRoutes: NSArray; cdecl;
    function initWithAddresses(addresses: NSArray; subnetMasks: NSArray): Pointer; cdecl;
    procedure setExcludedRoutes(excludedRoutes: NSArray); cdecl;
    procedure setIncludedRoutes(includedRoutes: NSArray); cdecl;
    function subnetMasks: NSArray; cdecl;
  end;
  TNEIPv4Settings = class(TOCGenericImport<NEIPv4SettingsClass, NEIPv4Settings>) end;

  NEIPv4RouteClass = interface(NSObjectClass)
    ['{F411B200-B9BA-4399-85D4-DDECEEA22472}']
    {class} function defaultRoute: NEIPv4Route; cdecl;
  end;

  NEIPv4Route = interface(NSObject)
    ['{B0907EC6-DD18-4196-AB26-11966654A569}']
    function destinationAddress: NSString; cdecl;
    function destinationSubnetMask: NSString; cdecl;
    function gatewayAddress: NSString; cdecl;
    function initWithDestinationAddress(address: NSString; subnetMask: NSString): Pointer; cdecl;
    procedure setGatewayAddress(gatewayAddress: NSString); cdecl;
  end;
  TNEIPv4Route = class(TOCGenericImport<NEIPv4RouteClass, NEIPv4Route>) end;

  NEIPv6SettingsClass = interface(NSObjectClass)
    ['{115BA9C1-99B4-40F1-B3C6-32E0BCE927B9}']
  end;

  NEIPv6Settings = interface(NSObject)
    ['{774F9E24-8653-4268-B0CD-4630C5C7CED6}']
    function addresses: NSArray; cdecl;
    function excludedRoutes: NSArray; cdecl;
    function includedRoutes: NSArray; cdecl;
    function initWithAddresses(addresses: NSArray; networkPrefixLengths: NSArray): Pointer; cdecl;
    function networkPrefixLengths: NSArray; cdecl;
    procedure setExcludedRoutes(excludedRoutes: NSArray); cdecl;
    procedure setIncludedRoutes(includedRoutes: NSArray); cdecl;
  end;
  TNEIPv6Settings = class(TOCGenericImport<NEIPv6SettingsClass, NEIPv6Settings>) end;

  NEIPv6RouteClass = interface(NSObjectClass)
    ['{1C29ADBD-2B5F-4383-BB9E-61B7001321A1}']
    {class} function defaultRoute: NEIPv6Route; cdecl;
  end;

  NEIPv6Route = interface(NSObject)
    ['{0C210D39-B895-4C76-B08C-B2473D5AA5B9}']
    function destinationAddress: NSString; cdecl;
    function destinationNetworkPrefixLength: NSNumber; cdecl;
    function gatewayAddress: NSString; cdecl;
    function initWithDestinationAddress(address: NSString; networkPrefixLength: NSNumber): Pointer; cdecl;
    procedure setGatewayAddress(gatewayAddress: NSString); cdecl;
  end;
  TNEIPv6Route = class(TOCGenericImport<NEIPv6RouteClass, NEIPv6Route>) end;

  NEOnDemandRuleClass = interface(NSObjectClass)
    ['{730709DF-3689-49C5-86A1-5E10BA485FF7}']
  end;

  NEOnDemandRule = interface(NSObject)
    ['{66E9DF5C-51CB-499C-A7F1-1D104BE76A27}']
    function action: NEOnDemandRuleAction; cdecl;
    function DNSSearchDomainMatch: NSArray; cdecl;
    function DNSServerAddressMatch: NSArray; cdecl;
    function interfaceTypeMatch: NEOnDemandRuleInterfaceType; cdecl;
    function probeURL: NSURL; cdecl;
    procedure setDNSSearchDomainMatch(DNSSearchDomainMatch: NSArray); cdecl;
    procedure setDNSServerAddressMatch(DNSServerAddressMatch: NSArray); cdecl;
    procedure setInterfaceTypeMatch(interfaceTypeMatch: NEOnDemandRuleInterfaceType); cdecl;
    procedure setProbeURL(probeURL: NSURL); cdecl;
    procedure setSSIDMatch(SSIDMatch: NSArray); cdecl;
    function SSIDMatch: NSArray; cdecl;
  end;
  TNEOnDemandRule = class(TOCGenericImport<NEOnDemandRuleClass, NEOnDemandRule>) end;

  NEOnDemandRuleConnectClass = interface(NEOnDemandRuleClass)
    ['{3215317E-6BD9-4007-BF25-0C298878CD21}']
  end;

  NEOnDemandRuleConnect = interface(NEOnDemandRule)
    ['{4314FD92-5909-4DBB-9B81-BEECAAA37A6A}']
  end;
  TNEOnDemandRuleConnect = class(TOCGenericImport<NEOnDemandRuleConnectClass, NEOnDemandRuleConnect>) end;

  NEOnDemandRuleDisconnectClass = interface(NEOnDemandRuleClass)
    ['{05FA954B-0E33-4ABC-930F-37882492578D}']
  end;

  NEOnDemandRuleDisconnect = interface(NEOnDemandRule)
    ['{96016BF4-E0B9-4875-871B-1A995D9E51A6}']
  end;
  TNEOnDemandRuleDisconnect = class(TOCGenericImport<NEOnDemandRuleDisconnectClass, NEOnDemandRuleDisconnect>) end;

  NEOnDemandRuleIgnoreClass = interface(NEOnDemandRuleClass)
    ['{984BDB95-0F49-465C-90D9-8A4416447A16}']
  end;

  NEOnDemandRuleIgnore = interface(NEOnDemandRule)
    ['{77E6337F-4AF1-48C8-BD4C-32CDEDD1FD14}']
  end;
  TNEOnDemandRuleIgnore = class(TOCGenericImport<NEOnDemandRuleIgnoreClass, NEOnDemandRuleIgnore>) end;

  NEOnDemandRuleEvaluateConnectionClass = interface(NEOnDemandRuleClass)
    ['{34F53D83-56BB-4974-ABB3-220B95E519E9}']
  end;

  NEOnDemandRuleEvaluateConnection = interface(NEOnDemandRule)
    ['{C4964DC4-9A2C-4C9D-AC3E-7AEB37289903}']
    function connectionRules: NSArray; cdecl;
    procedure setConnectionRules(connectionRules: NSArray); cdecl;
  end;
  TNEOnDemandRuleEvaluateConnection = class(TOCGenericImport<NEOnDemandRuleEvaluateConnectionClass, NEOnDemandRuleEvaluateConnection>) end;

  NEEvaluateConnectionRuleClass = interface(NSObjectClass)
    ['{42C25435-AFCB-4772-9460-87DF993F01B8}']
  end;

  NEEvaluateConnectionRule = interface(NSObject)
    ['{01D9B70F-C845-44CA-A2C2-29503007B98C}']
    function action: NEEvaluateConnectionRuleAction; cdecl;
    function initWithMatchDomains(domains: NSArray; andAction: NEEvaluateConnectionRuleAction): Pointer; cdecl;
    function matchDomains: NSArray; cdecl;
    function probeURL: NSURL; cdecl;
    procedure setProbeURL(probeURL: NSURL); cdecl;
    procedure setUseDNSServers(useDNSServers: NSArray); cdecl;
    function useDNSServers: NSArray; cdecl;
  end;
  TNEEvaluateConnectionRule = class(TOCGenericImport<NEEvaluateConnectionRuleClass, NEEvaluateConnectionRule>) end;

  NEPacketClass = interface(NSObjectClass)
    ['{233C63CB-3F00-4DA8-9EAD-C10DF4DEC169}']
  end;

  NEPacket = interface(NSObject)
    ['{37FFC3EF-4DBB-4B6F-A0D8-65D906097496}']
    function data: NSData; cdecl;
    function direction: NETrafficDirection; cdecl;
    function initWithData(data: NSData; protocolFamily: sa_family_t): Pointer; cdecl;
    function metadata: NEFlowMetaData; cdecl;
    function protocolFamily: sa_family_t; cdecl;
  end;
  TNEPacket = class(TOCGenericImport<NEPacketClass, NEPacket>) end;

  NEPacketTunnelFlowClass = interface(NSObjectClass)
    ['{74F38174-F85C-43CE-839E-266FAA3B3BD0}']
  end;

  NEPacketTunnelFlow = interface(NSObject)
    ['{75330DFF-063F-43BE-B418-E4CDEA66EEC7}']
    procedure readPacketObjectsWithCompletionHandler(completionHandler: TNEPacketTunnelFlowBlockMethod2); cdecl;
    procedure readPacketsWithCompletionHandler(completionHandler: TNEPacketTunnelFlowBlockMethod1); cdecl;
    function writePacketObjects(packets: NSArray): Boolean; cdecl;
    function writePackets(packets: NSArray; withProtocols: NSArray): Boolean; cdecl;
  end;
  TNEPacketTunnelFlow = class(TOCGenericImport<NEPacketTunnelFlowClass, NEPacketTunnelFlow>) end;

  NETunnelNetworkSettingsClass = interface(NSObjectClass)
    ['{F1E50AE4-E28B-4096-AE1C-5A33D9BF43D1}']
  end;

  NETunnelNetworkSettings = interface(NSObject)
    ['{474F67AD-DC24-4FEF-8705-FBD08E251986}']
    function DNSSettings: NEDNSSettings; cdecl;
    function initWithTunnelRemoteAddress(address: NSString): Pointer; cdecl;
    function proxySettings: NEProxySettings; cdecl;
    procedure setDNSSettings(DNSSettings: NEDNSSettings); cdecl;
    procedure setProxySettings(proxySettings: NEProxySettings); cdecl;
    function tunnelRemoteAddress: NSString; cdecl;
  end;
  TNETunnelNetworkSettings = class(TOCGenericImport<NETunnelNetworkSettingsClass, NETunnelNetworkSettings>) end;

  NEPacketTunnelNetworkSettingsClass = interface(NETunnelNetworkSettingsClass)
    ['{48F25146-BB10-499A-8C99-79790297FC8C}']
  end;

  NEPacketTunnelNetworkSettings = interface(NETunnelNetworkSettings)
    ['{1D40D62B-CCA7-43FA-933A-EEDD18F0A0BA}']
    function IPv4Settings: NEIPv4Settings; cdecl;
    function IPv6Settings: NEIPv6Settings; cdecl;
    function MTU: NSNumber; cdecl;
    procedure setIPv4Settings(IPv4Settings: NEIPv4Settings); cdecl;
    procedure setIPv6Settings(IPv6Settings: NEIPv6Settings); cdecl;
    procedure setMTU(MTU: NSNumber); cdecl;
    procedure setTunnelOverheadBytes(tunnelOverheadBytes: NSNumber); cdecl;
    function tunnelOverheadBytes: NSNumber; cdecl;
  end;
  TNEPacketTunnelNetworkSettings = class(TOCGenericImport<NEPacketTunnelNetworkSettingsClass, NEPacketTunnelNetworkSettings>) end;

  NEPacketTunnelProviderClass = interface(NETunnelProviderClass)
    ['{0172A569-DE2B-480F-9BDB-F241B865ACB7}']
  end;

  NEPacketTunnelProvider = interface(NETunnelProvider)
    ['{A12FE3B1-3971-434A-845C-3AB2047A0D7A}']
    procedure cancelTunnelWithError(error: NSError); cdecl;
    function createTCPConnectionThroughTunnelToEndpoint(remoteEndpoint: NWEndpoint; enableTLS: Boolean; TLSParameters: NWTLSParameters;
      delegate: Pointer): NWTCPConnection; cdecl;
    function createUDPSessionThroughTunnelToEndpoint(remoteEndpoint: NWEndpoint; fromEndpoint: NWHostEndpoint): NWUDPSession; cdecl;
    function packetFlow: NEPacketTunnelFlow; cdecl;
    procedure startTunnelWithOptions(options: NSDictionary; completionHandler: TNEPacketTunnelProviderBlockMethod1); cdecl;
    procedure stopTunnelWithReason(reason: NEProviderStopReason; completionHandler: TNEPacketTunnelProviderBlockMethod2); cdecl;
  end;
  TNEPacketTunnelProvider = class(TOCGenericImport<NEPacketTunnelProviderClass, NEPacketTunnelProvider>) end;

  NETransparentProxyManagerClass = interface(NEVPNManagerClass)
    ['{C0279EE4-8430-49B4-8020-A26C77F6A27F}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNETransparentProxyManagerBlockMethod1); cdecl;
  end;

  NETransparentProxyManager = interface(NEVPNManager)
    ['{76E529AC-B3FF-4BAD-89EE-C825EFC9ABB2}']
  end;
  TNETransparentProxyManager = class(TOCGenericImport<NETransparentProxyManagerClass, NETransparentProxyManager>) end;

  NETransparentProxyNetworkSettingsClass = interface(NETunnelNetworkSettingsClass)
    ['{4BCDCAF9-2F4C-4A20-A5C5-55796B88AD67}']
  end;

  NETransparentProxyNetworkSettings = interface(NETunnelNetworkSettings)
    ['{402D2BE3-F381-4923-B0C5-31B4BFD4EC4E}']
    function excludedNetworkRules: NSArray; cdecl;
    function includedNetworkRules: NSArray; cdecl;
    procedure setExcludedNetworkRules(excludedNetworkRules: NSArray); cdecl;
    procedure setIncludedNetworkRules(includedNetworkRules: NSArray); cdecl;
  end;
  TNETransparentProxyNetworkSettings = class(TOCGenericImport<NETransparentProxyNetworkSettingsClass, NETransparentProxyNetworkSettings>) end;

  NETransparentProxyProviderClass = interface(NEAppProxyProviderClass)
    ['{843009E0-8E6B-4F22-B920-5B6FD5F8F3F9}']
  end;

  NETransparentProxyProvider = interface(NEAppProxyProvider)
    ['{5762ED3A-7DE2-46B1-94B0-C80C05781807}']
  end;
  TNETransparentProxyProvider = class(TOCGenericImport<NETransparentProxyProviderClass, NETransparentProxyProvider>) end;

  NEVPNConnectionClass = interface(NSObjectClass)
    ['{8AAC106C-C063-47FA-8F7E-2CCA06F93C6B}']
  end;

  NEVPNConnection = interface(NSObject)
    ['{E1262334-F5A8-4AE4-9644-1B4C9FA5B243}']
    function connectedDate: NSDate; cdecl;
    function manager: NEVPNManager; cdecl;
    function startVPNTunnelAndReturnError(error: PPointer): Boolean; cdecl;
    function startVPNTunnelWithOptions(options: NSDictionary; andReturnError: PPointer): Boolean; cdecl;
    function status: NEVPNStatus; cdecl;
    procedure stopVPNTunnel; cdecl;
  end;
  TNEVPNConnection = class(TOCGenericImport<NEVPNConnectionClass, NEVPNConnection>) end;

  NETunnelProviderSessionClass = interface(NEVPNConnectionClass)
    ['{1FF9861D-953B-469A-A7A5-A05445F88CE3}']
  end;

  NETunnelProviderSession = interface(NEVPNConnection)
    ['{75EB9ADC-102C-4476-86E6-B6A79B734A81}']
    function sendProviderMessage(messageData: NSData; returnError: PPointer; responseHandler: TNETunnelProviderSessionBlockMethod1): Boolean; cdecl;
    function startTunnelWithOptions(options: NSDictionary; andReturnError: PPointer): Boolean; cdecl;
    procedure stopTunnel; cdecl;
  end;
  TNETunnelProviderSession = class(TOCGenericImport<NETunnelProviderSessionClass, NETunnelProviderSession>) end;

  NETunnelProviderProtocolClass = interface(NEVPNProtocolClass)
    ['{18CAEDF4-897C-418A-855C-9B04A1DF05FD}']
  end;

  NETunnelProviderProtocol = interface(NEVPNProtocol)
    ['{9E659D87-E011-4895-B469-A0ECAB99D571}']
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNETunnelProviderProtocol = class(TOCGenericImport<NETunnelProviderProtocolClass, NETunnelProviderProtocol>) end;

  NEVPNProtocolIPSecClass = interface(NEVPNProtocolClass)
    ['{AE678CCB-836B-4E91-A039-4048E06619AE}']
  end;

  NEVPNProtocolIPSec = interface(NEVPNProtocol)
    ['{93EF0A38-FDCF-4BDA-96DB-5AA31D3F6530}']
    function authenticationMethod: NEVPNIKEAuthenticationMethod; cdecl;
    function localIdentifier: NSString; cdecl;
    function remoteIdentifier: NSString; cdecl;
    procedure setAuthenticationMethod(authenticationMethod: NEVPNIKEAuthenticationMethod); cdecl;
    procedure setLocalIdentifier(localIdentifier: NSString); cdecl;
    procedure setRemoteIdentifier(remoteIdentifier: NSString); cdecl;
    procedure setSharedSecretReference(sharedSecretReference: NSData); cdecl;
    procedure setUseExtendedAuthentication(useExtendedAuthentication: Boolean); cdecl;
    function sharedSecretReference: NSData; cdecl;
    function useExtendedAuthentication: Boolean; cdecl;
  end;
  TNEVPNProtocolIPSec = class(TOCGenericImport<NEVPNProtocolIPSecClass, NEVPNProtocolIPSec>) end;

  NEVPNIKEv2SecurityAssociationParametersClass = interface(NSObjectClass)
    ['{CC36D2AA-3E25-464F-B589-92BAFE440392}']
  end;

  NEVPNIKEv2SecurityAssociationParameters = interface(NSObject)
    ['{CF02B785-013B-40F6-A3C3-5E849CAFDDD6}']
    function diffieHellmanGroup: NEVPNIKEv2DiffieHellmanGroup; cdecl;
    function encryptionAlgorithm: NEVPNIKEv2EncryptionAlgorithm; cdecl;
    function integrityAlgorithm: NEVPNIKEv2IntegrityAlgorithm; cdecl;
    function lifetimeMinutes: Int32; cdecl;
    procedure setDiffieHellmanGroup(diffieHellmanGroup: NEVPNIKEv2DiffieHellmanGroup); cdecl;
    procedure setEncryptionAlgorithm(encryptionAlgorithm: NEVPNIKEv2EncryptionAlgorithm); cdecl;
    procedure setIntegrityAlgorithm(integrityAlgorithm: NEVPNIKEv2IntegrityAlgorithm); cdecl;
    procedure setLifetimeMinutes(lifetimeMinutes: Int32); cdecl;
  end;
  TNEVPNIKEv2SecurityAssociationParameters = class(TOCGenericImport<NEVPNIKEv2SecurityAssociationParametersClass,
    NEVPNIKEv2SecurityAssociationParameters>) end;

  NEVPNProtocolIKEv2Class = interface(NEVPNProtocolIPSecClass)
    ['{6AF6370B-CEA2-4CB6-B6F6-5309162BCDC3}']
  end;

  NEVPNProtocolIKEv2 = interface(NEVPNProtocolIPSec)
    ['{EDAC9446-F48D-4B31-85BA-36233F20F1CC}']
    function certificateType: NEVPNIKEv2CertificateType; cdecl;
    function childSecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters; cdecl;
    function deadPeerDetectionRate: NEVPNIKEv2DeadPeerDetectionRate; cdecl;
    function disableMOBIKE: Boolean; cdecl;
    function disableRedirect: Boolean; cdecl;
    function enableFallback: Boolean; cdecl;
    function enablePFS: Boolean; cdecl;
    function enableRevocationCheck: Boolean; cdecl;
    function IKESecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters; cdecl;
    function maximumTLSVersion: NEVPNIKEv2TLSVersion; cdecl;
    function minimumTLSVersion: NEVPNIKEv2TLSVersion; cdecl;
    function mtu: NSUInteger; cdecl;
    function serverCertificateCommonName: NSString; cdecl;
    function serverCertificateIssuerCommonName: NSString; cdecl;
    procedure setCertificateType(certificateType: NEVPNIKEv2CertificateType); cdecl;
    procedure setDeadPeerDetectionRate(deadPeerDetectionRate: NEVPNIKEv2DeadPeerDetectionRate); cdecl;
    procedure setDisableMOBIKE(disableMOBIKE: Boolean); cdecl;
    procedure setDisableRedirect(disableRedirect: Boolean); cdecl;
    procedure setEnableFallback(enableFallback: Boolean); cdecl;
    procedure setEnablePFS(enablePFS: Boolean); cdecl;
    procedure setEnableRevocationCheck(enableRevocationCheck: Boolean); cdecl;
    procedure setMaximumTLSVersion(maximumTLSVersion: NEVPNIKEv2TLSVersion); cdecl;
    procedure setMinimumTLSVersion(minimumTLSVersion: NEVPNIKEv2TLSVersion); cdecl;
    procedure setMtu(mtu: NSUInteger); cdecl;
    procedure setServerCertificateCommonName(serverCertificateCommonName: NSString); cdecl;
    procedure setServerCertificateIssuerCommonName(serverCertificateIssuerCommonName: NSString); cdecl;
    procedure setStrictRevocationCheck(strictRevocationCheck: Boolean); cdecl;
    procedure setUseConfigurationAttributeInternalIPSubnet(useConfigurationAttributeInternalIPSubnet: Boolean); cdecl;
    function strictRevocationCheck: Boolean; cdecl;
    function useConfigurationAttributeInternalIPSubnet: Boolean; cdecl;
  end;
  TNEVPNProtocolIKEv2 = class(TOCGenericImport<NEVPNProtocolIKEv2Class, NEVPNProtocolIKEv2>) end;

  NEAppPushManagerClass = interface(NSObjectClass)
    ['{C1361B0A-B0A2-4EB1-A134-D80F319EA7D9}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod1); cdecl;
  end;

  NEAppPushManager = interface(NSObject)
    ['{4C7D0AA5-419E-4A40-A27C-4A9A520E1528}']
    function delegate: Pointer; cdecl;
    function isActive: Boolean; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    function localizedDescription: NSString; cdecl;
    function matchSSIDs: NSArray; cdecl;
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setMatchSSIDs(matchSSIDs: NSArray); cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNEAppPushManager = class(TOCGenericImport<NEAppPushManagerClass, NEAppPushManager>) end;

  NEAppPushDelegate = interface(IObjectiveC)
    ['{709D58AE-4AA4-4B8F-AD85-C234B9B588AA}']
    procedure appPushManager(manager: NEAppPushManager; didReceiveIncomingCallWithUserInfo: NSDictionary); cdecl;
  end;

  NEAppPushProviderClass = interface(NEProviderClass)
    ['{37F67FEE-3B2C-47BB-9A93-76C7722FB5D6}']
  end;

  NEAppPushProvider = interface(NEProvider)
    ['{086D4187-C687-4509-B293-DF742836FA54}']
    procedure handleTimerEvent; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure reportIncomingCallWithUserInfo(userInfo: NSDictionary); cdecl;
    procedure startWithCompletionHandler(completionHandler: TNEAppPushProviderBlockMethod1); cdecl;
    procedure stopWithReason(reason: NEProviderStopReason; completionHandler: TNEAppPushProviderBlockMethod2); cdecl;
  end;
  TNEAppPushProvider = class(TOCGenericImport<NEAppPushProviderClass, NEAppPushProvider>) end;

  NWEndpointClass = interface(NSObjectClass)
    ['{13C1AF08-393D-4470-A2A0-61D06767D993}']
  end;

  NWEndpoint = interface(NSObject)
    ['{FB353253-FD32-4D5D-9F97-CC049935B0B0}']
  end;
  TNWEndpoint = class(TOCGenericImport<NWEndpointClass, NWEndpoint>) end;

  NWHostEndpointClass = interface(NWEndpointClass)
    ['{BD56373D-11AD-4202-A1C6-806FF9254710}']
    {class} function endpointWithHostname(hostname: NSString; port: NSString): Pointer; cdecl;
  end;

  NWHostEndpoint = interface(NWEndpoint)
    ['{53EAFCED-ADDE-4760-B20F-1EFBD359CBBB}']
    function hostname: NSString; cdecl;
    function port: NSString; cdecl;
  end;
  TNWHostEndpoint = class(TOCGenericImport<NWHostEndpointClass, NWHostEndpoint>) end;

  NWBonjourServiceEndpointClass = interface(NWEndpointClass)
    ['{9C033234-89C0-489D-B47E-2F1D9748FCA7}']
    {class} function endpointWithName(name: NSString; &type: NSString; domain: NSString): Pointer; cdecl;
  end;

  NWBonjourServiceEndpoint = interface(NWEndpoint)
    ['{675C5BB3-DD83-4C3A-A2DC-4A3C2BA0D654}']
    [MethodName('type')]
    function &type: NSString; cdecl;
    function domain: NSString; cdecl;
    function name: NSString; cdecl;
  end;
  TNWBonjourServiceEndpoint = class(TOCGenericImport<NWBonjourServiceEndpointClass, NWBonjourServiceEndpoint>) end;

  NWPathClass = interface(NSObjectClass)
    ['{FAA4BF5E-B2F2-4D67-8565-238CEE1DBC5C}']
  end;

  NWPath = interface(NSObject)
    ['{B61AD213-51FB-4FF4-8103-3C2D3CC17FEA}']
    function isConstrained: Boolean; cdecl;
    function isEqualToPath(path: NWPath): Boolean; cdecl;
    function isExpensive: Boolean; cdecl;
    function status: NWPathStatus; cdecl;
  end;
  TNWPath = class(TOCGenericImport<NWPathClass, NWPath>) end;

  NWTCPConnectionClass = interface(NSObjectClass)
    ['{355E31A3-2DA8-458B-9D0E-1B05B79C33FC}']
  end;

  NWTCPConnection = interface(NSObject)
    ['{C564FD15-773F-40B9-B9B2-5F4DDAB4EBF9}']
    procedure cancel; cdecl;
    function connectedPath: NWPath; cdecl;
    function endpoint: NWEndpoint; cdecl;
    function error: NSError; cdecl;
    function hasBetterPath: Boolean; cdecl;
    function initWithUpgradeForConnection(connection: NWTCPConnection): Pointer; cdecl;
    function isViable: Boolean; cdecl;
    function localAddress: NWEndpoint; cdecl;
    procedure readLength(length: NSUInteger; completionHandler: TNWTCPConnectionBlockMethod1); cdecl;
    procedure readMinimumLength(minimum: NSUInteger; maximumLength: NSUInteger; completionHandler: TNWTCPConnectionBlockMethod1); cdecl;
    function remoteAddress: NWEndpoint; cdecl;
    function state: NWTCPConnectionState; cdecl;
    function txtRecord: NSData; cdecl;
    procedure write(data: NSData; completionHandler: TNWTCPConnectionBlockMethod2); cdecl;
    procedure writeClose; cdecl;
  end;
  TNWTCPConnection = class(TOCGenericImport<NWTCPConnectionClass, NWTCPConnection>) end;

  NWTCPConnectionAuthenticationDelegate = interface(IObjectiveC)
    ['{1B74D27D-A212-466F-BE9B-46F750911AA2}']
    procedure evaluateTrustForConnection(connection: NWTCPConnection; peerCertificateChain: NSArray;
      completionHandler: TNWTCPConnectionAuthenticationDelegateBlockMethod2); cdecl;
    procedure provideIdentityForConnection(connection: NWTCPConnection; completionHandler: TNWTCPConnectionAuthenticationDelegateBlockMethod1); cdecl;
    function shouldEvaluateTrustForConnection(connection: NWTCPConnection): Boolean; cdecl;
    function shouldProvideIdentityForConnection(connection: NWTCPConnection): Boolean; cdecl;
  end;

  NWUDPSessionClass = interface(NSObjectClass)
    ['{70C15707-7105-47DF-AD8A-FCF9D13EBAA2}']
  end;

  NWUDPSession = interface(NSObject)
    ['{B73B02B6-86A4-41B9-90C7-8E06A93639BE}']
    procedure cancel; cdecl;
    function currentPath: NWPath; cdecl;
    function endpoint: NWEndpoint; cdecl;
    function hasBetterPath: Boolean; cdecl;
    function initWithUpgradeForSession(session: NWUDPSession): Pointer; cdecl;
    function isViable: Boolean; cdecl;
    function maximumDatagramLength: NSUInteger; cdecl;
    function resolvedEndpoint: NWEndpoint; cdecl;
    procedure setReadHandler(handler: TNWUDPSessionBlockMethod1; maxDatagrams: NSUInteger); cdecl;
    function state: NWUDPSessionState; cdecl;
    procedure tryNextResolvedEndpoint; cdecl;
    procedure writeDatagram(datagram: NSData; completionHandler: TNWUDPSessionBlockMethod2); cdecl;
    procedure writeMultipleDatagrams(datagramArray: NSArray; completionHandler: TNWUDPSessionBlockMethod2); cdecl;
  end;
  TNWUDPSession = class(TOCGenericImport<NWUDPSessionClass, NWUDPSession>) end;

  NWTLSParametersClass = interface(NSObjectClass)
    ['{01C35C05-A358-4FAB-8D7D-145A6262171E}']
  end;

  NWTLSParameters = interface(NSObject)
    ['{3424AC92-03FC-4B29-8B80-019E3A8F187C}']
    function maximumSSLProtocolVersion: NSUInteger; cdecl;
    function minimumSSLProtocolVersion: NSUInteger; cdecl;
    procedure setMaximumSSLProtocolVersion(maximumSSLProtocolVersion: NSUInteger); cdecl;
    procedure setMinimumSSLProtocolVersion(minimumSSLProtocolVersion: NSUInteger); cdecl;
    procedure setSSLCipherSuites(SSLCipherSuites: NSSet); cdecl;
    procedure setTLSSessionID(TLSSessionID: NSData); cdecl;
    function SSLCipherSuites: NSSet; cdecl;
    function TLSSessionID: NSData; cdecl;
  end;
  TNWTLSParameters = class(TOCGenericImport<NWTLSParametersClass, NWTLSParameters>) end;

function NEAppProxyErrorDomain: NSString;
function NETunnelProviderErrorDomain: NSString;
function NEVPNErrorDomain: NSString;
function NEVPNConfigurationChangeNotification: NSString;
function NEDNSProxyErrorDomain: NSString;
function NEDNSProxyConfigurationDidChangeNotification: NSString;
function NEDNSSettingsErrorDomain: NSString;
function NEDNSSettingsConfigurationDidChangeNotification: NSString;
function NEFilterProviderRemediationMapRemediationURLs: NSString;
function NEFilterProviderRemediationMapRemediationButtonTexts: NSString;
function NEFilterErrorDomain: NSString;
function NEFilterConfigurationDidChangeNotification: NSString;
function kNEHotspotHelperOptionDisplayName: NSString;
function NEHotspotConfigurationErrorDomain: NSString;
function NEVPNStatusDidChangeNotification: NSString;
function NEVPNConnectionStartOptionUsername: NSString;
function NEVPNConnectionStartOptionPassword: NSString;
function NEAppPushErrorDomain: NSErrorDomain;

const
  libNetworkExtension = '/System/Library/Frameworks/NetworkExtension.framework/NetworkExtension';

implementation

uses
  Posix.Dlfcn;

var
  NetworkExtensionModule: THandle;

function NEAppProxyErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEAppProxyErrorDomain');
end;

function NETunnelProviderErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NETunnelProviderErrorDomain');
end;

function NEVPNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNErrorDomain');
end;

function NEVPNConfigurationChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNConfigurationChangeNotification');
end;

function NEDNSProxyErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEDNSProxyErrorDomain');
end;

function NEDNSProxyConfigurationDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEDNSProxyConfigurationDidChangeNotification');
end;

function NEDNSSettingsErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEDNSSettingsErrorDomain');
end;

function NEDNSSettingsConfigurationDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEDNSSettingsConfigurationDidChangeNotification');
end;

function NEFilterProviderRemediationMapRemediationURLs: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEFilterProviderRemediationMapRemediationURLs');
end;

function NEFilterProviderRemediationMapRemediationButtonTexts: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEFilterProviderRemediationMapRemediationButtonTexts');
end;

function NEFilterErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEFilterErrorDomain');
end;

function NEFilterConfigurationDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEFilterConfigurationDidChangeNotification');
end;

function kNEHotspotHelperOptionDisplayName: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'kNEHotspotHelperOptionDisplayName');
end;

function NEHotspotConfigurationErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEHotspotConfigurationErrorDomain');
end;

function NEVPNStatusDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNStatusDidChangeNotification');
end;

function NEVPNConnectionStartOptionUsername: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNConnectionStartOptionUsername');
end;

function NEVPNConnectionStartOptionPassword: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNConnectionStartOptionPassword');
end;

function NEAppPushErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEAppPushErrorDomain');
end;

initialization
  NetworkExtensionModule := dlopen(MarshaledAString(libNetworkExtension), RTLD_LAZY);

finalization
  dlclose(NetworkExtensionModule)

end.