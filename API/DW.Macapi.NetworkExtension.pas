unit DW.Macapi.NetworkExtension;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch, Macapi.CocoaTypes, Macapi.Foundation, Macapi.Security,
  // Posix
  Posix.SysSocket;

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
  NEHotspotNetworkSecurityTypeOpen = 0;
  NEHotspotNetworkSecurityTypeWEP = 1;
  NEHotspotNetworkSecurityTypePersonal = 2;
  NEHotspotNetworkSecurityTypeEnterprise = 3;
  NEHotspotNetworkSecurityTypeUnknown = 4;
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
  NERelayManagerErrorConfigurationInvalid = 1;
  NERelayManagerErrorConfigurationDisabled = 2;
  NERelayManagerErrorConfigurationStale = 3;
  NERelayManagerErrorConfigurationCannotBeRemoved = 4;
  NEVPNStatusInvalid = 0;
  NEVPNStatusDisconnected = 1;
  NEVPNStatusConnecting = 2;
  NEVPNStatusConnected = 3;
  NEVPNStatusReasserting = 4;
  NEVPNStatusDisconnecting = 5;
  NEVPNConnectionErrorOverslept = 1;
  NEVPNConnectionErrorNoNetworkAvailable = 2;
  NEVPNConnectionErrorUnrecoverableNetworkChange = 3;
  NEVPNConnectionErrorConfigurationFailed = 4;
  NEVPNConnectionErrorServerAddressResolutionFailed = 5;
  NEVPNConnectionErrorServerNotResponding = 6;
  NEVPNConnectionErrorServerDead = 7;
  NEVPNConnectionErrorAuthenticationFailed = 8;
  NEVPNConnectionErrorClientCertificateInvalid = 9;
  NEVPNConnectionErrorClientCertificateNotYetValid = 10;
  NEVPNConnectionErrorClientCertificateExpired = 11;
  NEVPNConnectionErrorPluginFailed = 12;
  NEVPNConnectionErrorConfigurationNotFound = 13;
  NEVPNConnectionErrorPluginDisabled = 14;
  NEVPNConnectionErrorNegotiationFailed = 15;
  NEVPNConnectionErrorServerDisconnected = 16;
  NEVPNConnectionErrorServerCertificateInvalid = 17;
  NEVPNConnectionErrorServerCertificateNotYetValid = 18;
  NEVPNConnectionErrorServerCertificateExpired = 19;
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
  NEVPNIKEv2DiffieHellmanGroup32 = 32;
  NEVPNIKEv2CertificateTypeRSA = 1;
  NEVPNIKEv2CertificateTypeECDSA256 = 2;
  NEVPNIKEv2CertificateTypeECDSA384 = 3;
  NEVPNIKEv2CertificateTypeECDSA521 = 4;
  NEVPNIKEv2CertificateTypeEd25519 = 5;
  NEVPNIKEv2CertificateTypeRSAPSS = 6;
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
  NETunnelNetworkSettings = interface;
  NEPacketTunnelNetworkSettings = interface;
  NEEthernetTunnelNetworkSettings = interface;
  NEPacketTunnelProvider = interface;
  NEEthernetTunnelProvider = interface;
  NEOnDemandRule = interface;
  NEOnDemandRuleConnect = interface;
  NEOnDemandRuleDisconnect = interface;
  NEOnDemandRuleIgnore = interface;
  NEOnDemandRuleEvaluateConnection = interface;
  NEEvaluateConnectionRule = interface;
  NEPacket = interface;
  NEPacketTunnelFlow = interface;
  NERelay = interface;
  NERelayManager = interface;
  NETransparentProxyManager = interface;
  NETransparentProxyNetworkSettings = interface;
  NETransparentProxyProvider = interface;
  NEVPNConnection = interface;
  NETunnelProviderSession = interface;
  NETunnelProviderProtocol = interface;
  NEVPNProtocolIPSec = interface;
  NEVPNIKEv2SecurityAssociationParameters = interface;
  NEVPNProtocolIKEv2 = interface;
  NEPrivateLTENetwork = interface;
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

  // Network framework:
  nw_interface_t = Pointer;
  nw_parameters_t = Pointer;

  NEFilterPacketHandler = function(context: NEFilterPacketContext; &interface: nw_interface_t; direction: NETrafficDirection; packetBytes: Pointer;
    packetLength: NativeUInt): NEFilterPacketProviderVerdict of object;
  NEHotspotNetworkSecurityType = NSInteger;
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
  NERelayManagerError = NSInteger;
  NEVPNStatus = NSInteger;
  NEVPNConnectionError = NSInteger;
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
  TNEPacketTunnelProviderBlockMethod1 = procedure(error: NSError) of object;
  TNEPacketTunnelProviderBlockMethod2 = procedure of object;
  TNEPacketTunnelFlowBlockMethod1 = procedure(packets: NSArray; protocols: NSArray) of object;
  TNEPacketTunnelFlowBlockMethod2 = procedure(packets: NSArray) of object;
  TNERelayManagerBlockMethod1 = procedure(error: NSError) of object;
  TNERelayManagerBlockMethod2 = procedure(managers: NSArray; error: NSError) of object;
  TNETransparentProxyManagerBlockMethod1 = procedure(managers: NSArray; error: NSError) of object;
  TNEVPNConnectionBlockMethod1 = procedure(param1: NSError) of object;
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
    ['{8C35149A-B78F-4F89-8080-8F4DB9EB7EF9}']
  end;

  NEAppProxyFlow = interface(NSObject)
    ['{39247CBD-5EC1-4CD8-9BDC-6FEBE21AA642}']
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
    ['{0E7C2726-5E8F-4BB5-B997-32B3417A519F}']
    {class} procedure startSystemExtensionMode; cdecl;
  end;

  NEProvider = interface(NSObject)
    ['{47F25D15-F358-4255-844B-6E298AA5859A}']
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
    ['{4CBB45AD-97D1-4F7E-966B-C726B0AC46EC}']
  end;

  NETunnelProvider = interface(NEProvider)
    ['{AB3B5669-7A6E-4D93-8AD4-7CB245068732}']
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
    ['{83EA9006-C307-433D-82B5-4F08AFA2F7B0}']
  end;

  NEAppProxyProvider = interface(NETunnelProvider)
    ['{8CBC3C6A-167A-4F99-8F6F-6C92148D6A92}']
    procedure cancelProxyWithError(error: NSError); cdecl;
    function handleNewFlow(flow: NEAppProxyFlow): Boolean; cdecl;
    function handleNewUDPFlow(flow: NEAppProxyUDPFlow; initialRemoteEndpoint: NWEndpoint): Boolean; cdecl;
    procedure startProxyWithOptions(options: NSDictionary; completionHandler: TNEAppProxyProviderBlockMethod1); cdecl;
    procedure stopProxyWithReason(reason: NEProviderStopReason; completionHandler: TNEAppProxyProviderBlockMethod2); cdecl;
  end;
  TNEAppProxyProvider = class(TOCGenericImport<NEAppProxyProviderClass, NEAppProxyProvider>) end;

  NEVPNManagerClass = interface(NSObjectClass)
    ['{41BB14D5-B8A9-4DAB-B1B5-F1007FB6EE21}']
    {class} function sharedManager: NEVPNManager; cdecl;
  end;

  NEVPNManager = interface(NSObject)
    ['{05DA9F24-C3EA-416E-A590-5A111D18BF66}']
    function connection: NEVPNConnection; cdecl;
    function isEnabled: Boolean; cdecl;
    function isOnDemandEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function onDemandRules: NSArray; cdecl;
    function protocol: NEVPNProtocol; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("protocolConfiguration", macos(10.11, 10.11), ios(8.0, 9.0))
    function protocolConfiguration: NEVPNProtocol; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEVPNManagerBlockMethod1); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setOnDemandEnabled(onDemandEnabled: Boolean); cdecl;
    procedure setOnDemandRules(onDemandRules: NSArray); cdecl;
    procedure setProtocol(protocol: NEVPNProtocol); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("protocolConfiguration", macos(10.11, 10.11), ios(8.0, 9.0))
    procedure setProtocolConfiguration(protocolConfiguration: NEVPNProtocol); cdecl;
  end;
  TNEVPNManager = class(TOCGenericImport<NEVPNManagerClass, NEVPNManager>) end;

  NETunnelProviderManagerClass = interface(NEVPNManagerClass)
    ['{8FD1290C-4E61-41FF-ABE9-52D7A37E9B55}']
    {class} function forPerAppVPN: Pointer; cdecl;
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNETunnelProviderManagerBlockMethod1); cdecl;
  end;

  NETunnelProviderManager = interface(NEVPNManager)
    ['{F9F12757-BB41-4B87-A4D1-32758BBB7E40}']
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
    ['{DC684D37-C917-444D-A1D6-F3758FCEAF17}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNEAppProxyProviderManagerBlockMethod1); cdecl;
  end;

  NEAppProxyProviderManager = interface(NETunnelProviderManager)
    ['{3AE24D90-7F43-4C79-8D53-2982DA4213C4}']
  end;
  TNEAppProxyProviderManager = class(TOCGenericImport<NEAppProxyProviderManagerClass, NEAppProxyProviderManager>) end;

  NEAppProxyTCPFlowClass = interface(NEAppProxyFlowClass)
    ['{599F042F-DE93-49E8-83F4-ACD0DF818517}']
  end;

  NEAppProxyTCPFlow = interface(NEAppProxyFlow)
    ['{7384C4C1-6D48-4E15-BD8A-BF8E0199B154}']
    procedure readDataWithCompletionHandler(completionHandler: TNEAppProxyTCPFlowBlockMethod1); cdecl;
    function remoteEndpoint: NWEndpoint; cdecl;
    procedure writeData(data: NSData; withCompletionHandler: TNEAppProxyTCPFlowBlockMethod2); cdecl;
  end;
  TNEAppProxyTCPFlow = class(TOCGenericImport<NEAppProxyTCPFlowClass, NEAppProxyTCPFlow>) end;

  NEAppProxyUDPFlowClass = interface(NEAppProxyFlowClass)
    ['{7BEA9959-6B19-42DE-B0B2-71194439F9D9}']
  end;

  NEAppProxyUDPFlow = interface(NEAppProxyFlow)
    ['{3DFF0306-C957-4710-B438-D2AC17A4BDD6}']
    function localEndpoint: NWEndpoint; cdecl;
    procedure readDatagramsWithCompletionHandler(completionHandler: TNEAppProxyUDPFlowBlockMethod1); cdecl;
    procedure writeDatagrams(datagrams: NSArray; sentByEndpoints: NSArray; completionHandler: TNEAppProxyUDPFlowBlockMethod2); cdecl;
  end;
  TNEAppProxyUDPFlow = class(TOCGenericImport<NEAppProxyUDPFlowClass, NEAppProxyUDPFlow>) end;

  NEAppRuleClass = interface(NSObjectClass)
    ['{459B8DFD-871F-4C40-B340-3CB5D68A0228}']
  end;

  NEAppRule = interface(NSObject)
    ['{DD88FF0B-6D6C-43F8-9093-A56D4BE61FAC}']
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
    ['{B09830C9-1BA6-495E-AB2A-DA7BC8CE0199}']
    {class} function sharedManager: NEDNSProxyManager; cdecl;
  end;

  NEDNSProxyManager = interface(NSObject)
    ['{81697C7C-78CA-4D82-A5FC-F90D3CCCCF5D}']
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
    ['{6995B139-370A-4DE9-9553-40BBB35F9B0C}']
  end;

  NEDNSProxyProvider = interface(NEProvider)
    ['{B0E80EFB-C7D5-41EB-9599-DC71279956E5}']
    procedure cancelProxyWithError(error: NSError); cdecl;
    function handleNewFlow(flow: NEAppProxyFlow): Boolean; cdecl;
    function handleNewUDPFlow(flow: NEAppProxyUDPFlow; initialRemoteEndpoint: NWEndpoint): Boolean; cdecl;
    procedure startProxyWithOptions(options: NSDictionary; completionHandler: TNEDNSProxyProviderBlockMethod1); cdecl;
    procedure stopProxyWithReason(reason: NEProviderStopReason; completionHandler: TNEDNSProxyProviderBlockMethod2); cdecl;
    function systemDNSSettings: NSArray; cdecl;
  end;
  TNEDNSProxyProvider = class(TOCGenericImport<NEDNSProxyProviderClass, NEDNSProxyProvider>) end;

  NEProxyServerClass = interface(NSObjectClass)
    ['{21EF126B-AA92-4C1A-87DA-C000BB117326}']
  end;

  NEProxyServer = interface(NSObject)
    ['{6E7B56E5-4C1F-469F-BABD-D4A7B6F2D4B2}']
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
    ['{099E33DA-2589-485A-9588-646DA171753A}']
  end;

  NEProxySettings = interface(NSObject)
    ['{CE96D36D-90EE-46B3-BBF6-0A7AC06EEDCC}']
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
    ['{71DBB815-3F9F-4188-87D0-700E3A5AF8EE}']
  end;

  NEVPNProtocol = interface(NSObject)
    ['{DF5088B1-837B-4433-8359-C2ACE2C2E82B}']
    function disconnectOnSleep: Boolean; cdecl;
    function enforceRoutes: Boolean; cdecl;
    function excludeAPNs: Boolean; cdecl;
    function excludeCellularServices: Boolean; cdecl;
    function excludeDeviceCommunication: Boolean; cdecl;
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
    procedure setExcludeAPNs(excludeAPNs: Boolean); cdecl;
    procedure setExcludeCellularServices(excludeCellularServices: Boolean); cdecl;
    procedure setExcludeDeviceCommunication(excludeDeviceCommunication: Boolean); cdecl;
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
    ['{2C652B34-0F0E-476A-8DCA-AA1554AAA081}']
  end;

  NEDNSProxyProviderProtocol = interface(NEVPNProtocol)
    ['{9B683AAF-84DA-422F-BE1A-D579039304D5}']
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNEDNSProxyProviderProtocol = class(TOCGenericImport<NEDNSProxyProviderProtocolClass, NEDNSProxyProviderProtocol>) end;

  NEDNSSettingsClass = interface(NSObjectClass)
    ['{36E0A2CB-BCF5-48E2-99CE-A933DD9828E6}']
  end;

  NEDNSSettings = interface(NSObject)
    ['{71FEDC0E-E7B2-4E6E-B7F2-132C341F2031}']
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
    ['{B50F7B70-DA18-4A7C-90BC-20E93C505E70}']
  end;

  NEDNSOverTLSSettings = interface(NEDNSSettings)
    ['{9DA7F94B-1FD6-4635-BF92-E54E77222275}']
    function identityReference: NSData; cdecl;
    function serverName: NSString; cdecl;
    procedure setIdentityReference(identityReference: NSData); cdecl;
    procedure setServerName(serverName: NSString); cdecl;
  end;
  TNEDNSOverTLSSettings = class(TOCGenericImport<NEDNSOverTLSSettingsClass, NEDNSOverTLSSettings>) end;

  NEDNSOverHTTPSSettingsClass = interface(NEDNSSettingsClass)
    ['{0A233A40-A961-4052-8C03-DE30C276E1CC}']
  end;

  NEDNSOverHTTPSSettings = interface(NEDNSSettings)
    ['{AF79649E-D73B-4B37-B53B-50F4C135D6AF}']
    function identityReference: NSData; cdecl;
    function serverURL: NSURL; cdecl;
    procedure setIdentityReference(identityReference: NSData); cdecl;
    procedure setServerURL(serverURL: NSURL); cdecl;
  end;
  TNEDNSOverHTTPSSettings = class(TOCGenericImport<NEDNSOverHTTPSSettingsClass, NEDNSOverHTTPSSettings>) end;

  NEDNSSettingsManagerClass = interface(NSObjectClass)
    ['{EA6F0C08-4E1D-4FC2-84F8-FF957F268558}']
    {class} function sharedManager: NEDNSSettingsManager; cdecl;
  end;

  NEDNSSettingsManager = interface(NSObject)
    ['{21CC91B3-40F5-489E-A1D1-75E624E90A8C}']
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
    ['{ABB556D7-29F4-4C0D-808A-56605BC69403}']
  end;

  NENetworkRule = interface(NSObject)
    ['{40000566-1484-4434-8470-82C1D3E5D574}']
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
    ['{92A04BC9-126D-4EED-8B58-5F423A75005C}']
  end;

  NEFilterFlow = interface(NSObject)
    ['{FDDC06A7-AB14-4E58-B62F-BA3FBD65D731}']
    function direction: NETrafficDirection; cdecl;
    function identifier: NSUUID; cdecl;
    function sourceAppAuditToken: NSData; cdecl;
    function sourceAppIdentifier: NSString; cdecl;
    function sourceAppUniqueIdentifier: NSData; cdecl;
    function sourceAppVersion: NSString; cdecl;
    function sourceProcessAuditToken: NSData; cdecl;
    function URL: NSURL; cdecl;
  end;
  TNEFilterFlow = class(TOCGenericImport<NEFilterFlowClass, NEFilterFlow>) end;

  NEFilterBrowserFlowClass = interface(NEFilterFlowClass)
    ['{53068307-6D2A-4FA3-8317-668F352145B3}']
  end;

  NEFilterBrowserFlow = interface(NEFilterFlow)
    ['{197F2157-4389-4E64-8AEB-97A533A3F841}']
    function parentURL: NSURL; cdecl;
    function request: NSURLRequest; cdecl;
    function response: NSURLResponse; cdecl;
  end;
  TNEFilterBrowserFlow = class(TOCGenericImport<NEFilterBrowserFlowClass, NEFilterBrowserFlow>) end;

  NEFilterSocketFlowClass = interface(NEFilterFlowClass)
    ['{25BF059D-2C5A-4769-AABE-B710D43FCBCC}']
  end;

  NEFilterSocketFlow = interface(NEFilterFlow)
    ['{B351FBAB-DE44-4512-8B50-0C0E60035F04}']
    function localEndpoint: NWEndpoint; cdecl;
    function remoteEndpoint: NWEndpoint; cdecl;
    function remoteHostname: NSString; cdecl;
    function socketFamily: Integer; cdecl;
    function socketProtocol: Integer; cdecl;
    function socketType: Integer; cdecl;
  end;
  TNEFilterSocketFlow = class(TOCGenericImport<NEFilterSocketFlowClass, NEFilterSocketFlow>) end;

  NEFilterProviderClass = interface(NEProviderClass)
    ['{637639E7-5B08-401F-B134-0D2828ED6E6D}']
  end;

  NEFilterProvider = interface(NEProvider)
    ['{43A785A4-7F2E-4DB3-A80C-AFDD2975E4E9}']
    function filterConfiguration: NEFilterProviderConfiguration; cdecl;
    procedure handleReport(report: NEFilterReport); cdecl;
    procedure startFilterWithCompletionHandler(completionHandler: TNEFilterProviderBlockMethod1); cdecl;
    procedure stopFilterWithReason(reason: NEProviderStopReason; completionHandler: TNEFilterProviderBlockMethod2); cdecl;
  end;
  TNEFilterProvider = class(TOCGenericImport<NEFilterProviderClass, NEFilterProvider>) end;

  NEFilterVerdictClass = interface(NSObjectClass)
    ['{03CE80C6-496F-4FE2-A548-0AF2AAAC61FA}']
  end;

  NEFilterVerdict = interface(NSObject)
    ['{7E8715E4-1008-441B-A489-9DE9915550AF}']
    procedure setShouldReport(shouldReport: Boolean); cdecl;
    function shouldReport: Boolean; cdecl;
  end;
  TNEFilterVerdict = class(TOCGenericImport<NEFilterVerdictClass, NEFilterVerdict>) end;

  NEFilterNewFlowVerdictClass = interface(NEFilterVerdictClass)
    ['{5BAD61F2-83F5-4D6B-A4D9-B0EF00B105A0}']
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
    ['{567903DB-737D-49CF-9C58-9EBFF18C5915}']
    procedure setStatisticsReportFrequency(statisticsReportFrequency: NEFilterReportFrequency); cdecl;
    function statisticsReportFrequency: NEFilterReportFrequency; cdecl;
  end;
  TNEFilterNewFlowVerdict = class(TOCGenericImport<NEFilterNewFlowVerdictClass, NEFilterNewFlowVerdict>) end;

  NEFilterControlVerdictClass = interface(NEFilterNewFlowVerdictClass)
    ['{00137948-580C-40F2-B791-224D4378B648}']
    {class} function allowVerdictWithUpdateRules(updateRules: Boolean): NEFilterControlVerdict; cdecl;
    {class} function dropVerdictWithUpdateRules(updateRules: Boolean): NEFilterControlVerdict; cdecl;
    {class} function updateRules: NEFilterControlVerdict; cdecl;
  end;

  NEFilterControlVerdict = interface(NEFilterNewFlowVerdict)
    ['{78241D8E-0EAA-4AAB-A5C8-898B7E139306}']
  end;
  TNEFilterControlVerdict = class(TOCGenericImport<NEFilterControlVerdictClass, NEFilterControlVerdict>) end;

  NEFilterReportClass = interface(NSObjectClass)
    ['{BF8BC24B-35A6-401A-847C-F75BF24482F9}']
  end;

  NEFilterReport = interface(NSObject)
    ['{FEF1AFE0-F12B-4B7D-9C37-80CE5D8656F6}']
    function action: NEFilterAction; cdecl;
    function bytesInboundCount: NSUInteger; cdecl;
    function bytesOutboundCount: NSUInteger; cdecl;
    function event: NEFilterReportEvent; cdecl;
    function flow: NEFilterFlow; cdecl;
  end;
  TNEFilterReport = class(TOCGenericImport<NEFilterReportClass, NEFilterReport>) end;

  NEFilterControlProviderClass = interface(NEFilterProviderClass)
    ['{7F721794-8E35-4D97-AD7C-5CF376378BE8}']
  end;

  NEFilterControlProvider = interface(NEFilterProvider)
    ['{304B629F-810C-44FE-A499-FF15EB36C794}']
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
    ['{86F66863-BB7B-45B2-A144-93F82BDBD1F2}']
  end;

  NEFilterDataProvider = interface(NEFilterProvider)
    ['{578EE619-FAC1-4556-A5F0-DC1ED143CE52}']
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
    ['{CBD583D3-4B65-4B15-8962-DC362A1D3FFE}']
    {class} function allowVerdict: NEFilterDataVerdict; cdecl;
    {class} function dataVerdictWithPassBytes(passBytes: NSUInteger; peekBytes: NSUInteger): NEFilterDataVerdict; cdecl;
    {class} function dropVerdict: NEFilterDataVerdict; cdecl;
    {class} function needRulesVerdict: NEFilterDataVerdict; cdecl;
    {class} function pauseVerdict: NEFilterDataVerdict; cdecl;
    {class} function remediateVerdictWithRemediationURLMapKey(remediationURLMapKey: NSString;
      remediationButtonTextMapKey: NSString): NEFilterDataVerdict; cdecl;
  end;

  NEFilterDataVerdict = interface(NEFilterVerdict)
    ['{9BA40A46-CD42-491D-BA0F-CA28F4D09E92}']
    procedure setStatisticsReportFrequency(statisticsReportFrequency: NEFilterReportFrequency); cdecl;
    function statisticsReportFrequency: NEFilterReportFrequency; cdecl;
  end;
  TNEFilterDataVerdict = class(TOCGenericImport<NEFilterDataVerdictClass, NEFilterDataVerdict>) end;

  NEFilterRemediationVerdictClass = interface(NEFilterVerdictClass)
    ['{A90BBA36-BD7D-4CC9-BA60-61EFAD623679}']
    {class} function allowVerdict: NEFilterRemediationVerdict; cdecl;
    {class} function dropVerdict: NEFilterRemediationVerdict; cdecl;
    {class} function needRulesVerdict: NEFilterRemediationVerdict; cdecl;
  end;

  NEFilterRemediationVerdict = interface(NEFilterVerdict)
    ['{66026D2A-6DD2-4FDE-A9EF-0DF348B3DAB5}']
  end;
  TNEFilterRemediationVerdict = class(TOCGenericImport<NEFilterRemediationVerdictClass, NEFilterRemediationVerdict>) end;

  NEFilterManagerClass = interface(NSObjectClass)
    ['{A99F5D3B-8F37-4A9F-A5A7-F6F33268DB69}']
    {class} function sharedManager: NEFilterManager; cdecl;
  end;

  NEFilterManager = interface(NSObject)
    ['{BADF7391-62A0-4A99-8240-43C1CD48475B}']
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
    ['{B9235D22-F525-4BEF-B600-79BB8A39AD2E}']
  end;

  NEFilterPacketContext = interface(NSObject)
    ['{6830C5D9-0017-47E1-8ECF-4D3181B6D504}']
  end;
  TNEFilterPacketContext = class(TOCGenericImport<NEFilterPacketContextClass, NEFilterPacketContext>) end;

  NEFilterPacketProviderClass = interface(NEFilterProviderClass)
    ['{BB6312EE-622B-4997-9A50-A1E69B986CAA}']
  end;

  NEFilterPacketProvider = interface(NEFilterProvider)
    ['{BC2D108A-4E9C-4F92-8D3C-197359B39012}']
    procedure allowPacket(packet: NEPacket); cdecl;
    function delayCurrentPacket(context: NEFilterPacketContext): NEPacket; cdecl;
    function packetHandler: NEFilterPacketHandler; cdecl;
    procedure setPacketHandler(packetHandler: NEFilterPacketHandler); cdecl;
  end;
  TNEFilterPacketProvider = class(TOCGenericImport<NEFilterPacketProviderClass, NEFilterPacketProvider>) end;

  NEFilterProviderConfigurationClass = interface(NSObjectClass)
    ['{4E913902-32DB-4E74-B5BA-25A7E56066C7}']
  end;

  NEFilterProviderConfiguration = interface(NSObject)
    ['{03145224-443F-44D7-AA58-1784B22833A5}']
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
    ['{D85CC2C4-6619-4506-A189-CEF4D0A341E6}']
  end;

  NEFilterRule = interface(NSObject)
    ['{4EFD3D3F-0D29-401C-A96D-021385310203}']
    function action: NEFilterAction; cdecl;
    function initWithNetworkRule(networkRule: NENetworkRule; action: NEFilterAction): Pointer; cdecl;
    function networkRule: NENetworkRule; cdecl;
  end;
  TNEFilterRule = class(TOCGenericImport<NEFilterRuleClass, NEFilterRule>) end;

  NEFilterSettingsClass = interface(NSObjectClass)
    ['{5F6813F4-845D-4E11-9063-F6D90F279A8F}']
  end;

  NEFilterSettings = interface(NSObject)
    ['{30F5BF5D-4F79-4887-A212-F11608B3948B}']
    function defaultAction: NEFilterAction; cdecl;
    function initWithRules(rules: NSArray; defaultAction: NEFilterAction): Pointer; cdecl;
    function rules: NSArray; cdecl;
  end;
  TNEFilterSettings = class(TOCGenericImport<NEFilterSettingsClass, NEFilterSettings>) end;

  NEFlowMetaDataClass = interface(NSObjectClass)
    ['{314E4D1E-E2A8-4B63-9C6D-0D123B215253}']
  end;

  NEFlowMetaData = interface(NSObject)
    ['{CC38787D-2B2E-4244-826A-F0F8BFDBF596}']
    function filterFlowIdentifier: NSUUID; cdecl;
    function sourceAppAuditToken: NSData; cdecl;
    function sourceAppSigningIdentifier: NSString; cdecl;
    function sourceAppUniqueIdentifier: NSData; cdecl;
  end;
  TNEFlowMetaData = class(TOCGenericImport<NEFlowMetaDataClass, NEFlowMetaData>) end;

  NEHotspotNetworkClass = interface(NSObjectClass)
    ['{7C94A2DE-BEB5-4CBA-85FF-46A6D24661A6}']
    {class} procedure fetchCurrentWithCompletionHandler(completionHandler: TNEHotspotNetworkBlockMethod1); cdecl;
  end;

  NEHotspotNetwork = interface(NSObject)
    ['{192161BC-AD45-4919-9335-3D6D0118E9F4}']
    function BSSID: NSString; cdecl;
    function didAutoJoin: Boolean; cdecl;
    function didJustJoin: Boolean; cdecl;
    function isChosenHelper: Boolean; cdecl;
    function isSecure: Boolean; cdecl;
    function securityType: NEHotspotNetworkSecurityType; cdecl;
    procedure setConfidence(confidence: NEHotspotHelperConfidence); cdecl;
    procedure setPassword(password: NSString); cdecl;
    function signalStrength: Double; cdecl;
    function SSID: NSString; cdecl;
  end;
  TNEHotspotNetwork = class(TOCGenericImport<NEHotspotNetworkClass, NEHotspotNetwork>) end;

  NEHotspotHelperCommandClass = interface(NSObjectClass)
    ['{5067D65E-C4D6-4E50-9DC4-8CD2828EDAE9}']
  end;

  NEHotspotHelperCommand = interface(NSObject)
    ['{B247D66F-8684-4033-B1E9-C0DBE21CA74D}']
    function commandType: NEHotspotHelperCommandType; cdecl;
    function createResponse(result: NEHotspotHelperResult): NEHotspotHelperResponse; cdecl;
    function createTCPConnection(endpoint: NWEndpoint): NWTCPConnection; cdecl;
    function createUDPSession(endpoint: NWEndpoint): NWUDPSession; cdecl;
    function network: NEHotspotNetwork; cdecl;
    function networkList: NSArray; cdecl;
  end;
  TNEHotspotHelperCommand = class(TOCGenericImport<NEHotspotHelperCommandClass, NEHotspotHelperCommand>) end;

  NEHotspotHelperResponseClass = interface(NSObjectClass)
    ['{E9E755CE-5B49-4A7F-A20B-1BB02E50330A}']
  end;

  NEHotspotHelperResponse = interface(NSObject)
    ['{5863318A-81F6-4F5D-BDDD-3B0D80816A15}']
    procedure deliver; cdecl;
    procedure setNetwork(network: NEHotspotNetwork); cdecl;
    procedure setNetworkList(networkList: NSArray); cdecl;
  end;
  TNEHotspotHelperResponse = class(TOCGenericImport<NEHotspotHelperResponseClass, NEHotspotHelperResponse>) end;

  NEHotspotHelperClass = interface(NSObjectClass)
    ['{5BFABD9E-FF72-421F-AE7B-AA42B7C52556}']
    {class} function logoff(network: NEHotspotNetwork): Boolean; cdecl;
    {class} function registerWithOptions(options: NSDictionary; queue: dispatch_queue_t; handler: NEHotspotHelperHandler): Boolean; cdecl;
    {class} function supportedNetworkInterfaces: NSArray; cdecl;
  end;

  NEHotspotHelper = interface(NSObject)
    ['{B0A74B78-EBF6-496A-B5EC-9964EB46A540}']
  end;
  TNEHotspotHelper = class(TOCGenericImport<NEHotspotHelperClass, NEHotspotHelper>) end;

  NEHotspotHS20SettingsClass = interface(NSObjectClass)
    ['{4DEC440C-EDB5-44B6-AE36-C044008507D9}']
  end;

  NEHotspotHS20Settings = interface(NSObject)
    ['{FE8A3DC7-88A4-469D-BD69-F194F33AAFEB}']
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
    ['{3E422623-A4E4-4375-9C8A-429BC127E9C8}']
  end;

  NEHotspotEAPSettings = interface(NSObject)
    ['{B06CA880-82E4-4801-80AC-CC59BAB4D694}']
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
    ['{6BFDBC50-90FB-4707-B9EA-313A63BD768C}']
  end;

  NEHotspotConfiguration = interface(NSObject)
    ['{A62D3B30-103E-4F79-B4DB-9F069816A90E}']
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
    ['{A0E536C0-DE4D-4C64-99C7-31694B770733}']
    {class} function sharedManager: NEHotspotConfigurationManager; cdecl;
  end;

  NEHotspotConfigurationManager = interface(NSObject)
    ['{7D1230AB-0A3A-4234-B969-E28B3AD8D7CC}']
    procedure applyConfiguration(configuration: NEHotspotConfiguration; completionHandler: TNEHotspotConfigurationManagerBlockMethod1); cdecl;
    procedure getConfiguredSSIDsWithCompletionHandler(completionHandler: TNEHotspotConfigurationManagerBlockMethod2); cdecl;
    procedure removeConfigurationForHS20DomainName(domainName: NSString); cdecl;
    procedure removeConfigurationForSSID(SSID: NSString); cdecl;
  end;
  TNEHotspotConfigurationManager = class(TOCGenericImport<NEHotspotConfigurationManagerClass, NEHotspotConfigurationManager>) end;

  NEIPv4SettingsClass = interface(NSObjectClass)
    ['{2DB63D31-5C88-4749-A01A-E96EC3F737FD}']
    {class} function settingsWithAutomaticAddressing: Pointer; cdecl;
  end;

  NEIPv4Settings = interface(NSObject)
    ['{CBE1C17F-CE33-4B54-89A8-B464A33ED149}']
    function addresses: NSArray; cdecl;
    function excludedRoutes: NSArray; cdecl;
    function includedRoutes: NSArray; cdecl;
    function initWithAddresses(addresses: NSArray; subnetMasks: NSArray): Pointer; cdecl;
    function router: NSString; cdecl;
    procedure setExcludedRoutes(excludedRoutes: NSArray); cdecl;
    procedure setIncludedRoutes(includedRoutes: NSArray); cdecl;
    procedure setRouter(router: NSString); cdecl;
    function subnetMasks: NSArray; cdecl;
  end;
  TNEIPv4Settings = class(TOCGenericImport<NEIPv4SettingsClass, NEIPv4Settings>) end;

  NEIPv4RouteClass = interface(NSObjectClass)
    ['{83E2682F-8F2F-455C-9E0A-61291568F3A8}']
    {class} function defaultRoute: NEIPv4Route; cdecl;
  end;

  NEIPv4Route = interface(NSObject)
    ['{3936DB7E-B59D-46C0-8394-289E5EDBE527}']
    function destinationAddress: NSString; cdecl;
    function destinationSubnetMask: NSString; cdecl;
    function gatewayAddress: NSString; cdecl;
    function initWithDestinationAddress(address: NSString; subnetMask: NSString): Pointer; cdecl;
    procedure setGatewayAddress(gatewayAddress: NSString); cdecl;
  end;
  TNEIPv4Route = class(TOCGenericImport<NEIPv4RouteClass, NEIPv4Route>) end;

  NEIPv6SettingsClass = interface(NSObjectClass)
    ['{946B802A-2C59-46DB-923E-FDE39D3C314A}']
    {class} function settingsWithAutomaticAddressing: Pointer; cdecl;
    {class} function settingsWithLinkLocalAddressing: Pointer; cdecl;
  end;

  NEIPv6Settings = interface(NSObject)
    ['{06F27AAE-F976-4F24-990D-485E02F459E7}']
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
    ['{B7EF2BDF-7E6B-4460-BA72-7133E36A05B4}']
    {class} function defaultRoute: NEIPv6Route; cdecl;
  end;

  NEIPv6Route = interface(NSObject)
    ['{D282DC68-C5C5-4EF6-8196-17AFCAAA046D}']
    function destinationAddress: NSString; cdecl;
    function destinationNetworkPrefixLength: NSNumber; cdecl;
    function gatewayAddress: NSString; cdecl;
    function initWithDestinationAddress(address: NSString; networkPrefixLength: NSNumber): Pointer; cdecl;
    procedure setGatewayAddress(gatewayAddress: NSString); cdecl;
  end;
  TNEIPv6Route = class(TOCGenericImport<NEIPv6RouteClass, NEIPv6Route>) end;

  NETunnelNetworkSettingsClass = interface(NSObjectClass)
    ['{AD8D8ACC-FB7A-42A1-BF71-683C29CB1AF6}']
  end;

  NETunnelNetworkSettings = interface(NSObject)
    ['{C97AA692-63CC-41AE-90D3-3BD9CB980377}']
    function DNSSettings: NEDNSSettings; cdecl;
    function initWithTunnelRemoteAddress(address: NSString): Pointer; cdecl;
    function proxySettings: NEProxySettings; cdecl;
    procedure setDNSSettings(DNSSettings: NEDNSSettings); cdecl;
    procedure setProxySettings(proxySettings: NEProxySettings); cdecl;
    function tunnelRemoteAddress: NSString; cdecl;
  end;
  TNETunnelNetworkSettings = class(TOCGenericImport<NETunnelNetworkSettingsClass, NETunnelNetworkSettings>) end;

  NEPacketTunnelNetworkSettingsClass = interface(NETunnelNetworkSettingsClass)
    ['{4C4EF042-2B6D-45DF-AC8D-1D718E4FFC4B}']
  end;

  NEPacketTunnelNetworkSettings = interface(NETunnelNetworkSettings)
    ['{74353660-D1DE-44DF-82C5-ACFEC3EBE020}']
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

  NEEthernetTunnelNetworkSettingsClass = interface(NEPacketTunnelNetworkSettingsClass)
    ['{043D7B63-F6C0-4B68-AB9F-AB89469CB3DB}']
  end;

  NEEthernetTunnelNetworkSettings = interface(NEPacketTunnelNetworkSettings)
    ['{01664861-59DD-49E5-BFA5-78A999D667E9}']
    function ethernetAddress: NSString; cdecl;
    function initWithTunnelRemoteAddress(address: NSString; ethernetAddress: NSString; mtu: NSInteger): Pointer; cdecl;
  end;
  TNEEthernetTunnelNetworkSettings = class(TOCGenericImport<NEEthernetTunnelNetworkSettingsClass, NEEthernetTunnelNetworkSettings>) end;

  NEPacketTunnelProviderClass = interface(NETunnelProviderClass)
    ['{16D43923-F89B-4F78-87C7-26E15F8C47A2}']
  end;

  NEPacketTunnelProvider = interface(NETunnelProvider)
    ['{3B343AC0-E8DC-4037-9C91-B09AC15F9A87}']
    procedure cancelTunnelWithError(error: NSError); cdecl;
    function createTCPConnectionThroughTunnelToEndpoint(remoteEndpoint: NWEndpoint; enableTLS: Boolean; TLSParameters: NWTLSParameters;
      delegate: Pointer): NWTCPConnection; cdecl;
    function createUDPSessionThroughTunnelToEndpoint(remoteEndpoint: NWEndpoint; fromEndpoint: NWHostEndpoint): NWUDPSession; cdecl;
    function packetFlow: NEPacketTunnelFlow; cdecl;
    procedure startTunnelWithOptions(options: NSDictionary; completionHandler: TNEPacketTunnelProviderBlockMethod1); cdecl;
    procedure stopTunnelWithReason(reason: NEProviderStopReason; completionHandler: TNEPacketTunnelProviderBlockMethod2); cdecl;
  end;
  TNEPacketTunnelProvider = class(TOCGenericImport<NEPacketTunnelProviderClass, NEPacketTunnelProvider>) end;

  NEEthernetTunnelProviderClass = interface(NEPacketTunnelProviderClass)
    ['{B0CDD71D-EC4F-4B80-B05E-CDA34FF67CBC}']
  end;

  NEEthernetTunnelProvider = interface(NEPacketTunnelProvider)
    ['{0E414BC9-045C-4397-9F4E-1E3A9E5E6854}']
  end;
  TNEEthernetTunnelProvider = class(TOCGenericImport<NEEthernetTunnelProviderClass, NEEthernetTunnelProvider>) end;

  NEOnDemandRuleClass = interface(NSObjectClass)
    ['{34B53E3E-F72A-4B28-9749-805DEF81CFC1}']
  end;

  NEOnDemandRule = interface(NSObject)
    ['{7409BAAD-03C0-4B0F-BC2D-8B7107F05600}']
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
    ['{91CEBC35-897F-44E9-BDD5-FFFFE45C4905}']
  end;

  NEOnDemandRuleConnect = interface(NEOnDemandRule)
    ['{CD57A271-25AC-4B28-A8F5-BF39C83CB293}']
  end;
  TNEOnDemandRuleConnect = class(TOCGenericImport<NEOnDemandRuleConnectClass, NEOnDemandRuleConnect>) end;

  NEOnDemandRuleDisconnectClass = interface(NEOnDemandRuleClass)
    ['{E3731F2E-2EC4-47C6-BFB9-3000D14FA667}']
  end;

  NEOnDemandRuleDisconnect = interface(NEOnDemandRule)
    ['{41A78C55-6900-4E22-AFAA-8A36A74077DF}']
  end;
  TNEOnDemandRuleDisconnect = class(TOCGenericImport<NEOnDemandRuleDisconnectClass, NEOnDemandRuleDisconnect>) end;

  NEOnDemandRuleIgnoreClass = interface(NEOnDemandRuleClass)
    ['{382CA067-7DE1-433A-850E-8B3114CFD680}']
  end;

  NEOnDemandRuleIgnore = interface(NEOnDemandRule)
    ['{AAFE4AF5-B709-4F50-A43F-F7DAC999E8D0}']
  end;
  TNEOnDemandRuleIgnore = class(TOCGenericImport<NEOnDemandRuleIgnoreClass, NEOnDemandRuleIgnore>) end;

  NEOnDemandRuleEvaluateConnectionClass = interface(NEOnDemandRuleClass)
    ['{CA8320A2-A7E5-46E2-9CF8-B7ECB4A10978}']
  end;

  NEOnDemandRuleEvaluateConnection = interface(NEOnDemandRule)
    ['{8F3244C1-BBA5-41F4-89B2-8CFE9B34999B}']
    function connectionRules: NSArray; cdecl;
    procedure setConnectionRules(connectionRules: NSArray); cdecl;
  end;
  TNEOnDemandRuleEvaluateConnection = class(TOCGenericImport<NEOnDemandRuleEvaluateConnectionClass, NEOnDemandRuleEvaluateConnection>) end;

  NEEvaluateConnectionRuleClass = interface(NSObjectClass)
    ['{3D727309-91AD-46B1-BE1F-5284F7F73050}']
  end;

  NEEvaluateConnectionRule = interface(NSObject)
    ['{77FD4DEB-8B20-4035-9AA5-12E69E580252}']
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
    ['{8CA91BB2-747A-4AFD-9074-21F5EBE6C34B}']
  end;

  NEPacket = interface(NSObject)
    ['{C521EFE2-3D60-4412-B1D0-06DCEA9F4655}']
    function data: NSData; cdecl;
    function direction: NETrafficDirection; cdecl;
    function initWithData(data: NSData; protocolFamily: sa_family_t): Pointer; cdecl;
    function metadata: NEFlowMetaData; cdecl;
    function protocolFamily: sa_family_t; cdecl;
  end;
  TNEPacket = class(TOCGenericImport<NEPacketClass, NEPacket>) end;

  NEPacketTunnelFlowClass = interface(NSObjectClass)
    ['{18D2E58A-E431-41DE-BA8B-A1B8CC4AE1D2}']
  end;

  NEPacketTunnelFlow = interface(NSObject)
    ['{3BD9F6D0-E0AC-4BE3-9CE6-1AFA352D7D35}']
    procedure readPacketObjectsWithCompletionHandler(completionHandler: TNEPacketTunnelFlowBlockMethod2); cdecl;
    procedure readPacketsWithCompletionHandler(completionHandler: TNEPacketTunnelFlowBlockMethod1); cdecl;
    function writePacketObjects(packets: NSArray): Boolean; cdecl;
    function writePackets(packets: NSArray; withProtocols: NSArray): Boolean; cdecl;
  end;
  TNEPacketTunnelFlow = class(TOCGenericImport<NEPacketTunnelFlowClass, NEPacketTunnelFlow>) end;

  NERelayClass = interface(NSObjectClass)
    ['{093DF724-537D-429C-ACEE-EC534F9C4117}']
  end;

  NERelay = interface(NSObject)
    ['{2BD7E306-2B8F-4FA9-8A7A-D4FE0897C7ED}']
    function additionalHTTPHeaderFields: NSDictionary; cdecl;
    function dnsOverHTTPSURL: NSURL; cdecl;
    function HTTP2RelayURL: NSURL; cdecl;
    function HTTP3RelayURL: NSURL; cdecl;
    function identityData: NSData; cdecl;
    function identityDataPassword: NSString; cdecl;
    function rawPublicKeys: NSArray; cdecl;
    procedure setAdditionalHTTPHeaderFields(additionalHTTPHeaderFields: NSDictionary); cdecl;
    procedure setDnsOverHTTPSURL(dnsOverHTTPSURL: NSURL); cdecl;
    procedure setHTTP2RelayURL(HTTP2RelayURL: NSURL); cdecl;
    procedure setHTTP3RelayURL(HTTP3RelayURL: NSURL); cdecl;
    procedure setIdentityData(identityData: NSData); cdecl;
    procedure setIdentityDataPassword(identityDataPassword: NSString); cdecl;
    procedure setRawPublicKeys(rawPublicKeys: NSArray); cdecl;
    procedure setSyntheticDNSAnswerIPv4Prefix(syntheticDNSAnswerIPv4Prefix: NSString); cdecl;
    procedure setSyntheticDNSAnswerIPv6Prefix(syntheticDNSAnswerIPv6Prefix: NSString); cdecl;
    function syntheticDNSAnswerIPv4Prefix: NSString; cdecl;
    function syntheticDNSAnswerIPv6Prefix: NSString; cdecl;
  end;
  TNERelay = class(TOCGenericImport<NERelayClass, NERelay>) end;

  NERelayManagerClass = interface(NSObjectClass)
    ['{BB8B00B9-AC15-4F28-A3EA-CA730AD7391F}']
    {class} procedure loadAllManagersFromPreferencesWithCompletionHandler(completionHandler: TNERelayManagerBlockMethod2); cdecl;
    {class} function sharedManager: NERelayManager; cdecl;
  end;

  NERelayManager = interface(NSObject)
    ['{9F52D177-71AB-4663-8B39-645D57BABFF9}']
    function excludedDomains: NSArray; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNERelayManagerBlockMethod1); cdecl;
    function localizedDescription: NSString; cdecl;
    function matchDomains: NSArray; cdecl;
    function onDemandRules: NSArray; cdecl;
    function relays: NSArray; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNERelayManagerBlockMethod1); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNERelayManagerBlockMethod1); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setExcludedDomains(excludedDomains: NSArray); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setMatchDomains(matchDomains: NSArray); cdecl;
    procedure setOnDemandRules(onDemandRules: NSArray); cdecl;
    procedure setRelays(relays: NSArray); cdecl;
  end;
  TNERelayManager = class(TOCGenericImport<NERelayManagerClass, NERelayManager>) end;

  NETransparentProxyManagerClass = interface(NEVPNManagerClass)
    ['{54A06A58-5B56-4C60-A4A3-CAD7926821C6}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNETransparentProxyManagerBlockMethod1); cdecl;
  end;

  NETransparentProxyManager = interface(NEVPNManager)
    ['{988B6464-3062-4927-A336-5FC31938DA8F}']
  end;
  TNETransparentProxyManager = class(TOCGenericImport<NETransparentProxyManagerClass, NETransparentProxyManager>) end;

  NETransparentProxyNetworkSettingsClass = interface(NETunnelNetworkSettingsClass)
    ['{205DB368-A9DD-491D-AA02-7E0720812C8F}']
  end;

  NETransparentProxyNetworkSettings = interface(NETunnelNetworkSettings)
    ['{E2A47AC6-25FF-4BDD-9FD9-FDAD94726137}']
    function excludedNetworkRules: NSArray; cdecl;
    function includedNetworkRules: NSArray; cdecl;
    procedure setExcludedNetworkRules(excludedNetworkRules: NSArray); cdecl;
    procedure setIncludedNetworkRules(includedNetworkRules: NSArray); cdecl;
  end;
  TNETransparentProxyNetworkSettings = class(TOCGenericImport<NETransparentProxyNetworkSettingsClass, NETransparentProxyNetworkSettings>) end;

  NETransparentProxyProviderClass = interface(NEAppProxyProviderClass)
    ['{7F80024E-6EE9-403C-BA6B-4BDC20D5D19B}']
  end;

  NETransparentProxyProvider = interface(NEAppProxyProvider)
    ['{1B63DBE1-6859-4BCF-9619-2AE03D2C3908}']
  end;
  TNETransparentProxyProvider = class(TOCGenericImport<NETransparentProxyProviderClass, NETransparentProxyProvider>) end;

  NEVPNConnectionClass = interface(NSObjectClass)
    ['{8D60DBBC-840C-4DD5-B2C4-4C40F5268C86}']
  end;

  NEVPNConnection = interface(NSObject)
    ['{8E1B6A19-5C83-4EA4-B9BE-ED3E9A94B875}']
    function connectedDate: NSDate; cdecl;
    procedure fetchLastDisconnectErrorWithCompletionHandler(handler: TNEVPNConnectionBlockMethod1); cdecl;
    function manager: NEVPNManager; cdecl;
    function startVPNTunnelAndReturnError(error: PPointer): Boolean; cdecl;
    function startVPNTunnelWithOptions(options: NSDictionary; andReturnError: PPointer): Boolean; cdecl;
    function status: NEVPNStatus; cdecl;
    procedure stopVPNTunnel; cdecl;
  end;
  TNEVPNConnection = class(TOCGenericImport<NEVPNConnectionClass, NEVPNConnection>) end;

  NETunnelProviderSessionClass = interface(NEVPNConnectionClass)
    ['{1E4C26A4-2A37-4093-ACBA-203126EAA8D3}']
  end;

  NETunnelProviderSession = interface(NEVPNConnection)
    ['{6A192D30-CDB7-4957-920D-694DCA22D694}']
    function sendProviderMessage(messageData: NSData; returnError: PPointer; responseHandler: TNETunnelProviderSessionBlockMethod1): Boolean; cdecl;
    function startTunnelWithOptions(options: NSDictionary; andReturnError: PPointer): Boolean; cdecl;
    procedure stopTunnel; cdecl;
  end;
  TNETunnelProviderSession = class(TOCGenericImport<NETunnelProviderSessionClass, NETunnelProviderSession>) end;

  NETunnelProviderProtocolClass = interface(NEVPNProtocolClass)
    ['{5892CB8D-4329-4046-922A-D6B296819374}']
  end;

  NETunnelProviderProtocol = interface(NEVPNProtocol)
    ['{91730517-9515-4BE0-A3E8-121FC99C9ADB}']
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNETunnelProviderProtocol = class(TOCGenericImport<NETunnelProviderProtocolClass, NETunnelProviderProtocol>) end;

  NEVPNProtocolIPSecClass = interface(NEVPNProtocolClass)
    ['{E4781A5C-C2DD-4E4A-AB98-D3FEDBBFC724}']
  end;

  NEVPNProtocolIPSec = interface(NEVPNProtocol)
    ['{65DF42A3-629C-4835-B13E-B15EE075980C}']
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
    ['{0D003181-766A-4677-A37C-9AE07040D3B2}']
  end;

  NEVPNIKEv2SecurityAssociationParameters = interface(NSObject)
    ['{2B5FDC1D-0931-4926-937B-F7CA81AACAAF}']
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
    ['{69787ABA-8AD6-4149-AEE0-558AE627C148}']
  end;

  NEVPNProtocolIKEv2 = interface(NEVPNProtocolIPSec)
    ['{A8909117-589E-4D66-86DD-199175284DE6}']
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

  NEPrivateLTENetworkClass = interface(NSObjectClass)
    ['{176C45EB-451A-488A-A787-96DC49D464E7}']
  end;

  NEPrivateLTENetwork = interface(NSObject)
    ['{60C86365-C8E7-44A0-95DD-0A8E57C37858}']
    function mobileCountryCode: NSString; cdecl;
    function mobileNetworkCode: NSString; cdecl;
    procedure setMobileCountryCode(mobileCountryCode: NSString); cdecl;
    procedure setMobileNetworkCode(mobileNetworkCode: NSString); cdecl;
    procedure setTrackingAreaCode(trackingAreaCode: NSString); cdecl;
    function trackingAreaCode: NSString; cdecl;
  end;
  TNEPrivateLTENetwork = class(TOCGenericImport<NEPrivateLTENetworkClass, NEPrivateLTENetwork>) end;

  NEAppPushManagerClass = interface(NSObjectClass)
    ['{5E229EE9-7663-4C3C-A9BF-9AB92D0B1169}']
    {class} procedure loadAllFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod1); cdecl;
  end;

  NEAppPushManager = interface(NSObject)
    ['{E6DBA9A5-3AB6-47F5-8248-23EEEB0D13FE}']
    function delegate: Pointer; cdecl;
    function isActive: Boolean; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure loadFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    function localizedDescription: NSString; cdecl;
    function matchPrivateLTENetworks: NSArray; cdecl;
    function matchSSIDs: NSArray; cdecl;
    function providerBundleIdentifier: NSString; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure removeFromPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    procedure saveToPreferencesWithCompletionHandler(completionHandler: TNEAppPushManagerBlockMethod2); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setMatchPrivateLTENetworks(matchPrivateLTENetworks: NSArray); cdecl;
    procedure setMatchSSIDs(matchSSIDs: NSArray); cdecl;
    procedure setProviderBundleIdentifier(providerBundleIdentifier: NSString); cdecl;
    procedure setProviderConfiguration(providerConfiguration: NSDictionary); cdecl;
  end;
  TNEAppPushManager = class(TOCGenericImport<NEAppPushManagerClass, NEAppPushManager>) end;

  NEAppPushDelegate = interface(IObjectiveC)
    ['{29348EC7-10DB-473D-BD5B-F54DFE0BDB36}']
    procedure appPushManager(manager: NEAppPushManager; didReceiveIncomingCallWithUserInfo: NSDictionary); cdecl;
  end;

  NEAppPushProviderClass = interface(NEProviderClass)
    ['{33B79E0B-25C7-4CA0-BC72-D9C557D3573A}']
  end;

  NEAppPushProvider = interface(NEProvider)
    ['{19189A84-B59A-4443-B39E-06D51DAA8E10}']
    procedure handleTimerEvent; cdecl;
    function providerConfiguration: NSDictionary; cdecl;
    procedure reportIncomingCallWithUserInfo(userInfo: NSDictionary); cdecl;
    procedure reportPushToTalkMessageWithUserInfo(userInfo: NSDictionary); cdecl;
    procedure start; cdecl;
    procedure startWithCompletionHandler(completionHandler: TNEAppPushProviderBlockMethod1); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("start", ios(14.0, API_TO_BE_DEPRECATED))
    procedure stopWithReason(reason: NEProviderStopReason; completionHandler: TNEAppPushProviderBlockMethod2); cdecl;
  end;
  TNEAppPushProvider = class(TOCGenericImport<NEAppPushProviderClass, NEAppPushProvider>) end;

  NWEndpointClass = interface(NSObjectClass)
    ['{489E4FC1-FDBB-437F-9683-A72137C1C7B3}']
  end;

  NWEndpoint = interface(NSObject)
    ['{B8618C3F-843B-4C21-9565-64563BE28A4A}']
  end;
  TNWEndpoint = class(TOCGenericImport<NWEndpointClass, NWEndpoint>) end;

  NWHostEndpointClass = interface(NWEndpointClass)
    ['{97D519C7-951E-49DD-832E-E18BE49B30B9}']
    {class} function endpointWithHostname(hostname: NSString; port: NSString): Pointer; cdecl;
  end;

  NWHostEndpoint = interface(NWEndpoint)
    ['{63D3034C-EC4A-40BA-BC12-20DBA24ABE4A}']
    function hostname: NSString; cdecl;
    function port: NSString; cdecl;
  end;
  TNWHostEndpoint = class(TOCGenericImport<NWHostEndpointClass, NWHostEndpoint>) end;

  NWBonjourServiceEndpointClass = interface(NWEndpointClass)
    ['{4861354F-AAA6-48DB-9ADF-840BEB60C90F}']
    {class} function endpointWithName(name: NSString; &type: NSString; domain: NSString): Pointer; cdecl;
  end;

  NWBonjourServiceEndpoint = interface(NWEndpoint)
    ['{11CBDD16-D860-4626-9A5F-43755AD954AF}']
    function &type: NSString; cdecl;
    function domain: NSString; cdecl;
    function name: NSString; cdecl;
  end;
  TNWBonjourServiceEndpoint = class(TOCGenericImport<NWBonjourServiceEndpointClass, NWBonjourServiceEndpoint>) end;

  NWPathClass = interface(NSObjectClass)
    ['{C032853D-7DD8-4AFA-9381-8B2ABB534547}']
  end;

  NWPath = interface(NSObject)
    ['{C4BE72C6-3B99-4612-8C14-EE1282DB669C}']
    function isConstrained: Boolean; cdecl;
    function isEqualToPath(path: NWPath): Boolean; cdecl;
    function isExpensive: Boolean; cdecl;
    function status: NWPathStatus; cdecl;
  end;
  TNWPath = class(TOCGenericImport<NWPathClass, NWPath>) end;

  NWTCPConnectionClass = interface(NSObjectClass)
    ['{147AFB55-F896-4F59-B0A4-BBAE7D41754B}']
  end;

  NWTCPConnection = interface(NSObject)
    ['{A989CAF8-DF13-461B-9353-C523F5DB4FA7}']
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
    ['{9CE4773B-261D-40B1-8D17-A8B21FB47EBC}']
    procedure evaluateTrustForConnection(connection: NWTCPConnection; peerCertificateChain: NSArray; completionHandler: Pointer); cdecl;
    procedure provideIdentityForConnection(connection: NWTCPConnection; completionHandler: Pointer); cdecl;
    function shouldEvaluateTrustForConnection(connection: NWTCPConnection): Boolean; cdecl;
    function shouldProvideIdentityForConnection(connection: NWTCPConnection): Boolean; cdecl;
  end;

  NWUDPSessionClass = interface(NSObjectClass)
    ['{6DAD5442-56FF-4396-9E93-01EA480D858E}']
  end;

  NWUDPSession = interface(NSObject)
    ['{CC7ECF59-AD44-4C6A-B0F8-98391926D952}']
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
    ['{C8EED0A9-6A37-4651-8507-E0FDD3C69C19}']
  end;

  NWTLSParameters = interface(NSObject)
    ['{2323F107-460D-4679-BF99-E68E5DD290F7}']
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
function NERelayErrorDomain: NSString;
function NERelayConfigurationDidChangeNotification: NSString;
function NEVPNStatusDidChangeNotification: NSString;
function NEVPNConnectionStartOptionUsername: NSString;
function NEVPNConnectionStartOptionPassword: NSString;
function NEVPNConnectionErrorDomain: NSString;
function NEAppPushErrorDomain: NSErrorDomain;

const
  libNetworkExtension = '/System/Library/Frameworks/NetworkExtension.framework/NetworkExtension';

implementation

uses
  System.SysUtils;

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

function NERelayErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NERelayErrorDomain');
end;

function NERelayConfigurationDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NERelayConfigurationDidChangeNotification');
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

function NEVPNConnectionErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEVPNConnectionErrorDomain');
end;

function NEAppPushErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libNetworkExtension, 'NEAppPushErrorDomain');
end;

initialization
  NetworkExtensionModule := LoadLibrary(libNetworkExtension);

finalization
  if NetworkExtensionModule <> 0 then
    FreeLibrary(NetworkExtensionModule);

end.