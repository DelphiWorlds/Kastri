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

unit DW.Macapi.SystemConfiguration;

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Dispatch, Macapi.Security, Macapi.CoreServices,
  // Posix
  Posix.SysSocket, Posix.SysTypes;

const
  kSCPreferencesNotificationCommit = 1;
  kSCPreferencesNotificationApply = 2;
  kSCNetworkFlagsTransientConnection = 1;
  kSCNetworkFlagsReachable = 2;
  kSCNetworkFlagsConnectionRequired = 4;
  kSCNetworkFlagsConnectionAutomatic = 8;
  kSCNetworkFlagsInterventionRequired = 16;
  kSCNetworkFlagsIsLocalAddress = 65536;
  kSCNetworkFlagsIsDirect = 131072;
  kSCStatusOK = 0;
  kSCStatusFailed = 1001;
  kSCStatusInvalidArgument = 1002;
  kSCStatusAccessError = 1003;
  kSCStatusNoKey = 1004;
  kSCStatusKeyExists = 1005;
  kSCStatusLocked = 1006;
  kSCStatusNeedLock = 1007;
  kSCStatusNoStoreSession = 2001;
  kSCStatusNoStoreServer = 2002;
  kSCStatusNotifierActive = 2003;
  kSCStatusNoPrefsSession = 3001;
  kSCStatusPrefsBusy = 3002;
  kSCStatusNoConfigFile = 3003;
  kSCStatusNoLink = 3004;
  kSCStatusStale = 3005;
  kSCStatusMaxLink = 3006;
  kSCStatusReachabilityUnknown = 4001;
  kSCStatusConnectionNoService = 5001;
  kSCStatusConnectionIgnore = 5002;
  kSCNetworkReachabilityFlagsTransientConnection = 1;
  kSCNetworkReachabilityFlagsReachable = 2;
  kSCNetworkReachabilityFlagsConnectionRequired = 4;
  kSCNetworkReachabilityFlagsConnectionOnTraffic = 8;
  kSCNetworkReachabilityFlagsInterventionRequired = 16;
  kSCNetworkReachabilityFlagsConnectionOnDemand = 32;
  kSCNetworkReachabilityFlagsIsLocalAddress = 65536;
  kSCNetworkReachabilityFlagsIsDirect = 131072;
  kSCNetworkReachabilityFlagsIsWWAN = 262144;
  kSCNetworkReachabilityFlagsConnectionAutomatic = kSCNetworkReachabilityFlagsConnectionOnTraffic;
  kSCNetworkConnectionInvalid = -1;
  kSCNetworkConnectionDisconnected = 0;
  kSCNetworkConnectionConnecting = 1;
  kSCNetworkConnectionConnected = 2;
  kSCNetworkConnectionDisconnecting = 3;
  kSCNetworkConnectionPPPDisconnected = 0;
  kSCNetworkConnectionPPPInitializing = 1;
  kSCNetworkConnectionPPPConnectingLink = 2;
  kSCNetworkConnectionPPPDialOnTraffic = 3;
  kSCNetworkConnectionPPPNegotiatingLink = 4;
  kSCNetworkConnectionPPPAuthenticating = 5;
  kSCNetworkConnectionPPPWaitingForCallBack = 6;
  kSCNetworkConnectionPPPNegotiatingNetwork = 7;
  kSCNetworkConnectionPPPConnected = 8;
  kSCNetworkConnectionPPPTerminating = 9;
  kSCNetworkConnectionPPPDisconnectingLink = 10;
  kSCNetworkConnectionPPPHoldingLinkOff = 11;
  kSCNetworkConnectionPPPSuspended = 12;
  kSCNetworkConnectionPPPWaitingForRedial = 13;
  kSCBondStatusOK = 0;
  kSCBondStatusLinkInvalid = 1;
  kSCBondStatusNoPartner = 2;
  kSCBondStatusNotInActiveGroup = 3;
  kSCBondStatusUnknown = 999;

type
  P__SCDynamicStore = Pointer;
  PP__SCDynamicStore = ^P__SCDynamicStore;
  P__SCPreferences = Pointer;
  PP__SCPreferences = ^P__SCPreferences;
  P__SCNetworkReachability = Pointer;
  PP__SCNetworkReachability = ^P__SCNetworkReachability;
  P__SCNetworkConnection = Pointer;
  PP__SCNetworkConnection = ^P__SCNetworkConnection;
  P__SCNetworkInterface = Pointer;
  PP__SCNetworkInterface = ^P__SCNetworkInterface;
  P__SCBondStatus = Pointer;
  PP__SCBondStatus = ^P__SCBondStatus;
  P__SCNetworkProtocol = Pointer;
  PP__SCNetworkProtocol = ^P__SCNetworkProtocol;
  P__SCNetworkService = Pointer;
  PP__SCNetworkService = ^P__SCNetworkService;
  P__SCNetworkSet = Pointer;
  PP__SCNetworkSet = ^P__SCNetworkSet;
  PSCDynamicStoreContext = ^SCDynamicStoreContext;
  PSCPreferencesContext = ^SCPreferencesContext;
  PSCNetworkReachabilityContext = ^SCNetworkReachabilityContext;
  PSCNetworkConnectionContext = ^SCNetworkConnectionContext;

  SCDynamicStoreRef = Pointer;
  PSCDynamicStoreRef = ^SCDynamicStoreRef;

  SCDynamicStoreContext = record
    version: CFIndex;
    info: Pointer;
    retain: function(info: Pointer): Pointer; cdecl;
    release: procedure(info: Pointer); cdecl;
    copyDescription: function(info: Pointer): CFStringRef; cdecl;
  end;

  SCDynamicStoreCallBack = procedure(store: SCDynamicStoreRef; changedKeys: CFArrayRef; info: Pointer); cdecl;
  SCPreferencesRef = Pointer;
  PSCPreferencesRef = ^SCPreferencesRef;
  SCPreferencesNotification = NSInteger;

  SCPreferencesContext = record
    version: CFIndex;
    info: Pointer;
    retain: function(info: Pointer): Pointer; cdecl;
    release: procedure(info: Pointer); cdecl;
    copyDescription: function(info: Pointer): CFStringRef; cdecl;
  end;

  SCPreferencesCallBack = procedure(prefs: SCPreferencesRef; notificationType: SCPreferencesNotification; info: Pointer); cdecl;
  SCNetworkConnectionFlags = UInt32;
  PSCNetworkConnectionFlags = ^SCNetworkConnectionFlags;
  SCNetworkReachabilityRef = Pointer;
  PSCNetworkReachabilityRef = ^SCNetworkReachabilityRef;

  SCNetworkReachabilityContext = record
    version: CFIndex;
    info: Pointer;
    retain: function(info: Pointer): Pointer; cdecl;
    release: procedure(info: Pointer); cdecl;
    copyDescription: function(info: Pointer): CFStringRef; cdecl;
  end;

  PSCNetworkReachabilityFlags = ^SCNetworkReachabilityFlags;
  SCNetworkReachabilityFlags = NSInteger;

  SCNetworkReachabilityCallBack = procedure(target: SCNetworkReachabilityRef; flags: SCNetworkReachabilityFlags; info: Pointer); cdecl;
  SCNetworkConnectionRef = Pointer;
  PSCNetworkConnectionRef = ^SCNetworkConnectionRef;

  SCNetworkConnectionContext = record
    version: CFIndex;
    info: Pointer;
    retain: function(info: Pointer): Pointer; cdecl;
    release: procedure(info: Pointer); cdecl;
    copyDescription: function(info: Pointer): CFStringRef; cdecl;
  end;

  SCNetworkConnectionStatus = NSInteger;
  SCNetworkConnectionPPPStatus = NSInteger;

  SCNetworkConnectionCallBack = procedure(connection: SCNetworkConnectionRef; status: SCNetworkConnectionStatus; info: Pointer); cdecl;
  SCNetworkInterfaceRef = Pointer;
  PSCNetworkInterfaceRef = ^SCNetworkInterfaceRef;
  SCBondInterfaceRef = Pointer;
  PSCBondInterfaceRef = ^SCBondInterfaceRef;
  SCBondStatusRef = Pointer;
  PSCBondStatusRef = ^SCBondStatusRef;
  SCVLANInterfaceRef = Pointer;
  PSCVLANInterfaceRef = ^SCVLANInterfaceRef;
  SCNetworkProtocolRef = Pointer;
  PSCNetworkProtocolRef = ^SCNetworkProtocolRef;
  SCNetworkServiceRef = Pointer;
  PSCNetworkServiceRef = ^SCNetworkServiceRef;
  SCNetworkSetRef = Pointer;
  PSCNetworkSetRef = ^SCNetworkSetRef;

function kSCDynamicStoreUseSessionKeys: CFStringRef;
function kSCResvLink: CFStringRef;
function kSCResvInactive: CFStringRef;
function kSCPropInterfaceName: CFStringRef;
function kSCPropMACAddress: CFStringRef;
function kSCPropUserDefinedName: CFStringRef;
function kSCPropVersion: CFStringRef;
function kSCPrefCurrentSet: CFStringRef;
function kSCPrefNetworkServices: CFStringRef;
function kSCPrefSets: CFStringRef;
function kSCPrefSystem: CFStringRef;
function kSCCompNetwork: CFStringRef;
function kSCCompService: CFStringRef;
function kSCCompGlobal: CFStringRef;
function kSCCompHostNames: CFStringRef;
function kSCCompInterface: CFStringRef;
function kSCCompSystem: CFStringRef;
function kSCCompUsers: CFStringRef;
function kSCCompAnyRegex: CFStringRef;
function kSCEntNetAirPort: CFStringRef;
function kSCEntNetDHCP: CFStringRef;
function kSCEntNetDNS: CFStringRef;
function kSCEntNetEthernet: CFStringRef;
function kSCEntNetFireWire: CFStringRef;
function kSCEntNetInterface: CFStringRef;
function kSCEntNetIPSec: CFStringRef;
function kSCEntNetIPv4: CFStringRef;
function kSCEntNetIPv6: CFStringRef;
function kSCEntNetL2TP: CFStringRef;
function kSCEntNetLink: CFStringRef;
function kSCEntNetModem: CFStringRef;
function kSCEntNetPPP: CFStringRef;
function kSCEntNetPPPoE: CFStringRef;
function kSCEntNetPPPSerial: CFStringRef;
function kSCEntNetPPTP: CFStringRef;
function kSCEntNetProxies: CFStringRef;
function kSCEntNetSMB: CFStringRef;
function kSCEntNet6to4: CFStringRef;
function kSCPropNetOverridePrimary: CFStringRef;
function kSCPropNetServiceOrder: CFStringRef;
function kSCPropNetPPPOverridePrimary: CFStringRef;
function kSCPropNetInterfaces: CFStringRef;
function kSCPropNetLocalHostName: CFStringRef;
function kSCPropNetAirPortAllowNetCreation: CFStringRef;
function kSCPropNetAirPortAuthPassword: CFStringRef;
function kSCPropNetAirPortAuthPasswordEncryption: CFStringRef;
function kSCPropNetAirPortJoinMode: CFStringRef;
function kSCPropNetAirPortPowerEnabled: CFStringRef;
function kSCPropNetAirPortPreferredNetwork: CFStringRef;
function kSCPropNetAirPortSavePasswords: CFStringRef;
function kSCValNetAirPortJoinModeAutomatic: CFStringRef;
function kSCValNetAirPortJoinModePreferred: CFStringRef;
function kSCValNetAirPortJoinModeRanked: CFStringRef;
function kSCValNetAirPortJoinModeRecent: CFStringRef;
function kSCValNetAirPortJoinModeStrongest: CFStringRef;
function kSCValNetAirPortAuthPasswordEncryptionKeychain: CFStringRef;
function kSCPropNetDNSDomainName: CFStringRef;
function kSCPropNetDNSOptions: CFStringRef;
function kSCPropNetDNSSearchDomains: CFStringRef;
function kSCPropNetDNSSearchOrder: CFStringRef;
function kSCPropNetDNSServerAddresses: CFStringRef;
function kSCPropNetDNSServerPort: CFStringRef;
function kSCPropNetDNSServerTimeout: CFStringRef;
function kSCPropNetDNSSortList: CFStringRef;
function kSCPropNetDNSSupplementalMatchDomains: CFStringRef;
function kSCPropNetDNSSupplementalMatchOrders: CFStringRef;
function kSCPropNetEthernetMediaSubType: CFStringRef;
function kSCPropNetEthernetMediaOptions: CFStringRef;
function kSCPropNetEthernetMTU: CFStringRef;
function kSCPropNetInterfaceDeviceName: CFStringRef;
function kSCPropNetInterfaceHardware: CFStringRef;
function kSCPropNetInterfaceType: CFStringRef;
function kSCPropNetInterfaceSubType: CFStringRef;
function kSCPropNetInterfaceSupportsModemOnHold: CFStringRef;
function kSCValNetInterfaceTypeEthernet: CFStringRef;
function kSCValNetInterfaceTypeFireWire: CFStringRef;
function kSCValNetInterfaceTypePPP: CFStringRef;
function kSCValNetInterfaceType6to4: CFStringRef;
function kSCValNetInterfaceTypeIPSec: CFStringRef;
function kSCValNetInterfaceSubTypePPPoE: CFStringRef;
function kSCValNetInterfaceSubTypePPPSerial: CFStringRef;
function kSCValNetInterfaceSubTypePPTP: CFStringRef;
function kSCValNetInterfaceSubTypeL2TP: CFStringRef;
function kSCPropNetIPSecAuthenticationMethod: CFStringRef;
function kSCPropNetIPSecLocalCertificate: CFStringRef;
function kSCPropNetIPSecLocalIdentifier: CFStringRef;
function kSCPropNetIPSecLocalIdentifierType: CFStringRef;
function kSCPropNetIPSecSharedSecret: CFStringRef;
function kSCPropNetIPSecSharedSecretEncryption: CFStringRef;
function kSCPropNetIPSecConnectTime: CFStringRef;
function kSCPropNetIPSecRemoteAddress: CFStringRef;
function kSCPropNetIPSecStatus: CFStringRef;
function kSCPropNetIPSecXAuthEnabled: CFStringRef;
function kSCPropNetIPSecXAuthName: CFStringRef;
function kSCPropNetIPSecXAuthPassword: CFStringRef;
function kSCPropNetIPSecXAuthPasswordEncryption: CFStringRef;
function kSCValNetIPSecAuthenticationMethodSharedSecret: CFStringRef;
function kSCValNetIPSecAuthenticationMethodCertificate: CFStringRef;
function kSCValNetIPSecAuthenticationMethodHybrid: CFStringRef;
function kSCValNetIPSecLocalIdentifierTypeKeyID: CFStringRef;
function kSCValNetIPSecSharedSecretEncryptionKeychain: CFStringRef;
function kSCValNetIPSecXAuthPasswordEncryptionKeychain: CFStringRef;
function kSCValNetIPSecXAuthPasswordEncryptionPrompt: CFStringRef;
function kSCPropNetIPv4Addresses: CFStringRef;
function kSCPropNetIPv4ConfigMethod: CFStringRef;
function kSCPropNetIPv4DHCPClientID: CFStringRef;
function kSCPropNetIPv4Router: CFStringRef;
function kSCPropNetIPv4SubnetMasks: CFStringRef;
function kSCPropNetIPv4DestAddresses: CFStringRef;
function kSCPropNetIPv4BroadcastAddresses: CFStringRef;
function kSCValNetIPv4ConfigMethodAutomatic: CFStringRef;
function kSCValNetIPv4ConfigMethodBOOTP: CFStringRef;
function kSCValNetIPv4ConfigMethodDHCP: CFStringRef;
function kSCValNetIPv4ConfigMethodINFORM: CFStringRef;
function kSCValNetIPv4ConfigMethodLinkLocal: CFStringRef;
function kSCValNetIPv4ConfigMethodManual: CFStringRef;
function kSCValNetIPv4ConfigMethodPPP: CFStringRef;
function kSCPropNetIPv6Addresses: CFStringRef;
function kSCPropNetIPv6ConfigMethod: CFStringRef;
function kSCPropNetIPv6DestAddresses: CFStringRef;
function kSCPropNetIPv6Flags: CFStringRef;
function kSCPropNetIPv6PrefixLength: CFStringRef;
function kSCPropNetIPv6Router: CFStringRef;
function kSCValNetIPv6ConfigMethodAutomatic: CFStringRef;
function kSCValNetIPv6ConfigMethodLinkLocal: CFStringRef;
function kSCValNetIPv6ConfigMethodManual: CFStringRef;
function kSCValNetIPv6ConfigMethodRouterAdvertisement: CFStringRef;
function kSCValNetIPv6ConfigMethod6to4: CFStringRef;
function kSCPropNet6to4Relay: CFStringRef;
function kSCPropNetLinkActive: CFStringRef;
function kSCPropNetLinkDetaching: CFStringRef;
function kSCPropNetModemAccessPointName: CFStringRef;
function kSCPropNetModemConnectionPersonality: CFStringRef;
function kSCPropNetModemConnectionScript: CFStringRef;
function kSCPropNetModemConnectSpeed: CFStringRef;
function kSCPropNetModemDataCompression: CFStringRef;
function kSCPropNetModemDeviceContextID: CFStringRef;
function kSCPropNetModemDeviceModel: CFStringRef;
function kSCPropNetModemDeviceVendor: CFStringRef;
function kSCPropNetModemDialMode: CFStringRef;
function kSCPropNetModemErrorCorrection: CFStringRef;
function kSCPropNetModemHoldCallWaitingAudibleAlert: CFStringRef;
function kSCPropNetModemHoldDisconnectOnAnswer: CFStringRef;
function kSCPropNetModemHoldEnabled: CFStringRef;
function kSCPropNetModemHoldReminder: CFStringRef;
function kSCPropNetModemHoldReminderTime: CFStringRef;
function kSCPropNetModemNote: CFStringRef;
function kSCPropNetModemPulseDial: CFStringRef;
function kSCPropNetModemSpeaker: CFStringRef;
function kSCPropNetModemSpeed: CFStringRef;
function kSCValNetModemDialModeIgnoreDialTone: CFStringRef;
function kSCValNetModemDialModeManual: CFStringRef;
function kSCValNetModemDialModeWaitForDialTone: CFStringRef;
function kSCPropNetPPPACSPEnabled: CFStringRef;
function kSCPropNetPPPConnectTime: CFStringRef;
function kSCPropNetPPPDeviceLastCause: CFStringRef;
function kSCPropNetPPPDialOnDemand: CFStringRef;
function kSCPropNetPPPDisconnectOnFastUserSwitch: CFStringRef;
function kSCPropNetPPPDisconnectOnIdle: CFStringRef;
function kSCPropNetPPPDisconnectOnIdleTimer: CFStringRef;
function kSCPropNetPPPDisconnectOnLogout: CFStringRef;
function kSCPropNetPPPDisconnectOnSleep: CFStringRef;
function kSCPropNetPPPDisconnectTime: CFStringRef;
function kSCPropNetPPPIdleReminder: CFStringRef;
function kSCPropNetPPPIdleReminderTimer: CFStringRef;
function kSCPropNetPPPLastCause: CFStringRef;
function kSCPropNetPPPLogfile: CFStringRef;
function kSCPropNetPPPPlugins: CFStringRef;
function kSCPropNetPPPRetryConnectTime: CFStringRef;
function kSCPropNetPPPSessionTimer: CFStringRef;
function kSCPropNetPPPStatus: CFStringRef;
function kSCPropNetPPPUseSessionTimer: CFStringRef;
function kSCPropNetPPPVerboseLogging: CFStringRef;
function kSCPropNetPPPAuthEAPPlugins: CFStringRef;
function kSCPropNetPPPAuthName: CFStringRef;
function kSCPropNetPPPAuthPassword: CFStringRef;
function kSCPropNetPPPAuthPasswordEncryption: CFStringRef;
function kSCPropNetPPPAuthPrompt: CFStringRef;
function kSCPropNetPPPAuthProtocol: CFStringRef;
function kSCValNetPPPAuthPasswordEncryptionKeychain: CFStringRef;
function kSCValNetPPPAuthPasswordEncryptionToken: CFStringRef;
function kSCValNetPPPAuthPromptBefore: CFStringRef;
function kSCValNetPPPAuthPromptAfter: CFStringRef;
function kSCValNetPPPAuthProtocolCHAP: CFStringRef;
function kSCValNetPPPAuthProtocolEAP: CFStringRef;
function kSCValNetPPPAuthProtocolMSCHAP1: CFStringRef;
function kSCValNetPPPAuthProtocolMSCHAP2: CFStringRef;
function kSCValNetPPPAuthProtocolPAP: CFStringRef;
function kSCPropNetPPPCommAlternateRemoteAddress: CFStringRef;
function kSCPropNetPPPCommConnectDelay: CFStringRef;
function kSCPropNetPPPCommDisplayTerminalWindow: CFStringRef;
function kSCPropNetPPPCommRedialCount: CFStringRef;
function kSCPropNetPPPCommRedialEnabled: CFStringRef;
function kSCPropNetPPPCommRedialInterval: CFStringRef;
function kSCPropNetPPPCommRemoteAddress: CFStringRef;
function kSCPropNetPPPCommTerminalScript: CFStringRef;
function kSCPropNetPPPCommUseTerminalScript: CFStringRef;
function kSCPropNetPPPCCPEnabled: CFStringRef;
function kSCPropNetPPPCCPMPPE40Enabled: CFStringRef;
function kSCPropNetPPPCCPMPPE128Enabled: CFStringRef;
function kSCPropNetPPPIPCPCompressionVJ: CFStringRef;
function kSCPropNetPPPIPCPUsePeerDNS: CFStringRef;
function kSCPropNetPPPLCPEchoEnabled: CFStringRef;
function kSCPropNetPPPLCPEchoFailure: CFStringRef;
function kSCPropNetPPPLCPEchoInterval: CFStringRef;
function kSCPropNetPPPLCPCompressionACField: CFStringRef;
function kSCPropNetPPPLCPCompressionPField: CFStringRef;
function kSCPropNetPPPLCPMRU: CFStringRef;
function kSCPropNetPPPLCPMTU: CFStringRef;
function kSCPropNetPPPLCPReceiveACCM: CFStringRef;
function kSCPropNetPPPLCPTransmitACCM: CFStringRef;
function kSCPropNetL2TPIPSecSharedSecret: CFStringRef;
function kSCPropNetL2TPIPSecSharedSecretEncryption: CFStringRef;
function kSCPropNetL2TPTransport: CFStringRef;
function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: CFStringRef;
function kSCValNetL2TPTransportIP: CFStringRef;
function kSCValNetL2TPTransportIPSec: CFStringRef;
function kSCPropNetProxiesExceptionsList: CFStringRef;
function kSCPropNetProxiesExcludeSimpleHostnames: CFStringRef;
function kSCPropNetProxiesFTPEnable: CFStringRef;
function kSCPropNetProxiesFTPPassive: CFStringRef;
function kSCPropNetProxiesFTPPort: CFStringRef;
function kSCPropNetProxiesFTPProxy: CFStringRef;
function kSCPropNetProxiesGopherEnable: CFStringRef;
function kSCPropNetProxiesGopherPort: CFStringRef;
function kSCPropNetProxiesGopherProxy: CFStringRef;
function kSCPropNetProxiesHTTPEnable: CFStringRef;
function kSCPropNetProxiesHTTPPort: CFStringRef;
function kSCPropNetProxiesHTTPProxy: CFStringRef;
function kSCPropNetProxiesHTTPSEnable: CFStringRef;
function kSCPropNetProxiesHTTPSPort: CFStringRef;
function kSCPropNetProxiesHTTPSProxy: CFStringRef;
function kSCPropNetProxiesRTSPEnable: CFStringRef;
function kSCPropNetProxiesRTSPPort: CFStringRef;
function kSCPropNetProxiesRTSPProxy: CFStringRef;
function kSCPropNetProxiesSOCKSEnable: CFStringRef;
function kSCPropNetProxiesSOCKSPort: CFStringRef;
function kSCPropNetProxiesSOCKSProxy: CFStringRef;
function kSCPropNetProxiesProxyAutoConfigEnable: CFStringRef;
function kSCPropNetProxiesProxyAutoConfigJavaScript: CFStringRef;
function kSCPropNetProxiesProxyAutoConfigURLString: CFStringRef;
function kSCPropNetProxiesProxyAutoDiscoveryEnable: CFStringRef;
function kSCPropNetSMBNetBIOSName: CFStringRef;
function kSCPropNetSMBNetBIOSNodeType: CFStringRef;
function kSCPropNetSMBNetBIOSScope: CFStringRef;
function kSCPropNetSMBWINSAddresses: CFStringRef;
function kSCPropNetSMBWorkgroup: CFStringRef;
function kSCValNetSMBNetBIOSNodeTypeBroadcast: CFStringRef;
function kSCValNetSMBNetBIOSNodeTypePeer: CFStringRef;
function kSCValNetSMBNetBIOSNodeTypeMixed: CFStringRef;
function kSCValNetSMBNetBIOSNodeTypeHybrid: CFStringRef;
function kSCEntUsersConsoleUser: CFStringRef;
function kSCPropSystemComputerName: CFStringRef;
function kSCPropSystemComputerNameEncoding: CFStringRef;
function kSCDynamicStoreDomainFile: CFStringRef;
function kSCDynamicStoreDomainPlugin: CFStringRef;
function kSCDynamicStoreDomainSetup: CFStringRef;
function kSCDynamicStoreDomainState: CFStringRef;
function kSCDynamicStoreDomainPrefs: CFStringRef;
function kSCDynamicStorePropSetupCurrentSet: CFStringRef;
function kSCDynamicStorePropSetupLastUpdated: CFStringRef;
function kSCDynamicStorePropNetInterfaces: CFStringRef;
function kSCDynamicStorePropNetPrimaryInterface: CFStringRef;
function kSCDynamicStorePropNetPrimaryService: CFStringRef;
function kSCDynamicStorePropNetServiceIDs: CFStringRef;
function kSCPropUsersConsoleUserName: CFStringRef;
function kSCPropUsersConsoleUserUID: CFStringRef;
function kSCPropUsersConsoleUserGID: CFStringRef;
function kCFErrorDomainSystemConfiguration: CFStringRef;
function kSCNetworkInterfaceType6to4: CFStringRef;
function kSCNetworkInterfaceTypeBluetooth: CFStringRef;
function kSCNetworkInterfaceTypeBond: CFStringRef;
function kSCNetworkInterfaceTypeEthernet: CFStringRef;
function kSCNetworkInterfaceTypeFireWire: CFStringRef;
function kSCNetworkInterfaceTypeIEEE80211: CFStringRef;
function kSCNetworkInterfaceTypeIPSec: CFStringRef;
function kSCNetworkInterfaceTypeIrDA: CFStringRef;
function kSCNetworkInterfaceTypeL2TP: CFStringRef;
function kSCNetworkInterfaceTypeModem: CFStringRef;
function kSCNetworkInterfaceTypePPP: CFStringRef;
function kSCNetworkInterfaceTypePPTP: CFStringRef;
function kSCNetworkInterfaceTypeSerial: CFStringRef;
function kSCNetworkInterfaceTypeVLAN: CFStringRef;
function kSCNetworkInterfaceTypeWWAN: CFStringRef;
function kSCNetworkInterfaceTypeIPv4: CFStringRef;
function kSCNetworkInterfaceIPv4: SCNetworkInterfaceRef;
function kSCBondStatusDeviceAggregationStatus: CFStringRef;
function kSCBondStatusDeviceCollecting: CFStringRef;
function kSCBondStatusDeviceDistributing: CFStringRef;
function kSCNetworkProtocolTypeDNS: CFStringRef;
function kSCNetworkProtocolTypeIPv4: CFStringRef;
function kSCNetworkProtocolTypeIPv6: CFStringRef;
function kSCNetworkProtocolTypeProxies: CFStringRef;
function kSCNetworkProtocolTypeSMB: CFStringRef;
function kCNNetworkInfoKeySSIDData: CFStringRef;
function kCNNetworkInfoKeySSID: CFStringRef;
function kCNNetworkInfoKeyBSSID: CFStringRef;

const
  libSystemConfiguration = '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';

function SCDynamicStoreGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreGetTypeID';

function SCDynamicStoreCreate(allocator: CFAllocatorRef; name: CFStringRef; callout: SCDynamicStoreCallBack;
  context: PSCDynamicStoreContext): SCDynamicStoreRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCreate';

function SCDynamicStoreCreateWithOptions(allocator: CFAllocatorRef; name: CFStringRef; storeOptions: CFDictionaryRef; callout: SCDynamicStoreCallBack;
  context: PSCDynamicStoreContext): SCDynamicStoreRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCreateWithOptions';

function SCDynamicStoreCreateRunLoopSource(allocator: CFAllocatorRef; store: SCDynamicStoreRef; order: CFIndex): CFRunLoopSourceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCreateRunLoopSource';

function SCDynamicStoreSetDispatchQueue(store: SCDynamicStoreRef; queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreSetDispatchQueue';

function SCDynamicStoreCopyKeyList(store: SCDynamicStoreRef; pattern: CFStringRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyKeyList';

function SCDynamicStoreAddValue(store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreAddValue';

function SCDynamicStoreAddTemporaryValue(store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreAddTemporaryValue';

function SCDynamicStoreCopyValue(store: SCDynamicStoreRef; key: CFStringRef): CFPropertyListRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyValue';

function SCDynamicStoreCopyMultiple(store: SCDynamicStoreRef; keys: CFArrayRef; patterns: CFArrayRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyMultiple';

function SCDynamicStoreSetValue(store: SCDynamicStoreRef; key: CFStringRef; value: CFPropertyListRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreSetValue';

function SCDynamicStoreSetMultiple(store: SCDynamicStoreRef; keysToSet: CFDictionaryRef; keysToRemove: CFArrayRef;
  keysToNotify: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreSetMultiple';

function SCDynamicStoreRemoveValue(store: SCDynamicStoreRef; key: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreRemoveValue';

function SCDynamicStoreNotifyValue(store: SCDynamicStoreRef; key: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreNotifyValue';

function SCDynamicStoreSetNotificationKeys(store: SCDynamicStoreRef; keys: CFArrayRef; patterns: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreSetNotificationKeys';

function SCDynamicStoreCopyNotifiedKeys(store: SCDynamicStoreRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyNotifiedKeys';

function SCPreferencesGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesGetTypeID';

function SCPreferencesCreate(allocator: CFAllocatorRef; name: CFStringRef; prefsID: CFStringRef): SCPreferencesRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesCreate';

function SCPreferencesCreateWithAuthorization(allocator: CFAllocatorRef; name: CFStringRef; prefsID: CFStringRef;
  authorization: AuthorizationRef): SCPreferencesRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesCreateWithAuthorization';

function SCPreferencesLock(prefs: SCPreferencesRef; wait: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesLock';

function SCPreferencesCommitChanges(prefs: SCPreferencesRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesCommitChanges';

function SCPreferencesApplyChanges(prefs: SCPreferencesRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesApplyChanges';

function SCPreferencesUnlock(prefs: SCPreferencesRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesUnlock';

function SCPreferencesGetSignature(prefs: SCPreferencesRef): CFDataRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesGetSignature';

function SCPreferencesCopyKeyList(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesCopyKeyList';

function SCPreferencesGetValue(prefs: SCPreferencesRef; key: CFStringRef): CFPropertyListRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesGetValue';

function SCPreferencesAddValue(prefs: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesAddValue';

function SCPreferencesSetValue(prefs: SCPreferencesRef; key: CFStringRef; value: CFPropertyListRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetValue';

function SCPreferencesRemoveValue(prefs: SCPreferencesRef; key: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesRemoveValue';

function SCPreferencesSetCallback(prefs: SCPreferencesRef; callout: SCPreferencesCallBack; context: PSCPreferencesContext): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetCallback';

function SCPreferencesScheduleWithRunLoop(prefs: SCPreferencesRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesScheduleWithRunLoop';

function SCPreferencesUnscheduleFromRunLoop(prefs: SCPreferencesRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesUnscheduleFromRunLoop';

function SCPreferencesSetDispatchQueue(prefs: SCPreferencesRef; queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetDispatchQueue';

procedure SCPreferencesSynchronize(prefs: SCPreferencesRef); cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSynchronize';

function SCDynamicStoreCopyDHCPInfo(store: SCDynamicStoreRef; serviceID: CFStringRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyDHCPInfo';

function DHCPInfoGetOptionData(info: CFDictionaryRef; code: UInt8): CFDataRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetOptionData';

function DHCPInfoGetLeaseStartTime(info: CFDictionaryRef): CFDateRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetLeaseStartTime';

function DHCPInfoGetLeaseExpirationTime(info: CFDictionaryRef): CFDateRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetLeaseExpirationTime';

function SCPreferencesPathCreateUniqueChild(prefs: SCPreferencesRef; prefix: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathCreateUniqueChild';

function SCPreferencesPathGetValue(prefs: SCPreferencesRef; path: CFStringRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathGetValue';

function SCPreferencesPathGetLink(prefs: SCPreferencesRef; path: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathGetLink';

function SCPreferencesPathSetValue(prefs: SCPreferencesRef; path: CFStringRef; value: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathSetValue';

function SCPreferencesPathSetLink(prefs: SCPreferencesRef; path: CFStringRef; link: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathSetLink';

function SCPreferencesPathRemoveValue(prefs: SCPreferencesRef; path: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesPathRemoveValue';

function SCNetworkCheckReachabilityByAddress(address: Psockaddr; addrlen: socklen_t; flags: PSCNetworkConnectionFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkCheckReachabilityByAddress';

function SCNetworkCheckReachabilityByName(nodename: PAnsiChar; flags: PSCNetworkConnectionFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkCheckReachabilityByName';

function SCNetworkInterfaceRefreshConfiguration(ifName: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceRefreshConfiguration';

function SCDynamicStoreKeyCreate(allocator: CFAllocatorRef; fmt: CFStringRef): CFStringRef varargs; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreate';

function SCDynamicStoreKeyCreateNetworkGlobalEntity(allocator: CFAllocatorRef; domain: CFStringRef; entity: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateNetworkGlobalEntity';

function SCDynamicStoreKeyCreateNetworkInterface(allocator: CFAllocatorRef; domain: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateNetworkInterface';

function SCDynamicStoreKeyCreateNetworkInterfaceEntity(allocator: CFAllocatorRef; domain: CFStringRef; ifname: CFStringRef;
  entity: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateNetworkInterfaceEntity';

function SCDynamicStoreKeyCreateNetworkServiceEntity(allocator: CFAllocatorRef; domain: CFStringRef; serviceID: CFStringRef;
  entity: CFStringRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateNetworkServiceEntity';

function SCDynamicStoreKeyCreateComputerName(allocator: CFAllocatorRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateComputerName';

function SCDynamicStoreKeyCreateConsoleUser(allocator: CFAllocatorRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateConsoleUser';

function SCDynamicStoreKeyCreateHostNames(allocator: CFAllocatorRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateHostNames';

function SCDynamicStoreKeyCreateLocation(allocator: CFAllocatorRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateLocation';

function SCDynamicStoreKeyCreateProxies(allocator: CFAllocatorRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreKeyCreateProxies';

function SCDynamicStoreCopyComputerName(store: SCDynamicStoreRef; nameEncoding: PCFStringEncoding): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyComputerName';

function SCDynamicStoreCopyConsoleUser(store: SCDynamicStoreRef; uid: Puid_t; gid: Pgid_t): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyConsoleUser';

function SCDynamicStoreCopyLocalHostName(store: SCDynamicStoreRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyLocalHostName';

function SCDynamicStoreCopyLocation(store: SCDynamicStoreRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyLocation';

function SCDynamicStoreCopyProxies(store: SCDynamicStoreRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyProxies';

function SCPreferencesSetComputerName(prefs: SCPreferencesRef; name: CFStringRef; nameEncoding: CFStringEncoding): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetComputerName';

function SCPreferencesSetLocalHostName(prefs: SCPreferencesRef; name: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetLocalHostName';

function SCNetworkReachabilityCreateWithAddress(allocator: CFAllocatorRef; address: Psockaddr): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddress';

function SCNetworkReachabilityCreateWithAddressPair(allocator: CFAllocatorRef; localAddress: Psockaddr;
  remoteAddress: Psockaddr): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddressPair';

function SCNetworkReachabilityCreateWithName(allocator: CFAllocatorRef; nodename: PAnsiChar): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithName';

function SCNetworkReachabilityGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetTypeID';

function SCNetworkReachabilityGetFlags(target: SCNetworkReachabilityRef; flags: PSCNetworkReachabilityFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetFlags';

function SCNetworkReachabilitySetCallback(target: SCNetworkReachabilityRef; callout: SCNetworkReachabilityCallBack;
  context: PSCNetworkReachabilityContext): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetCallback';

function SCNetworkReachabilityScheduleWithRunLoop(target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityScheduleWithRunLoop';

function SCNetworkReachabilityUnscheduleFromRunLoop(target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityUnscheduleFromRunLoop';

function SCNetworkReachabilitySetDispatchQueue(target: SCNetworkReachabilityRef; queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetDispatchQueue';

function SCNetworkConnectionGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionGetTypeID';

function SCNetworkConnectionCopyUserPreferences(selectionOptions: CFDictionaryRef; serviceID: PCFStringRef;
  userOptions: PCFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCopyUserPreferences';

function SCNetworkConnectionCreateWithServiceID(allocator: CFAllocatorRef; serviceID: CFStringRef; callout: SCNetworkConnectionCallBack;
  context: PSCNetworkConnectionContext): SCNetworkConnectionRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCreateWithServiceID';

function SCNetworkConnectionCopyServiceID(connection: SCNetworkConnectionRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCopyServiceID';

function SCNetworkConnectionGetStatus(connection: SCNetworkConnectionRef): SCNetworkConnectionStatus; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionGetStatus';

function SCNetworkConnectionCopyExtendedStatus(connection: SCNetworkConnectionRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCopyExtendedStatus';

function SCNetworkConnectionCopyStatistics(connection: SCNetworkConnectionRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCopyStatistics';

function SCNetworkConnectionStart(connection: SCNetworkConnectionRef; userOptions: CFDictionaryRef; linger: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionStart';

function SCNetworkConnectionStop(connection: SCNetworkConnectionRef; forceDisconnect: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionStop';

function SCNetworkConnectionCopyUserOptions(connection: SCNetworkConnectionRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionCopyUserOptions';

function SCNetworkConnectionScheduleWithRunLoop(connection: SCNetworkConnectionRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionScheduleWithRunLoop';

function SCNetworkConnectionUnscheduleFromRunLoop(connection: SCNetworkConnectionRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionUnscheduleFromRunLoop';

function SCNetworkConnectionSetDispatchQueue(connection: SCNetworkConnectionRef; queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionSetDispatchQueue';

function SCCopyLastError: CFErrorRef; cdecl;
  external libSystemConfiguration name _PU + 'SCCopyLastError';

function SCError: Integer; cdecl;
  external libSystemConfiguration name _PU + 'SCError';

function SCErrorString(status: Integer): PAnsiChar; cdecl;
  external libSystemConfiguration name _PU + 'SCErrorString';

function SCNetworkInterfaceGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetTypeID';

function SCNetworkInterfaceCopyAll: CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyAll';

function SCNetworkInterfaceGetSupportedInterfaceTypes(&interface: SCNetworkInterfaceRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetSupportedInterfaceTypes';

function SCNetworkInterfaceGetSupportedProtocolTypes(&interface: SCNetworkInterfaceRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetSupportedProtocolTypes';

function SCNetworkInterfaceCreateWithInterface(&interface: SCNetworkInterfaceRef; interfaceType: CFStringRef): SCNetworkInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCreateWithInterface';

function SCNetworkInterfaceGetBSDName(&interface: SCNetworkInterfaceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetBSDName';

function SCNetworkInterfaceGetConfiguration(&interface: SCNetworkInterfaceRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetConfiguration';

function SCNetworkInterfaceGetExtendedConfiguration(&interface: SCNetworkInterfaceRef; extendedType: CFStringRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetExtendedConfiguration';

function SCNetworkInterfaceGetHardwareAddressString(&interface: SCNetworkInterfaceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetHardwareAddressString';

function SCNetworkInterfaceGetInterface(&interface: SCNetworkInterfaceRef): SCNetworkInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetInterface';

function SCNetworkInterfaceGetInterfaceType(&interface: SCNetworkInterfaceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetInterfaceType';

function SCNetworkInterfaceGetLocalizedDisplayName(&interface: SCNetworkInterfaceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetLocalizedDisplayName';

function SCNetworkInterfaceSetConfiguration(&interface: SCNetworkInterfaceRef; config: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceSetConfiguration';

function SCNetworkInterfaceSetExtendedConfiguration(&interface: SCNetworkInterfaceRef; extendedType: CFStringRef;
  config: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceSetExtendedConfiguration';

function SCNetworkInterfaceCopyMediaOptions(&interface: SCNetworkInterfaceRef; current: PCFDictionaryRef; active: PCFDictionaryRef;
  available: PCFArrayRef; filter: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyMediaOptions';

function SCNetworkInterfaceCopyMediaSubTypes(available: CFArrayRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyMediaSubTypes';

function SCNetworkInterfaceCopyMediaSubTypeOptions(available: CFArrayRef; subType: CFStringRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyMediaSubTypeOptions';

function SCNetworkInterfaceCopyMTU(&interface: SCNetworkInterfaceRef; mtu_cur: PInteger; mtu_min: PInteger; mtu_max: PInteger): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyMTU';

function SCNetworkInterfaceSetMediaOptions(&interface: SCNetworkInterfaceRef; subtype: CFStringRef; options: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceSetMediaOptions';

function SCNetworkInterfaceSetMTU(&interface: SCNetworkInterfaceRef; mtu: Integer): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceSetMTU';

function SCNetworkInterfaceForceConfigurationRefresh(&interface: SCNetworkInterfaceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceForceConfigurationRefresh';

function SCBondInterfaceCopyAll(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceCopyAll';

function SCBondInterfaceCopyAvailableMemberInterfaces(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceCopyAvailableMemberInterfaces';

function SCBondInterfaceCreate(prefs: SCPreferencesRef): SCBondInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceCreate';

function SCBondInterfaceRemove(bond: SCBondInterfaceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceRemove';

function SCBondInterfaceGetMemberInterfaces(bond: SCBondInterfaceRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceGetMemberInterfaces';

function SCBondInterfaceGetOptions(bond: SCBondInterfaceRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceGetOptions';

function SCBondInterfaceSetMemberInterfaces(bond: SCBondInterfaceRef; members: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceSetMemberInterfaces';

function SCBondInterfaceSetLocalizedDisplayName(bond: SCBondInterfaceRef; newName: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceSetLocalizedDisplayName';

function SCBondInterfaceSetOptions(bond: SCBondInterfaceRef; newOptions: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceSetOptions';

function SCBondInterfaceCopyStatus(bond: SCBondInterfaceRef): SCBondStatusRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondInterfaceCopyStatus';

function SCBondStatusGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCBondStatusGetTypeID';

function SCBondStatusGetMemberInterfaces(bondStatus: SCBondStatusRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondStatusGetMemberInterfaces';

function SCBondStatusGetInterfaceStatus(bondStatus: SCBondStatusRef; &interface: SCNetworkInterfaceRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCBondStatusGetInterfaceStatus';

function SCVLANInterfaceCopyAll(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceCopyAll';

function SCVLANInterfaceCopyAvailablePhysicalInterfaces: CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceCopyAvailablePhysicalInterfaces';

function SCVLANInterfaceCreate(prefs: SCPreferencesRef; physical: SCNetworkInterfaceRef; tag: CFNumberRef): SCVLANInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceCreate';

function SCVLANInterfaceRemove(vlan: SCVLANInterfaceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceRemove';

function SCVLANInterfaceGetPhysicalInterface(vlan: SCVLANInterfaceRef): SCNetworkInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceGetPhysicalInterface';

function SCVLANInterfaceGetTag(vlan: SCVLANInterfaceRef): CFNumberRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceGetTag';

function SCVLANInterfaceGetOptions(vlan: SCVLANInterfaceRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceGetOptions';

function SCVLANInterfaceSetPhysicalInterfaceAndTag(vlan: SCVLANInterfaceRef; physical: SCNetworkInterfaceRef; tag: CFNumberRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceSetPhysicalInterfaceAndTag';

function SCVLANInterfaceSetLocalizedDisplayName(vlan: SCVLANInterfaceRef; newName: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceSetLocalizedDisplayName';

function SCVLANInterfaceSetOptions(vlan: SCVLANInterfaceRef; newOptions: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCVLANInterfaceSetOptions';

function SCNetworkProtocolGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolGetTypeID';

function SCNetworkProtocolGetConfiguration(protocol: SCNetworkProtocolRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolGetConfiguration';

function SCNetworkProtocolGetEnabled(protocol: SCNetworkProtocolRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolGetEnabled';

function SCNetworkProtocolGetProtocolType(protocol: SCNetworkProtocolRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolGetProtocolType';

function SCNetworkProtocolSetConfiguration(protocol: SCNetworkProtocolRef; config: CFDictionaryRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolSetConfiguration';

function SCNetworkProtocolSetEnabled(protocol: SCNetworkProtocolRef; enabled: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkProtocolSetEnabled';

function SCNetworkServiceGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceGetTypeID';

function SCNetworkServiceAddProtocolType(service: SCNetworkServiceRef; protocolType: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceAddProtocolType';

function SCNetworkServiceCopyAll(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceCopyAll';

function SCNetworkServiceCopyProtocols(service: SCNetworkServiceRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceCopyProtocols';

function SCNetworkServiceCreate(prefs: SCPreferencesRef; &interface: SCNetworkInterfaceRef): SCNetworkServiceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceCreate';

function SCNetworkServiceCopy(prefs: SCPreferencesRef; serviceID: CFStringRef): SCNetworkServiceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceCopy';

function SCNetworkServiceEstablishDefaultConfiguration(service: SCNetworkServiceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceEstablishDefaultConfiguration';

function SCNetworkServiceGetEnabled(service: SCNetworkServiceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceGetEnabled';

function SCNetworkServiceGetInterface(service: SCNetworkServiceRef): SCNetworkInterfaceRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceGetInterface';

function SCNetworkServiceGetName(service: SCNetworkServiceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceGetName';

function SCNetworkServiceCopyProtocol(service: SCNetworkServiceRef; protocolType: CFStringRef): SCNetworkProtocolRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceCopyProtocol';

function SCNetworkServiceGetServiceID(service: SCNetworkServiceRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceGetServiceID';

function SCNetworkServiceRemove(service: SCNetworkServiceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceRemove';

function SCNetworkServiceRemoveProtocolType(service: SCNetworkServiceRef; protocolType: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceRemoveProtocolType';

function SCNetworkServiceSetEnabled(service: SCNetworkServiceRef; enabled: Boolean): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceSetEnabled';

function SCNetworkServiceSetName(service: SCNetworkServiceRef; name: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkServiceSetName';

function SCNetworkSetGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetGetTypeID';

function SCNetworkSetAddService(&set: SCNetworkSetRef; service: SCNetworkServiceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetAddService';

function SCNetworkSetContainsInterface(&set: SCNetworkSetRef; &interface: SCNetworkInterfaceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetContainsInterface';

function SCNetworkSetCopyAll(prefs: SCPreferencesRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetCopyAll';

function SCNetworkSetCopyCurrent(prefs: SCPreferencesRef): SCNetworkSetRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetCopyCurrent';

function SCNetworkSetCopyServices(&set: SCNetworkSetRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetCopyServices';

function SCNetworkSetCreate(prefs: SCPreferencesRef): SCNetworkSetRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetCreate';

function SCNetworkSetCopy(prefs: SCPreferencesRef; setID: CFStringRef): SCNetworkSetRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetCopy';

function SCNetworkSetGetName(&set: SCNetworkSetRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetGetName';

function SCNetworkSetGetSetID(&set: SCNetworkSetRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetGetSetID';

function SCNetworkSetGetServiceOrder(&set: SCNetworkSetRef): CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetGetServiceOrder';

function SCNetworkSetRemove(&set: SCNetworkSetRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetRemove';

function SCNetworkSetRemoveService(&set: SCNetworkSetRef; service: SCNetworkServiceRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetRemoveService';

function SCNetworkSetSetCurrent(&set: SCNetworkSetRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetSetCurrent';

function SCNetworkSetSetName(&set: SCNetworkSetRef; name: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetSetName';

function SCNetworkSetSetServiceOrder(&set: SCNetworkSetRef; newOrder: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkSetSetServiceOrder';

function DHCPClientPreferencesSetApplicationOptions(applicationID: CFStringRef; options: PUInt8; count: CFIndex): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'DHCPClientPreferencesSetApplicationOptions';

function DHCPClientPreferencesCopyApplicationOptions(applicationID: CFStringRef; count: PCFIndex): PUInt8; cdecl;
  external libSystemConfiguration name _PU + 'DHCPClientPreferencesCopyApplicationOptions';

function CNSetSupportedSSIDs(ssidArray: CFArrayRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'CNSetSupportedSSIDs';

function CNMarkPortalOnline(interfaceName: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'CNMarkPortalOnline';

function CNMarkPortalOffline(interfaceName: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'CNMarkPortalOffline';

function CNCopySupportedInterfaces: CFArrayRef; cdecl;
  external libSystemConfiguration name _PU + 'CNCopySupportedInterfaces';

function CNCopyCurrentNetworkInfo(interfaceName: CFStringRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'CNCopyCurrentNetworkInfo';

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Foundation;

var
  SystemConfigurationModule: THandle;

function kSCDynamicStoreUseSessionKeys: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreUseSessionKeys'));
end;

function kSCResvLink: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCResvLink'));
end;

function kSCResvInactive: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCResvInactive'));
end;

function kSCPropInterfaceName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropInterfaceName'));
end;

function kSCPropMACAddress: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropMACAddress'));
end;

function kSCPropUserDefinedName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropUserDefinedName'));
end;

function kSCPropVersion: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropVersion'));
end;

function kSCPrefCurrentSet: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPrefCurrentSet'));
end;

function kSCPrefNetworkServices: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPrefNetworkServices'));
end;

function kSCPrefSets: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPrefSets'));
end;

function kSCPrefSystem: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPrefSystem'));
end;

function kSCCompNetwork: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompNetwork'));
end;

function kSCCompService: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompService'));
end;

function kSCCompGlobal: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompGlobal'));
end;

function kSCCompHostNames: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompHostNames'));
end;

function kSCCompInterface: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompInterface'));
end;

function kSCCompSystem: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompSystem'));
end;

function kSCCompUsers: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompUsers'));
end;

function kSCCompAnyRegex: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCCompAnyRegex'));
end;

function kSCEntNetAirPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetAirPort'));
end;

function kSCEntNetDHCP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetDHCP'));
end;

function kSCEntNetDNS: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetDNS'));
end;

function kSCEntNetEthernet: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetEthernet'));
end;

function kSCEntNetFireWire: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetFireWire'));
end;

function kSCEntNetInterface: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetInterface'));
end;

function kSCEntNetIPSec: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPSec'));
end;

function kSCEntNetIPv4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPv4'));
end;

function kSCEntNetIPv6: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPv6'));
end;

function kSCEntNetL2TP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetL2TP'));
end;

function kSCEntNetLink: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetLink'));
end;

function kSCEntNetModem: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetModem'));
end;

function kSCEntNetPPP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPP'));
end;

function kSCEntNetPPPoE: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPPoE'));
end;

function kSCEntNetPPPSerial: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPPSerial'));
end;

function kSCEntNetPPTP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPTP'));
end;

function kSCEntNetProxies: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetProxies'));
end;

function kSCEntNetSMB: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNetSMB'));
end;

function kSCEntNet6to4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntNet6to4'));
end;

function kSCPropNetOverridePrimary: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetOverridePrimary'));
end;

function kSCPropNetServiceOrder: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetServiceOrder'));
end;

function kSCPropNetPPPOverridePrimary: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPOverridePrimary'));
end;

function kSCPropNetInterfaces: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaces'));
end;

function kSCPropNetLocalHostName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLocalHostName'));
end;

function kSCPropNetAirPortAllowNetCreation: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAllowNetCreation'));
end;

function kSCPropNetAirPortAuthPassword: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPassword'));
end;

function kSCPropNetAirPortAuthPasswordEncryption: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPasswordEncryption'));
end;

function kSCPropNetAirPortJoinMode: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortJoinMode'));
end;

function kSCPropNetAirPortPowerEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortPowerEnabled'));
end;

function kSCPropNetAirPortPreferredNetwork: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortPreferredNetwork'));
end;

function kSCPropNetAirPortSavePasswords: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortSavePasswords'));
end;

function kSCValNetAirPortJoinModeAutomatic: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeAutomatic'));
end;

function kSCValNetAirPortJoinModePreferred: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModePreferred'));
end;

function kSCValNetAirPortJoinModeRanked: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRanked'));
end;

function kSCValNetAirPortJoinModeRecent: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRecent'));
end;

function kSCValNetAirPortJoinModeStrongest: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeStrongest'));
end;

function kSCValNetAirPortAuthPasswordEncryptionKeychain: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortAuthPasswordEncryptionKeychain'));
end;

function kSCPropNetDNSDomainName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSDomainName'));
end;

function kSCPropNetDNSOptions: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSOptions'));
end;

function kSCPropNetDNSSearchDomains: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSearchDomains'));
end;

function kSCPropNetDNSSearchOrder: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSearchOrder'));
end;

function kSCPropNetDNSServerAddresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerAddresses'));
end;

function kSCPropNetDNSServerPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerPort'));
end;

function kSCPropNetDNSServerTimeout: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerTimeout'));
end;

function kSCPropNetDNSSortList: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSortList'));
end;

function kSCPropNetDNSSupplementalMatchDomains: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchDomains'));
end;

function kSCPropNetDNSSupplementalMatchOrders: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchOrders'));
end;

function kSCPropNetEthernetMediaSubType: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMediaSubType'));
end;

function kSCPropNetEthernetMediaOptions: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMediaOptions'));
end;

function kSCPropNetEthernetMTU: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMTU'));
end;

function kSCPropNetInterfaceDeviceName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceDeviceName'));
end;

function kSCPropNetInterfaceHardware: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceHardware'));
end;

function kSCPropNetInterfaceType: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceType'));
end;

function kSCPropNetInterfaceSubType: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceSubType'));
end;

function kSCPropNetInterfaceSupportsModemOnHold: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceSupportsModemOnHold'));
end;

function kSCValNetInterfaceTypeEthernet: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeEthernet'));
end;

function kSCValNetInterfaceTypeFireWire: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeFireWire'));
end;

function kSCValNetInterfaceTypePPP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypePPP'));
end;

function kSCValNetInterfaceType6to4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceType6to4'));
end;

function kSCValNetInterfaceTypeIPSec: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeIPSec'));
end;

function kSCValNetInterfaceSubTypePPPoE: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPoE'));
end;

function kSCValNetInterfaceSubTypePPPSerial: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPSerial'));
end;

function kSCValNetInterfaceSubTypePPTP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPTP'));
end;

function kSCValNetInterfaceSubTypeL2TP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypeL2TP'));
end;

function kSCPropNetIPSecAuthenticationMethod: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecAuthenticationMethod'));
end;

function kSCPropNetIPSecLocalCertificate: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalCertificate'));
end;

function kSCPropNetIPSecLocalIdentifier: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifier'));
end;

function kSCPropNetIPSecLocalIdentifierType: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifierType'));
end;

function kSCPropNetIPSecSharedSecret: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecret'));
end;

function kSCPropNetIPSecSharedSecretEncryption: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecretEncryption'));
end;

function kSCPropNetIPSecConnectTime: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecConnectTime'));
end;

function kSCPropNetIPSecRemoteAddress: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecRemoteAddress'));
end;

function kSCPropNetIPSecStatus: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecStatus'));
end;

function kSCPropNetIPSecXAuthEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthEnabled'));
end;

function kSCPropNetIPSecXAuthName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthName'));
end;

function kSCPropNetIPSecXAuthPassword: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPassword'));
end;

function kSCPropNetIPSecXAuthPasswordEncryption: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPasswordEncryption'));
end;

function kSCValNetIPSecAuthenticationMethodSharedSecret: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodSharedSecret'));
end;

function kSCValNetIPSecAuthenticationMethodCertificate: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodCertificate'));
end;

function kSCValNetIPSecAuthenticationMethodHybrid: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodHybrid'));
end;

function kSCValNetIPSecLocalIdentifierTypeKeyID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecLocalIdentifierTypeKeyID'));
end;

function kSCValNetIPSecSharedSecretEncryptionKeychain: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecSharedSecretEncryptionKeychain'));
end;

function kSCValNetIPSecXAuthPasswordEncryptionKeychain: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionKeychain'));
end;

function kSCValNetIPSecXAuthPasswordEncryptionPrompt: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionPrompt'));
end;

function kSCPropNetIPv4Addresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4Addresses'));
end;

function kSCPropNetIPv4ConfigMethod: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4ConfigMethod'));
end;

function kSCPropNetIPv4DHCPClientID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4DHCPClientID'));
end;

function kSCPropNetIPv4Router: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4Router'));
end;

function kSCPropNetIPv4SubnetMasks: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4SubnetMasks'));
end;

function kSCPropNetIPv4DestAddresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4DestAddresses'));
end;

function kSCPropNetIPv4BroadcastAddresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4BroadcastAddresses'));
end;

function kSCValNetIPv4ConfigMethodAutomatic: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodAutomatic'));
end;

function kSCValNetIPv4ConfigMethodBOOTP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodBOOTP'));
end;

function kSCValNetIPv4ConfigMethodDHCP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodDHCP'));
end;

function kSCValNetIPv4ConfigMethodINFORM: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodINFORM'));
end;

function kSCValNetIPv4ConfigMethodLinkLocal: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodLinkLocal'));
end;

function kSCValNetIPv4ConfigMethodManual: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodManual'));
end;

function kSCValNetIPv4ConfigMethodPPP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodPPP'));
end;

function kSCPropNetIPv6Addresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Addresses'));
end;

function kSCPropNetIPv6ConfigMethod: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6ConfigMethod'));
end;

function kSCPropNetIPv6DestAddresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6DestAddresses'));
end;

function kSCPropNetIPv6Flags: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Flags'));
end;

function kSCPropNetIPv6PrefixLength: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6PrefixLength'));
end;

function kSCPropNetIPv6Router: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Router'));
end;

function kSCValNetIPv6ConfigMethodAutomatic: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodAutomatic'));
end;

function kSCValNetIPv6ConfigMethodLinkLocal: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodLinkLocal'));
end;

function kSCValNetIPv6ConfigMethodManual: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodManual'));
end;

function kSCValNetIPv6ConfigMethodRouterAdvertisement: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodRouterAdvertisement'));
end;

function kSCValNetIPv6ConfigMethod6to4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethod6to4'));
end;

function kSCPropNet6to4Relay: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNet6to4Relay'));
end;

function kSCPropNetLinkActive: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLinkActive'));
end;

function kSCPropNetLinkDetaching: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLinkDetaching'));
end;

function kSCPropNetModemAccessPointName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemAccessPointName'));
end;

function kSCPropNetModemConnectionPersonality: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectionPersonality'));
end;

function kSCPropNetModemConnectionScript: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectionScript'));
end;

function kSCPropNetModemConnectSpeed: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectSpeed'));
end;

function kSCPropNetModemDataCompression: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDataCompression'));
end;

function kSCPropNetModemDeviceContextID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceContextID'));
end;

function kSCPropNetModemDeviceModel: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceModel'));
end;

function kSCPropNetModemDeviceVendor: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceVendor'));
end;

function kSCPropNetModemDialMode: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDialMode'));
end;

function kSCPropNetModemErrorCorrection: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemErrorCorrection'));
end;

function kSCPropNetModemHoldCallWaitingAudibleAlert: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldCallWaitingAudibleAlert'));
end;

function kSCPropNetModemHoldDisconnectOnAnswer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldDisconnectOnAnswer'));
end;

function kSCPropNetModemHoldEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldEnabled'));
end;

function kSCPropNetModemHoldReminder: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldReminder'));
end;

function kSCPropNetModemHoldReminderTime: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldReminderTime'));
end;

function kSCPropNetModemNote: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemNote'));
end;

function kSCPropNetModemPulseDial: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemPulseDial'));
end;

function kSCPropNetModemSpeaker: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemSpeaker'));
end;

function kSCPropNetModemSpeed: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemSpeed'));
end;

function kSCValNetModemDialModeIgnoreDialTone: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeIgnoreDialTone'));
end;

function kSCValNetModemDialModeManual: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeManual'));
end;

function kSCValNetModemDialModeWaitForDialTone: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeWaitForDialTone'));
end;

function kSCPropNetPPPACSPEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPACSPEnabled'));
end;

function kSCPropNetPPPConnectTime: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPConnectTime'));
end;

function kSCPropNetPPPDeviceLastCause: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDeviceLastCause'));
end;

function kSCPropNetPPPDialOnDemand: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDialOnDemand'));
end;

function kSCPropNetPPPDisconnectOnFastUserSwitch: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnFastUserSwitch'));
end;

function kSCPropNetPPPDisconnectOnIdle: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdle'));
end;

function kSCPropNetPPPDisconnectOnIdleTimer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdleTimer'));
end;

function kSCPropNetPPPDisconnectOnLogout: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnLogout'));
end;

function kSCPropNetPPPDisconnectOnSleep: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnSleep'));
end;

function kSCPropNetPPPDisconnectTime: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectTime'));
end;

function kSCPropNetPPPIdleReminder: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminder'));
end;

function kSCPropNetPPPIdleReminderTimer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminderTimer'));
end;

function kSCPropNetPPPLastCause: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLastCause'));
end;

function kSCPropNetPPPLogfile: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLogfile'));
end;

function kSCPropNetPPPPlugins: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPPlugins'));
end;

function kSCPropNetPPPRetryConnectTime: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPRetryConnectTime'));
end;

function kSCPropNetPPPSessionTimer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPSessionTimer'));
end;

function kSCPropNetPPPStatus: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPStatus'));
end;

function kSCPropNetPPPUseSessionTimer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPUseSessionTimer'));
end;

function kSCPropNetPPPVerboseLogging: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPVerboseLogging'));
end;

function kSCPropNetPPPAuthEAPPlugins: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthEAPPlugins'));
end;

function kSCPropNetPPPAuthName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthName'));
end;

function kSCPropNetPPPAuthPassword: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPassword'));
end;

function kSCPropNetPPPAuthPasswordEncryption: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPasswordEncryption'));
end;

function kSCPropNetPPPAuthPrompt: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPrompt'));
end;

function kSCPropNetPPPAuthProtocol: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthProtocol'));
end;

function kSCValNetPPPAuthPasswordEncryptionKeychain: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionKeychain'));
end;

function kSCValNetPPPAuthPasswordEncryptionToken: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionToken'));
end;

function kSCValNetPPPAuthPromptBefore: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptBefore'));
end;

function kSCValNetPPPAuthPromptAfter: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptAfter'));
end;

function kSCValNetPPPAuthProtocolCHAP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolCHAP'));
end;

function kSCValNetPPPAuthProtocolEAP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolEAP'));
end;

function kSCValNetPPPAuthProtocolMSCHAP1: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP1'));
end;

function kSCValNetPPPAuthProtocolMSCHAP2: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP2'));
end;

function kSCValNetPPPAuthProtocolPAP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolPAP'));
end;

function kSCPropNetPPPCommAlternateRemoteAddress: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommAlternateRemoteAddress'));
end;

function kSCPropNetPPPCommConnectDelay: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommConnectDelay'));
end;

function kSCPropNetPPPCommDisplayTerminalWindow: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommDisplayTerminalWindow'));
end;

function kSCPropNetPPPCommRedialCount: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialCount'));
end;

function kSCPropNetPPPCommRedialEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialEnabled'));
end;

function kSCPropNetPPPCommRedialInterval: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialInterval'));
end;

function kSCPropNetPPPCommRemoteAddress: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRemoteAddress'));
end;

function kSCPropNetPPPCommTerminalScript: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommTerminalScript'));
end;

function kSCPropNetPPPCommUseTerminalScript: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommUseTerminalScript'));
end;

function kSCPropNetPPPCCPEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPEnabled'));
end;

function kSCPropNetPPPCCPMPPE40Enabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE40Enabled'));
end;

function kSCPropNetPPPCCPMPPE128Enabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE128Enabled'));
end;

function kSCPropNetPPPIPCPCompressionVJ: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIPCPCompressionVJ'));
end;

function kSCPropNetPPPIPCPUsePeerDNS: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIPCPUsePeerDNS'));
end;

function kSCPropNetPPPLCPEchoEnabled: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoEnabled'));
end;

function kSCPropNetPPPLCPEchoFailure: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoFailure'));
end;

function kSCPropNetPPPLCPEchoInterval: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoInterval'));
end;

function kSCPropNetPPPLCPCompressionACField: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionACField'));
end;

function kSCPropNetPPPLCPCompressionPField: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionPField'));
end;

function kSCPropNetPPPLCPMRU: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPMRU'));
end;

function kSCPropNetPPPLCPMTU: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPMTU'));
end;

function kSCPropNetPPPLCPReceiveACCM: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPReceiveACCM'));
end;

function kSCPropNetPPPLCPTransmitACCM: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPTransmitACCM'));
end;

function kSCPropNetL2TPIPSecSharedSecret: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecret'));
end;

function kSCPropNetL2TPIPSecSharedSecretEncryption: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecretEncryption'));
end;

function kSCPropNetL2TPTransport: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPTransport'));
end;

function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPIPSecSharedSecretEncryptionKeychain'));
end;

function kSCValNetL2TPTransportIP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPTransportIP'));
end;

function kSCValNetL2TPTransportIPSec: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPTransportIPSec'));
end;

function kSCPropNetProxiesExceptionsList: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesExceptionsList'));
end;

function kSCPropNetProxiesExcludeSimpleHostnames: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesExcludeSimpleHostnames'));
end;

function kSCPropNetProxiesFTPEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPEnable'));
end;

function kSCPropNetProxiesFTPPassive: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPassive'));
end;

function kSCPropNetProxiesFTPPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPort'));
end;

function kSCPropNetProxiesFTPProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPProxy'));
end;

function kSCPropNetProxiesGopherEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherEnable'));
end;

function kSCPropNetProxiesGopherPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherPort'));
end;

function kSCPropNetProxiesGopherProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherProxy'));
end;

function kSCPropNetProxiesHTTPEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPEnable'));
end;

function kSCPropNetProxiesHTTPPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPPort'));
end;

function kSCPropNetProxiesHTTPProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPProxy'));
end;

function kSCPropNetProxiesHTTPSEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSEnable'));
end;

function kSCPropNetProxiesHTTPSPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSPort'));
end;

function kSCPropNetProxiesHTTPSProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSProxy'));
end;

function kSCPropNetProxiesRTSPEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPEnable'));
end;

function kSCPropNetProxiesRTSPPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPPort'));
end;

function kSCPropNetProxiesRTSPProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPProxy'));
end;

function kSCPropNetProxiesSOCKSEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSEnable'));
end;

function kSCPropNetProxiesSOCKSPort: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSPort'));
end;

function kSCPropNetProxiesSOCKSProxy: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSProxy'));
end;

function kSCPropNetProxiesProxyAutoConfigEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigEnable'));
end;

function kSCPropNetProxiesProxyAutoConfigJavaScript: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigJavaScript'));
end;

function kSCPropNetProxiesProxyAutoConfigURLString: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigURLString'));
end;

function kSCPropNetProxiesProxyAutoDiscoveryEnable: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoDiscoveryEnable'));
end;

function kSCPropNetSMBNetBIOSName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSName'));
end;

function kSCPropNetSMBNetBIOSNodeType: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSNodeType'));
end;

function kSCPropNetSMBNetBIOSScope: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSScope'));
end;

function kSCPropNetSMBWINSAddresses: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBWINSAddresses'));
end;

function kSCPropNetSMBWorkgroup: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBWorkgroup'));
end;

function kSCValNetSMBNetBIOSNodeTypeBroadcast: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeBroadcast'));
end;

function kSCValNetSMBNetBIOSNodeTypePeer: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypePeer'));
end;

function kSCValNetSMBNetBIOSNodeTypeMixed: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeMixed'));
end;

function kSCValNetSMBNetBIOSNodeTypeHybrid: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeHybrid'));
end;

function kSCEntUsersConsoleUser: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCEntUsersConsoleUser'));
end;

function kSCPropSystemComputerName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropSystemComputerName'));
end;

function kSCPropSystemComputerNameEncoding: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropSystemComputerNameEncoding'));
end;

function kSCDynamicStoreDomainFile: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainFile'));
end;

function kSCDynamicStoreDomainPlugin: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainPlugin'));
end;

function kSCDynamicStoreDomainSetup: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainSetup'));
end;

function kSCDynamicStoreDomainState: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainState'));
end;

function kSCDynamicStoreDomainPrefs: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainPrefs'));
end;

function kSCDynamicStorePropSetupCurrentSet: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropSetupCurrentSet'));
end;

function kSCDynamicStorePropSetupLastUpdated: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropSetupLastUpdated'));
end;

function kSCDynamicStorePropNetInterfaces: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetInterfaces'));
end;

function kSCDynamicStorePropNetPrimaryInterface: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryInterface'));
end;

function kSCDynamicStorePropNetPrimaryService: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryService'));
end;

function kSCDynamicStorePropNetServiceIDs: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetServiceIDs'));
end;

function kSCPropUsersConsoleUserName: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserName'));
end;

function kSCPropUsersConsoleUserUID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserUID'));
end;

function kSCPropUsersConsoleUserGID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserGID'));
end;

function kCFErrorDomainSystemConfiguration: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kCFErrorDomainSystemConfiguration'));
end;

function kSCNetworkInterfaceType6to4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceType6to4'));
end;

function kSCNetworkInterfaceTypeBluetooth: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBluetooth'));
end;

function kSCNetworkInterfaceTypeBond: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBond'));
end;

function kSCNetworkInterfaceTypeEthernet: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeEthernet'));
end;

function kSCNetworkInterfaceTypeFireWire: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeFireWire'));
end;

function kSCNetworkInterfaceTypeIEEE80211: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIEEE80211'));
end;

function kSCNetworkInterfaceTypeIPSec: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPSec'));
end;

function kSCNetworkInterfaceTypeIrDA: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIrDA'));
end;

function kSCNetworkInterfaceTypeL2TP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeL2TP'));
end;

function kSCNetworkInterfaceTypeModem: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeModem'));
end;

function kSCNetworkInterfaceTypePPP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPP'));
end;

function kSCNetworkInterfaceTypePPTP: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPTP'));
end;

function kSCNetworkInterfaceTypeSerial: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeSerial'));
end;

function kSCNetworkInterfaceTypeVLAN: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeVLAN'));
end;

function kSCNetworkInterfaceTypeWWAN: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeWWAN'));
end;

function kSCNetworkInterfaceTypeIPv4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPv4'));
end;

function kSCNetworkInterfaceIPv4: SCNetworkInterfaceRef;
begin
  Result := SCNetworkInterfaceRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceIPv4'));
end;

function kSCBondStatusDeviceAggregationStatus: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceAggregationStatus'));
end;

function kSCBondStatusDeviceCollecting: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceCollecting'));
end;

function kSCBondStatusDeviceDistributing: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceDistributing'));
end;

function kSCNetworkProtocolTypeDNS: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeDNS'));
end;

function kSCNetworkProtocolTypeIPv4: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv4'));
end;

function kSCNetworkProtocolTypeIPv6: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv6'));
end;

function kSCNetworkProtocolTypeProxies: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeProxies'));
end;

function kSCNetworkProtocolTypeSMB: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeSMB'));
end;

function kCNNetworkInfoKeySSIDData: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeySSIDData'));
end;

function kCNNetworkInfoKeySSID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeySSID'));
end;

function kCNNetworkInfoKeyBSSID: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeyBSSID'));
end;

initialization
  SystemConfigurationModule := LoadLibrary(libSystemConfiguration);

finalization
  if SystemConfigurationModule <> 0 then
    FreeLibrary(SystemConfigurationModule);

end.