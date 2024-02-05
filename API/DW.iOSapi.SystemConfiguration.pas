unit DW.iOSapi.SystemConfiguration;

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
  Macapi.CoreFoundation, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.Security;

const
  kSCNetworkFlagsTransientConnection = 1 shl 0;
  kSCNetworkFlagsReachable = 1 shl 1;
  kSCNetworkFlagsConnectionRequired = 1 shl 2;
  kSCNetworkFlagsConnectionAutomatic = 1 shl 3;
  kSCNetworkFlagsInterventionRequired = 1 shl 4;
  kSCNetworkFlagsIsLocalAddress = 1 shl 16;
  kSCNetworkFlagsIsDirect = 1 shl 17;
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
  kSCPreferencesNotificationCommit = 1 shl 0;
  kSCPreferencesNotificationApply = 1 shl 1;
  kSCNetworkReachabilityFlagsTransientConnection = 1 shl 0;
  kSCNetworkReachabilityFlagsReachable = 1 shl 1;
  kSCNetworkReachabilityFlagsConnectionRequired = 1 shl 2;
  kSCNetworkReachabilityFlagsConnectionOnTraffic = 1 shl 3;
  kSCNetworkReachabilityFlagsInterventionRequired = 1 shl 4;
  kSCNetworkReachabilityFlagsConnectionOnDemand = 1 shl 5;
  kSCNetworkReachabilityFlagsIsLocalAddress = 1 shl 16;
  kSCNetworkReachabilityFlagsIsDirect = 1 shl 17;
  kSCNetworkReachabilityFlagsIsWWAN = 1 shl 18;
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
  __darwin_socklen_t = LongWord;
  P__darwin_socklen_t = ^__darwin_socklen_t;
  AuthorizationRef = Pointer;
  SCDynamicStoreRef = Pointer;
  PSCDynamicStoreRef = ^SCDynamicStoreRef;
  SCNetworkConnectionFlags = LongWord;
  PSCNetworkConnectionFlags = ^SCNetworkConnectionFlags;
  SCPreferencesRef = Pointer;
  PSCPreferencesRef = ^SCPreferencesRef;
  SCPreferencesNotification = LongWord;
  SCNetworkReachabilityRef = Pointer;
  PSCNetworkReachabilityRef = ^SCNetworkReachabilityRef;
  SCNetworkReachabilityFlags = LongWord;
  PSCNetworkReachabilityFlags = ^SCNetworkReachabilityFlags;
  SCNetworkConnectionRef = Pointer;
  PSCNetworkConnectionRef = ^SCNetworkConnectionRef;
  SCNetworkConnectionStatus = Int32;
  SCNetworkConnectionPPPStatus = Int32;
  SCNetworkInterfaceRef = Pointer;
  PSCNetworkInterfaceRef = ^SCNetworkInterfaceRef;
  SCBondInterfaceRef = SCNetworkInterfaceRef;
  PSCBondInterfaceRef = ^SCBondInterfaceRef;
  SCBondStatusRef = Pointer;
  PSCBondStatusRef = ^SCBondStatusRef;
  SCVLANInterfaceRef = SCNetworkInterfaceRef;
  PSCVLANInterfaceRef = ^SCVLANInterfaceRef;
  SCNetworkProtocolRef = Pointer;
  PSCNetworkProtocolRef = ^SCNetworkProtocolRef;
  SCNetworkServiceRef = Pointer;
  PSCNetworkServiceRef = ^SCNetworkServiceRef;
  SCNetworkSetRef = Pointer;
  PSCNetworkSetRef = ^SCNetworkSetRef;

  TSystemConfigurationRetain = function(param1: Pointer): Pointer; cdecl;
  TSystemConfigurationRelease = procedure(param1: Pointer); cdecl;
  TSystemConfigurationCopyDescription = function(param1: Pointer): CFStringRef; cdecl;

  SCDynamicStoreCallBack = procedure(param1: SCDynamicStoreRef; param2: CFArrayRef; param3: Pointer); cdecl;
  PSCDynamicStoreCallBack = ^SCDynamicStoreCallBack;
  SCPreferencesCallBack = procedure(param1: SCPreferencesRef; param2: SCPreferencesNotification; param3: Pointer); cdecl;
  PSCPreferencesCallBack = ^SCPreferencesCallBack;
  SCNetworkReachabilityCallBack = procedure(param1: SCNetworkReachabilityRef; param2: SCNetworkReachabilityFlags; param3: Pointer); cdecl;
  PSCNetworkReachabilityCallBack = ^SCNetworkReachabilityCallBack;
  SCNetworkConnectionCallBack = procedure(param1: SCNetworkConnectionRef; param2: SCNetworkConnectionStatus; param3: Pointer); cdecl;
  PSCNetworkConnectionCallBack = ^SCNetworkConnectionCallBack;

  SCDynamicStoreContext = record
    version: CFIndex;
    info: Pointer;
    retain: TSystemConfigurationRetain;
    release: TSystemConfigurationRelease;
    copyDescription: TSystemConfigurationCopyDescription;
  end;
  PSCDynamicStoreContext = ^SCDynamicStoreContext;

  SCPreferencesContext = record
    version: CFIndex;
    info: Pointer;
    retain: TSystemConfigurationRetain;
    release: TSystemConfigurationRelease;
    copyDescription: TSystemConfigurationCopyDescription;
  end;
  PSCPreferencesContext = ^SCPreferencesContext;

  SCNetworkReachabilityContext = record
    version: CFIndex;
    info: Pointer;
    retain: TSystemConfigurationRetain;
    release: TSystemConfigurationRelease;
    copyDescription: TSystemConfigurationCopyDescription;
  end;
  PSCNetworkReachabilityContext = ^SCNetworkReachabilityContext;

  SCNetworkConnectionContext = record
    version: CFIndex;
    info: Pointer;
    retain: TSystemConfigurationRetain;
    release: TSystemConfigurationRelease;
    copyDescription: TSystemConfigurationCopyDescription;
  end;
  PSCNetworkConnectionContext = ^SCNetworkConnectionContext;

function kCNNetworkInfoKeySSIDData: NSString;
function kCNNetworkInfoKeySSID: NSString;
function kCNNetworkInfoKeyBSSID: NSString;
function kSCDynamicStoreUseSessionKeys: NSString;
function kSCResvLink: NSString;
function kSCResvInactive: NSString;
function kSCPropInterfaceName: NSString;
function kSCPropMACAddress: NSString;
function kSCPropUserDefinedName: NSString;
function kSCPropVersion: NSString;
function kSCPrefCurrentSet: NSString;
function kSCPrefNetworkServices: NSString;
function kSCPrefSets: NSString;
function kSCPrefSystem: NSString;
function kSCCompNetwork: NSString;
function kSCCompService: NSString;
function kSCCompGlobal: NSString;
function kSCCompHostNames: NSString;
function kSCCompInterface: NSString;
function kSCCompSystem: NSString;
function kSCCompUsers: NSString;
function kSCCompAnyRegex: NSString;
function kSCEntNetAirPort: NSString;
function kSCEntNetDHCP: NSString;
function kSCEntNetDNS: NSString;
function kSCEntNetEthernet: NSString;
function kSCEntNetFireWire: NSString;
function kSCEntNetInterface: NSString;
function kSCEntNetIPSec: NSString;
function kSCEntNetIPv4: NSString;
function kSCEntNetIPv6: NSString;
function kSCEntNetL2TP: NSString;
function kSCEntNetLink: NSString;
function kSCEntNetModem: NSString;
function kSCEntNetPPP: NSString;
function kSCEntNetPPPoE: NSString;
function kSCEntNetPPPSerial: NSString;
function kSCEntNetPPTP: NSString;
function kSCEntNetProxies: NSString;
function kSCEntNetSMB: NSString;
function kSCEntNet6to4: NSString;
function kSCPropNetOverridePrimary: NSString;
function kSCPropNetServiceOrder: NSString;
function kSCPropNetPPPOverridePrimary: NSString;
function kSCPropNetInterfaces: NSString;
function kSCPropNetLocalHostName: NSString;
function kSCPropNetAirPortAllowNetCreation: NSString;
function kSCPropNetAirPortAuthPassword: NSString;
function kSCPropNetAirPortAuthPasswordEncryption: NSString;
function kSCPropNetAirPortJoinMode: NSString;
function kSCPropNetAirPortPowerEnabled: NSString;
function kSCPropNetAirPortPreferredNetwork: NSString;
function kSCPropNetAirPortSavePasswords: NSString;
function kSCValNetAirPortJoinModeAutomatic: NSString;
function kSCValNetAirPortJoinModePreferred: NSString;
function kSCValNetAirPortJoinModeRanked: NSString;
function kSCValNetAirPortJoinModeRecent: NSString;
function kSCValNetAirPortJoinModeStrongest: NSString;
function kSCValNetAirPortAuthPasswordEncryptionKeychain: NSString;
function kSCPropNetDNSDomainName: NSString;
function kSCPropNetDNSOptions: NSString;
function kSCPropNetDNSSearchDomains: NSString;
function kSCPropNetDNSSearchOrder: NSString;
function kSCPropNetDNSServerAddresses: NSString;
function kSCPropNetDNSServerPort: NSString;
function kSCPropNetDNSServerTimeout: NSString;
function kSCPropNetDNSSortList: NSString;
function kSCPropNetDNSSupplementalMatchDomains: NSString;
function kSCPropNetDNSSupplementalMatchOrders: NSString;
function kSCPropNetEthernetMediaSubType: NSString;
function kSCPropNetEthernetMediaOptions: NSString;
function kSCPropNetEthernetMTU: NSString;
function kSCPropNetInterfaceDeviceName: NSString;
function kSCPropNetInterfaceHardware: NSString;
function kSCPropNetInterfaceType: NSString;
function kSCPropNetInterfaceSubType: NSString;
function kSCPropNetInterfaceSupportsModemOnHold: NSString;
function kSCValNetInterfaceTypeEthernet: NSString;
function kSCValNetInterfaceTypeFireWire: NSString;
function kSCValNetInterfaceTypePPP: NSString;
function kSCValNetInterfaceType6to4: NSString;
function kSCValNetInterfaceTypeIPSec: NSString;
function kSCValNetInterfaceSubTypePPPoE: NSString;
function kSCValNetInterfaceSubTypePPPSerial: NSString;
function kSCValNetInterfaceSubTypePPTP: NSString;
function kSCValNetInterfaceSubTypeL2TP: NSString;
function kSCPropNetIPSecAuthenticationMethod: NSString;
function kSCPropNetIPSecLocalCertificate: NSString;
function kSCPropNetIPSecLocalIdentifier: NSString;
function kSCPropNetIPSecLocalIdentifierType: NSString;
function kSCPropNetIPSecSharedSecret: NSString;
function kSCPropNetIPSecSharedSecretEncryption: NSString;
function kSCPropNetIPSecConnectTime: NSString;
function kSCPropNetIPSecRemoteAddress: NSString;
function kSCPropNetIPSecStatus: NSString;
function kSCPropNetIPSecXAuthEnabled: NSString;
function kSCPropNetIPSecXAuthName: NSString;
function kSCPropNetIPSecXAuthPassword: NSString;
function kSCPropNetIPSecXAuthPasswordEncryption: NSString;
function kSCValNetIPSecAuthenticationMethodSharedSecret: NSString;
function kSCValNetIPSecAuthenticationMethodCertificate: NSString;
function kSCValNetIPSecAuthenticationMethodHybrid: NSString;
function kSCValNetIPSecLocalIdentifierTypeKeyID: NSString;
function kSCValNetIPSecSharedSecretEncryptionKeychain: NSString;
function kSCValNetIPSecXAuthPasswordEncryptionKeychain: NSString;
function kSCValNetIPSecXAuthPasswordEncryptionPrompt: NSString;
function kSCPropNetIPv4Addresses: NSString;
function kSCPropNetIPv4ConfigMethod: NSString;
function kSCPropNetIPv4DHCPClientID: NSString;
function kSCPropNetIPv4Router: NSString;
function kSCPropNetIPv4SubnetMasks: NSString;
function kSCPropNetIPv4DestAddresses: NSString;
function kSCPropNetIPv4BroadcastAddresses: NSString;
function kSCValNetIPv4ConfigMethodAutomatic: NSString;
function kSCValNetIPv4ConfigMethodBOOTP: NSString;
function kSCValNetIPv4ConfigMethodDHCP: NSString;
function kSCValNetIPv4ConfigMethodINFORM: NSString;
function kSCValNetIPv4ConfigMethodLinkLocal: NSString;
function kSCValNetIPv4ConfigMethodManual: NSString;
function kSCValNetIPv4ConfigMethodPPP: NSString;
function kSCPropNetIPv6Addresses: NSString;
function kSCPropNetIPv6ConfigMethod: NSString;
function kSCPropNetIPv6DestAddresses: NSString;
function kSCPropNetIPv6Flags: NSString;
function kSCPropNetIPv6PrefixLength: NSString;
function kSCPropNetIPv6Router: NSString;
function kSCValNetIPv6ConfigMethodAutomatic: NSString;
function kSCValNetIPv6ConfigMethodLinkLocal: NSString;
function kSCValNetIPv6ConfigMethodManual: NSString;
function kSCValNetIPv6ConfigMethodRouterAdvertisement: NSString;
function kSCValNetIPv6ConfigMethod6to4: NSString;
function kSCPropNet6to4Relay: NSString;
function kSCPropNetLinkActive: NSString;
function kSCPropNetLinkDetaching: NSString;
function kSCPropNetModemAccessPointName: NSString;
function kSCPropNetModemConnectionPersonality: NSString;
function kSCPropNetModemConnectionScript: NSString;
function kSCPropNetModemConnectSpeed: NSString;
function kSCPropNetModemDataCompression: NSString;
function kSCPropNetModemDeviceContextID: NSString;
function kSCPropNetModemDeviceModel: NSString;
function kSCPropNetModemDeviceVendor: NSString;
function kSCPropNetModemDialMode: NSString;
function kSCPropNetModemErrorCorrection: NSString;
function kSCPropNetModemHoldCallWaitingAudibleAlert: NSString;
function kSCPropNetModemHoldDisconnectOnAnswer: NSString;
function kSCPropNetModemHoldEnabled: NSString;
function kSCPropNetModemHoldReminder: NSString;
function kSCPropNetModemHoldReminderTime: NSString;
function kSCPropNetModemNote: NSString;
function kSCPropNetModemPulseDial: NSString;
function kSCPropNetModemSpeaker: NSString;
function kSCPropNetModemSpeed: NSString;
function kSCValNetModemDialModeIgnoreDialTone: NSString;
function kSCValNetModemDialModeManual: NSString;
function kSCValNetModemDialModeWaitForDialTone: NSString;
function kSCPropNetPPPACSPEnabled: NSString;
function kSCPropNetPPPConnectTime: NSString;
function kSCPropNetPPPDeviceLastCause: NSString;
function kSCPropNetPPPDialOnDemand: NSString;
function kSCPropNetPPPDisconnectOnFastUserSwitch: NSString;
function kSCPropNetPPPDisconnectOnIdle: NSString;
function kSCPropNetPPPDisconnectOnIdleTimer: NSString;
function kSCPropNetPPPDisconnectOnLogout: NSString;
function kSCPropNetPPPDisconnectOnSleep: NSString;
function kSCPropNetPPPDisconnectTime: NSString;
function kSCPropNetPPPIdleReminderTimer: NSString;
function kSCPropNetPPPIdleReminder: NSString;
function kSCPropNetPPPLastCause: NSString;
function kSCPropNetPPPLogfile: NSString;
function kSCPropNetPPPPlugins: NSString;
function kSCPropNetPPPRetryConnectTime: NSString;
function kSCPropNetPPPSessionTimer: NSString;
function kSCPropNetPPPStatus: NSString;
function kSCPropNetPPPUseSessionTimer: NSString;
function kSCPropNetPPPVerboseLogging: NSString;
function kSCPropNetPPPAuthEAPPlugins: NSString;
function kSCPropNetPPPAuthName: NSString;
function kSCPropNetPPPAuthPassword: NSString;
function kSCPropNetPPPAuthPasswordEncryption: NSString;
function kSCPropNetPPPAuthPrompt: NSString;
function kSCPropNetPPPAuthProtocol: NSString;
function kSCValNetPPPAuthPasswordEncryptionKeychain: NSString;
function kSCValNetPPPAuthPasswordEncryptionToken: NSString;
function kSCValNetPPPAuthPromptBefore: NSString;
function kSCValNetPPPAuthPromptAfter: NSString;
function kSCValNetPPPAuthProtocolCHAP: NSString;
function kSCValNetPPPAuthProtocolEAP: NSString;
function kSCValNetPPPAuthProtocolMSCHAP1: NSString;
function kSCValNetPPPAuthProtocolMSCHAP2: NSString;
function kSCValNetPPPAuthProtocolPAP: NSString;
function kSCPropNetPPPCommAlternateRemoteAddress: NSString;
function kSCPropNetPPPCommConnectDelay: NSString;
function kSCPropNetPPPCommDisplayTerminalWindow: NSString;
function kSCPropNetPPPCommRedialCount: NSString;
function kSCPropNetPPPCommRedialEnabled: NSString;
function kSCPropNetPPPCommRedialInterval: NSString;
function kSCPropNetPPPCommRemoteAddress: NSString;
function kSCPropNetPPPCommTerminalScript: NSString;
function kSCPropNetPPPCommUseTerminalScript: NSString;
function kSCPropNetPPPCCPEnabled: NSString;
function kSCPropNetPPPCCPMPPE40Enabled: NSString;
function kSCPropNetPPPCCPMPPE128Enabled: NSString;
function kSCPropNetPPPIPCPCompressionVJ: NSString;
function kSCPropNetPPPIPCPUsePeerDNS: NSString;
function kSCPropNetPPPLCPEchoEnabled: NSString;
function kSCPropNetPPPLCPEchoFailure: NSString;
function kSCPropNetPPPLCPEchoInterval: NSString;
function kSCPropNetPPPLCPCompressionACField: NSString;
function kSCPropNetPPPLCPCompressionPField: NSString;
function kSCPropNetPPPLCPMRU: NSString;
function kSCPropNetPPPLCPMTU: NSString;
function kSCPropNetPPPLCPReceiveACCM: NSString;
function kSCPropNetPPPLCPTransmitACCM: NSString;
function kSCPropNetL2TPIPSecSharedSecret: NSString;
function kSCPropNetL2TPIPSecSharedSecretEncryption: NSString;
function kSCPropNetL2TPTransport: NSString;
function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: NSString;
function kSCValNetL2TPTransportIP: NSString;
function kSCValNetL2TPTransportIPSec: NSString;
function kSCPropNetProxiesExceptionsList: NSString;
function kSCPropNetProxiesExcludeSimpleHostnames: NSString;
function kSCPropNetProxiesFTPEnable: NSString;
function kSCPropNetProxiesFTPPassive: NSString;
function kSCPropNetProxiesFTPPort: NSString;
function kSCPropNetProxiesFTPProxy: NSString;
function kSCPropNetProxiesGopherEnable: NSString;
function kSCPropNetProxiesGopherPort: NSString;
function kSCPropNetProxiesGopherProxy: NSString;
function kSCPropNetProxiesHTTPEnable: NSString;
function kSCPropNetProxiesHTTPPort: NSString;
function kSCPropNetProxiesHTTPProxy: NSString;
function kSCPropNetProxiesHTTPSEnable: NSString;
function kSCPropNetProxiesHTTPSPort: NSString;
function kSCPropNetProxiesHTTPSProxy: NSString;
function kSCPropNetProxiesRTSPEnable: NSString;
function kSCPropNetProxiesRTSPPort: NSString;
function kSCPropNetProxiesRTSPProxy: NSString;
function kSCPropNetProxiesSOCKSEnable: NSString;
function kSCPropNetProxiesSOCKSPort: NSString;
function kSCPropNetProxiesSOCKSProxy: NSString;
function kSCPropNetProxiesProxyAutoConfigEnable: NSString;
function kSCPropNetProxiesProxyAutoConfigJavaScript: NSString;
function kSCPropNetProxiesProxyAutoConfigURLString: NSString;
function kSCPropNetProxiesProxyAutoDiscoveryEnable: NSString;
function kSCPropNetSMBNetBIOSName: NSString;
function kSCPropNetSMBNetBIOSNodeType: NSString;
function kSCPropNetSMBNetBIOSScope: NSString;
function kSCPropNetSMBWINSAddresses: NSString;
function kSCPropNetSMBWorkgroup: NSString;
function kSCValNetSMBNetBIOSNodeTypeBroadcast: NSString;
function kSCValNetSMBNetBIOSNodeTypePeer: NSString;
function kSCValNetSMBNetBIOSNodeTypeMixed: NSString;
function kSCValNetSMBNetBIOSNodeTypeHybrid: NSString;
function kSCEntUsersConsoleUser: NSString;
function kSCPropSystemComputerName: NSString;
function kSCPropSystemComputerNameEncoding: NSString;
function kSCDynamicStoreDomainFile: NSString;
function kSCDynamicStoreDomainPlugin: NSString;
function kSCDynamicStoreDomainSetup: NSString;
function kSCDynamicStoreDomainState: NSString;
function kSCDynamicStoreDomainPrefs: NSString;
function kSCDynamicStorePropSetupCurrentSet: NSString;
function kSCDynamicStorePropSetupLastUpdated: NSString;
function kSCDynamicStorePropNetInterfaces: NSString;
function kSCDynamicStorePropNetPrimaryInterface: NSString;
function kSCDynamicStorePropNetPrimaryService: NSString;
function kSCDynamicStorePropNetServiceIDs: NSString;
function kSCPropUsersConsoleUserName: NSString;
function kSCPropUsersConsoleUserUID: NSString;
function kSCPropUsersConsoleUserGID: NSString;
function kCFErrorDomainSystemConfiguration: NSString;
function kSCNetworkInterfaceType6to4: NSString;
function kSCNetworkInterfaceTypeBluetooth: NSString;
function kSCNetworkInterfaceTypeBond: NSString;
function kSCNetworkInterfaceTypeEthernet: NSString;
function kSCNetworkInterfaceTypeFireWire: NSString;
function kSCNetworkInterfaceTypeIEEE80211: NSString;
function kSCNetworkInterfaceTypeIPSec: NSString;
function kSCNetworkInterfaceTypeIrDA: NSString;
function kSCNetworkInterfaceTypeL2TP: NSString;
function kSCNetworkInterfaceTypeModem: NSString;
function kSCNetworkInterfaceTypePPP: NSString;
function kSCNetworkInterfaceTypePPTP: NSString;
function kSCNetworkInterfaceTypeSerial: NSString;
function kSCNetworkInterfaceTypeVLAN: NSString;
function kSCNetworkInterfaceTypeWWAN: NSString;
function kSCNetworkInterfaceTypeIPv4: NSString;
function kSCNetworkInterfaceIPv4: NSString;
function kSCBondStatusDeviceAggregationStatus: NSString;
function kSCBondStatusDeviceCollecting: NSString;
function kSCBondStatusDeviceDistributing: NSString;
function kSCNetworkProtocolTypeDNS: NSString;
function kSCNetworkProtocolTypeIPv4: NSString;
function kSCNetworkProtocolTypeIPv6: NSString;
function kSCNetworkProtocolTypeProxies: NSString;
function kSCNetworkProtocolTypeSMB: NSString;

const
  libSystemConfiguration = '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';

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
function DHCPClientPreferencesSetApplicationOptions(applicationID: CFStringRef; options: PByte; count: CFIndex): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'DHCPClientPreferencesSetApplicationOptions';
function DHCPClientPreferencesCopyApplicationOptions(applicationID: CFStringRef; count: PCFIndex): PByte; cdecl;
  external libSystemConfiguration name _PU + 'DHCPClientPreferencesCopyApplicationOptions';
function SCDynamicStoreGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreGetTypeID';
function SCDynamicStoreCreate(allocator: CFAllocatorRef; name: CFStringRef; callout: SCDynamicStoreCallBack;
  context: PSCDynamicStoreContext): SCDynamicStoreRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCreate';
function SCDynamicStoreCreateWithOptions(allocator: CFAllocatorRef; name: CFStringRef; storeOptions: CFDictionaryRef;
  callout: SCDynamicStoreCallBack; context: PSCDynamicStoreContext): SCDynamicStoreRef; cdecl;
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
function SCDynamicStoreCopyDHCPInfo(store: SCDynamicStoreRef; serviceID: CFStringRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyDHCPInfo';
function DHCPInfoGetOptionData(info: CFDictionaryRef; code: Byte): CFDataRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetOptionData';
function DHCPInfoGetLeaseStartTime(info: CFDictionaryRef): CFDateRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetLeaseStartTime';
function DHCPInfoGetLeaseExpirationTime(info: CFDictionaryRef): CFDateRef; cdecl;
  external libSystemConfiguration name _PU + 'DHCPInfoGetLeaseExpirationTime';
function SCDynamicStoreCopyComputerName(store: SCDynamicStoreRef; nameEncoding: PLongWord): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyComputerName';
function SCDynamicStoreCopyConsoleUser(store: SCDynamicStoreRef; uid: PCardinal; gid: PCardinal): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyConsoleUser';
function SCDynamicStoreCopyLocalHostName(store: SCDynamicStoreRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyLocalHostName';
function SCDynamicStoreCopyLocation(store: SCDynamicStoreRef): CFStringRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyLocation';
function SCDynamicStoreCopyProxies(store: SCDynamicStoreRef): CFDictionaryRef; cdecl;
  external libSystemConfiguration name _PU + 'SCDynamicStoreCopyProxies';
function SCDynamicStoreKeyCreate(allocator: CFAllocatorRef; fmt: CFStringRef): CFStringRef; cdecl;
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
function SCNetworkCheckReachabilityByAddress(address: Pointer; addrlen: LongWord; flags: PSCNetworkConnectionFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkCheckReachabilityByAddress';
function SCNetworkCheckReachabilityByName(nodename: MarshaledAString; flags: PSCNetworkConnectionFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkCheckReachabilityByName';
function SCNetworkInterfaceRefreshConfiguration(ifname: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkInterfaceRefreshConfiguration';
function SCPreferencesGetTypeID: CFTypeID; cdecl; external libSystemConfiguration name _PU + 'SCPreferencesGetTypeID';
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
procedure SCPreferencesSynchronize(prefs: SCPreferencesRef); cdecl; external libSystemConfiguration name _PU + 'SCPreferencesSynchronize';
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
function SCPreferencesSetComputerName(prefs: SCPreferencesRef; name: CFStringRef; nameEncoding: CFStringEncoding): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetComputerName';
function SCPreferencesSetLocalHostName(prefs: SCPreferencesRef; name: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCPreferencesSetLocalHostName';
function SCNetworkReachabilityCreateWithAddress(allocator: CFAllocatorRef; address: Pointer): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddress';
function SCNetworkReachabilityCreateWithAddressPair(allocator: CFAllocatorRef; localAddress: Pointer;
  remoteAddress: Pointer): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithAddressPair';
function SCNetworkReachabilityCreateWithName(allocator: CFAllocatorRef; nodename: MarshaledAString): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityCreateWithName';
function SCNetworkReachabilityGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetTypeID';
function SCNetworkReachabilityGetFlags(target: SCNetworkReachabilityRef; flags: PSCNetworkReachabilityFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetFlags';
function SCNetworkReachabilitySetCallback(target: SCNetworkReachabilityRef; callout: SCNetworkReachabilityCallBack;
// function SCNetworkReachabilitySetCallback(target: SCNetworkReachabilityRef; callout: PSCNetworkReachabilityCallBack;
  context: PSCNetworkReachabilityContext): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetCallback';
function SCNetworkReachabilityScheduleWithRunLoop(target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityScheduleWithRunLoop';
function SCNetworkReachabilityUnscheduleFromRunLoop(target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef;
  runLoopMode: CFStringRef): Boolean; cdecl;
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
function SCNetworkConnectionScheduleWithRunLoop(connection: SCNetworkConnectionRef; runLoop: CFRunLoopRef;
  runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionScheduleWithRunLoop';
function SCNetworkConnectionUnscheduleFromRunLoop(connection: SCNetworkConnectionRef; runLoop: CFRunLoopRef;
  runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionUnscheduleFromRunLoop';
function SCNetworkConnectionSetDispatchQueue(connection: SCNetworkConnectionRef; queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkConnectionSetDispatchQueue';
function SCCopyLastError: CFErrorRef; cdecl; external libSystemConfiguration name _PU + 'SCCopyLastError';
function SCError: Integer; cdecl; external libSystemConfiguration name _PU + 'SCError';
function SCErrorString(status: Integer): MarshaledAString; cdecl; external libSystemConfiguration name _PU + 'SCErrorString';
function SCNetworkInterfaceGetTypeID: CFTypeID; cdecl; external libSystemConfiguration name _PU + 'SCNetworkInterfaceGetTypeID';
function SCNetworkInterfaceCopyAll: CFArrayRef; cdecl; external libSystemConfiguration name _PU + 'SCNetworkInterfaceCopyAll';
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
function SCNetworkInterfaceSetMediaOptions(&interface: SCNetworkInterfaceRef; subType: CFStringRef; options: CFArrayRef): Boolean; cdecl;
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

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  // Posix
  Posix.Dlfcn;

var
  SystemConfigurationModule: THandle;
{$ENDIF IOS}

function kCNNetworkInfoKeySSIDData: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kCNNetworkInfoKeySSIDData');
end;

function kCNNetworkInfoKeySSID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kCNNetworkInfoKeySSID');
end;

function kCNNetworkInfoKeyBSSID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kCNNetworkInfoKeyBSSID');
end;

function kSCDynamicStoreUseSessionKeys: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreUseSessionKeys');
end;

function kSCResvLink: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCResvLink');
end;

function kSCResvInactive: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCResvInactive');
end;

function kSCPropInterfaceName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropInterfaceName');
end;

function kSCPropMACAddress: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropMACAddress');
end;

function kSCPropUserDefinedName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropUserDefinedName');
end;

function kSCPropVersion: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropVersion');
end;

function kSCPrefCurrentSet: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPrefCurrentSet');
end;

function kSCPrefNetworkServices: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPrefNetworkServices');
end;

function kSCPrefSets: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPrefSets');
end;

function kSCPrefSystem: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPrefSystem');
end;

function kSCCompNetwork: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompNetwork');
end;

function kSCCompService: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompService');
end;

function kSCCompGlobal: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompGlobal');
end;

function kSCCompHostNames: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompHostNames');
end;

function kSCCompInterface: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompInterface');
end;

function kSCCompSystem: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompSystem');
end;

function kSCCompUsers: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompUsers');
end;

function kSCCompAnyRegex: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCCompAnyRegex');
end;

function kSCEntNetAirPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetAirPort');
end;

function kSCEntNetDHCP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetDHCP');
end;

function kSCEntNetDNS: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetDNS');
end;

function kSCEntNetEthernet: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetEthernet');
end;

function kSCEntNetFireWire: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetFireWire');
end;

function kSCEntNetInterface: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetInterface');
end;

function kSCEntNetIPSec: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetIPSec');
end;

function kSCEntNetIPv4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetIPv4');
end;

function kSCEntNetIPv6: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetIPv6');
end;

function kSCEntNetL2TP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetL2TP');
end;

function kSCEntNetLink: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetLink');
end;

function kSCEntNetModem: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetModem');
end;

function kSCEntNetPPP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetPPP');
end;

function kSCEntNetPPPoE: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetPPPoE');
end;

function kSCEntNetPPPSerial: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetPPPSerial');
end;

function kSCEntNetPPTP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetPPTP');
end;

function kSCEntNetProxies: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetProxies');
end;

function kSCEntNetSMB: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNetSMB');
end;

function kSCEntNet6to4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntNet6to4');
end;

function kSCPropNetOverridePrimary: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetOverridePrimary');
end;

function kSCPropNetServiceOrder: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetServiceOrder');
end;

function kSCPropNetPPPOverridePrimary: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPOverridePrimary');
end;

function kSCPropNetInterfaces: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaces');
end;

function kSCPropNetLocalHostName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetLocalHostName');
end;

function kSCPropNetAirPortAllowNetCreation: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortAllowNetCreation');
end;

function kSCPropNetAirPortAuthPassword: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPassword');
end;

function kSCPropNetAirPortAuthPasswordEncryption: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPasswordEncryption');
end;

function kSCPropNetAirPortJoinMode: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortJoinMode');
end;

function kSCPropNetAirPortPowerEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortPowerEnabled');
end;

function kSCPropNetAirPortPreferredNetwork: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortPreferredNetwork');
end;

function kSCPropNetAirPortSavePasswords: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetAirPortSavePasswords');
end;

function kSCValNetAirPortJoinModeAutomatic: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeAutomatic');
end;

function kSCValNetAirPortJoinModePreferred: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortJoinModePreferred');
end;

function kSCValNetAirPortJoinModeRanked: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRanked');
end;

function kSCValNetAirPortJoinModeRecent: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRecent');
end;

function kSCValNetAirPortJoinModeStrongest: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeStrongest');
end;

function kSCValNetAirPortAuthPasswordEncryptionKeychain: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetAirPortAuthPasswordEncryptionKeychain');
end;

function kSCPropNetDNSDomainName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSDomainName');
end;

function kSCPropNetDNSOptions: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSOptions');
end;

function kSCPropNetDNSSearchDomains: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSSearchDomains');
end;

function kSCPropNetDNSSearchOrder: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSSearchOrder');
end;

function kSCPropNetDNSServerAddresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSServerAddresses');
end;

function kSCPropNetDNSServerPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSServerPort');
end;

function kSCPropNetDNSServerTimeout: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSServerTimeout');
end;

function kSCPropNetDNSSortList: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSSortList');
end;

function kSCPropNetDNSSupplementalMatchDomains: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchDomains');
end;

function kSCPropNetDNSSupplementalMatchOrders: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchOrders');
end;

function kSCPropNetEthernetMediaSubType: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetEthernetMediaSubType');
end;

function kSCPropNetEthernetMediaOptions: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetEthernetMediaOptions');
end;

function kSCPropNetEthernetMTU: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetEthernetMTU');
end;

function kSCPropNetInterfaceDeviceName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaceDeviceName');
end;

function kSCPropNetInterfaceHardware: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaceHardware');
end;

function kSCPropNetInterfaceType: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaceType');
end;

function kSCPropNetInterfaceSubType: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaceSubType');
end;

function kSCPropNetInterfaceSupportsModemOnHold: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetInterfaceSupportsModemOnHold');
end;

function kSCValNetInterfaceTypeEthernet: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceTypeEthernet');
end;

function kSCValNetInterfaceTypeFireWire: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceTypeFireWire');
end;

function kSCValNetInterfaceTypePPP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceTypePPP');
end;

function kSCValNetInterfaceType6to4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceType6to4');
end;

function kSCValNetInterfaceTypeIPSec: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceTypeIPSec');
end;

function kSCValNetInterfaceSubTypePPPoE: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPoE');
end;

function kSCValNetInterfaceSubTypePPPSerial: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPSerial');
end;

function kSCValNetInterfaceSubTypePPTP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPTP');
end;

function kSCValNetInterfaceSubTypeL2TP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypeL2TP');
end;

function kSCPropNetIPSecAuthenticationMethod: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecAuthenticationMethod');
end;

function kSCPropNetIPSecLocalCertificate: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecLocalCertificate');
end;

function kSCPropNetIPSecLocalIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifier');
end;

function kSCPropNetIPSecLocalIdentifierType: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifierType');
end;

function kSCPropNetIPSecSharedSecret: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecret');
end;

function kSCPropNetIPSecSharedSecretEncryption: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecretEncryption');
end;

function kSCPropNetIPSecConnectTime: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecConnectTime');
end;

function kSCPropNetIPSecRemoteAddress: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecRemoteAddress');
end;

function kSCPropNetIPSecStatus: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecStatus');
end;

function kSCPropNetIPSecXAuthEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthEnabled');
end;

function kSCPropNetIPSecXAuthName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthName');
end;

function kSCPropNetIPSecXAuthPassword: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPassword');
end;

function kSCPropNetIPSecXAuthPasswordEncryption: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPasswordEncryption');
end;

function kSCValNetIPSecAuthenticationMethodSharedSecret: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodSharedSecret');
end;

function kSCValNetIPSecAuthenticationMethodCertificate: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodCertificate');
end;

function kSCValNetIPSecAuthenticationMethodHybrid: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodHybrid');
end;

function kSCValNetIPSecLocalIdentifierTypeKeyID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecLocalIdentifierTypeKeyID');
end;

function kSCValNetIPSecSharedSecretEncryptionKeychain: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecSharedSecretEncryptionKeychain');
end;

function kSCValNetIPSecXAuthPasswordEncryptionKeychain: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionKeychain');
end;

function kSCValNetIPSecXAuthPasswordEncryptionPrompt: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionPrompt');
end;

function kSCPropNetIPv4Addresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4Addresses');
end;

function kSCPropNetIPv4ConfigMethod: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4ConfigMethod');
end;

function kSCPropNetIPv4DHCPClientID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4DHCPClientID');
end;

function kSCPropNetIPv4Router: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4Router');
end;

function kSCPropNetIPv4SubnetMasks: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4SubnetMasks');
end;

function kSCPropNetIPv4DestAddresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4DestAddresses');
end;

function kSCPropNetIPv4BroadcastAddresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv4BroadcastAddresses');
end;

function kSCValNetIPv4ConfigMethodAutomatic: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodAutomatic');
end;

function kSCValNetIPv4ConfigMethodBOOTP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodBOOTP');
end;

function kSCValNetIPv4ConfigMethodDHCP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodDHCP');
end;

function kSCValNetIPv4ConfigMethodINFORM: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodINFORM');
end;

function kSCValNetIPv4ConfigMethodLinkLocal: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodLinkLocal');
end;

function kSCValNetIPv4ConfigMethodManual: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodManual');
end;

function kSCValNetIPv4ConfigMethodPPP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodPPP');
end;

function kSCPropNetIPv6Addresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6Addresses');
end;

function kSCPropNetIPv6ConfigMethod: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6ConfigMethod');
end;

function kSCPropNetIPv6DestAddresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6DestAddresses');
end;

function kSCPropNetIPv6Flags: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6Flags');
end;

function kSCPropNetIPv6PrefixLength: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6PrefixLength');
end;

function kSCPropNetIPv6Router: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetIPv6Router');
end;

function kSCValNetIPv6ConfigMethodAutomatic: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodAutomatic');
end;

function kSCValNetIPv6ConfigMethodLinkLocal: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodLinkLocal');
end;

function kSCValNetIPv6ConfigMethodManual: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodManual');
end;

function kSCValNetIPv6ConfigMethodRouterAdvertisement: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodRouterAdvertisement');
end;

function kSCValNetIPv6ConfigMethod6to4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethod6to4');
end;

function kSCPropNet6to4Relay: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNet6to4Relay');
end;

function kSCPropNetLinkActive: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetLinkActive');
end;

function kSCPropNetLinkDetaching: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetLinkDetaching');
end;

function kSCPropNetModemAccessPointName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemAccessPointName');
end;

function kSCPropNetModemConnectionPersonality: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemConnectionPersonality');
end;

function kSCPropNetModemConnectionScript: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemConnectionScript');
end;

function kSCPropNetModemConnectSpeed: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemConnectSpeed');
end;

function kSCPropNetModemDataCompression: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemDataCompression');
end;

function kSCPropNetModemDeviceContextID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemDeviceContextID');
end;

function kSCPropNetModemDeviceModel: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemDeviceModel');
end;

function kSCPropNetModemDeviceVendor: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemDeviceVendor');
end;

function kSCPropNetModemDialMode: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemDialMode');
end;

function kSCPropNetModemErrorCorrection: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemErrorCorrection');
end;

function kSCPropNetModemHoldCallWaitingAudibleAlert: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemHoldCallWaitingAudibleAlert');
end;

function kSCPropNetModemHoldDisconnectOnAnswer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemHoldDisconnectOnAnswer');
end;

function kSCPropNetModemHoldEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemHoldEnabled');
end;

function kSCPropNetModemHoldReminder: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemHoldReminder');
end;

function kSCPropNetModemHoldReminderTime: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemHoldReminderTime');
end;

function kSCPropNetModemNote: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemNote');
end;

function kSCPropNetModemPulseDial: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemPulseDial');
end;

function kSCPropNetModemSpeaker: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemSpeaker');
end;

function kSCPropNetModemSpeed: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetModemSpeed');
end;

function kSCValNetModemDialModeIgnoreDialTone: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetModemDialModeIgnoreDialTone');
end;

function kSCValNetModemDialModeManual: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetModemDialModeManual');
end;

function kSCValNetModemDialModeWaitForDialTone: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetModemDialModeWaitForDialTone');
end;

function kSCPropNetPPPACSPEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPACSPEnabled');
end;

function kSCPropNetPPPConnectTime: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPConnectTime');
end;

function kSCPropNetPPPDeviceLastCause: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDeviceLastCause');
end;

function kSCPropNetPPPDialOnDemand: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDialOnDemand');
end;

function kSCPropNetPPPDisconnectOnFastUserSwitch: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnFastUserSwitch');
end;

function kSCPropNetPPPDisconnectOnIdle: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdle');
end;

function kSCPropNetPPPDisconnectOnIdleTimer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdleTimer');
end;

function kSCPropNetPPPDisconnectOnLogout: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnLogout');
end;

function kSCPropNetPPPDisconnectOnSleep: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnSleep');
end;

function kSCPropNetPPPDisconnectTime: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectTime');
end;

function kSCPropNetPPPIdleReminderTimer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminderTimer');
end;

function kSCPropNetPPPIdleReminder: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminder');
end;

function kSCPropNetPPPLastCause: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLastCause');
end;

function kSCPropNetPPPLogfile: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLogfile');
end;

function kSCPropNetPPPPlugins: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPPlugins');
end;

function kSCPropNetPPPRetryConnectTime: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPRetryConnectTime');
end;

function kSCPropNetPPPSessionTimer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPSessionTimer');
end;

function kSCPropNetPPPStatus: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPStatus');
end;

function kSCPropNetPPPUseSessionTimer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPUseSessionTimer');
end;

function kSCPropNetPPPVerboseLogging: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPVerboseLogging');
end;

function kSCPropNetPPPAuthEAPPlugins: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthEAPPlugins');
end;

function kSCPropNetPPPAuthName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthName');
end;

function kSCPropNetPPPAuthPassword: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthPassword');
end;

function kSCPropNetPPPAuthPasswordEncryption: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthPasswordEncryption');
end;

function kSCPropNetPPPAuthPrompt: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthPrompt');
end;

function kSCPropNetPPPAuthProtocol: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPAuthProtocol');
end;

function kSCValNetPPPAuthPasswordEncryptionKeychain: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionKeychain');
end;

function kSCValNetPPPAuthPasswordEncryptionToken: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionToken');
end;

function kSCValNetPPPAuthPromptBefore: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptBefore');
end;

function kSCValNetPPPAuthPromptAfter: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptAfter');
end;

function kSCValNetPPPAuthProtocolCHAP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolCHAP');
end;

function kSCValNetPPPAuthProtocolEAP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolEAP');
end;

function kSCValNetPPPAuthProtocolMSCHAP1: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP1');
end;

function kSCValNetPPPAuthProtocolMSCHAP2: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP2');
end;

function kSCValNetPPPAuthProtocolPAP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolPAP');
end;

function kSCPropNetPPPCommAlternateRemoteAddress: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommAlternateRemoteAddress');
end;

function kSCPropNetPPPCommConnectDelay: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommConnectDelay');
end;

function kSCPropNetPPPCommDisplayTerminalWindow: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommDisplayTerminalWindow');
end;

function kSCPropNetPPPCommRedialCount: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialCount');
end;

function kSCPropNetPPPCommRedialEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialEnabled');
end;

function kSCPropNetPPPCommRedialInterval: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialInterval');
end;

function kSCPropNetPPPCommRemoteAddress: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommRemoteAddress');
end;

function kSCPropNetPPPCommTerminalScript: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommTerminalScript');
end;

function kSCPropNetPPPCommUseTerminalScript: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCommUseTerminalScript');
end;

function kSCPropNetPPPCCPEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCCPEnabled');
end;

function kSCPropNetPPPCCPMPPE40Enabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE40Enabled');
end;

function kSCPropNetPPPCCPMPPE128Enabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE128Enabled');
end;

function kSCPropNetPPPIPCPCompressionVJ: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPIPCPCompressionVJ');
end;

function kSCPropNetPPPIPCPUsePeerDNS: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPIPCPUsePeerDNS');
end;

function kSCPropNetPPPLCPEchoEnabled: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoEnabled');
end;

function kSCPropNetPPPLCPEchoFailure: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoFailure');
end;

function kSCPropNetPPPLCPEchoInterval: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoInterval');
end;

function kSCPropNetPPPLCPCompressionACField: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionACField');
end;

function kSCPropNetPPPLCPCompressionPField: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionPField');
end;

function kSCPropNetPPPLCPMRU: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPMRU');
end;

function kSCPropNetPPPLCPMTU: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPMTU');
end;

function kSCPropNetPPPLCPReceiveACCM: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPReceiveACCM');
end;

function kSCPropNetPPPLCPTransmitACCM: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetPPPLCPTransmitACCM');
end;

function kSCPropNetL2TPIPSecSharedSecret: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecret');
end;

function kSCPropNetL2TPIPSecSharedSecretEncryption: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecretEncryption');
end;

function kSCPropNetL2TPTransport: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetL2TPTransport');
end;

function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetL2TPIPSecSharedSecretEncryptionKeychain');
end;

function kSCValNetL2TPTransportIP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetL2TPTransportIP');
end;

function kSCValNetL2TPTransportIPSec: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetL2TPTransportIPSec');
end;

function kSCPropNetProxiesExceptionsList: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesExceptionsList');
end;

function kSCPropNetProxiesExcludeSimpleHostnames: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesExcludeSimpleHostnames');
end;

function kSCPropNetProxiesFTPEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesFTPEnable');
end;

function kSCPropNetProxiesFTPPassive: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPassive');
end;

function kSCPropNetProxiesFTPPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPort');
end;

function kSCPropNetProxiesFTPProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesFTPProxy');
end;

function kSCPropNetProxiesGopherEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesGopherEnable');
end;

function kSCPropNetProxiesGopherPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesGopherPort');
end;

function kSCPropNetProxiesGopherProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesGopherProxy');
end;

function kSCPropNetProxiesHTTPEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPEnable');
end;

function kSCPropNetProxiesHTTPPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPPort');
end;

function kSCPropNetProxiesHTTPProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPProxy');
end;

function kSCPropNetProxiesHTTPSEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSEnable');
end;

function kSCPropNetProxiesHTTPSPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSPort');
end;

function kSCPropNetProxiesHTTPSProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSProxy');
end;

function kSCPropNetProxiesRTSPEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPEnable');
end;

function kSCPropNetProxiesRTSPPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPPort');
end;

function kSCPropNetProxiesRTSPProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPProxy');
end;

function kSCPropNetProxiesSOCKSEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSEnable');
end;

function kSCPropNetProxiesSOCKSPort: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSPort');
end;

function kSCPropNetProxiesSOCKSProxy: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSProxy');
end;

function kSCPropNetProxiesProxyAutoConfigEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigEnable');
end;

function kSCPropNetProxiesProxyAutoConfigJavaScript: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigJavaScript');
end;

function kSCPropNetProxiesProxyAutoConfigURLString: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigURLString');
end;

function kSCPropNetProxiesProxyAutoDiscoveryEnable: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoDiscoveryEnable');
end;

function kSCPropNetSMBNetBIOSName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSName');
end;

function kSCPropNetSMBNetBIOSNodeType: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSNodeType');
end;

function kSCPropNetSMBNetBIOSScope: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSScope');
end;

function kSCPropNetSMBWINSAddresses: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetSMBWINSAddresses');
end;

function kSCPropNetSMBWorkgroup: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropNetSMBWorkgroup');
end;

function kSCValNetSMBNetBIOSNodeTypeBroadcast: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeBroadcast');
end;

function kSCValNetSMBNetBIOSNodeTypePeer: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypePeer');
end;

function kSCValNetSMBNetBIOSNodeTypeMixed: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeMixed');
end;

function kSCValNetSMBNetBIOSNodeTypeHybrid: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeHybrid');
end;

function kSCEntUsersConsoleUser: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCEntUsersConsoleUser');
end;

function kSCPropSystemComputerName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropSystemComputerName');
end;

function kSCPropSystemComputerNameEncoding: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropSystemComputerNameEncoding');
end;

function kSCDynamicStoreDomainFile: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreDomainFile');
end;

function kSCDynamicStoreDomainPlugin: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreDomainPlugin');
end;

function kSCDynamicStoreDomainSetup: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreDomainSetup');
end;

function kSCDynamicStoreDomainState: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreDomainState');
end;

function kSCDynamicStoreDomainPrefs: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStoreDomainPrefs');
end;

function kSCDynamicStorePropSetupCurrentSet: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropSetupCurrentSet');
end;

function kSCDynamicStorePropSetupLastUpdated: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropSetupLastUpdated');
end;

function kSCDynamicStorePropNetInterfaces: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropNetInterfaces');
end;

function kSCDynamicStorePropNetPrimaryInterface: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryInterface');
end;

function kSCDynamicStorePropNetPrimaryService: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryService');
end;

function kSCDynamicStorePropNetServiceIDs: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCDynamicStorePropNetServiceIDs');
end;

function kSCPropUsersConsoleUserName: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropUsersConsoleUserName');
end;

function kSCPropUsersConsoleUserUID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropUsersConsoleUserUID');
end;

function kSCPropUsersConsoleUserGID: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCPropUsersConsoleUserGID');
end;

function kCFErrorDomainSystemConfiguration: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kCFErrorDomainSystemConfiguration');
end;

function kSCNetworkInterfaceType6to4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceType6to4');
end;

function kSCNetworkInterfaceTypeBluetooth: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBluetooth');
end;

function kSCNetworkInterfaceTypeBond: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBond');
end;

function kSCNetworkInterfaceTypeEthernet: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeEthernet');
end;

function kSCNetworkInterfaceTypeFireWire: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeFireWire');
end;

function kSCNetworkInterfaceTypeIEEE80211: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIEEE80211');
end;

function kSCNetworkInterfaceTypeIPSec: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPSec');
end;

function kSCNetworkInterfaceTypeIrDA: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIrDA');
end;

function kSCNetworkInterfaceTypeL2TP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeL2TP');
end;

function kSCNetworkInterfaceTypeModem: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeModem');
end;

function kSCNetworkInterfaceTypePPP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPP');
end;

function kSCNetworkInterfaceTypePPTP: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPTP');
end;

function kSCNetworkInterfaceTypeSerial: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeSerial');
end;

function kSCNetworkInterfaceTypeVLAN: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeVLAN');
end;

function kSCNetworkInterfaceTypeWWAN: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeWWAN');
end;

function kSCNetworkInterfaceTypeIPv4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPv4');
end;

function kSCNetworkInterfaceIPv4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkInterfaceIPv4');
end;

function kSCBondStatusDeviceAggregationStatus: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCBondStatusDeviceAggregationStatus');
end;

function kSCBondStatusDeviceCollecting: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCBondStatusDeviceCollecting');
end;

function kSCBondStatusDeviceDistributing: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCBondStatusDeviceDistributing');
end;

function kSCNetworkProtocolTypeDNS: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkProtocolTypeDNS');
end;

function kSCNetworkProtocolTypeIPv4: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv4');
end;

function kSCNetworkProtocolTypeIPv6: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv6');
end;

function kSCNetworkProtocolTypeProxies: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkProtocolTypeProxies');
end;

function kSCNetworkProtocolTypeSMB: NSString;
begin
  Result := CocoaNSStringConst(libSystemConfiguration, 'kSCNetworkProtocolTypeSMB');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}
initialization
  SystemConfigurationModule := dlopen(MarshaledAString(libSystemConfiguration), RTLD_LAZY);

finalization
  dlclose(SystemConfigurationModule);
{$ENDIF IOS}

end.
