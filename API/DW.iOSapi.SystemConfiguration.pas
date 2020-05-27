unit DW.iOSapi.SystemConfiguration;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
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

function kCNNetworkInfoKeySSIDData: Pointer;
function kCNNetworkInfoKeySSID: Pointer;
function kCNNetworkInfoKeyBSSID: Pointer;
function kSCDynamicStoreUseSessionKeys: Pointer;
function kSCResvLink: Pointer;
function kSCResvInactive: Pointer;
function kSCPropInterfaceName: Pointer;
function kSCPropMACAddress: Pointer;
function kSCPropUserDefinedName: Pointer;
function kSCPropVersion: Pointer;
function kSCPrefCurrentSet: Pointer;
function kSCPrefNetworkServices: Pointer;
function kSCPrefSets: Pointer;
function kSCPrefSystem: Pointer;
function kSCCompNetwork: Pointer;
function kSCCompService: Pointer;
function kSCCompGlobal: Pointer;
function kSCCompHostNames: Pointer;
function kSCCompInterface: Pointer;
function kSCCompSystem: Pointer;
function kSCCompUsers: Pointer;
function kSCCompAnyRegex: Pointer;
function kSCEntNetAirPort: Pointer;
function kSCEntNetDHCP: Pointer;
function kSCEntNetDNS: Pointer;
function kSCEntNetEthernet: Pointer;
function kSCEntNetFireWire: Pointer;
function kSCEntNetInterface: Pointer;
function kSCEntNetIPSec: Pointer;
function kSCEntNetIPv4: Pointer;
function kSCEntNetIPv6: Pointer;
function kSCEntNetL2TP: Pointer;
function kSCEntNetLink: Pointer;
function kSCEntNetModem: Pointer;
function kSCEntNetPPP: Pointer;
function kSCEntNetPPPoE: Pointer;
function kSCEntNetPPPSerial: Pointer;
function kSCEntNetPPTP: Pointer;
function kSCEntNetProxies: Pointer;
function kSCEntNetSMB: Pointer;
function kSCEntNet6to4: Pointer;
function kSCPropNetOverridePrimary: Pointer;
function kSCPropNetServiceOrder: Pointer;
function kSCPropNetPPPOverridePrimary: Pointer;
function kSCPropNetInterfaces: Pointer;
function kSCPropNetLocalHostName: Pointer;
function kSCPropNetAirPortAllowNetCreation: Pointer;
function kSCPropNetAirPortAuthPassword: Pointer;
function kSCPropNetAirPortAuthPasswordEncryption: Pointer;
function kSCPropNetAirPortJoinMode: Pointer;
function kSCPropNetAirPortPowerEnabled: Pointer;
function kSCPropNetAirPortPreferredNetwork: Pointer;
function kSCPropNetAirPortSavePasswords: Pointer;
function kSCValNetAirPortJoinModeAutomatic: Pointer;
function kSCValNetAirPortJoinModePreferred: Pointer;
function kSCValNetAirPortJoinModeRanked: Pointer;
function kSCValNetAirPortJoinModeRecent: Pointer;
function kSCValNetAirPortJoinModeStrongest: Pointer;
function kSCValNetAirPortAuthPasswordEncryptionKeychain: Pointer;
function kSCPropNetDNSDomainName: Pointer;
function kSCPropNetDNSOptions: Pointer;
function kSCPropNetDNSSearchDomains: Pointer;
function kSCPropNetDNSSearchOrder: Pointer;
function kSCPropNetDNSServerAddresses: Pointer;
function kSCPropNetDNSServerPort: Pointer;
function kSCPropNetDNSServerTimeout: Pointer;
function kSCPropNetDNSSortList: Pointer;
function kSCPropNetDNSSupplementalMatchDomains: Pointer;
function kSCPropNetDNSSupplementalMatchOrders: Pointer;
function kSCPropNetEthernetMediaSubType: Pointer;
function kSCPropNetEthernetMediaOptions: Pointer;
function kSCPropNetEthernetMTU: Pointer;
function kSCPropNetInterfaceDeviceName: Pointer;
function kSCPropNetInterfaceHardware: Pointer;
function kSCPropNetInterfaceType: Pointer;
function kSCPropNetInterfaceSubType: Pointer;
function kSCPropNetInterfaceSupportsModemOnHold: Pointer;
function kSCValNetInterfaceTypeEthernet: Pointer;
function kSCValNetInterfaceTypeFireWire: Pointer;
function kSCValNetInterfaceTypePPP: Pointer;
function kSCValNetInterfaceType6to4: Pointer;
function kSCValNetInterfaceTypeIPSec: Pointer;
function kSCValNetInterfaceSubTypePPPoE: Pointer;
function kSCValNetInterfaceSubTypePPPSerial: Pointer;
function kSCValNetInterfaceSubTypePPTP: Pointer;
function kSCValNetInterfaceSubTypeL2TP: Pointer;
function kSCPropNetIPSecAuthenticationMethod: Pointer;
function kSCPropNetIPSecLocalCertificate: Pointer;
function kSCPropNetIPSecLocalIdentifier: Pointer;
function kSCPropNetIPSecLocalIdentifierType: Pointer;
function kSCPropNetIPSecSharedSecret: Pointer;
function kSCPropNetIPSecSharedSecretEncryption: Pointer;
function kSCPropNetIPSecConnectTime: Pointer;
function kSCPropNetIPSecRemoteAddress: Pointer;
function kSCPropNetIPSecStatus: Pointer;
function kSCPropNetIPSecXAuthEnabled: Pointer;
function kSCPropNetIPSecXAuthName: Pointer;
function kSCPropNetIPSecXAuthPassword: Pointer;
function kSCPropNetIPSecXAuthPasswordEncryption: Pointer;
function kSCValNetIPSecAuthenticationMethodSharedSecret: Pointer;
function kSCValNetIPSecAuthenticationMethodCertificate: Pointer;
function kSCValNetIPSecAuthenticationMethodHybrid: Pointer;
function kSCValNetIPSecLocalIdentifierTypeKeyID: Pointer;
function kSCValNetIPSecSharedSecretEncryptionKeychain: Pointer;
function kSCValNetIPSecXAuthPasswordEncryptionKeychain: Pointer;
function kSCValNetIPSecXAuthPasswordEncryptionPrompt: Pointer;
function kSCPropNetIPv4Addresses: Pointer;
function kSCPropNetIPv4ConfigMethod: Pointer;
function kSCPropNetIPv4DHCPClientID: Pointer;
function kSCPropNetIPv4Router: Pointer;
function kSCPropNetIPv4SubnetMasks: Pointer;
function kSCPropNetIPv4DestAddresses: Pointer;
function kSCPropNetIPv4BroadcastAddresses: Pointer;
function kSCValNetIPv4ConfigMethodAutomatic: Pointer;
function kSCValNetIPv4ConfigMethodBOOTP: Pointer;
function kSCValNetIPv4ConfigMethodDHCP: Pointer;
function kSCValNetIPv4ConfigMethodINFORM: Pointer;
function kSCValNetIPv4ConfigMethodLinkLocal: Pointer;
function kSCValNetIPv4ConfigMethodManual: Pointer;
function kSCValNetIPv4ConfigMethodPPP: Pointer;
function kSCPropNetIPv6Addresses: Pointer;
function kSCPropNetIPv6ConfigMethod: Pointer;
function kSCPropNetIPv6DestAddresses: Pointer;
function kSCPropNetIPv6Flags: Pointer;
function kSCPropNetIPv6PrefixLength: Pointer;
function kSCPropNetIPv6Router: Pointer;
function kSCValNetIPv6ConfigMethodAutomatic: Pointer;
function kSCValNetIPv6ConfigMethodLinkLocal: Pointer;
function kSCValNetIPv6ConfigMethodManual: Pointer;
function kSCValNetIPv6ConfigMethodRouterAdvertisement: Pointer;
function kSCValNetIPv6ConfigMethod6to4: Pointer;
function kSCPropNet6to4Relay: Pointer;
function kSCPropNetLinkActive: Pointer;
function kSCPropNetLinkDetaching: Pointer;
function kSCPropNetModemAccessPointName: Pointer;
function kSCPropNetModemConnectionPersonality: Pointer;
function kSCPropNetModemConnectionScript: Pointer;
function kSCPropNetModemConnectSpeed: Pointer;
function kSCPropNetModemDataCompression: Pointer;
function kSCPropNetModemDeviceContextID: Pointer;
function kSCPropNetModemDeviceModel: Pointer;
function kSCPropNetModemDeviceVendor: Pointer;
function kSCPropNetModemDialMode: Pointer;
function kSCPropNetModemErrorCorrection: Pointer;
function kSCPropNetModemHoldCallWaitingAudibleAlert: Pointer;
function kSCPropNetModemHoldDisconnectOnAnswer: Pointer;
function kSCPropNetModemHoldEnabled: Pointer;
function kSCPropNetModemHoldReminder: Pointer;
function kSCPropNetModemHoldReminderTime: Pointer;
function kSCPropNetModemNote: Pointer;
function kSCPropNetModemPulseDial: Pointer;
function kSCPropNetModemSpeaker: Pointer;
function kSCPropNetModemSpeed: Pointer;
function kSCValNetModemDialModeIgnoreDialTone: Pointer;
function kSCValNetModemDialModeManual: Pointer;
function kSCValNetModemDialModeWaitForDialTone: Pointer;
function kSCPropNetPPPACSPEnabled: Pointer;
function kSCPropNetPPPConnectTime: Pointer;
function kSCPropNetPPPDeviceLastCause: Pointer;
function kSCPropNetPPPDialOnDemand: Pointer;
function kSCPropNetPPPDisconnectOnFastUserSwitch: Pointer;
function kSCPropNetPPPDisconnectOnIdle: Pointer;
function kSCPropNetPPPDisconnectOnIdleTimer: Pointer;
function kSCPropNetPPPDisconnectOnLogout: Pointer;
function kSCPropNetPPPDisconnectOnSleep: Pointer;
function kSCPropNetPPPDisconnectTime: Pointer;
function kSCPropNetPPPIdleReminderTimer: Pointer;
function kSCPropNetPPPIdleReminder: Pointer;
function kSCPropNetPPPLastCause: Pointer;
function kSCPropNetPPPLogfile: Pointer;
function kSCPropNetPPPPlugins: Pointer;
function kSCPropNetPPPRetryConnectTime: Pointer;
function kSCPropNetPPPSessionTimer: Pointer;
function kSCPropNetPPPStatus: Pointer;
function kSCPropNetPPPUseSessionTimer: Pointer;
function kSCPropNetPPPVerboseLogging: Pointer;
function kSCPropNetPPPAuthEAPPlugins: Pointer;
function kSCPropNetPPPAuthName: Pointer;
function kSCPropNetPPPAuthPassword: Pointer;
function kSCPropNetPPPAuthPasswordEncryption: Pointer;
function kSCPropNetPPPAuthPrompt: Pointer;
function kSCPropNetPPPAuthProtocol: Pointer;
function kSCValNetPPPAuthPasswordEncryptionKeychain: Pointer;
function kSCValNetPPPAuthPasswordEncryptionToken: Pointer;
function kSCValNetPPPAuthPromptBefore: Pointer;
function kSCValNetPPPAuthPromptAfter: Pointer;
function kSCValNetPPPAuthProtocolCHAP: Pointer;
function kSCValNetPPPAuthProtocolEAP: Pointer;
function kSCValNetPPPAuthProtocolMSCHAP1: Pointer;
function kSCValNetPPPAuthProtocolMSCHAP2: Pointer;
function kSCValNetPPPAuthProtocolPAP: Pointer;
function kSCPropNetPPPCommAlternateRemoteAddress: Pointer;
function kSCPropNetPPPCommConnectDelay: Pointer;
function kSCPropNetPPPCommDisplayTerminalWindow: Pointer;
function kSCPropNetPPPCommRedialCount: Pointer;
function kSCPropNetPPPCommRedialEnabled: Pointer;
function kSCPropNetPPPCommRedialInterval: Pointer;
function kSCPropNetPPPCommRemoteAddress: Pointer;
function kSCPropNetPPPCommTerminalScript: Pointer;
function kSCPropNetPPPCommUseTerminalScript: Pointer;
function kSCPropNetPPPCCPEnabled: Pointer;
function kSCPropNetPPPCCPMPPE40Enabled: Pointer;
function kSCPropNetPPPCCPMPPE128Enabled: Pointer;
function kSCPropNetPPPIPCPCompressionVJ: Pointer;
function kSCPropNetPPPIPCPUsePeerDNS: Pointer;
function kSCPropNetPPPLCPEchoEnabled: Pointer;
function kSCPropNetPPPLCPEchoFailure: Pointer;
function kSCPropNetPPPLCPEchoInterval: Pointer;
function kSCPropNetPPPLCPCompressionACField: Pointer;
function kSCPropNetPPPLCPCompressionPField: Pointer;
function kSCPropNetPPPLCPMRU: Pointer;
function kSCPropNetPPPLCPMTU: Pointer;
function kSCPropNetPPPLCPReceiveACCM: Pointer;
function kSCPropNetPPPLCPTransmitACCM: Pointer;
function kSCPropNetL2TPIPSecSharedSecret: Pointer;
function kSCPropNetL2TPIPSecSharedSecretEncryption: Pointer;
function kSCPropNetL2TPTransport: Pointer;
function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: Pointer;
function kSCValNetL2TPTransportIP: Pointer;
function kSCValNetL2TPTransportIPSec: Pointer;
function kSCPropNetProxiesExceptionsList: Pointer;
function kSCPropNetProxiesExcludeSimpleHostnames: Pointer;
function kSCPropNetProxiesFTPEnable: Pointer;
function kSCPropNetProxiesFTPPassive: Pointer;
function kSCPropNetProxiesFTPPort: Pointer;
function kSCPropNetProxiesFTPProxy: Pointer;
function kSCPropNetProxiesGopherEnable: Pointer;
function kSCPropNetProxiesGopherPort: Pointer;
function kSCPropNetProxiesGopherProxy: Pointer;
function kSCPropNetProxiesHTTPEnable: Pointer;
function kSCPropNetProxiesHTTPPort: Pointer;
function kSCPropNetProxiesHTTPProxy: Pointer;
function kSCPropNetProxiesHTTPSEnable: Pointer;
function kSCPropNetProxiesHTTPSPort: Pointer;
function kSCPropNetProxiesHTTPSProxy: Pointer;
function kSCPropNetProxiesRTSPEnable: Pointer;
function kSCPropNetProxiesRTSPPort: Pointer;
function kSCPropNetProxiesRTSPProxy: Pointer;
function kSCPropNetProxiesSOCKSEnable: Pointer;
function kSCPropNetProxiesSOCKSPort: Pointer;
function kSCPropNetProxiesSOCKSProxy: Pointer;
function kSCPropNetProxiesProxyAutoConfigEnable: Pointer;
function kSCPropNetProxiesProxyAutoConfigJavaScript: Pointer;
function kSCPropNetProxiesProxyAutoConfigURLString: Pointer;
function kSCPropNetProxiesProxyAutoDiscoveryEnable: Pointer;
function kSCPropNetSMBNetBIOSName: Pointer;
function kSCPropNetSMBNetBIOSNodeType: Pointer;
function kSCPropNetSMBNetBIOSScope: Pointer;
function kSCPropNetSMBWINSAddresses: Pointer;
function kSCPropNetSMBWorkgroup: Pointer;
function kSCValNetSMBNetBIOSNodeTypeBroadcast: Pointer;
function kSCValNetSMBNetBIOSNodeTypePeer: Pointer;
function kSCValNetSMBNetBIOSNodeTypeMixed: Pointer;
function kSCValNetSMBNetBIOSNodeTypeHybrid: Pointer;
function kSCEntUsersConsoleUser: Pointer;
function kSCPropSystemComputerName: Pointer;
function kSCPropSystemComputerNameEncoding: Pointer;
function kSCDynamicStoreDomainFile: Pointer;
function kSCDynamicStoreDomainPlugin: Pointer;
function kSCDynamicStoreDomainSetup: Pointer;
function kSCDynamicStoreDomainState: Pointer;
function kSCDynamicStoreDomainPrefs: Pointer;
function kSCDynamicStorePropSetupCurrentSet: Pointer;
function kSCDynamicStorePropSetupLastUpdated: Pointer;
function kSCDynamicStorePropNetInterfaces: Pointer;
function kSCDynamicStorePropNetPrimaryInterface: Pointer;
function kSCDynamicStorePropNetPrimaryService: Pointer;
function kSCDynamicStorePropNetServiceIDs: Pointer;
function kSCPropUsersConsoleUserName: Pointer;
function kSCPropUsersConsoleUserUID: Pointer;
function kSCPropUsersConsoleUserGID: Pointer;
function kCFErrorDomainSystemConfiguration: Pointer;
function kSCNetworkInterfaceType6to4: Pointer;
function kSCNetworkInterfaceTypeBluetooth: Pointer;
function kSCNetworkInterfaceTypeBond: Pointer;
function kSCNetworkInterfaceTypeEthernet: Pointer;
function kSCNetworkInterfaceTypeFireWire: Pointer;
function kSCNetworkInterfaceTypeIEEE80211: Pointer;
function kSCNetworkInterfaceTypeIPSec: Pointer;
function kSCNetworkInterfaceTypeIrDA: Pointer;
function kSCNetworkInterfaceTypeL2TP: Pointer;
function kSCNetworkInterfaceTypeModem: Pointer;
function kSCNetworkInterfaceTypePPP: Pointer;
function kSCNetworkInterfaceTypePPTP: Pointer;
function kSCNetworkInterfaceTypeSerial: Pointer;
function kSCNetworkInterfaceTypeVLAN: Pointer;
function kSCNetworkInterfaceTypeWWAN: Pointer;
function kSCNetworkInterfaceTypeIPv4: Pointer;
function kSCNetworkInterfaceIPv4: Pointer;
function kSCBondStatusDeviceAggregationStatus: Pointer;
function kSCBondStatusDeviceCollecting: Pointer;
function kSCBondStatusDeviceDistributing: Pointer;
function kSCNetworkProtocolTypeDNS: Pointer;
function kSCNetworkProtocolTypeIPv4: Pointer;
function kSCNetworkProtocolTypeIPv6: Pointer;
function kSCNetworkProtocolTypeProxies: Pointer;
function kSCNetworkProtocolTypeSMB: Pointer;

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

function kCNNetworkInfoKeySSIDData: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeySSIDData');
end;

function kCNNetworkInfoKeySSID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeySSID');
end;

function kCNNetworkInfoKeyBSSID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kCNNetworkInfoKeyBSSID');
end;

function kSCDynamicStoreUseSessionKeys: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreUseSessionKeys');
end;

function kSCResvLink: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCResvLink');
end;

function kSCResvInactive: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCResvInactive');
end;

function kSCPropInterfaceName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropInterfaceName');
end;

function kSCPropMACAddress: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropMACAddress');
end;

function kSCPropUserDefinedName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropUserDefinedName');
end;

function kSCPropVersion: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropVersion');
end;

function kSCPrefCurrentSet: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPrefCurrentSet');
end;

function kSCPrefNetworkServices: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPrefNetworkServices');
end;

function kSCPrefSets: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPrefSets');
end;

function kSCPrefSystem: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPrefSystem');
end;

function kSCCompNetwork: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompNetwork');
end;

function kSCCompService: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompService');
end;

function kSCCompGlobal: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompGlobal');
end;

function kSCCompHostNames: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompHostNames');
end;

function kSCCompInterface: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompInterface');
end;

function kSCCompSystem: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompSystem');
end;

function kSCCompUsers: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompUsers');
end;

function kSCCompAnyRegex: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCCompAnyRegex');
end;

function kSCEntNetAirPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetAirPort');
end;

function kSCEntNetDHCP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetDHCP');
end;

function kSCEntNetDNS: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetDNS');
end;

function kSCEntNetEthernet: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetEthernet');
end;

function kSCEntNetFireWire: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetFireWire');
end;

function kSCEntNetInterface: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetInterface');
end;

function kSCEntNetIPSec: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPSec');
end;

function kSCEntNetIPv4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPv4');
end;

function kSCEntNetIPv6: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetIPv6');
end;

function kSCEntNetL2TP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetL2TP');
end;

function kSCEntNetLink: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetLink');
end;

function kSCEntNetModem: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetModem');
end;

function kSCEntNetPPP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPP');
end;

function kSCEntNetPPPoE: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPPoE');
end;

function kSCEntNetPPPSerial: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPPSerial');
end;

function kSCEntNetPPTP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetPPTP');
end;

function kSCEntNetProxies: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetProxies');
end;

function kSCEntNetSMB: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNetSMB');
end;

function kSCEntNet6to4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntNet6to4');
end;

function kSCPropNetOverridePrimary: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetOverridePrimary');
end;

function kSCPropNetServiceOrder: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetServiceOrder');
end;

function kSCPropNetPPPOverridePrimary: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPOverridePrimary');
end;

function kSCPropNetInterfaces: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaces');
end;

function kSCPropNetLocalHostName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLocalHostName');
end;

function kSCPropNetAirPortAllowNetCreation: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAllowNetCreation');
end;

function kSCPropNetAirPortAuthPassword: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPassword');
end;

function kSCPropNetAirPortAuthPasswordEncryption: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortAuthPasswordEncryption');
end;

function kSCPropNetAirPortJoinMode: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortJoinMode');
end;

function kSCPropNetAirPortPowerEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortPowerEnabled');
end;

function kSCPropNetAirPortPreferredNetwork: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortPreferredNetwork');
end;

function kSCPropNetAirPortSavePasswords: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetAirPortSavePasswords');
end;

function kSCValNetAirPortJoinModeAutomatic: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeAutomatic');
end;

function kSCValNetAirPortJoinModePreferred: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModePreferred');
end;

function kSCValNetAirPortJoinModeRanked: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRanked');
end;

function kSCValNetAirPortJoinModeRecent: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeRecent');
end;

function kSCValNetAirPortJoinModeStrongest: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortJoinModeStrongest');
end;

function kSCValNetAirPortAuthPasswordEncryptionKeychain: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetAirPortAuthPasswordEncryptionKeychain');
end;

function kSCPropNetDNSDomainName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSDomainName');
end;

function kSCPropNetDNSOptions: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSOptions');
end;

function kSCPropNetDNSSearchDomains: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSearchDomains');
end;

function kSCPropNetDNSSearchOrder: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSearchOrder');
end;

function kSCPropNetDNSServerAddresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerAddresses');
end;

function kSCPropNetDNSServerPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerPort');
end;

function kSCPropNetDNSServerTimeout: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSServerTimeout');
end;

function kSCPropNetDNSSortList: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSortList');
end;

function kSCPropNetDNSSupplementalMatchDomains: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchDomains');
end;

function kSCPropNetDNSSupplementalMatchOrders: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetDNSSupplementalMatchOrders');
end;

function kSCPropNetEthernetMediaSubType: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMediaSubType');
end;

function kSCPropNetEthernetMediaOptions: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMediaOptions');
end;

function kSCPropNetEthernetMTU: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetEthernetMTU');
end;

function kSCPropNetInterfaceDeviceName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceDeviceName');
end;

function kSCPropNetInterfaceHardware: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceHardware');
end;

function kSCPropNetInterfaceType: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceType');
end;

function kSCPropNetInterfaceSubType: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceSubType');
end;

function kSCPropNetInterfaceSupportsModemOnHold: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetInterfaceSupportsModemOnHold');
end;

function kSCValNetInterfaceTypeEthernet: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeEthernet');
end;

function kSCValNetInterfaceTypeFireWire: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeFireWire');
end;

function kSCValNetInterfaceTypePPP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypePPP');
end;

function kSCValNetInterfaceType6to4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceType6to4');
end;

function kSCValNetInterfaceTypeIPSec: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceTypeIPSec');
end;

function kSCValNetInterfaceSubTypePPPoE: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPoE');
end;

function kSCValNetInterfaceSubTypePPPSerial: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPPSerial');
end;

function kSCValNetInterfaceSubTypePPTP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypePPTP');
end;

function kSCValNetInterfaceSubTypeL2TP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetInterfaceSubTypeL2TP');
end;

function kSCPropNetIPSecAuthenticationMethod: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecAuthenticationMethod');
end;

function kSCPropNetIPSecLocalCertificate: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalCertificate');
end;

function kSCPropNetIPSecLocalIdentifier: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifier');
end;

function kSCPropNetIPSecLocalIdentifierType: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecLocalIdentifierType');
end;

function kSCPropNetIPSecSharedSecret: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecret');
end;

function kSCPropNetIPSecSharedSecretEncryption: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecSharedSecretEncryption');
end;

function kSCPropNetIPSecConnectTime: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecConnectTime');
end;

function kSCPropNetIPSecRemoteAddress: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecRemoteAddress');
end;

function kSCPropNetIPSecStatus: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecStatus');
end;

function kSCPropNetIPSecXAuthEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthEnabled');
end;

function kSCPropNetIPSecXAuthName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthName');
end;

function kSCPropNetIPSecXAuthPassword: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPassword');
end;

function kSCPropNetIPSecXAuthPasswordEncryption: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPSecXAuthPasswordEncryption');
end;

function kSCValNetIPSecAuthenticationMethodSharedSecret: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodSharedSecret');
end;

function kSCValNetIPSecAuthenticationMethodCertificate: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodCertificate');
end;

function kSCValNetIPSecAuthenticationMethodHybrid: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecAuthenticationMethodHybrid');
end;

function kSCValNetIPSecLocalIdentifierTypeKeyID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecLocalIdentifierTypeKeyID');
end;

function kSCValNetIPSecSharedSecretEncryptionKeychain: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecSharedSecretEncryptionKeychain');
end;

function kSCValNetIPSecXAuthPasswordEncryptionKeychain: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionKeychain');
end;

function kSCValNetIPSecXAuthPasswordEncryptionPrompt: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPSecXAuthPasswordEncryptionPrompt');
end;

function kSCPropNetIPv4Addresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4Addresses');
end;

function kSCPropNetIPv4ConfigMethod: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4ConfigMethod');
end;

function kSCPropNetIPv4DHCPClientID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4DHCPClientID');
end;

function kSCPropNetIPv4Router: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4Router');
end;

function kSCPropNetIPv4SubnetMasks: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4SubnetMasks');
end;

function kSCPropNetIPv4DestAddresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4DestAddresses');
end;

function kSCPropNetIPv4BroadcastAddresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv4BroadcastAddresses');
end;

function kSCValNetIPv4ConfigMethodAutomatic: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodAutomatic');
end;

function kSCValNetIPv4ConfigMethodBOOTP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodBOOTP');
end;

function kSCValNetIPv4ConfigMethodDHCP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodDHCP');
end;

function kSCValNetIPv4ConfigMethodINFORM: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodINFORM');
end;

function kSCValNetIPv4ConfigMethodLinkLocal: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodLinkLocal');
end;

function kSCValNetIPv4ConfigMethodManual: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodManual');
end;

function kSCValNetIPv4ConfigMethodPPP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv4ConfigMethodPPP');
end;

function kSCPropNetIPv6Addresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Addresses');
end;

function kSCPropNetIPv6ConfigMethod: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6ConfigMethod');
end;

function kSCPropNetIPv6DestAddresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6DestAddresses');
end;

function kSCPropNetIPv6Flags: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Flags');
end;

function kSCPropNetIPv6PrefixLength: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6PrefixLength');
end;

function kSCPropNetIPv6Router: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetIPv6Router');
end;

function kSCValNetIPv6ConfigMethodAutomatic: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodAutomatic');
end;

function kSCValNetIPv6ConfigMethodLinkLocal: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodLinkLocal');
end;

function kSCValNetIPv6ConfigMethodManual: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodManual');
end;

function kSCValNetIPv6ConfigMethodRouterAdvertisement: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethodRouterAdvertisement');
end;

function kSCValNetIPv6ConfigMethod6to4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetIPv6ConfigMethod6to4');
end;

function kSCPropNet6to4Relay: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNet6to4Relay');
end;

function kSCPropNetLinkActive: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLinkActive');
end;

function kSCPropNetLinkDetaching: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetLinkDetaching');
end;

function kSCPropNetModemAccessPointName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemAccessPointName');
end;

function kSCPropNetModemConnectionPersonality: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectionPersonality');
end;

function kSCPropNetModemConnectionScript: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectionScript');
end;

function kSCPropNetModemConnectSpeed: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemConnectSpeed');
end;

function kSCPropNetModemDataCompression: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDataCompression');
end;

function kSCPropNetModemDeviceContextID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceContextID');
end;

function kSCPropNetModemDeviceModel: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceModel');
end;

function kSCPropNetModemDeviceVendor: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDeviceVendor');
end;

function kSCPropNetModemDialMode: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemDialMode');
end;

function kSCPropNetModemErrorCorrection: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemErrorCorrection');
end;

function kSCPropNetModemHoldCallWaitingAudibleAlert: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldCallWaitingAudibleAlert');
end;

function kSCPropNetModemHoldDisconnectOnAnswer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldDisconnectOnAnswer');
end;

function kSCPropNetModemHoldEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldEnabled');
end;

function kSCPropNetModemHoldReminder: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldReminder');
end;

function kSCPropNetModemHoldReminderTime: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemHoldReminderTime');
end;

function kSCPropNetModemNote: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemNote');
end;

function kSCPropNetModemPulseDial: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemPulseDial');
end;

function kSCPropNetModemSpeaker: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemSpeaker');
end;

function kSCPropNetModemSpeed: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetModemSpeed');
end;

function kSCValNetModemDialModeIgnoreDialTone: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeIgnoreDialTone');
end;

function kSCValNetModemDialModeManual: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeManual');
end;

function kSCValNetModemDialModeWaitForDialTone: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetModemDialModeWaitForDialTone');
end;

function kSCPropNetPPPACSPEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPACSPEnabled');
end;

function kSCPropNetPPPConnectTime: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPConnectTime');
end;

function kSCPropNetPPPDeviceLastCause: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDeviceLastCause');
end;

function kSCPropNetPPPDialOnDemand: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDialOnDemand');
end;

function kSCPropNetPPPDisconnectOnFastUserSwitch: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnFastUserSwitch');
end;

function kSCPropNetPPPDisconnectOnIdle: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdle');
end;

function kSCPropNetPPPDisconnectOnIdleTimer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnIdleTimer');
end;

function kSCPropNetPPPDisconnectOnLogout: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnLogout');
end;

function kSCPropNetPPPDisconnectOnSleep: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectOnSleep');
end;

function kSCPropNetPPPDisconnectTime: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPDisconnectTime');
end;

function kSCPropNetPPPIdleReminderTimer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminderTimer');
end;

function kSCPropNetPPPIdleReminder: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIdleReminder');
end;

function kSCPropNetPPPLastCause: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLastCause');
end;

function kSCPropNetPPPLogfile: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLogfile');
end;

function kSCPropNetPPPPlugins: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPPlugins');
end;

function kSCPropNetPPPRetryConnectTime: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPRetryConnectTime');
end;

function kSCPropNetPPPSessionTimer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPSessionTimer');
end;

function kSCPropNetPPPStatus: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPStatus');
end;

function kSCPropNetPPPUseSessionTimer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPUseSessionTimer');
end;

function kSCPropNetPPPVerboseLogging: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPVerboseLogging');
end;

function kSCPropNetPPPAuthEAPPlugins: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthEAPPlugins');
end;

function kSCPropNetPPPAuthName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthName');
end;

function kSCPropNetPPPAuthPassword: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPassword');
end;

function kSCPropNetPPPAuthPasswordEncryption: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPasswordEncryption');
end;

function kSCPropNetPPPAuthPrompt: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthPrompt');
end;

function kSCPropNetPPPAuthProtocol: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPAuthProtocol');
end;

function kSCValNetPPPAuthPasswordEncryptionKeychain: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionKeychain');
end;

function kSCValNetPPPAuthPasswordEncryptionToken: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPasswordEncryptionToken');
end;

function kSCValNetPPPAuthPromptBefore: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptBefore');
end;

function kSCValNetPPPAuthPromptAfter: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthPromptAfter');
end;

function kSCValNetPPPAuthProtocolCHAP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolCHAP');
end;

function kSCValNetPPPAuthProtocolEAP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolEAP');
end;

function kSCValNetPPPAuthProtocolMSCHAP1: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP1');
end;

function kSCValNetPPPAuthProtocolMSCHAP2: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolMSCHAP2');
end;

function kSCValNetPPPAuthProtocolPAP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetPPPAuthProtocolPAP');
end;

function kSCPropNetPPPCommAlternateRemoteAddress: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommAlternateRemoteAddress');
end;

function kSCPropNetPPPCommConnectDelay: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommConnectDelay');
end;

function kSCPropNetPPPCommDisplayTerminalWindow: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommDisplayTerminalWindow');
end;

function kSCPropNetPPPCommRedialCount: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialCount');
end;

function kSCPropNetPPPCommRedialEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialEnabled');
end;

function kSCPropNetPPPCommRedialInterval: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRedialInterval');
end;

function kSCPropNetPPPCommRemoteAddress: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommRemoteAddress');
end;

function kSCPropNetPPPCommTerminalScript: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommTerminalScript');
end;

function kSCPropNetPPPCommUseTerminalScript: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCommUseTerminalScript');
end;

function kSCPropNetPPPCCPEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPEnabled');
end;

function kSCPropNetPPPCCPMPPE40Enabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE40Enabled');
end;

function kSCPropNetPPPCCPMPPE128Enabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPCCPMPPE128Enabled');
end;

function kSCPropNetPPPIPCPCompressionVJ: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIPCPCompressionVJ');
end;

function kSCPropNetPPPIPCPUsePeerDNS: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPIPCPUsePeerDNS');
end;

function kSCPropNetPPPLCPEchoEnabled: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoEnabled');
end;

function kSCPropNetPPPLCPEchoFailure: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoFailure');
end;

function kSCPropNetPPPLCPEchoInterval: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPEchoInterval');
end;

function kSCPropNetPPPLCPCompressionACField: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionACField');
end;

function kSCPropNetPPPLCPCompressionPField: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPCompressionPField');
end;

function kSCPropNetPPPLCPMRU: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPMRU');
end;

function kSCPropNetPPPLCPMTU: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPMTU');
end;

function kSCPropNetPPPLCPReceiveACCM: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPReceiveACCM');
end;

function kSCPropNetPPPLCPTransmitACCM: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetPPPLCPTransmitACCM');
end;

function kSCPropNetL2TPIPSecSharedSecret: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecret');
end;

function kSCPropNetL2TPIPSecSharedSecretEncryption: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPIPSecSharedSecretEncryption');
end;

function kSCPropNetL2TPTransport: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetL2TPTransport');
end;

function kSCValNetL2TPIPSecSharedSecretEncryptionKeychain: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPIPSecSharedSecretEncryptionKeychain');
end;

function kSCValNetL2TPTransportIP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPTransportIP');
end;

function kSCValNetL2TPTransportIPSec: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetL2TPTransportIPSec');
end;

function kSCPropNetProxiesExceptionsList: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesExceptionsList');
end;

function kSCPropNetProxiesExcludeSimpleHostnames: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesExcludeSimpleHostnames');
end;

function kSCPropNetProxiesFTPEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPEnable');
end;

function kSCPropNetProxiesFTPPassive: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPassive');
end;

function kSCPropNetProxiesFTPPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPPort');
end;

function kSCPropNetProxiesFTPProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesFTPProxy');
end;

function kSCPropNetProxiesGopherEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherEnable');
end;

function kSCPropNetProxiesGopherPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherPort');
end;

function kSCPropNetProxiesGopherProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesGopherProxy');
end;

function kSCPropNetProxiesHTTPEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPEnable');
end;

function kSCPropNetProxiesHTTPPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPPort');
end;

function kSCPropNetProxiesHTTPProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPProxy');
end;

function kSCPropNetProxiesHTTPSEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSEnable');
end;

function kSCPropNetProxiesHTTPSPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSPort');
end;

function kSCPropNetProxiesHTTPSProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesHTTPSProxy');
end;

function kSCPropNetProxiesRTSPEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPEnable');
end;

function kSCPropNetProxiesRTSPPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPPort');
end;

function kSCPropNetProxiesRTSPProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesRTSPProxy');
end;

function kSCPropNetProxiesSOCKSEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSEnable');
end;

function kSCPropNetProxiesSOCKSPort: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSPort');
end;

function kSCPropNetProxiesSOCKSProxy: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesSOCKSProxy');
end;

function kSCPropNetProxiesProxyAutoConfigEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigEnable');
end;

function kSCPropNetProxiesProxyAutoConfigJavaScript: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigJavaScript');
end;

function kSCPropNetProxiesProxyAutoConfigURLString: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoConfigURLString');
end;

function kSCPropNetProxiesProxyAutoDiscoveryEnable: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetProxiesProxyAutoDiscoveryEnable');
end;

function kSCPropNetSMBNetBIOSName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSName');
end;

function kSCPropNetSMBNetBIOSNodeType: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSNodeType');
end;

function kSCPropNetSMBNetBIOSScope: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBNetBIOSScope');
end;

function kSCPropNetSMBWINSAddresses: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBWINSAddresses');
end;

function kSCPropNetSMBWorkgroup: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropNetSMBWorkgroup');
end;

function kSCValNetSMBNetBIOSNodeTypeBroadcast: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeBroadcast');
end;

function kSCValNetSMBNetBIOSNodeTypePeer: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypePeer');
end;

function kSCValNetSMBNetBIOSNodeTypeMixed: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeMixed');
end;

function kSCValNetSMBNetBIOSNodeTypeHybrid: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCValNetSMBNetBIOSNodeTypeHybrid');
end;

function kSCEntUsersConsoleUser: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCEntUsersConsoleUser');
end;

function kSCPropSystemComputerName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropSystemComputerName');
end;

function kSCPropSystemComputerNameEncoding: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropSystemComputerNameEncoding');
end;

function kSCDynamicStoreDomainFile: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainFile');
end;

function kSCDynamicStoreDomainPlugin: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainPlugin');
end;

function kSCDynamicStoreDomainSetup: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainSetup');
end;

function kSCDynamicStoreDomainState: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainState');
end;

function kSCDynamicStoreDomainPrefs: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStoreDomainPrefs');
end;

function kSCDynamicStorePropSetupCurrentSet: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropSetupCurrentSet');
end;

function kSCDynamicStorePropSetupLastUpdated: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropSetupLastUpdated');
end;

function kSCDynamicStorePropNetInterfaces: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetInterfaces');
end;

function kSCDynamicStorePropNetPrimaryInterface: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryInterface');
end;

function kSCDynamicStorePropNetPrimaryService: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetPrimaryService');
end;

function kSCDynamicStorePropNetServiceIDs: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCDynamicStorePropNetServiceIDs');
end;

function kSCPropUsersConsoleUserName: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserName');
end;

function kSCPropUsersConsoleUserUID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserUID');
end;

function kSCPropUsersConsoleUserGID: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCPropUsersConsoleUserGID');
end;

function kCFErrorDomainSystemConfiguration: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kCFErrorDomainSystemConfiguration');
end;

function kSCNetworkInterfaceType6to4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceType6to4');
end;

function kSCNetworkInterfaceTypeBluetooth: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBluetooth');
end;

function kSCNetworkInterfaceTypeBond: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeBond');
end;

function kSCNetworkInterfaceTypeEthernet: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeEthernet');
end;

function kSCNetworkInterfaceTypeFireWire: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeFireWire');
end;

function kSCNetworkInterfaceTypeIEEE80211: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIEEE80211');
end;

function kSCNetworkInterfaceTypeIPSec: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPSec');
end;

function kSCNetworkInterfaceTypeIrDA: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIrDA');
end;

function kSCNetworkInterfaceTypeL2TP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeL2TP');
end;

function kSCNetworkInterfaceTypeModem: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeModem');
end;

function kSCNetworkInterfaceTypePPP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPP');
end;

function kSCNetworkInterfaceTypePPTP: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypePPTP');
end;

function kSCNetworkInterfaceTypeSerial: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeSerial');
end;

function kSCNetworkInterfaceTypeVLAN: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeVLAN');
end;

function kSCNetworkInterfaceTypeWWAN: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeWWAN');
end;

function kSCNetworkInterfaceTypeIPv4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceTypeIPv4');
end;

function kSCNetworkInterfaceIPv4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkInterfaceIPv4');
end;

function kSCBondStatusDeviceAggregationStatus: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceAggregationStatus');
end;

function kSCBondStatusDeviceCollecting: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceCollecting');
end;

function kSCBondStatusDeviceDistributing: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCBondStatusDeviceDistributing');
end;

function kSCNetworkProtocolTypeDNS: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeDNS');
end;

function kSCNetworkProtocolTypeIPv4: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv4');
end;

function kSCNetworkProtocolTypeIPv6: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeIPv6');
end;

function kSCNetworkProtocolTypeProxies: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeProxies');
end;

function kSCNetworkProtocolTypeSMB: Pointer;
begin
  Result := CocoaPointerConst(libSystemConfiguration, 'kSCNetworkProtocolTypeSMB');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}
initialization
  SystemConfigurationModule := dlopen(MarshaledAString(libSystemConfiguration), RTLD_LAZY);

finalization
  dlclose(SystemConfigurationModule);
{$ENDIF IOS}

end.
