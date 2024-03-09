unit DW.Androidapi.JNI.Java.Net;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Net;

type
  JAuthenticator = interface;
  JAuthenticator_RequestorType = interface;
  JCookieHandler = interface;
  JCookieManager = interface;
  JCookiePolicy = interface;
  JCookieStore = interface;
  JHttpCookie = interface;
  JHttpRetryException = interface;
  JIDN = interface;
  JInterfaceAddress = interface;
  JMulticastSocket = interface;
  JPasswordAuthentication = interface;
  JStandardSocketOptions = interface;
  JURLDecoder = interface;
  JURLEncoder = interface;

  JURLEncoderClass = interface(JObjectClass)
    ['{0FDF0B8E-6308-4469-B3F7-F2EAC7E7BEAB}']
    {class} function encode(string_1: JString; charset: JCharset): JString; cdecl; overload;
    {class} function encode(string_1: JString; string_2: JString): JString; cdecl; overload;
    {class} function encode(string_1: JString): JString; cdecl; overload;
  end;

  [JavaSignature('java/net/URLEncoder')]
  JURLEncoder = interface(JObject)
    ['{CDCEBCA2-5B47-415B-8016-0F3F4DB17750}']
  end;
  TJURLEncoder = class(TJavaGenericImport<JURLEncoderClass, JURLEncoder>) end;

  JURLDecoderClass = interface(JObjectClass)
    ['{C1EAF245-A1AD-4058-905D-76C3D187D8C9}']
    {class} function decode(string_1: JString; charset: JCharset): JString; cdecl; overload;
    {class} function decode(string_1: JString; string_2: JString): JString; cdecl; overload;
    {class} function decode(string_1: JString): JString; cdecl; overload;
    {class} function init: JURLDecoder; cdecl;
  end;

  [JavaSignature('java/net/URLDecoder')]
  JURLDecoder = interface(JObject)
    ['{F0239549-35D7-4DB2-BE53-8F53FD435C68}']
  end;
  TJURLDecoder = class(TJavaGenericImport<JURLDecoderClass, JURLDecoder>) end;

  JStandardSocketOptionsClass = interface(JObjectClass)
    ['{6D039000-0A33-43B9-94E6-89DAC9B11190}']
    {class} function _GetIP_MULTICAST_IF: JSocketOption; cdecl;
    {class} function _GetIP_MULTICAST_LOOP: JSocketOption; cdecl;
    {class} function _GetIP_MULTICAST_TTL: JSocketOption; cdecl;
    {class} function _GetIP_TOS: JSocketOption; cdecl;
    {class} function _GetSO_BROADCAST: JSocketOption; cdecl;
    {class} function _GetSO_KEEPALIVE: JSocketOption; cdecl;
    {class} function _GetSO_LINGER: JSocketOption; cdecl;
    {class} function _GetSO_RCVBUF: JSocketOption; cdecl;
    {class} function _GetSO_REUSEADDR: JSocketOption; cdecl;
    {class} function _GetSO_REUSEPORT: JSocketOption; cdecl;
    {class} function _GetSO_SNDBUF: JSocketOption; cdecl;
    {class} function _GetTCP_NODELAY: JSocketOption; cdecl;
    {class} property IP_MULTICAST_IF: JSocketOption read _GetIP_MULTICAST_IF;
    {class} property IP_MULTICAST_LOOP: JSocketOption read _GetIP_MULTICAST_LOOP;
    {class} property IP_MULTICAST_TTL: JSocketOption read _GetIP_MULTICAST_TTL;
    {class} property IP_TOS: JSocketOption read _GetIP_TOS;
    {class} property SO_BROADCAST: JSocketOption read _GetSO_BROADCAST;
    {class} property SO_KEEPALIVE: JSocketOption read _GetSO_KEEPALIVE;
    {class} property SO_LINGER: JSocketOption read _GetSO_LINGER;
    {class} property SO_RCVBUF: JSocketOption read _GetSO_RCVBUF;
    {class} property SO_REUSEADDR: JSocketOption read _GetSO_REUSEADDR;
    {class} property SO_REUSEPORT: JSocketOption read _GetSO_REUSEPORT;
    {class} property SO_SNDBUF: JSocketOption read _GetSO_SNDBUF;
    {class} property TCP_NODELAY: JSocketOption read _GetTCP_NODELAY;
  end;

  [JavaSignature('java/net/StandardSocketOptions')]
  JStandardSocketOptions = interface(JObject)
    ['{04163C25-3F82-4A3F-8E5C-2A1C75FA5BC2}']
  end;
  TJStandardSocketOptions = class(TJavaGenericImport<JStandardSocketOptionsClass, JStandardSocketOptions>) end;

  JMulticastSocketClass = interface(JDatagramSocketClass)
    ['{096AB772-6FF7-4C85-A89B-A767816CC45D}']
    {class} function init: JMulticastSocket; cdecl; overload;
    {class} function init(int: Integer): JMulticastSocket; cdecl; overload;
    {class} function init(socketaddress: JSocketAddress): JMulticastSocket; cdecl; overload;
  end;

  [JavaSignature('java/net/MulticastSocket')]
  JMulticastSocket = interface(JDatagramSocket)
    ['{2EC09CEA-111B-4C3B-ABDF-D0F347630C69}']
    function getInterface: JInetAddress; cdecl;
    function getLoopbackMode: Boolean; cdecl;
    function getNetworkInterface: JNetworkInterface; cdecl;
    function getTTL: Byte; cdecl;
    function getTimeToLive: Integer; cdecl;
    procedure joinGroup(socketaddress: JSocketAddress; networkinterface: JNetworkInterface); cdecl; overload;
    procedure joinGroup(inetaddress: JInetAddress); cdecl; overload;
    procedure leaveGroup(socketaddress: JSocketAddress; networkinterface: JNetworkInterface); cdecl; overload;
    procedure leaveGroup(inetaddress: JInetAddress); cdecl; overload;
    procedure send(datagrampacket: JDatagramPacket; byte: Byte); cdecl;
    procedure setInterface(inetaddress: JInetAddress); cdecl;
    procedure setLoopbackMode(boolean: Boolean); cdecl;
    procedure setNetworkInterface(networkinterface: JNetworkInterface); cdecl;
    procedure setTTL(byte: Byte); cdecl;
    procedure setTimeToLive(int: Integer); cdecl;
    function supportedOptions: JSet; cdecl;
  end;
  TJMulticastSocket = class(TJavaGenericImport<JMulticastSocketClass, JMulticastSocket>) end;

  JInterfaceAddressClass = interface(JObjectClass)
    ['{9E7492F7-14C6-466C-959F-1090142825EF}']
  end;

  [JavaSignature('java/net/InterfaceAddress')]
  JInterfaceAddress = interface(JObject)
    ['{9CAAFA71-FB9E-496F-A183-37E3EE26F2EE}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getAddress: JInetAddress; cdecl;
    function getBroadcast: JInetAddress; cdecl;
    function getNetworkPrefixLength: SmallInt; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJInterfaceAddress = class(TJavaGenericImport<JInterfaceAddressClass, JInterfaceAddress>) end;

  JIDNClass = interface(JObjectClass)
    ['{92DB5A70-C60B-4E9E-9D78-B8145B901248}']
    {class} function _GetALLOW_UNASSIGNED: Integer; cdecl;
    {class} function _GetUSE_STD3_ASCII_RULES: Integer; cdecl;
    {class} function toASCII(string_1: JString): JString; cdecl; overload;
    {class} function toASCII(string_1: JString; int: Integer): JString; cdecl; overload;
    {class} function toUnicode(string_1: JString): JString; cdecl; overload;
    {class} function toUnicode(string_1: JString; int: Integer): JString; cdecl; overload;
    {class} property ALLOW_UNASSIGNED: Integer read _GetALLOW_UNASSIGNED;
    {class} property USE_STD3_ASCII_RULES: Integer read _GetUSE_STD3_ASCII_RULES;
  end;

  [JavaSignature('java/net/IDN')]
  JIDN = interface(JObject)
    ['{80D2F1FE-4E69-4482-874B-2366E4208830}']
  end;
  TJIDN = class(TJavaGenericImport<JIDNClass, JIDN>) end;

  JHttpRetryExceptionClass = interface(JIOExceptionClass)
    ['{16369855-766C-4A91-940C-1AED8E81F3A6}']
    {class} function init(string_1: JString; int: Integer; string_2: JString): JHttpRetryException; cdecl; overload;
    {class} function init(string_1: JString; int: Integer): JHttpRetryException; cdecl; overload;
  end;

  [JavaSignature('java/net/HttpRetryException')]
  JHttpRetryException = interface(JIOException)
    ['{FBE2F36E-1820-4267-BA9C-15C77E8B7DF5}']
    function getLocation: JString; cdecl;
    function getReason: JString; cdecl;
    function responseCode: Integer; cdecl;
  end;
  TJHttpRetryException = class(TJavaGenericImport<JHttpRetryExceptionClass, JHttpRetryException>) end;

  JCookieHandlerClass = interface(JObjectClass)
    ['{78F1BE16-9775-4422-90CF-44897A5D7FE7}']
    {class} function getDefault: JCookieHandler; cdecl;
    {class} function init: JCookieHandler; cdecl;
    {class} procedure setDefault(cookiehandler: JCookieHandler); cdecl;
  end;

  [JavaSignature('java/net/CookieHandler')]
  JCookieHandler = interface(JObject)
    ['{1D329733-26F0-46F0-81CA-FE0376E76356}']
    function get(uri: JURI; map: JMap): JMap; cdecl;
    procedure put(uri: JURI; map: JMap); cdecl;
  end;
  TJCookieHandler = class(TJavaGenericImport<JCookieHandlerClass, JCookieHandler>) end;

  JCookiePolicyClass = interface(IJavaClass)
    ['{FA0FD78D-143A-462B-AF4C-176DA228D9D0}']
    {class} function _GetACCEPT_ALL: JCookiePolicy; cdecl;
    {class} function _GetACCEPT_NONE: JCookiePolicy; cdecl;
    {class} function _GetACCEPT_ORIGINAL_SERVER: JCookiePolicy; cdecl;
    {class} property ACCEPT_ALL: JCookiePolicy read _GetACCEPT_ALL;
    {class} property ACCEPT_NONE: JCookiePolicy read _GetACCEPT_NONE;
    {class} property ACCEPT_ORIGINAL_SERVER: JCookiePolicy read _GetACCEPT_ORIGINAL_SERVER;
  end;

  [JavaSignature('java/net/CookiePolicy')]
  JCookiePolicy = interface(IJavaInstance)
    ['{7068D426-79A9-46F5-BF01-C0401C2C0B0E}']
    function shouldAccept(uri: JURI; httpcookie: JHttpCookie): Boolean; cdecl;
  end;
  TJCookiePolicy = class(TJavaGenericImport<JCookiePolicyClass, JCookiePolicy>) end;
  JCookieManagerClass = interface(JCookieHandlerClass)
    ['{635F5723-9BE9-4C11-B682-FEBA916A4D2A}']
    {class} function init(cookiestore: JCookieStore; cookiepolicy: JCookiePolicy): JCookieManager; cdecl; overload;
    {class} function init: JCookieManager; cdecl; overload;
  end;

  [JavaSignature('java/net/CookieManager')]
  JCookieManager = interface(JCookieHandler)
    ['{53AEB99A-9F78-4EB1-B5D6-7AA9CC2A0770}']
    function get(uri: JURI; map: JMap): JMap; cdecl;
    function getCookieStore: JCookieStore; cdecl;
    procedure put(uri: JURI; map: JMap); cdecl;
    procedure setCookiePolicy(cookiepolicy: JCookiePolicy); cdecl;
  end;
  TJCookieManager = class(TJavaGenericImport<JCookieManagerClass, JCookieManager>) end;

  JPasswordAuthenticationClass = interface(JObjectClass)
    ['{69849014-9EFE-40EE-AC74-315E4FCBEEA0}']
    {class} function init(string_1: JString; chars: TJavaArray<Char>): JPasswordAuthentication; cdecl;
  end;

  [JavaSignature('java/net/PasswordAuthentication')]
  JPasswordAuthentication = interface(JObject)
    ['{4020125E-BC12-4BE4-88F7-462EADBB9504}']
    function getPassword: TJavaArray<Char>; cdecl;
    function getUserName: JString; cdecl;
  end;
  TJPasswordAuthentication = class(TJavaGenericImport<JPasswordAuthenticationClass, JPasswordAuthentication>) end;

  JAuthenticator_RequestorTypeClass = interface(JEnumClass)
    ['{F6FF1D19-FAF4-4F68-87BB-EBC9621D8CEE}']
    {class} function _GetPROXY: JAuthenticator_RequestorType; cdecl;
    {class} function _GetSERVER: JAuthenticator_RequestorType; cdecl;
    {class} function valueOf(string_1: JString): JAuthenticator_RequestorType; cdecl;
    {class} function values: TJavaObjectArray<JAuthenticator_RequestorType>; cdecl;
    {class} property PROXY: JAuthenticator_RequestorType read _GetPROXY;
    {class} property SERVER: JAuthenticator_RequestorType read _GetSERVER;
  end;

  [JavaSignature('java/net/Authenticator$RequestorType')]
  JAuthenticator_RequestorType = interface(JEnum)
    ['{BD002881-72F5-484C-A238-0D557CBBC798}']
  end;
  TJAuthenticator_RequestorType = class(TJavaGenericImport<JAuthenticator_RequestorTypeClass, JAuthenticator_RequestorType>) end;

  JAuthenticatorClass = interface(JObjectClass)
    ['{72A8C98D-B77D-4CF7-B7E6-CC8D1788638A}']
    {class} function getDefault: JAuthenticator; cdecl;
    {class} function init: JAuthenticator; cdecl;
    {class} function requestPasswordAuthentication(string_1: JString; inetaddress: JInetAddress; int: Integer; string_2: JString; string_3: JString;
      string_4: JString; url: JURL; requestortype: JAuthenticator_RequestorType): JPasswordAuthentication; cdecl; overload;
    {class} function requestPasswordAuthentication(authenticator: JAuthenticator; string_1: JString; inetaddress: JInetAddress; int: Integer;
      string_2: JString; string_3: JString; string_4: JString; url: JURL;
        requestortype: JAuthenticator_RequestorType): JPasswordAuthentication; cdecl; overload;
    {class} function requestPasswordAuthentication(string_1: JString; inetaddress: JInetAddress; int: Integer; string_2: JString; string_3: JString;
      string_4: JString): JPasswordAuthentication; cdecl; overload;
    {class} function requestPasswordAuthentication(inetaddress: JInetAddress; int: Integer; string_1: JString; string_2: JString;
      string_3: JString): JPasswordAuthentication; cdecl; overload;
    {class} procedure setDefault(authenticator: JAuthenticator); cdecl;
  end;

  [JavaSignature('java/net/Authenticator')]
  JAuthenticator = interface(JObject)
    ['{2FD1ACC9-F5BE-4507-9BA7-BD8537069810}']
    function requestPasswordAuthenticationInstance(string_1: JString; inetaddress: JInetAddress; int: Integer; string_2: JString; string_3: JString;
      string_4: JString; url: JURL; requestortype: JAuthenticator_RequestorType): JPasswordAuthentication; cdecl;
  end;
  TJAuthenticator = class(TJavaGenericImport<JAuthenticatorClass, JAuthenticator>) end;

  JHttpCookieClass = interface(JObjectClass)
    ['{06F464EE-F5E6-4E7E-A4C9-0A97CF1B6C9B}']
    {class} function domainMatches(string_1: JString; string_2: JString): Boolean; cdecl;
    {class} function init(string_1: JString; string_2: JString): JHttpCookie; cdecl;
    {class} function parse(string_1: JString): JList; cdecl;
  end;

  [JavaSignature('java/net/HttpCookie')]
  JHttpCookie = interface(JObject)
    ['{66506AEA-87A6-4621-B1D0-2BCD0D4E23A7}']
    function clone: JObject; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getComment: JString; cdecl;
    function getCommentURL: JString; cdecl;
    function getDiscard: Boolean; cdecl;
    function getDomain: JString; cdecl;
    function getMaxAge: Int64; cdecl;
    function getName: JString; cdecl;
    function getPath: JString; cdecl;
    function getPortlist: JString; cdecl;
    function getSecure: Boolean; cdecl;
    function getValue: JString; cdecl;
    function getVersion: Integer; cdecl;
    function hasExpired: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isHttpOnly: Boolean; cdecl;
    procedure setComment(string_1: JString); cdecl;
    procedure setCommentURL(string_1: JString); cdecl;
    procedure setDiscard(boolean: Boolean); cdecl;
    procedure setDomain(string_1: JString); cdecl;
    procedure setHttpOnly(boolean: Boolean); cdecl;
    procedure setMaxAge(long: Int64); cdecl;
    procedure setPath(string_1: JString); cdecl;
    procedure setPortlist(string_1: JString); cdecl;
    procedure setSecure(boolean: Boolean); cdecl;
    procedure setValue(string_1: JString); cdecl;
    procedure setVersion(int: Integer); cdecl;
    function toString: JString; cdecl;
  end;
  TJHttpCookie = class(TJavaGenericImport<JHttpCookieClass, JHttpCookie>) end;

  JCookieStoreClass = interface(IJavaClass)
    ['{029E81D4-61C6-4B16-BB4B-8BC326DD1498}']
  end;

  [JavaSignature('java/net/CookieStore')]
  JCookieStore = interface(IJavaInstance)
    ['{9C265D9A-7C95-4110-843B-E9F2A145EBD9}']
    procedure add(uri: JURI; httpcookie: JHttpCookie); cdecl;
    function get(uri: JURI): JList; cdecl;
    function getCookies: JList; cdecl;
    function getURIs: JList; cdecl;
    function remove(uri: JURI; httpcookie: JHttpCookie): Boolean; cdecl;
    function removeAll: Boolean; cdecl;
  end;
  TJCookieStore = class(TJavaGenericImport<JCookieStoreClass, JCookieStore>) end;

implementation

end.
