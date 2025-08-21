unit DW.Androidapi.JNI.Net;

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
  // Android
  Androidapi.JNIBridge,
  Androidapi.JNI.Java.Net;

type
  JMulticastSocket = interface;

  JMulticastSocketClass = interface(JDatagramSocketClass)
    ['{D7905CC3-BA3E-4248-BDEB-655DD101F4E9}']
    {class} function init: JMulticastSocket; cdecl; overload;
    {class} function init(port: Integer): JMulticastSocket; cdecl; overload;
    {class} function init(localAddress: JSocketAddress): JMulticastSocket; cdecl; overload;
  end;

  [JavaSignature('java/net/MulticastSocket')]
  JMulticastSocket = interface(JDatagramSocket)
    ['{CAAC9A06-8EFB-42C2-B6E4-DEBAE2D9319A}']
    function getInterface: JInetAddress; cdecl;
    function getLoopbackMode: Boolean; cdecl;
    function getNetworkInterface: JNetworkInterface; cdecl;
    function getTimeToLive: Integer; cdecl;
    function getTTL: Byte; cdecl;
    procedure joinGroup(groupAddr: JInetAddress); cdecl; overload;
    procedure joinGroup(groupAddress: JSocketAddress; netInterface: JNetworkInterface); cdecl; overload;
    procedure leaveGroup(groupAddr: JInetAddress); cdecl; overload;
    procedure leaveGroup(groupAddress: JSocketAddress; netInterface: JNetworkInterface); cdecl; overload;
    procedure send(packet: JDatagramPacket; ttl: Byte); cdecl; overload;
    procedure setInterface(address: JInetAddress); cdecl;
    procedure setTimeToLive(ttl: Integer); cdecl;
    procedure setLoopbackMode(disable: Boolean); cdecl;
    procedure setNetworkInterface(networkInterface: JNetworkInterface); cdecl;
    procedure setTTL(ttl: Byte); cdecl;
  end;
  TJMulticastSocket = class(TJavaGenericImport<JMulticastSocketClass, JMulticastSocket>) end;

implementation

end.

