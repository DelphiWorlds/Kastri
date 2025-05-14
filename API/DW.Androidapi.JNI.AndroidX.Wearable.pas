unit DW.Androidapi.JNI.AndroidX.Wearable;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App,
  Androidapi.JNI.PlayServices, Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.Net,
  // DW
  DW.Androidapi.JNI.PlayServices, DW.Androidapi.JNI.Util, DW.Androidapi.JNI.Icu;

type
  JAsset = interface;
  JChannelClient = interface;
  JChannelClient_Channel = interface;
  JChannelClient_ChannelCallback = interface;
  JDataApi_DataListener = interface;
  JDataClient = interface;
  JDataClient_OnDataChangedListener = interface;
  JDataEvent = interface;
  JDataEventBuffer = interface;
  JDataItem = interface;
  JDataItemBuffer = interface;
  JDataMap = interface;
  JDataMapItem = interface;
  JMessageApi_MessageListener = interface;
  JMessageClient = interface;
  JMessageClient_OnMessageReceivedListener = interface;
  JMessageEvent = interface;
  JMessageOptions = interface;
  JNode = interface;
  JNodeClient = interface;
  JPutDataRequest = interface;
  JPutDataMapRequest = interface;
  JRemoteActivityHelper = interface;

  JChannelClientClass = interface(JGoogleApiClass)
    ['{B72F95E1-1CD9-436B-8221-4FB5FB168905}']
    {class} function _GetACTION_CHANNEL_EVENT: JString; cdecl;
    // {class} function init(context: JContext; settings: JGoogleApi_Settings): JChannelClient; overload; cdecl;
    // {class} function init(activity: JActivity; settings: JGoogleApi_Settings): JChannelClient; overload; cdecl;
    {class} property ACTION_CHANNEL_EVENT: JString read _GetACTION_CHANNEL_EVENT;
  end;

  [JavaSignature('com/google/android/gms/wearable/ChannelClient')]
  JChannelClient = interface(JGoogleApi)
    ['{A102973F-59BC-4398-83A2-4C6430923EA0}']
    function close(channel: JChannelClient_Channel): JTask; overload; cdecl;
    function close(channel: JChannelClient_Channel; int: Integer): JTask; overload; cdecl;
    function getInputStream(channel: JChannelClient_Channel): JTask; cdecl;
    function getOutputStream(channel: JChannelClient_Channel): JTask; cdecl;
    function openChannel(string_1: JString; string_2: JString): JTask; cdecl;
    function receiveFile(channel: JChannelClient_Channel; uri: Jnet_Uri; boolean: Boolean): JTask; cdecl;
    function registerChannelCallback(channel: JChannelClient_Channel; channelcallback: JChannelClient_ChannelCallback): JTask; overload; cdecl;
    function registerChannelCallback(channelcallback: JChannelClient_ChannelCallback): JTask; overload; cdecl;
    function sendFile(channel: JChannelClient_Channel; uri: Jnet_Uri; long: Int64; long_1: Int64): JTask; overload; cdecl;
    function sendFile(channel: JChannelClient_Channel; uri: Jnet_Uri): JTask; overload; cdecl;
    function unregisterChannelCallback(channel: JChannelClient_Channel; channelcallback: JChannelClient_ChannelCallback): JTask; overload; cdecl;
    function unregisterChannelCallback(channelcallback: JChannelClient_ChannelCallback): JTask; overload; cdecl;
  end;
  TJChannelClient = class(TJavaGenericImport<JChannelClientClass, JChannelClient>) end;

  JChannelClient_ChannelCallbackClass = interface(JObjectClass)
    ['{1705801D-1F88-40BA-A04E-0B854CAD5681}']
    {class} function _GetCLOSE_REASON_DISCONNECTED: Integer; cdecl;
    {class} function _GetCLOSE_REASON_LOCAL_CLOSE: Integer; cdecl;
    {class} function _GetCLOSE_REASON_NORMAL: Integer; cdecl;
    {class} function _GetCLOSE_REASON_REMOTE_CLOSE: Integer; cdecl;
    {class} function init: JChannelClient_ChannelCallback; cdecl;
    {class} property CLOSE_REASON_DISCONNECTED: Integer read _GetCLOSE_REASON_DISCONNECTED;
    {class} property CLOSE_REASON_LOCAL_CLOSE: Integer read _GetCLOSE_REASON_LOCAL_CLOSE;
    {class} property CLOSE_REASON_NORMAL: Integer read _GetCLOSE_REASON_NORMAL;
    {class} property CLOSE_REASON_REMOTE_CLOSE: Integer read _GetCLOSE_REASON_REMOTE_CLOSE;
  end;

  [JavaSignature('com/google/android/gms/wearable/ChannelClient$ChannelCallback')]
  JChannelClient_ChannelCallback = interface(JObject)
    ['{7C50DAAB-9E04-4449-B518-92158E89D618}']
    procedure onChannelClosed(channel: JChannelClient_Channel; closeReason: Integer; errorCode: Integer); cdecl;
    procedure onChannelOpened(channel: JChannelClient_Channel); cdecl;
    procedure onInputClosed(channel: JChannelClient_Channel; closeReason: Integer; errorCode: Integer); cdecl;
    procedure onOutputClosed(channel: JChannelClient_Channel; closeReason: Integer; errorCode: Integer); cdecl;
  end;
  TJChannelClient_ChannelCallback = class(TJavaGenericImport<JChannelClient_ChannelCallbackClass, JChannelClient_ChannelCallback>) end;

  JChannelClient_ChannelClass = interface(JParcelableClass)
    ['{98BE32F9-50FA-4916-A3CE-C4866A89204B}']
  end;

  [JavaSignature('com/google/android/gms/wearable/ChannelClient$Channel')]
  JChannelClient_Channel = interface(JParcelable)
    ['{27F0CED5-E2AD-432D-ADFF-BC73F38681E5}']
    function getNodeId: JString; cdecl;
    function getPath: JString; cdecl;
  end;
  TJChannelClient_Channel = class(TJavaGenericImport<JChannelClient_ChannelClass, JChannelClient_Channel>) end;

  JDataApi_DataListenerClass = interface(IJavaClass)
    ['{62C12DBC-2085-4F67-9A73-960DEF2FD0A4}']
  end;

  [JavaSignature('com/google/android/gms/wearable/DataApi$DataListener')]
  JDataApi_DataListener = interface(IJavaInstance)
    ['{5EDCD0BE-B2B0-47E7-93C9-4099E6AC1D64}']
    procedure onDataChanged(dataeventbuffer: JDataEventBuffer); cdecl;
  end;
  TJDataApi_DataListener = class(TJavaGenericImport<JDataApi_DataListenerClass, JDataApi_DataListener>) end;

  JDataClient_OnDataChangedListenerClass = interface(JDataApi_DataListenerClass)
    ['{ABC305CD-3844-4845-AFE1-55630F13F3DA}']
  end;

  [JavaSignature('com/google/android/gms/wearable/DataClient$OnDataChangedListener')]
  JDataClient_OnDataChangedListener = interface(JDataApi_DataListener)
    ['{0384149A-406E-401D-9040-43A3AD354328}']
    procedure onDataChanged(dataeventbuffer: JDataEventBuffer); cdecl;
  end;
  TJDataClient_OnDataChangedListener = class(TJavaGenericImport<JDataClient_OnDataChangedListenerClass, JDataClient_OnDataChangedListener>) end;

  JNodeClass = interface(IJavaClass)
    ['{DDF7B237-7C03-445B-B8EC-92AAA7535B41}']
  end;

  [JavaSignature('com/google/android/gms/wearable/Node')]
  JNode = interface(IJavaInstance)
    ['{24AB8CBD-268D-4B3E-9BBB-C3849D43D57F}']
    function getDisplayName: JString; cdecl;
    function getId: JString; cdecl;
    function isNearby: Boolean; cdecl;
  end;
  TJNode = class(TJavaGenericImport<JNodeClass, JNode>) end;

  JNodeClientClass = interface(JGoogleApiClass)
    ['{E761E5D9-40FF-4D6A-A20A-A500DEC71BF9}']
    // {class} function init(activity: JActivity; settings: JGoogleApi_Settings): JNodeClient; cdecl; overload;
    // {class} function init(context: JContext; settings: JGoogleApi_Settings): JNodeClient; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/wearable/NodeClient')]
  JNodeClient = interface(JGoogleApi)
    ['{816E232A-7957-4DFF-BACC-74AC332FA409}']
    function getCompanionPackageForNode(string_1: JString): JTask; cdecl;
    function getConnectedNodes: JTask; cdecl;
    function getLocalNode: JTask; cdecl;
  end;
  TJNodeClient = class(TJavaGenericImport<JNodeClientClass, JNodeClient>) end;

  JDataItemClass = interface(JFreezableClass)
    ['{7621E8A8-AC86-43C3-A52F-F0E7237F684A}']
  end;

  [JavaSignature('com/google/android/gms/wearable/DataItem')]
  JDataItem = interface(JFreezable)
    ['{6B05ECFC-DFDD-4789-88E3-F0C2978B9550}']
    function getAssets: JMap; cdecl;
    function getData: TJavaArray<Byte>; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function setData(bytes: TJavaArray<Byte>): JDataItem; cdecl;
  end;
  TJDataItem = class(TJavaGenericImport<JDataItemClass, JDataItem>) end;

  JDataItemBufferClass = interface(JEntityBufferClass)
    ['{6DAF9058-5FD9-44CE-8A31-6734483C232D}']
    {class} function init(dataholder: JDataHolder): JDataItemBuffer; cdecl;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataItemBuffer')]
  JDataItemBuffer = interface(JEntityBuffer)
    ['{82878782-4198-4993-BC48-1A0BC8BF41F5}']
    function getStatus: JStatus; cdecl;
  end;
  TJDataItemBuffer = class(TJavaGenericImport<JDataItemBufferClass, JDataItemBuffer>) end;

  JDataEventClass = interface(JFreezableClass)
    ['{67EEDA1D-8131-4117-A320-114FF841CF84}']
    {class} function _GetTYPE_CHANGED: Integer; cdecl;
    {class} function _GetTYPE_DELETED: Integer; cdecl;
    {class} property TYPE_CHANGED: Integer read _GetTYPE_CHANGED;
    {class} property TYPE_DELETED: Integer read _GetTYPE_DELETED;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataEvent')]
  JDataEvent = interface(JFreezable)
    ['{A8E26766-FC90-4397-8EEA-2A7DD6DD69A8}']
    function getDataItem: JDataItem; cdecl;
    function getType: Integer; cdecl;
  end;
  TJDataEvent = class(TJavaGenericImport<JDataEventClass, JDataEvent>) end;

  JDataClientClass = interface(JGoogleApiClass)
    ['{2F1FDB19-7295-45B3-B0D9-89C51850935A}']
    {class} function _GetACTION_DATA_CHANGED: JString; cdecl;
    {class} function _GetFILTER_LITERAL: Integer; cdecl;
    {class} function _GetFILTER_PREFIX: Integer; cdecl;
    // {class} function init(context: JContext; settings: JGoogleApi_Settings): JDataClient; cdecl; overload;
    // {class} function init(activity: JActivity; settings: JGoogleApi_Settings): JDataClient; cdecl; overload;
    {class} property ACTION_DATA_CHANGED: JString read _GetACTION_DATA_CHANGED;
    {class} property FILTER_LITERAL: Integer read _GetFILTER_LITERAL;
    {class} property FILTER_PREFIX: Integer read _GetFILTER_PREFIX;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataClient')]
  JDataClient = interface(JGoogleApi)
    ['{D1922003-55EA-4E6A-B2E6-FB63D7A9161B}']
    function addListener(ondatachangedlistener: JDataClient_OnDataChangedListener): JTask; cdecl; overload;
    function addListener(ondatachangedlistener: JDataClient_OnDataChangedListener; uri: Jnet_Uri; int: Integer): JTask; cdecl; overload;
    function deleteDataItems(uri: Jnet_Uri): JTask; cdecl; overload;
    function deleteDataItems(uri: Jnet_Uri; int: Integer): JTask; cdecl; overload;
    function getDataItem(uri: Jnet_Uri): JTask; cdecl;
    function getDataItems(uri: Jnet_Uri; int: Integer): JTask; cdecl; overload;
    function getDataItems: JTask; cdecl; overload;
    function getDataItems(uri: Jnet_Uri): JTask; cdecl; overload;
    // function getFdForAsset(dataitemasset: JDataItemAsset): JTask; cdecl; overload;
    function getFdForAsset(asset: JAsset): JTask; cdecl; overload;
    function putDataItem(putdatarequest: JPutDataRequest): JTask; cdecl;
    function removeListener(ondatachangedlistener: JDataClient_OnDataChangedListener): JTask; cdecl;
  end;
  TJDataClient = class(TJavaGenericImport<JDataClientClass, JDataClient>) end;

  JDataEventBufferClass = interface(JEntityBufferClass)
    ['{471D4E12-A8CA-4439-BF89-3A7497AD2F12}']
    {class} function init(dataholder: JDataHolder): JDataEventBuffer; cdecl;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataEventBuffer')]
  JDataEventBuffer = interface(JEntityBuffer)
    ['{B9297CD9-4C41-4486-B54F-D3EED2577158}']
    function getStatus: JStatus; cdecl;
  end;
  TJDataEventBuffer = class(TJavaGenericImport<JDataEventBufferClass, JDataEventBuffer>) end;

  JAssetClass = interface(JAbstractSafeParcelableClass)
    ['{4EC6AB77-78BB-4385-B5CF-3CCD8F1D0F72}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function createFromBytes(bytes: TJavaArray<Byte>): JAsset; cdecl;
    {class} function createFromFd(parcelfiledescriptor: JParcelFileDescriptor): JAsset; cdecl;
    {class} function createFromRef(string_1: JString): JAsset; cdecl;
    {class} function createFromUri(uri: Jnet_Uri): JAsset; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/wearable/Asset')]
  JAsset = interface(JAbstractSafeParcelable)
    ['{8BB32D76-C034-4DFB-9CE0-8465C2A2E256}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getDigest: JString; cdecl;
    function getFd: JParcelFileDescriptor; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJAsset = class(TJavaGenericImport<JAssetClass, JAsset>) end;

  JMessageOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{6D443C75-2117-4FB4-ABED-5992E7A9D93B}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetMESSAGE_PRIORITY_HIGH: Integer; cdecl;
    {class} function _GetMESSAGE_PRIORITY_LOW: Integer; cdecl;
    {class} function init(int: Integer): JMessageOptions; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property MESSAGE_PRIORITY_HIGH: Integer read _GetMESSAGE_PRIORITY_HIGH;
    {class} property MESSAGE_PRIORITY_LOW: Integer read _GetMESSAGE_PRIORITY_LOW;
  end;

  [JavaSignature('com/google/android/gms/wearable/MessageOptions')]
  JMessageOptions = interface(JAbstractSafeParcelable)
    ['{0649707E-0827-41B5-A23F-EEDF728868DD}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getPriority: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJMessageOptions = class(TJavaGenericImport<JMessageOptionsClass, JMessageOptions>) end;

  JMessageEventClass = interface(IJavaClass)
    ['{1FF68D68-99E7-4F0A-9F0B-125BFC2256E5}']
  end;

  [JavaSignature('com/google/android/gms/wearable/MessageEvent')]
  JMessageEvent = interface(IJavaInstance)
    ['{EFA6674B-915B-4510-A8C6-0134E47F0F62}']
    function getData: TJavaArray<Byte>; cdecl;
    function getPath: JString; cdecl;
    function getRequestId: Integer; cdecl;
    function getSourceNodeId: JString; cdecl;
  end;
  TJMessageEvent = class(TJavaGenericImport<JMessageEventClass, JMessageEvent>) end;

  JMessageApi_MessageListenerClass = interface(IJavaClass)
    ['{64D7E412-892B-40FB-BD10-B8EAEFBDB8A7}']
  end;

  [JavaSignature('com/google/android/gms/wearable/MessageApi$MessageListener')]
  JMessageApi_MessageListener = interface(IJavaInstance)
    ['{FEA90B36-A932-42F6-9CE1-A233A79CBF12}']
    procedure onMessageReceived(messageevent: JMessageEvent); cdecl;
  end;
  TJMessageApi_MessageListener = class(TJavaGenericImport<JMessageApi_MessageListenerClass, JMessageApi_MessageListener>) end;

  JMessageClient_OnMessageReceivedListenerClass = interface(JMessageApi_MessageListenerClass)
    ['{EF9397D9-ADC0-42B9-A6E9-40C2716AF28D}']
  end;

  [JavaSignature('com/google/android/gms/wearable/MessageClient$OnMessageReceivedListener')]
  JMessageClient_OnMessageReceivedListener = interface(JMessageApi_MessageListener)
    ['{040FE634-7E7C-4A1E-B755-DE97C0B77BC1}']
    procedure onMessageReceived(messageevent: JMessageEvent); cdecl;
  end;
  TJMessageClient_OnMessageReceivedListener = class(TJavaGenericImport<JMessageClient_OnMessageReceivedListenerClass,
    JMessageClient_OnMessageReceivedListener>) end;

  JMessageClientClass = interface(JGoogleApiClass)
    ['{4D1B28AB-F84B-413E-928D-B976096D874B}']
    {class} function _GetACTION_MESSAGE_RECEIVED: JString; cdecl;
    {class} function _GetFILTER_LITERAL: Integer; cdecl;
    {class} function _GetFILTER_PREFIX: Integer; cdecl;
    // {class} function init(context: JContext; settings: JGoogleApi_Settings): JMessageClient; cdecl; overload;
    // {class} function init(activity: JActivity; settings: JGoogleApi_Settings): JMessageClient; cdecl; overload;
    {class} property ACTION_MESSAGE_RECEIVED: JString read _GetACTION_MESSAGE_RECEIVED;
    {class} property FILTER_LITERAL: Integer read _GetFILTER_LITERAL;
    {class} property FILTER_PREFIX: Integer read _GetFILTER_PREFIX;
  end;

  [JavaSignature('com/google/android/gms/wearable/MessageClient')]
  JMessageClient = interface(JGoogleApi)
    ['{48D94F23-70DD-4786-A815-20416D7A0314}']
    function addListener(onmessagereceivedlistener: JMessageClient_OnMessageReceivedListener; uri: Jnet_Uri; int: Integer): JTask; cdecl; overload;
    function addListener(onmessagereceivedlistener: JMessageClient_OnMessageReceivedListener): JTask; cdecl; overload;
    function removeListener(onmessagereceivedlistener: JMessageClient_OnMessageReceivedListener): JTask; cdecl;
    function sendMessage(string_1: JString; string_2: JString; bytes: TJavaArray<Byte>): JTask; cdecl; overload;
    function sendMessage(string_1: JString; string_2: JString; bytes: TJavaArray<Byte>; messageoptions: JMessageOptions): JTask; cdecl; overload;
  end;
  TJMessageClient = class(TJavaGenericImport<JMessageClientClass, JMessageClient>) end;

  JPutDataRequestClass = interface(JAbstractSafeParcelableClass)
    ['{ADE7209C-918A-4823-9627-AD7B54D0E154}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetWEAR_URI_SCHEME: JString; cdecl;
    {class} function create(string_1: JString): JPutDataRequest; cdecl;
    {class} function createFromDataItem(dataitem: JDataItem): JPutDataRequest; cdecl;
    {class} function createWithAutoAppendedId(string_1: JString): JPutDataRequest; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property WEAR_URI_SCHEME: JString read _GetWEAR_URI_SCHEME;
  end;

  [JavaSignature('com/google/android/gms/wearable/PutDataRequest')]
  JPutDataRequest = interface(JAbstractSafeParcelable)
    ['{9C8BA501-6ED7-44E1-BFB3-410EFB00D048}']
    function getAsset(string_1: JString): JAsset; cdecl;
    function getAssets: JMap; cdecl;
    function getData: TJavaArray<Byte>; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function hasAsset(string_1: JString): Boolean; cdecl;
    function isUrgent: Boolean; cdecl;
    function putAsset(string_1: JString; asset: JAsset): JPutDataRequest; cdecl;
    function removeAsset(string_1: JString): JPutDataRequest; cdecl;
    function setData(bytes: TJavaArray<Byte>): JPutDataRequest; cdecl;
    function setUrgent: JPutDataRequest; cdecl;
    function toString(boolean: Boolean): JString; cdecl; overload;
    function toString: JString; cdecl; overload;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJPutDataRequest = class(TJavaGenericImport<JPutDataRequestClass, JPutDataRequest>) end;

  JDataMapClass = interface(JObjectClass)
    ['{9E3EBC31-754F-4E8B-A666-3D3C37DC872B}']
    {class} function _GetTAG: JString; cdecl;
    {class} function arrayListFromBundleArrayList(arraylist: JArrayList): JArrayList; cdecl;
    {class} function fromBundle(bundle: JBundle): JDataMap; cdecl;
    {class} function fromByteArray(bytes: TJavaArray<Byte>): JDataMap; cdecl;
    {class} function init: JDataMap; cdecl;
    {class} property TAG: JString read _GetTAG;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataMap')]
  JDataMap = interface(JObject)
    ['{079FA525-DC6A-487B-9D9D-BC4E97B52784}']
    procedure clear; cdecl;
    function containsKey(string_1: JString): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(string_1: JString): JObject; cdecl;
    function getAsset(string_1: JString): JAsset; cdecl;
    function getBoolean(string_1: JString): Boolean; cdecl; overload;
    function getBoolean(string_1: JString; boolean: Boolean): Boolean; cdecl; overload;
    function getByte(string_1: JString; byte: Byte): Byte; cdecl; overload;
    function getByte(string_1: JString): Byte; cdecl; overload;
    function getByteArray(string_1: JString): TJavaArray<Byte>; cdecl;
    function getDataMap(string_1: JString): JDataMap; cdecl;
    function getDataMapArrayList(string_1: JString): JArrayList; cdecl;
    function getDouble(string_1: JString; double: Double): Double; cdecl; overload;
    function getDouble(string_1: JString): Double; cdecl; overload;
    function getFloat(string_1: JString): Single; cdecl; overload;
    function getFloat(string_1: JString; float: Single): Single; cdecl; overload;
    function getFloatArray(string_1: JString): TJavaArray<Single>; cdecl;
    function getInt(string_1: JString; int: Integer): Integer; cdecl; overload;
    function getInt(string_1: JString): Integer; cdecl; overload;
    function getIntegerArrayList(string_1: JString): JArrayList; cdecl;
    function getLong(string_1: JString): Int64; cdecl; overload;
    function getLong(string_1: JString; long: Int64): Int64; cdecl; overload;
    function getLongArray(string_1: JString): TJavaArray<Int64>; cdecl;
    function getString(string_1: JString): JString; cdecl; overload;
    function getString(string_1: JString; string_2: JString): JString; cdecl; overload;
    function getStringArray(string_1: JString): TJavaObjectArray<JString>; cdecl;
    function getStringArrayList(string_1: JString): JArrayList; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl;
    procedure putAll(datamap: JDataMap); cdecl;
    procedure putAsset(string_1: JString; asset: JAsset); cdecl;
    procedure putBoolean(string_1: JString; boolean: Boolean); cdecl;
    procedure putByte(string_1: JString; byte: Byte); cdecl;
    procedure putByteArray(string_1: JString; bytes: TJavaArray<Byte>); cdecl;
    procedure putDataMap(string_1: JString; datamap: JDataMap); cdecl;
    procedure putDataMapArrayList(string_1: JString; arraylist: JArrayList); cdecl;
    procedure putDouble(string_1: JString; double: Double); cdecl;
    procedure putFloat(string_1: JString; float: Single); cdecl;
    procedure putFloatArray(string_1: JString; floats: TJavaArray<Single>); cdecl;
    procedure putInt(string_1: JString; int: Integer); cdecl;
    procedure putIntegerArrayList(string_1: JString; arraylist: JArrayList); cdecl;
    procedure putLong(string_1: JString; long: Int64); cdecl;
    procedure putLongArray(string_1: JString; longs: TJavaArray<Int64>); cdecl;
    procedure putString(string_1: JString; string_2: JString); cdecl;
    procedure putStringArray(string_1: JString; strings: TJavaObjectArray<JString>); cdecl;
    procedure putStringArrayList(string_1: JString; arraylist: JArrayList); cdecl;
    function remove(string_1: JString): JObject; cdecl;
    function size: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    function toByteArray: TJavaArray<Byte>; cdecl;
    function toString: JString; cdecl;
  end;
  TJDataMap = class(TJavaGenericImport<JDataMapClass, JDataMap>) end;

  JDataMapItemClass = interface(JObjectClass)
    ['{27170694-8333-4142-9373-FF8D86B58600}']
    {class} function fromDataItem(dataitem: JDataItem): JDataMapItem; cdecl;
  end;

  [JavaSignature('com/google/android/gms/wearable/DataMapItem')]
  JDataMapItem = interface(JObject)
    ['{D5C37CCA-FC0F-49C2-872D-3C655A111E52}']
    function getDataMap: JDataMap; cdecl;
    function getUri: Jnet_Uri; cdecl;
  end;
  TJDataMapItem = class(TJavaGenericImport<JDataMapItemClass, JDataMapItem>) end;

  JPutDataMapRequestClass = interface(JObjectClass)
    ['{0E30B651-FFA7-4A5B-8F8C-5FC18B4FBA09}']
    {class} function create(string_1: JString): JPutDataMapRequest; cdecl;
    {class} function createFromDataMapItem(datamapitem: JDataMapItem): JPutDataMapRequest; cdecl;
    {class} function createWithAutoAppendedId(string_1: JString): JPutDataMapRequest; cdecl;
  end;

  [JavaSignature('com/google/android/gms/wearable/PutDataMapRequest')]
  JPutDataMapRequest = interface(JObject)
    ['{D095677B-F557-4CF5-95E4-DEBF2454D3AE}']
    function asPutDataRequest: JPutDataRequest; cdecl;
    function getDataMap: JDataMap; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function isUrgent: Boolean; cdecl;
    function setUrgent: JPutDataMapRequest; cdecl;
  end;
  TJPutDataMapRequest = class(TJavaGenericImport<JPutDataMapRequestClass, JPutDataMapRequest>) end;

  JRemoteActivityHelperClass = interface(JObjectClass)
    ['{3C442472-383C-4478-98B1-75EA040EB3E6}']
    {class} function _GetACTION_REMOTE_INTENT: JString; cdecl;
    {class} function _GetRESULT_FAILED: Integer; cdecl;
    {class} function _GetRESULT_OK: Integer; cdecl;
    {class} function getTargetIntent(intent: JIntent): JIntent; cdecl;
    {class} function getTargetNodeId(intent: JIntent): JString; cdecl;
    {class} function init(context: JContext; executor: JExecutor): JRemoteActivityHelper; cdecl;
    {class} property ACTION_REMOTE_INTENT: JString read _GetACTION_REMOTE_INTENT;
    {class} property RESULT_FAILED: Integer read _GetRESULT_FAILED;
    {class} property RESULT_OK: Integer read _GetRESULT_OK;
  end;

  [JavaSignature('androidx/wear/remote/interactions/RemoteActivityHelper')]
  JRemoteActivityHelper = interface(JObject)
    ['{D59D9FB3-8837-4493-BEA2-76BF859909FF}']
    function startRemoteActivity(targetIntent: JIntent; targetNodeId: JString): JListenableFuture; cdecl;
  end;
  TJRemoteActivityHelper = class(TJavaGenericImport<JRemoteActivityHelperClass, JRemoteActivityHelper>) end;

  JWearableClass = interface(JObjectClass)
    ['{5A3E2BA7-F5EB-4C65-98A8-36C0B2E5723E}']
//    {class} function _GetAPI: JApi; cdecl;
//    {class} function _GetCapabilityApi: JCapabilityApi; cdecl;
//    {class} function _GetChannelApi: JChannelApi; cdecl;
//    {class} function _GetDataApi: JDataApi; cdecl;
//    {class} function _GetMessageApi: JMessageApi; cdecl;
//    {class} function _GetNodeApi: JNodeApi; cdecl;
//    {class} function getCapabilityClient(activity: JActivity; wearableoptions: JWearable_WearableOptions): JCapabilityClient; cdecl; overload;
//    {class} function getCapabilityClient(activity: JActivity): JCapabilityClient; cdecl; overload;
//    {class} function getCapabilityClient(context: JContext; wearableoptions: JWearable_WearableOptions): JCapabilityClient; cdecl; overload;
//    {class} function getCapabilityClient(context: JContext): JCapabilityClient; cdecl; overload;
//    {class} function getChannelClient(context: JContext; wearableoptions: JWearable_WearableOptions): JChannelClient; cdecl; overload;
//    {class} function getChannelClient(activity: JActivity): JChannelClient; cdecl; overload;
//    {class} function getChannelClient(activity: JActivity; wearableoptions: JWearable_WearableOptions): JChannelClient; cdecl; overload;
//    {class} function getChannelClient(context: JContext): JChannelClient; cdecl; overload;
//    {class} function getDataClient(activity: JActivity; wearableoptions: JWearable_WearableOptions): JDataClient; cdecl; overload;
    {class} function getDataClient(context: JContext): JDataClient; cdecl; overload;
//    {class} function getDataClient(context: JContext; wearableoptions: JWearable_WearableOptions): JDataClient; cdecl; overload;
    {class} function getDataClient(activity: JActivity): JDataClient; cdecl; overload;
//    {class} function getMessageClient(activity: JActivity; wearableoptions: JWearable_WearableOptions): JMessageClient; cdecl; overload;
    {class} function getMessageClient(context: JContext): JMessageClient; cdecl; overload;
//    {class} function getMessageClient(context: JContext; wearableoptions: JWearable_WearableOptions): JMessageClient; cdecl; overload;
    {class} function getMessageClient(activity: JActivity): JMessageClient; cdecl; overload;
    {class} function getNodeClient(activity: JActivity): JNodeClient; cdecl; overload;
//    {class} function getNodeClient(activity: JActivity; wearableoptions: JWearable_WearableOptions): JNodeClient; cdecl; overload;
//    {class} function getNodeClient(context: JContext; wearableoptions: JWearable_WearableOptions): JNodeClient; cdecl; overload;
    {class} function getNodeClient(context: JContext): JNodeClient; cdecl; overload;
//    {class} property API: JApi read _GetAPI;
//    {class} property CapabilityApi: JCapabilityApi read _GetCapabilityApi;
//    {class} property ChannelApi: JChannelApi read _GetChannelApi;
//    {class} property DataApi: JDataApi read _GetDataApi;
//    {class} property MessageApi: JMessageApi read _GetMessageApi;
//    {class} property NodeApi: JNodeApi read _GetNodeApi;
  end;

  [JavaSignature('com/google/android/gms/wearable/Wearable')]
  JWearable = interface(JObject)
    ['{FB9B9F7A-2004-4539-9A6D-BFD7321244C6}']
  end;
  TJWearable = class(TJavaGenericImport<JWearableClass, JWearable>) end;

implementation

end.
