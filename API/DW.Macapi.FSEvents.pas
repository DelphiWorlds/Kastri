unit DW.Macapi.FSEvents;

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
  Macapi.CocoaTypes, Macapi.CoreFoundation, Macapi.CoreServices, Macapi.Dispatch;

const
  kFSEventStreamCreateFlagNone = 0;
  kFSEventStreamCreateFlagUseCFTypes = 1;
  kFSEventStreamCreateFlagNoDefer = 2;
  kFSEventStreamCreateFlagWatchRoot = 4;
  kFSEventStreamCreateFlagIgnoreSelf = 8;
  kFSEventStreamCreateFlagFileEvents = 16;
  kFSEventStreamCreateFlagMarkSelf = 32;
  kFSEventStreamEventFlagNone = 0;
  kFSEventStreamEventFlagMustScanSubDirs = 1;
  kFSEventStreamEventFlagUserDropped = 2;
  kFSEventStreamEventFlagKernelDropped = 4;
  kFSEventStreamEventFlagEventIdsWrapped = 8;
  kFSEventStreamEventFlagHistoryDone = 16;
  kFSEventStreamEventFlagRootChanged = 32;
  kFSEventStreamEventFlagMount = 64;
  kFSEventStreamEventFlagUnmount = 128;
  kFSEventStreamEventFlagItemCreated = 256;
  kFSEventStreamEventFlagItemRemoved = 512;
  kFSEventStreamEventFlagItemInodeMetaMod = 1024;
  kFSEventStreamEventFlagItemRenamed = 2048;
  kFSEventStreamEventFlagItemModified = 4096;
  kFSEventStreamEventFlagItemFinderInfoMod = 8192;
  kFSEventStreamEventFlagItemChangeOwner = 16384;
  kFSEventStreamEventFlagItemXattrMod = 32768;
  kFSEventStreamEventFlagItemIsFile = 65536;
  kFSEventStreamEventFlagItemIsDir = 131072;
  kFSEventStreamEventFlagItemIsSymlink = 262144;
  kFSEventStreamEventFlagOwnEvent = 524288;
  kFSEventStreamEventFlagItemIsHardlink = 1048576;
  kFSEventStreamEventFlagItemIsLastHardlink = 2097152;
  kFSEventStreamEventIdSinceNow = 18446744073709551615;

type
  dev_t = Integer;
  FSEventStreamCreateFlags = UInt32;
  PFSEventStreamCreateFlags = ^FSEventStreamCreateFlags;
  FSEventStreamEventFlags = UInt32;
  PFSEventStreamEventFlags = ^FSEventStreamEventFlags;
  FSEventStreamEventId = UInt64;
  PFSEventStreamEventId = ^FSEventStreamEventId;
  FSEventStreamRef = Pointer;
  PFSEventStreamRef = ^FSEventStreamRef;
  ConstFSEventStreamRef = Pointer;
  PConstFSEventStreamRef = ^ConstFSEventStreamRef;

  FSEventStreamContext = record
    version: CFIndex;
    info: Pointer;
    retain: CFAllocatorRetainCallBack;
    release: CFAllocatorReleaseCallBack;
    copyDescription: CFAllocatorCopyDescriptionCallBack;
  end;

  PFSEventStreamContext = ^FSEventStreamContext;

  FSEventStreamCallback = procedure(streamRef: ConstFSEventStreamRef; clientCallBackInfo: Pointer; numEvents: LongWord; eventPaths: Pointer;
    eventFlags: FSEventStreamEventFlags; eventIds: FSEventStreamEventId); cdecl;
  PFSEventStreamCallback = ^FSEventStreamCallback;

function FSEventStreamCreate(allocator: CFAllocatorRef; callback: FSEventStreamCallback; context: PFSEventStreamContext; pathsToWatch: CFArrayRef;
  sinceWhen: FSEventStreamEventId; latency: CFTimeInterval; flags: FSEventStreamCreateFlags): FSEventStreamRef; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamCreate';
function FSEventStreamCreateRelativeToDevice(allocator: CFAllocatorRef; callback: FSEventStreamCallback; context: PFSEventStreamContext;
  deviceToWatch: dev_t; pathsToWatchRelativeToDevice: CFArrayRef; sinceWhen: FSEventStreamEventId; latency: CFTimeInterval;
  flags: FSEventStreamCreateFlags): FSEventStreamRef; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamCreateRelativeToDevice';
function FSEventStreamGetLatestEventId(streamRef: ConstFSEventStreamRef): FSEventStreamEventId; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamGetLatestEventId';
function FSEventStreamGetDeviceBeingWatched(streamRef: ConstFSEventStreamRef): dev_t; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamGetDeviceBeingWatched';
function FSEventStreamCopyPathsBeingWatched(streamRef: ConstFSEventStreamRef): CFArrayRef; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamCopyPathsBeingWatched';
function FSEventsGetCurrentEventId: FSEventStreamEventId; cdecl;
  external CoreServicesLib name _PU + 'FSEventsGetCurrentEventId';
function FSEventsCopyUUIDForDevice(dev: dev_t): CFUUIDRef; cdecl;
  external CoreServicesLib name _PU + 'FSEventsCopyUUIDForDevice';
function FSEventsGetLastEventIdForDeviceBeforeTime(dev: dev_t; time: CFAbsoluteTime): FSEventStreamEventId; cdecl;
  external CoreServicesLib name _PU + 'FSEventsGetLastEventIdForDeviceBeforeTime';
function FSEventsPurgeEventsForDeviceUpToEventId(dev: dev_t; eventID: FSEventStreamEventId): Boolean; cdecl;
  external CoreServicesLib name _PU + 'FSEventsPurgeEventsForDeviceUpToEventId';
procedure FSEventStreamRetain(streamRef: FSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamRetain';
procedure FSEventStreamRelease(streamRef: FSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamRelease';
procedure FSEventStreamScheduleWithRunLoop(streamRef: FSEventStreamRef; runloop: CFRunLoopRef; runloopMode: CFStringRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamScheduleWithRunLoop';
procedure FSEventStreamUnscheduleFromRunLoop(streamRef: FSEventStreamRef; runloop: CFRunLoopRef; runloopMode: CFStringRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamUnscheduleFromRunLoop';
procedure FSEventStreamSetDispatchQueue(streamRef: FSEventStreamRef; q: dispatch_queue_t); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamSetDispatchQueue';
procedure FSEventStreamInvalidate(streamRef: FSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamInvalidate';
function FSEventStreamStart(streamRef: FSEventStreamRef): Boolean; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamStart';
function FSEventStreamFlushAsync(streamRef: FSEventStreamRef): FSEventStreamEventId; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamFlushAsync';
procedure FSEventStreamFlushSync(streamRef: FSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamFlushSync';
procedure FSEventStreamStop(streamRef: FSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamStop';
procedure FSEventStreamShow(streamRef: ConstFSEventStreamRef); cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamShow';
function FSEventStreamCopyDescription(streamRef: ConstFSEventStreamRef): CFStringRef; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamCopyDescription';
function FSEventStreamSetExclusionPaths(streamRef: FSEventStreamRef; pathsToExclude: CFArrayRef): Boolean; cdecl;
  external CoreServicesLib name _PU + 'FSEventStreamSetExclusionPaths';

implementation

end.
