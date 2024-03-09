unit DW.iOSapi.IOSurface;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Mach,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  kIOSurfaceLockReadOnly = 1;
  kIOSurfaceLockAvoidSync = 2;
  kIOSurfacePurgeableNonVolatile = 0;
  kIOSurfacePurgeableVolatile = 1;
  kIOSurfacePurgeableEmpty = 2;
  kIOSurfacePurgeableKeepCurrent = 3;
  kIOSurfaceDefaultCache = 0;
  kIOSurfaceInhibitCache = 1;
  kIOSurfaceWriteThruCache = 2;
  kIOSurfaceCopybackCache = 3;
  kIOSurfaceWriteCombineCache = 4;
  kIOSurfaceCopybackInnerCache = 5;
  kIOSurfaceMapCacheShift = 8;
  kIOSurfaceMapDefaultCache = 0;
  kIOSurfaceMapInhibitCache = 256;
  kIOSurfaceMapWriteThruCache = 512;
  kIOSurfaceMapCopybackCache = 768;
  kIOSurfaceMapWriteCombineCache = 1024;
  kIOSurfaceMapCopybackInnerCache = 1280;
  kIOSurfaceComponentNameUnknown = 0;
  kIOSurfaceComponentNameAlpha = 1;
  kIOSurfaceComponentNameRed = 2;
  kIOSurfaceComponentNameGreen = 3;
  kIOSurfaceComponentNameBlue = 4;
  kIOSurfaceComponentNameLuma = 5;
  kIOSurfaceComponentNameChromaRed = 6;
  kIOSurfaceComponentNameChromaBlue = 7;
  kIOSurfaceComponentTypeUnknown = 0;
  kIOSurfaceComponentTypeUnsignedInteger = 1;
  kIOSurfaceComponentTypeSignedInteger = 2;
  kIOSurfaceComponentTypeFloat = 3;
  kIOSurfaceComponentRangeUnknown = 0;
  kIOSurfaceComponentRangeFullRange = 1;
  kIOSurfaceComponentRangeVideoRange = 2;
  kIOSurfaceComponentRangeWideRange = 3;
  kIOSurfaceSubsamplingUnknown = 0;
  kIOSurfaceSubsamplingNone = 1;
  kIOSurfaceSubsampling422 = 2;
  kIOSurfaceSubsampling420 = 3;
  kIOSurfaceSubsampling411 = 4;

type
  IOSurface = interface;

  PUInt32 = ^UInt32;
  P__IOSurface = Pointer;
  PP__IOSurface = ^P__IOSurface;

  IOSurfaceID = UInt32;
  IOSurfaceLockOptions = NSInteger;
  IOSurfacePurgeabilityState = NSInteger;
  IOSurfacePropertyKey = NSString;
  IOSurfaceRef = Pointer;
  PIOSurfaceRef = ^IOSurfaceRef;
  IOSurfaceComponentName = NSInteger;
  IOSurfaceComponentType = NSInteger;
  IOSurfaceComponentRange = NSInteger;
  IOSurfaceSubsampling = NSInteger;
  PIOSurfacePurgeabilityState = ^IOSurfacePurgeabilityState;

  IOSurfaceClass = interface(NSObjectClass)
    ['{FF658214-0315-4AF4-8528-DD46D1B0CD77}']
  end;

  IOSurface = interface(NSObject)
    ['{B0EF7132-3197-4425-819D-7EF5126F52D0}']
    function allAttachments: NSDictionary; cdecl;
    function allocationSize: NSInteger; cdecl;
    function allowsPixelSizeCasting: Boolean; cdecl;
    function attachmentForKey(key: NSString): Pointer; cdecl;
    function baseAddress: Pointer; cdecl;
    function baseAddressOfPlaneAtIndex(planeIndex: NSUInteger): Pointer; cdecl;
    function bytesPerElement: NSInteger; cdecl;
    function bytesPerElementOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
    function bytesPerRow: NSInteger; cdecl;
    function bytesPerRowOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
    procedure decrementUseCount; cdecl;
    function elementHeight: NSInteger; cdecl;
    function elementHeightOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
    function elementWidth: NSInteger; cdecl;
    function elementWidthOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
    function height: NSInteger; cdecl;
    function heightOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
    procedure incrementUseCount; cdecl;
    function initWithProperties(properties: NSDictionary): Pointer; cdecl;
    function isInUse: Boolean; cdecl;
    function localUseCount: Int32; cdecl;
    function lockWithOptions(options: IOSurfaceLockOptions; seed: PUInt32): kern_return_t; cdecl;
    function pixelFormat: OSType; cdecl;
    function planeCount: NSUInteger; cdecl;
    procedure removeAllAttachments; cdecl;
    procedure removeAttachmentForKey(key: NSString); cdecl;
    function seed: UInt32; cdecl;
    procedure setAllAttachments(dict: NSDictionary); cdecl;
    procedure setAttachment(anObject: Pointer; forKey: NSString); cdecl;
    function setPurgeable(newState: IOSurfacePurgeabilityState; oldState: PIOSurfacePurgeabilityState): kern_return_t; cdecl;
    function unlockWithOptions(options: IOSurfaceLockOptions; seed: PUInt32): kern_return_t; cdecl;
    function width: NSInteger; cdecl;
    function widthOfPlaneAtIndex(planeIndex: NSUInteger): NSInteger; cdecl;
  end;
  TIOSurface = class(TOCGenericImport<IOSurfaceClass, IOSurface>) end;

function IOSurfacePropertyKeyAllocSize: IOSurfacePropertyKey;
function IOSurfacePropertyKeyWidth: IOSurfacePropertyKey;
function IOSurfacePropertyKeyHeight: IOSurfacePropertyKey;
function IOSurfacePropertyKeyBytesPerRow: IOSurfacePropertyKey;
function IOSurfacePropertyKeyBytesPerElement: IOSurfacePropertyKey;
function IOSurfacePropertyKeyElementWidth: IOSurfacePropertyKey;
function IOSurfacePropertyKeyElementHeight: IOSurfacePropertyKey;
function IOSurfacePropertyKeyOffset: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneInfo: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneWidth: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneHeight: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneBytesPerRow: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneOffset: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneSize: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneBase: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneBytesPerElement: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneElementWidth: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPlaneElementHeight: IOSurfacePropertyKey;
function IOSurfacePropertyKeyCacheMode: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPixelFormat: IOSurfacePropertyKey;
function IOSurfacePropertyKeyPixelSizeCastingAllowed: IOSurfacePropertyKey;
function IOSurfacePropertyAllocSizeKey: IOSurfacePropertyKey;
function kIOSurfaceAllocSize: CFStringRef;
function kIOSurfaceWidth: CFStringRef;
function kIOSurfaceHeight: CFStringRef;
function kIOSurfaceBytesPerRow: CFStringRef;
function kIOSurfaceBytesPerElement: CFStringRef;
function kIOSurfaceElementWidth: CFStringRef;
function kIOSurfaceElementHeight: CFStringRef;
function kIOSurfaceOffset: CFStringRef;
function kIOSurfacePlaneInfo: CFStringRef;
function kIOSurfacePlaneWidth: CFStringRef;
function kIOSurfacePlaneHeight: CFStringRef;
function kIOSurfacePlaneBytesPerRow: CFStringRef;
function kIOSurfacePlaneOffset: CFStringRef;
function kIOSurfacePlaneSize: CFStringRef;
function kIOSurfacePlaneBase: CFStringRef;
function kIOSurfacePlaneBitsPerElement: CFStringRef;
function kIOSurfacePlaneBytesPerElement: CFStringRef;
function kIOSurfacePlaneElementWidth: CFStringRef;
function kIOSurfacePlaneElementHeight: CFStringRef;
function kIOSurfaceCacheMode: CFStringRef;
function kIOSurfaceIsGlobal: CFStringRef;
function kIOSurfacePixelFormat: CFStringRef;
function kIOSurfacePixelSizeCastingAllowed: CFStringRef;
function kIOSurfacePlaneComponentBitDepths: CFStringRef;
function kIOSurfacePlaneComponentBitOffsets: CFStringRef;
function kIOSurfacePlaneComponentNames: CFStringRef;
function kIOSurfacePlaneComponentTypes: CFStringRef;
function kIOSurfacePlaneComponentRanges: CFStringRef;
function kIOSurfaceSubsampling: CFStringRef;

const
  libIOSurface = '/System/Library/Frameworks/IOSurface.framework/IOSurface';

function IOSurfaceGetTypeID: CFTypeID; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetTypeID';

function IOSurfaceCreate(properties: CFDictionaryRef): IOSurfaceRef; cdecl;
  external libIOSurface name _PU + 'IOSurfaceCreate';

function IOSurfaceLookup(csid: IOSurfaceID): IOSurfaceRef; cdecl;
  external libIOSurface name _PU + 'IOSurfaceLookup';

function IOSurfaceGetID(buffer: IOSurfaceRef): IOSurfaceID; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetID';

function IOSurfaceLock(buffer: IOSurfaceRef; options: IOSurfaceLockOptions; seed: PUInt32): kern_return_t; cdecl;
  external libIOSurface name _PU + 'IOSurfaceLock';

function IOSurfaceUnlock(buffer: IOSurfaceRef; options: IOSurfaceLockOptions; seed: PUInt32): kern_return_t; cdecl;
  external libIOSurface name _PU + 'IOSurfaceUnlock';

function IOSurfaceGetAllocSize(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetAllocSize';

function IOSurfaceGetWidth(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetWidth';

function IOSurfaceGetHeight(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetHeight';

function IOSurfaceGetBytesPerElement(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBytesPerElement';

function IOSurfaceGetBytesPerRow(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBytesPerRow';

function IOSurfaceGetBaseAddress(buffer: IOSurfaceRef): Pointer; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBaseAddress';

function IOSurfaceGetElementWidth(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetElementWidth';

function IOSurfaceGetElementHeight(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetElementHeight';

function IOSurfaceGetPixelFormat(buffer: IOSurfaceRef): OSType; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetPixelFormat';

function IOSurfaceGetSeed(buffer: IOSurfaceRef): UInt32; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetSeed';

function IOSurfaceGetPlaneCount(buffer: IOSurfaceRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetPlaneCount';

function IOSurfaceGetWidthOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetWidthOfPlane';

function IOSurfaceGetHeightOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetHeightOfPlane';

function IOSurfaceGetBytesPerElementOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBytesPerElementOfPlane';

function IOSurfaceGetBytesPerRowOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBytesPerRowOfPlane';

function IOSurfaceGetBaseAddressOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): Pointer; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBaseAddressOfPlane';

function IOSurfaceGetElementWidthOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetElementWidthOfPlane';

function IOSurfaceGetElementHeightOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetElementHeightOfPlane';

function IOSurfaceGetNumberOfComponentsOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetNumberOfComponentsOfPlane';

function IOSurfaceGetNameOfComponentOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt; componentIndex: NativeUInt): IOSurfaceComponentName; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetNameOfComponentOfPlane';

function IOSurfaceGetTypeOfComponentOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt; componentIndex: NativeUInt): IOSurfaceComponentType; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetTypeOfComponentOfPlane';

function IOSurfaceGetRangeOfComponentOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt; componentIndex: NativeUInt): IOSurfaceComponentRange; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetRangeOfComponentOfPlane';

function IOSurfaceGetBitDepthOfComponentOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt; componentIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBitDepthOfComponentOfPlane';

function IOSurfaceGetBitOffsetOfComponentOfPlane(buffer: IOSurfaceRef; planeIndex: NativeUInt; componentIndex: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetBitOffsetOfComponentOfPlane';

function IOSurfaceGetSubsampling(buffer: IOSurfaceRef): IOSurfaceSubsampling; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetSubsampling';

procedure IOSurfaceSetValue(buffer: IOSurfaceRef; key: CFStringRef; value: CFTypeRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceSetValue';

function IOSurfaceCopyValue(buffer: IOSurfaceRef; key: CFStringRef): CFTypeRef; cdecl;
  external libIOSurface name _PU + 'IOSurfaceCopyValue';

procedure IOSurfaceRemoveValue(buffer: IOSurfaceRef; key: CFStringRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceRemoveValue';

procedure IOSurfaceSetValues(buffer: IOSurfaceRef; keysAndValues: CFDictionaryRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceSetValues';

function IOSurfaceCopyAllValues(buffer: IOSurfaceRef): CFDictionaryRef; cdecl;
  external libIOSurface name _PU + 'IOSurfaceCopyAllValues';

procedure IOSurfaceRemoveAllValues(buffer: IOSurfaceRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceRemoveAllValues';

function IOSurfaceCreateMachPort(buffer: IOSurfaceRef): mach_port_t; cdecl;
  external libIOSurface name _PU + 'IOSurfaceCreateMachPort';

function IOSurfaceLookupFromMachPort(port: mach_port_t): IOSurfaceRef; cdecl;
  external libIOSurface name _PU + 'IOSurfaceLookupFromMachPort';

function IOSurfaceGetPropertyMaximum(&property: CFStringRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetPropertyMaximum';

function IOSurfaceGetPropertyAlignment(&property: CFStringRef): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetPropertyAlignment';

function IOSurfaceAlignProperty(&property: CFStringRef; value: NativeUInt): NativeUInt; cdecl;
  external libIOSurface name _PU + 'IOSurfaceAlignProperty';

procedure IOSurfaceIncrementUseCount(buffer: IOSurfaceRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceIncrementUseCount';

procedure IOSurfaceDecrementUseCount(buffer: IOSurfaceRef); cdecl;
  external libIOSurface name _PU + 'IOSurfaceDecrementUseCount';

function IOSurfaceGetUseCount(buffer: IOSurfaceRef): Int32; cdecl;
  external libIOSurface name _PU + 'IOSurfaceGetUseCount';

function IOSurfaceIsInUse(buffer: IOSurfaceRef): Boolean; cdecl;
  external libIOSurface name _PU + 'IOSurfaceIsInUse';

function IOSurfaceAllowsPixelSizeCasting(buffer: IOSurfaceRef): Boolean; cdecl;
  external libIOSurface name _PU + 'IOSurfaceAllowsPixelSizeCasting';

function IOSurfaceSetPurgeable(buffer: IOSurfaceRef; newState: UInt32; oldState: PUInt32): kern_return_t; cdecl;
  external libIOSurface name _PU + 'IOSurfaceSetPurgeable';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  IOSurfaceModule: THandle;
{$ENDIF}

function IOSurfacePropertyKeyAllocSize: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyAllocSize');
end;

function IOSurfacePropertyKeyWidth: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyWidth');
end;

function IOSurfacePropertyKeyHeight: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyHeight');
end;

function IOSurfacePropertyKeyBytesPerRow: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyBytesPerRow');
end;

function IOSurfacePropertyKeyBytesPerElement: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyBytesPerElement');
end;

function IOSurfacePropertyKeyElementWidth: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyElementWidth');
end;

function IOSurfacePropertyKeyElementHeight: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyElementHeight');
end;

function IOSurfacePropertyKeyOffset: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyOffset');
end;

function IOSurfacePropertyKeyPlaneInfo: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneInfo');
end;

function IOSurfacePropertyKeyPlaneWidth: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneWidth');
end;

function IOSurfacePropertyKeyPlaneHeight: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneHeight');
end;

function IOSurfacePropertyKeyPlaneBytesPerRow: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneBytesPerRow');
end;

function IOSurfacePropertyKeyPlaneOffset: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneOffset');
end;

function IOSurfacePropertyKeyPlaneSize: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneSize');
end;

function IOSurfacePropertyKeyPlaneBase: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneBase');
end;

function IOSurfacePropertyKeyPlaneBytesPerElement: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneBytesPerElement');
end;

function IOSurfacePropertyKeyPlaneElementWidth: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneElementWidth');
end;

function IOSurfacePropertyKeyPlaneElementHeight: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPlaneElementHeight');
end;

function IOSurfacePropertyKeyCacheMode: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyCacheMode');
end;

function IOSurfacePropertyKeyPixelFormat: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPixelFormat');
end;

function IOSurfacePropertyKeyPixelSizeCastingAllowed: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyKeyPixelSizeCastingAllowed');
end;

function IOSurfacePropertyAllocSizeKey: IOSurfacePropertyKey;
begin
  Result := CocoaNSStringConst(libIOSurface, 'IOSurfacePropertyAllocSizeKey');
end;

function kIOSurfaceAllocSize: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceAllocSize'));
end;

function kIOSurfaceWidth: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceWidth'));
end;

function kIOSurfaceHeight: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceHeight'));
end;

function kIOSurfaceBytesPerRow: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceBytesPerRow'));
end;

function kIOSurfaceBytesPerElement: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceBytesPerElement'));
end;

function kIOSurfaceElementWidth: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceElementWidth'));
end;

function kIOSurfaceElementHeight: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceElementHeight'));
end;

function kIOSurfaceOffset: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceOffset'));
end;

function kIOSurfacePlaneInfo: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneInfo'));
end;

function kIOSurfacePlaneWidth: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneWidth'));
end;

function kIOSurfacePlaneHeight: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneHeight'));
end;

function kIOSurfacePlaneBytesPerRow: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneBytesPerRow'));
end;

function kIOSurfacePlaneOffset: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneOffset'));
end;

function kIOSurfacePlaneSize: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneSize'));
end;

function kIOSurfacePlaneBase: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneBase'));
end;

function kIOSurfacePlaneBitsPerElement: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneBitsPerElement'));
end;

function kIOSurfacePlaneBytesPerElement: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneBytesPerElement'));
end;

function kIOSurfacePlaneElementWidth: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneElementWidth'));
end;

function kIOSurfacePlaneElementHeight: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneElementHeight'));
end;

function kIOSurfaceCacheMode: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceCacheMode'));
end;

function kIOSurfaceIsGlobal: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceIsGlobal'));
end;

function kIOSurfacePixelFormat: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePixelFormat'));
end;

function kIOSurfacePixelSizeCastingAllowed: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePixelSizeCastingAllowed'));
end;

function kIOSurfacePlaneComponentBitDepths: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneComponentBitDepths'));
end;

function kIOSurfacePlaneComponentBitOffsets: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneComponentBitOffsets'));
end;

function kIOSurfacePlaneComponentNames: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneComponentNames'));
end;

function kIOSurfacePlaneComponentTypes: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneComponentTypes'));
end;

function kIOSurfacePlaneComponentRanges: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfacePlaneComponentRanges'));
end;

function kIOSurfaceSubsampling: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libIOSurface, 'kIOSurfaceSubsampling'));
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  IOSurfaceModule := dlopen(MarshaledAString(libIOSurface), RTLD_LAZY);

finalization
  dlclose(IOSurfaceModule)
{$ENDIF}

end.