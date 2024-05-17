unit DW.iOSapi.SwiftCompat;

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


// NOTE: This file depends on importing Swift Support files as per the instructions at this link:
//   https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport

interface

implementation

procedure libswiftCompatibility50Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility50.a';
procedure libswiftCompatibility51Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility51.a';
procedure libswiftCompatibilityConcurrencyLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityConcurrency.a';
procedure libswiftCompatibilityDynamicReplacementsLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityDynamicReplacements.a';
procedure libswift_Concurrency; cdecl; external '/usr/lib/swift/libswift_Concurrency.dylib';
procedure libswift_StringProcessingLoader; cdecl; external '/usr/lib/swift/libswift_StringProcessing.dylib';
procedure libswiftCoreLoader; cdecl; external '/usr/lib/swift/libswiftCore.dylib';
procedure libswiftCoreFoundationLoader; cdecl; external '/usr/lib/swift/libswiftCoreFoundation.dylib';
procedure libswiftCoreGraphicsLoader; cdecl; external '/usr/lib/swift/libswiftCoreGraphics.dylib';
procedure libswiftCoreImageLoader; cdecl; external '/usr/lib/swift/libswiftCoreImage.dylib';
procedure libswiftDarwinLoader; cdecl; external '/usr/lib/swift/libswiftDarwin.dylib';
procedure libswiftDataDetectionLoader; cdecl; external '/usr/lib/swift/libswiftDataDetection.dylib';
procedure libswiftDispatchLoader; cdecl; external '/usr/lib/swift/libswiftDispatch.dylib';
procedure libswiftFileProviderLoader; cdecl; external '/usr/lib/swift/libswiftFileProvider.dylib';
procedure libswiftFoundationLoader; cdecl; external '/usr/lib/swift/libswiftFoundation.dylib';
procedure libswiftMetalLoader; cdecl; external '/usr/lib/swift/libswiftMetal.dylib';
procedure libswiftObjectiveCLoader; cdecl; external '/usr/lib/swift/libswiftObjectiveC.dylib';
procedure libswiftQuartzCoreLoader; cdecl; external '/usr/lib/swift/libswiftQuartzCore.dylib';
procedure libswiftUIKitLoader; cdecl; external '/usr/lib/swift/libswiftUIKit.dylib';

end.
