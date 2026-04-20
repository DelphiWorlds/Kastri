unit DW.iOSapi.SwiftCompat;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}


// NOTE: This file depends on importing Swift Support files as per the instructions at this link:
//   https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport
// This link shows which Swift frameworks are available in which versions of iOS:
//   https://github.com/MyBuzzTechnologies/UsefulDevResources/blob/master/iOS/SwiftFrameworkAvailability.md
//
// If your code requires one of the libraries listed below marked with a comment at the end like this:
//   // iOS 18.0
// if your app supports a LOWER version of iOS, you will need to add a:
//   -weak_library libname
// reference to the Options passed to the LD linker setting in Building > Delphi Compiler > Linking section in the Project Options
// for EACH library where libname is the filename for the library e.g.
//   -weak_library /usr/lib/swift/libswiftObservation.dylib
// Unfortunately this can result in a lot of -weak_library references, such as in:
//   https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted#linker-options
// Most of these libraries are present in iOS 15.0 or later, so if you set the Minimum iOS version supported value to 15.0 (or higher)
// you will not need ANY -weak_library references BUT of course your app will support that version of iOS or higher ONLY

interface

implementation

{$IF Defined(IOSSIMULATOR)}
procedure libswiftCompatibility50Loader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibility50.a';
procedure libswiftCompatibility51Loader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibility51.a';
procedure libswiftCompatibility56Loader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibility56.a';
procedure libswiftCompatibilityConcurrencyLoader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibilityConcurrency.a';
procedure libswiftCompatibilityDynamicReplacementsLoader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibilityDynamicReplacements.a';
{$ELSE}
procedure libswiftCompatibility50Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility50.a';
procedure libswiftCompatibility51Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility51.a';
procedure libswiftCompatibility56Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility56.a';
procedure libswiftCompatibilityConcurrencyLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityConcurrency.a';
procedure libswiftCompatibilityDynamicReplacementsLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityDynamicReplacements.a';
procedure libswiftCompatibilityPacksLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityPacks.a';
{$ENDIF}

procedure libswift_Builtin_floatLoader; cdecl; external '/usr/lib/swift/libswift_Builtin_float.dylib'; // iOS 18.0
procedure libswift_Concurrency; cdecl; external '/usr/lib/swift/libswift_Concurrency.dylib'; // iOS 15.0
procedure libswift_errnoLoader; cdecl; external '/usr/lib/swift/libswift_errno.dylib'; // iOS 18.0
procedure libswift_mathLoader; cdecl; external '/usr/lib/swift/libswift_math.dylib'; // iOS 18.0
procedure libswift_signalLoader; cdecl; external '/usr/lib/swift/libswift_signal.dylib'; // iOS 18.0
procedure libswift_stdioLoader; cdecl; external '/usr/lib/swift/libswift_stdio.dylib'; // iOS 18.0
procedure libswift_StringProcessingLoader; cdecl; external '/usr/lib/swift/libswift_StringProcessing.dylib'; // iOS 16.0
procedure libswift_timeLoader; cdecl; external '/usr/lib/swift/libswift_time.dylib'; // iOS 18.0
procedure libswiftCoreLoader; cdecl; external '/usr/lib/swift/libswiftCore.dylib';
procedure libswiftCoreFoundationLoader; cdecl; external '/usr/lib/swift/libswiftCoreFoundation.dylib';
procedure libswiftCoreGraphicsLoader; cdecl; external '/usr/lib/swift/libswiftCoreGraphics.dylib';
procedure libswiftCoreImageLoader; cdecl; external '/usr/lib/swift/libswiftCoreImage.dylib';
procedure libswiftDarwinLoader; cdecl; external '/usr/lib/swift/libswiftDarwin.dylib';
procedure libswiftDataDetectionLoader; cdecl; external '/usr/lib/swift/libswiftDataDetection.dylib'; // iOS 15.0
procedure libswiftDispatchLoader; cdecl; external '/usr/lib/swift/libswiftDispatch.dylib';
procedure libswiftFileProviderLoader; cdecl; external '/usr/lib/swift/libswiftFileProvider.dylib'; // iOS 15.0
procedure libswiftFoundationLoader; cdecl; external '/usr/lib/swift/libswiftFoundation.dylib';
procedure libswiftMetalLoader; cdecl; external '/usr/lib/swift/libswiftMetal.dylib';
procedure libswiftNetworkLoader; cdecl; external '/usr/lib/swift/libswiftNetwork.dylib';
procedure libswiftObjectiveCLoader; cdecl; external '/usr/lib/swift/libswiftObjectiveC.dylib';
procedure libswiftObservationLoader; cdecl; external '/usr/lib/swift/libswiftObservation.dylib'; // iOS 17.0
procedure libswiftosLoader; cdecl; external '/usr/lib/swift/libswiftos.dylib';
procedure libswiftOSLogLoader; cdecl; external '/usr/lib/swift/libswiftOSLog.dylib'; // iOS 15.0
procedure libswiftQuartzCoreLoader; cdecl; external '/usr/lib/swift/libswiftQuartzCore.dylib';
procedure libswiftsys_timeLoader; cdecl; external '/usr/lib/swift/libswiftsys_time.dylib';
procedure libswiftUIKitLoader; cdecl; external '/usr/lib/swift/libswiftUIKit.dylib';
procedure libswiftUniformTypeIdentifiersLoader; cdecl; external '/usr/lib/swift/libswiftUniformTypeIdentifiers.dylib'; // iOS 14.0
procedure libswiftunistdLoader; cdecl; external '/usr/lib/swift/libswiftunistd.dylib'; // iOS 18.0
procedure libswiftXPCLoader; cdecl; external '/usr/lib/swift/libswiftXPC.dylib'; // iOS 15.0

end.
