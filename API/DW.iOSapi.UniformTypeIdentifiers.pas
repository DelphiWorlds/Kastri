unit DW.iOSapi.UniformTypeIdentifiers;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  UTType = interface;

  TNSItemProviderBlockMethod1 = procedure of object;
  TNSItemProviderBlockMethod2 = procedure(completionHandler: TNSItemProviderBlockMethod1) of object;
  TNSItemProviderBlockMethod3 = procedure(data: NSData; error: NSError) of object;
  TNSItemProviderBlockMethod4 = procedure(URL: NSURL; openInPlace: Boolean; error: NSError) of object;

  UTTypeClass = interface(NSObjectClass)
    ['{32F2DE19-85A5-4684-A290-ADCAE6836E51}']
    {class} function exportedTypeWithIdentifier(identifier: NSString; conformingToType: UTType): UTType; overload; cdecl;
    {class} function exportedTypeWithIdentifier(identifier: NSString): UTType; overload; cdecl;
    {class} function importedTypeWithIdentifier(identifier: NSString; conformingToType: UTType): UTType; overload; cdecl;
    {class} function importedTypeWithIdentifier(identifier: NSString): UTType; overload; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function typesWithTag(tag: NSString; tagClass: NSString; conformingToType: UTType): NSArray; cdecl;
    {class} function typeWithFilenameExtension(filenameExtension: NSString): Pointer; overload; cdecl;
    {class} function typeWithFilenameExtension(filenameExtension: NSString; conformingToType: UTType): Pointer; overload; cdecl;
    {class} function typeWithIdentifier(identifier: NSString): Pointer; cdecl;
    {class} function typeWithMIMEType(mimeType: NSString): Pointer; overload; cdecl;
    {class} function typeWithMIMEType(mimeType: NSString; conformingToType: UTType): Pointer; overload; cdecl;
    {class} function typeWithTag(tag: NSString; tagClass: NSString; conformingToType: UTType): Pointer; cdecl;
  end;

  UTType = interface(NSObject)
    ['{ABD80F68-933F-4FE7-A963-51C556E51FE9}']
    function conformsToType(&type: UTType): Boolean; cdecl;
    function identifier: NSString; cdecl;
    function isDeclared: Boolean; cdecl;
    function isDynamic: Boolean; cdecl;
    function isPublicType: Boolean; cdecl;
    function isSubtypeOfType(&type: UTType): Boolean; cdecl;
    function isSupertypeOfType(&type: UTType): Boolean; cdecl;
    function localizedDescription: NSString; cdecl;
    function preferredFilenameExtension: NSString; cdecl;
    function preferredMIMEType: NSString; cdecl;
    function referenceURL: NSURL; cdecl;
    function supertypes: NSSet; cdecl;
    function tags: NSDictionary; cdecl;
    function version: NSNumber; cdecl;
  end;
  TUTType = class(TOCGenericImport<UTTypeClass, UTType>) end;

function UTTagClassFilenameExtension: NSString;
function UTTagClassMIMEType: NSString;

const
  libUniformTypeIdentifiers = '/System/Library/Frameworks/UniformTypeIdentifiers.framework/UniformTypeIdentifiers';

implementation

uses
  Posix.Dlfcn;

var
  UniformTypeIdentifiersModule: THandle;

function UTTagClassFilenameExtension: NSString;
begin
  Result := CocoaNSStringConst(libUniformTypeIdentifiers, 'UTTagClassFilenameExtension');
end;

function UTTagClassMIMEType: NSString;
begin
  Result := CocoaNSStringConst(libUniformTypeIdentifiers, 'UTTagClassMIMEType');
end;

initialization
  UniformTypeIdentifiersModule := dlopen(MarshaledAString(libUniformTypeIdentifiers), RTLD_LAZY);

finalization
  dlclose(UniformTypeIdentifiersModule);

end.