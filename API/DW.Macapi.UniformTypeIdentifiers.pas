unit DW.Macapi.UniformTypeIdentifiers;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation;

type
  UTType = interface;

  TNSItemProviderBlockMethod1 = procedure of object;
  TNSItemProviderBlockMethod2 = procedure(completionHandler: TNSItemProviderBlockMethod1) of object;
  TNSItemProviderBlockMethod3 = procedure(data: NSData; error: NSError) of object;
  TNSItemProviderBlockMethod4 = procedure(URL: NSURL; openInPlace: Boolean; error: NSError) of object;

  UTTypeClass = interface(NSObjectClass)
    ['{A0DB9D47-0208-41C9-8F81-7F9FAC1D60FF}']
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
    ['{7471DF3E-F91F-4D7F-85FE-E205A9E99813}']
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
  System.SysUtils;

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
  UniformTypeIdentifiersModule := LoadLibrary(libUniformTypeIdentifiers);

finalization
  if UniformTypeIdentifiersModule <> 0 then
    FreeLibrary(UniformTypeIdentifiersModule);

end.