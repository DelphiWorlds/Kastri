unit DW.Macapi.AppKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.ObjectiveC, Macapi.AppKit;

type
  NSOpenPanel = interface;
  NSSavePanel = interface;

  NSModalResponse = NSInteger;

  TNSSavePanelBlockMethod1 = procedure(result: NSModalResponse) of object;

  NSSavePanelClass = interface(NSPanelClass)
    ['{E810D910-C0A9-4EFE-AFAE-FD417F913EEB}']
    {class} function savePanel: NSSavePanel; cdecl;
  end;

  NSSavePanel = interface(NSPanel)
    ['{FA70EDA2-6CE6-484F-AB17-FB138E680978}']
    function accessoryView: NSView; cdecl;
    function allowedFileTypes: NSArray; cdecl;
    function allowsOtherFileTypes: Boolean; cdecl;
    [MethodName('beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:')]
    procedure beginSheetForDirectory(path: NSString; name: NSString; docWindow: NSWindow; delegate: Pointer; didEndSelector: SEL;
      contextInfo: Pointer); cdecl; // API_DEPRECATED("Use beginSheetModalForWindow:completionHandler: instead. The following parameters are replaced by properties: 'path' is replaced by 'directoryURL' and 'name' by 'nameFieldStringValue'.", macos(10.0,10.6))
    [MethodName('beginSheetModalForWindow:completionHandler:')]
    procedure beginSheetModalForWindow(window: NSWindow; handler: TNSSavePanelBlockMethod1); cdecl;
    procedure beginWithCompletionHandler(handler: TNSSavePanelBlockMethod1); cdecl;
    procedure cancel(sender: Pointer); cdecl;
    function canCreateDirectories: Boolean; cdecl;
    function canSelectHiddenExtension: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function directory: NSString; cdecl; // API_DEPRECATED("Use -directoryURL instead", macos(10.0,10.6))
    function directoryURL: NSURL; cdecl;
    function filename: NSString; cdecl; // API_DEPRECATED("Use -URL instead", macos(10.0,10.6))
    function isExpanded: Boolean; cdecl;
    function isExtensionHidden: Boolean; cdecl;
    function message: NSString; cdecl;
    function nameFieldLabel: NSString; cdecl;
    function nameFieldStringValue: NSString; cdecl;
    procedure ok(sender: Pointer); cdecl;
    function prompt: NSString; cdecl;
    function requiredFileType: NSString; cdecl; // API_DEPRECATED("Use -allowedFileTypes instead", macos(10.0,10.6))
    function runModal: NSModalResponse; cdecl;
    [MethodName('runModalForDirectory:file:')]
    function runModalForDirectory(path: NSString; name: NSString): NSInteger; cdecl; // API_DEPRECATED("Use -runModal instead. The following parameters are replaced by properties: 'path' is replaced by 'directoryURL' and 'name' by 'nameFieldStringValue'.", macos(10.0,10.6))
    procedure selectText(sender: Pointer); cdecl; // API_DEPRECATED("Default implementation does nothing.", macos(10.0,10.3))
    procedure setAccessoryView(accessoryView: NSView); cdecl;
    procedure setAllowedFileTypes(allowedFileTypes: NSArray); cdecl;
    procedure setAllowsOtherFileTypes(allowsOtherFileTypes: Boolean); cdecl;
    procedure setCanCreateDirectories(canCreateDirectories: Boolean); cdecl;
    procedure setCanSelectHiddenExtension(canSelectHiddenExtension: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDirectory(path: NSString); cdecl; // API_DEPRECATED("Use -setDirectoryURL: instead", macos(10.0,10.6))
    procedure setDirectoryURL(directoryURL: NSURL); cdecl;
    procedure setExtensionHidden(extensionHidden: Boolean); cdecl;
    procedure setMessage(message: NSString); cdecl;
    procedure setNameFieldLabel(nameFieldLabel: NSString); cdecl;
    procedure setNameFieldStringValue(nameFieldStringValue: NSString); cdecl;
    procedure setPrompt(prompt: NSString); cdecl;
    procedure setRequiredFileType(&type: NSString); cdecl; // API_DEPRECATED("Use -setAllowedFileTypes: instead", macos(10.0,10.6))
    procedure setShowsHiddenFiles(showsHiddenFiles: Boolean); cdecl;
    procedure setShowsTagField(showsTagField: Boolean); cdecl;
    procedure setTagNames(tagNames: NSArray); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setTreatsFilePackagesAsDirectories(treatsFilePackagesAsDirectories: Boolean); cdecl;
    function showsHiddenFiles: Boolean; cdecl;
    function showsTagField: Boolean; cdecl;
    function tagNames: NSArray; cdecl;
    function title: NSString; cdecl;
    function treatsFilePackagesAsDirectories: Boolean; cdecl;
    function URL: NSURL; cdecl;
    procedure validateVisibleColumns; cdecl;
  end;
  TNSSavePanel = class(TOCGenericImport<NSSavePanelClass, NSSavePanel>) end;

  NSOpenSavePanelDelegate = interface(IObjectiveC)
    ['{9050433D-C976-4C36-AC96-E51F8176F039}']
    [MethodName('panel:didChangeToDirectoryURL:')]
    procedure panelDidChangeToDirectoryURL(sender: Pointer; url: NSURL); cdecl;
    procedure panelSelectionDidChange(sender: Pointer); cdecl;
    [MethodName('panel:shouldEnableURL:')]
    function panelShouldEnableURL(sender: Pointer; url: NSURL): Boolean; cdecl;
    [MethodName('panel:userEnteredFilename:confirmed:')]
    function panelUserEnteredFilename(sender: Pointer; filename: NSString; okFlag: Boolean): NSString; cdecl;
    [MethodName('panel:validateURL:error:')]
    function panelValidateURL(sender: Pointer; url: NSURL; outError: PPointer): Boolean; cdecl;
    [MethodName('panel:willExpand:')]
    procedure panelWillExpand(sender: Pointer; expanding: Boolean); cdecl;
  end;

  NSOpenPanelClass = interface(NSSavePanelClass)
    ['{FBF31A4B-7C94-45BA-BCC4-F266AD4B8695}']
    {class} function openPanel: NSOpenPanel; cdecl;
  end;

  NSOpenPanel = interface(NSSavePanel)
    ['{4D11EF4C-D9B7-43F6-8288-60675540FDCB}']
    function allowsMultipleSelection: Boolean; cdecl;
    [MethodName('beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:')]
    procedure beginForDirectory(path: NSString; name: NSString; fileTypes: NSArray; delegate: Pointer; didEndSelector: SEL; contextInfo: Pointer); cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    [MethodName('beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:')]
    procedure beginSheetForDirectory(path: NSString; name: NSString; fileTypes: NSArray; docWindow: NSWindow; delegate: Pointer; didEndSelector:
      SEL; contextInfo: Pointer); cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function canChooseDirectories: Boolean; cdecl;
    function canChooseFiles: Boolean; cdecl;
    function canDownloadUbiquitousContents: Boolean; cdecl;
    function canResolveUbiquitousConflicts: Boolean; cdecl;
    function filenames: NSArray; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function isAccessoryViewDisclosed: Boolean; cdecl;
    function resolvesAliases: Boolean; cdecl;
    [MethodName('runModalForDirectory:file:types:')]
    function runModalForDirectory(path: NSString; name: NSString; fileTypes: NSArray): NSInteger; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function runModalForTypes(fileTypes: NSArray): NSInteger; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    procedure setAccessoryViewDisclosed(accessoryViewDisclosed: Boolean); cdecl;
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    procedure setCanChooseDirectories(canChooseDirectories: Boolean); cdecl;
    procedure setCanChooseFiles(canChooseFiles: Boolean); cdecl;
    procedure setCanDownloadUbiquitousContents(canDownloadUbiquitousContents: Boolean); cdecl;
    procedure setCanResolveUbiquitousConflicts(canResolveUbiquitousConflicts: Boolean); cdecl;
    procedure setResolvesAliases(resolvesAliases: Boolean); cdecl;
    function URLs: NSArray; cdecl;
  end;
  TNSOpenPanel = class(TOCGenericImport<NSOpenPanelClass, NSOpenPanel>) end;

implementation

end.
