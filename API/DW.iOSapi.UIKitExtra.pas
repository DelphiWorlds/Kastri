unit DW.iOSapi.UIKitExtra;

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
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

type
  NSDiffableDataSourceSectionSnapshotClass = interface(NSObjectClass)
    ['{A00072CA-19FD-4BB5-A939-15BE9FB51717}']
  end;

  NSDiffableDataSourceSectionSnapshot = interface(NSObject)
    ['{A285E090-782F-414B-8145-954AC6E163B3}']
    procedure appendItems(items: NSArray; intoParentItem: Pointer); overload; cdecl;
    procedure appendItems(items: NSArray); overload; cdecl;
    procedure collapseItems(items: NSArray); cdecl;
    function containsItem(item: Pointer): Boolean; cdecl;
    procedure deleteAllItems; cdecl;
    procedure deleteItems(items: NSArray); cdecl;
    function expandedItems: NSArray; cdecl;
    procedure expandItems(items: NSArray); cdecl;
    function indexOfItem(item: Pointer): NSInteger; cdecl;
    procedure insertItems(items: NSArray; beforeItem: Pointer); cdecl;
    [MethodName('insertItems:afterItem:')]
    procedure insertItemsAfterItem(items: NSArray; afterItem: Pointer); cdecl;
    procedure insertSnapshot(snapshot: NSDiffableDataSourceSectionSnapshot; beforeItem: Pointer); cdecl;
    [MethodName('insertSnapshot:afterItem:')]
    function insertSnapshotAfterItem(snapshot: NSDiffableDataSourceSectionSnapshot; afterItem: Pointer): Pointer; cdecl;
    function isExpanded(item: Pointer): Boolean; cdecl;
    function isVisible(item: Pointer): Boolean; cdecl;
    function items: NSArray; cdecl;
    function levelOfItem(item: Pointer): NSInteger; cdecl;
    function parentOfChildItem(childItem: Pointer): Pointer; cdecl;
    procedure replaceChildrenOfParentItem(parentItem: Pointer; withSnapshot: NSDiffableDataSourceSectionSnapshot); cdecl;
    function rootItems: NSArray; cdecl;
    function snapshotOfParentItem(parentItem: Pointer; includingParentItem: Boolean): NSDiffableDataSourceSectionSnapshot; overload; cdecl;
    function snapshotOfParentItem(parentItem: Pointer): NSDiffableDataSourceSectionSnapshot; overload; cdecl;
    function visibleItems: NSArray; cdecl;
    function visualDescription: NSString; cdecl;
  end;
  TNSDiffableDataSourceSectionSnapshot = class(TOCGenericImport<NSDiffableDataSourceSectionSnapshotClass, NSDiffableDataSourceSectionSnapshot>) end;

  NSDiffableDataSourceSnapshotClass = interface(NSObjectClass)
    ['{497C8188-EE2D-4CF9-9A64-545AA5132FF8}']
  end;

  NSDiffableDataSourceSnapshot = interface(NSObject)
    ['{618720D7-84B4-42DD-886F-F83FA795A52F}']
    procedure appendItemsWithIdentifiers(identifiers: NSArray; intoSectionWithIdentifier: Pointer); overload; cdecl;
    procedure appendItemsWithIdentifiers(identifiers: NSArray); overload; cdecl;
    procedure appendSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    procedure deleteAllItems; cdecl;
    procedure deleteItemsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure deleteSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    function indexOfItemIdentifier(itemIdentifier: Pointer): NSInteger; cdecl;
    function indexOfSectionIdentifier(sectionIdentifier: Pointer): NSInteger; cdecl;
    procedure insertItemsWithIdentifiers(identifiers: NSArray; beforeItemWithIdentifier: Pointer); cdecl;
    [MethodName('insertItemsWithIdentifiers:afterItemWithIdentifier:')]
    procedure insertItemsWithIdentifiersAfterItemWithIdentifier(identifiers: NSArray; afterItemWithIdentifier: Pointer); cdecl;
    procedure insertSectionsWithIdentifiers(sectionIdentifiers: NSArray; beforeSectionWithIdentifier: Pointer); cdecl;
    [MethodName('insertSectionsWithIdentifiers:afterSectionWithIdentifier:')]
    procedure insertSectionsWithIdentifiersAfterSectionWithIdentifier(sectionIdentifiers: NSArray;
      afterSectionWithIdentifier: Pointer); cdecl;
    function itemIdentifiers: NSArray; cdecl;
    function itemIdentifiersInSectionWithIdentifier(sectionIdentifier: Pointer): NSArray; cdecl;
    procedure moveItemWithIdentifier(fromIdentifier: Pointer; beforeItemWithIdentifier: Pointer); cdecl;
    [MethodName('moveItemWithIdentifier:afterItemWithIdentifier:')]
    procedure moveItemWithIdentifierAfterItemWithIdentifier(fromIdentifier: Pointer; afterItemWithIdentifier: Pointer); cdecl;
    procedure moveSectionWithIdentifier(fromSectionIdentifier: Pointer; beforeSectionWithIdentifier: Pointer); cdecl;
    [MethodName('moveSectionWithIdentifier:afterSectionWithIdentifier:')]
    procedure moveSectionWithIdentifierAfterSectionWithIdentifier(fromSectionIdentifier: Pointer;
      afterSectionWithIdentifier: Pointer); cdecl;
    function numberOfItems: NSInteger; cdecl;
    function numberOfItemsInSection(sectionIdentifier: Pointer): NSInteger; cdecl;
    function numberOfSections: NSInteger; cdecl;
    function reconfiguredItemIdentifiers: NSArray; cdecl;
    procedure reconfigureItemsWithIdentifiers(identifiers: NSArray); cdecl;
    function reloadedItemIdentifiers: NSArray; cdecl;
    function reloadedSectionIdentifiers: NSArray; cdecl;
    procedure reloadItemsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure reloadSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    function sectionIdentifierForSectionContainingItemIdentifier(itemIdentifier: Pointer): Pointer; cdecl;
    function sectionIdentifiers: NSArray; cdecl;
  end;
  TNSDiffableDataSourceSnapshot = class(TOCGenericImport<NSDiffableDataSourceSnapshotClass, NSDiffableDataSourceSnapshot>) end;

  NSDiffableDataSourceSectionTransactionClass = interface(NSObjectClass)
    ['{0BC19FA1-89FE-4BAC-8697-7FD4F2200B03}']
  end;

  NSDiffableDataSourceSectionTransaction = interface(NSObject)
    ['{AE9736CB-3EDC-44F9-BCC4-00CBD8686EEC}']
    function difference: NSOrderedCollectionDifference; cdecl;
    function finalSnapshot: NSDiffableDataSourceSectionSnapshot; cdecl;
    function initialSnapshot: NSDiffableDataSourceSectionSnapshot; cdecl;
    function sectionIdentifier: Pointer; cdecl;
  end;
  TNSDiffableDataSourceSectionTransaction = class(TOCGenericImport<NSDiffableDataSourceSectionTransactionClass, NSDiffableDataSourceSectionTransaction>) end;

  NSDiffableDataSourceTransactionClass = interface(NSObjectClass)
    ['{D6A6BF2A-8921-480B-903D-D5D341A25D55}']
  end;

  NSDiffableDataSourceTransaction = interface(NSObject)
    ['{78D5AE26-A199-4DFF-A1D9-C1AB805B0FA4}']
    function difference: NSOrderedCollectionDifference; cdecl;
    function finalSnapshot: NSDiffableDataSourceSnapshot; cdecl;
    function initialSnapshot: NSDiffableDataSourceSnapshot; cdecl;
    function sectionTransactions: NSArray; cdecl;
  end;
  TNSDiffableDataSourceTransaction = class(TOCGenericImport<NSDiffableDataSourceTransactionClass, NSDiffableDataSourceTransaction>) end;

implementation

uses
  {$IF CompilerVersion < 37}
  DW.iOSapi.UIKit;
  {$ELSE}
  iOSapi.UIKit;
  {$ENDIF}

end.
