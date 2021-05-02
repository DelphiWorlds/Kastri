unit DW.iOSapi.PDFKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics;

const
  kPDFActionNamedNone = 0;
  kPDFActionNamedNextPage = 1;
  kPDFActionNamedPreviousPage = 2;
  kPDFActionNamedFirstPage = 3;
  kPDFActionNamedLastPage = 4;
  kPDFActionNamedGoBack = 5;
  kPDFActionNamedGoForward = 6;
  kPDFActionNamedGoToPage = 7;
  kPDFActionNamedFind = 8;
  kPDFActionNamedPrint = 9;
  kPDFActionNamedZoomIn = 10;
  kPDFActionNamedZoomOut = 11;
  kPDFDisplayBoxMediaBox = 0;
  kPDFDisplayBoxCropBox = 1;
  kPDFDisplayBoxBleedBox = 2;
  kPDFDisplayBoxTrimBox = 3;
  kPDFDisplayBoxArtBox = 4;
  kPDFLineStyleNone = 0;
  kPDFLineStyleSquare = 1;
  kPDFLineStyleCircle = 2;
  kPDFLineStyleDiamond = 3;
  kPDFLineStyleOpenArrow = 4;
  kPDFLineStyleClosedArrow = 5;
  kPDFTextAnnotationIconComment = 0;
  kPDFTextAnnotationIconKey = 1;
  kPDFTextAnnotationIconNote = 2;
  kPDFTextAnnotationIconHelp = 3;
  kPDFTextAnnotationIconNewParagraph = 4;
  kPDFTextAnnotationIconParagraph = 5;
  kPDFTextAnnotationIconInsert = 6;
  kPDFMarkupTypeHighlight = 0;
  kPDFMarkupTypeStrikeOut = 1;
  kPDFMarkupTypeUnderline = 2;
  kPDFMarkupTypeRedact = 3;
  kPDFWidgetUnknownControl = -1;
  kPDFWidgetPushButtonControl = 0;
  kPDFWidgetRadioButtonControl = 1;
  kPDFWidgetCheckBoxControl = 2;
  kPDFWidgetMixedState = -1;
  kPDFWidgetOffState = 0;
  kPDFWidgetOnState = 1;
  kPDFBorderStyleSolid = 0;
  kPDFBorderStyleDashed = 1;
  kPDFBorderStyleBeveled = 2;
  kPDFBorderStyleInset = 3;
  kPDFBorderStyleUnderline = 4;
  kPDFDocumentPermissionsNone = 0;
  kPDFDocumentPermissionsUser = 1;
  kPDFDocumentPermissionsOwner = 2;
  PDFThumbnailLayoutModeVertical = 0;
  PDFThumbnailLayoutModeHorizontal = 1;
  kPDFDisplaySinglePage = 0;
  kPDFDisplaySinglePageContinuous = 1;
  kPDFDisplayTwoUp = 2;
  kPDFDisplayTwoUpContinuous = 3;
  kPDFDisplayDirectionVertical = 0;
  kPDFDisplayDirectionHorizontal = 1;
  kPDFNoArea = 0;
  kPDFPageArea = 1;
  kPDFTextArea = 2;
  kPDFAnnotationArea = 4;
  kPDFLinkArea = 8;
  kPDFControlArea = 16;
  kPDFTextFieldArea = 32;
  kPDFIconArea = 64;
  kPDFPopupArea = 128;
  kPDFImageArea = 256;
  kPDFInterpolationQualityNone = 0;
  kPDFInterpolationQualityLow = 1;
  kPDFInterpolationQualityHigh = 2;

type
  PDFAction = interface;
  PDFActionGoTo = interface;
  PDFActionNamed = interface;
  PDFActionRemoteGoTo = interface;
  PDFActionResetForm = interface;
  PDFActionURL = interface;
  PDFPage = interface;
  PDFAnnotation = interface;
  PDFAppearanceCharacteristics = interface;
  PDFBorder = interface;
  PDFDestination = interface;
  PDFDocument = interface;
  PDFDocumentDelegate = interface;
  PDFOutline = interface;
  PDFSelection = interface;
  PDFThumbnailView = interface;
  PDFView = interface;
  PDFViewDelegate = interface;

  PDFActionNamedName = NSInteger;
  PDFDisplayBox = NSInteger;
  PDFAnnotationSubtype = NSString;
  PDFAnnotationKey = NSString;
  PDFLineStyle = NSInteger;
  PDFTextAnnotationIconType = NSInteger;
  PDFMarkupType = NSInteger;
  PDFWidgetControlType = NSInteger;
  PDFWidgetCellState = NSInteger;
  PDFAnnotationWidgetSubtype = NSString;
  PDFAnnotationLineEndingStyle = NSString;
  PDFAnnotationTextIconType = NSString;
  PDFAnnotationHighlightingMode = NSString;
  PDFAppearanceCharacteristicsKey = NSString;
  PDFBorderStyle = NSInteger;
  PDFBorderKey = NSString;
  PDFDocumentPermissions = NSInteger;
  PDFDocumentAttribute = NSString;
  PDFDocumentWriteOption = NSString;
  PDFThumbnailLayoutMode = NSInteger;
  PDFDisplayMode = NSInteger;
  PDFDisplayDirection = NSInteger;
  PDFAreaOfInterest = NSInteger;
  PDFInterpolationQuality = NSInteger;

  PDFActionClass = interface(NSObjectClass)
    ['{C153FFCF-8745-4DBB-BD84-31934FFDF7A6}']
  end;

  PDFAction = interface(NSObject)
    ['{4020A55C-A3DB-44A8-AA2E-DD1BBF4F5164}']
    function &type: NSString; cdecl;
  end;
  TPDFAction = class(TOCGenericImport<PDFActionClass, PDFAction>) end;

  PDFActionGoToClass = interface(PDFActionClass)
    ['{437F4AC5-10A2-4385-AB78-3912E073FA54}']
  end;

  PDFActionGoTo = interface(PDFAction)
    ['{7A5F5883-70E3-466F-8E70-5D4D453AE369}']
    function destination: PDFDestination; cdecl;
    function initWithDestination(destination: PDFDestination): Pointer; cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
  end;
  TPDFActionGoTo = class(TOCGenericImport<PDFActionGoToClass, PDFActionGoTo>) end;

  PDFActionNamedClass = interface(PDFActionClass)
    ['{D8F686D8-6C0A-41DB-8A28-2263655C351E}']
  end;

  PDFActionNamed = interface(PDFAction)
    ['{7BD41F1A-5955-4A4C-93B5-97E9C1024A71}']
    function initWithName(name: PDFActionNamedName): Pointer; cdecl;
    function name: PDFActionNamedName; cdecl;
    procedure setName(name: PDFActionNamedName); cdecl;
  end;
  TPDFActionNamed = class(TOCGenericImport<PDFActionNamedClass, PDFActionNamed>) end;

  PDFActionRemoteGoToClass = interface(PDFActionClass)
    ['{71CD5F05-169A-4FFE-A377-97E82773B03B}']
  end;

  PDFActionRemoteGoTo = interface(PDFAction)
    ['{9F3C01EB-AB08-4C90-A85D-20186DB94F9C}']
    function initWithPageIndex(pageIndex: NSUInteger; atPoint: CGPoint; fileURL: NSURL): Pointer; cdecl;
    function pageIndex: NSUInteger; cdecl;
    function point: CGPoint; cdecl;
    procedure setPageIndex(pageIndex: NSUInteger); cdecl;
    procedure setPoint(point: CGPoint); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionRemoteGoTo = class(TOCGenericImport<PDFActionRemoteGoToClass, PDFActionRemoteGoTo>) end;

  PDFActionResetFormClass = interface(PDFActionClass)
    ['{69E13E78-F873-49A0-A5CE-52069CDAAE01}']
  end;

  PDFActionResetForm = interface(PDFAction)
    ['{3DE15BDC-57EC-4CDE-8ABE-0680CAD3AF06}']
    function fields: NSArray; cdecl;
    function fieldsIncludedAreCleared: Boolean; cdecl;
    procedure setFields(fields: NSArray); cdecl;
    procedure setFieldsIncludedAreCleared(fieldsIncludedAreCleared: Boolean); cdecl;
  end;
  TPDFActionResetForm = class(TOCGenericImport<PDFActionResetFormClass, PDFActionResetForm>) end;

  PDFActionURLClass = interface(PDFActionClass)
    ['{0DD2A0AF-7CC2-4569-B287-482A9F00A276}']
  end;

  PDFActionURL = interface(PDFAction)
    ['{62AC67D1-8E15-4C73-90B5-4EA7435E85E1}']
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionURL = class(TOCGenericImport<PDFActionURLClass, PDFActionURL>) end;

  PDFPageClass = interface(NSObjectClass)
    ['{EE7118A3-4A33-41E8-87B4-4733ECBDC8ED}']
  end;

  PDFPage = interface(NSObject)
    ['{42F4D5C2-D67A-4BCA-B092-A18C6C5B6B11}']
    procedure addAnnotation(annotation: PDFAnnotation); cdecl;
    function annotationAtPoint(point: CGPoint): PDFAnnotation; cdecl;
    function annotations: NSArray; cdecl;
    function attributedString: NSAttributedString; cdecl;
    function boundsForBox(box: PDFDisplayBox): CGRect; cdecl;
    function characterBoundsAtIndex(index: NSInteger): CGRect; cdecl;
    function characterIndexAtPoint(point: CGPoint): NSInteger; cdecl;
    function dataRepresentation: NSData; cdecl;
    function displaysAnnotations: Boolean; cdecl;
    function document: PDFDocument; cdecl;
    procedure drawWithBox(box: PDFDisplayBox; toContext: CGContextRef); cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
    function &label: NSString; cdecl;
    function numberOfCharacters: NSUInteger; cdecl;
    function pageRef: CGPDFPageRef; cdecl;
    procedure removeAnnotation(annotation: PDFAnnotation); cdecl;
    function rotation: NSInteger; cdecl;
    function selectionForLineAtPoint(point: CGPoint): PDFSelection; cdecl;
    function selectionForRange(range: NSRange): PDFSelection; cdecl;
    function selectionForRect(rect: CGRect): PDFSelection; cdecl;
    function selectionForWordAtPoint(point: CGPoint): PDFSelection; cdecl;
    function selectionFromPoint(startPoint: CGPoint; toPoint: CGPoint): PDFSelection; cdecl;
    procedure setBounds(bounds: CGRect; forBox: PDFDisplayBox); cdecl;
    procedure setDisplaysAnnotations(displaysAnnotations: Boolean); cdecl;
    procedure setRotation(rotation: NSInteger); cdecl;
    function &string: NSString; cdecl;
    function thumbnailOfSize(size: CGSize; forBox: PDFDisplayBox): UIImage; cdecl;
    procedure transformContext(context: CGContextRef; forBox: PDFDisplayBox); cdecl;
    function transformForBox(box: PDFDisplayBox): CGAffineTransform; cdecl;
  end;
  TPDFPage = class(TOCGenericImport<PDFPageClass, PDFPage>) end;

  PDFAnnotationClass = interface(NSObjectClass)
    ['{91D65EDE-2AC1-4012-8F4D-06178A8F76B9}']
    {class} function lineStyleFromName(name: NSString): PDFLineStyle; cdecl;
    {class} function nameForLineStyle(style: PDFLineStyle): NSString; cdecl;
  end;

  PDFAnnotation = interface(NSObject)
    ['{BD1D5DF2-FF93-4275-99A5-D6B643F07369}']
    function action: PDFAction; cdecl;
    procedure addBezierPath(path: UIBezierPath); cdecl;
    function alignment: NSTextAlignment; cdecl;
    function allowsToggleToOff: Boolean; cdecl;
    function annotationKeyValues: NSDictionary; cdecl;
    function backgroundColor: UIColor; cdecl;
    function border: PDFBorder; cdecl;
    function bounds: CGRect; cdecl;
    function buttonWidgetState: PDFWidgetCellState; cdecl;
    function buttonWidgetStateString: NSString; cdecl;
    function caption: NSString; cdecl;
    function choices: NSArray; cdecl;
    function color: UIColor; cdecl;
    function contents: NSString; cdecl;
    function destination: PDFDestination; cdecl;
    procedure drawWithBox(box: PDFDisplayBox; inContext: CGContextRef); cdecl;
    function endLineStyle: PDFLineStyle; cdecl;
    function endPoint: CGPoint; cdecl;
    function fieldName: NSString; cdecl;
    function font: UIFont; cdecl;
    function fontColor: UIColor; cdecl;
    function hasAppearanceStream: Boolean; cdecl;
    function hasComb: Boolean; cdecl;
    function iconType: PDFTextAnnotationIconType; cdecl;
    function initWithBounds(bounds: CGRect; forType: PDFAnnotationSubtype; withProperties: NSDictionary): Pointer; cdecl;
    function interiorColor: UIColor; cdecl;
    function isHighlighted: Boolean; cdecl;
    function isListChoice: Boolean; cdecl;
    function isMultiline: Boolean; cdecl;
    function isOpen: Boolean; cdecl;
    function isPasswordField: Boolean; cdecl;
    function isReadOnly: Boolean; cdecl;
    function markupType: PDFMarkupType; cdecl;
    function maximumLength: NSInteger; cdecl;
    function modificationDate: NSDate; cdecl;
    function page: PDFPage; cdecl;
    function paths: NSArray; cdecl;
    function popup: PDFAnnotation; cdecl;
    function quadrilateralPoints: NSArray; cdecl;
    function radiosInUnison: Boolean; cdecl;
    procedure removeBezierPath(path: UIBezierPath); cdecl;
    procedure removeValueForAnnotationKey(key: PDFAnnotationKey); cdecl;
    procedure setAction(action: PDFAction); cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    procedure setAllowsToggleToOff(allowsToggleToOff: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    function setBoolean(value: Boolean; forAnnotationKey: PDFAnnotationKey): Boolean; cdecl;
    procedure setBorder(border: PDFBorder); cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setButtonWidgetState(buttonWidgetState: PDFWidgetCellState); cdecl;
    procedure setButtonWidgetStateString(buttonWidgetStateString: NSString); cdecl;
    procedure setCaption(caption: NSString); cdecl;
    procedure setChoices(choices: NSArray); cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setComb(comb: Boolean); cdecl;
    procedure setContents(contents: NSString); cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
    procedure setEndLineStyle(endLineStyle: PDFLineStyle); cdecl;
    procedure setEndPoint(endPoint: CGPoint); cdecl;
    procedure setFieldName(fieldName: NSString); cdecl;
    procedure setFont(font: UIFont); cdecl;
    procedure setFontColor(fontColor: UIColor); cdecl;
    procedure setHighlighted(highlighted: Boolean); cdecl;
    procedure setIconType(iconType: PDFTextAnnotationIconType); cdecl;
    procedure setInteriorColor(interiorColor: UIColor); cdecl;
    procedure setListChoice(listChoice: Boolean); cdecl;
    procedure setMarkupType(markupType: PDFMarkupType); cdecl;
    procedure setMaximumLength(maximumLength: NSInteger); cdecl;
    procedure setModificationDate(modificationDate: NSDate); cdecl;
    procedure setMultiline(multiline: Boolean); cdecl;
    procedure setOpen(open: Boolean); cdecl;
    procedure setPage(page: PDFPage); cdecl;
    procedure setPopup(popup: PDFAnnotation); cdecl;
    procedure setQuadrilateralPoints(quadrilateralPoints: NSArray); cdecl;
    procedure setRadiosInUnison(radiosInUnison: Boolean); cdecl;
    procedure setReadOnly(readOnly: Boolean); cdecl;
    function setRect(value: CGRect; forAnnotationKey: PDFAnnotationKey): Boolean; cdecl;
    procedure setShouldDisplay(shouldDisplay: Boolean); cdecl;
    procedure setShouldPrint(shouldPrint: Boolean); cdecl;
    procedure setStampName(stampName: NSString); cdecl;
    procedure setStartLineStyle(startLineStyle: PDFLineStyle); cdecl;
    procedure setStartPoint(startPoint: CGPoint); cdecl;
    procedure setType(&type: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    procedure setUserName(userName: NSString); cdecl;
    function setValue(value: Pointer; forAnnotationKey: PDFAnnotationKey): Boolean; cdecl;
    procedure setValues(values: NSArray); cdecl;
    procedure setWidgetControlType(widgetControlType: PDFWidgetControlType); cdecl;
    procedure setWidgetDefaultStringValue(widgetDefaultStringValue: NSString); cdecl;
    procedure setWidgetFieldType(widgetFieldType: NSString); cdecl;
    procedure setWidgetStringValue(widgetStringValue: NSString); cdecl;
    function shouldDisplay: Boolean; cdecl;
    function shouldPrint: Boolean; cdecl;
    function stampName: NSString; cdecl;
    function startLineStyle: PDFLineStyle; cdecl;
    function startPoint: CGPoint; cdecl;
    function &type: NSString; cdecl;
    function URL: NSURL; cdecl;
    function userName: NSString; cdecl;
    function valueForAnnotationKey(key: PDFAnnotationKey): Pointer; cdecl;
    function values: NSArray; cdecl;
    function widgetControlType: PDFWidgetControlType; cdecl;
    function widgetDefaultStringValue: NSString; cdecl;
    function widgetFieldType: NSString; cdecl;
    function widgetStringValue: NSString; cdecl;
  end;
  TPDFAnnotation = class(TOCGenericImport<PDFAnnotationClass, PDFAnnotation>) end;

  PDFAppearanceCharacteristicsClass = interface(NSObjectClass)
    ['{AA78A095-9E47-488E-9A6C-ED1489365523}']
  end;

  PDFAppearanceCharacteristics = interface(NSObject)
    ['{39020923-0491-4210-B094-15AECC217CDB}']
    function appearanceCharacteristicsKeyValues: NSDictionary; cdecl;
    function backgroundColor: UIColor; cdecl;
    function borderColor: UIColor; cdecl;
    function caption: NSString; cdecl;
    function controlType: PDFWidgetControlType; cdecl;
    function downCaption: NSString; cdecl;
    function rolloverCaption: NSString; cdecl;
    function rotation: NSInteger; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setBorderColor(borderColor: UIColor); cdecl;
    procedure setCaption(caption: NSString); cdecl;
    procedure setControlType(controlType: PDFWidgetControlType); cdecl;
    procedure setDownCaption(downCaption: NSString); cdecl;
    procedure setRolloverCaption(rolloverCaption: NSString); cdecl;
    procedure setRotation(rotation: NSInteger); cdecl;
  end;
  TPDFAppearanceCharacteristics = class(TOCGenericImport<PDFAppearanceCharacteristicsClass, PDFAppearanceCharacteristics>) end;

  PDFBorderClass = interface(NSObjectClass)
    ['{AB18BFB4-D847-4336-A3EF-3B4400265166}']
  end;

  PDFBorder = interface(NSObject)
    ['{934A1F52-8E51-4086-B7A9-2D47721FD4B3}']
    function borderKeyValues: NSDictionary; cdecl;
    function dashPattern: NSArray; cdecl;
    procedure drawInRect(rect: CGRect); cdecl;
    function lineWidth: CGFloat; cdecl;
    procedure setDashPattern(dashPattern: NSArray); cdecl;
    procedure setLineWidth(lineWidth: CGFloat); cdecl;
    procedure setStyle(style: PDFBorderStyle); cdecl;
    function style: PDFBorderStyle; cdecl;
  end;
  TPDFBorder = class(TOCGenericImport<PDFBorderClass, PDFBorder>) end;

  PDFDestinationClass = interface(NSObjectClass)
    ['{1FDD9FB0-533C-4302-AFD2-A9747F38B158}']
  end;

  PDFDestination = interface(NSObject)
    ['{ACE9B194-4B81-4C8E-B209-D451F1206537}']
    function compare(destination: PDFDestination): NSComparisonResult; cdecl;
    function initWithPage(page: PDFPage; atPoint: CGPoint): Pointer; cdecl;
    function page: PDFPage; cdecl;
    function point: CGPoint; cdecl;
    procedure setZoom(zoom: CGFloat); cdecl;
    function zoom: CGFloat; cdecl;
  end;
  TPDFDestination = class(TOCGenericImport<PDFDestinationClass, PDFDestination>) end;

  PDFDocumentClass = interface(NSObjectClass)
    ['{C0A5018A-17B9-43F7-BD3C-29905517E889}']
  end;

  PDFDocument = interface(NSObject)
    ['{D04948B1-5B96-41F4-B6E8-9DCEF8CBEF75}']
    function allowsCommenting: Boolean; cdecl;
    function allowsContentAccessibility: Boolean; cdecl;
    function allowsCopying: Boolean; cdecl;
    function allowsDocumentAssembly: Boolean; cdecl;
    function allowsDocumentChanges: Boolean; cdecl;
    function allowsFormFieldEntry: Boolean; cdecl;
    function allowsPrinting: Boolean; cdecl;
    procedure beginFindString(&string: NSString; withOptions: NSStringCompareOptions); cdecl;
    procedure beginFindStrings(strings: NSArray; withOptions: NSStringCompareOptions); cdecl;
    procedure cancelFindString; cdecl;
    function dataRepresentation: NSData; cdecl;
    function dataRepresentationWithOptions(options: NSDictionary): NSData; cdecl;
    function delegate: Pointer; cdecl;
    function documentAttributes: NSDictionary; cdecl;
    function documentRef: CGPDFDocumentRef; cdecl;
    function documentURL: NSURL; cdecl;
    procedure exchangePageAtIndex(indexA: NSUInteger; withPageAtIndex: NSUInteger); cdecl;
    function findString(&string: NSString; withOptions: NSStringCompareOptions): NSArray; overload; cdecl;
    function findString(&string: NSString; fromSelection: PDFSelection; withOptions: NSStringCompareOptions): PDFSelection; overload; cdecl;
    function indexForPage(page: PDFPage): NSUInteger; cdecl;
    function initWithData(data: NSData): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure insertPage(page: PDFPage; atIndex: NSUInteger); cdecl;
    function isEncrypted: Boolean; cdecl;
    function isFinding: Boolean; cdecl;
    function isLocked: Boolean; cdecl;
    function majorVersion: NSInteger; cdecl;
    function minorVersion: NSInteger; cdecl;
    function outlineItemForSelection(selection: PDFSelection): PDFOutline; cdecl;
    function outlineRoot: PDFOutline; cdecl;
    function pageAtIndex(index: NSUInteger): PDFPage; cdecl;
    function pageClass: Pointer; cdecl;
    function pageCount: NSUInteger; cdecl;
    function permissionsStatus: PDFDocumentPermissions; cdecl;
    procedure removePageAtIndex(index: NSUInteger); cdecl;
    function selectionForEntireDocument: PDFSelection; cdecl;
    [MethodName('selectionFromPage:startPage:atPoint:toPage:atPoint')]
    function selectionFromPage(startPage: PDFPage; startPoint: CGPoint; toPage: PDFPage; endPoint: CGPoint): PDFSelection; overload; cdecl;
    [MethodName('selectionFromPage:startPage:atCharacterIndex:toPage:atCharacterIndex')]
    function selectionFromPage(startPage: PDFPage; startCharacterIndex: NSUInteger; toPage: PDFPage;
      endCharacterIndex: NSUInteger): PDFSelection; overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDocumentAttributes(documentAttributes: NSDictionary); cdecl;
    procedure setOutlineRoot(outlineRoot: PDFOutline); cdecl;
    function &string: NSString; cdecl;
    function unlockWithPassword(password: NSString): Boolean; cdecl;
    function writeToFile(path: NSString; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToFile(path: NSString): Boolean; overload; cdecl;
    function writeToURL(url: NSURL; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToURL(url: NSURL): Boolean; overload; cdecl;
  end;
  TPDFDocument = class(TOCGenericImport<PDFDocumentClass, PDFDocument>) end;

  PDFDocumentDelegate = interface(IObjectiveC)
    ['{27AAAC1F-503A-416C-875D-46EFB0C2A904}']
    function classForAnnotationType(annotationType: NSString): Pointer; cdecl;
    function classForPage: Pointer; cdecl;
    procedure didMatchString(instance: PDFSelection); cdecl;
    procedure documentDidBeginDocumentFind(notification: NSNotification); cdecl;
    procedure documentDidBeginPageFind(notification: NSNotification); cdecl;
    procedure documentDidEndDocumentFind(notification: NSNotification); cdecl;
    procedure documentDidEndPageFind(notification: NSNotification); cdecl;
    procedure documentDidFindMatch(notification: NSNotification); cdecl;
    procedure documentDidUnlock(notification: NSNotification); cdecl;
  end;

  PDFOutlineClass = interface(NSObjectClass)
    ['{17167333-DEEB-445D-A0F9-0A1409E2C036}']
  end;

  PDFOutline = interface(NSObject)
    ['{D6DDE178-BE77-4358-BBF9-A479D2CF5D57}']
    function action: PDFAction; cdecl;
    function childAtIndex(index: NSUInteger): PDFOutline; cdecl;
    function destination: PDFDestination; cdecl;
    function document: PDFDocument; cdecl;
    function index: NSUInteger; cdecl;
    procedure insertChild(child: PDFOutline; atIndex: NSUInteger); cdecl;
    function isOpen: Boolean; cdecl;
    function &label: NSString; cdecl;
    function numberOfChildren: NSUInteger; cdecl;
    function parent: PDFOutline; cdecl;
    procedure removeFromParent; cdecl;
    procedure setAction(action: PDFAction); cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
    procedure setIsOpen(isOpen: Boolean); cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;
  TPDFOutline = class(TOCGenericImport<PDFOutlineClass, PDFOutline>) end;

  PDFSelectionClass = interface(NSObjectClass)
    ['{9C044CAE-552B-43F0-8F9D-7AB29F28FC85}']
  end;

  PDFSelection = interface(NSObject)
    ['{C9E8383B-4818-4EA5-9A19-37572191DBC9}']
    procedure addSelection(selection: PDFSelection); cdecl;
    procedure addSelections(selections: NSArray); cdecl;
    function attributedString: NSAttributedString; cdecl;
    function boundsForPage(page: PDFPage): CGRect; cdecl;
    function color: UIColor; cdecl;
    procedure drawForPage(page: PDFPage; active: Boolean); overload; cdecl;
    procedure drawForPage(page: PDFPage; withBox: PDFDisplayBox; active: Boolean); overload; cdecl;
    procedure extendSelectionAtEnd(succeed: NSInteger); cdecl;
    procedure extendSelectionAtStart(precede: NSInteger); cdecl;
    procedure extendSelectionForLineBoundaries; cdecl;
    function initWithDocument(document: PDFDocument): Pointer; cdecl;
    function numberOfTextRangesOnPage(page: PDFPage): NSUInteger; cdecl;
    function pages: NSArray; cdecl;
    function rangeAtIndex(index: NSUInteger; onPage: PDFPage): NSRange; cdecl;
    function selectionsByLine: NSArray; cdecl;
    procedure setColor(color: UIColor); cdecl;
    function &string: NSString; cdecl;
  end;
  TPDFSelection = class(TOCGenericImport<PDFSelectionClass, PDFSelection>) end;

  PDFThumbnailViewClass = interface(UIViewClass)
    ['{7211424C-1662-4E4E-9CF5-2C9C555D373A}']
  end;

  PDFThumbnailView = interface(UIView)
    ['{DFE52798-27FE-4461-8FB2-C68CD6327F84}']
    function backgroundColor: UIColor; cdecl;
    function contentInset: UIEdgeInsets; cdecl;
    function layoutMode: PDFThumbnailLayoutMode; cdecl;
    function PDFView: PDFView; cdecl;
    function selectedPages: NSArray; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setContentInset(contentInset: UIEdgeInsets); cdecl;
    procedure setLayoutMode(layoutMode: PDFThumbnailLayoutMode); cdecl;
    procedure setPDFView(PDFView: PDFView); cdecl;
    procedure setThumbnailSize(thumbnailSize: CGSize); cdecl;
    function thumbnailSize: CGSize; cdecl;
  end;
  TPDFThumbnailView = class(TOCGenericImport<PDFThumbnailViewClass, PDFThumbnailView>) end;

  PDFViewClass = interface(UIViewClass)
    ['{F688EA1D-A175-4784-9C9A-75F474E461B4}']
  end;

  PDFView = interface(UIView)
    ['{FDB47171-995D-49FD-B0B2-C11883CD0E97}']
    procedure annotationsChangedOnPage(page: PDFPage); cdecl;
    function areaOfInterestForMouse(event: UIEvent): PDFAreaOfInterest; cdecl;
    function areaOfInterestForPoint(cursorLocation: CGPoint): PDFAreaOfInterest; cdecl;
    function autoScales: Boolean; cdecl;
    function backgroundColor: UIColor; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function canGoToFirstPage: Boolean; cdecl;
    function canGoToLastPage: Boolean; cdecl;
    function canGoToNextPage: Boolean; cdecl;
    function canGoToPreviousPage: Boolean; cdecl;
    function canZoomIn: Boolean; cdecl;
    function canZoomOut: Boolean; cdecl;
    procedure clearSelection; cdecl;
    [MethodName('convertPoint:fromPage:')]
    function convertPointFromPage(point: CGPoint; fromPage: PDFPage): CGPoint; cdecl;
    [MethodName('convertPoint:toPage:')]
    function convertPointToPage(point: CGPoint; toPage: PDFPage): CGPoint; cdecl;
    [MethodName('convertRect:fromPage:')]
    function convertRectFromPage(rect: CGRect; fromPage: PDFPage): CGRect; cdecl;
    [MethodName('convertRect:toPage:')]
    function convertRectToPage(rect: CGRect; toPage: PDFPage): CGRect; cdecl;
    procedure copy(sender: Pointer); cdecl;
    function currentDestination: PDFDestination; cdecl;
    function currentPage: PDFPage; cdecl;
    function currentSelection: PDFSelection; cdecl;
    function delegate: Pointer; cdecl;
    function displayBox: PDFDisplayBox; cdecl;
    function displayDirection: PDFDisplayDirection; cdecl;
    function displayMode: PDFDisplayMode; cdecl;
    function displaysAsBook: Boolean; cdecl;
    function displaysPageBreaks: Boolean; cdecl;
    function displaysRTL: Boolean; cdecl;
    function document: PDFDocument; cdecl;
    function documentView: UIView; cdecl;
    procedure drawPage(page: PDFPage; toContext: CGContextRef); cdecl;
    procedure drawPagePost(page: PDFPage; toContext: CGContextRef); cdecl;
    function enableDataDetectors: Boolean; cdecl;
    procedure enablePageShadows(pageShadowsEnabled: Boolean); cdecl;
    procedure goBack(sender: Pointer); cdecl;
    procedure goForward(sender: Pointer); cdecl;
    procedure goToDestination(destination: PDFDestination); cdecl;
    procedure goToFirstPage(sender: Pointer); cdecl;
    procedure goToLastPage(sender: Pointer); cdecl;
    procedure goToNextPage(sender: Pointer); cdecl;
    procedure goToPage(page: PDFPage); cdecl;
    procedure goToPreviousPage(sender: Pointer); cdecl;
    procedure goToRect(rect: CGRect; onPage: PDFPage); cdecl;
    procedure goToSelection(selection: PDFSelection); cdecl;
    function highlightedSelections: NSArray; cdecl;
    function interpolationQuality: PDFInterpolationQuality; cdecl;
    function isUsingPageViewController: Boolean; cdecl;
    procedure layoutDocumentView; cdecl;
    function maxScaleFactor: CGFloat; cdecl;
    function minScaleFactor: CGFloat; cdecl;
    function pageBreakMargins: UIEdgeInsets; cdecl;
    function pageForPoint(point: CGPoint; nearest: Boolean): PDFPage; cdecl;
    function pageShadowsEnabled: Boolean; cdecl;
    procedure performAction(action: PDFAction); cdecl;
    function rowSizeForPage(page: PDFPage): CGSize; cdecl;
    function scaleFactor: CGFloat; cdecl;
    function scaleFactorForSizeToFit: CGFloat; cdecl;
    procedure scrollSelectionToVisible(sender: Pointer); cdecl;
    procedure selectAll(sender: Pointer); cdecl;
    procedure setAutoScales(autoScales: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setCurrentSelection(currentSelection: PDFSelection); overload; cdecl;
    procedure setCurrentSelection(selection: PDFSelection; animate: Boolean); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDisplayBox(displayBox: PDFDisplayBox); cdecl;
    procedure setDisplayDirection(displayDirection: PDFDisplayDirection); cdecl;
    procedure setDisplayMode(displayMode: PDFDisplayMode); cdecl;
    procedure setDisplaysAsBook(displaysAsBook: Boolean); cdecl;
    procedure setDisplaysPageBreaks(displaysPageBreaks: Boolean); cdecl;
    procedure setDisplaysRTL(displaysRTL: Boolean); cdecl;
    procedure setDocument(document: PDFDocument); cdecl;
    procedure setEnableDataDetectors(enableDataDetectors: Boolean); cdecl;
    procedure setHighlightedSelections(highlightedSelections: NSArray); cdecl;
    procedure setInterpolationQuality(interpolationQuality: PDFInterpolationQuality); cdecl;
    procedure setMaxScaleFactor(maxScaleFactor: CGFloat); cdecl;
    procedure setMinScaleFactor(minScaleFactor: CGFloat); cdecl;
    procedure setPageBreakMargins(pageBreakMargins: UIEdgeInsets); cdecl;
    procedure setScaleFactor(scaleFactor: CGFloat); cdecl;
    procedure usePageViewController(enable: Boolean; withViewOptions: NSDictionary); cdecl;
    function visiblePages: NSArray; cdecl;
    procedure zoomIn(sender: Pointer); cdecl;
    procedure zoomOut(sender: Pointer); cdecl;
  end;
  TPDFView = class(TOCGenericImport<PDFViewClass, PDFView>) end;

  PDFViewDelegate = interface(IObjectiveC)
    ['{CE8C625A-C43C-4BF4-B45B-383D05044684}']
    procedure PDFViewOpenPDF(sender: PDFView; forRemoteGoToAction: PDFActionRemoteGoTo); cdecl;
    function PDFViewParentViewController: UIViewController; cdecl;
    procedure PDFViewPerformFind(sender: PDFView); cdecl;
    procedure PDFViewPerformGoToPage(sender: PDFView); cdecl;
    procedure PDFViewWillClickOnLink(sender: PDFView; withURL: NSURL); cdecl;
  end;

function PDFAnnotationKeyAppearanceDictionary: PDFAnnotationKey;
function PDFAnnotationKeyAppearanceState: PDFAnnotationKey;
function PDFAnnotationKeyBorder: PDFAnnotationKey;
function PDFAnnotationKeyColor: PDFAnnotationKey;
function PDFAnnotationKeyContents: PDFAnnotationKey;
function PDFAnnotationKeyFlags: PDFAnnotationKey;
function PDFAnnotationKeyDate: PDFAnnotationKey;
function PDFAnnotationKeyName: PDFAnnotationKey;
function PDFAnnotationKeyPage: PDFAnnotationKey;
function PDFAnnotationKeyRect: PDFAnnotationKey;
function PDFAnnotationKeySubtype: PDFAnnotationKey;
function PDFAnnotationKeyAction: PDFAnnotationKey;
function PDFAnnotationKeyAdditionalActions: PDFAnnotationKey;
function PDFAnnotationKeyBorderStyle: PDFAnnotationKey;
function PDFAnnotationKeyDefaultAppearance: PDFAnnotationKey;
function PDFAnnotationKeyDestination: PDFAnnotationKey;
function PDFAnnotationKeyHighlightingMode: PDFAnnotationKey;
function PDFAnnotationKeyInklist: PDFAnnotationKey;
function PDFAnnotationKeyInteriorColor: PDFAnnotationKey;
function PDFAnnotationKeyLinePoints: PDFAnnotationKey;
function PDFAnnotationKeyLineEndingStyles: PDFAnnotationKey;
function PDFAnnotationKeyIconName: PDFAnnotationKey;
function PDFAnnotationKeyOpen: PDFAnnotationKey;
function PDFAnnotationKeyParent: PDFAnnotationKey;
function PDFAnnotationKeyPopup: PDFAnnotationKey;
function PDFAnnotationKeyQuadding: PDFAnnotationKey;
function PDFAnnotationKeyQuadPoints: PDFAnnotationKey;
function PDFAnnotationKeyTextLabel: PDFAnnotationKey;
function PDFAnnotationKeyWidgetDownCaption: PDFAnnotationKey;
function PDFAnnotationKeyWidgetBorderColor: PDFAnnotationKey;
function PDFAnnotationKeyWidgetBackgroundColor: PDFAnnotationKey;
function PDFAnnotationKeyWidgetCaption: PDFAnnotationKey;
function PDFAnnotationKeyWidgetDefaultValue: PDFAnnotationKey;
function PDFAnnotationKeyWidgetFieldFlags: PDFAnnotationKey;
function PDFAnnotationKeyWidgetFieldType: PDFAnnotationKey;
function PDFAnnotationKeyWidgetAppearanceDictionary: PDFAnnotationKey;
function PDFAnnotationKeyWidgetMaxLen: PDFAnnotationKey;
function PDFAnnotationKeyWidgetOptions: PDFAnnotationKey;
function PDFAnnotationKeyWidgetRotation: PDFAnnotationKey;
function PDFAnnotationKeyWidgetRolloverCaption: PDFAnnotationKey;
function PDFAnnotationKeyWidgetTextLabelUI: PDFAnnotationKey;
function PDFAnnotationKeyWidgetValue: PDFAnnotationKey;
function PDFAnnotationSubtypeText: PDFAnnotationSubtype;
function PDFAnnotationSubtypeLink: PDFAnnotationSubtype;
function PDFAnnotationSubtypeFreeText: PDFAnnotationSubtype;
function PDFAnnotationSubtypeLine: PDFAnnotationSubtype;
function PDFAnnotationSubtypeSquare: PDFAnnotationSubtype;
function PDFAnnotationSubtypeCircle: PDFAnnotationSubtype;
function PDFAnnotationSubtypeHighlight: PDFAnnotationSubtype;
function PDFAnnotationSubtypeUnderline: PDFAnnotationSubtype;
function PDFAnnotationSubtypeStrikeOut: PDFAnnotationSubtype;
function PDFAnnotationSubtypeInk: PDFAnnotationSubtype;
function PDFAnnotationSubtypeStamp: PDFAnnotationSubtype;
function PDFAnnotationSubtypePopup: PDFAnnotationSubtype;
function PDFAnnotationSubtypeWidget: PDFAnnotationSubtype;
function PDFAnnotationWidgetSubtypeButton: PDFAnnotationWidgetSubtype;
function PDFAnnotationWidgetSubtypeChoice: PDFAnnotationWidgetSubtype;
function PDFAnnotationWidgetSubtypeSignature: PDFAnnotationWidgetSubtype;
function PDFAnnotationWidgetSubtypeText: PDFAnnotationWidgetSubtype;
function PDFAnnotationLineEndingStyleNone: PDFAnnotationLineEndingStyle;
function PDFAnnotationLineEndingStyleSquare: PDFAnnotationLineEndingStyle;
function PDFAnnotationLineEndingStyleCircle: PDFAnnotationLineEndingStyle;
function PDFAnnotationLineEndingStyleDiamond: PDFAnnotationLineEndingStyle;
function PDFAnnotationLineEndingStyleOpenArrow: PDFAnnotationLineEndingStyle;
function PDFAnnotationLineEndingStyleClosedArrow: PDFAnnotationLineEndingStyle;
function PDFAnnotationTextIconTypeComment: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeKey: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeNote: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeHelp: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeNewParagraph: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeParagraph: PDFAnnotationTextIconType;
function PDFAnnotationTextIconTypeInsert: PDFAnnotationTextIconType;
function PDFAnnotationHighlightingModeNone: PDFAnnotationHighlightingMode;
function PDFAnnotationHighlightingModeInvert: PDFAnnotationHighlightingMode;
function PDFAnnotationHighlightingModeOutline: PDFAnnotationHighlightingMode;
function PDFAnnotationHighlightingModePush: PDFAnnotationHighlightingMode;
function PDFAppearanceCharacteristicsKeyBackgroundColor: PDFAppearanceCharacteristicsKey;
function PDFAppearanceCharacteristicsKeyBorderColor: PDFAppearanceCharacteristicsKey;
function PDFAppearanceCharacteristicsKeyRotation: PDFAppearanceCharacteristicsKey;
function PDFAppearanceCharacteristicsKeyCaption: PDFAppearanceCharacteristicsKey;
function PDFAppearanceCharacteristicsKeyRolloverCaption: PDFAppearanceCharacteristicsKey;
function PDFAppearanceCharacteristicsKeyDownCaption: PDFAppearanceCharacteristicsKey;
function PDFBorderKeyLineWidth: PDFBorderKey;
function PDFBorderKeyStyle: PDFBorderKey;
function PDFBorderKeyDashPattern: PDFBorderKey;
function PDFDocumentDidUnlockNotification: NSNotificationName;
function PDFDocumentDidBeginFindNotification: NSNotificationName;
function PDFDocumentDidEndFindNotification: NSNotificationName;
function PDFDocumentDidBeginPageFindNotification: NSNotificationName;
function PDFDocumentDidEndPageFindNotification: NSNotificationName;
function PDFDocumentDidFindMatchNotification: NSNotificationName;
function PDFDocumentDidBeginWriteNotification: NSNotificationName;
function PDFDocumentDidEndWriteNotification: NSNotificationName;
function PDFDocumentDidBeginPageWriteNotification: NSNotificationName;
function PDFDocumentDidEndPageWriteNotification: NSNotificationName;
function PDFDocumentTitleAttribute: PDFDocumentAttribute;
function PDFDocumentAuthorAttribute: PDFDocumentAttribute;
function PDFDocumentSubjectAttribute: PDFDocumentAttribute;
function PDFDocumentCreatorAttribute: PDFDocumentAttribute;
function PDFDocumentProducerAttribute: PDFDocumentAttribute;
function PDFDocumentCreationDateAttribute: PDFDocumentAttribute;
function PDFDocumentModificationDateAttribute: PDFDocumentAttribute;
function PDFDocumentKeywordsAttribute: PDFDocumentAttribute;
function PDFDocumentOwnerPasswordOption: PDFDocumentWriteOption;
function PDFDocumentUserPasswordOption: PDFDocumentWriteOption;
function PDFThumbnailViewDocumentEditedNotification: NSString;
function PDFViewDocumentChangedNotification: NSNotificationName;
function PDFViewChangedHistoryNotification: NSNotificationName;
function PDFViewPageChangedNotification: NSNotificationName;
function PDFViewScaleChangedNotification: NSNotificationName;
function PDFViewAnnotationHitNotification: NSNotificationName;
function PDFViewCopyPermissionNotification: NSNotificationName;
function PDFViewPrintPermissionNotification: NSNotificationName;
function PDFViewAnnotationWillHitNotification: NSNotificationName;
function PDFViewSelectionChangedNotification: NSNotificationName;
function PDFViewDisplayModeChangedNotification: NSNotificationName;
function PDFViewDisplayBoxChangedNotification: NSNotificationName;
function PDFViewVisiblePagesChangedNotification: NSNotificationName;

const
  libPDFKit = '/System/Library/Frameworks/PDFKit.framework/PDFKit';

implementation

uses
  Posix.Dlfcn;

var
  PDFKitModule: THandle;

function PDFAnnotationKeyAppearanceDictionary: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAppearanceDictionary');
end;

function PDFAnnotationKeyAppearanceState: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAppearanceState');
end;

function PDFAnnotationKeyBorder: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyBorder');
end;

function PDFAnnotationKeyColor: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyColor');
end;

function PDFAnnotationKeyContents: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyContents');
end;

function PDFAnnotationKeyFlags: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyFlags');
end;

function PDFAnnotationKeyDate: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDate');
end;

function PDFAnnotationKeyName: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyName');
end;

function PDFAnnotationKeyPage: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyPage');
end;

function PDFAnnotationKeyRect: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyRect');
end;

function PDFAnnotationKeySubtype: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeySubtype');
end;

function PDFAnnotationKeyAction: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAction');
end;

function PDFAnnotationKeyAdditionalActions: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAdditionalActions');
end;

function PDFAnnotationKeyBorderStyle: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyBorderStyle');
end;

function PDFAnnotationKeyDefaultAppearance: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDefaultAppearance');
end;

function PDFAnnotationKeyDestination: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDestination');
end;

function PDFAnnotationKeyHighlightingMode: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyHighlightingMode');
end;

function PDFAnnotationKeyInklist: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyInklist');
end;

function PDFAnnotationKeyInteriorColor: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyInteriorColor');
end;

function PDFAnnotationKeyLinePoints: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyLinePoints');
end;

function PDFAnnotationKeyLineEndingStyles: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyLineEndingStyles');
end;

function PDFAnnotationKeyIconName: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyIconName');
end;

function PDFAnnotationKeyOpen: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyOpen');
end;

function PDFAnnotationKeyParent: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyParent');
end;

function PDFAnnotationKeyPopup: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyPopup');
end;

function PDFAnnotationKeyQuadding: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyQuadding');
end;

function PDFAnnotationKeyQuadPoints: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyQuadPoints');
end;

function PDFAnnotationKeyTextLabel: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyTextLabel');
end;

function PDFAnnotationKeyWidgetDownCaption: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetDownCaption');
end;

function PDFAnnotationKeyWidgetBorderColor: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetBorderColor');
end;

function PDFAnnotationKeyWidgetBackgroundColor: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetBackgroundColor');
end;

function PDFAnnotationKeyWidgetCaption: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetCaption');
end;

function PDFAnnotationKeyWidgetDefaultValue: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetDefaultValue');
end;

function PDFAnnotationKeyWidgetFieldFlags: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetFieldFlags');
end;

function PDFAnnotationKeyWidgetFieldType: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetFieldType');
end;

function PDFAnnotationKeyWidgetAppearanceDictionary: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetAppearanceDictionary');
end;

function PDFAnnotationKeyWidgetMaxLen: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetMaxLen');
end;

function PDFAnnotationKeyWidgetOptions: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetOptions');
end;

function PDFAnnotationKeyWidgetRotation: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetRotation');
end;

function PDFAnnotationKeyWidgetRolloverCaption: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetRolloverCaption');
end;

function PDFAnnotationKeyWidgetTextLabelUI: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetTextLabelUI');
end;

function PDFAnnotationKeyWidgetValue: PDFAnnotationKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetValue');
end;

function PDFAnnotationSubtypeText: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeText');
end;

function PDFAnnotationSubtypeLink: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeLink');
end;

function PDFAnnotationSubtypeFreeText: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeFreeText');
end;

function PDFAnnotationSubtypeLine: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeLine');
end;

function PDFAnnotationSubtypeSquare: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeSquare');
end;

function PDFAnnotationSubtypeCircle: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeCircle');
end;

function PDFAnnotationSubtypeHighlight: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeHighlight');
end;

function PDFAnnotationSubtypeUnderline: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeUnderline');
end;

function PDFAnnotationSubtypeStrikeOut: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeStrikeOut');
end;

function PDFAnnotationSubtypeInk: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeInk');
end;

function PDFAnnotationSubtypeStamp: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeStamp');
end;

function PDFAnnotationSubtypePopup: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypePopup');
end;

function PDFAnnotationSubtypeWidget: PDFAnnotationSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeWidget');
end;

function PDFAnnotationWidgetSubtypeButton: PDFAnnotationWidgetSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeButton');
end;

function PDFAnnotationWidgetSubtypeChoice: PDFAnnotationWidgetSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeChoice');
end;

function PDFAnnotationWidgetSubtypeSignature: PDFAnnotationWidgetSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeSignature');
end;

function PDFAnnotationWidgetSubtypeText: PDFAnnotationWidgetSubtype;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeText');
end;

function PDFAnnotationLineEndingStyleNone: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleNone');
end;

function PDFAnnotationLineEndingStyleSquare: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleSquare');
end;

function PDFAnnotationLineEndingStyleCircle: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleCircle');
end;

function PDFAnnotationLineEndingStyleDiamond: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleDiamond');
end;

function PDFAnnotationLineEndingStyleOpenArrow: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleOpenArrow');
end;

function PDFAnnotationLineEndingStyleClosedArrow: PDFAnnotationLineEndingStyle;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleClosedArrow');
end;

function PDFAnnotationTextIconTypeComment: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeComment');
end;

function PDFAnnotationTextIconTypeKey: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeKey');
end;

function PDFAnnotationTextIconTypeNote: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeNote');
end;

function PDFAnnotationTextIconTypeHelp: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeHelp');
end;

function PDFAnnotationTextIconTypeNewParagraph: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeNewParagraph');
end;

function PDFAnnotationTextIconTypeParagraph: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeParagraph');
end;

function PDFAnnotationTextIconTypeInsert: PDFAnnotationTextIconType;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeInsert');
end;

function PDFAnnotationHighlightingModeNone: PDFAnnotationHighlightingMode;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeNone');
end;

function PDFAnnotationHighlightingModeInvert: PDFAnnotationHighlightingMode;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeInvert');
end;

function PDFAnnotationHighlightingModeOutline: PDFAnnotationHighlightingMode;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeOutline');
end;

function PDFAnnotationHighlightingModePush: PDFAnnotationHighlightingMode;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModePush');
end;

function PDFAppearanceCharacteristicsKeyBackgroundColor: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyBackgroundColor');
end;

function PDFAppearanceCharacteristicsKeyBorderColor: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyBorderColor');
end;

function PDFAppearanceCharacteristicsKeyRotation: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyRotation');
end;

function PDFAppearanceCharacteristicsKeyCaption: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyCaption');
end;

function PDFAppearanceCharacteristicsKeyRolloverCaption: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyRolloverCaption');
end;

function PDFAppearanceCharacteristicsKeyDownCaption: PDFAppearanceCharacteristicsKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyDownCaption');
end;

function PDFBorderKeyLineWidth: PDFBorderKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyLineWidth');
end;

function PDFBorderKeyStyle: PDFBorderKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyStyle');
end;

function PDFBorderKeyDashPattern: PDFBorderKey;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyDashPattern');
end;

function PDFDocumentDidUnlockNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidUnlockNotification');
end;

function PDFDocumentDidBeginFindNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginFindNotification');
end;

function PDFDocumentDidEndFindNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndFindNotification');
end;

function PDFDocumentDidBeginPageFindNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginPageFindNotification');
end;

function PDFDocumentDidEndPageFindNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndPageFindNotification');
end;

function PDFDocumentDidFindMatchNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidFindMatchNotification');
end;

function PDFDocumentDidBeginWriteNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginWriteNotification');
end;

function PDFDocumentDidEndWriteNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndWriteNotification');
end;

function PDFDocumentDidBeginPageWriteNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginPageWriteNotification');
end;

function PDFDocumentDidEndPageWriteNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndPageWriteNotification');
end;

function PDFDocumentTitleAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentTitleAttribute');
end;

function PDFDocumentAuthorAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentAuthorAttribute');
end;

function PDFDocumentSubjectAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSubjectAttribute');
end;

function PDFDocumentCreatorAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentCreatorAttribute');
end;

function PDFDocumentProducerAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentProducerAttribute');
end;

function PDFDocumentCreationDateAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentCreationDateAttribute');
end;

function PDFDocumentModificationDateAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentModificationDateAttribute');
end;

function PDFDocumentKeywordsAttribute: PDFDocumentAttribute;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentKeywordsAttribute');
end;

function PDFDocumentOwnerPasswordOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentOwnerPasswordOption');
end;

function PDFDocumentUserPasswordOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentUserPasswordOption');
end;

function PDFThumbnailViewDocumentEditedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFThumbnailViewDocumentEditedNotification');
end;

function PDFViewDocumentChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDocumentChangedNotification');
end;

function PDFViewChangedHistoryNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewChangedHistoryNotification');
end;

function PDFViewPageChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewPageChangedNotification');
end;

function PDFViewScaleChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewScaleChangedNotification');
end;

function PDFViewAnnotationHitNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewAnnotationHitNotification');
end;

function PDFViewCopyPermissionNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewCopyPermissionNotification');
end;

function PDFViewPrintPermissionNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewPrintPermissionNotification');
end;

function PDFViewAnnotationWillHitNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewAnnotationWillHitNotification');
end;

function PDFViewSelectionChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewSelectionChangedNotification');
end;

function PDFViewDisplayModeChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDisplayModeChangedNotification');
end;

function PDFViewDisplayBoxChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDisplayBoxChangedNotification');
end;

function PDFViewVisiblePagesChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewVisiblePagesChangedNotification');
end;

initialization
  PDFKitModule := dlopen(MarshaledAString(libPDFKit), RTLD_LAZY);

finalization
  dlclose(PDFKitModule);

end.