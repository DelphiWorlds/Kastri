unit DW.iOSapi.PDFKit;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics
  // DW
  {$IF CompilerVersion < 37}, DW.iOSapi.UIKit {$ENDIF};

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
  PDFAllowsLowQualityPrinting = 1;
  PDFAllowsHighQualityPrinting = 2;
  PDFAllowsDocumentChanges = 4;
  PDFAllowsDocumentAssembly = 8;
  PDFAllowsContentCopying = 16;
  PDFAllowsContentAccessibility = 32;
  PDFAllowsCommenting = 64;
  PDFAllowsFormFieldEntry = 128;
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
  kPDFAnyArea = 2147483647;
  kPDFInterpolationQualityNone = 0;
  kPDFInterpolationQualityLow = 1;
  kPDFInterpolationQualityHigh = 2;

type
  PDFAction = interface;
  PDFActionGoTo = interface;
  PDFActionNamed = interface;
  PDFActionResetForm = interface;
  PDFActionRemoteGoTo = interface;
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
  PDFPageOverlayViewProvider = interface;

  PDFActionNamedName = NSInteger;
  PDFDisplayBox = NSInteger;
  PDFPageImageInitializationOption = NSString;
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
  PDFAccessPermissions = NSInteger;
  PDFThumbnailLayoutMode = NSInteger;
  PDFDisplayMode = NSInteger;
  PDFDisplayDirection = NSInteger;
  PDFAreaOfInterest = NSInteger;
  PDFInterpolationQuality = NSInteger;

  PDFActionClass = interface(NSObjectClass)
    ['{4C9BBD4B-E79E-4981-B892-6AABDA448DE5}']
  end;

  PDFAction = interface(NSObject)
    ['{2118C2AC-8DA3-4109-9496-C8023C3FAC40}']
    function &type: NSString; cdecl;
  end;
  TPDFAction = class(TOCGenericImport<PDFActionClass, PDFAction>) end;

  PDFActionGoToClass = interface(PDFActionClass)
    ['{1C7A13E4-B73A-4D88-BDE4-7F02A813604F}']
  end;

  PDFActionGoTo = interface(PDFAction)
    ['{143E284D-1375-47ED-9244-07E46E583AC0}']
    function destination: PDFDestination; cdecl;
    function initWithDestination(destination: PDFDestination): Pointer; cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
  end;
  TPDFActionGoTo = class(TOCGenericImport<PDFActionGoToClass, PDFActionGoTo>) end;

  PDFActionNamedClass = interface(PDFActionClass)
    ['{CF4257D3-BDB0-44C1-87C6-E937B588DF3D}']
  end;

  PDFActionNamed = interface(PDFAction)
    ['{38FE0046-AF0D-4A36-9E39-FDFC64AABEEA}']
    function initWithName(name: PDFActionNamedName): Pointer; cdecl;
    function name: PDFActionNamedName; cdecl;
    procedure setName(name: PDFActionNamedName); cdecl;
  end;
  TPDFActionNamed = class(TOCGenericImport<PDFActionNamedClass, PDFActionNamed>) end;

  PDFActionResetFormClass = interface(PDFActionClass)
    ['{B48F502D-5E67-40A4-9B7E-1FBB30F486C6}']
  end;

  PDFActionResetForm = interface(PDFAction)
    ['{83215791-173B-46DB-A9AB-8F601067BA21}']
    function fields: NSArray; cdecl;
    function fieldsIncludedAreCleared: Boolean; cdecl;
    procedure setFields(fields: NSArray); cdecl;
    procedure setFieldsIncludedAreCleared(fieldsIncludedAreCleared: Boolean); cdecl;
  end;
  TPDFActionResetForm = class(TOCGenericImport<PDFActionResetFormClass, PDFActionResetForm>) end;

  PDFActionRemoteGoToClass = interface(PDFActionClass)
    ['{958B0002-0638-4D18-A7EA-6A1E3FEEE310}']
  end;

  PDFActionRemoteGoTo = interface(PDFAction)
    ['{08184F2F-3BC0-4893-B848-C4501BD50E83}']
    function initWithPageIndex(pageIndex: NSUInteger; atPoint: CGPoint; fileURL: NSURL): Pointer; cdecl;
    function pageIndex: NSUInteger; cdecl;
    function point: CGPoint; cdecl;
    procedure setPageIndex(pageIndex: NSUInteger); cdecl;
    procedure setPoint(point: CGPoint); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionRemoteGoTo = class(TOCGenericImport<PDFActionRemoteGoToClass, PDFActionRemoteGoTo>) end;

  PDFActionURLClass = interface(PDFActionClass)
    ['{2DD0B28B-7EFC-47A6-9C35-4C1089C60BCA}']
  end;

  PDFActionURL = interface(PDFAction)
    ['{AD68ED50-C4FB-44BA-8802-275ED0E2B01C}']
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionURL = class(TOCGenericImport<PDFActionURLClass, PDFActionURL>) end;

  PDFPageClass = interface(NSObjectClass)
    ['{A6DD8008-2DDB-41C8-B45B-C77B42E12463}']
  end;

  PDFPage = interface(NSObject)
    ['{AC296F83-8FA2-4B5C-898F-31ED231C1D44}']
    function &label: NSString; cdecl;
    function &string: NSString; cdecl;
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
    function initWithImage(image: UIImage; options: NSDictionary): Pointer; overload; cdecl;
    function initWithImage(image: UIImage): Pointer; overload; cdecl;
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
    function thumbnailOfSize(size: CGSize; forBox: PDFDisplayBox): UIImage; cdecl;
    procedure transformContext(context: CGContextRef; forBox: PDFDisplayBox); cdecl;
    function transformForBox(box: PDFDisplayBox): CGAffineTransform; cdecl;
  end;
  TPDFPage = class(TOCGenericImport<PDFPageClass, PDFPage>) end;

  PDFAnnotationClass = interface(NSObjectClass)
    ['{D4AC98FE-6383-444C-A738-61C0E90FE658}']
    {class} function lineStyleFromName(name: NSString): PDFLineStyle; cdecl;
    {class} function nameForLineStyle(style: PDFLineStyle): NSString; cdecl;
  end;

  PDFAnnotation = interface(NSObject)
    ['{D690B0AF-1089-4E7B-BD01-0AF332A014D6}']
    function &type: NSString; cdecl;
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
    ['{145CF234-E316-4150-AD86-00A46F4B89BD}']
  end;

  PDFAppearanceCharacteristics = interface(NSObject)
    ['{0892C247-2049-420D-9709-74154F41D8F3}']
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
    ['{70166163-2673-429B-BA64-6BB8E2A5CC41}']
  end;

  PDFBorder = interface(NSObject)
    ['{5DDA24D7-B16F-43D6-9D97-E4BD4803F34C}']
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
    ['{33EB60B8-507D-48EA-AA72-B08EBE539E04}']
  end;

  PDFDestination = interface(NSObject)
    ['{215E9302-C38B-445D-8276-282803C14524}']
    function compare(destination: PDFDestination): NSComparisonResult; cdecl;
    function initWithPage(page: PDFPage; atPoint: CGPoint): Pointer; cdecl;
    function page: PDFPage; cdecl;
    function point: CGPoint; cdecl;
    procedure setZoom(zoom: CGFloat); cdecl;
    function zoom: CGFloat; cdecl;
  end;
  TPDFDestination = class(TOCGenericImport<PDFDestinationClass, PDFDestination>) end;

  PDFDocumentClass = interface(NSObjectClass)
    ['{ADE419C4-C875-4167-A65A-65050D1C6182}']
  end;

  PDFDocument = interface(NSObject)
    ['{071D3BB0-DD3C-44F2-B27D-FE7E73F17836}']
    function &string: NSString; cdecl;
    function accessPermissions: PDFAccessPermissions; cdecl;
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
    function findString(&string: NSString; fromSelection: PDFSelection; withOptions: NSStringCompareOptions): PDFSelection; overload; cdecl;
    function findString(&string: NSString; withOptions: NSStringCompareOptions): NSArray; overload; cdecl;
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
    [MethodName('selectionFromPage:atPoint:toPage:atPoint:')]
    function selectionFromPage(startPage: PDFPage; atPoint: CGPoint; toPage: PDFPage; endPoint: CGPoint): PDFSelection; overload; cdecl;
    [MethodName('selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:')]
    function selectionFromPage(startPage: PDFPage; atCharacterIndex: NSUInteger; toPage: PDFPage; endCharacter: NSUInteger): PDFSelection; overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDocumentAttributes(documentAttributes: NSDictionary); cdecl;
    procedure setOutlineRoot(outlineRoot: PDFOutline); cdecl;
    function unlockWithPassword(password: NSString): Boolean; cdecl;
    function writeToFile(path: NSString): Boolean; overload; cdecl;
    function writeToFile(path: NSString; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToURL(url: NSURL; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToURL(url: NSURL): Boolean; overload; cdecl;
  end;
  TPDFDocument = class(TOCGenericImport<PDFDocumentClass, PDFDocument>) end;

  PDFDocumentDelegate = interface(IObjectiveC)
    ['{7BE1674B-641E-4A4B-815F-7F9B7897AB34}']
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
    ['{C6B4FF53-6B56-4240-8416-4E03D0318880}']
  end;

  PDFOutline = interface(NSObject)
    ['{B7C73CE3-3C9F-49A5-B4DC-200D326AE35A}']
    function &label: NSString; cdecl;
    function action: PDFAction; cdecl;
    function childAtIndex(index: NSUInteger): PDFOutline; cdecl;
    function destination: PDFDestination; cdecl;
    function document: PDFDocument; cdecl;
    function index: NSUInteger; cdecl;
    procedure insertChild(child: PDFOutline; atIndex: NSUInteger); cdecl;
    function isOpen: Boolean; cdecl;
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
    ['{790235A1-E266-4167-8E49-E7FD390E391B}']
  end;

  PDFSelection = interface(NSObject)
    ['{E798AC2B-04F8-4900-9635-87A2C9F766C0}']
    function &string: NSString; cdecl;
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
  end;
  TPDFSelection = class(TOCGenericImport<PDFSelectionClass, PDFSelection>) end;

  PDFThumbnailViewClass = interface(UIViewClass)
    ['{96B1B157-6EC6-47AF-BE29-0C7CF48D1A9A}']
  end;

  PDFThumbnailView = interface(UIView)
    ['{64655607-F895-4945-9E52-4189244E516A}']
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
    ['{17831A2A-9716-4573-A212-53912411C235}']
  end;

  PDFView = interface(UIView)
    ['{783575F3-3FAE-4DEE-A6EC-D938A917587B}']
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
    function findInteraction: UIFindInteraction; cdecl;
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
    function isFindInteractionEnabled: Boolean; cdecl;
    function isInMarkupMode: Boolean; cdecl;
    function isUsingPageViewController: Boolean; cdecl;
    procedure layoutDocumentView; cdecl;
    function maxScaleFactor: CGFloat; cdecl;
    function minScaleFactor: CGFloat; cdecl;
    function pageBreakMargins: UIEdgeInsets; cdecl;
    function pageForPoint(point: CGPoint; nearest: Boolean): PDFPage; cdecl;
    function pageOverlayViewProvider: Pointer; cdecl;
    function pageShadowsEnabled: Boolean; cdecl;
    procedure performAction(action: PDFAction); cdecl;
    function rowSizeForPage(page: PDFPage): CGSize; cdecl;
    function scaleFactor: CGFloat; cdecl;
    function scaleFactorForSizeToFit: CGFloat; cdecl;
    procedure scrollSelectionToVisible(sender: Pointer); cdecl;
    procedure selectAll(sender: Pointer); cdecl;
    procedure setAutoScales(autoScales: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setCurrentSelection(selection: PDFSelection; animate: Boolean); overload; cdecl;
    procedure setCurrentSelection(currentSelection: PDFSelection); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDisplayBox(displayBox: PDFDisplayBox); cdecl;
    procedure setDisplayDirection(displayDirection: PDFDisplayDirection); cdecl;
    procedure setDisplayMode(displayMode: PDFDisplayMode); cdecl;
    procedure setDisplaysAsBook(displaysAsBook: Boolean); cdecl;
    procedure setDisplaysPageBreaks(displaysPageBreaks: Boolean); cdecl;
    procedure setDisplaysRTL(displaysRTL: Boolean); cdecl;
    procedure setDocument(document: PDFDocument); cdecl;
    procedure setEnableDataDetectors(enableDataDetectors: Boolean); cdecl;
    procedure setFindInteractionEnabled(findInteractionEnabled: Boolean); cdecl;
    procedure setHighlightedSelections(highlightedSelections: NSArray); cdecl;
    procedure setInMarkupMode(inMarkupMode: Boolean); cdecl;
    procedure setInterpolationQuality(interpolationQuality: PDFInterpolationQuality); cdecl;
    procedure setMaxScaleFactor(maxScaleFactor: CGFloat); cdecl;
    procedure setMinScaleFactor(minScaleFactor: CGFloat); cdecl;
    procedure setPageBreakMargins(pageBreakMargins: UIEdgeInsets); cdecl;
    procedure setPageOverlayViewProvider(pageOverlayViewProvider: Pointer); cdecl;
    procedure setScaleFactor(scaleFactor: CGFloat); cdecl;
    procedure usePageViewController(enable: Boolean; withViewOptions: NSDictionary); cdecl;
    function visiblePages: NSArray; cdecl;
    procedure zoomIn(sender: Pointer); cdecl;
    procedure zoomOut(sender: Pointer); cdecl;
  end;
  TPDFView = class(TOCGenericImport<PDFViewClass, PDFView>) end;

  PDFViewDelegate = interface(IObjectiveC)
    ['{8B663321-6395-44C1-8C2B-E2DA70A6AD47}']
    procedure PDFViewOpenPDF(sender: PDFView; forRemoteGoToAction: PDFActionRemoteGoTo); cdecl;
    function PDFViewParentViewController: UIViewController; cdecl;
    procedure PDFViewPerformFind(sender: PDFView); cdecl;
    procedure PDFViewPerformGoToPage(sender: PDFView); cdecl;
    procedure PDFViewWillClickOnLink(sender: PDFView; withURL: NSURL); cdecl;
  end;

  PDFPageOverlayViewProvider = interface(IObjectiveC)
    ['{E0A01002-6E74-4FD5-927F-99B422B96A6E}']
    [MethodName('pdfView:overlayViewForPage:')]
    function pdfViewOverlayViewForPage(view: PDFView; overlayViewForPage: PDFPage): UIView; cdecl;
    [MethodName('pdfView:willDisplayOverlayView:forPage:')]
    procedure pdfViewWillDisplayOverlayView(pdfView: PDFView; willDisplayOverlayView: UIView; forPage: PDFPage); cdecl;
    [MethodName('pdfView:willEndDisplayingOverlayView:forPage:')]
    procedure pdfViewWillEndDisplayingOverlayView(pdfView: PDFView; willEndDisplayingOverlayView: UIView; forPage: PDFPage); cdecl;
  end;

function PDFPageImageInitializationOptionMediaBox: PDFPageImageInitializationOption;
function PDFPageImageInitializationOptionRotation: PDFPageImageInitializationOption;
function PDFPageImageInitializationOptionUpscaleIfSmaller: PDFPageImageInitializationOption;
function PDFPageImageInitializationOptionCompressionQuality: PDFPageImageInitializationOption;
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
function PDFDocumentFoundSelectionKey: NSString;
function PDFDocumentPageIndexKey: NSString;
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
function PDFDocumentAccessPermissionsOption: PDFDocumentWriteOption;
function PDFDocumentBurnInAnnotationsOption: PDFDocumentWriteOption;
function PDFDocumentSaveTextFromOCROption: PDFDocumentWriteOption;
function PDFDocumentSaveImagesAsJPEGOption: PDFDocumentWriteOption;
function PDFDocumentOptimizeImagesForScreenOption: PDFDocumentWriteOption;
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

function PDFPageImageInitializationOptionMediaBox: PDFPageImageInitializationOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionMediaBox');
end;

function PDFPageImageInitializationOptionRotation: PDFPageImageInitializationOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionRotation');
end;

function PDFPageImageInitializationOptionUpscaleIfSmaller: PDFPageImageInitializationOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionUpscaleIfSmaller');
end;

function PDFPageImageInitializationOptionCompressionQuality: PDFPageImageInitializationOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionCompressionQuality');
end;

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

function PDFDocumentFoundSelectionKey: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentFoundSelectionKey');
end;

function PDFDocumentPageIndexKey: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentPageIndexKey');
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

function PDFDocumentAccessPermissionsOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentAccessPermissionsOption');
end;

function PDFDocumentBurnInAnnotationsOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentBurnInAnnotationsOption');
end;

function PDFDocumentSaveTextFromOCROption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSaveTextFromOCROption');
end;

function PDFDocumentSaveImagesAsJPEGOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSaveImagesAsJPEGOption');
end;

function PDFDocumentOptimizeImagesForScreenOption: PDFDocumentWriteOption;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentOptimizeImagesForScreenOption');
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