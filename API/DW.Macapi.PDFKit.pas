unit DW.Macapi.PDFKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Foundation, Macapi.CocoaTypes, Macapi.AppKit;

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
  kPDFPrintPageScaleNone = 0;
  kPDFPrintPageScaleToFit = 1;
  kPDFPrintPageScaleDownToFit = 2;
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
  kPDFAnyArea = NSIntegerMax;
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
  PDFAnnotationButtonWidget = interface;
  PDFAnnotationChoiceWidget = interface;
  PDFAnnotationCircle = interface;
  PDFAnnotationFreeText = interface;
  PDFAnnotationInk = interface;
  PDFAnnotationLine = interface;
  PDFAnnotationLink = interface;
  PDFAnnotationMarkup = interface;
  PDFAnnotationPopup = interface;
  PDFAnnotationSquare = interface;
  PDFAnnotationStamp = interface;
  PDFAnnotationText = interface;
  PDFAnnotationTextWidget = interface;

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
  PDFPrintScalingMode = NSInteger;
  PDFDocumentPermissions = NSInteger;
  PDFDocumentAttribute = NSString;
  PDFDocumentWriteOption = NSString;
  PDFAccessPermissions = NSInteger;
  PDFDisplayMode = NSInteger;
  PDFDisplayDirection = NSInteger;
  PDFAreaOfInterest = NSInteger;
  PDFInterpolationQuality = NSInteger;

  PDFActionClass = interface(NSObjectClass)
    ['{FD203D03-361E-4DB0-A599-A8FC2EB22808}']
  end;

  PDFAction = interface(NSObject)
    ['{56C00771-8215-4976-A568-3B197077EDEE}']
    function &type: NSString; cdecl;
  end;
  TPDFAction = class(TOCGenericImport<PDFActionClass, PDFAction>) end;

  PDFActionGoToClass = interface(PDFActionClass)
    ['{30B7DD8D-17C2-4669-9F5B-24518A67A741}']
  end;

  PDFActionGoTo = interface(PDFAction)
    ['{30CCFE1A-DCF1-4DCE-AAF8-76F48008BA20}']
    function destination: PDFDestination; cdecl;
    function initWithDestination(destination: PDFDestination): Pointer; cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
  end;
  TPDFActionGoTo = class(TOCGenericImport<PDFActionGoToClass, PDFActionGoTo>) end;

  PDFActionNamedClass = interface(PDFActionClass)
    ['{A6642C85-617A-4C3A-9217-CF83DB05717C}']
  end;

  PDFActionNamed = interface(PDFAction)
    ['{1FF0EAF5-A41B-4CF4-B0FB-420B0D3437F5}']
    function initWithName(name: PDFActionNamedName): Pointer; cdecl;
    function name: PDFActionNamedName; cdecl;
    procedure setName(name: PDFActionNamedName); cdecl;
  end;
  TPDFActionNamed = class(TOCGenericImport<PDFActionNamedClass, PDFActionNamed>) end;

  PDFActionResetFormClass = interface(PDFActionClass)
    ['{54FFAC60-20D2-4664-A6A2-2DAF9F29F6D9}']
  end;

  PDFActionResetForm = interface(PDFAction)
    ['{048A0192-30BB-4AA4-9B11-111A361C6784}']
    function fields: NSArray; cdecl;
    function fieldsIncludedAreCleared: Boolean; cdecl;
    procedure setFields(fields: NSArray); cdecl;
    procedure setFieldsIncludedAreCleared(fieldsIncludedAreCleared: Boolean); cdecl;
  end;
  TPDFActionResetForm = class(TOCGenericImport<PDFActionResetFormClass, PDFActionResetForm>) end;

  PDFActionRemoteGoToClass = interface(PDFActionClass)
    ['{82EAFBC0-6999-4F2B-85B7-53C96B061A39}']
  end;

  PDFActionRemoteGoTo = interface(PDFAction)
    ['{3FC877F2-DA22-4500-BD16-9146BB9D6547}']
    function initWithPageIndex(pageIndex: NSUInteger; atPoint: NSPoint; fileURL: NSURL): Pointer; cdecl;
    function pageIndex: NSUInteger; cdecl;
    function point: NSPoint; cdecl;
    procedure setPageIndex(pageIndex: NSUInteger); cdecl;
    procedure setPoint(point: NSPoint); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionRemoteGoTo = class(TOCGenericImport<PDFActionRemoteGoToClass, PDFActionRemoteGoTo>) end;

  PDFActionURLClass = interface(PDFActionClass)
    ['{9B2E7B8D-AC2C-4F0D-8FAA-3E61CB41DE91}']
  end;

  PDFActionURL = interface(PDFAction)
    ['{E819AD75-A1E6-49F2-B713-02E29EE29835}']
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFActionURL = class(TOCGenericImport<PDFActionURLClass, PDFActionURL>) end;

  PDFPageClass = interface(NSObjectClass)
    ['{5E64DFCA-DC94-447D-83F3-DBA32EFABEEA}']
  end;

  PDFPage = interface(NSObject)
    ['{7CBCC3E6-AD61-4E6A-B8C2-96313582E840}']
    procedure addAnnotation(annotation: PDFAnnotation); cdecl;
    function annotationAtPoint(point: NSPoint): PDFAnnotation; cdecl;
    function annotations: NSArray; cdecl;
    function attributedString: NSAttributedString; cdecl;
    function boundsForBox(box: PDFDisplayBox): NSRect; cdecl;
    function characterBoundsAtIndex(index: NSInteger): NSRect; cdecl;
    function characterIndexAtPoint(point: NSPoint): NSInteger; cdecl;
    function dataRepresentation: NSData; cdecl;
    function displaysAnnotations: Boolean; cdecl;
    function document: PDFDocument; cdecl;
    procedure drawWithBox(box: PDFDisplayBox; toContext: CGContextRef); overload; cdecl;
    procedure drawWithBox(box: PDFDisplayBox); overload; cdecl;
    function initWithImage(image: NSImage; options: NSDictionary): Pointer; overload; cdecl;
    function initWithImage(image: NSImage): Pointer; overload; cdecl;
    function &label: NSString; cdecl;
    function numberOfCharacters: NSUInteger; cdecl;
    function pageRef: CGPDFPageRef; cdecl;
    procedure removeAnnotation(annotation: PDFAnnotation); cdecl;
    function rotation: NSInteger; cdecl;
    function selectionForLineAtPoint(point: NSPoint): PDFSelection; cdecl;
    function selectionForRange(range: NSRange): PDFSelection; cdecl;
    function selectionForRect(rect: NSRect): PDFSelection; cdecl;
    function selectionForWordAtPoint(point: NSPoint): PDFSelection; cdecl;
    function selectionFromPoint(startPoint: NSPoint; toPoint: NSPoint): PDFSelection; cdecl;
    procedure setBounds(bounds: NSRect; forBox: PDFDisplayBox); cdecl;
    procedure setDisplaysAnnotations(displaysAnnotations: Boolean); cdecl;
    procedure setRotation(rotation: NSInteger); cdecl;
    function &string: NSString; cdecl;
    function thumbnailOfSize(size: NSSize; forBox: PDFDisplayBox): NSImage; cdecl;
    procedure transformContext(context: CGContextRef; forBox: PDFDisplayBox); cdecl;
    procedure transformContextForBox(box: PDFDisplayBox); cdecl;
    function transformForBox(box: PDFDisplayBox): CGAffineTransform; cdecl;
  end;
  TPDFPage = class(TOCGenericImport<PDFPageClass, PDFPage>) end;

  PDFAnnotationClass = interface(NSObjectClass)
    ['{1681C752-0840-43F7-85EE-6F7154C1E6E2}']
    {class} function lineStyleFromName(name: NSString): PDFLineStyle; cdecl;
    {class} function nameForLineStyle(style: PDFLineStyle): NSString; cdecl;
  end;

  PDFAnnotation = interface(NSObject)
    ['{F02D0969-55AE-434D-9D93-5797317578D2}']
    function action: PDFAction; cdecl;
    procedure addBezierPath(path: NSBezierPath); cdecl;
    function alignment: NSTextAlignment; cdecl;
    function allowsToggleToOff: Boolean; cdecl;
    function annotationKeyValues: NSDictionary; cdecl;
    function backgroundColor: NSColor; cdecl;
    function border: PDFBorder; cdecl;
    function bounds: NSRect; cdecl;
    function buttonWidgetState: PDFWidgetCellState; cdecl;
    function buttonWidgetStateString: NSString; cdecl;
    function caption: NSString; cdecl;
    function choices: NSArray; cdecl;
    function color: NSColor; cdecl;
    function contents: NSString; cdecl;
    function destination: PDFDestination; cdecl;
    procedure drawWithBox(box: PDFDisplayBox); overload; cdecl;
    procedure drawWithBox(box: PDFDisplayBox; inContext: CGContextRef); overload; cdecl;
    function endLineStyle: PDFLineStyle; cdecl;
    function endPoint: NSPoint; cdecl;
    function fieldName: NSString; cdecl;
    function font: NSFont; cdecl;
    function fontColor: NSColor; cdecl;
    function hasAppearanceStream: Boolean; cdecl;
    function hasComb: Boolean; cdecl;
    function iconType: PDFTextAnnotationIconType; cdecl;
    function initWithBounds(bounds: NSRect): Pointer; overload; cdecl;
    function initWithBounds(bounds: NSRect; forType: PDFAnnotationSubtype; withProperties: NSDictionary): Pointer; overload; cdecl;
    function initWithDictionary(dictionary: NSDictionary; forPage: PDFPage): Pointer; cdecl;
    function interiorColor: NSColor; cdecl;
    function isActivatableTextField: Boolean; cdecl;
    function isHighlighted: Boolean; cdecl;
    function isListChoice: Boolean; cdecl;
    function isMultiline: Boolean; cdecl;
    function isOpen: Boolean; cdecl;
    function isPasswordField: Boolean; cdecl;
    function isReadOnly: Boolean; cdecl;
    function markupType: PDFMarkupType; cdecl;
    function maximumLength: NSInteger; cdecl;
    function modificationDate: NSDate; cdecl;
    function mouseUpAction: PDFAction; cdecl;
    function page: PDFPage; cdecl;
    function paths: NSArray; cdecl;
    function popup: PDFAnnotation; cdecl;
    function quadrilateralPoints: NSArray; cdecl;
    function radiosInUnison: Boolean; cdecl;
    procedure removeAllAppearanceStreams; cdecl;
    procedure removeBezierPath(path: NSBezierPath); cdecl;
    procedure removeValueForAnnotationKey(key: PDFAnnotationKey); cdecl;
    procedure setAction(action: PDFAction); cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    procedure setAllowsToggleToOff(allowsToggleToOff: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: NSColor); cdecl;
    function setBoolean(value: Boolean; forAnnotationKey: PDFAnnotationKey): Boolean; cdecl;
    procedure setBorder(border: PDFBorder); cdecl;
    procedure setBounds(bounds: NSRect); cdecl;
    procedure setButtonWidgetState(buttonWidgetState: PDFWidgetCellState); cdecl;
    procedure setButtonWidgetStateString(buttonWidgetStateString: NSString); cdecl;
    procedure setCaption(caption: NSString); cdecl;
    procedure setChoices(choices: NSArray); cdecl;
    procedure setColor(color: NSColor); cdecl;
    procedure setComb(comb: Boolean); cdecl;
    procedure setContents(contents: NSString); cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
    procedure setEndLineStyle(endLineStyle: PDFLineStyle); cdecl;
    procedure setEndPoint(endPoint: NSPoint); cdecl;
    procedure setFieldName(fieldName: NSString); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setFontColor(fontColor: NSColor); cdecl;
    procedure setHighlighted(highlighted: Boolean); cdecl;
    procedure setIconType(iconType: PDFTextAnnotationIconType); cdecl;
    procedure setInteriorColor(interiorColor: NSColor); cdecl;
    procedure setListChoice(listChoice: Boolean); cdecl;
    procedure setMarkupType(markupType: PDFMarkupType); cdecl;
    procedure setMaximumLength(maximumLength: NSInteger); cdecl;
    procedure setModificationDate(modificationDate: NSDate); cdecl;
    procedure setMouseUpAction(mouseUpAction: PDFAction); cdecl;
    procedure setMultiline(multiline: Boolean); cdecl;
    procedure setOpen(open: Boolean); cdecl;
    procedure setPage(page: PDFPage); cdecl;
    procedure setPopup(popup: PDFAnnotation); cdecl;
    procedure setQuadrilateralPoints(quadrilateralPoints: NSArray); cdecl;
    procedure setRadiosInUnison(radiosInUnison: Boolean); cdecl;
    procedure setReadOnly(readOnly: Boolean); cdecl;
    function setRect(value: NSRect; forAnnotationKey: PDFAnnotationKey): Boolean; cdecl;
    procedure setShouldDisplay(shouldDisplay: Boolean); cdecl;
    procedure setShouldPrint(shouldPrint: Boolean); cdecl;
    procedure setStampName(stampName: NSString); cdecl;
    procedure setStartLineStyle(startLineStyle: PDFLineStyle); cdecl;
    procedure setStartPoint(startPoint: NSPoint); cdecl;
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
    function startPoint: NSPoint; cdecl;
    function toolTip: NSString; cdecl;
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
    ['{F39266CC-9EA6-427C-877E-DC36BD1A3F5B}']
  end;

  PDFAppearanceCharacteristics = interface(NSObject)
    ['{EC3C03B8-BDE6-4593-8A76-3299F1DFB3F8}']
    function appearanceCharacteristicsKeyValues: NSDictionary; cdecl;
    function backgroundColor: NSColor; cdecl;
    function borderColor: NSColor; cdecl;
    function caption: NSString; cdecl;
    function controlType: PDFWidgetControlType; cdecl;
    function downCaption: NSString; cdecl;
    function rolloverCaption: NSString; cdecl;
    function rotation: NSInteger; cdecl;
    procedure setBackgroundColor(backgroundColor: NSColor); cdecl;
    procedure setBorderColor(borderColor: NSColor); cdecl;
    procedure setCaption(caption: NSString); cdecl;
    procedure setControlType(controlType: PDFWidgetControlType); cdecl;
    procedure setDownCaption(downCaption: NSString); cdecl;
    procedure setRolloverCaption(rolloverCaption: NSString); cdecl;
    procedure setRotation(rotation: NSInteger); cdecl;
  end;
  TPDFAppearanceCharacteristics = class(TOCGenericImport<PDFAppearanceCharacteristicsClass, PDFAppearanceCharacteristics>) end;

  PDFBorderClass = interface(NSObjectClass)
    ['{A381A9CD-A8C0-443C-92FE-A36901E06033}']
  end;

  PDFBorder = interface(NSObject)
    ['{CFC412DF-D24A-46B0-96E4-6D148FE3D1CB}']
    function borderKeyValues: NSDictionary; cdecl;
    function dashPattern: NSArray; cdecl;
    procedure drawInRect(rect: NSRect); cdecl;
    function lineWidth: CGFloat; cdecl;
    procedure setDashPattern(dashPattern: NSArray); cdecl;
    procedure setLineWidth(lineWidth: CGFloat); cdecl;
    procedure setStyle(style: PDFBorderStyle); cdecl;
    function style: PDFBorderStyle; cdecl;
  end;
  TPDFBorder = class(TOCGenericImport<PDFBorderClass, PDFBorder>) end;

  PDFDestinationClass = interface(NSObjectClass)
    ['{73EA7118-9C9E-4186-B67D-14587925B555}']
  end;

  PDFDestination = interface(NSObject)
    ['{DF29D8F6-234B-46B2-9140-6C7F0279BE3D}']
    function compare(destination: PDFDestination): NSComparisonResult; cdecl;
    function initWithPage(page: PDFPage; atPoint: NSPoint): Pointer; cdecl;
    function page: PDFPage; cdecl;
    function point: NSPoint; cdecl;
    procedure setZoom(zoom: CGFloat); cdecl;
    function zoom: CGFloat; cdecl;
  end;
  TPDFDestination = class(TOCGenericImport<PDFDestinationClass, PDFDestination>) end;

  PDFDocumentClass = interface(NSObjectClass)
    ['{67AD31FC-9B7A-401C-8567-1FA9B44DF8C1}']
  end;

  PDFDocument = interface(NSObject)
    ['{91E4081A-AE9A-46EE-8967-BBDE3BF6DF60}']
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
    function printOperationForPrintInfo(printInfo: NSPrintInfo; scalingMode: PDFPrintScalingMode; autoRotate: Boolean): NSPrintOperation; cdecl;
    procedure removePageAtIndex(index: NSUInteger); cdecl;
    function selectionForEntireDocument: PDFSelection; cdecl;
    [MethodName('selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:')]
    function selectionFromPage(startPage: PDFPage; atCharacterIndex: NSUInteger; toPage: PDFPage;
      endCharacter: NSUInteger): PDFSelection; overload; cdecl;
    [MethodName('selectionFromPage:atPoint:toPage:atPoint:')]
    function selectionFromPage(startPage: PDFPage; atPoint: NSPoint; toPage: PDFPage; endPoint: NSPoint): PDFSelection; overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDocumentAttributes(documentAttributes: NSDictionary); cdecl;
    procedure setOutlineRoot(outlineRoot: PDFOutline); cdecl;
    function &string: NSString; cdecl;
    function unlockWithPassword(password: NSString): Boolean; cdecl;
    function writeToFile(path: NSString): Boolean; overload; cdecl;
    function writeToFile(path: NSString; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToURL(url: NSURL; withOptions: NSDictionary): Boolean; overload; cdecl;
    function writeToURL(url: NSURL): Boolean; overload; cdecl;
  end;
  TPDFDocument = class(TOCGenericImport<PDFDocumentClass, PDFDocument>) end;

  PDFDocumentDelegate = interface(IObjectiveC)
    ['{4EFE9AAD-EA21-4290-AFC6-7C971CD3F199}']
    function classForAnnotationClass(annotationClass: Pointer): Pointer; cdecl;
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
    ['{567F0A63-16F4-4B84-A944-CFC8EFED151F}']
  end;

  PDFOutline = interface(NSObject)
    ['{4FEC4C93-C09A-4E2A-B27B-E41096D463AD}']
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
    ['{2252CE8A-6F94-4410-A9BB-B286037D6E70}']
  end;

  PDFSelection = interface(NSObject)
    ['{E7456EEC-5F33-4ACF-A766-10132F250234}']
    procedure addSelection(selection: PDFSelection); cdecl;
    procedure addSelections(selections: NSArray); cdecl;
    function attributedString: NSAttributedString; cdecl;
    function boundsForPage(page: PDFPage): NSRect; cdecl;
    function color: NSColor; cdecl;
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
    procedure setColor(color: NSColor); cdecl;
    function &string: NSString; cdecl;
  end;
  TPDFSelection = class(TOCGenericImport<PDFSelectionClass, PDFSelection>) end;

  PDFThumbnailViewClass = interface(NSViewClass)
    ['{2847FF07-8DB2-498D-8D0D-576E1D3427BF}']
  end;

  PDFThumbnailView = interface(NSView)
    ['{2ED6C95E-B83F-48A0-B78A-4CB26A8E5D5B}']
    function allowsDragging: Boolean; cdecl;
    function allowsMultipleSelection: Boolean; cdecl;
    function backgroundColor: NSColor; cdecl;
    function labelFont: NSFont; cdecl;
    function maximumNumberOfColumns: NSUInteger; cdecl;
    function PDFView: PDFView; cdecl;
    function selectedPages: NSArray; cdecl;
    procedure setAllowsDragging(allowsDragging: Boolean); cdecl;
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: NSColor); cdecl;
    procedure setLabelFont(labelFont: NSFont); cdecl;
    procedure setMaximumNumberOfColumns(maximumNumberOfColumns: NSUInteger); cdecl;
    procedure setPDFView(PDFView: PDFView); cdecl;
    procedure setThumbnailSize(thumbnailSize: NSSize); cdecl;
    function thumbnailSize: NSSize; cdecl;
  end;
  TPDFThumbnailView = class(TOCGenericImport<PDFThumbnailViewClass, PDFThumbnailView>) end;

  PDFViewClass = interface(NSViewClass)
    ['{5C982233-9158-448C-A833-C96670E2D476}']
  end;

  PDFView = interface(NSView)
    ['{41DA2235-B8D0-426F-BCCE-B3444082D51B}']
    function acceptsDraggedFiles: Boolean; cdecl;
    function allowsDragging: Boolean; cdecl;
    procedure annotationsChangedOnPage(page: PDFPage); cdecl;
    function areaOfInterestForMouse(event: NSEvent): PDFAreaOfInterest; cdecl;
    function areaOfInterestForPoint(cursorLocation: NSPoint): PDFAreaOfInterest; cdecl;
    function autoScales: Boolean; cdecl;
    function backgroundColor: NSColor; cdecl;
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
    function convertPointFromPage(point: NSPoint; fromPage: PDFPage): NSPoint; cdecl;
    [MethodName('convertPoint:toPage:')]
    function convertPointToPage(point: NSPoint; toPage: PDFPage): NSPoint; cdecl;
    [MethodName('convertRect:fromPage:')]
    function convertRectFromPage(rect: NSRect; fromPage: PDFPage): NSRect; cdecl;
    [MethodName('convertRect:toPage:')]
    function convertRectToPage(rect: NSRect; toPage: PDFPage): NSRect; cdecl;
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
    function documentView: NSView; cdecl;
    procedure drawPage(page: PDFPage; toContext: CGContextRef); overload; cdecl;
    procedure drawPage(page: PDFPage); overload; cdecl;
    procedure drawPagePost(page: PDFPage; toContext: CGContextRef); overload; cdecl;
    procedure drawPagePost(page: PDFPage); overload; cdecl;
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
    procedure goToRect(rect: NSRect; onPage: PDFPage); cdecl;
    procedure goToSelection(selection: PDFSelection); cdecl;
    function greekingThreshold: CGFloat; cdecl;
    function highlightedSelections: NSArray; cdecl;
    function interpolationQuality: PDFInterpolationQuality; cdecl;
    function isInMarkupMode: Boolean; cdecl;
    procedure layoutDocumentView; cdecl;
    function maxScaleFactor: CGFloat; cdecl;
    function minScaleFactor: CGFloat; cdecl;
    function pageBreakMargins: NSEdgeInsets; cdecl;
    function pageForPoint(point: NSPoint; nearest: Boolean): PDFPage; cdecl;
    function pageOverlayViewProvider: Pointer; cdecl;
    function pageShadowsEnabled: Boolean; cdecl;
    procedure performAction(action: PDFAction); cdecl;
    procedure printWithInfo(printInfo: NSPrintInfo; autoRotate: Boolean); overload; cdecl;
    procedure printWithInfo(printInfo: NSPrintInfo; autoRotate: Boolean; pageScaling: PDFPrintScalingMode); overload; cdecl;
    function rowSizeForPage(page: PDFPage): NSSize; cdecl;
    function scaleFactor: CGFloat; cdecl;
    function scaleFactorForSizeToFit: CGFloat; cdecl;
    procedure scrollSelectionToVisible(sender: Pointer); cdecl;
    procedure selectAll(sender: Pointer); cdecl;
    procedure setAcceptsDraggedFiles(acceptsDraggedFiles: Boolean); cdecl;
    procedure setAllowsDragging(allowsDragging: Boolean); cdecl;
    procedure setAutoScales(autoScales: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: NSColor); cdecl;
    procedure setCurrentSelection(selection: PDFSelection; animate: Boolean); overload; cdecl;
    procedure setCurrentSelection(currentSelection: PDFSelection); overload; cdecl;
    procedure setCursorForAreaOfInterest(area: PDFAreaOfInterest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDisplayBox(displayBox: PDFDisplayBox); cdecl;
    procedure setDisplayDirection(displayDirection: PDFDisplayDirection); cdecl;
    procedure setDisplayMode(displayMode: PDFDisplayMode); cdecl;
    procedure setDisplaysAsBook(displaysAsBook: Boolean); cdecl;
    procedure setDisplaysPageBreaks(displaysPageBreaks: Boolean); cdecl;
    procedure setDisplaysRTL(displaysRTL: Boolean); cdecl;
    procedure setDocument(document: PDFDocument); cdecl;
    procedure setEnableDataDetectors(enableDataDetectors: Boolean); cdecl;
    procedure setGreekingThreshold(greekingThreshold: CGFloat); cdecl;
    procedure setHighlightedSelections(highlightedSelections: NSArray); cdecl;
    procedure setInMarkupMode(inMarkupMode: Boolean); cdecl;
    procedure setInterpolationQuality(interpolationQuality: PDFInterpolationQuality); cdecl;
    procedure setMaxScaleFactor(maxScaleFactor: CGFloat); cdecl;
    procedure setMinScaleFactor(minScaleFactor: CGFloat); cdecl;
    procedure setPageBreakMargins(pageBreakMargins: NSEdgeInsets); cdecl;
    procedure setPageOverlayViewProvider(pageOverlayViewProvider: Pointer); cdecl;
    procedure setScaleFactor(scaleFactor: CGFloat); cdecl;
    procedure setShouldAntiAlias(shouldAntiAlias: Boolean); cdecl;
    function shouldAntiAlias: Boolean; cdecl;
    procedure takeBackgroundColorFrom(sender: Pointer); cdecl;
    procedure takePasswordFrom(sender: Pointer); cdecl;
    function visiblePages: NSArray; cdecl;
    procedure zoomIn(sender: Pointer); cdecl;
    procedure zoomOut(sender: Pointer); cdecl;
  end;
  TPDFView = class(TOCGenericImport<PDFViewClass, PDFView>) end;

  PDFViewDelegate = interface(IObjectiveC)
    ['{D119B289-9D2D-4D9A-AD11-8E63D6DF4D26}']
    procedure PDFViewOpenPDF(sender: PDFView; forRemoteGoToAction: PDFActionRemoteGoTo); cdecl;
    procedure PDFViewPerformFind(sender: PDFView); cdecl;
    procedure PDFViewPerformGoToPage(sender: PDFView); cdecl;
    procedure PDFViewPerformPrint(sender: PDFView); cdecl;
    function PDFViewPrintJobTitle(sender: PDFView): NSString; cdecl;
    function PDFViewWillChangeScaleFactor(sender: PDFView; toScale: CGFloat): CGFloat; cdecl;
    procedure PDFViewWillClickOnLink(sender: PDFView; withURL: NSURL); cdecl;
  end;

  PDFPageOverlayViewProvider = interface(IObjectiveC)
    ['{37A528BD-53D9-49E9-9BE1-FA1488F9AF6E}']
    [MethodName('pdfView:overlayViewForPage:')]
    function pdfViewOverlayViewForPage(view: PDFView; overlayViewForPage: PDFPage): NSView; cdecl;
    [MethodName('pdfView:willDisplayOverlayView:forPage:')]
    procedure pdfViewWillDisplayOverlayView(pdfView: PDFView; willDisplayOverlayView: NSView; forPage: PDFPage); cdecl;
    [MethodName('pdfView:willEndDisplayingOverlayView:forPage:')]
    procedure pdfViewWillEndDisplayingOverlayView(pdfView: PDFView; willEndDisplayingOverlayView: NSView; forPage: PDFPage); cdecl;
  end;

  PDFAnnotationButtonWidgetClass = interface(PDFAnnotationClass)
    ['{04B2A786-9DDF-4EDA-9657-8E4ADA3001AE}']
  end;

  PDFAnnotationButtonWidget = interface(PDFAnnotation)
    ['{BC38095A-EA09-4942-84C5-BD5F4A95084B}']
    function allowsToggleToOff: Boolean; cdecl;
    function backgroundColor: NSColor; cdecl;
    function caption: NSString; cdecl;
    function controlType: PDFWidgetControlType; cdecl;
    function fieldName: NSString; cdecl;
    function font: NSFont; cdecl;
    function fontColor: NSColor; cdecl;
    function onStateValue: NSString; cdecl;
    procedure setAllowsToggleToOff(allowOff: Boolean); cdecl;
    procedure setBackgroundColor(color: NSColor); cdecl;
    procedure setCaption(name: NSString); cdecl;
    procedure setControlType(&type: PDFWidgetControlType); cdecl;
    procedure setFieldName(name: NSString); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setFontColor(color: NSColor); cdecl;
    procedure setOnStateValue(name: NSString); cdecl;
    procedure setState(value: NSInteger); cdecl;
    function state: NSInteger; cdecl;
  end;
  TPDFAnnotationButtonWidget = class(TOCGenericImport<PDFAnnotationButtonWidgetClass, PDFAnnotationButtonWidget>) end;

  PDFAnnotationChoiceWidgetClass = interface(PDFAnnotationClass)
    ['{22612AEC-CDC2-4E97-ADFE-30B1589C6772}']
  end;

  PDFAnnotationChoiceWidget = interface(PDFAnnotation)
    ['{5709F979-2F71-4428-A228-D7420949DDFF}']
    function backgroundColor: NSColor; cdecl;
    function choices: NSArray; cdecl;
    function fieldName: NSString; cdecl;
    function font: NSFont; cdecl;
    function fontColor: NSColor; cdecl;
    function isListChoice: Boolean; cdecl;
    procedure setBackgroundColor(color: NSColor); cdecl;
    procedure setChoices(options: NSArray); cdecl;
    procedure setFieldName(name: NSString); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setFontColor(color: NSColor); cdecl;
    procedure setIsListChoice(isList: Boolean); cdecl;
    procedure setStringValue(value: NSString); cdecl;
    function stringValue: NSString; cdecl;
  end;
  TPDFAnnotationChoiceWidget = class(TOCGenericImport<PDFAnnotationChoiceWidgetClass, PDFAnnotationChoiceWidget>) end;

  PDFAnnotationCircleClass = interface(PDFAnnotationClass)
    ['{420FBBF2-5185-475D-AA66-60B78699CEFB}']
  end;

  PDFAnnotationCircle = interface(PDFAnnotation)
    ['{A093F9FE-D9D0-4C61-9ABF-F4E15771B712}']
    function interiorColor: NSColor; cdecl;
    procedure setInteriorColor(color: NSColor); cdecl;
  end;
  TPDFAnnotationCircle = class(TOCGenericImport<PDFAnnotationCircleClass, PDFAnnotationCircle>) end;

  PDFAnnotationFreeTextClass = interface(PDFAnnotationClass)
    ['{715F9FAE-1F2C-4632-B9CD-EC72C3C46792}']
  end;

  PDFAnnotationFreeText = interface(PDFAnnotation)
    ['{F35C0B88-C640-4E56-9FAD-E6133264F277}']
    function alignment: NSTextAlignment; cdecl;
    function font: NSFont; cdecl;
    function fontColor: NSColor; cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setFontColor(color: NSColor); cdecl;
  end;
  TPDFAnnotationFreeText = class(TOCGenericImport<PDFAnnotationFreeTextClass, PDFAnnotationFreeText>) end;

  PDFAnnotationInkClass = interface(PDFAnnotationClass)
    ['{968C3397-096A-4B7F-99FC-4200F2CBAC80}']
  end;

  PDFAnnotationInk = interface(PDFAnnotation)
    ['{E176821B-5F19-456D-9E4A-62EF53CC2566}']
    procedure addBezierPath(path: NSBezierPath); cdecl;
    function paths: NSArray; cdecl;
    procedure removeBezierPath(path: NSBezierPath); cdecl;
  end;
  TPDFAnnotationInk = class(TOCGenericImport<PDFAnnotationInkClass, PDFAnnotationInk>) end;

  PDFAnnotationLineClass = interface(PDFAnnotationClass)
    ['{DB5838D4-22AC-4C9E-BD68-E1432E114F2E}']
  end;

  PDFAnnotationLine = interface(PDFAnnotation)
    ['{1DD9A741-EEDD-4502-B0ED-441EEE104D90}']
    function endLineStyle: PDFLineStyle; cdecl;
    function endPoint: NSPoint; cdecl;
    function interiorColor: NSColor; cdecl;
    procedure setEndLineStyle(style: PDFLineStyle); cdecl;
    procedure setEndPoint(point: NSPoint); cdecl;
    procedure setInteriorColor(color: NSColor); cdecl;
    procedure setStartLineStyle(style: PDFLineStyle); cdecl;
    procedure setStartPoint(point: NSPoint); cdecl;
    function startLineStyle: PDFLineStyle; cdecl;
    function startPoint: NSPoint; cdecl;
  end;
  TPDFAnnotationLine = class(TOCGenericImport<PDFAnnotationLineClass, PDFAnnotationLine>) end;

  PDFAnnotationLinkClass = interface(PDFAnnotationClass)
    ['{9C5F8E47-A322-4813-8928-B015FF2D30C3}']
  end;

  PDFAnnotationLink = interface(PDFAnnotation)
    ['{652627D7-06E1-4E35-90F9-BD387CE7DFFA}']
    function destination: PDFDestination; cdecl;
    procedure setDestination(destination: PDFDestination); cdecl;
    procedure setURL(url: NSURL); cdecl;
    function URL: NSURL; cdecl;
  end;
  TPDFAnnotationLink = class(TOCGenericImport<PDFAnnotationLinkClass, PDFAnnotationLink>) end;

  PDFAnnotationMarkupClass = interface(PDFAnnotationClass)
    ['{F2148F9B-4472-469C-97EC-51D02A43595A}']
  end;

  PDFAnnotationMarkup = interface(PDFAnnotation)
    ['{0F2F9E50-8B61-4BBE-90D3-E6167083809A}']
    function markupType: PDFMarkupType; cdecl;
    function quadrilateralPoints: NSArray; cdecl;
    procedure setMarkupType(&type: PDFMarkupType); cdecl;
    procedure setQuadrilateralPoints(points: NSArray); cdecl;
  end;
  TPDFAnnotationMarkup = class(TOCGenericImport<PDFAnnotationMarkupClass, PDFAnnotationMarkup>) end;

  PDFAnnotationPopupClass = interface(PDFAnnotationClass)
    ['{B2249588-E785-4E1E-949C-97966DB5B3B8}']
  end;

  PDFAnnotationPopup = interface(PDFAnnotation)
    ['{D4D40003-C196-4BD5-8A54-8542F52B7E72}']
    function isOpen: Boolean; cdecl;
    procedure setIsOpen(isOpen: Boolean); cdecl;
  end;
  TPDFAnnotationPopup = class(TOCGenericImport<PDFAnnotationPopupClass, PDFAnnotationPopup>) end;

  PDFAnnotationSquareClass = interface(PDFAnnotationClass)
    ['{08E2F192-38E5-4EFA-955B-DAB08279617F}']
  end;

  PDFAnnotationSquare = interface(PDFAnnotation)
    ['{12D379FD-1059-492F-95DB-EC63F10D9817}']
    function interiorColor: NSColor; cdecl;
    procedure setInteriorColor(color: NSColor); cdecl;
  end;
  TPDFAnnotationSquare = class(TOCGenericImport<PDFAnnotationSquareClass, PDFAnnotationSquare>) end;

  PDFAnnotationStampClass = interface(PDFAnnotationClass)
    ['{274C4378-1053-4070-AA6D-7E6442711E6B}']
  end;

  PDFAnnotationStamp = interface(PDFAnnotation)
    ['{F97BF2D5-B508-495C-BA37-A02E29543BEB}']
    function name: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
  end;
  TPDFAnnotationStamp = class(TOCGenericImport<PDFAnnotationStampClass, PDFAnnotationStamp>) end;

  PDFAnnotationTextClass = interface(PDFAnnotationClass)
    ['{FA6AE1B3-EB49-4245-8540-036AAF491DDF}']
  end;

  PDFAnnotationText = interface(PDFAnnotation)
    ['{267ECB78-51F6-4E08-872B-DA241B1EA829}']
    function iconType: PDFTextAnnotationIconType; cdecl;
    procedure setIconType(&type: PDFTextAnnotationIconType); cdecl;
  end;
  TPDFAnnotationText = class(TOCGenericImport<PDFAnnotationTextClass, PDFAnnotationText>) end;

  PDFAnnotationTextWidgetClass = interface(PDFAnnotationClass)
    ['{E995F1FE-6430-4742-AF99-E6F0A0160390}']
  end;

  PDFAnnotationTextWidget = interface(PDFAnnotation)
    ['{36B3C8BB-211E-4828-84DB-4F23C9675A98}']
    function alignment: NSTextAlignment; cdecl;
    function attributedStringValue: NSAttributedString; cdecl;
    function backgroundColor: NSColor; cdecl;
    function fieldName: NSString; cdecl;
    function font: NSFont; cdecl;
    function fontColor: NSColor; cdecl;
    function isMultiline: Boolean; cdecl;
    function maximumLength: NSUInteger; cdecl;
    function rotation: NSInteger; cdecl;
    procedure setAlignment(alignment: NSTextAlignment); cdecl;
    procedure setAttributedStringValue(value: NSAttributedString); cdecl;
    procedure setBackgroundColor(color: NSColor); cdecl;
    procedure setFieldName(name: NSString); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setFontColor(color: NSColor); cdecl;
    procedure setIsMultiline(multiline: Boolean); cdecl;
    procedure setMaximumLength(maxLen: NSUInteger); cdecl;
    procedure setRotation(rotation: Integer); cdecl;
    procedure setStringValue(value: NSString); cdecl;
    function stringValue: NSString; cdecl;
  end;
  TPDFAnnotationTextWidget = class(TOCGenericImport<PDFAnnotationTextWidgetClass, PDFAnnotationTextWidget>) end;

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
function kPDFAnnotationKey_AppearanceDictionary: PDFAnnotationKey;
function kPDFAnnotationKey_AppearanceState: PDFAnnotationKey;
function kPDFAnnotationKey_Border: PDFAnnotationKey;
function kPDFAnnotationKey_Color: PDFAnnotationKey;
function kPDFAnnotationKey_Contents: PDFAnnotationKey;
function kPDFAnnotationKey_Flags: PDFAnnotationKey;
function kPDFAnnotationKey_Date: PDFAnnotationKey;
function kPDFAnnotationKey_Name: PDFAnnotationKey;
function kPDFAnnotationKey_Page: PDFAnnotationKey;
function kPDFAnnotationKey_Rect: PDFAnnotationKey;
function kPDFAnnotationKey_Subtype: PDFAnnotationKey;
function kPDFAnnotationKey_Action: PDFAnnotationKey;
function kPDFAnnotationKey_AdditionalActions: PDFAnnotationKey;
function kPDFAnnotationKey_BorderStyle: PDFAnnotationKey;
function kPDFAnnotationKey_DefaultAppearance: PDFAnnotationKey;
function kPDFAnnotationKey_Destination: PDFAnnotationKey;
function kPDFAnnotationKey_HighlightingMode: PDFAnnotationKey;
function kPDFAnnotationKey_Inklist: PDFAnnotationKey;
function kPDFAnnotationKey_InteriorColor: PDFAnnotationKey;
function kPDFAnnotationKey_LinePoints: PDFAnnotationKey;
function kPDFAnnotationKey_LineEndingStyles: PDFAnnotationKey;
function kPDFAnnotationKey_IconName: PDFAnnotationKey;
function kPDFAnnotationKey_Open: PDFAnnotationKey;
function kPDFAnnotationKey_Parent: PDFAnnotationKey;
function kPDFAnnotationKey_Popup: PDFAnnotationKey;
function kPDFAnnotationKey_Quadding: PDFAnnotationKey;
function kPDFAnnotationKey_QuadPoints: PDFAnnotationKey;
function kPDFAnnotationKey_TextLabel: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetDefaultValue: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetFieldFlags: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetFieldType: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetAppearanceDictionary: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetMaxLen: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetOptions: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetTextLabelUI: PDFAnnotationKey;
function kPDFAnnotationKey_WidgetValue: PDFAnnotationKey;
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
function kPDFDestinationUnspecifiedValue: CGFloat;
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
  System.SysUtils;

var
  PDFKitModule: THandle;

function PDFPageImageInitializationOptionMediaBox: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionMediaBox');
end;

function PDFPageImageInitializationOptionRotation: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionRotation');
end;

function PDFPageImageInitializationOptionUpscaleIfSmaller: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionUpscaleIfSmaller');
end;

function PDFPageImageInitializationOptionCompressionQuality: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFPageImageInitializationOptionCompressionQuality');
end;

function PDFAnnotationKeyAppearanceDictionary: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAppearanceDictionary');
end;

function PDFAnnotationKeyAppearanceState: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAppearanceState');
end;

function PDFAnnotationKeyBorder: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyBorder');
end;

function PDFAnnotationKeyColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyColor');
end;

function PDFAnnotationKeyContents: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyContents');
end;

function PDFAnnotationKeyFlags: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyFlags');
end;

function PDFAnnotationKeyDate: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDate');
end;

function PDFAnnotationKeyName: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyName');
end;

function PDFAnnotationKeyPage: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyPage');
end;

function PDFAnnotationKeyRect: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyRect');
end;

function PDFAnnotationKeySubtype: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeySubtype');
end;

function PDFAnnotationKeyAction: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAction');
end;

function PDFAnnotationKeyAdditionalActions: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyAdditionalActions');
end;

function PDFAnnotationKeyBorderStyle: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyBorderStyle');
end;

function PDFAnnotationKeyDefaultAppearance: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDefaultAppearance');
end;

function PDFAnnotationKeyDestination: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyDestination');
end;

function PDFAnnotationKeyHighlightingMode: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyHighlightingMode');
end;

function PDFAnnotationKeyInklist: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyInklist');
end;

function PDFAnnotationKeyInteriorColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyInteriorColor');
end;

function PDFAnnotationKeyLinePoints: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyLinePoints');
end;

function PDFAnnotationKeyLineEndingStyles: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyLineEndingStyles');
end;

function PDFAnnotationKeyIconName: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyIconName');
end;

function PDFAnnotationKeyOpen: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyOpen');
end;

function PDFAnnotationKeyParent: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyParent');
end;

function PDFAnnotationKeyPopup: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyPopup');
end;

function PDFAnnotationKeyQuadding: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyQuadding');
end;

function PDFAnnotationKeyQuadPoints: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyQuadPoints');
end;

function PDFAnnotationKeyTextLabel: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyTextLabel');
end;

function PDFAnnotationKeyWidgetDownCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetDownCaption');
end;

function PDFAnnotationKeyWidgetBorderColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetBorderColor');
end;

function PDFAnnotationKeyWidgetBackgroundColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetBackgroundColor');
end;

function PDFAnnotationKeyWidgetCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetCaption');
end;

function PDFAnnotationKeyWidgetDefaultValue: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetDefaultValue');
end;

function PDFAnnotationKeyWidgetFieldFlags: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetFieldFlags');
end;

function PDFAnnotationKeyWidgetFieldType: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetFieldType');
end;

function PDFAnnotationKeyWidgetAppearanceDictionary: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetAppearanceDictionary');
end;

function PDFAnnotationKeyWidgetMaxLen: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetMaxLen');
end;

function PDFAnnotationKeyWidgetOptions: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetOptions');
end;

function PDFAnnotationKeyWidgetRotation: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetRotation');
end;

function PDFAnnotationKeyWidgetRolloverCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetRolloverCaption');
end;

function PDFAnnotationKeyWidgetTextLabelUI: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetTextLabelUI');
end;

function PDFAnnotationKeyWidgetValue: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationKeyWidgetValue');
end;

function kPDFAnnotationKey_AppearanceDictionary: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_AppearanceDictionary');
end;

function kPDFAnnotationKey_AppearanceState: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_AppearanceState');
end;

function kPDFAnnotationKey_Border: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Border');
end;

function kPDFAnnotationKey_Color: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Color');
end;

function kPDFAnnotationKey_Contents: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Contents');
end;

function kPDFAnnotationKey_Flags: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Flags');
end;

function kPDFAnnotationKey_Date: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Date');
end;

function kPDFAnnotationKey_Name: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Name');
end;

function kPDFAnnotationKey_Page: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Page');
end;

function kPDFAnnotationKey_Rect: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Rect');
end;

function kPDFAnnotationKey_Subtype: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Subtype');
end;

function kPDFAnnotationKey_Action: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Action');
end;

function kPDFAnnotationKey_AdditionalActions: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_AdditionalActions');
end;

function kPDFAnnotationKey_BorderStyle: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_BorderStyle');
end;

function kPDFAnnotationKey_DefaultAppearance: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_DefaultAppearance');
end;

function kPDFAnnotationKey_Destination: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Destination');
end;

function kPDFAnnotationKey_HighlightingMode: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_HighlightingMode');
end;

function kPDFAnnotationKey_Inklist: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Inklist');
end;

function kPDFAnnotationKey_InteriorColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_InteriorColor');
end;

function kPDFAnnotationKey_LinePoints: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_LinePoints');
end;

function kPDFAnnotationKey_LineEndingStyles: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_LineEndingStyles');
end;

function kPDFAnnotationKey_IconName: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_IconName');
end;

function kPDFAnnotationKey_Open: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Open');
end;

function kPDFAnnotationKey_Parent: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Parent');
end;

function kPDFAnnotationKey_Popup: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Popup');
end;

function kPDFAnnotationKey_Quadding: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_Quadding');
end;

function kPDFAnnotationKey_QuadPoints: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_QuadPoints');
end;

function kPDFAnnotationKey_TextLabel: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_TextLabel');
end;

function kPDFAnnotationKey_WidgetDefaultValue: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetDefaultValue');
end;

function kPDFAnnotationKey_WidgetFieldFlags: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetFieldFlags');
end;

function kPDFAnnotationKey_WidgetFieldType: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetFieldType');
end;

function kPDFAnnotationKey_WidgetAppearanceDictionary: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetAppearanceDictionary');
end;

function kPDFAnnotationKey_WidgetMaxLen: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetMaxLen');
end;

function kPDFAnnotationKey_WidgetOptions: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetOptions');
end;

function kPDFAnnotationKey_WidgetTextLabelUI: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetTextLabelUI');
end;

function kPDFAnnotationKey_WidgetValue: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'kPDFAnnotationKey_WidgetValue');
end;

function PDFAnnotationSubtypeText: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeText');
end;

function PDFAnnotationSubtypeLink: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeLink');
end;

function PDFAnnotationSubtypeFreeText: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeFreeText');
end;

function PDFAnnotationSubtypeLine: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeLine');
end;

function PDFAnnotationSubtypeSquare: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeSquare');
end;

function PDFAnnotationSubtypeCircle: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeCircle');
end;

function PDFAnnotationSubtypeHighlight: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeHighlight');
end;

function PDFAnnotationSubtypeUnderline: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeUnderline');
end;

function PDFAnnotationSubtypeStrikeOut: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeStrikeOut');
end;

function PDFAnnotationSubtypeInk: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeInk');
end;

function PDFAnnotationSubtypeStamp: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeStamp');
end;

function PDFAnnotationSubtypePopup: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypePopup');
end;

function PDFAnnotationSubtypeWidget: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationSubtypeWidget');
end;

function PDFAnnotationWidgetSubtypeButton: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeButton');
end;

function PDFAnnotationWidgetSubtypeChoice: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeChoice');
end;

function PDFAnnotationWidgetSubtypeSignature: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeSignature');
end;

function PDFAnnotationWidgetSubtypeText: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationWidgetSubtypeText');
end;

function PDFAnnotationLineEndingStyleNone: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleNone');
end;

function PDFAnnotationLineEndingStyleSquare: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleSquare');
end;

function PDFAnnotationLineEndingStyleCircle: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleCircle');
end;

function PDFAnnotationLineEndingStyleDiamond: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleDiamond');
end;

function PDFAnnotationLineEndingStyleOpenArrow: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleOpenArrow');
end;

function PDFAnnotationLineEndingStyleClosedArrow: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationLineEndingStyleClosedArrow');
end;

function PDFAnnotationTextIconTypeComment: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeComment');
end;

function PDFAnnotationTextIconTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeKey');
end;

function PDFAnnotationTextIconTypeNote: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeNote');
end;

function PDFAnnotationTextIconTypeHelp: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeHelp');
end;

function PDFAnnotationTextIconTypeNewParagraph: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeNewParagraph');
end;

function PDFAnnotationTextIconTypeParagraph: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeParagraph');
end;

function PDFAnnotationTextIconTypeInsert: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationTextIconTypeInsert');
end;

function PDFAnnotationHighlightingModeNone: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeNone');
end;

function PDFAnnotationHighlightingModeInvert: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeInvert');
end;

function PDFAnnotationHighlightingModeOutline: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModeOutline');
end;

function PDFAnnotationHighlightingModePush: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAnnotationHighlightingModePush');
end;

function PDFAppearanceCharacteristicsKeyBackgroundColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyBackgroundColor');
end;

function PDFAppearanceCharacteristicsKeyBorderColor: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyBorderColor');
end;

function PDFAppearanceCharacteristicsKeyRotation: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyRotation');
end;

function PDFAppearanceCharacteristicsKeyCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyCaption');
end;

function PDFAppearanceCharacteristicsKeyRolloverCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyRolloverCaption');
end;

function PDFAppearanceCharacteristicsKeyDownCaption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFAppearanceCharacteristicsKeyDownCaption');
end;

function PDFBorderKeyLineWidth: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyLineWidth');
end;

function PDFBorderKeyStyle: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyStyle');
end;

function PDFBorderKeyDashPattern: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFBorderKeyDashPattern');
end;

function kPDFDestinationUnspecifiedValue: CGFloat;
begin
  Result := CocoaDoubleConst(libPDFKit, 'kPDFDestinationUnspecifiedValue');
end;

function PDFDocumentDidUnlockNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidUnlockNotification');
end;

function PDFDocumentDidBeginFindNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginFindNotification');
end;

function PDFDocumentDidEndFindNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndFindNotification');
end;

function PDFDocumentDidBeginPageFindNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginPageFindNotification');
end;

function PDFDocumentDidEndPageFindNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndPageFindNotification');
end;

function PDFDocumentDidFindMatchNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidFindMatchNotification');
end;

function PDFDocumentDidBeginWriteNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginWriteNotification');
end;

function PDFDocumentDidEndWriteNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidEndWriteNotification');
end;

function PDFDocumentDidBeginPageWriteNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentDidBeginPageWriteNotification');
end;

function PDFDocumentDidEndPageWriteNotification: NSString;
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

function PDFDocumentTitleAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentTitleAttribute');
end;

function PDFDocumentAuthorAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentAuthorAttribute');
end;

function PDFDocumentSubjectAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSubjectAttribute');
end;

function PDFDocumentCreatorAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentCreatorAttribute');
end;

function PDFDocumentProducerAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentProducerAttribute');
end;

function PDFDocumentCreationDateAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentCreationDateAttribute');
end;

function PDFDocumentModificationDateAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentModificationDateAttribute');
end;

function PDFDocumentKeywordsAttribute: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentKeywordsAttribute');
end;

function PDFDocumentOwnerPasswordOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentOwnerPasswordOption');
end;

function PDFDocumentUserPasswordOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentUserPasswordOption');
end;

function PDFDocumentAccessPermissionsOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentAccessPermissionsOption');
end;

function PDFDocumentBurnInAnnotationsOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentBurnInAnnotationsOption');
end;

function PDFDocumentSaveTextFromOCROption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSaveTextFromOCROption');
end;

function PDFDocumentSaveImagesAsJPEGOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentSaveImagesAsJPEGOption');
end;

function PDFDocumentOptimizeImagesForScreenOption: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFDocumentOptimizeImagesForScreenOption');
end;

function PDFThumbnailViewDocumentEditedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFThumbnailViewDocumentEditedNotification');
end;

function PDFViewDocumentChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDocumentChangedNotification');
end;

function PDFViewChangedHistoryNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewChangedHistoryNotification');
end;

function PDFViewPageChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewPageChangedNotification');
end;

function PDFViewScaleChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewScaleChangedNotification');
end;

function PDFViewAnnotationHitNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewAnnotationHitNotification');
end;

function PDFViewCopyPermissionNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewCopyPermissionNotification');
end;

function PDFViewPrintPermissionNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewPrintPermissionNotification');
end;

function PDFViewAnnotationWillHitNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewAnnotationWillHitNotification');
end;

function PDFViewSelectionChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewSelectionChangedNotification');
end;

function PDFViewDisplayModeChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDisplayModeChangedNotification');
end;

function PDFViewDisplayBoxChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewDisplayBoxChangedNotification');
end;

function PDFViewVisiblePagesChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libPDFKit, 'PDFViewVisiblePagesChangedNotification');
end;

initialization
  PDFKitModule := LoadLibrary(libPDFKit);

finalization
  if PDFKitModule <> 0 then
    FreeLibrary(PDFKitModule);

end.