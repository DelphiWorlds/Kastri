unit DW.iOSapi.CarPlay;

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

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  {$IF CompilerVersion < 37} DW.iOSapi.UIKit, {$ENDIF}
  DW.iOSapi.Foundation, iOSapi.MapKit;

const
  CPAlertActionStyleDefault = 0;
  CPAlertActionStyleCancel = 1;
  CPAlertActionStyleDestructive = 2;
  CPBarButtonStyleNone = 0;
  CPBarButtonStyleRounded = 1;
  CPBarButtonTypeText = 0;
  CPBarButtonTypeImage = 1;
  CPTextButtonStyleNormal = 0;
  CPTextButtonStyleCancel = 1;
  CPTextButtonStyleConfirm = 2;
  CPInformationTemplateLayoutLeading = 0;
  CPInformationTemplateLayoutTwoColumn = 1;
  CPInstrumentClusterSettingUnspecified = 0;
  CPInstrumentClusterSettingEnabled = 1;
  CPInstrumentClusterSettingDisabled = 2;
  CPInstrumentClusterSettingUserPreference = 3;
  CPListItemAccessoryTypeNone = 0;
  CPListItemAccessoryTypeDisclosureIndicator = 1;
  CPListItemAccessoryTypeCloud = 2;
  CPListItemPlayingIndicatorLocationLeading = 0;
  CPListItemPlayingIndicatorLocationTrailing = 1;
  CPAssistantCellActionTypePlayMedia = 0;
  CPAssistantCellActionTypeStartCall = 1;
  CPAssistantCellVisibilityOff = 0;
  CPAssistantCellVisibilityWhileLimitedUIActive = 1;
  CPAssistantCellVisibilityAlways = 2;
  CPAssistantCellPositionTop = 0;
  CPAssistantCellPositionBottom = 1;
  CPLaneStatusNotGood = 0;
  CPLaneStatusGood = 1;
  CPLaneStatusPreferred = 2;
  CPManeuverTypeNoTurn = 0;
  CPManeuverTypeLeftTurn = 1;
  CPManeuverTypeRightTurn = 2;
  CPManeuverTypeStraightAhead = 3;
  CPManeuverTypeUTurn = 4;
  CPManeuverTypeFollowRoad = 5;
  CPManeuverTypeEnterRoundabout = 6;
  CPManeuverTypeExitRoundabout = 7;
  CPManeuverTypeOffRamp = 8;
  CPManeuverTypeOnRamp = 9;
  CPManeuverTypeArriveEndOfNavigation = 10;
  CPManeuverTypeStartRoute = 11;
  CPManeuverTypeArriveAtDestination = 12;
  CPManeuverTypeKeepLeft = 13;
  CPManeuverTypeKeepRight = 14;
  CPManeuverTypeEnter_Ferry = 15;
  CPManeuverTypeExitFerry = 16;
  CPManeuverTypeChangeFerry = 17;
  CPManeuverTypeStartRouteWithUTurn = 18;
  CPManeuverTypeUTurnAtRoundabout = 19;
  CPManeuverTypeLeftTurnAtEnd = 20;
  CPManeuverTypeRightTurnAtEnd = 21;
  CPManeuverTypeHighwayOffRampLeft = 22;
  CPManeuverTypeHighwayOffRampRight = 23;
  CPManeuverTypeArriveAtDestinationLeft = 24;
  CPManeuverTypeArriveAtDestinationRight = 25;
  CPManeuverTypeUTurnWhenPossible = 26;
  CPManeuverTypeArriveEndOfDirections = 27;
  CPManeuverTypeRoundaboutExit1 = 28;
  CPManeuverTypeRoundaboutExit2 = 29;
  CPManeuverTypeRoundaboutExit3 = 30;
  CPManeuverTypeRoundaboutExit4 = 31;
  CPManeuverTypeRoundaboutExit5 = 32;
  CPManeuverTypeRoundaboutExit6 = 33;
  CPManeuverTypeRoundaboutExit7 = 34;
  CPManeuverTypeRoundaboutExit8 = 35;
  CPManeuverTypeRoundaboutExit9 = 36;
  CPManeuverTypeRoundaboutExit10 = 37;
  CPManeuverTypeRoundaboutExit11 = 38;
  CPManeuverTypeRoundaboutExit12 = 39;
  CPManeuverTypeRoundaboutExit13 = 40;
  CPManeuverTypeRoundaboutExit14 = 41;
  CPManeuverTypeRoundaboutExit15 = 42;
  CPManeuverTypeRoundaboutExit16 = 43;
  CPManeuverTypeRoundaboutExit17 = 44;
  CPManeuverTypeRoundaboutExit18 = 45;
  CPManeuverTypeRoundaboutExit19 = 46;
  CPManeuverTypeSharpLeftTurn = 47;
  CPManeuverTypeSharpRightTurn = 48;
  CPManeuverTypeSlightLeftTurn = 49;
  CPManeuverTypeSlightRightTurn = 50;
  CPManeuverTypeChangeHighway = 51;
  CPManeuverTypeChangeHighwayLeft = 52;
  CPManeuverTypeChangeHighwayRight = 53;
  CPJunctionTypeIntersection = 0;
  CPJunctionTypeRoundabout = 1;
  CPTrafficSideRight = 0;
  CPTrafficSideLeft = 1;
  CPManeuverStateContinue = 0;
  CPManeuverStateInitial = 1;
  CPManeuverStatePrepare = 2;
  CPManeuverStateExecute = 3;
  CPNavigationAlertDismissalContextTimeout = 0;
  CPNavigationAlertDismissalContextUserDismissed = 1;
  CPNavigationAlertDismissalContextSystemDismissed = 2;
  CPTripPauseReasonArrived = 1;
  CPTripPauseReasonLoading = 2;
  CPTripPauseReasonLocating = 3;
  CPTripPauseReasonRerouting = 4;
  CPTripPauseReasonProceedToRoute = 5;
  CPPanDirectionNone = 0;
  CPPanDirectionLeft = 1;
  CPPanDirectionRight = 2;
  CPPanDirectionUp = 4;
  CPPanDirectionDown = 8;
  CPManeuverDisplayStyleDefault = 0;
  CPManeuverDisplayStyleLeadingSymbol = 1;
  CPManeuverDisplayStyleTrailingSymbol = 2;
  CPManeuverDisplayStyleSymbolOnly = 3;
  CPManeuverDisplayStyleInstructionOnly = 4;
  CPTimeRemainingColorDefault = 0;
  CPTimeRemainingColorGreen = 1;
  CPTimeRemainingColorOrange = 2;
  CPTimeRemainingColorRed = 3;
  CPTripEstimateStyleLight = 0;
  CPTripEstimateStyleDark = 1;
  CPMessageLeadingItemNone = 0;
  CPMessageLeadingItemPin = 1;
  CPMessageLeadingItemStar = 2;
  CPMessageTrailingItemNone = 0;
  CPMessageTrailingItemMute = 1;
  CPLimitableUserInterfaceKeyboard = 1;
  CPLimitableUserInterfaceLists = 2;
  CPContentStyleLight = 1;
  CPContentStyleDark = 2;

type
  CPAlertAction = interface;
  CPTemplate = interface;
  CPActionSheetTemplate = interface;
  CPAlertTemplate = interface;
  CPBarButton = interface;
  CPBarButtonProviding = interface;
  CPButton = interface;
  CPContact = interface;
  CPContactCallButton = interface;
  CPContactMessageButton = interface;
  CPContactDirectionsButton = interface;
  CPContactTemplate = interface;
  CPDashboardButton = interface;
  CPDashboardController = interface;
  CPGridButton = interface;
  CPGridTemplate = interface;
  CPImageSet = interface;
  CPTextButton = interface;
  CPInformationItem = interface;
  CPInformationRatingItem = interface;
  CPInformationTemplate = interface;
  CPWindow = interface;
  CPInterfaceController = interface;
  CPInterfaceControllerDelegate = interface;
  CPApplicationDelegate = interface;
  CPInstrumentClusterController = interface;
  CPInstrumentClusterControllerDelegate = interface;
  CPListTemplateItem = interface;
  CPSelectableListItem = interface;
  CPListSection = interface;
  CPListImageRowItem = interface;
  CPListItem = interface;
  CPAssistantCellConfiguration = interface;
  CPListTemplate = interface;
  CPListTemplateDelegate = interface;
  CPLane = interface;
  CPLaneGuidance = interface;
  CPTravelEstimates = interface;
  CPManeuver = interface;
  CPMapButton = interface;
  CPNavigationAlert = interface;
  CPRouteInformation = interface;
  CPRouteChoice = interface;
  CPTrip = interface;
  CPNavigationSession = interface;
  CPTripPreviewTextConfiguration = interface;
  CPMapTemplate = interface;
  CPMapTemplateDelegate = interface;
  CPMessageComposeBarButton = interface;
  CPMessageListItemLeadingConfiguration = interface;
  CPMessageListItemTrailingConfiguration = interface;
  CPMessageListItem = interface;
  CPNowPlayingButton = interface;
  CPNowPlayingShuffleButton = interface;
  CPNowPlayingAddToLibraryButton = interface;
  CPNowPlayingMoreButton = interface;
  CPNowPlayingPlaybackRateButton = interface;
  CPNowPlayingRepeatButton = interface;
  CPNowPlayingImageButton = interface;
  CPNowPlayingTemplateObserver = interface;
  CPNowPlayingTemplate = interface;
  CPPointOfInterest = interface;
  CPPointOfInterestTemplateDelegate = interface;
  CPPointOfInterestTemplate = interface;
  CPSearchTemplate = interface;
  CPSearchTemplateDelegate = interface;
  CPSessionConfiguration = interface;
  CPSessionConfigurationDelegate = interface;
  CPTabBarTemplate = interface;
  CPTabBarTemplateDelegate = interface;
  CPTemplateApplicationDashboardSceneDelegate = interface;
  CPTemplateApplicationDashboardScene = interface;
  CPTemplateApplicationInstrumentClusterSceneDelegate = interface;
  CPTemplateApplicationInstrumentClusterScene = interface;
  CPTemplateApplicationSceneDelegate = interface;
  CPTemplateApplicationScene = interface;
  CPVoiceControlState = interface;
  CPVoiceControlTemplate = interface;

  CPAlertActionStyle = NSInteger;

  CPAlertActionHandler = procedure(p1: CPAlertAction) of object;
  CPBarButtonStyle = NSInteger;
  CPBarButtonType = NSInteger;

  CPBarButtonHandler = procedure(p1: CPBarButton) of object;
  CPTextButtonStyle = NSInteger;
  CPInformationTemplateLayout = NSInteger;
  CPInstrumentClusterSetting = NSInteger;
  CPListItemAccessoryType = NSInteger;
  CPListItemPlayingIndicatorLocation = NSInteger;
  CPAssistantCellActionType = NSInteger;
  CPAssistantCellVisibility = NSInteger;
  CPAssistantCellPosition = NSInteger;
  CPLaneStatus = NSInteger;
  CPManeuverType = NSInteger;
  CPJunctionType = NSInteger;
  CPTrafficSide = NSInteger;
  CPManeuverState = NSInteger;
  CPNavigationAlertDismissalContext = NSInteger;
  CPTripPauseReason = NSInteger;
  CPPanDirection = NSInteger;
  CPManeuverDisplayStyle = NSInteger;
  CPTimeRemainingColor = NSInteger;
  CPTripEstimateStyle = NSInteger;
  CPMessageLeadingItem = NSInteger;
  CPMessageTrailingItem = NSInteger;
  CPLimitableUserInterface = NSInteger;
  CPContentStyle = NSInteger;
  TCPButtonBlockMethod1 = procedure(button: CPButton) of object;
  TCPContactCallButtonBlockMethod1 = procedure(contactButton: CPButton) of object;
  TCPContactDirectionsButtonBlockMethod1 = procedure(contactButton: CPButton) of object;
  TCPDashboardButtonBlockMethod1 = procedure(barButton: CPDashboardButton) of object;
  TCPGridButtonBlockMethod1 = procedure(barButton: CPGridButton) of object;
  TCPTextButtonBlockMethod1 = procedure(contactButton: CPTextButton) of object;
  TCPInterfaceControllerBlockMethod1 = procedure(success: Boolean; error: NSError) of object;
  TCPSelectableListItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPSelectableListItemBlockMethod2 = procedure of object;
  TCPListImageRowItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPListImageRowItemBlockMethod2 = procedure of object;
  TCPListImageRowItemBlockMethod3 = procedure(param1: CPListImageRowItem; param2: NSInteger; param3: dispatch_block_t) of object;
  TCPListItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPListItemBlockMethod2 = procedure of object;
  TCPListTemplateDelegateBlockMethod1 = procedure of object;
  TCPMapButtonBlockMethod1 = procedure(mapButton: CPMapButton) of object;
  TCPMapTemplateBlockMethod1 = procedure(dismissed: Boolean) of object;
  TCPNowPlayingButtonBlockMethod1 = procedure(param1: CPNowPlayingButton) of object;
  TCPNowPlayingImageButtonBlockMethod1 = procedure(param1: CPNowPlayingButton) of object;
  TCPSearchTemplateDelegateBlockMethod1 = procedure(searchResults: NSArray) of object;
  TCPSearchTemplateDelegateBlockMethod2 = procedure of object;

  CPAlertActionClass = interface(NSObjectClass)
    ['{310CC77B-781D-4114-BC5B-72116AD0A94F}']
    {class} function new: Pointer; cdecl;
  end;

  CPAlertAction = interface(NSObject)
    ['{5FBC7830-EC72-421A-807D-56576DFC3FD3}']
    function color: UIColor; cdecl;
    function handler: CPAlertActionHandler; cdecl;
    function initWithTitle(title: NSString; style: CPAlertActionStyle; handler: CPAlertActionHandler): Pointer; overload; cdecl;
    function initWithTitle(title: NSString; color: UIColor; handler: CPAlertActionHandler): Pointer; overload; cdecl;
    function style: CPAlertActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TCPAlertAction = class(TOCGenericImport<CPAlertActionClass, CPAlertAction>) end;

  CPTemplateClass = interface(NSObjectClass)
    ['{579A9E38-D209-4DE3-AF93-8372ACC9C163}']
  end;

  CPTemplate = interface(NSObject)
    ['{A256FEF4-2C8E-4712-B1F8-75C92711571B}']
    procedure setShowsTabBadge(showsTabBadge: Boolean); cdecl;
    procedure setTabImage(tabImage: UIImage); cdecl;
    procedure setTabSystemItem(tabSystemItem: UITabBarSystemItem); cdecl;
    procedure setTabTitle(tabTitle: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function showsTabBadge: Boolean; cdecl;
    function tabImage: UIImage; cdecl;
    function tabSystemItem: UITabBarSystemItem; cdecl;
    function tabTitle: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPTemplate = class(TOCGenericImport<CPTemplateClass, CPTemplate>) end;

  CPActionSheetTemplateClass = interface(CPTemplateClass)
    ['{6C2F5D91-63AB-4BF5-94E5-39FE74DD781C}']
    {class} function new: Pointer; cdecl;
  end;

  CPActionSheetTemplate = interface(CPTemplate)
    ['{2E5AF3BB-A6D6-4AAB-86BE-CAAE64174835}']
    function actions: NSArray; cdecl;
    function initWithTitle(title: NSString; message: NSString; actions: NSArray): Pointer; cdecl;
    function message: NSString; cdecl;
    function title: NSString; cdecl;
  end;
  TCPActionSheetTemplate = class(TOCGenericImport<CPActionSheetTemplateClass, CPActionSheetTemplate>) end;

  CPAlertTemplateClass = interface(CPTemplateClass)
    ['{64295082-384E-46C8-BFCF-BF0815D4D8AC}']
    {class} function maximumActionCount: NSUInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPAlertTemplate = interface(CPTemplate)
    ['{F00A4B40-0DC6-46FF-8A6A-7B487DE28D27}']
    function actions: NSArray; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; actions: NSArray): Pointer; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPAlertTemplate = class(TOCGenericImport<CPAlertTemplateClass, CPAlertTemplate>) end;

  CPBarButtonClass = interface(NSObjectClass)
    ['{ACFEC784-B14B-4D13-B06E-27FE0361FED6}']
    {class} function new: Pointer; cdecl;
  end;

  CPBarButton = interface(NSObject)
    ['{8929193F-1E91-4589-ACB0-3587792E8F3E}']
    function buttonStyle: CPBarButtonStyle; cdecl;
    function buttonType: CPBarButtonType; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPBarButton initWithImage:handler:] or -[CPBarButton initWithTitle:handler:]", ios(12.0, 14.0))
    function image: UIImage; cdecl;
    function initWithImage(image: UIImage; handler: CPBarButtonHandler): Pointer; cdecl;
    function initWithTitle(title: NSString; handler: CPBarButtonHandler): Pointer; cdecl;
    function initWithType(&type: CPBarButtonType; handler: CPBarButtonHandler): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPBarButton initWithImage:handler:] or -[CPBarButton buttonWithTitle:handler:]", ios(12.0, 14.0))
    function isEnabled: Boolean; cdecl;
    procedure setButtonStyle(buttonStyle: CPBarButtonStyle); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPBarButton = class(TOCGenericImport<CPBarButtonClass, CPBarButton>) end;

  CPBarButtonProviding = interface(IObjectiveC)
    ['{F048B8BF-B606-4DAA-8276-E4EDAB1AA55A}']
    function backButton: CPBarButton; cdecl;
    function leadingNavigationBarButtons: NSArray; cdecl;
    procedure setBackButton(backButton: CPBarButton); cdecl;
    procedure setLeadingNavigationBarButtons(leadingNavigationBarButtons: NSArray); cdecl;
    procedure setTrailingNavigationBarButtons(trailingNavigationBarButtons: NSArray); cdecl;
    function trailingNavigationBarButtons: NSArray; cdecl;
  end;

  CPButtonClass = interface(NSObjectClass)
    ['{88045C7D-D5BC-4D56-AB06-3D7E5738903E}']
    {class} function new: Pointer; cdecl;
  end;

  CPButton = interface(NSObject)
    ['{A406EEE8-B238-4155-8D82-83B2B112BEA8}']
    function image: UIImage; cdecl;
    function initWithImage(image: UIImage; handler: TCPButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPButton = class(TOCGenericImport<CPButtonClass, CPButton>) end;

  CPContactClass = interface(NSObjectClass)
    ['{EB7AC95F-220D-4CC5-9063-EDED2133B9E6}']
  end;

  CPContact = interface(NSObject)
    ['{225879B3-1397-45B2-BBEB-53C6883AEB0D}']
    function actions: NSArray; cdecl;
    function image: UIImage; cdecl;
    function informativeText: NSString; cdecl;
    function initWithName(name: NSString; image: UIImage): Pointer; cdecl;
    function name: NSString; cdecl;
    procedure setActions(actions: NSArray); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setInformativeText(informativeText: NSString); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    function subtitle: NSString; cdecl;
  end;
  TCPContact = class(TOCGenericImport<CPContactClass, CPContact>) end;

  CPContactCallButtonClass = interface(CPButtonClass)
    ['{C6B1152C-5DEA-41A2-B724-2FA6457EE3A7}']
  end;

  CPContactCallButton = interface(CPButton)
    ['{B6805DE6-3C1B-405B-AC4B-8A96DF54C31E}']
    function initWithHandler(handler: TCPContactCallButtonBlockMethod1): Pointer; cdecl;
    function initWithImage(image: UIImage; handler: TCPContactCallButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPContactCallButton = class(TOCGenericImport<CPContactCallButtonClass, CPContactCallButton>) end;

  CPContactMessageButtonClass = interface(CPButtonClass)
    ['{E9F16125-268F-45A9-BDFE-832B6591B0FB}']
  end;

  CPContactMessageButton = interface(CPButton)
    ['{619CABB5-A057-4102-BF22-3A96707F4F46}']
    function initWithPhoneOrEmail(phoneOrEmail: NSString): Pointer; cdecl;
    function phoneOrEmail: NSString; cdecl;
  end;
  TCPContactMessageButton = class(TOCGenericImport<CPContactMessageButtonClass, CPContactMessageButton>) end;

  CPContactDirectionsButtonClass = interface(CPButtonClass)
    ['{EB67DA5F-A4F1-42CE-BA7C-6078027E1151}']
  end;

  CPContactDirectionsButton = interface(CPButton)
    ['{DB13A280-9EF8-4DE2-ACEF-BB71B42AA4B0}']
    function initWithHandler(handler: TCPContactDirectionsButtonBlockMethod1): Pointer; cdecl;
    function initWithImage(image: UIImage; handler: TCPContactDirectionsButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPContactDirectionsButton = class(TOCGenericImport<CPContactDirectionsButtonClass, CPContactDirectionsButton>) end;

  CPContactTemplateClass = interface(CPTemplateClass)
    ['{E3D153C1-4792-422B-A25C-427618361344}']
    {class} function new: Pointer; cdecl;
  end;

  CPContactTemplate = interface(CPTemplate)
    ['{A02A02E2-B140-405B-A9C3-708841AFCFF0}']
    function contact: CPContact; cdecl;
    function initWithContact(contact: CPContact): Pointer; cdecl;
    procedure setContact(contact: CPContact); cdecl;
  end;
  TCPContactTemplate = class(TOCGenericImport<CPContactTemplateClass, CPContactTemplate>) end;

  CPDashboardButtonClass = interface(NSObjectClass)
    ['{A490DE81-8353-403D-B6B2-7BB2E78590D1}']
    {class} function new: Pointer; cdecl;
  end;

  CPDashboardButton = interface(NSObject)
    ['{5321DEC3-DEBE-41F7-9045-C9A0A6F5F489}']
    function image: UIImage; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; subtitleVariants: NSArray; image: UIImage;
      handler: TCPDashboardButtonBlockMethod1): Pointer; cdecl;
    function subtitleVariants: NSArray; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPDashboardButton = class(TOCGenericImport<CPDashboardButtonClass, CPDashboardButton>) end;

  CPDashboardControllerClass = interface(NSObjectClass)
    ['{1885AD3E-687A-411A-8FD1-E981DAA02615}']
    {class} function new: Pointer; cdecl;
  end;

  CPDashboardController = interface(NSObject)
    ['{B6D50584-501A-4535-AE9A-4B77174D94DC}']
    procedure setShortcutButtons(shortcutButtons: NSArray); cdecl;
    function shortcutButtons: NSArray; cdecl;
  end;
  TCPDashboardController = class(TOCGenericImport<CPDashboardControllerClass, CPDashboardController>) end;

  CPGridButtonClass = interface(NSObjectClass)
    ['{C5556BFF-2563-4154-8E67-8920B496C2E5}']
    {class} function new: Pointer; cdecl;
  end;

  CPGridButton = interface(NSObject)
    ['{B128213C-77A8-4BAE-9EB1-55C4DD4DF181}']
    function image: UIImage; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; image: UIImage; handler: TCPGridButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPGridButton = class(TOCGenericImport<CPGridButtonClass, CPGridButton>) end;

  CPGridTemplateClass = interface(CPTemplateClass)
    ['{640053D0-67C9-487D-B610-C69D8E305992}']
    {class} function new: Pointer; cdecl;
  end;

  CPGridTemplate = interface(CPTemplate)
    ['{01C6541E-59F8-4D9F-94C7-6589C8ADC284}']
    function gridButtons: NSArray; cdecl;
    function initWithTitle(title: NSString; gridButtons: NSArray): Pointer; cdecl;
    function title: NSString; cdecl;
    procedure updateGridButtons(gridButtons: NSArray); cdecl;
    procedure updateTitle(title: NSString); cdecl;
  end;
  TCPGridTemplate = class(TOCGenericImport<CPGridTemplateClass, CPGridTemplate>) end;

  CPImageSetClass = interface(NSObjectClass)
    ['{238BC1F8-1673-4091-A57F-F5FEBB70A693}']
    {class} function new: Pointer; cdecl;
  end;

  CPImageSet = interface(NSObject)
    ['{23CA19EE-A17E-437A-BE1D-E567185DAAC4}']
    function darkContentImage: UIImage; cdecl;
    function initWithLightContentImage(lightImage: UIImage; darkContentImage: UIImage): Pointer; cdecl;
    function lightContentImage: UIImage; cdecl;
  end;
  TCPImageSet = class(TOCGenericImport<CPImageSetClass, CPImageSet>) end;

  CPTextButtonClass = interface(NSObjectClass)
    ['{0BDF21D4-32AF-4724-A173-BA8EE9525DC3}']
    {class} function new: Pointer; cdecl;
  end;

  CPTextButton = interface(NSObject)
    ['{89CC4DAF-1B54-404E-B613-CB956C527A65}']
    function initWithTitle(title: NSString; textStyle: CPTextButtonStyle; handler: TCPTextButtonBlockMethod1): Pointer; cdecl;
    procedure setTextStyle(textStyle: CPTextButtonStyle); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function textStyle: CPTextButtonStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TCPTextButton = class(TOCGenericImport<CPTextButtonClass, CPTextButton>) end;

  CPInformationItemClass = interface(NSObjectClass)
    ['{2787323A-F1F3-4B24-AA13-7381046FD068}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationItem = interface(NSObject)
    ['{AEADC61C-F337-4821-8B01-7C232C04DB09}']
    function detail: NSString; cdecl;
    function initWithTitle(title: NSString; detail: NSString): Pointer; cdecl;
    function title: NSString; cdecl;
  end;
  TCPInformationItem = class(TOCGenericImport<CPInformationItemClass, CPInformationItem>) end;

  CPInformationRatingItemClass = interface(CPInformationItemClass)
    ['{C72EBA20-FCC8-4CBC-857D-ED6BA37F1B0C}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationRatingItem = interface(CPInformationItem)
    ['{38AD0A66-3135-4932-935B-971B24D6F2D2}']
    function initWithRating(rating: NSNumber; maximumRating: NSNumber; title: NSString; detail: NSString): Pointer; cdecl;
    function maximumRating: NSNumber; cdecl;
    function rating: NSNumber; cdecl;
  end;
  TCPInformationRatingItem = class(TOCGenericImport<CPInformationRatingItemClass, CPInformationRatingItem>) end;

  CPInformationTemplateClass = interface(CPTemplateClass)
    ['{B734D7D8-0F5F-4591-B304-2BDA00ED3171}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationTemplate = interface(CPTemplate)
    ['{B4FE33EE-8127-48B5-AA3A-6AEB427CA032}']
    function actions: NSArray; cdecl;
    function initWithTitle(title: NSString; layout: CPInformationTemplateLayout; items: NSArray; actions: NSArray): Pointer; cdecl;
    function items: NSArray; cdecl;
    function layout: CPInformationTemplateLayout; cdecl;
    procedure setActions(actions: NSArray); cdecl;
    procedure setItems(items: NSArray); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPInformationTemplate = class(TOCGenericImport<CPInformationTemplateClass, CPInformationTemplate>) end;

  CPWindowClass = interface(UIWindowClass)
    ['{0313F6C8-7A0C-4445-B22A-137B2E43E3D8}']
  end;

  CPWindow = interface(UIWindow)
    ['{5930757F-FE95-49B8-96F2-C1C3F50868A1}']
    function mapButtonSafeAreaLayoutGuide: UILayoutGuide; cdecl;
    procedure setTemplateApplicationScene(templateApplicationScene: CPTemplateApplicationScene); cdecl;
    procedure setWindowScene(windowScene: UIWindowScene); cdecl;
    function templateApplicationScene: CPTemplateApplicationScene; cdecl;
    function windowScene: UIWindowScene; cdecl;
  end;
  TCPWindow = class(TOCGenericImport<CPWindowClass, CPWindow>) end;

  CPInterfaceControllerClass = interface(NSObjectClass)
    ['{8D28EE46-21BE-4445-A7F0-931E05E291AB}']
    {class} function new: Pointer; cdecl;
  end;

  CPInterfaceController = interface(NSObject)
    ['{6FC136B5-FF0C-4CA6-A763-69AFD7CD9A5C}']
    function carTraitCollection: UITraitCollection; cdecl;
    function delegate: Pointer; cdecl;
    procedure dismissTemplateAnimated(animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    procedure dismissTemplateAnimated(animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-dismissTemplateAnimated:completion:", ios(12.0, 14.0))
    procedure popTemplateAnimated(animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-popTemplateAnimated:completion:", ios(12.0, 14.0))
    procedure popTemplateAnimated(animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    procedure popToRootTemplateAnimated(animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    procedure popToRootTemplateAnimated(animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-popToRootTemplateAnimated:completion:", ios(12.0, 14.0))
    procedure popToTemplate(targetTemplate: CPTemplate; animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-popToTemplate:animated:completion:", ios(12.0, 14.0))
    procedure popToTemplate(targetTemplate: CPTemplate; animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    function prefersDarkUserInterfaceStyle: Boolean; cdecl;
    function presentedTemplate: CPTemplate; cdecl;
    procedure presentTemplate(templateToPresent: CPTemplate; animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-presentTemplate:animated:completion:", ios(12.0, 14.0))
    procedure presentTemplate(templateToPresent: CPTemplate; animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    procedure pushTemplate(templateToPush: CPTemplate; animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-pushTemplate:animated:completion:", ios(12.0, 14.0))
    procedure pushTemplate(templateToPush: CPTemplate; animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    function rootTemplate: CPTemplate; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPrefersDarkUserInterfaceStyle(prefersDarkUserInterfaceStyle: Boolean); cdecl;
    procedure setRootTemplate(rootTemplate: CPTemplate; animated: Boolean; completion: TCPInterfaceControllerBlockMethod1); overload; cdecl;
    procedure setRootTemplate(rootTemplate: CPTemplate; animated: Boolean); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-setRootTemplate:animated:completion:", ios(12.0, 14.0))
    function templates: NSArray; cdecl;
    function topTemplate: CPTemplate; cdecl;
  end;
  TCPInterfaceController = class(TOCGenericImport<CPInterfaceControllerClass, CPInterfaceController>) end;

  CPInterfaceControllerDelegate = interface(IObjectiveC)
    ['{0D4A0B7C-7BAD-4F6F-A28F-B6957DAF28C0}']
    procedure templateDidAppear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateDidDisappear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateWillAppear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateWillDisappear(aTemplate: CPTemplate; animated: Boolean); cdecl;
  end;

  CPApplicationDelegate = interface(IObjectiveC)
    ['{F9E15E3D-E68D-4233-BC7C-D5EC6BDF1AA2}']
    procedure application(application: UIApplication; didSelectNavigationAlert: CPNavigationAlert); overload; cdecl;
    procedure application(application: UIApplication; didSelectManeuver: CPManeuver); overload; cdecl;
    procedure application(application: UIApplication; didConnectCarInterfaceController: CPInterfaceController; toWindow: CPWindow); overload; cdecl;
    [MethodName('application:didDisconnectCarInterfaceController:fromWindow:')]
    procedure applicationDidDisconnectCarInterfaceController(application: UIApplication; didDisconnectCarInterfaceController: CPInterfaceController;
      fromWindow: CPWindow); cdecl;
  end;

  CPInstrumentClusterControllerClass = interface(NSObjectClass)
    ['{B00E0797-0FDC-4822-9856-53CCBC96CFB0}']
    {class} function new: Pointer; cdecl;
  end;

  CPInstrumentClusterController = interface(NSObject)
    ['{15D8A094-0764-4E94-A750-FC68B8982C48}']
    function attributedInactiveDescriptionVariants: NSArray; cdecl;
    function compassSetting: CPInstrumentClusterSetting; cdecl;
    function delegate: Pointer; cdecl;
    function inactiveDescriptionVariants: NSArray; cdecl;
    function instrumentClusterWindow: UIWindow; cdecl;
    procedure setAttributedInactiveDescriptionVariants(attributedInactiveDescriptionVariants: NSArray); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setInactiveDescriptionVariants(inactiveDescriptionVariants: NSArray); cdecl;
    function speedLimitSetting: CPInstrumentClusterSetting; cdecl;
  end;
  TCPInstrumentClusterController = class(TOCGenericImport<CPInstrumentClusterControllerClass, CPInstrumentClusterController>) end;

  CPInstrumentClusterControllerDelegate = interface(IObjectiveC)
    ['{889BF2E7-77F5-401C-8795-2381A90179EE}']
    procedure instrumentClusterController(instrumentClusterController: CPInstrumentClusterController;
      didChangeCompassSetting: CPInstrumentClusterSetting); cdecl;
    [MethodName('instrumentClusterController:didChangeSpeedLimitSetting:')]
    procedure instrumentClusterControllerDidChangeSpeedLimitSetting(instrumentClusterController: CPInstrumentClusterController;
      didChangeSpeedLimitSetting: CPInstrumentClusterSetting); cdecl;
    procedure instrumentClusterControllerDidConnectWindow(instrumentClusterWindow: UIWindow); cdecl;
    procedure instrumentClusterControllerDidDisconnectWindow(instrumentClusterWindow: UIWindow); cdecl;
    procedure instrumentClusterControllerDidZoomIn(instrumentClusterController: CPInstrumentClusterController); cdecl;
    procedure instrumentClusterControllerDidZoomOut(instrumentClusterController: CPInstrumentClusterController); cdecl;
  end;

  CPListTemplateItem = interface(IObjectiveC)
    ['{C44DDF14-28CE-4D63-880F-DC5DB95D770B}']
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function text: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;

  CPSelectableListItem = interface(IObjectiveC)
    ['{2A89BA60-7650-44B6-8766-DA057EE47DA7}']
    function handler: TCPSelectableListItemBlockMethod1; cdecl;
    procedure setHandler(handler: Pointer); cdecl;
  end;

  CPListSectionClass = interface(NSObjectClass)
    ['{03895BA9-7CC0-441B-988F-EFD1A929AAA5}']
    {class} function new: Pointer; cdecl;
  end;

  CPListSection = interface(NSObject)
    ['{6CCB6075-7EBD-47B6-91C0-E78B8F7D24F0}']
    function header: NSString; cdecl;
    function headerButton: CPButton; cdecl;
    function headerImage: UIImage; cdecl;
    function headerSubtitle: NSString; cdecl;
    function indexOfItem(item: Pointer): NSUInteger; cdecl;
    function initWithItems(items: NSArray): Pointer; overload; cdecl;
    function initWithItems(items: NSArray; header: NSString; headerSubtitle: NSString; headerImage: UIImage; headerButton: CPButton;
      sectionIndexTitle: NSString): Pointer; overload; cdecl;
    function initWithItems(items: NSArray; header: NSString; sectionIndexTitle: NSString): Pointer; overload; cdecl;
    function itemAtIndex(index: NSUInteger): Pointer; cdecl;
    function items: NSArray; cdecl;
    function sectionIndexTitle: NSString; cdecl;
    procedure setHeaderImage(headerImage: UIImage); cdecl;
  end;
  TCPListSection = class(TOCGenericImport<CPListSectionClass, CPListSection>) end;

  CPListImageRowItemClass = interface(NSObjectClass)
    ['{208F34E3-A569-46E9-863C-A627743769FD}']
    {class} function maximumImageSize: CGSize; cdecl;
  end;

  CPListImageRowItem = interface(NSObject)
    ['{911CC54C-BE99-4015-A764-B72E42CF978C}']
    function gridImages: NSArray; cdecl;
    function handler: TCPListImageRowItemBlockMethod1; cdecl;
    function imageTitles: NSArray; cdecl;
    function initWithText(text: NSString; images: NSArray; imageTitles: NSArray): Pointer; overload; cdecl;
    function initWithText(text: NSString; images: NSArray): Pointer; overload; cdecl;
    function isEnabled: Boolean; cdecl;
    function listImageRowHandler: TCPListImageRowItemBlockMethod3; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setHandler(handler: TCPListImageRowItemBlockMethod2); cdecl;
    procedure setImageTitles(imageTitles: NSArray); cdecl;
    procedure setListImageRowHandler(listImageRowHandler: TCPListImageRowItemBlockMethod2); cdecl;
    procedure setText(text: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function text: NSString; cdecl;
    procedure updateImages(gridImages: NSArray); cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPListImageRowItem = class(TOCGenericImport<CPListImageRowItemClass, CPListImageRowItem>) end;

  CPListItemClass = interface(NSObjectClass)
    ['{C2114DDE-B190-4668-B2A7-E4F122AEC029}']
    {class} function maximumImageSize: CGSize; cdecl;
  end;

  CPListItem = interface(NSObject)
    ['{F1BBE32A-2CEA-4AFB-BAA2-1D1F53C8753D}']
    function accessoryImage: UIImage; cdecl;
    function accessoryType: CPListItemAccessoryType; cdecl;
    function detailText: NSString; cdecl;
    function handler: TCPListItemBlockMethod1; cdecl;
    function image: UIImage; cdecl;
    function initWithText(text: NSString; detailText: NSString; image: UIImage; accessoryImage: UIImage;
      accessoryType: CPListItemAccessoryType): Pointer; overload; cdecl;
    function initWithText(text: NSString; detailText: NSString; image: UIImage): Pointer; overload; cdecl;
    function initWithText(text: NSString; detailText: NSString; image: UIImage; showsDisclosureIndicator: Boolean): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initWithText:detailText:image:accessoryImage:accessoryType:", ios(12.0, 14.0))
    function initWithText(text: NSString; detailText: NSString): Pointer; overload; cdecl;
    function isEnabled: Boolean; cdecl;
    function isExplicitContent: Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    function playbackProgress: CGFloat; cdecl;
    function playingIndicatorLocation: CPListItemPlayingIndicatorLocation; cdecl;
    procedure setAccessoryImage(accessoryImage: UIImage); cdecl;
    procedure setAccessoryType(accessoryType: CPListItemAccessoryType); cdecl;
    procedure setDetailText(detailText: NSString); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setExplicitContent(explicitContent: Boolean); cdecl;
    procedure setHandler(handler: TCPListItemBlockMethod2); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setPlaybackProgress(playbackProgress: CGFloat); cdecl;
    procedure setPlaying(playing: Boolean); cdecl;
    procedure setPlayingIndicatorLocation(playingIndicatorLocation: CPListItemPlayingIndicatorLocation); cdecl;
    procedure setShowsExplicitLabel(showsExplicitLabel: Boolean); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-explicitContent", ios(14.0, 14.0))
    procedure setText(text: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function showsDisclosureIndicator: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("CPListItemAccessoryType", ios(12.0, 14.0))
    function showsExplicitLabel: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-explicitContent", ios(14.0, 14.0))
    function text: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPListItem = class(TOCGenericImport<CPListItemClass, CPListItem>) end;

  CPAssistantCellConfigurationClass = interface(NSObjectClass)
    ['{0260B5EC-7737-4855-BB3E-8981BFE0E31B}']
  end;

  CPAssistantCellConfiguration = interface(NSObject)
    ['{FCC8B5FB-EEE8-4909-98F8-F87D865CAF26}']
    function assistantAction: CPAssistantCellActionType; cdecl;
    function initWithPosition(position: CPAssistantCellPosition; visibility: CPAssistantCellVisibility;
      assistantAction: CPAssistantCellActionType): Pointer; cdecl;
    function position: CPAssistantCellPosition; cdecl;
    function visibility: CPAssistantCellVisibility; cdecl;
  end;
  TCPAssistantCellConfiguration = class(TOCGenericImport<CPAssistantCellConfigurationClass, CPAssistantCellConfiguration>) end;

  CPListTemplateClass = interface(CPTemplateClass)
    ['{818948F6-C60B-4605-8B4A-F7DA1DF36D0B}']
    {class} function maximumItemCount: NSUInteger; cdecl;
    {class} function maximumSectionCount: NSUInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPListTemplate = interface(CPTemplate)
    ['{9AA66967-312E-4FEE-A4D7-04131F6FE413}']
    function assistantCellConfiguration: CPAssistantCellConfiguration; cdecl;
    function delegate: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
    function emptyViewSubtitleVariants: NSArray; cdecl;
    function emptyViewTitleVariants: NSArray; cdecl;
    function indexPathForItem(item: Pointer): NSIndexPath; cdecl;
    function initWithTitle(title: NSString; sections: NSArray; assistantCellConfiguration: CPAssistantCellConfiguration): Pointer; overload; cdecl;
    function initWithTitle(title: NSString; sections: NSArray): Pointer; overload; cdecl;
    function itemCount: NSUInteger; cdecl;
    function sectionCount: NSUInteger; cdecl;
    function sections: NSArray; cdecl;
    procedure setAssistantCellConfiguration(assistantCellConfiguration: CPAssistantCellConfiguration); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
    procedure setEmptyViewSubtitleVariants(emptyViewSubtitleVariants: NSArray); cdecl;
    procedure setEmptyViewTitleVariants(emptyViewTitleVariants: NSArray); cdecl;
    function title: NSString; cdecl;
    procedure updateSections(sections: NSArray); cdecl;
  end;
  TCPListTemplate = class(TOCGenericImport<CPListTemplateClass, CPListTemplate>) end;

  CPListTemplateDelegate = interface(IObjectiveC)
    ['{BAB1C5D8-8257-4A88-ACCB-65D26ED931E1}']
    procedure listTemplate(listTemplate: CPListTemplate; didSelectListItem: CPListItem; completionHandler: Pointer); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
  end;

  CPLaneClass = interface(NSObjectClass)
    ['{BBBED441-35C1-4E0E-8C45-5EA5F60095ED}']
  end;

  CPLane = interface(NSObject)
    ['{0C3FA948-8424-4F78-91C2-564F15C7C244}']
    function angles: NSArray; cdecl;
    function highlightedAngle: NSMeasurement; cdecl;
    function initWithAngles(angles: NSArray; highlightedAngle: NSMeasurement; isPreferred: Boolean): Pointer; overload; cdecl;
    function initWithAngles(angles: NSArray): Pointer; overload; cdecl;
    function primaryAngle: NSMeasurement; cdecl; // API_DEPRECATED("Use highlightedAngle to get value, use -[CPLane initAngles:highlightedAngle:isPreferred:] to create a CPLane with highlightedAngle set", ios(17.4, 18.0))
    function secondaryAngles: NSArray; cdecl; // API_DEPRECATED("Use angles to get value, Use -[CPLane initWithAngles:] or -[CPLane initAngles:highlightedAngle:isPreferred:] to create a CPLane with angles", ios(17.4, 18.0))
    procedure setPrimaryAngle(primaryAngle: NSMeasurement); cdecl; // API_DEPRECATED("Use highlightedAngle to get value, use -[CPLane initAngles:highlightedAngle:isPreferred:] to create a CPLane with highlightedAngle set", ios(17.4, 18.0))
    procedure setSecondaryAngles(secondaryAngles: NSArray); cdecl; // API_DEPRECATED("Use angles to get value, Use -[CPLane initWithAngles:] or -[CPLane initAngles:highlightedAngle:isPreferred:] to create a CPLane with angles", ios(17.4, 18.0))
    procedure setStatus(status: CPLaneStatus); cdecl; // API_DEPRECATED("Use -[CPLane initWithAngles:] to create a CPLane with CPLaneStatusNotGood, use -[CPLane initAngles:highlightedAngle:isPreferred:] to create a CPLane with status CPLaneStatusGood or CPLaneStatusPreferred", ios(17.4, 18.0))
    function status: CPLaneStatus; cdecl;
  end;
  TCPLane = class(TOCGenericImport<CPLaneClass, CPLane>) end;

  CPLaneGuidanceClass = interface(NSObjectClass)
    ['{F06802CE-B40F-4F73-A5F1-9C00376F08DA}']
  end;

  CPLaneGuidance = interface(NSObject)
    ['{5FCD8D2F-B45F-4157-8387-D710AD6CAD18}']
    function instructionVariants: NSArray; cdecl;
    function lanes: NSArray; cdecl;
    procedure setInstructionVariants(instructionVariants: NSArray); cdecl;
    procedure setLanes(lanes: NSArray); cdecl;
  end;
  TCPLaneGuidance = class(TOCGenericImport<CPLaneGuidanceClass, CPLaneGuidance>) end;

  CPTravelEstimatesClass = interface(NSObjectClass)
    ['{1C0384D1-E760-4077-B569-0765A2751829}']
    {class} function new: Pointer; cdecl;
  end;

  CPTravelEstimates = interface(NSObject)
    ['{A63D12CF-78F5-4662-B613-4FE735EC6C11}']
    function distanceRemaining: NSMeasurement; cdecl;
    function distanceRemainingToDisplay: NSMeasurement; cdecl;
    function initWithDistanceRemaining(distance: NSMeasurement; timeRemaining: NSTimeInterval): Pointer; overload; cdecl;
    function initWithDistanceRemaining(distanceRemaining: NSMeasurement; distanceRemainingToDisplay: NSMeasurement;
      timeRemaining: NSTimeInterval): Pointer; overload; cdecl;
    function timeRemaining: NSTimeInterval; cdecl;
  end;
  TCPTravelEstimates = class(TOCGenericImport<CPTravelEstimatesClass, CPTravelEstimates>) end;

  CPManeuverClass = interface(NSObjectClass)
    ['{EDBA3800-2206-44ED-A0C6-B09CFB6D06E0}']
  end;

  CPManeuver = interface(NSObject)
    ['{BCE0180D-646E-4EA9-AF7A-4FA316E68223}']
    function attributedInstructionVariants: NSArray; cdecl;
    function cardBackgroundColor: UIColor; cdecl;
    function dashboardAttributedInstructionVariants: NSArray; cdecl;
    function dashboardInstructionVariants: NSArray; cdecl;
    function dashboardJunctionImage: UIImage; cdecl;
    function dashboardSymbolImage: UIImage; cdecl;
    function highwayExitLabel: NSString; cdecl;
    function initialTravelEstimates: CPTravelEstimates; cdecl;
    function instructionVariants: NSArray; cdecl;
    function junctionElementAngles: NSSet; cdecl;
    function junctionExitAngle: NSMeasurement; cdecl;
    function junctionImage: UIImage; cdecl;
    function junctionType: CPJunctionType; cdecl;
    function linkedLaneGuidance: CPLaneGuidance; cdecl;
    function maneuverType: CPManeuverType; cdecl;
    function notificationAttributedInstructionVariants: NSArray; cdecl;
    function notificationInstructionVariants: NSArray; cdecl;
    function notificationSymbolImage: UIImage; cdecl;
    function roadFollowingManeuverVariants: NSArray; cdecl;
    procedure setAttributedInstructionVariants(attributedInstructionVariants: NSArray); cdecl;
    procedure setCardBackgroundColor(cardBackgroundColor: UIColor); cdecl;
    procedure setDashboardAttributedInstructionVariants(dashboardAttributedInstructionVariants: NSArray); cdecl;
    procedure setDashboardInstructionVariants(dashboardInstructionVariants: NSArray); cdecl;
    procedure setDashboardJunctionImage(dashboardJunctionImage: UIImage); cdecl;
    procedure setDashboardSymbolImage(dashboardSymbolImage: UIImage); cdecl;
    procedure setHighwayExitLabel(highwayExitLabel: NSString); cdecl;
    procedure setInitialTravelEstimates(initialTravelEstimates: CPTravelEstimates); cdecl;
    procedure setInstructionVariants(instructionVariants: NSArray); cdecl;
    procedure setJunctionElementAngles(junctionElementAngles: NSSet); cdecl;
    procedure setJunctionExitAngle(junctionExitAngle: NSMeasurement); cdecl;
    procedure setJunctionImage(junctionImage: UIImage); cdecl;
    procedure setJunctionType(junctionType: CPJunctionType); cdecl;
    procedure setLinkedLaneGuidance(linkedLaneGuidance: CPLaneGuidance); cdecl;
    procedure setManeuverType(maneuverType: CPManeuverType); cdecl;
    procedure setNotificationAttributedInstructionVariants(notificationAttributedInstructionVariants: NSArray); cdecl;
    procedure setNotificationInstructionVariants(notificationInstructionVariants: NSArray); cdecl;
    procedure setNotificationSymbolImage(notificationSymbolImage: UIImage); cdecl;
    procedure setRoadFollowingManeuverVariants(roadFollowingManeuverVariants: NSArray); cdecl;
    procedure setSymbolImage(symbolImage: UIImage); cdecl;
    procedure setSymbolSet(symbolSet: CPImageSet); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("symbolImage", ios(12.0, 13.0))
    procedure setTrafficSide(trafficSide: CPTrafficSide); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function symbolImage: UIImage; cdecl;
    function symbolSet: CPImageSet; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("symbolImage", ios(12.0, 13.0))
    function trafficSide: CPTrafficSide; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPManeuver = class(TOCGenericImport<CPManeuverClass, CPManeuver>) end;

  CPMapButtonClass = interface(NSObjectClass)
    ['{73594D8C-ABE2-4F8B-BBA8-2595941612DA}']
  end;

  CPMapButton = interface(NSObject)
    ['{51AA4C50-73CC-4F9D-9E7B-622300D15E61}']
    function focusedImage: UIImage; cdecl;
    function image: UIImage; cdecl;
    function initWithHandler(handler: TCPMapButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setFocusedImage(focusedImage: UIImage); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setImage(image: UIImage); cdecl;
  end;
  TCPMapButton = class(TOCGenericImport<CPMapButtonClass, CPMapButton>) end;

  CPNavigationAlertClass = interface(NSObjectClass)
    ['{8988036C-E9AD-47A6-951B-D724ECD5170F}']
  end;

  CPNavigationAlert = interface(NSObject)
    ['{CAD842CB-9EE1-4E64-BC08-D759AC53A44D}']
    function duration: NSTimeInterval; cdecl;
    function image: UIImage; cdecl;
    function imageSet: CPImageSet; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; subtitleVariants: NSArray; image: UIImage; primaryAction: CPAlertAction;
      secondaryAction: CPAlertAction; duration: NSTimeInterval): Pointer; overload; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; subtitleVariants: NSArray; imageSet: CPImageSet; primaryAction: CPAlertAction;
      secondaryAction: CPAlertAction; duration: NSTimeInterval): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initWithTitleVariants:subtitleVariants:image:primaryAction:secondaryAction:duration", ios(12.0, 13.0))
    function primaryAction: CPAlertAction; cdecl;
    function secondaryAction: CPAlertAction; cdecl;
    function subtitleVariants: NSArray; cdecl;
    function titleVariants: NSArray; cdecl;
    procedure updateTitleVariants(newTitleVariants: NSArray; subtitleVariants: NSArray); cdecl;
  end;
  TCPNavigationAlert = class(TOCGenericImport<CPNavigationAlertClass, CPNavigationAlert>) end;

  CPRouteInformationClass = interface(NSObjectClass)
    ['{9CE8C723-D81C-439A-A873-0B492546DAE3}']
  end;

  CPRouteInformation = interface(NSObject)
    ['{8C941AEF-7753-4C5D-B27D-4055F0F45110}']
    function currentLaneGuidance: CPLaneGuidance; cdecl;
    function currentManeuvers: NSArray; cdecl;
    function initWithManeuvers(maneuvers: NSArray; laneGuidances: NSArray; currentManeuvers: NSArray; currentLaneGuidance: CPLaneGuidance;
      tripTravelEstimates: CPTravelEstimates; maneuverTravelEstimates: CPTravelEstimates): Pointer; cdecl;
    function laneGuidances: NSArray; cdecl;
    function maneuvers: NSArray; cdecl;
    function maneuverTravelEstimates: CPTravelEstimates; cdecl;
    function tripTravelEstimates: CPTravelEstimates; cdecl;
  end;
  TCPRouteInformation = class(TOCGenericImport<CPRouteInformationClass, CPRouteInformation>) end;

  CPRouteChoiceClass = interface(NSObjectClass)
    ['{506534F1-3BF1-4C4E-9FFE-F80749E354F1}']
    {class} function new: Pointer; cdecl;
  end;

  CPRouteChoice = interface(NSObject)
    ['{D6A8F176-1E55-4AEF-991C-853249B22BD8}']
    function additionalInformationVariants: NSArray; cdecl;
    function initWithSummaryVariants(summaryVariants: NSArray; additionalInformationVariants: NSArray;
      selectionSummaryVariants: NSArray): Pointer; cdecl;
    function selectionSummaryVariants: NSArray; cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function summaryVariants: NSArray; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPRouteChoice = class(TOCGenericImport<CPRouteChoiceClass, CPRouteChoice>) end;

  CPTripClass = interface(NSObjectClass)
    ['{E2377887-F1AA-457D-A6AC-E6E284DBB55B}']
    {class} function new: Pointer; cdecl;
  end;

  CPTrip = interface(NSObject)
    ['{D167FAAA-1F09-4050-B5CF-F62E59CE9D6F}']
    function destination: MKMapItem; cdecl;
    function destinationNameVariants: NSArray; cdecl;
    function initWithOrigin(origin: MKMapItem; destination: MKMapItem; routeChoices: NSArray): Pointer; cdecl;
    function origin: MKMapItem; cdecl;
    function routeChoices: NSArray; cdecl;
    procedure setDestinationNameVariants(destinationNameVariants: NSArray); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPTrip = class(TOCGenericImport<CPTripClass, CPTrip>) end;

  CPNavigationSessionClass = interface(NSObjectClass)
    ['{F153CD74-8DBA-4160-8013-C1FE5738B6FE}']
    {class} function new: Pointer; cdecl;
  end;

  CPNavigationSession = interface(NSObject)
    ['{37101D7F-6012-46AF-A7AC-F168EAD4B5F9}']
    procedure addLaneGuidances(laneGuidances: NSArray); cdecl;
    procedure addManeuvers(maneuvers: NSArray); cdecl;
    procedure cancelTrip; cdecl;
    function currentLaneGuidance: CPLaneGuidance; cdecl;
    function currentRoadNameVariants: NSArray; cdecl;
    procedure finishTrip; cdecl;
    function maneuverState: CPManeuverState; cdecl;
    procedure pauseTripForReason(reason: CPTripPauseReason; description: NSString; turnCardColor: UIColor); overload; cdecl;
    procedure pauseTripForReason(reason: CPTripPauseReason; description: NSString); overload; cdecl;
    procedure resumeTripWithUpdatedRouteInformation(routeInformation: CPRouteInformation); cdecl;
    procedure setCurrentLaneGuidance(currentLaneGuidance: CPLaneGuidance); cdecl;
    procedure setCurrentRoadNameVariants(currentRoadNameVariants: NSArray); cdecl;
    procedure setManeuverState(maneuverState: CPManeuverState); cdecl;
    procedure setUpcomingManeuvers(upcomingManeuvers: NSArray); cdecl;
    function trip: CPTrip; cdecl;
    function upcomingManeuvers: NSArray; cdecl;
    procedure updateTravelEstimates(estimates: CPTravelEstimates; forManeuver: CPManeuver); cdecl;
  end;
  TCPNavigationSession = class(TOCGenericImport<CPNavigationSessionClass, CPNavigationSession>) end;

  CPTripPreviewTextConfigurationClass = interface(NSObjectClass)
    ['{B92D7327-31E5-4750-ADA9-CF97E486C395}']
  end;

  CPTripPreviewTextConfiguration = interface(NSObject)
    ['{0DE93A89-236B-4852-B3CC-184379561BD9}']
    function additionalRoutesButtonTitle: NSString; cdecl;
    function initWithStartButtonTitle(startButtonTitle: NSString; additionalRoutesButtonTitle: NSString;
      overviewButtonTitle: NSString): Pointer; cdecl;
    function overviewButtonTitle: NSString; cdecl;
    function startButtonTitle: NSString; cdecl;
  end;
  TCPTripPreviewTextConfiguration = class(TOCGenericImport<CPTripPreviewTextConfigurationClass, CPTripPreviewTextConfiguration>) end;

  CPMapTemplateClass = interface(CPTemplateClass)
    ['{C092BA11-B6C2-4522-B52C-3F2CE99B1277}']
  end;

  CPMapTemplate = interface(CPTemplate)
    ['{4B28F0C9-90AF-4E58-97EE-A931278B8170}']
    function automaticallyHidesNavigationBar: Boolean; cdecl;
    function currentNavigationAlert: CPNavigationAlert; cdecl;
    procedure dismissNavigationAlertAnimated(animated: Boolean; completion: TCPMapTemplateBlockMethod1); cdecl;
    procedure dismissPanningInterfaceAnimated(animated: Boolean); cdecl;
    function guidanceBackgroundColor: UIColor; cdecl;
    function hidesButtonsWithNavigationBar: Boolean; cdecl;
    procedure hideTripPreviews; cdecl;
    function isPanningInterfaceVisible: Boolean; cdecl;
    function mapButtons: NSArray; cdecl;
    function mapDelegate: Pointer; cdecl;
    procedure presentNavigationAlert(navigationAlert: CPNavigationAlert; animated: Boolean); cdecl;
    procedure setAutomaticallyHidesNavigationBar(automaticallyHidesNavigationBar: Boolean); cdecl;
    procedure setGuidanceBackgroundColor(guidanceBackgroundColor: UIColor); cdecl;
    procedure setHidesButtonsWithNavigationBar(hidesButtonsWithNavigationBar: Boolean); cdecl;
    procedure setMapButtons(mapButtons: NSArray); cdecl;
    procedure setMapDelegate(mapDelegate: Pointer); cdecl;
    procedure setTripEstimateStyle(tripEstimateStyle: CPTripEstimateStyle); cdecl;
    procedure showPanningInterfaceAnimated(animated: Boolean); cdecl;
    procedure showRouteChoicesPreviewForTrip(tripPreview: CPTrip; textConfiguration: CPTripPreviewTextConfiguration); cdecl;
    procedure showTripPreviews(tripPreviews: NSArray; selectedTrip: CPTrip; textConfiguration: CPTripPreviewTextConfiguration); overload; cdecl;
    procedure showTripPreviews(tripPreviews: NSArray; textConfiguration: CPTripPreviewTextConfiguration); overload; cdecl;
    function startNavigationSessionForTrip(trip: CPTrip): CPNavigationSession; cdecl;
    function tripEstimateStyle: CPTripEstimateStyle; cdecl;
    procedure updateTravelEstimates(estimates: CPTravelEstimates; forTrip: CPTrip); overload; cdecl;
    procedure updateTravelEstimates(estimates: CPTravelEstimates; forTrip: CPTrip; withTimeRemainingColor: CPTimeRemainingColor); overload; cdecl;
  end;
  TCPMapTemplate = class(TOCGenericImport<CPMapTemplateClass, CPMapTemplate>) end;

  CPMapTemplateDelegate = interface(IObjectiveC)
    ['{542A7BBB-A0D0-4717-8930-0F873AA3F857}']
    procedure mapTemplate(mapTemplate: CPMapTemplate; willDismissNavigationAlert: CPNavigationAlert;
      dismissalContext: CPNavigationAlertDismissalContext); overload; cdecl;
    procedure mapTemplate(mapTemplate: CPMapTemplate; didEndPanGestureWithVelocity: CGPoint); overload; cdecl;
    procedure mapTemplate(mapTemplate: CPMapTemplate; didUpdatePanGestureWithTranslation: CGPoint; velocity: CGPoint); overload; cdecl;
    procedure mapTemplate(mapTemplate: CPMapTemplate; panBeganWithDirection: CPPanDirection); overload; cdecl;
    procedure mapTemplate(mapTemplate: CPMapTemplate; selectedPreviewForTrip: CPTrip; usingRouteChoice: CPRouteChoice); overload; cdecl;
    function mapTemplate(mapTemplate: CPMapTemplate; shouldShowNotificationForManeuver: CPManeuver): Boolean; overload; cdecl;
    function mapTemplate(mapTemplate: CPMapTemplate; shouldShowNotificationForNavigationAlert: CPNavigationAlert): Boolean; overload; cdecl;
    function mapTemplate(mapTemplate: CPMapTemplate; shouldUpdateNotificationForManeuver: CPManeuver;
      withTravelEstimates: CPTravelEstimates): Boolean; overload; cdecl;
    procedure mapTemplateDidBeginPanGesture(mapTemplate: CPMapTemplate); cdecl;
    procedure mapTemplateDidCancelNavigation(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:didDismissNavigationAlert:dismissalContext:')]
    procedure mapTemplateDidDismissNavigationAlert(mapTemplate: CPMapTemplate; didDismissNavigationAlert: CPNavigationAlert;
      dismissalContext: CPNavigationAlertDismissalContext); cdecl;
    procedure mapTemplateDidDismissPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:didShowNavigationAlert:')]
    procedure mapTemplateDidShowNavigationAlert(mapTemplate: CPMapTemplate; didShowNavigationAlert: CPNavigationAlert); cdecl;
    procedure mapTemplateDidShowPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:displayStyleForManeuver:')]
    function mapTemplateDisplayStyleForManeuver(mapTemplate: CPMapTemplate; displayStyleForManeuver: CPManeuver): CPManeuverDisplayStyle; cdecl;
    [MethodName('mapTemplate:panEndedWithDirection:')]
    procedure mapTemplatePanEndedWithDirection(mapTemplate: CPMapTemplate; panEndedWithDirection: CPPanDirection); cdecl;
    [MethodName('mapTemplate:panWithDirection:')]
    procedure mapTemplatePanWithDirection(mapTemplate: CPMapTemplate; panWithDirection: CPPanDirection); cdecl;
    function mapTemplateShouldProvideNavigationMetadata(mapTemplate: CPMapTemplate): Boolean; cdecl;
    [MethodName('mapTemplate:startedTrip:usingRouteChoice:')]
    procedure mapTemplateStartedTrip(mapTemplate: CPMapTemplate; startedTrip: CPTrip; usingRouteChoice: CPRouteChoice); cdecl;
    procedure mapTemplateWillDismissPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:willShowNavigationAlert:')]
    procedure mapTemplateWillShowNavigationAlert(mapTemplate: CPMapTemplate; willShowNavigationAlert: CPNavigationAlert); cdecl;
  end;

  CPMessageComposeBarButtonClass = interface(CPBarButtonClass)
    ['{8B99538B-B92B-4F5A-A170-23E0F0DDBAD1}']
    {class} function new: Pointer; cdecl;
  end;

  CPMessageComposeBarButton = interface(CPBarButton)
    ['{EB8B0609-9941-458A-87D3-FA128F7EAFA9}']
    function initWithImage(image: UIImage): Pointer; overload; cdecl;
    function initWithImage(image: UIImage; handler: CPBarButtonHandler): Pointer; overload; cdecl;
    function initWithTitle(title: NSString; handler: CPBarButtonHandler): Pointer; cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPMessageComposeBarButton = class(TOCGenericImport<CPMessageComposeBarButtonClass, CPMessageComposeBarButton>) end;

  CPMessageListItemLeadingConfigurationClass = interface(NSObjectClass)
    ['{D5289252-88DF-4CB9-9DD2-35A155DD5A8C}']
  end;

  CPMessageListItemLeadingConfiguration = interface(NSObject)
    ['{9EDB432E-C92A-461F-AABC-C03B2191BA8C}']
    function initWithLeadingItem(leadingItem: CPMessageLeadingItem; leadingImage: UIImage; unread: Boolean): Pointer; cdecl;
    function isUnread: Boolean; cdecl;
    function leadingImage: UIImage; cdecl;
    function leadingItem: CPMessageLeadingItem; cdecl;
  end;
  TCPMessageListItemLeadingConfiguration = class(TOCGenericImport<CPMessageListItemLeadingConfigurationClass, CPMessageListItemLeadingConfiguration>) end;

  CPMessageListItemTrailingConfigurationClass = interface(NSObjectClass)
    ['{43B23CA3-E4D8-4552-B2D1-E20F6D10A11A}']
  end;

  CPMessageListItemTrailingConfiguration = interface(NSObject)
    ['{06BF347D-4DAA-4E5B-8C77-BAB820229030}']
    function initWithTrailingItem(trailingItem: CPMessageTrailingItem; trailingImage: UIImage): Pointer; cdecl;
    function trailingImage: UIImage; cdecl;
    function trailingItem: CPMessageTrailingItem; cdecl;
  end;
  TCPMessageListItemTrailingConfiguration = class(TOCGenericImport<CPMessageListItemTrailingConfigurationClass, CPMessageListItemTrailingConfiguration>) end;

  CPMessageListItemClass = interface(NSObjectClass)
    ['{2BE069E8-0F8A-4440-A214-344049AB6F83}']
  end;

  CPMessageListItem = interface(NSObject)
    ['{A9D9812A-8441-49FF-A451-9B81FB355BF5}']
    function conversationIdentifier: NSString; cdecl;
    function detailText: NSString; cdecl;
    function initWithConversationIdentifier(conversationIdentifier: NSString; text: NSString;
      leadingConfiguration: CPMessageListItemLeadingConfiguration; trailingConfiguration: CPMessageListItemTrailingConfiguration;
      detailText: NSString; trailingText: NSString): Pointer; cdecl;
    function initWithFullName(fullName: NSString; phoneOrEmailAddress: NSString; leadingConfiguration: CPMessageListItemLeadingConfiguration;
      trailingConfiguration: CPMessageListItemTrailingConfiguration; detailText: NSString; trailingText: NSString): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    function leadingConfiguration: CPMessageListItemLeadingConfiguration; cdecl;
    function phoneOrEmailAddress: NSString; cdecl;
    procedure setConversationIdentifier(conversationIdentifier: NSString); cdecl;
    procedure setDetailText(detailText: NSString); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setLeadingConfiguration(leadingConfiguration: CPMessageListItemLeadingConfiguration); cdecl;
    procedure setPhoneOrEmailAddress(phoneOrEmailAddress: NSString); cdecl;
    procedure setText(text: NSString); cdecl;
    procedure setTrailingConfiguration(trailingConfiguration: CPMessageListItemTrailingConfiguration); cdecl;
    procedure setTrailingText(trailingText: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function text: NSString; cdecl;
    function trailingConfiguration: CPMessageListItemTrailingConfiguration; cdecl;
    function trailingText: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPMessageListItem = class(TOCGenericImport<CPMessageListItemClass, CPMessageListItem>) end;

  CPNowPlayingButtonClass = interface(NSObjectClass)
    ['{E93B7F47-093A-493E-89CA-251FA6BFE949}']
    {class} function new: Pointer; cdecl;
  end;

  CPNowPlayingButton = interface(NSObject)
    ['{09DA9D3F-77AE-4471-9DE7-FFA07144F742}']
    function initWithHandler(handler: TCPNowPlayingButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    function isSelected: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setSelected(selected: Boolean); cdecl;
  end;
  TCPNowPlayingButton = class(TOCGenericImport<CPNowPlayingButtonClass, CPNowPlayingButton>) end;

  CPNowPlayingShuffleButtonClass = interface(CPNowPlayingButtonClass)
    ['{79090E8F-0CDC-4B7E-BA55-80108217AD31}']
  end;

  CPNowPlayingShuffleButton = interface(CPNowPlayingButton)
    ['{FE8A550E-F68B-41FD-8912-7643EB0BC1BB}']
  end;
  TCPNowPlayingShuffleButton = class(TOCGenericImport<CPNowPlayingShuffleButtonClass, CPNowPlayingShuffleButton>) end;

  CPNowPlayingAddToLibraryButtonClass = interface(CPNowPlayingButtonClass)
    ['{1A619020-F210-4C6B-9723-CFE929436A7A}']
  end;

  CPNowPlayingAddToLibraryButton = interface(CPNowPlayingButton)
    ['{9CE00512-5456-4C6C-949F-C88A45ADFDD1}']
  end;
  TCPNowPlayingAddToLibraryButton = class(TOCGenericImport<CPNowPlayingAddToLibraryButtonClass, CPNowPlayingAddToLibraryButton>) end;

  CPNowPlayingMoreButtonClass = interface(CPNowPlayingButtonClass)
    ['{0594AE41-2B13-417C-985E-03334822D533}']
  end;

  CPNowPlayingMoreButton = interface(CPNowPlayingButton)
    ['{D69302B1-F02C-42DE-86F7-614A54059E31}']
  end;
  TCPNowPlayingMoreButton = class(TOCGenericImport<CPNowPlayingMoreButtonClass, CPNowPlayingMoreButton>) end;

  CPNowPlayingPlaybackRateButtonClass = interface(CPNowPlayingButtonClass)
    ['{9A2C0DFA-A937-4A3F-BA62-9D7976AAD626}']
  end;

  CPNowPlayingPlaybackRateButton = interface(CPNowPlayingButton)
    ['{4A149B03-FCC3-47F0-BCD2-23149E4EAA7C}']
  end;
  TCPNowPlayingPlaybackRateButton = class(TOCGenericImport<CPNowPlayingPlaybackRateButtonClass, CPNowPlayingPlaybackRateButton>) end;

  CPNowPlayingRepeatButtonClass = interface(CPNowPlayingButtonClass)
    ['{E8741759-2915-4ABF-B1AF-2445FF21588F}']
  end;

  CPNowPlayingRepeatButton = interface(CPNowPlayingButton)
    ['{CB705F92-C3BB-4302-9A57-69AD1DF89C76}']
  end;
  TCPNowPlayingRepeatButton = class(TOCGenericImport<CPNowPlayingRepeatButtonClass, CPNowPlayingRepeatButton>) end;

  CPNowPlayingImageButtonClass = interface(CPNowPlayingButtonClass)
    ['{E9E258AF-A1C9-4521-81C8-BADC2EAEA7F8}']
  end;

  CPNowPlayingImageButton = interface(CPNowPlayingButton)
    ['{27FDE279-3C9B-45FB-963B-9A15F4A8BBFE}']
    function image: UIImage; cdecl;
    function initWithImage(image: UIImage; handler: TCPNowPlayingImageButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPNowPlayingImageButton = class(TOCGenericImport<CPNowPlayingImageButtonClass, CPNowPlayingImageButton>) end;

  CPNowPlayingTemplateObserver = interface(IObjectiveC)
    ['{07047E0F-D77B-4A65-AE9B-A3ED646368BD}']
    procedure nowPlayingTemplateAlbumArtistButtonTapped(nowPlayingTemplate: CPNowPlayingTemplate); cdecl;
    procedure nowPlayingTemplateUpNextButtonTapped(nowPlayingTemplate: CPNowPlayingTemplate); cdecl;
  end;

  CPNowPlayingTemplateClass = interface(CPTemplateClass)
    ['{49B85FA3-3682-4BE9-8318-5B6BEC03C1F5}']
    {class} function new: Pointer; cdecl;
    {class} function sharedTemplate: CPNowPlayingTemplate; cdecl;
  end;

  CPNowPlayingTemplate = interface(CPTemplate)
    ['{BAD3E2DE-3A3A-4034-BF74-847B9B83225D}']
    procedure addObserver(observer: Pointer); cdecl;
    function isAlbumArtistButtonEnabled: Boolean; cdecl;
    function isUpNextButtonEnabled: Boolean; cdecl;
    function nowPlayingButtons: NSArray; cdecl;
    procedure removeObserver(observer: Pointer); cdecl;
    procedure setAlbumArtistButtonEnabled(albumArtistButtonEnabled: Boolean); cdecl;
    procedure setUpNextButtonEnabled(upNextButtonEnabled: Boolean); cdecl;
    procedure setUpNextTitle(upNextTitle: NSString); cdecl;
    procedure updateNowPlayingButtons(nowPlayingButtons: NSArray); cdecl;
    function upNextTitle: NSString; cdecl;
  end;
  TCPNowPlayingTemplate = class(TOCGenericImport<CPNowPlayingTemplateClass, CPNowPlayingTemplate>) end;

  CPPointOfInterestClass = interface(NSObjectClass)
    ['{075A03F5-C783-468F-B19D-DCFB2729D2E4}']
    {class} function new: Pointer; cdecl;
    {class} function pinImageSize: CGSize; cdecl;
    {class} function selectedPinImageSize: CGSize; cdecl;
  end;

  CPPointOfInterest = interface(NSObject)
    ['{B6948796-4864-46FD-985C-82DDAABC2AAC}']
    function detailSubtitle: NSString; cdecl;
    function detailSummary: NSString; cdecl;
    function detailTitle: NSString; cdecl;
    function initWithLocation(location: MKMapItem; title: NSString; subtitle: NSString; summary: NSString; detailTitle: NSString;
      detailSubtitle: NSString; detailSummary: NSString; pinImage: UIImage; selectedPinImage: UIImage): Pointer; overload; cdecl;
    function initWithLocation(location: MKMapItem; title: NSString; subtitle: NSString; summary: NSString; detailTitle: NSString;
      detailSubtitle: NSString; detailSummary: NSString; pinImage: UIImage): Pointer; overload; cdecl;
    function location: MKMapItem; cdecl;
    function pinImage: UIImage; cdecl;
    function primaryButton: CPTextButton; cdecl;
    function secondaryButton: CPTextButton; cdecl;
    function selectedPinImage: UIImage; cdecl;
    procedure setDetailSubtitle(detailSubtitle: NSString); cdecl;
    procedure setDetailSummary(detailSummary: NSString); cdecl;
    procedure setDetailTitle(detailTitle: NSString); cdecl;
    procedure setLocation(location: MKMapItem); cdecl;
    procedure setPinImage(pinImage: UIImage); cdecl;
    procedure setPrimaryButton(primaryButton: CPTextButton); cdecl;
    procedure setSecondaryButton(secondaryButton: CPTextButton); cdecl;
    procedure setSelectedPinImage(selectedPinImage: UIImage); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    procedure setSummary(summary: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function subtitle: NSString; cdecl;
    function summary: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPPointOfInterest = class(TOCGenericImport<CPPointOfInterestClass, CPPointOfInterest>) end;

  CPPointOfInterestTemplateDelegate = interface(IObjectiveC)
    ['{D395CE54-AE89-48C2-AB1E-B075139FDCC2}']
    procedure pointOfInterestTemplate(pointOfInterestTemplate: CPPointOfInterestTemplate; didChangeMapRegion: MKCoordinateRegion); overload; cdecl;
    procedure pointOfInterestTemplate(pointOfInterestTemplate: CPPointOfInterestTemplate; didSelectPointOfInterest: CPPointOfInterest); overload; cdecl;
  end;

  CPPointOfInterestTemplateClass = interface(CPTemplateClass)
    ['{C3E91923-F3B7-4FAB-86F5-43A696152D58}']
    {class} function new: Pointer; cdecl;
  end;

  CPPointOfInterestTemplate = interface(CPTemplate)
    ['{FF8CA55D-9A0D-4A9B-9274-B27F00D24C09}']
    function initWithTitle(title: NSString; pointsOfInterest: NSArray; selectedIndex: NSUInteger): Pointer; cdecl;
    function pointOfInterestDelegate: Pointer; cdecl;
    function pointsOfInterest: NSArray; cdecl;
    function selectedIndex: NSUInteger; cdecl;
    procedure setPointOfInterestDelegate(pointOfInterestDelegate: Pointer); cdecl;
    procedure setPointsOfInterest(pointsOfInterest: NSArray; selectedIndex: NSUInteger); cdecl;
    procedure setSelectedIndex(selectedIndex: NSUInteger); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPPointOfInterestTemplate = class(TOCGenericImport<CPPointOfInterestTemplateClass, CPPointOfInterestTemplate>) end;

  CPSearchTemplateClass = interface(CPTemplateClass)
    ['{CE0253DA-3DD0-4112-8ABA-36E4E1177557}']
  end;

  CPSearchTemplate = interface(CPTemplate)
    ['{1FDA6F3A-3F7D-4A72-8AC1-D4FC48549570}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPSearchTemplate = class(TOCGenericImport<CPSearchTemplateClass, CPSearchTemplate>) end;

  CPSearchTemplateDelegate = interface(IObjectiveC)
    ['{EFB4CB3F-BE84-46A8-8257-71C4A8911702}']
    procedure searchTemplate(searchTemplate: CPSearchTemplate; selectedResult: CPListItem; completionHandler: Pointer); overload; cdecl;
    procedure searchTemplate(searchTemplate: CPSearchTemplate; updatedSearchText: NSString; completionHandler: Pointer); overload; cdecl;
    procedure searchTemplateSearchButtonPressed(searchTemplate: CPSearchTemplate); cdecl;
  end;

  CPSessionConfigurationClass = interface(NSObjectClass)
    ['{5A01BEB0-D92C-4D62-B17E-4276730CD02E}']
    {class} function new: Pointer; cdecl;
  end;

  CPSessionConfiguration = interface(NSObject)
    ['{B4C00C8B-EC4B-42FA-9BFE-946DA39CC3FE}']
    function contentStyle: CPContentStyle; cdecl;
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function limitedUserInterfaces: CPLimitableUserInterface; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPSessionConfiguration = class(TOCGenericImport<CPSessionConfigurationClass, CPSessionConfiguration>) end;

  CPSessionConfigurationDelegate = interface(IObjectiveC)
    ['{4E72EB23-F27C-4E63-9DE4-DFD6B505E73D}']
    procedure sessionConfiguration(sessionConfiguration: CPSessionConfiguration); cdecl;
    [MethodName('sessionConfiguration:contentStyleChanged:')]
    procedure sessionConfigurationContentStyleChanged(sessionConfiguration: CPSessionConfiguration; contentStyleChanged: CPContentStyle); cdecl;
  end;

  CPTabBarTemplateClass = interface(CPTemplateClass)
    ['{629A615D-AD91-42C0-93BA-7DEDAA47215E}']
    {class} function maximumTabCount: NSInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPTabBarTemplate = interface(CPTemplate)
    ['{E6ACF417-D95D-42C3-A072-5059ECB91429}']
    function delegate: Pointer; cdecl;
    function initWithTemplates(templates: NSArray): Pointer; cdecl;
    function selectedTemplate: CPTemplate; cdecl;
    procedure selectTemplate(newTemplate: CPTemplate); cdecl;
    procedure selectTemplateAtIndex(index: NSInteger); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function templates: NSArray; cdecl;
    procedure updateTemplates(newTemplates: NSArray); cdecl;
  end;
  TCPTabBarTemplate = class(TOCGenericImport<CPTabBarTemplateClass, CPTabBarTemplate>) end;

  CPTabBarTemplateDelegate = interface(IObjectiveC)
    ['{4431CC4C-D3CB-4621-9BEC-D5ECB7097552}']
    procedure tabBarTemplate(tabBarTemplate: CPTabBarTemplate; didSelectTemplate: CPTemplate); cdecl;
  end;

  CPTemplateApplicationDashboardSceneDelegate = interface(IObjectiveC)
    ['{579CE61B-3A75-483E-9E2B-3FB1CB141411}']
    procedure templateApplicationDashboardScene(templateApplicationDashboardScene: CPTemplateApplicationDashboardScene;
      didConnectDashboardController: CPDashboardController; toWindow: UIWindow); cdecl;
    [MethodName('templateApplicationDashboardScene:didDisconnectDashboardController:fromWindow:')]
    procedure templateApplicationDashboardSceneDidDisconnectDashboardController(templateApplicationDashboardScene: CPTemplateApplicationDashboardScene;
      didDisconnectDashboardController: CPDashboardController; fromWindow: UIWindow); cdecl;
  end;

  CPTemplateApplicationDashboardSceneClass = interface(UISceneClass)
    ['{AC176973-962B-4FD4-A189-FFE77763163F}']
  end;

  CPTemplateApplicationDashboardScene = interface(UIScene)
    ['{6EBE51C4-6539-4BB9-9ADF-E46B9FAE5C79}']
    function dashboardController: CPDashboardController; cdecl;
    function dashboardWindow: UIWindow; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPTemplateApplicationDashboardScene = class(TOCGenericImport<CPTemplateApplicationDashboardSceneClass, CPTemplateApplicationDashboardScene>) end;

  CPTemplateApplicationInstrumentClusterSceneDelegate = interface(IObjectiveC)
    ['{5B690200-4592-4836-821B-F973CA719AFB}']
    procedure contentStyleDidChange(contentStyle: UIUserInterfaceStyle); cdecl;
    procedure templateApplicationInstrumentClusterScene(templateApplicationInstrumentClusterScene: CPTemplateApplicationInstrumentClusterScene;
      didConnectInstrumentClusterController: CPInstrumentClusterController); cdecl;
    [MethodName('templateApplicationInstrumentClusterScene:didDisconnectInstrumentClusterController:')]
    procedure templateApplicationInstrumentClusterSceneDidDisconnectInstrumentClusterController(templateApplicationInstrumentClusterScene: CPTemplateApplicationInstrumentClusterScene;
      didDisconnectInstrumentClusterController: CPInstrumentClusterController); cdecl;
  end;

  CPTemplateApplicationInstrumentClusterSceneClass = interface(UISceneClass)
    ['{2E2828E8-F43D-4346-AA44-FBFDFAD9DD5D}']
  end;

  CPTemplateApplicationInstrumentClusterScene = interface(UIScene)
    ['{FD872B1F-54F4-41F5-AA38-B862E38D5EF1}']
    function contentStyle: UIUserInterfaceStyle; cdecl;
    function delegate: Pointer; cdecl;
    function instrumentClusterController: CPInstrumentClusterController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPTemplateApplicationInstrumentClusterScene = class(TOCGenericImport<CPTemplateApplicationInstrumentClusterSceneClass, CPTemplateApplicationInstrumentClusterScene>) end;

  CPTemplateApplicationSceneDelegate = interface(IObjectiveC)
    ['{3B7AE956-8070-4EFA-ACAD-61DE32D2894C}']
    procedure contentStyleDidChange(contentStyle: UIUserInterfaceStyle); cdecl;
    procedure templateApplicationScene(templateApplicationScene: CPTemplateApplicationScene;
      didSelectNavigationAlert: CPNavigationAlert); overload; cdecl;
    procedure templateApplicationScene(templateApplicationScene: CPTemplateApplicationScene; didSelectManeuver: CPManeuver); overload; cdecl;
    procedure templateApplicationScene(templateApplicationScene: CPTemplateApplicationScene;
      didConnectInterfaceController: CPInterfaceController; toWindow: CPWindow); overload; cdecl;
    procedure templateApplicationScene(templateApplicationScene: CPTemplateApplicationScene;
      didConnectInterfaceController: CPInterfaceController); overload; cdecl;
    [MethodName('templateApplicationScene:didDisconnectInterfaceController:')]
    procedure templateApplicationSceneDidDisconnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didDisconnectInterfaceController: CPInterfaceController); overload; cdecl;
    [MethodName('templateApplicationScene:didDisconnectInterfaceController:fromWindow:')]
    procedure templateApplicationSceneDidDisconnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didDisconnectInterfaceController: CPInterfaceController; fromWindow: CPWindow); overload; cdecl;
  end;

  CPTemplateApplicationSceneClass = interface(UISceneClass)
    ['{867A1FCF-FE54-41C7-A476-E275BC2E4868}']
  end;

  CPTemplateApplicationScene = interface(UIScene)
    ['{9B2DBFB3-4795-4DCB-85D6-CAAB45A7B765}']
    function carWindow: CPWindow; cdecl;
    function contentStyle: UIUserInterfaceStyle; cdecl;
    function delegate: Pointer; cdecl;
    function interfaceController: CPInterfaceController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPTemplateApplicationScene = class(TOCGenericImport<CPTemplateApplicationSceneClass, CPTemplateApplicationScene>) end;

  CPVoiceControlStateClass = interface(NSObjectClass)
    ['{0D247AFB-A9C5-4583-A997-6F3E87E2C360}']
  end;

  CPVoiceControlState = interface(NSObject)
    ['{B10F2188-EDD9-400F-BF1C-FA366717BC40}']
    function identifier: NSString; cdecl;
    function image: UIImage; cdecl;
    function initWithIdentifier(identifier: NSString; titleVariants: NSArray; image: UIImage; repeats: Boolean): Pointer; cdecl;
    function repeats: Boolean; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPVoiceControlState = class(TOCGenericImport<CPVoiceControlStateClass, CPVoiceControlState>) end;

  CPVoiceControlTemplateClass = interface(CPTemplateClass)
    ['{C62CF5F8-2187-4792-B5FD-0EDBDFC4AB75}']
  end;

  CPVoiceControlTemplate = interface(CPTemplate)
    ['{285756FB-1ACF-43BE-9509-D9BA848C2514}']
    procedure activateVoiceControlStateWithIdentifier(identifier: NSString); cdecl;
    function activeStateIdentifier: NSString; cdecl;
    function initWithVoiceControlStates(voiceControlStates: NSArray): Pointer; cdecl;
    function voiceControlStates: NSArray; cdecl;
  end;
  TCPVoiceControlTemplate = class(TOCGenericImport<CPVoiceControlTemplateClass, CPVoiceControlTemplate>) end;

function CarPlayErrorDomain: NSString;
function CPTemplateApplicationDashboardSceneSessionRoleApplication: UISceneSessionRole;
function CPTemplateApplicationInstrumentClusterSceneSessionRoleApplication: UISceneSessionRole;
function CPTemplateApplicationSceneSessionRoleApplication: UISceneSessionRole;

const
  libCarPlay = '/System/Library/Frameworks/CarPlay.framework/CarPlay';

implementation

uses
  Posix.Dlfcn;

var
  CarPlayModule: THandle;

function CarPlayErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCarPlay, 'CarPlayErrorDomain');
end;

function CPTemplateApplicationDashboardSceneSessionRoleApplication: UISceneSessionRole;
begin
  Result := CocoaNSStringConst(libCarPlay, 'CPTemplateApplicationDashboardSceneSessionRoleApplication');
end;

function CPTemplateApplicationInstrumentClusterSceneSessionRoleApplication: UISceneSessionRole;
begin
  Result := CocoaNSStringConst(libCarPlay, 'CPTemplateApplicationInstrumentClusterSceneSessionRoleApplication');
end;

function CPTemplateApplicationSceneSessionRoleApplication: UISceneSessionRole;
begin
  Result := CocoaNSStringConst(libCarPlay, 'CPTemplateApplicationSceneSessionRoleApplication');
end;

initialization
  CarPlayModule := dlopen(MarshaledAString(libCarPlay), RTLD_LAZY);

finalization
  dlclose(CarPlayModule);

end.