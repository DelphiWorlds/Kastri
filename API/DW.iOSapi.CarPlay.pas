unit DW.iOSapi.CarPlay;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.UIKit, DW.iOSapi.Foundation, iOSapi.MapKit;

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
  CPListItemAccessoryTypeNone = 0;
  CPListItemAccessoryTypeDisclosureIndicator = 1;
  CPListItemAccessoryTypeCloud = 2;
  CPListItemPlayingIndicatorLocationLeading = 0;
  CPListItemPlayingIndicatorLocationTrailing = 1;
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
  CPListTemplateItem = interface;
  CPSelectableListItem = interface;
  CPListSection = interface;
  CPListImageRowItem = interface;
  CPListItem = interface;
  CPListTemplate = interface;
  CPListTemplateDelegate = interface;
  CPTravelEstimates = interface;
  CPManeuver = interface;
  CPMapButton = interface;
  CPNavigationAlert = interface;
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
  CPListItemAccessoryType = NSInteger;
  CPListItemPlayingIndicatorLocation = NSInteger;
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
  TCPInterfaceControllerBlockMethod1 = procedure(param1: Boolean; param2: NSError) of object;
  TCPSelectableListItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPSelectableListItemBlockMethod2 = procedure() of object;
  TCPListImageRowItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPListImageRowItemBlockMethod2 = procedure() of object;
  TCPListImageRowItemBlockMethod3 = procedure(param1: CPListImageRowItem; param2: NSInteger; param3: dispatch_block_t) of object;
  TCPListItemBlockMethod1 = procedure(param1: Pointer; param2: dispatch_block_t) of object;
  TCPListItemBlockMethod2 = procedure() of object;
  TCPListTemplateDelegateBlockMethod1 = procedure of object;
  TCPMapButtonBlockMethod1 = procedure(mapButton: CPMapButton) of object;
  TCPMapTemplateBlockMethod1 = procedure(dismissed: Boolean) of object;
  TCPNowPlayingButtonBlockMethod1 = procedure(param1: CPNowPlayingButton) of object;
  TCPNowPlayingImageButtonBlockMethod1 = procedure(param1: CPNowPlayingButton) of object;
  TCPSearchTemplateDelegateBlockMethod1 = procedure(searchResults: NSArray) of object;
  TCPSearchTemplateDelegateBlockMethod2 = procedure of object;

  CPAlertActionClass = interface(NSObjectClass)
    ['{AB4C4A8A-4CD7-4E81-9FC8-F45B5264F786}']
    {class} function new: Pointer; cdecl;
  end;

  CPAlertAction = interface(NSObject)
    ['{F90F6CF7-B3A7-4404-8183-1DFB7D04BF24}']
    function handler: CPAlertActionHandler; cdecl;
    function initWithTitle(title: NSString; style: CPAlertActionStyle; handler: CPAlertActionHandler): Pointer; cdecl;
    function style: CPAlertActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TCPAlertAction = class(TOCGenericImport<CPAlertActionClass, CPAlertAction>) end;

  CPTemplateClass = interface(NSObjectClass)
    ['{5DB1594A-1267-4C9A-83C1-E0BABD6BD311}']
  end;

  CPTemplate = interface(NSObject)
    ['{312E0436-72E3-468C-9FB6-25EAA8762FEC}']
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
    ['{CFF6E8E2-661A-43FB-A1B1-5FEE07B34AB9}']
    {class} function new: Pointer; cdecl;
  end;

  CPActionSheetTemplate = interface(CPTemplate)
    ['{48C3D13B-EBA9-41F7-87E3-25786C64A085}']
    function actions: NSArray; cdecl;
    function initWithTitle(title: NSString; message: NSString; actions: NSArray): Pointer; cdecl;
    function message: NSString; cdecl;
    function title: NSString; cdecl;
  end;
  TCPActionSheetTemplate = class(TOCGenericImport<CPActionSheetTemplateClass, CPActionSheetTemplate>) end;

  CPAlertTemplateClass = interface(CPTemplateClass)
    ['{B8E42683-1EBB-4FBD-88D2-38720776C6F0}']
    {class} function maximumActionCount: NSUInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPAlertTemplate = interface(CPTemplate)
    ['{02547DCD-4E86-4041-9370-63A7B9210946}']
    function actions: NSArray; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; actions: NSArray): Pointer; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPAlertTemplate = class(TOCGenericImport<CPAlertTemplateClass, CPAlertTemplate>) end;

  CPBarButtonClass = interface(NSObjectClass)
    ['{B048AB12-C1B8-48AD-8778-F0AEA60F6C3E}']
    {class} function new: Pointer; cdecl;
  end;

  CPBarButton = interface(NSObject)
    ['{B657F12B-F00A-4A5D-8661-BDC1FE60C5FA}']
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
    ['{8356EB6F-F1B8-47E7-AA88-C7F399B3467F}']
    function backButton: CPBarButton; cdecl;
    function leadingNavigationBarButtons: NSArray; cdecl;
    procedure setBackButton(backButton: CPBarButton); cdecl;
    procedure setLeadingNavigationBarButtons(leadingNavigationBarButtons: NSArray); cdecl;
    procedure setTrailingNavigationBarButtons(trailingNavigationBarButtons: NSArray); cdecl;
    function trailingNavigationBarButtons: NSArray; cdecl;
  end;

  CPButtonClass = interface(NSObjectClass)
    ['{E22ECC53-762D-43C4-A31B-5AD2AEF231F7}']
    {class} function new: Pointer; cdecl;
  end;

  CPButton = interface(NSObject)
    ['{5C0A5058-165A-48D8-860C-9E67B5AF6F85}']
    function image: UIImage; cdecl;
    function initWithImage(image: UIImage; handler: TCPButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPButton = class(TOCGenericImport<CPButtonClass, CPButton>) end;

  CPContactClass = interface(NSObjectClass)
    ['{6089BCDE-3AB3-4A47-8CE9-186E090F5909}']
  end;

  CPContact = interface(NSObject)
    ['{535E42D0-B07F-4F46-88D9-F42B1866898E}']
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
    ['{5AEF346A-92E0-467C-B38F-8A79042EEC73}']
  end;

  CPContactCallButton = interface(CPButton)
    ['{C7562427-9B0C-4592-B74F-A7944E5651EF}']
    function initWithHandler(handler: TCPContactCallButtonBlockMethod1): Pointer; cdecl;
    function initWithImage(image: UIImage; handler: TCPContactCallButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPContactCallButton = class(TOCGenericImport<CPContactCallButtonClass, CPContactCallButton>) end;

  CPContactMessageButtonClass = interface(CPButtonClass)
    ['{A159D152-A8C2-4D3C-891A-2DA972C06194}']
  end;

  CPContactMessageButton = interface(CPButton)
    ['{7C3F9716-11FE-4064-8295-CF865F326FFA}']
    function initWithPhoneOrEmail(phoneOrEmail: NSString): Pointer; cdecl;
    function phoneOrEmail: NSString; cdecl;
  end;
  TCPContactMessageButton = class(TOCGenericImport<CPContactMessageButtonClass, CPContactMessageButton>) end;

  CPContactDirectionsButtonClass = interface(CPButtonClass)
    ['{B2F90EF2-EEE1-4463-AB85-2A2EB4171514}']
  end;

  CPContactDirectionsButton = interface(CPButton)
    ['{8CB0782A-6E06-42C9-89DA-8ECA33DEBCC8}']
    function initWithHandler(handler: TCPContactDirectionsButtonBlockMethod1): Pointer; cdecl;
    function initWithImage(image: UIImage; handler: TCPContactDirectionsButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPContactDirectionsButton = class(TOCGenericImport<CPContactDirectionsButtonClass, CPContactDirectionsButton>) end;

  CPContactTemplateClass = interface(CPTemplateClass)
    ['{1AF77947-607B-4E4A-A1AD-9BF4C5AB5758}']
    {class} function new: Pointer; cdecl;
  end;

  CPContactTemplate = interface(CPTemplate)
    ['{E8B621C0-33EB-4E2A-A0DF-C9A8255F7092}']
    function contact: CPContact; cdecl;
    function initWithContact(contact: CPContact): Pointer; cdecl;
    procedure setContact(contact: CPContact); cdecl;
  end;
  TCPContactTemplate = class(TOCGenericImport<CPContactTemplateClass, CPContactTemplate>) end;

  CPDashboardButtonClass = interface(NSObjectClass)
    ['{2F48762C-76EF-431E-8942-1EE18F652FC3}']
    {class} function new: Pointer; cdecl;
  end;

  CPDashboardButton = interface(NSObject)
    ['{621F1A2A-F5DB-4057-9166-A020F2D87A06}']
    function image: UIImage; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; subtitleVariants: NSArray; image: UIImage;
      handler: TCPDashboardButtonBlockMethod1): Pointer; cdecl;
    function subtitleVariants: NSArray; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPDashboardButton = class(TOCGenericImport<CPDashboardButtonClass, CPDashboardButton>) end;

  CPDashboardControllerClass = interface(NSObjectClass)
    ['{10EFE16F-3954-48A1-9122-99E3D1452AF2}']
    {class} function new: Pointer; cdecl;
  end;

  CPDashboardController = interface(NSObject)
    ['{70CC1DA2-7763-4AB3-8ACE-9F1C5A310818}']
    procedure setShortcutButtons(shortcutButtons: NSArray); cdecl;
    function shortcutButtons: NSArray; cdecl;
  end;
  TCPDashboardController = class(TOCGenericImport<CPDashboardControllerClass, CPDashboardController>) end;

  CPGridButtonClass = interface(NSObjectClass)
    ['{32083650-7BCB-4A0A-8359-96BD7AC2C7E9}']
    {class} function new: Pointer; cdecl;
  end;

  CPGridButton = interface(NSObject)
    ['{2C4696AD-3DFE-49A0-A87E-7F3450674099}']
    function image: UIImage; cdecl;
    function initWithTitleVariants(titleVariants: NSArray; image: UIImage; handler: TCPGridButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPGridButton = class(TOCGenericImport<CPGridButtonClass, CPGridButton>) end;

  CPGridTemplateClass = interface(CPTemplateClass)
    ['{69CB3F1F-B8AC-474C-854A-7D8FBA60A5FC}']
    {class} function new: Pointer; cdecl;
  end;

  CPGridTemplate = interface(CPTemplate)
    ['{8EDF8733-A57C-4922-BC04-BD7246F93A82}']
    function gridButtons: NSArray; cdecl;
    function initWithTitle(title: NSString; gridButtons: NSArray): Pointer; cdecl;
    function title: NSString; cdecl;
  end;
  TCPGridTemplate = class(TOCGenericImport<CPGridTemplateClass, CPGridTemplate>) end;

  CPImageSetClass = interface(NSObjectClass)
    ['{22DEBB3A-0A6D-4FE7-87FD-75553BC34C90}']
    {class} function new: Pointer; cdecl;
  end;

  CPImageSet = interface(NSObject)
    ['{1E176632-0644-4B9C-ACFC-8A97AA50C102}']
    function darkContentImage: UIImage; cdecl;
    function initWithLightContentImage(lightImage: UIImage; darkContentImage: UIImage): Pointer; cdecl;
    function lightContentImage: UIImage; cdecl;
  end;
  TCPImageSet = class(TOCGenericImport<CPImageSetClass, CPImageSet>) end;

  CPTextButtonClass = interface(NSObjectClass)
    ['{DD8CF0E3-8294-413D-AAE5-C327D54DAFA1}']
    {class} function new: Pointer; cdecl;
  end;

  CPTextButton = interface(NSObject)
    ['{F88800FF-09D2-4F51-86E4-3D951C9B3973}']
    function initWithTitle(title: NSString; textStyle: CPTextButtonStyle; handler: TCPTextButtonBlockMethod1): Pointer; cdecl;
    procedure setTextStyle(textStyle: CPTextButtonStyle); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function textStyle: CPTextButtonStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TCPTextButton = class(TOCGenericImport<CPTextButtonClass, CPTextButton>) end;

  CPInformationItemClass = interface(NSObjectClass)
    ['{E4506CCB-8D8C-4E17-88FC-209F8C574E03}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationItem = interface(NSObject)
    ['{D182D796-C2B4-49F4-A682-1477DA405AF6}']
    function detail: NSString; cdecl;
    function initWithTitle(title: NSString; detail: NSString): Pointer; cdecl;
    function title: NSString; cdecl;
  end;
  TCPInformationItem = class(TOCGenericImport<CPInformationItemClass, CPInformationItem>) end;

  CPInformationRatingItemClass = interface(CPInformationItemClass)
    ['{29DC4463-69D3-461E-9C70-B0AB6144730C}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationRatingItem = interface(CPInformationItem)
    ['{7300472F-98EA-4673-9A15-2303A34C9414}']
    function initWithRating(rating: NSNumber; maximumRating: NSNumber; title: NSString; detail: NSString): Pointer; cdecl;
    function maximumRating: NSNumber; cdecl;
    function rating: NSNumber; cdecl;
  end;
  TCPInformationRatingItem = class(TOCGenericImport<CPInformationRatingItemClass, CPInformationRatingItem>) end;

  CPInformationTemplateClass = interface(CPTemplateClass)
    ['{AE2F0CDB-08C4-4EF8-8CC9-C00AD40AC193}']
    {class} function new: Pointer; cdecl;
  end;

  CPInformationTemplate = interface(CPTemplate)
    ['{34375725-CE82-4D3C-8A7B-289EAC32A50A}']
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
    ['{E4BF6334-8DBF-4971-9C08-0F58DAEF3030}']
  end;

  CPWindow = interface(UIWindow)
    ['{1BAD1CC7-F404-4B0F-9ACA-F5190F29A050}']
    function mapButtonSafeAreaLayoutGuide: UILayoutGuide; cdecl;
    procedure setTemplateApplicationScene(templateApplicationScene: CPTemplateApplicationScene); cdecl;
    procedure setWindowScene(windowScene: UIWindowScene); cdecl;
    function templateApplicationScene: CPTemplateApplicationScene; cdecl;
    function windowScene: UIWindowScene; cdecl;
  end;
  TCPWindow = class(TOCGenericImport<CPWindowClass, CPWindow>) end;

  CPInterfaceControllerClass = interface(NSObjectClass)
    ['{6EA3A79C-1634-4C2A-BFEE-E7E290F1F9E8}']
    {class} function new: Pointer; cdecl;
  end;

  CPInterfaceController = interface(NSObject)
    ['{2FE3F6CA-F614-4702-8395-CD264CE7E7E6}']
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
    ['{8D58FB0A-CC91-4721-B66D-352BB22F5CDD}']
    procedure templateDidAppear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateDidDisappear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateWillAppear(aTemplate: CPTemplate; animated: Boolean); cdecl;
    procedure templateWillDisappear(aTemplate: CPTemplate; animated: Boolean); cdecl;
  end;

  CPApplicationDelegate = interface(IObjectiveC)
    ['{85A6B38A-246D-4353-98DA-0710A56ADCFE}']
    [MethodName('application:didConnectCarInterfaceController:toWindow:')]
    procedure applicationDidConnectCarInterfaceController(application: UIApplication; didConnectCarInterfaceController: CPInterfaceController;
      toWindow: CPWindow); cdecl;
    [MethodName('application:didDisconnectCarInterfaceController:fromWindow:')]
    procedure applicationDidDisconnectCarInterfaceController(application: UIApplication; didDisconnectCarInterfaceController: CPInterfaceController;
      fromWindow: CPWindow); cdecl;
    [MethodName('application:didSelectManeuver:')]
    procedure applicationDidSelectManeuver(application: UIApplication; didSelectManeuver: CPManeuver); cdecl;
    [MethodName('application:didSelectNavigationAlert:')]
    procedure applicationDidSelectNavigationAlert(application: UIApplication; didSelectNavigationAlert: CPNavigationAlert); cdecl;
  end;

  CPListTemplateItem = interface(IObjectiveC)
    ['{A2F2C48F-8D0F-4E8F-A8D3-BFD8ACEF0824}']
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function text: NSString; cdecl;
    function userInfo: Pointer; cdecl;
  end;

  CPSelectableListItem = interface(IObjectiveC)
    ['{84329398-A87A-44AE-82C9-00914AC2552A}']
    function handler: TCPSelectableListItemBlockMethod1; cdecl;
    procedure setHandler(handler: TCPSelectableListItemBlockMethod2); cdecl;
  end;

  CPListSectionClass = interface(NSObjectClass)
    ['{4A7BD5D4-436D-46DF-8BA9-7CB7CA242A58}']
    {class} function new: Pointer; cdecl;
  end;

  CPListSection = interface(NSObject)
    ['{C5855042-A06D-433F-A61A-08EC6171EC60}']
    function header: NSString; cdecl;
    function indexOfItem(item: Pointer): NSUInteger; cdecl;
    function initWithItems(items: NSArray): Pointer; overload; cdecl;
    function initWithItems(items: NSArray; header: NSString; sectionIndexTitle: NSString): Pointer; overload; cdecl;
    function itemAtIndex(index: NSUInteger): Pointer; cdecl;
    function items: NSArray; cdecl;
    function sectionIndexTitle: NSString; cdecl;
  end;
  TCPListSection = class(TOCGenericImport<CPListSectionClass, CPListSection>) end;

  CPListImageRowItemClass = interface(NSObjectClass)
    ['{C4F98C3A-308B-4C7E-8230-E03E55B6D0A2}']
    {class} function maximumImageSize: CGSize; cdecl;
  end;

  CPListImageRowItem = interface(NSObject)
    ['{88432BC9-C1C8-466D-985B-E42D0AC868F4}']
    function gridImages: NSArray; cdecl;
    function handler: TCPListImageRowItemBlockMethod1; cdecl;
    function initWithText(text: NSString; images: NSArray): Pointer; cdecl;
    function listImageRowHandler: TCPListImageRowItemBlockMethod3; cdecl;
    procedure setHandler(handler: TCPListImageRowItemBlockMethod2); cdecl;
    procedure setListImageRowHandler(listImageRowHandler: TCPListImageRowItemBlockMethod2); cdecl;
    procedure setText(text: NSString); cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function text: NSString; cdecl;
    procedure updateImages(gridImages: NSArray); cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPListImageRowItem = class(TOCGenericImport<CPListImageRowItemClass, CPListImageRowItem>) end;

  CPListItemClass = interface(NSObjectClass)
    ['{55092702-CCA3-49F1-8500-7538392F2F74}']
    {class} function maximumImageSize: CGSize; cdecl;
  end;

  CPListItem = interface(NSObject)
    ['{5F390FE4-0C35-4D0C-87B8-0595CFF60E79}']
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
    function isExplicitContent: Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    function playbackProgress: CGFloat; cdecl;
    function playingIndicatorLocation: CPListItemPlayingIndicatorLocation; cdecl;
    procedure setAccessoryImage(accessoryImage: UIImage); cdecl;
    procedure setAccessoryType(accessoryType: CPListItemAccessoryType); cdecl;
    procedure setDetailText(detailText: NSString); cdecl;
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

  CPListTemplateClass = interface(CPTemplateClass)
    ['{5C465C3D-3926-4FAE-A8C6-A4231CB82398}']
    {class} function maximumItemCount: NSUInteger; cdecl;
    {class} function maximumSectionCount: NSUInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPListTemplate = interface(CPTemplate)
    ['{0CCCC5E3-2425-44A3-8E7E-A81D1B3B48CA}']
    function delegate: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
    function emptyViewSubtitleVariants: NSArray; cdecl;
    function emptyViewTitleVariants: NSArray; cdecl;
    function indexPathForItem(item: Pointer): NSIndexPath; cdecl;
    function initWithTitle(title: NSString; sections: NSArray): Pointer; cdecl;
    function itemCount: NSUInteger; cdecl;
    function sectionCount: NSUInteger; cdecl;
    function sections: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
    procedure setEmptyViewSubtitleVariants(emptyViewSubtitleVariants: NSArray); cdecl;
    procedure setEmptyViewTitleVariants(emptyViewTitleVariants: NSArray); cdecl;
    function title: NSString; cdecl;
    procedure updateSections(sections: NSArray); cdecl;
  end;
  TCPListTemplate = class(TOCGenericImport<CPListTemplateClass, CPListTemplate>) end;

  CPListTemplateDelegate = interface(IObjectiveC)
    ['{7B97E2E5-6C06-48AD-89EF-383551CBB942}']
    procedure listTemplate(listTemplate: CPListTemplate; didSelectListItem: CPListItem;
      completionHandler: TCPListTemplateDelegateBlockMethod1); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[CPListItem handler]", ios(12.0, 14.0))
  end;

  CPTravelEstimatesClass = interface(NSObjectClass)
    ['{CB669A22-75A7-447D-B1AA-DEFF9D7B53C2}']
    {class} function new: Pointer; cdecl;
  end;

  CPTravelEstimates = interface(NSObject)
    ['{672CB504-A457-4349-A7B6-B819BA0C5DA2}']
    function distanceRemaining: NSMeasurement; cdecl;
    function initWithDistanceRemaining(distance: NSMeasurement; timeRemaining: NSTimeInterval): Pointer; cdecl;
    function timeRemaining: NSTimeInterval; cdecl;
  end;
  TCPTravelEstimates = class(TOCGenericImport<CPTravelEstimatesClass, CPTravelEstimates>) end;

  CPManeuverClass = interface(NSObjectClass)
    ['{6F326093-EDE6-47D3-A33C-508C0DAC3996}']
  end;

  CPManeuver = interface(NSObject)
    ['{E97ED9B8-0001-4593-BA94-9D49EC970A61}']
    function attributedInstructionVariants: NSArray; cdecl;
    function dashboardAttributedInstructionVariants: NSArray; cdecl;
    function dashboardInstructionVariants: NSArray; cdecl;
    function dashboardJunctionImage: UIImage; cdecl;
    function dashboardSymbolImage: UIImage; cdecl;
    function initialTravelEstimates: CPTravelEstimates; cdecl;
    function instructionVariants: NSArray; cdecl;
    function junctionImage: UIImage; cdecl;
    function notificationAttributedInstructionVariants: NSArray; cdecl;
    function notificationInstructionVariants: NSArray; cdecl;
    function notificationSymbolImage: UIImage; cdecl;
    procedure setAttributedInstructionVariants(attributedInstructionVariants: NSArray); cdecl;
    procedure setDashboardAttributedInstructionVariants(dashboardAttributedInstructionVariants: NSArray); cdecl;
    procedure setDashboardInstructionVariants(dashboardInstructionVariants: NSArray); cdecl;
    procedure setDashboardJunctionImage(dashboardJunctionImage: UIImage); cdecl;
    procedure setDashboardSymbolImage(dashboardSymbolImage: UIImage); cdecl;
    procedure setInitialTravelEstimates(initialTravelEstimates: CPTravelEstimates); cdecl;
    procedure setInstructionVariants(instructionVariants: NSArray); cdecl;
    procedure setJunctionImage(junctionImage: UIImage); cdecl;
    procedure setNotificationAttributedInstructionVariants(notificationAttributedInstructionVariants: NSArray); cdecl;
    procedure setNotificationInstructionVariants(notificationInstructionVariants: NSArray); cdecl;
    procedure setNotificationSymbolImage(notificationSymbolImage: UIImage); cdecl;
    procedure setSymbolImage(symbolImage: UIImage); cdecl;
    procedure setSymbolSet(symbolSet: CPImageSet); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("symbolImage", ios(12.0, 13.0))
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function symbolImage: UIImage; cdecl;
    function symbolSet: CPImageSet; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("symbolImage", ios(12.0, 13.0))
    function userInfo: Pointer; cdecl;
  end;
  TCPManeuver = class(TOCGenericImport<CPManeuverClass, CPManeuver>) end;

  CPMapButtonClass = interface(NSObjectClass)
    ['{CB97DE06-54B3-415F-86C7-1A7AFE08B190}']
  end;

  CPMapButton = interface(NSObject)
    ['{79BFAA84-65F4-40BA-819C-6A7D4E503D17}']
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
    ['{A819FD99-C08E-457E-A83E-4EFEC521726E}']
  end;

  CPNavigationAlert = interface(NSObject)
    ['{724A239E-E907-4F73-B076-6D620C12930D}']
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

  CPRouteChoiceClass = interface(NSObjectClass)
    ['{99042C15-88D4-4645-BFD5-850B8D14C33F}']
    {class} function new: Pointer; cdecl;
  end;

  CPRouteChoice = interface(NSObject)
    ['{8D2F0BE0-B046-4D35-906C-AA7C72A51964}']
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
    ['{499A18E9-A27D-4D8B-91B7-8796CC7C0201}']
    {class} function new: Pointer; cdecl;
  end;

  CPTrip = interface(NSObject)
    ['{AEEAB149-C56A-4CB1-A234-A5C3B85084C9}']
    function destination: MKMapItem; cdecl;
    function initWithOrigin(origin: MKMapItem; destination: MKMapItem; routeChoices: NSArray): Pointer; cdecl;
    function origin: MKMapItem; cdecl;
    function routeChoices: NSArray; cdecl;
    procedure setUserInfo(userInfo: Pointer); cdecl;
    function userInfo: Pointer; cdecl;
  end;
  TCPTrip = class(TOCGenericImport<CPTripClass, CPTrip>) end;

  CPNavigationSessionClass = interface(NSObjectClass)
    ['{96272CCD-C79E-4CEF-9FBD-C91217A09474}']
    {class} function new: Pointer; cdecl;
  end;

  CPNavigationSession = interface(NSObject)
    ['{17601BD6-40DA-4F2F-B274-7FC1E396A572}']
    procedure cancelTrip; cdecl;
    procedure finishTrip; cdecl;
    procedure pauseTripForReason(reason: CPTripPauseReason; description: NSString); cdecl;
    procedure setUpcomingManeuvers(upcomingManeuvers: NSArray); cdecl;
    function trip: CPTrip; cdecl;
    function upcomingManeuvers: NSArray; cdecl;
    procedure updateTravelEstimates(estimates: CPTravelEstimates; forManeuver: CPManeuver); cdecl;
  end;
  TCPNavigationSession = class(TOCGenericImport<CPNavigationSessionClass, CPNavigationSession>) end;

  CPTripPreviewTextConfigurationClass = interface(NSObjectClass)
    ['{52B1FD01-1310-4709-AE20-60D01E202AAB}']
  end;

  CPTripPreviewTextConfiguration = interface(NSObject)
    ['{82AA0C31-12E4-4CB9-B5B7-08D7752B820D}']
    function additionalRoutesButtonTitle: NSString; cdecl;
    function initWithStartButtonTitle(startButtonTitle: NSString; additionalRoutesButtonTitle: NSString;
      overviewButtonTitle: NSString): Pointer; cdecl;
    function overviewButtonTitle: NSString; cdecl;
    function startButtonTitle: NSString; cdecl;
  end;
  TCPTripPreviewTextConfiguration = class(TOCGenericImport<CPTripPreviewTextConfigurationClass, CPTripPreviewTextConfiguration>) end;

  CPMapTemplateClass = interface(CPTemplateClass)
    ['{4CCA1A76-4B63-4DF5-97B5-D908F98C2D9E}']
  end;

  CPMapTemplate = interface(CPTemplate)
    ['{4EED798F-4298-436A-99CB-F5F53F037B88}']
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
    ['{2811F7B5-3212-467C-B6E4-77662A23F38F}']
    procedure mapTemplateDidBeginPanGesture(mapTemplate: CPMapTemplate); cdecl;
    procedure mapTemplateDidCancelNavigation(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:didDismissNavigationAlert:dismissalContext:')]
    procedure mapTemplateDidDismissNavigationAlert(mapTemplate: CPMapTemplate; didDismissNavigationAlert: CPNavigationAlert;
      dismissalContext: CPNavigationAlertDismissalContext); cdecl;
    procedure mapTemplateDidDismissPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:didEndPanGestureWithVelocity:')]
    procedure mapTemplateDidEndPanGestureWithVelocity(mapTemplate: CPMapTemplate; didEndPanGestureWithVelocity: CGPoint); cdecl;
    [MethodName('mapTemplate:didShowNavigationAlert:')]
    procedure mapTemplateDidShowNavigationAlert(mapTemplate: CPMapTemplate; didShowNavigationAlert: CPNavigationAlert); cdecl;
    procedure mapTemplateDidShowPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:didUpdatePanGestureWithTranslation:velocity:')]
    procedure mapTemplateDidUpdatePanGestureWithTranslation(mapTemplate: CPMapTemplate; didUpdatePanGestureWithTranslation: CGPoint;
      velocity: CGPoint); cdecl;
    [MethodName('mapTemplate:displayStyleForManeuver:')]
    function mapTemplateDisplayStyleForManeuver(mapTemplate: CPMapTemplate; displayStyleForManeuver: CPManeuver): CPManeuverDisplayStyle; cdecl;
    [MethodName('mapTemplate:panBeganWithDirection:')]
    procedure mapTemplatePanBeganWithDirection(mapTemplate: CPMapTemplate; panBeganWithDirection: CPPanDirection); cdecl;
    [MethodName('mapTemplate:panEndedWithDirection:')]
    procedure mapTemplatePanEndedWithDirection(mapTemplate: CPMapTemplate; panEndedWithDirection: CPPanDirection); cdecl;
    [MethodName('mapTemplate:panWithDirection:')]
    procedure mapTemplatePanWithDirection(mapTemplate: CPMapTemplate; panWithDirection: CPPanDirection); cdecl;
    [MethodName('mapTemplate:selectedPreviewForTrip:usingRouteChoice:')]
    procedure mapTemplateSelectedPreviewForTrip(mapTemplate: CPMapTemplate; selectedPreviewForTrip: CPTrip; usingRouteChoice: CPRouteChoice); cdecl;
    [MethodName('mapTemplate:shouldShowNotificationForManeuver:')]
    function mapTemplateShouldShowNotificationForManeuver(mapTemplate: CPMapTemplate; shouldShowNotificationForManeuver: CPManeuver): Boolean; cdecl;
    [MethodName('mapTemplate:shouldShowNotificationForNavigationAlert:')]
    function mapTemplateShouldShowNotificationForNavigationAlert(mapTemplate: CPMapTemplate;
      shouldShowNotificationForNavigationAlert: CPNavigationAlert): Boolean; cdecl;
    [MethodName('mapTemplate:shouldUpdateNotificationForManeuver:withTravelEstimates:')]
    function mapTemplateShouldUpdateNotificationForManeuver(mapTemplate: CPMapTemplate; shouldUpdateNotificationForManeuver: CPManeuver;
      withTravelEstimates: CPTravelEstimates): Boolean; cdecl;
    [MethodName('mapTemplate:startedTrip:usingRouteChoice:')]
    procedure mapTemplateStartedTrip(mapTemplate: CPMapTemplate; startedTrip: CPTrip; usingRouteChoice: CPRouteChoice); cdecl;
    [MethodName('mapTemplate:willDismissNavigationAlert:dismissalContext:')]
    procedure mapTemplateWillDismissNavigationAlert(mapTemplate: CPMapTemplate; willDismissNavigationAlert: CPNavigationAlert;
      dismissalContext: CPNavigationAlertDismissalContext); cdecl;
    procedure mapTemplateWillDismissPanningInterface(mapTemplate: CPMapTemplate); cdecl;
    [MethodName('mapTemplate:willShowNavigationAlert:')]
    procedure mapTemplateWillShowNavigationAlert(mapTemplate: CPMapTemplate; willShowNavigationAlert: CPNavigationAlert); cdecl;
  end;

  CPMessageComposeBarButtonClass = interface(CPBarButtonClass)
    ['{85CD919A-4850-42B4-987C-C9E164C2B011}']
    {class} function new: Pointer; cdecl;
  end;

  CPMessageComposeBarButton = interface(CPBarButton)
    ['{D479D8C8-3AE7-4E65-A52C-C3C18EE9AB83}']
    function initWithImage(image: UIImage): Pointer; overload; cdecl;
    function initWithImage(image: UIImage; handler: CPBarButtonHandler): Pointer; overload; cdecl;
    function initWithTitle(title: NSString; handler: CPBarButtonHandler): Pointer; cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TCPMessageComposeBarButton = class(TOCGenericImport<CPMessageComposeBarButtonClass, CPMessageComposeBarButton>) end;

  CPMessageListItemLeadingConfigurationClass = interface(NSObjectClass)
    ['{988E3337-3D6C-4310-B112-F368381046A2}']
  end;

  CPMessageListItemLeadingConfiguration = interface(NSObject)
    ['{27C2EF22-46BF-4B96-9693-14A73A8A7E53}']
    function initWithLeadingItem(leadingItem: CPMessageLeadingItem; leadingImage: UIImage; unread: Boolean): Pointer; cdecl;
    function isUnread: Boolean; cdecl;
    function leadingImage: UIImage; cdecl;
    function leadingItem: CPMessageLeadingItem; cdecl;
  end;
  TCPMessageListItemLeadingConfiguration = class(TOCGenericImport<CPMessageListItemLeadingConfigurationClass,
    CPMessageListItemLeadingConfiguration>) end;

  CPMessageListItemTrailingConfigurationClass = interface(NSObjectClass)
    ['{C07BFBB0-EEE3-4DE0-B0C6-F80F39E1F902}']
  end;

  CPMessageListItemTrailingConfiguration = interface(NSObject)
    ['{EC795DC0-B04C-43A8-AEEB-07B1A3922AE2}']
    function initWithTrailingItem(trailingItem: CPMessageTrailingItem; trailingImage: UIImage): Pointer; cdecl;
    function trailingImage: UIImage; cdecl;
    function trailingItem: CPMessageTrailingItem; cdecl;
  end;
  TCPMessageListItemTrailingConfiguration = class(TOCGenericImport<CPMessageListItemTrailingConfigurationClass,
    CPMessageListItemTrailingConfiguration>) end;

  CPMessageListItemClass = interface(NSObjectClass)
    ['{78EC7FD7-936E-4BE4-808D-BC6E5125B628}']
  end;

  CPMessageListItem = interface(NSObject)
    ['{CB1319F1-522C-4D37-88B9-783CCDBFBF65}']
    function conversationIdentifier: NSString; cdecl;
    function detailText: NSString; cdecl;
    function initWithConversationIdentifier(conversationIdentifier: NSString; text: NSString;
      leadingConfiguration: CPMessageListItemLeadingConfiguration; trailingConfiguration: CPMessageListItemTrailingConfiguration;
      detailText: NSString; trailingText: NSString): Pointer; cdecl;
    function initWithFullName(fullName: NSString; phoneOrEmailAddress: NSString; leadingConfiguration: CPMessageListItemLeadingConfiguration;
      trailingConfiguration: CPMessageListItemTrailingConfiguration; detailText: NSString; trailingText: NSString): Pointer; cdecl;
    function leadingConfiguration: CPMessageListItemLeadingConfiguration; cdecl;
    function phoneOrEmailAddress: NSString; cdecl;
    procedure setConversationIdentifier(conversationIdentifier: NSString); cdecl;
    procedure setDetailText(detailText: NSString); cdecl;
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
    ['{B1B31715-D8EF-457B-83EB-CFB5817A984C}']
    {class} function new: Pointer; cdecl;
  end;

  CPNowPlayingButton = interface(NSObject)
    ['{A4B6E29E-00CF-4368-B7A9-D1F38AA60F96}']
    function initWithHandler(handler: TCPNowPlayingButtonBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    function isSelected: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setSelected(selected: Boolean); cdecl;
  end;
  TCPNowPlayingButton = class(TOCGenericImport<CPNowPlayingButtonClass, CPNowPlayingButton>) end;

  CPNowPlayingShuffleButtonClass = interface(CPNowPlayingButtonClass)
    ['{0AFB3923-C8DA-46BC-9C53-DBD165F1BDF7}']
  end;

  CPNowPlayingShuffleButton = interface(CPNowPlayingButton)
    ['{1C480FD4-BF93-4B5D-89C2-89975451D2B1}']
  end;
  TCPNowPlayingShuffleButton = class(TOCGenericImport<CPNowPlayingShuffleButtonClass, CPNowPlayingShuffleButton>) end;

  CPNowPlayingAddToLibraryButtonClass = interface(CPNowPlayingButtonClass)
    ['{B8A0EC2D-6917-4914-A325-AF5FDC133EF9}']
  end;

  CPNowPlayingAddToLibraryButton = interface(CPNowPlayingButton)
    ['{504990FA-570B-4D62-8A47-49CC7E5B9FF7}']
  end;
  TCPNowPlayingAddToLibraryButton = class(TOCGenericImport<CPNowPlayingAddToLibraryButtonClass, CPNowPlayingAddToLibraryButton>) end;

  CPNowPlayingMoreButtonClass = interface(CPNowPlayingButtonClass)
    ['{AC837269-8D72-4B68-9B82-4EFDB4E34B0F}']
  end;

  CPNowPlayingMoreButton = interface(CPNowPlayingButton)
    ['{47AC44AD-3291-42A9-BE03-2FC091321FE7}']
  end;
  TCPNowPlayingMoreButton = class(TOCGenericImport<CPNowPlayingMoreButtonClass, CPNowPlayingMoreButton>) end;

  CPNowPlayingPlaybackRateButtonClass = interface(CPNowPlayingButtonClass)
    ['{3BF3EE93-DD93-401B-B4AE-66073305E452}']
  end;

  CPNowPlayingPlaybackRateButton = interface(CPNowPlayingButton)
    ['{E2AB42CE-68CB-4518-930F-4D956F1944C4}']
  end;
  TCPNowPlayingPlaybackRateButton = class(TOCGenericImport<CPNowPlayingPlaybackRateButtonClass, CPNowPlayingPlaybackRateButton>) end;

  CPNowPlayingRepeatButtonClass = interface(CPNowPlayingButtonClass)
    ['{13F3C5ED-205A-4D63-9999-E0057B292E95}']
  end;

  CPNowPlayingRepeatButton = interface(CPNowPlayingButton)
    ['{BF11F0AF-8C1A-4664-98DF-A92B7E3C0009}']
  end;
  TCPNowPlayingRepeatButton = class(TOCGenericImport<CPNowPlayingRepeatButtonClass, CPNowPlayingRepeatButton>) end;

  CPNowPlayingImageButtonClass = interface(CPNowPlayingButtonClass)
    ['{55A7806B-C085-4121-A06D-AA029C344385}']
  end;

  CPNowPlayingImageButton = interface(CPNowPlayingButton)
    ['{5CC9E2F5-FB7D-4CBD-8560-C53DBDAC65E0}']
    function image: UIImage; cdecl;
    function initWithImage(image: UIImage; handler: TCPNowPlayingImageButtonBlockMethod1): Pointer; cdecl;
  end;
  TCPNowPlayingImageButton = class(TOCGenericImport<CPNowPlayingImageButtonClass, CPNowPlayingImageButton>) end;

  CPNowPlayingTemplateObserver = interface(IObjectiveC)
    ['{D039B3EF-CA1E-4320-B6FE-AD897B2F11F6}']
    procedure nowPlayingTemplateAlbumArtistButtonTapped(nowPlayingTemplate: CPNowPlayingTemplate); cdecl;
    procedure nowPlayingTemplateUpNextButtonTapped(nowPlayingTemplate: CPNowPlayingTemplate); cdecl;
  end;

  CPNowPlayingTemplateClass = interface(CPTemplateClass)
    ['{10C757E0-81AD-4ABE-B491-C0F5EC6556F7}']
    {class} function new: Pointer; cdecl;
    {class} function sharedTemplate: CPNowPlayingTemplate; cdecl;
  end;

  CPNowPlayingTemplate = interface(CPTemplate)
    ['{FFF5F725-D0B5-40F1-9B44-CDD301F5DFEF}']
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
    ['{639C18F9-574F-4175-8FB2-7DC8A325A330}']
    {class} function new: Pointer; cdecl;
  end;

  CPPointOfInterest = interface(NSObject)
    ['{48625484-BB26-48BF-9209-6BB861B783B3}']
    function detailSubtitle: NSString; cdecl;
    function detailSummary: NSString; cdecl;
    function detailTitle: NSString; cdecl;
    function initWithLocation(location: MKMapItem; title: NSString; subtitle: NSString; summary: NSString; detailTitle: NSString;
      detailSubtitle: NSString; detailSummary: NSString; pinImage: UIImage): Pointer; cdecl;
    function location: MKMapItem; cdecl;
    function pinImage: UIImage; cdecl;
    function primaryButton: CPTextButton; cdecl;
    function secondaryButton: CPTextButton; cdecl;
    procedure setDetailSubtitle(detailSubtitle: NSString); cdecl;
    procedure setDetailSummary(detailSummary: NSString); cdecl;
    procedure setDetailTitle(detailTitle: NSString); cdecl;
    procedure setLocation(location: MKMapItem); cdecl;
    procedure setPinImage(pinImage: UIImage); cdecl;
    procedure setPrimaryButton(primaryButton: CPTextButton); cdecl;
    procedure setSecondaryButton(secondaryButton: CPTextButton); cdecl;
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
    ['{DD134B2C-F978-4EAA-AC02-327E63FFB0C0}']
    procedure pointOfInterestTemplate(pointOfInterestTemplate: CPPointOfInterestTemplate;
      didChangeMapRegion: MKCoordinateRegion); overload; cdecl;
    procedure pointOfInterestTemplate(pointOfInterestTemplate: CPPointOfInterestTemplate;
      didSelectPointOfInterest: CPPointOfInterest); overload; cdecl;
  end;

  CPPointOfInterestTemplateClass = interface(CPTemplateClass)
    ['{B43BDC98-AF7E-43B7-9375-145B381CB59D}']
    {class} function new: Pointer; cdecl;
  end;

  CPPointOfInterestTemplate = interface(CPTemplate)
    ['{798FA4FC-05E0-4580-8901-E4461CFD56E2}']
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
    ['{B76AC7BB-FA63-49E1-86F6-5E9D0BAD03D0}']
  end;

  CPSearchTemplate = interface(CPTemplate)
    ['{BC4DE66C-01E1-4D8C-9D05-EC3D7F88EC3F}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPSearchTemplate = class(TOCGenericImport<CPSearchTemplateClass, CPSearchTemplate>) end;

  CPSearchTemplateDelegate = interface(IObjectiveC)
    ['{5FD6EAD8-E803-4B9A-AD10-A991CED4F152}']
    procedure searchTemplate(searchTemplate: CPSearchTemplate; selectedResult: CPListItem;
      completionHandler: TCPSearchTemplateDelegateBlockMethod2); overload; cdecl;
    procedure searchTemplate(searchTemplate: CPSearchTemplate; updatedSearchText: NSString;
      completionHandler: TCPSearchTemplateDelegateBlockMethod1); overload; cdecl;
    procedure searchTemplateSearchButtonPressed(searchTemplate: CPSearchTemplate); cdecl;
  end;

  CPSessionConfigurationClass = interface(NSObjectClass)
    ['{E7ED4E47-93B9-40E9-B691-928DC71F722C}']
    {class} function new: Pointer; cdecl;
  end;

  CPSessionConfiguration = interface(NSObject)
    ['{5A3F1A7B-FEDA-4D88-953F-32DA59ED359D}']
    function contentStyle: CPContentStyle; cdecl;
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function limitedUserInterfaces: CPLimitableUserInterface; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPSessionConfiguration = class(TOCGenericImport<CPSessionConfigurationClass, CPSessionConfiguration>) end;

  CPSessionConfigurationDelegate = interface(IObjectiveC)
    ['{88D2EC3F-C60C-4D19-B5DC-F586F22BC300}']
    procedure sessionConfiguration(sessionConfiguration: CPSessionConfiguration; limitedUserInterfacesChanged: CPLimitableUserInterface); cdecl;
    [MethodName('sessionConfiguration:contentStyleChanged:')]
    procedure sessionConfigurationContentStyleChanged(sessionConfiguration: CPSessionConfiguration; contentStyleChanged: CPContentStyle); cdecl;
  end;

  CPTabBarTemplateClass = interface(CPTemplateClass)
    ['{C0731C0A-7263-40AE-AA21-4CE29A698C2C}']
    {class} function maximumTabCount: NSUInteger; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CPTabBarTemplate = interface(CPTemplate)
    ['{B4B8B842-8BF3-45B9-BA99-9DBBACE387BE}']
    function delegate: Pointer; cdecl;
    function initWithTemplates(templates: NSArray): Pointer; cdecl;
    function selectedTemplate: CPTemplate; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function templates: NSArray; cdecl;
    procedure updateTemplates(newTemplates: NSArray); cdecl;
  end;
  TCPTabBarTemplate = class(TOCGenericImport<CPTabBarTemplateClass, CPTabBarTemplate>) end;

  CPTabBarTemplateDelegate = interface(IObjectiveC)
    ['{6CFA512D-D9FF-400E-BB48-9F1811948304}']
    procedure tabBarTemplate(tabBarTemplate: CPTabBarTemplate; didSelectTemplate: CPTemplate); cdecl;
  end;

  CPTemplateApplicationDashboardSceneDelegate = interface(IObjectiveC)
    ['{33E689EC-385F-4CF9-8B7E-53D66759A648}']
    [MethodName('templateApplicationDashboardScene:didConnectDashboardController:toWindow:')]
    procedure templateApplicationDashboardSceneDidConnectDashboardController(templateApplicationDashboardScene: CPTemplateApplicationDashboardScene;
      didConnectDashboardController: CPDashboardController; toWindow: UIWindow); cdecl;
    [MethodName('templateApplicationDashboardScene:didDisconnectDashboardController:fromWindow:')]
    procedure templateApplicationDashboardSceneDidDisconnectDashboardController(templateApplicationDashboardScene: CPTemplateApplicationDashboardScene;
      didDisconnectDashboardController: CPDashboardController; fromWindow: UIWindow); cdecl;
  end;

  CPTemplateApplicationDashboardSceneClass = interface(UISceneClass)
    ['{0608296F-DFD9-49F8-83C3-F4E6286153C9}']
  end;

  CPTemplateApplicationDashboardScene = interface(UIScene)
    ['{F3FCA820-E600-4624-86D3-B2F3E9C96566}']
    function dashboardController: CPDashboardController; cdecl;
    function dashboardWindow: UIWindow; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPTemplateApplicationDashboardScene = class(TOCGenericImport<CPTemplateApplicationDashboardSceneClass, CPTemplateApplicationDashboardScene>) end;

  CPTemplateApplicationSceneDelegate = interface(IObjectiveC)
    ['{20720B5A-1DE3-487F-8E9F-285FDF4CE592}']
    [MethodName('templateApplicationScene:didConnectInterfaceController:')]
    procedure templateApplicationSceneDidConnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didConnectInterfaceController: CPInterfaceController); overload; cdecl;
    [MethodName('templateApplicationScene:didConnectInterfaceController:toWindow:')]
    procedure templateApplicationSceneDidConnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didConnectInterfaceController: CPInterfaceController; toWindow: CPWindow); overload; cdecl;
    [MethodName('templateApplicationScene:didDisconnectInterfaceController:')]
    procedure templateApplicationSceneDidDisconnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didDisconnectInterfaceController: CPInterfaceController); overload; cdecl;
    [MethodName('templateApplicationScene:didDisconnectInterfaceController:fromWindow:')]
    procedure templateApplicationSceneDidDisconnectInterfaceController(templateApplicationScene: CPTemplateApplicationScene;
      didDisconnectInterfaceController: CPInterfaceController; fromWindow: CPWindow); overload; cdecl;
    [MethodName('templateApplicationScene:didSelectManeuver:')]
    procedure templateApplicationSceneDidSelectManeuver(templateApplicationScene: CPTemplateApplicationScene; didSelectManeuver: CPManeuver); cdecl;
    [MethodName('templateApplicationScene:didSelectNavigationAlert:')]
    procedure templateApplicationSceneDidSelectNavigationAlert(templateApplicationScene: CPTemplateApplicationScene;
      didSelectNavigationAlert: CPNavigationAlert); cdecl;
  end;

  CPTemplateApplicationSceneClass = interface(UISceneClass)
    ['{5432A3EB-A16F-4F9F-8FC2-966C7F403E93}']
  end;

  CPTemplateApplicationScene = interface(UIScene)
    ['{D77B4382-B8CD-476F-99A5-847881600618}']
    function carWindow: CPWindow; cdecl;
    function delegate: Pointer; cdecl;
    function interfaceController: CPInterfaceController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCPTemplateApplicationScene = class(TOCGenericImport<CPTemplateApplicationSceneClass, CPTemplateApplicationScene>) end;

  CPVoiceControlStateClass = interface(NSObjectClass)
    ['{4D04FB0F-D106-41CF-9862-651BDF34F8C3}']
  end;

  CPVoiceControlState = interface(NSObject)
    ['{7C4E78F9-5C20-4509-8D88-44C2D9DB1EF2}']
    function identifier: NSString; cdecl;
    function image: UIImage; cdecl;
    function initWithIdentifier(identifier: NSString; titleVariants: NSArray; image: UIImage; repeats: Boolean): Pointer; cdecl;
    function repeats: Boolean; cdecl;
    function titleVariants: NSArray; cdecl;
  end;
  TCPVoiceControlState = class(TOCGenericImport<CPVoiceControlStateClass, CPVoiceControlState>) end;

  CPVoiceControlTemplateClass = interface(CPTemplateClass)
    ['{7DA05ED4-4184-4AC0-9879-1A061CA48D2E}']
  end;

  CPVoiceControlTemplate = interface(CPTemplate)
    ['{E44195C8-CD5D-423F-B509-ED492F4493B9}']
    procedure activateVoiceControlStateWithIdentifier(identifier: NSString); cdecl;
    function activeStateIdentifier: NSString; cdecl;
    function initWithVoiceControlStates(voiceControlStates: NSArray): Pointer; cdecl;
    function voiceControlStates: NSArray; cdecl;
  end;
  TCPVoiceControlTemplate = class(TOCGenericImport<CPVoiceControlTemplateClass, CPVoiceControlTemplate>) end;

function CarPlayErrorDomain: NSString;
function CPTemplateApplicationDashboardSceneSessionRoleApplication: UISceneSessionRole;
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

function CPTemplateApplicationSceneSessionRoleApplication: UISceneSessionRole;
begin
  Result := CocoaNSStringConst(libCarPlay, 'CPTemplateApplicationSceneSessionRoleApplication');
end;

initialization
  CarPlayModule := dlopen(MarshaledAString(libCarPlay), RTLD_LAZY);

finalization
  dlclose(CarPlayModule)

end.