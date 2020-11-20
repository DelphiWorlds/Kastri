unit DW.iOSapi.UIKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.QuartzCore, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.UserNotifications,
  // DW
  DW.iOSapi.Foundation;

const
  UIAlertActionStyleDefault = 0;
  UIAlertActionStyleCancel = 1;
  UIAlertActionStyleDestructive = 2;
  UIAlertControllerStyleActionSheet = 0;
  UIAlertControllerStyleAlert = 1;
  UITableViewCellStateEditingMask = UITableViewCellStateShowingEditControlMask;
  UIMenuElementStateOff = 0;
  UIMenuElementStateOn = 1;
  UIMenuElementStateMixed = 2;
  UIMenuElementAttributesDisabled = 1;
  UIMenuElementAttributesDestructive = 2;
  UIMenuElementAttributesHidden = 4;
  UIMenuOptionsDisplayInline = 1;
  UIMenuOptionsDestructive = 2;
  UIEventTypePresses = 3;
  UIEventTypeScroll = 10;
  UIEventTypeHover = 11;
  UIEventTypeTransform = 14;
  UIEventButtonMaskPrimary = 1;
  UIEventButtonMaskSecondary = 2;
  UIEditingInteractionConfigurationNone = 0;
  UIEditingInteractionConfigurationDefault = 1;
  UITraitEnvironmentLayoutDirectionUnspecified = -1;
  UITraitEnvironmentLayoutDirectionLeftToRight = UIUserInterfaceLayoutDirectionLeftToRight;
  UITraitEnvironmentLayoutDirectionRightToLeft = UIUserInterfaceLayoutDirectionRightToLeft;
  UIDisplayGamutUnspecified = -1;
  UIDisplayGamutSRGB = 0;
  UIDisplayGamutP3 = 1;
  UIAccessibilityContrastUnspecified = -1;
  UIAccessibilityContrastNormal = 0;
  UIAccessibilityContrastHigh = 1;
  UILegibilityWeightUnspecified = -1;
  UILegibilityWeightRegular = 0;
  UILegibilityWeightBold = 1;
  UIUserInterfaceLevelUnspecified = -1;
  UIUserInterfaceLevelBase = 0;
  UIUserInterfaceLevelElevated = 1;
  UIUserInterfaceActiveAppearanceUnspecified = -1;
  UIUserInterfaceActiveAppearanceInactive = 0;
  UIUserInterfaceActiveAppearanceActive = 1;
  UIUserInterfaceIdiomUnspecified = -1;
  UIUserInterfaceIdiomTV = 2;
  UIUserInterfaceIdiomCarPlay = 3;
  UIUserInterfaceIdiomMac = 5;
  UIAxisNeither = 0;
  UIAxisHorizontal = 1;
  UIAxisVertical = 2;
  UIAxisBoth = UIAxisHorizontal  or  UIAxisVertical;
  NSDirectionalRectEdgeNone = 0;
  NSDirectionalRectEdgeTop = 1;
  NSDirectionalRectEdgeLeading = 2;
  NSDirectionalRectEdgeBottom = 4;
  NSDirectionalRectEdgeTrailing = 8;
  NSDirectionalRectEdgeAll = 15;
  UIDirectionalRectEdgeNone = 0;
  UIDirectionalRectEdgeTop = 1;
  UIDirectionalRectEdgeLeading = 2;
  UIDirectionalRectEdgeBottom = 4;
  UIDirectionalRectEdgeTrailing = 8;
  UIDirectionalRectEdgeAll = 15;
  NSRectAlignmentNone = 0;
  NSRectAlignmentTop = 1;
  NSRectAlignmentTopLeading = 2;
  NSRectAlignmentLeading = 3;
  NSRectAlignmentBottomLeading = 4;
  NSRectAlignmentBottom = 5;
  NSRectAlignmentBottomTrailing = 6;
  NSRectAlignmentTrailing = 7;
  NSRectAlignmentTopTrailing = 8;
  UIDynamicItemCollisionBoundsTypeRectangle = 0;
  UIDynamicItemCollisionBoundsTypeEllipse = 1;
  UIDynamicItemCollisionBoundsTypePath = 2;
  NSLayoutAttributeLastBaseline = 11;
  NSLayoutAttributeFirstBaseline = 12;
  NSLayoutFormatAlignAllLastBaseline = 2048;
  NSLayoutFormatAlignAllFirstBaseline = 4096;
  NSLayoutFormatSpacingEdgeToEdge = 0;
  NSLayoutFormatSpacingBaselineToBaseline = 524288;
  NSLayoutFormatSpacingMask = 524288;
  UITouchPhaseRegionEntered = 5;
  UITouchPhaseRegionMoved = 6;
  UITouchPhaseRegionExited = 7;
  UIForceTouchCapabilityUnknown = 0;
  UIForceTouchCapabilityUnavailable = 1;
  UIForceTouchCapabilityAvailable = 2;
  UITouchTypeDirect = 0;
  UITouchTypeIndirect = 1;
  UITouchTypePencil = 2;
  UITouchTypeStylus = UITouchTypePencil;
  UITouchTypeIndirectPointer = 3;
  UITouchPropertyForce = 1;
  UITouchPropertyAzimuth = 2;
  UITouchPropertyAltitude = 4;
  UITouchPropertyLocation = 8;
  UIFocusHeadingNone = 0;
  UIFocusHeadingUp = 1;
  UIFocusHeadingDown = 2;
  UIFocusHeadingLeft = 4;
  UIFocusHeadingRight = 8;
  UIFocusHeadingNext = 16;
  UIFocusHeadingPrevious = 32;
  UIViewAnimationOptionPreferredFramesPerSecondDefault = 0;
  UIViewAnimationOptionPreferredFramesPerSecond60 = 50331648;
  UIViewAnimationOptionPreferredFramesPerSecond30 = 117440512;
  UISemanticContentAttributeUnspecified = 0;
  UISemanticContentAttributePlayback = 1;
  UISemanticContentAttributeSpatial = 2;
  UISemanticContentAttributeForceLeftToRight = 3;
  UISemanticContentAttributeForceRightToLeft = 4;
  UIContextMenuInteractionCommitStyleDismiss = 0;
  UIContextMenuInteractionCommitStylePop = 1;
  UIContextMenuInteractionAppearanceUnknown = 0;
  UIContextMenuInteractionAppearanceRich = 1;
  UIContextMenuInteractionAppearanceCompact = 2;
  UIControlEventPrimaryActionTriggered = 8192;
  UIControlEventMenuActionTriggered = 16384;
  UIControlContentHorizontalAlignmentLeading = 4;
  UIControlContentHorizontalAlignmentTrailing = 5;
  UIControlStateFocused = 8;
  NSLineBreakStrategyNone = 0;
  NSLineBreakStrategyPushOut = 1;
  NSLineBreakStrategyHangulWordPriority = 2;
  NSLineBreakStrategyStandard = 65535;
  UIDropOperationCancel = 0;
  UIDropOperationForbidden = 1;
  UIDropOperationCopy = 2;
  UIDropOperationMove = 3;
  UIViewAnimatingStateInactive = 0;
  UIViewAnimatingStateActive = 1;
  UIViewAnimatingStateStopped = 2;
  UIViewAnimatingPositionEnd = 0;
  UIViewAnimatingPositionStart = 1;
  UIViewAnimatingPositionCurrent = 2;
  UITextSmartQuotesTypeDefault = 0;
  UITextSmartQuotesTypeNo = 1;
  UITextSmartQuotesTypeYes = 2;
  UITextSmartDashesTypeDefault = 0;
  UITextSmartDashesTypeNo = 1;
  UITextSmartDashesTypeYes = 2;
  UITextSmartInsertDeleteTypeDefault = 0;
  UITextSmartInsertDeleteTypeNo = 1;
  UITextSmartInsertDeleteTypeYes = 2;
  UIKeyboardTypeASCIICapableNumberPad = 11;
  UIReturnKeyContinue = 11;
  UITextAlternativeStyleNone = 0;
  UITextAlternativeStyleLowConfidence = 1;
  UITextDragOptionsNone = 0;
  UITextDragOptionStripTextColorFromPreviews = 1 shl 0;
  UITextDropActionInsert = 0;
  UITextDropActionReplaceSelection = 1;
  UITextDropActionReplaceAll = 2;
  UITextDropProgressModeSystem = 0;
  UITextDropProgressModeCustom = 1;
  UITextDropPerformerView = 0;
  UITextDropPerformerDelegate = 1;
  UITextDropEditabilityNo = 0;
  UITextDropEditabilityTemporary = 1;
  UITextDropEditabilityYes = 2;
  UITextFieldDidEndEditingReasonCommitted = 0;
  UITextFieldDidEndEditingReasonCancelled = 1;
  UISceneActivationStateUnattached = -1;
  UISceneActivationStateForegroundActive = 0;
  UISceneActivationStateForegroundInactive = 1;
  UISceneActivationStateBackground = 2;
  UISceneErrorCodeMultipleScenesNotSupported = 0;
  UISceneErrorCodeRequestDenied = 1;
  UIStatusBarStyleDarkContent = 3;
  UIInterfaceOrientationUnknown = UIDeviceOrientationUnknown;
  UIModalPresentationOverFullScreen = 5;
  UIModalPresentationOverCurrentContext = 6;
  UIModalPresentationPopover = 7;
  UIModalPresentationBlurOverFullScreen = 8;
  UIModalPresentationAutomatic = -2;
  UIPreviewActionStyleDefault = 0;
  UIPreviewActionStyleSelected = 1;
  UIPreviewActionStyleDestructive = 2;
  UITimingCurveTypeBuiltin = 0;
  UITimingCurveTypeCubic = 1;
  UITimingCurveTypeSpring = 2;
  UITimingCurveTypeComposed = 3;
  UIDocumentBrowserErrorGeneric = 1;
  UIDocumentBrowserErrorNoLocationAvailable = 2;
  UIDocumentBrowserImportModeNone = 0;
  UIDocumentBrowserImportModeCopy = 1;
  UIDocumentBrowserImportModeMove = 2;
  UIDocumentBrowserUserInterfaceStyleWhite = 0;
  UIDocumentBrowserUserInterfaceStyleLight = 1;
  UIDocumentBrowserUserInterfaceStyleDark = 2;
  UIDocumentBrowserActionAvailabilityMenu = 1;
  UIDocumentBrowserActionAvailabilityNavigationBar = 2;
  NSUnderlineStylePatternSolid = 0;
  NSUnderlineStylePatternDot = 256;
  NSUnderlineStylePatternDash = 512;
  NSUnderlineStylePatternDashDot = 768;
  NSUnderlineStylePatternDashDotDot = 1024;
  NSUnderlineStyleByWord = 32768;
  NSWritingDirectionEmbedding = 0 shl 1;
  NSWritingDirectionOverride = 1 shl 1;
  NSTextScalingStandard = 0;
  NSTextScalingiOS = 1;
  UIPreferredPresentationStyleUnspecified = 0;
  UIPreferredPresentationStyleInline = 1;
  UIPreferredPresentationStyleAttachment = 2;
  NSControlCharacterActionZeroAdvancement = 1 shl 0;
  NSControlCharacterActionWhitespace = 1 shl 1;
  NSControlCharacterActionHorizontalTab = 1 shl 2;
  NSControlCharacterActionLineBreak = 1 shl 3;
  NSControlCharacterActionParagraphBreak = 1 shl 4;
  NSControlCharacterActionContainerBreak = 1 shl 5;
  UIScrollViewIndexDisplayModeAutomatic = 0;
  UIScrollViewIndexDisplayModeAlwaysHidden = 1;
  UIScrollViewContentInsetAdjustmentAutomatic = 0;
  UIScrollViewContentInsetAdjustmentScrollableAxes = 1;
  UIScrollViewContentInsetAdjustmentNever = 2;
  UIScrollViewContentInsetAdjustmentAlways = 3;
  UIAccessibilityNavigationStyleAutomatic = 0;
  UIAccessibilityNavigationStyleSeparate = 1;
  UIAccessibilityNavigationStyleCombined = 2;
  UIAccessibilityContainerTypeNone = 0;
  UIAccessibilityContainerTypeDataTable = 1;
  UIAccessibilityContainerTypeList = 2;
  UIAccessibilityContainerTypeLandmark = 3;
  UIAccessibilityContainerTypeSemanticGroup = 4;
  UIAccessibilityCustomRotorDirectionPrevious = 0;
  UIAccessibilityCustomRotorDirectionNext = 1;
  UIAccessibilityCustomSystemRotorTypeNone = 0;
  UIAccessibilityCustomSystemRotorTypeLink = 1;
  UIAccessibilityCustomSystemRotorTypeVisitedLink = 2;
  UIAccessibilityCustomSystemRotorTypeHeading = 3;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel1 = 4;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel2 = 5;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel3 = 6;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel4 = 7;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel5 = 8;
  UIAccessibilityCustomSystemRotorTypeHeadingLevel6 = 9;
  UIAccessibilityCustomSystemRotorTypeBoldText = 10;
  UIAccessibilityCustomSystemRotorTypeItalicText = 11;
  UIAccessibilityCustomSystemRotorTypeUnderlineText = 12;
  UIAccessibilityCustomSystemRotorTypeMisspelledWord = 13;
  UIAccessibilityCustomSystemRotorTypeImage = 14;
  UIAccessibilityCustomSystemRotorTypeTextField = 15;
  UIAccessibilityCustomSystemRotorTypeTable = 16;
  UIAccessibilityCustomSystemRotorTypeList = 17;
  UIAccessibilityCustomSystemRotorTypeLandmark = 18;
  UIGuidedAccessErrorPermissionDenied = 0;
  UIGuidedAccessErrorFailed = 2147483647;
  UIGuidedAccessAccessibilityFeatureVoiceOver = 1;
  UIGuidedAccessAccessibilityFeatureZoom = 2;
  UIGuidedAccessAccessibilityFeatureAssistiveTouch = 4;
  UIGuidedAccessAccessibilityFeatureInvertColors = 8;
  UIGuidedAccessAccessibilityFeatureGrayscaleDisplay = 16;
  UIAccessibilityHearingDeviceEarNone = 0;
  UIAccessibilityHearingDeviceEarLeft = 2;
  UIAccessibilityHearingDeviceEarRight = 4;
  UIAccessibilityHearingDeviceEarBoth = 6;
  UIButtonTypePlain = 6;
  UIButtonTypeClose = 7;
  UIButtonRoleNormal = 0;
  UIButtonRolePrimary = 1;
  UIButtonRoleCancel = 2;
  UIButtonRoleDestructive = 3;
  UIActivityIndicatorViewStyleMedium = 100;
  UIActivityIndicatorViewStyleLarge = 101;
  UIApplicationShortcutIconTypeCompose = 0;
  UIApplicationShortcutIconTypePlay = 1;
  UIApplicationShortcutIconTypePause = 2;
  UIApplicationShortcutIconTypeAdd = 3;
  UIApplicationShortcutIconTypeLocation = 4;
  UIApplicationShortcutIconTypeSearch = 5;
  UIApplicationShortcutIconTypeShare = 6;
  UIApplicationShortcutIconTypeProhibit = 7;
  UIApplicationShortcutIconTypeContact = 8;
  UIApplicationShortcutIconTypeHome = 9;
  UIApplicationShortcutIconTypeMarkLocation = 10;
  UIApplicationShortcutIconTypeFavorite = 11;
  UIApplicationShortcutIconTypeLove = 12;
  UIApplicationShortcutIconTypeCloud = 13;
  UIApplicationShortcutIconTypeInvitation = 14;
  UIApplicationShortcutIconTypeConfirmation = 15;
  UIApplicationShortcutIconTypeMail = 16;
  UIApplicationShortcutIconTypeMessage = 17;
  UIApplicationShortcutIconTypeDate = 18;
  UIApplicationShortcutIconTypeTime = 19;
  UIApplicationShortcutIconTypeCapturePhoto = 20;
  UIApplicationShortcutIconTypeCaptureVideo = 21;
  UIApplicationShortcutIconTypeTask = 22;
  UIApplicationShortcutIconTypeTaskCompleted = 23;
  UIApplicationShortcutIconTypeAlarm = 24;
  UIApplicationShortcutIconTypeBookmark = 25;
  UIApplicationShortcutIconTypeShuffle = 26;
  UIApplicationShortcutIconTypeAudio = 27;
  UIApplicationShortcutIconTypeUpdate = 28;
  UIBarMetricsCompact = 1;
  UIBarMetricsCompactPrompt = 102;
  UIBarButtonSystemItemClose = 24;
  UIBlurEffectStyleExtraLight = 0;
  UIBlurEffectStyleLight = 1;
  UIBlurEffectStyleDark = 2;
  UIBlurEffectStyleExtraDark = 3;
  UIBlurEffectStyleRegular = 4;
  UIBlurEffectStyleProminent = 5;
  UIBlurEffectStyleSystemUltraThinMaterial = 6;
  UIBlurEffectStyleSystemThinMaterial = 7;
  UIBlurEffectStyleSystemMaterial = 8;
  UIBlurEffectStyleSystemThickMaterial = 9;
  UIBlurEffectStyleSystemChromeMaterial = 10;
  UIBlurEffectStyleSystemUltraThinMaterialLight = 11;
  UIBlurEffectStyleSystemThinMaterialLight = 12;
  UIBlurEffectStyleSystemMaterialLight = 13;
  UIBlurEffectStyleSystemThickMaterialLight = 14;
  UIBlurEffectStyleSystemChromeMaterialLight = 15;
  UIBlurEffectStyleSystemUltraThinMaterialDark = 16;
  UIBlurEffectStyleSystemThinMaterialDark = 17;
  UIBlurEffectStyleSystemMaterialDark = 18;
  UIBlurEffectStyleSystemThickMaterialDark = 19;
  UIBlurEffectStyleSystemChromeMaterialDark = 20;
  UICellAccessoryDisplayedAlways = 0;
  UICellAccessoryDisplayedWhenEditing = 1;
  UICellAccessoryDisplayedWhenNotEditing = 2;
  UICellAccessoryOutlineDisclosureStyleAutomatic = 0;
  UICellAccessoryOutlineDisclosureStyleHeader = 1;
  UICellAccessoryOutlineDisclosureStyleCell = 2;
  UICellAccessoryPlacementLeading = 0;
  UICellAccessoryPlacementTrailing = 1;
  UICellConfigurationDragStateNone = 0;
  UICellConfigurationDragStateLifting = 1;
  UICellConfigurationDragStateDragging = 2;
  UICellConfigurationDropStateNone = 0;
  UICellConfigurationDropStateNotTargeted = 1;
  UICellConfigurationDropStateTargeted = 2;
  UICloudSharingPermissionStandard = 0;
  UICloudSharingPermissionAllowPublic = 1;
  UICloudSharingPermissionAllowPrivate = 2;
  UICloudSharingPermissionAllowReadOnly = 4;
  UICloudSharingPermissionAllowReadWrite = 8;
  UIContentInsetsReferenceAutomatic = 0;
  UIContentInsetsReferenceNone = 1;
  UIContentInsetsReferenceSafeArea = 2;
  UIContentInsetsReferenceLayoutMargins = 3;
  UIContentInsetsReferenceReadableContent = 4;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorNone = 0;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorContinuous = 1;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorContinuousGroupLeadingBoundary = 2;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorPaging = 3;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorGroupPaging = 4;
  UICollectionLayoutSectionOrthogonalScrollingBehaviorGroupPagingCentered = 5;
  UICollectionLayoutListAppearancePlain = 0;
  UICollectionLayoutListAppearanceGrouped = 1;
  UICollectionLayoutListAppearanceInsetGrouped = 2;
  UICollectionLayoutListAppearanceSidebar = 3;
  UICollectionLayoutListAppearanceSidebarPlain = 4;
  UICollectionLayoutListHeaderModeNone = 0;
  UICollectionLayoutListHeaderModeSupplementary = 1;
  UICollectionLayoutListHeaderModeFirstItemInSection = 2;
  UICollectionLayoutListFooterModeNone = 0;
  UICollectionLayoutListFooterModeSupplementary = 1;
  UICollectionViewReorderingCadenceImmediate = 0;
  UICollectionViewReorderingCadenceFast = 1;
  UICollectionViewReorderingCadenceSlow = 2;
  UICollectionViewDropIntentUnspecified = 0;
  UICollectionViewDropIntentInsertAtDestinationIndexPath = 1;
  UICollectionViewDropIntentInsertIntoDestinationIndexPath = 2;
  UICollectionViewCellDragStateNone = 0;
  UICollectionViewCellDragStateLifting = 1;
  UICollectionViewCellDragStateDragging = 2;
  UICollectionViewFlowLayoutSectionInsetFromContentInset = 0;
  UICollectionViewFlowLayoutSectionInsetFromSafeArea = 1;
  UICollectionViewFlowLayoutSectionInsetFromLayoutMargins = 2;
  UIContextualActionStyleNormal = 0;
  UIContextualActionStyleDestructive = 1;
  UIDataDetectorTypeShipmentTrackingNumber = 16;
  UIDataDetectorTypeFlightNumber = 32;
  UIDataDetectorTypeLookupSuggestion = 64;
  UIDatePickerStyleAutomatic = 0;
  UIDatePickerStyleWheels = 1;
  UIDatePickerStyleCompact = 2;
  UIDatePickerStyleInline = 3;
  UITableViewCellFocusStyleDefault = 0;
  UITableViewCellFocusStyleCustom = 1;
  UITableViewCellDragStateNone = 0;
  UITableViewCellDragStateLifting = 1;
  UITableViewCellDragStateDragging = 2;
  UITableViewStyleInsetGrouped = 2;
  UITableViewRowActionStyleDefault = 0;
  UITableViewRowActionStyleDestructive = UITableViewRowActionStyleDefault;
  UITableViewRowActionStyleNormal = 1;
  UITableViewSeparatorInsetFromCellEdges = 0;
  UITableViewSeparatorInsetFromAutomaticInsets = 1;
  UITableViewDropIntentUnspecified = 0;
  UITableViewDropIntentInsertAtDestinationIndexPath = 1;
  UITableViewDropIntentInsertIntoDestinationIndexPath = 2;
  UITableViewDropIntentAutomatic = 3;
  UIDocumentStateProgressAvailable = 16;
  UIDocumentPickerModeImport = 0;
  UIDocumentPickerModeOpen = 1;
  UIDocumentPickerModeExportToService = 2;
  UIDocumentPickerModeMoveToService = 3;
  UIDocumentMenuOrderFirst = 0;
  UIDocumentMenuOrderLast = 1;
  UIDropSessionProgressIndicatorStyleNone = 0;
  UIDropSessionProgressIndicatorStyleDefault = 1;
  UIGraphicsImageRendererFormatRangeUnspecified = -1;
  UIGraphicsImageRendererFormatRangeAutomatic = 0;
  UIGraphicsImageRendererFormatRangeExtended = 1;
  UIGraphicsImageRendererFormatRangeStandard = 2;
  UIScrollTypeDiscrete = 0;
  UIScrollTypeContinuous = 1;
  UIScrollTypeMaskDiscrete = 1;
  UIScrollTypeMaskContinuous = 2;
  UIScrollTypeMaskAll = 3;
  UIImagePickerControllerImageURLExportPresetCompatible = 0;
  UIImagePickerControllerImageURLExportPresetCurrent = 1;
  UIImageSymbolScaleDefault = -1;
  UIImageSymbolScaleUnspecified = 0;
  UIImageSymbolScaleSmall = 1;
  UIImageSymbolScaleMedium = 2;
  UIImageSymbolScaleLarge = 3;
  UIImageSymbolWeightUnspecified = 0;
  UIImageSymbolWeightUltraLight = 1;
  UIImageSymbolWeightThin = 2;
  UIImageSymbolWeightLight = 3;
  UIImageSymbolWeightRegular = 4;
  UIImageSymbolWeightMedium = 5;
  UIImageSymbolWeightSemibold = 6;
  UIImageSymbolWeightBold = 7;
  UIImageSymbolWeightHeavy = 8;
  UIImageSymbolWeightBlack = 9;
  UIImpactFeedbackStyleLight = 0;
  UIImpactFeedbackStyleMedium = 1;
  UIImpactFeedbackStyleHeavy = 2;
  UIImpactFeedbackStyleSoft = 3;
  UIImpactFeedbackStyleRigid = 4;
  UIKeyboardHIDUsageKeyboardErrorRollOver = 1;
  UIKeyboardHIDUsageKeyboardPOSTFail = 2;
  UIKeyboardHIDUsageKeyboardErrorUndefined = 3;
  UIKeyboardHIDUsageKeyboardA = 4;
  UIKeyboardHIDUsageKeyboardB = 5;
  UIKeyboardHIDUsageKeyboardC = 6;
  UIKeyboardHIDUsageKeyboardD = 7;
  UIKeyboardHIDUsageKeyboardE = 8;
  UIKeyboardHIDUsageKeyboardF = 9;
  UIKeyboardHIDUsageKeyboardG = 10;
  UIKeyboardHIDUsageKeyboardH = 11;
  UIKeyboardHIDUsageKeyboardI = 12;
  UIKeyboardHIDUsageKeyboardJ = 13;
  UIKeyboardHIDUsageKeyboardK = 14;
  UIKeyboardHIDUsageKeyboardL = 15;
  UIKeyboardHIDUsageKeyboardM = 16;
  UIKeyboardHIDUsageKeyboardN = 17;
  UIKeyboardHIDUsageKeyboardO = 18;
  UIKeyboardHIDUsageKeyboardP = 19;
  UIKeyboardHIDUsageKeyboardQ = 20;
  UIKeyboardHIDUsageKeyboardR = 21;
  UIKeyboardHIDUsageKeyboardS = 22;
  UIKeyboardHIDUsageKeyboardT = 23;
  UIKeyboardHIDUsageKeyboardU = 24;
  UIKeyboardHIDUsageKeyboardV = 25;
  UIKeyboardHIDUsageKeyboardW = 26;
  UIKeyboardHIDUsageKeyboardX = 27;
  UIKeyboardHIDUsageKeyboardY = 28;
  UIKeyboardHIDUsageKeyboardZ = 29;
  UIKeyboardHIDUsageKeyboard1 = 30;
  UIKeyboardHIDUsageKeyboard2 = 31;
  UIKeyboardHIDUsageKeyboard3 = 32;
  UIKeyboardHIDUsageKeyboard4 = 33;
  UIKeyboardHIDUsageKeyboard5 = 34;
  UIKeyboardHIDUsageKeyboard6 = 35;
  UIKeyboardHIDUsageKeyboard7 = 36;
  UIKeyboardHIDUsageKeyboard8 = 37;
  UIKeyboardHIDUsageKeyboard9 = 38;
  UIKeyboardHIDUsageKeyboard0 = 39;
  UIKeyboardHIDUsageKeyboardReturnOrEnter = 40;
  UIKeyboardHIDUsageKeyboardEscape = 41;
  UIKeyboardHIDUsageKeyboardDeleteOrBackspace = 42;
  UIKeyboardHIDUsageKeyboardTab = 43;
  UIKeyboardHIDUsageKeyboardSpacebar = 44;
  UIKeyboardHIDUsageKeyboardHyphen = 45;
  UIKeyboardHIDUsageKeyboardEqualSign = 46;
  UIKeyboardHIDUsageKeyboardOpenBracket = 47;
  UIKeyboardHIDUsageKeyboardCloseBracket = 48;
  UIKeyboardHIDUsageKeyboardBackslash = 49;
  UIKeyboardHIDUsageKeyboardNonUSPound = 50;
  UIKeyboardHIDUsageKeyboardSemicolon = 51;
  UIKeyboardHIDUsageKeyboardQuote = 52;
  UIKeyboardHIDUsageKeyboardGraveAccentAndTilde = 53;
  UIKeyboardHIDUsageKeyboardComma = 54;
  UIKeyboardHIDUsageKeyboardPeriod = 55;
  UIKeyboardHIDUsageKeyboardSlash = 56;
  UIKeyboardHIDUsageKeyboardCapsLock = 57;
  UIKeyboardHIDUsageKeyboardF1 = 58;
  UIKeyboardHIDUsageKeyboardF2 = 59;
  UIKeyboardHIDUsageKeyboardF3 = 60;
  UIKeyboardHIDUsageKeyboardF4 = 61;
  UIKeyboardHIDUsageKeyboardF5 = 62;
  UIKeyboardHIDUsageKeyboardF6 = 63;
  UIKeyboardHIDUsageKeyboardF7 = 64;
  UIKeyboardHIDUsageKeyboardF8 = 65;
  UIKeyboardHIDUsageKeyboardF9 = 66;
  UIKeyboardHIDUsageKeyboardF10 = 67;
  UIKeyboardHIDUsageKeyboardF11 = 68;
  UIKeyboardHIDUsageKeyboardF12 = 69;
  UIKeyboardHIDUsageKeyboardPrintScreen = 70;
  UIKeyboardHIDUsageKeyboardScrollLock = 71;
  UIKeyboardHIDUsageKeyboardPause = 72;
  UIKeyboardHIDUsageKeyboardInsert = 73;
  UIKeyboardHIDUsageKeyboardHome = 74;
  UIKeyboardHIDUsageKeyboardPageUp = 75;
  UIKeyboardHIDUsageKeyboardDeleteForward = 76;
  UIKeyboardHIDUsageKeyboardEnd = 77;
  UIKeyboardHIDUsageKeyboardPageDown = 78;
  UIKeyboardHIDUsageKeyboardRightArrow = 79;
  UIKeyboardHIDUsageKeyboardLeftArrow = 80;
  UIKeyboardHIDUsageKeyboardDownArrow = 81;
  UIKeyboardHIDUsageKeyboardUpArrow = 82;
  UIKeyboardHIDUsageKeypadNumLock = 83;
  UIKeyboardHIDUsageKeypadSlash = 84;
  UIKeyboardHIDUsageKeypadAsterisk = 85;
  UIKeyboardHIDUsageKeypadHyphen = 86;
  UIKeyboardHIDUsageKeypadPlus = 87;
  UIKeyboardHIDUsageKeypadEnter = 88;
  UIKeyboardHIDUsageKeypad1 = 89;
  UIKeyboardHIDUsageKeypad2 = 90;
  UIKeyboardHIDUsageKeypad3 = 91;
  UIKeyboardHIDUsageKeypad4 = 92;
  UIKeyboardHIDUsageKeypad5 = 93;
  UIKeyboardHIDUsageKeypad6 = 94;
  UIKeyboardHIDUsageKeypad7 = 95;
  UIKeyboardHIDUsageKeypad8 = 96;
  UIKeyboardHIDUsageKeypad9 = 97;
  UIKeyboardHIDUsageKeypad0 = 98;
  UIKeyboardHIDUsageKeypadPeriod = 99;
  UIKeyboardHIDUsageKeyboardNonUSBackslash = 100;
  UIKeyboardHIDUsageKeyboardApplication = 101;
  UIKeyboardHIDUsageKeyboardPower = 102;
  UIKeyboardHIDUsageKeypadEqualSign = 103;
  UIKeyboardHIDUsageKeyboardF13 = 104;
  UIKeyboardHIDUsageKeyboardF14 = 105;
  UIKeyboardHIDUsageKeyboardF15 = 106;
  UIKeyboardHIDUsageKeyboardF16 = 107;
  UIKeyboardHIDUsageKeyboardF17 = 108;
  UIKeyboardHIDUsageKeyboardF18 = 109;
  UIKeyboardHIDUsageKeyboardF19 = 110;
  UIKeyboardHIDUsageKeyboardF20 = 111;
  UIKeyboardHIDUsageKeyboardF21 = 112;
  UIKeyboardHIDUsageKeyboardF22 = 113;
  UIKeyboardHIDUsageKeyboardF23 = 114;
  UIKeyboardHIDUsageKeyboardF24 = 115;
  UIKeyboardHIDUsageKeyboardExecute = 116;
  UIKeyboardHIDUsageKeyboardHelp = 117;
  UIKeyboardHIDUsageKeyboardMenu = 118;
  UIKeyboardHIDUsageKeyboardSelect = 119;
  UIKeyboardHIDUsageKeyboardStop = 120;
  UIKeyboardHIDUsageKeyboardAgain = 121;
  UIKeyboardHIDUsageKeyboardUndo = 122;
  UIKeyboardHIDUsageKeyboardCut = 123;
  UIKeyboardHIDUsageKeyboardCopy = 124;
  UIKeyboardHIDUsageKeyboardPaste = 125;
  UIKeyboardHIDUsageKeyboardFind = 126;
  UIKeyboardHIDUsageKeyboardMute = 127;
  UIKeyboardHIDUsageKeyboardVolumeUp = 128;
  UIKeyboardHIDUsageKeyboardVolumeDown = 129;
  UIKeyboardHIDUsageKeyboardLockingCapsLock = 130;
  UIKeyboardHIDUsageKeyboardLockingNumLock = 131;
  UIKeyboardHIDUsageKeyboardLockingScrollLock = 132;
  UIKeyboardHIDUsageKeypadComma = 133;
  UIKeyboardHIDUsageKeypadEqualSignAS400 = 134;
  UIKeyboardHIDUsageKeyboardInternational1 = 135;
  UIKeyboardHIDUsageKeyboardInternational2 = 136;
  UIKeyboardHIDUsageKeyboardInternational3 = 137;
  UIKeyboardHIDUsageKeyboardInternational4 = 138;
  UIKeyboardHIDUsageKeyboardInternational5 = 139;
  UIKeyboardHIDUsageKeyboardInternational6 = 140;
  UIKeyboardHIDUsageKeyboardInternational7 = 141;
  UIKeyboardHIDUsageKeyboardInternational8 = 142;
  UIKeyboardHIDUsageKeyboardInternational9 = 143;
  UIKeyboardHIDUsageKeyboardLANG1 = 144;
  UIKeyboardHIDUsageKeyboardLANG2 = 145;
  UIKeyboardHIDUsageKeyboardLANG3 = 146;
  UIKeyboardHIDUsageKeyboardLANG4 = 147;
  UIKeyboardHIDUsageKeyboardLANG5 = 148;
  UIKeyboardHIDUsageKeyboardLANG6 = 149;
  UIKeyboardHIDUsageKeyboardLANG7 = 150;
  UIKeyboardHIDUsageKeyboardLANG8 = 151;
  UIKeyboardHIDUsageKeyboardLANG9 = 152;
  UIKeyboardHIDUsageKeyboardAlternateErase = 153;
  UIKeyboardHIDUsageKeyboardSysReqOrAttention = 154;
  UIKeyboardHIDUsageKeyboardCancel = 155;
  UIKeyboardHIDUsageKeyboardClear = 156;
  UIKeyboardHIDUsageKeyboardPrior = 157;
  UIKeyboardHIDUsageKeyboardReturn = 158;
  UIKeyboardHIDUsageKeyboardSeparator = 159;
  UIKeyboardHIDUsageKeyboardOut = 160;
  UIKeyboardHIDUsageKeyboardOper = 161;
  UIKeyboardHIDUsageKeyboardClearOrAgain = 162;
  UIKeyboardHIDUsageKeyboardCrSelOrProps = 163;
  UIKeyboardHIDUsageKeyboardExSel = 164;
  UIKeyboardHIDUsageKeyboardLeftControl = 224;
  UIKeyboardHIDUsageKeyboardLeftShift = 225;
  UIKeyboardHIDUsageKeyboardLeftAlt = 226;
  UIKeyboardHIDUsageKeyboardLeftGUI = 227;
  UIKeyboardHIDUsageKeyboardRightControl = 228;
  UIKeyboardHIDUsageKeyboardRightShift = 229;
  UIKeyboardHIDUsageKeyboardRightAlt = 230;
  UIKeyboardHIDUsageKeyboardRightGUI = 231;
  UIKeyboardHIDUsageKeyboard_Reserved = 65535;
  UIKeyboardHIDUsageKeyboardHangul = UIKeyboardHIDUsageKeyboardLANG1;
  UIKeyboardHIDUsageKeyboardHanja = UIKeyboardHIDUsageKeyboardLANG2;
  UIKeyboardHIDUsageKeyboardKanaSwitch = UIKeyboardHIDUsageKeyboardLANG1;
  UIKeyboardHIDUsageKeyboardAlphanumericSwitch = UIKeyboardHIDUsageKeyboardLANG2;
  UIKeyboardHIDUsageKeyboardKatakana = UIKeyboardHIDUsageKeyboardLANG3;
  UIKeyboardHIDUsageKeyboardHiragana = UIKeyboardHIDUsageKeyboardLANG4;
  UIKeyboardHIDUsageKeyboardZenkakuHankakuKanji = UIKeyboardHIDUsageKeyboardLANG5;
  UIListContentTextAlignmentNatural = 0;
  UIListContentTextAlignmentCenter = 1;
  UIListContentTextAlignmentJustified = 2;
  UIListContentTextTransformNone = 0;
  UIListContentTextTransformUppercase = 1;
  UIListContentTextTransformLowercase = 2;
  UIListContentTextTransformCapitalized = 3;
  UIVibrancyEffectStyleLabel = 0;
  UIVibrancyEffectStyleSecondaryLabel = 1;
  UIVibrancyEffectStyleTertiaryLabel = 2;
  UIVibrancyEffectStyleQuaternaryLabel = 3;
  UIVibrancyEffectStyleFill = 4;
  UIVibrancyEffectStyleSecondaryFill = 5;
  UIVibrancyEffectStyleTertiaryFill = 6;
  UIVibrancyEffectStyleSeparator = 7;
  UIUserNotificationActionBehaviorDefault = 0;
  UIUserNotificationActionBehaviorTextInput = 1;
  UIUserNotificationActivationModeForeground = 0;
  UIUserNotificationActivationModeBackground = 1;
  UIUserNotificationActionContextDefault = 0;
  UIUserNotificationActionContextMinimal = 1;
  UINavigationItemLargeTitleDisplayModeAutomatic = 0;
  UINavigationItemLargeTitleDisplayModeAlways = 1;
  UINavigationItemLargeTitleDisplayModeNever = 2;
  UINavigationItemBackButtonDisplayModeDefault = 0;
  UINavigationItemBackButtonDisplayModeGeneric = 1;
  UINavigationItemBackButtonDisplayModeMinimal = 2;
  UIPageControlInteractionStateNone = 0;
  UIPageControlInteractionStateDiscrete = 1;
  UIPageControlInteractionStateContinuous = 2;
  UIPageControlBackgroundStyleAutomatic = 0;
  UIPageControlBackgroundStyleProminent = 1;
  UIPageControlBackgroundStyleMinimal = 2;
  UIPressPhaseBegan = 0;
  UIPressPhaseChanged = 1;
  UIPressPhaseStationary = 2;
  UIPressPhaseEnded = 3;
  UIPressPhaseCancelled = 4;
  UIPressTypeUpArrow = 0;
  UIPressTypeDownArrow = 1;
  UIPressTypeLeftArrow = 2;
  UIPressTypeRightArrow = 3;
  UIPressTypeSelect = 4;
  UIPressTypeMenu = 5;
  UIPressTypePlayPause = 6;
  UIPrinterJobTypeUnknown = 0;
  UIPrinterJobTypeDocument = 1;
  UIPrinterJobTypeEnvelope = 2;
  UIPrinterJobTypeLabel = 4;
  UIPrinterJobTypePhoto = 8;
  UIPrinterJobTypeReceipt = 16;
  UIPrinterJobTypeRoll = 32;
  UIPrinterJobTypeLargeFormat = 64;
  UIPrinterJobTypePostcard = 128;
  UIPrinterCutterBehaviorNoCut = 0;
  UIPrinterCutterBehaviorPrinterDefault = 1;
  UIPrinterCutterBehaviorCutAfterEachPage = 2;
  UIPrinterCutterBehaviorCutAfterEachCopy = 3;
  UIPrinterCutterBehaviorCutAfterEachJob = 4;
  UIScreenOverscanCompensationNone = 2;
  UISplitViewControllerDisplayModeAutomatic = 0;
  UISplitViewControllerDisplayModeSecondaryOnly = 1;
  UISplitViewControllerDisplayModeOneBesideSecondary = 2;
  UISplitViewControllerDisplayModeOneOverSecondary = 3;
  UISplitViewControllerDisplayModeTwoBesideSecondary = 4;
  UISplitViewControllerDisplayModeTwoOverSecondary = 5;
  UISplitViewControllerDisplayModeTwoDisplaceSecondary = 6;
  UISplitViewControllerDisplayModePrimaryHidden = UISplitViewControllerDisplayModeSecondaryOnly;
  UISplitViewControllerDisplayModeAllVisible = UISplitViewControllerDisplayModeOneBesideSecondary;
  UISplitViewControllerDisplayModePrimaryOverlay = UISplitViewControllerDisplayModeOneOverSecondary;
  UISplitViewControllerPrimaryEdgeLeading = 0;
  UISplitViewControllerPrimaryEdgeTrailing = 1;
  UISplitViewControllerBackgroundStyleNone = 0;
  UISplitViewControllerBackgroundStyleSidebar = 1;
  UISplitViewControllerStyleUnspecified = 0;
  UISplitViewControllerStyleDoubleColumn = 1;
  UISplitViewControllerStyleTripleColumn = 2;
  UISplitViewControllerColumnPrimary = 0;
  UISplitViewControllerColumnSupplementary = 1;
  UISplitViewControllerColumnSecondary = 2;
  UISplitViewControllerColumnCompact = 3;
  UISplitViewControllerSplitBehaviorAutomatic = 0;
  UISplitViewControllerSplitBehaviorTile = 1;
  UISplitViewControllerSplitBehaviorOverlay = 2;
  UISplitViewControllerSplitBehaviorDisplace = 3;
  UISwitchStyleAutomatic = 0;
  UISwitchStyleCheckbox = 1;
  UISwitchStyleSliding = 2;
  UITextItemInteractionInvokeDefaultAction = 0;
  UITextItemInteractionPresentActions = 1;
  UITextItemInteractionPreview = 2;
  UISpringLoadedInteractionEffectStateInactive = 0;
  UISpringLoadedInteractionEffectStatePossible = 1;
  UISpringLoadedInteractionEffectStateActivating = 2;
  UISpringLoadedInteractionEffectStateActivated = 3;
  UITabBarItemAppearanceStyleStacked = 0;
  UITabBarItemAppearanceStyleInline = 1;
  UITabBarItemAppearanceStyleCompactInline = 2;
  UIStackViewDistributionFill = 0;
  UIStackViewDistributionFillEqually = 1;
  UIStackViewDistributionFillProportionally = 2;
  UIStackViewDistributionEqualSpacing = 3;
  UIStackViewDistributionEqualCentering = 4;
  UIStackViewAlignmentFill = 0;
  UIStackViewAlignmentLeading = 1;
  UIStackViewAlignmentTop = UIStackViewAlignmentLeading;
  UIStackViewAlignmentFirstBaseline = 2;
  UIStackViewAlignmentCenter = 3;
  UIStackViewAlignmentTrailing = 4;
  UIStackViewAlignmentBottom = UIStackViewAlignmentTrailing;
  UIStackViewAlignmentLastBaseline = 5;
  UINotificationFeedbackTypeSuccess = 0;
  UINotificationFeedbackTypeWarning = 1;
  UINotificationFeedbackTypeError = 2;
  UITextInteractionModeEditable = 0;
  UITextInteractionModeNonEditable = 1;
  UIPencilPreferredActionIgnore = 0;
  UIPencilPreferredActionSwitchEraser = 1;
  UIPencilPreferredActionSwitchPrevious = 2;
  UIPencilPreferredActionShowColorPalette = 3;
  UISceneCollectionJoinBehaviorAutomatic = 0;
  UISceneCollectionJoinBehaviorPreferred = 1;
  UISceneCollectionJoinBehaviorDisallowed = 2;
  UISceneCollectionJoinBehaviorPreferredWithoutActivating = 3;
  UIWindowSceneDismissalAnimationStandard = 1;
  UIWindowSceneDismissalAnimationCommit = 2;
  UIWindowSceneDismissalAnimationDecline = 3;
  UIPointerEffectTintModeNone = 0;
  UIPointerEffectTintModeOverlay = 1;
  UIPointerEffectTintModeUnderlay = 2;

type
  UIAlertAction = interface;
  UIAlertController = interface;
  UIApplication = interface;
  NSCollectionLayoutAnchor = interface;
  NSCollectionLayoutBoundarySupplementaryItem = interface;
  NSCollectionLayoutContainer = interface;
  NSCollectionLayoutDecorationItem = interface;
  NSCollectionLayoutDimension = interface;
  NSCollectionLayoutEdgeSpacing = interface;
  NSCollectionLayoutEnvironment = interface;
  NSCollectionLayoutGroup = interface;
  NSCollectionLayoutGroupCustomItem = interface;
  NSCollectionLayoutItem = interface;
  NSCollectionLayoutSection = interface;
  NSCollectionLayoutSize = interface;
  NSCollectionLayoutSpacing = interface;
  NSCollectionLayoutSupplementaryItem = interface;
  NSCollectionLayoutVisibleItem = interface;
  NSDataAsset = interface;
  NSLayoutAnchor = interface;
  NSLayoutDimension = interface;
  NSLayoutXAxisAnchor = interface;
  NSLayoutYAxisAnchor = interface;
  UIAccessibilityContainerDataTable = interface;
  UIAccessibilityContainerDataTableCell = interface;
  UIAccessibilityContentSizeCategoryImageAdjusting = interface;
  UIAccessibilityCustomAction = interface;
  UIAccessibilityCustomRotor = interface;
  UIAccessibilityCustomRotorItemResult = interface;
  UIAccessibilityCustomRotorSearchPredicate = interface;
  UIAccessibilityLocationDescriptor = interface;
  UIAction = interface;
  UIActivityItemsConfiguration = interface;
  UIActivityItemsConfigurationReading = interface;
  UIApplicationShortcutIcon = interface;
  UIApplicationShortcutItem = interface;
  UIBackgroundConfiguration = interface;
  UIBarAppearance = interface;
  UIBarButtonItemAppearance = interface;
  UIBarButtonItemGroup = interface;
  UIBarButtonItemStateAppearance = interface;
  UIBlurEffect = interface;
  UICellAccessory = interface;
  UICellAccessoryCheckmark = interface;
  UICellAccessoryCustomView = interface;
  UICellAccessoryDelete = interface;
  UICellAccessoryDisclosureIndicator = interface;
  UICellAccessoryInsert = interface;
  UICellAccessoryLabel = interface;
  UICellAccessoryMultiselect = interface;
  UICellAccessoryOutlineDisclosure = interface;
  UICellAccessoryReorder = interface;
  UICellConfigurationState = interface;
  UICloudSharingController = interface;
  UICloudSharingControllerDelegate = interface;
  UICollectionLayoutListConfiguration = interface;
  UICollectionViewCellRegistration = interface;
  UICollectionViewCompositionalLayout = interface;
  UICollectionViewCompositionalLayoutConfiguration = interface;
  UICollectionViewDataSourcePrefetching = interface;
  UICollectionViewDragDelegate = interface;
  UICollectionViewDropCoordinator = interface;
  UICollectionViewDropDelegate = interface;
  UICollectionViewDropItem = interface;
  UICollectionViewDropPlaceholder = interface;
  UICollectionViewDropPlaceholderContext = interface;
  UICollectionViewDropProposal = interface;
  UICollectionViewFocusUpdateContext = interface;
  UICollectionViewListCell = interface;
  UICollectionViewPlaceholder = interface;
  UICollectionViewSupplementaryRegistration = interface;
  UIColorPickerViewController = interface;
  UIColorPickerViewControllerDelegate = interface;
  UIColorWell = interface;
  UICommand = interface;
  UICommandAlternate = interface;
  UIConfigurationState = interface;
  UIContentConfiguration = interface;
  UIContentSizeCategoryAdjusting = interface;
  UIContentView = interface;
  UIContextMenuInteraction = interface;
  UIContextMenuInteractionAnimating = interface;
  UIContextMenuInteractionCommitAnimating = interface;
  UIContextMenuInteractionDelegate = interface;
  UIContextualAction = interface;
  UICoordinateSpace = interface;
  UICubicTimingParameters = interface;
  UIDataSourceTranslating = interface;
  UIDeferredMenuElement = interface;
  UIDocumentBrowserAction = interface;
  UIDocumentBrowserTransitionController = interface;
  UIDocumentBrowserViewController = interface;
  UIDocumentBrowserViewControllerDelegate = interface;
  UIDocumentMenuDelegate = interface;
  UIDocumentMenuViewController = interface;
  UIDocumentPickerDelegate = interface;
  UIDocumentPickerExtensionViewController = interface;
  UIDocumentPickerViewController = interface;
  UIDragAnimating = interface;
  UIDragDropSession = interface;
  UIDragInteraction = interface;
  UIDragInteractionDelegate = interface;
  UIDragItem = interface;
  UIDragPreview = interface;
  UIDragPreviewParameters = interface;
  UIDragPreviewTarget = interface;
  UIDragSession = interface;
  UIDropInteraction = interface;
  UIDropInteractionDelegate = interface;
  UIDropProposal = interface;
  UIDropSession = interface;
  UIDynamicItemGroup = interface;
  UIFeedbackGenerator = interface;
  UIFieldBehavior = interface;
  UIFocusAnimationContext = interface;
  UIFocusAnimationCoordinator = interface;
  UIFocusDebugger = interface;
  UIFocusDebuggerOutput = interface;
  UIFocusEnvironment = interface;
  UIFocusGuide = interface;
  UIFocusItem = interface;
  UIFocusItemContainer = interface;
  UIFocusItemScrollableContainer = interface;
  UIFocusMovementHint = interface;
  UIFocusSystem = interface;
  UIFocusUpdateContext = interface;
  UIFontMetrics = interface;
  UIFontPickerViewController = interface;
  UIFontPickerViewControllerConfiguration = interface;
  UIFontPickerViewControllerDelegate = interface;
  UIGraphicsImageRenderer = interface;
  UIGraphicsImageRendererContext = interface;
  UIGraphicsImageRendererFormat = interface;
  UIGraphicsPDFRenderer = interface;
  UIGraphicsPDFRendererContext = interface;
  UIGraphicsPDFRendererFormat = interface;
  UIGraphicsRenderer = interface;
  UIGraphicsRendererContext = interface;
  UIGraphicsRendererFormat = interface;
  UIHoverGestureRecognizer = interface;
  UIImageAsset = interface;
  UIImageConfiguration = interface;
  UIImageSymbolConfiguration = interface;
  UIImpactFeedbackGenerator = interface;
  UIIndirectScribbleInteraction = interface;
  UIIndirectScribbleInteractionDelegate = interface;
  UIInputViewController = interface;
  UIInteraction = interface;
  UIItemProviderPresentationSizeProviding = interface;
  UIKey = interface;
  UILargeContentViewerInteraction = interface;
  UILargeContentViewerInteractionDelegate = interface;
  UILargeContentViewerItem = interface;
  UILayoutGuide = interface;
  UILexicon = interface;
  UILexiconEntry = interface;
  UIListContentConfiguration = interface;
  UIListContentImageProperties = interface;
  UIListContentTextProperties = interface;
  UIListContentView = interface;
  UIMenuBuilder = interface;
  UIMenuSystem = interface;
  UIMutableApplicationShortcutItem = interface;
  UIMutableUserNotificationAction = interface;
  UIMutableUserNotificationCategory = interface;
  UINavigationBarAppearance = interface;
  UINotificationFeedbackGenerator = interface;
  UIOpenURLContext = interface;
  UIPasteConfiguration = interface;
  UIPasteConfigurationSupporting = interface;
  UIPencilInteraction = interface;
  UIPencilInteractionDelegate = interface;
  UIPointerEffect = interface;
  UIPointerHighlightEffect = interface;
  UIPointerHoverEffect = interface;
  UIPointerInteraction = interface;
  UIPointerInteractionAnimating = interface;
  UIPointerInteractionDelegate = interface;
  UIPointerLiftEffect = interface;
  UIPointerLockState = interface;
  UIPointerRegion = interface;
  UIPointerRegionRequest = interface;
  UIPointerShape = interface;
  UIPointerStyle = interface;
  UIPress = interface;
  UIPressesEvent = interface;
  UIPreviewAction = interface;
  UIPreviewActionGroup = interface;
  UIPreviewActionItem = interface;
  UIPreviewInteraction = interface;
  UIPreviewInteractionDelegate = interface;
  UIPreviewParameters = interface;
  UIPreviewTarget = interface;
  UIRegion = interface;
  UIResponderStandardEditActions = interface;
  UIScene = interface;
  UISceneActivationConditions = interface;
  UISceneActivationRequestOptions = interface;
  UISceneConfiguration = interface;
  UISceneConnectionOptions = interface;
  UISceneDelegate = interface;
  UISceneDestructionRequestOptions = interface;
  UISceneOpenExternalURLOptions = interface;
  UISceneOpenURLOptions = interface;
  UISceneSession = interface;
  UISceneSizeRestrictions = interface;
  UIScreenshotService = interface;
  UIScreenshotServiceDelegate = interface;
  UIScribbleInteraction = interface;
  UIScribbleInteractionDelegate = interface;
  UISearchContainerViewController = interface;
  UISearchSuggestion = interface;
  UISearchSuggestionItem = interface;
  UISearchTextField = interface;
  UISearchTextFieldDelegate = interface;
  UISearchTextFieldPasteItem = interface;
  UISearchToken = interface;
  UISelectionFeedbackGenerator = interface;
  UISpringLoadedInteraction = interface;
  UISpringLoadedInteractionBehavior = interface;
  UISpringLoadedInteractionContext = interface;
  UISpringLoadedInteractionEffect = interface;
  UISpringLoadedInteractionSupporting = interface;
  UISpringTimingParameters = interface;
  UIStackView = interface;
  UIStatusBarManager = interface;
  UIStoryboardUnwindSegueSource = interface;
  UISwipeActionsConfiguration = interface;
  UITabBarAppearance = interface;
  UITabBarItemAppearance = interface;
  UITabBarItemStateAppearance = interface;
  UITableViewDataSourcePrefetching = interface;
  UITableViewDragDelegate = interface;
  UITableViewDropCoordinator = interface;
  UITableViewDropDelegate = interface;
  UITableViewDropItem = interface;
  UITableViewDropPlaceholder = interface;
  UITableViewDropPlaceholderContext = interface;
  UITableViewDropProposal = interface;
  UITableViewFocusUpdateContext = interface;
  UITableViewPlaceholder = interface;
  UITableViewRowAction = interface;
  UITargetedDragPreview = interface;
  UITargetedPreview = interface;
  UITextDragDelegate = interface;
  UITextDraggable = interface;
  UITextDragPreviewRenderer = interface;
  UITextDragRequest = interface;
  UITextDropDelegate = interface;
  UITextDroppable = interface;
  UITextDropProposal = interface;
  UITextDropRequest = interface;
  UITextFormattingCoordinator = interface;
  UITextFormattingCoordinatorDelegate = interface;
  UITextInputAssistantItem = interface;
  UITextInputPasswordRules = interface;
  UITextInteraction = interface;
  UITextInteractionDelegate = interface;
  UITextPasteConfigurationSupporting = interface;
  UITextPasteDelegate = interface;
  UITextPasteItem = interface;
  UITextPlaceholder = interface;
  UITimingCurveProvider = interface;
  UIToolbarAppearance = interface;
  UIUserActivityRestoring = interface;
  UIUserNotificationAction = interface;
  UIUserNotificationCategory = interface;
  UIVibrancyEffect = interface;
  UIViewAnimating = interface;
  UIViewConfigurationState = interface;
  UIViewControllerPreviewing = interface;
  UIViewControllerPreviewingDelegate = interface;
  UIViewImplicitlyAnimating = interface;
  UIViewPropertyAnimator = interface;
  UIVisualEffect = interface;
  UIVisualEffectView = interface;
  UIWindowScene = interface;
  UIWindowSceneDelegate = interface;
  UIWindowSceneDestructionRequestOptions = interface;

  UIAlertActionStyle = NSInteger;
  UIAlertControllerStyle = NSInteger;
  UIMenuElementState = NSInteger;
  UIMenuElementAttributes = NSInteger;
  UIMenuIdentifier = NSString;
  UIMenuOptions = NSInteger;
  UIKeyModifierFlags = NSInteger;
  UIEventType = NSInteger;
  UIEventSubtype = NSInteger;
  UIEventButtonMask = NSInteger;
  UIEditingInteractionConfiguration = NSInteger;
  UIFontDescriptorSymbolicTraits = NSInteger;
  UIFontDescriptorClass = NSUInteger;
  UIFontTextStyle = NSString;
  UIFontDescriptorAttributeName = NSString;
  UIFontDescriptorTraitKey = NSString;
  UIFontDescriptorFeatureKey = NSString;
  UIFontWeight = CGFloat;
  UIFontDescriptorSystemDesign = NSString;
  UIBarStyle = NSInteger;
  UIUserInterfaceSizeClass = NSInteger;
  UIUserInterfaceStyle = NSInteger;
  UIUserInterfaceLayoutDirection = NSInteger;
  UITraitEnvironmentLayoutDirection = NSInteger;
  UIDisplayGamut = NSInteger;
  UIAccessibilityContrast = NSInteger;
  UILegibilityWeight = NSInteger;
  UIUserInterfaceLevel = NSInteger;
  UIUserInterfaceActiveAppearance = NSInteger;
  UIDeviceOrientation = NSInteger;
  UIDeviceBatteryState = NSInteger;
  UIUserInterfaceIdiom = NSInteger;
  UIRectEdge = NSInteger;
  UIRectCorner = NSInteger;
  UIAxis = NSInteger;
  NSDirectionalRectEdge = NSInteger;
  UIDirectionalRectEdge = NSInteger;
  NSRectAlignment = NSInteger;
  UIDynamicItemCollisionBoundsType = NSInteger;
  UILayoutPriority = Single;
  NSLayoutRelation = NSInteger;
  NSLayoutAttribute = NSInteger;
  NSLayoutFormatOptions = NSInteger;
  UITouchPhase = NSInteger;
  UIForceTouchCapability = NSInteger;
  UITouchType = NSInteger;
  UITouchProperties = NSInteger;
  UIContentSizeCategory = NSString;
  UIFocusHeading = NSInteger;
  UIFocusSoundIdentifier = NSString;
  UIViewAnimationCurve = NSInteger;
  UIViewContentMode = NSInteger;
  UIViewAnimationTransition = NSInteger;
  UIViewAutoresizing = NSInteger;
  UIViewAnimationOptions = NSInteger;
  UIViewKeyframeAnimationOptions = NSInteger;
  UISystemAnimation = NSInteger;
  UIViewTintAdjustmentMode = NSInteger;
  UISemanticContentAttribute = NSInteger;
  UILayoutConstraintAxis = NSInteger;
  UIActionIdentifier = NSString;
  UIContextMenuInteractionCommitStyle = NSInteger;
  UIContextMenuInteractionAppearance = NSInteger;
  UIControlEvents = NSInteger;
  UIControlContentVerticalAlignment = NSInteger;
  UIControlContentHorizontalAlignment = NSInteger;
  UIControlState = NSInteger;
  NSTextAlignment = NSInteger;
  NSWritingDirection = NSInteger;
  NSLineBreakMode = NSInteger;
  NSLineBreakStrategy = NSInteger;
  NSTextTabOptionKey = NSString;
  UILineBreakMode = NSInteger;
  UITextAlignment = NSInteger;
  UIBaselineAdjustment = NSInteger;
  UIDropOperation = NSInteger;
  UIViewAnimatingState = NSInteger;
  UIViewAnimatingPosition = NSInteger;
  UITextAutocapitalizationType = NSInteger;
  UITextAutocorrectionType = NSInteger;
  UITextSpellCheckingType = NSInteger;
  UITextSmartQuotesType = NSInteger;
  UITextSmartDashesType = NSInteger;
  UITextSmartInsertDeleteType = NSInteger;
  UIKeyboardType = NSInteger;
  UIKeyboardAppearance = NSInteger;
  UIReturnKeyType = NSInteger;
  UITextContentType = NSString;
  UITextStorageDirection = NSInteger;
  UITextLayoutDirection = NSInteger;
  UITextDirection = NSInteger;
  UITextGranularity = NSInteger;
  UITextAlternativeStyle = NSInteger;
  UITextWritingDirection = NSWritingDirection;
  UITextDragOptions = NSInteger;
  UITextDropAction = NSInteger;
  UITextDropProgressMode = NSInteger;
  UITextDropPerformer = NSInteger;
  UITextDropEditability = NSInteger;
  UITextBorderStyle = NSInteger;
  UITextFieldViewMode = NSInteger;
  UITextFieldDidEndEditingReason = NSInteger;
  UIActionSheetStyle = NSInteger;
  UIAlertViewStyle = NSInteger;
  UISceneActivationState = NSInteger;
  UISceneSessionRole = NSString;
  UISceneErrorCode = NSInteger;
  UIStatusBarStyle = NSInteger;
  UIStatusBarAnimation = NSInteger;
  UIInterfaceOrientation = NSInteger;
  UIInterfaceOrientationMask = NSInteger;
  UIRemoteNotificationType = NSInteger;
  UIBackgroundFetchResult = NSInteger;
  UIBackgroundRefreshStatus = NSInteger;
  UIApplicationState = NSInteger;
  UIBackgroundTaskIdentifier = NSUInteger;
  UIApplicationOpenExternalURLOptionsKey = NSString;
  UIApplicationLaunchOptionsKey = NSString;
  UIApplicationOpenURLOptionsKey = NSString;
  UIApplicationExtensionPointIdentifier = NSString;
  UIModalTransitionStyle = NSInteger;
  UIModalPresentationStyle = NSInteger;
  UIPreviewActionStyle = NSInteger;
  UITransitionContextViewControllerKey = NSString;
  UITransitionContextViewKey = NSString;
  UITimingCurveType = NSInteger;
  UIDocumentBrowserErrorCode = NSInteger;
  UIDocumentBrowserImportMode = NSInteger;
  UIDocumentBrowserUserInterfaceStyle = NSInteger;
  UIDocumentBrowserActionAvailability = NSInteger;
  NSUnderlineStyle = NSInteger;
  NSWritingDirectionFormatType = NSInteger;
  NSTextEffectStyle = NSString;
  NSAttributedStringDocumentType = NSString;
  NSTextLayoutSectionKey = NSString;
  NSTextScalingType = NSInteger;
  NSAttributedStringDocumentAttributeKey = NSString;
  NSAttributedStringDocumentReadingOptionKey = NSString;
  NSTextWritingDirection = NSInteger;
  NSDataAssetName = NSString;
  UIPreferredPresentationStyle = NSInteger;
  NSTextStorageEditActions = NSInteger;
  NSTextLayoutOrientation = NSInteger;
  NSGlyphProperty = NSInteger;
  NSControlCharacterAction = NSInteger;
  NSStringDrawingOptions = NSInteger;
  UIActivityType = NSString;
  UIActivityCategory = NSInteger;
  UIAccelerationValue = Double;
  UIScrollViewIndicatorStyle = NSInteger;
  UIScrollViewKeyboardDismissMode = NSInteger;
  UIScrollViewIndexDisplayMode = NSInteger;
  UIScrollViewContentInsetAdjustmentBehavior = NSInteger;
  UIScrollViewDecelerationRate = CGFloat;
  UIAccessibilityTraits = UInt64;
  UIAccessibilityNotifications = UInt32;
  UIAccessibilityAssistiveTechnologyIdentifier = NSString;
  UIAccessibilityNavigationStyle = NSInteger;
  UIAccessibilityContainerType = NSInteger;
  UIAccessibilityTextualContext = NSString;
  UIImageOrientation = NSInteger;
  UIImageResizingMode = NSInteger;
  UIImageRenderingMode = NSInteger;
  UIAccessibilityCustomRotorDirection = NSInteger;
  UIAccessibilityCustomSystemRotorType = NSInteger;
  UIAccessibilityZoomType = NSInteger;
  UIGuidedAccessErrorCode = NSInteger;
  UIGuidedAccessRestrictionState = NSInteger;
  UIGuidedAccessAccessibilityFeature = NSInteger;
  UIAccessibilityScrollDirection = NSInteger;
  UIAccessibilityHearingDeviceEar = NSInteger;
  UIButtonType = NSInteger;
  UIButtonRole = NSInteger;
  UIActivityIndicatorViewStyle = NSInteger;
  UIActivityItemsConfigurationMetadataKey = NSString;
  UIActivityItemsConfigurationPreviewIntent = NSString;
  UIActivityItemsConfigurationInteraction = NSString;
  UIApplicationShortcutIconType = NSInteger;
  UIAttachmentBehaviorType = NSInteger;
  UIBarMetrics = NSInteger;
  UIBarPosition = NSInteger;
  UIBarButtonItemStyle = NSInteger;
  UIBarButtonSystemItem = NSInteger;
  UIBlurEffectStyle = NSInteger;
  UICellAccessoryDisplayedState = NSInteger;
  UICellAccessoryOutlineDisclosureStyle = NSInteger;
  UICellAccessoryPlacement = NSInteger;
  UIConfigurationStateCustomKey = NSString;
  UICellConfigurationDragState = NSInteger;
  UICellConfigurationDropState = NSInteger;
  UICloudSharingPermissionOptions = NSInteger;
  UICollectionViewScrollDirection = NSInteger;
  UICollectionElementCategory = NSInteger;
  UICollectionUpdateAction = NSInteger;
  UIContentInsetsReference = NSInteger;
  UICollectionLayoutSectionOrthogonalScrollingBehavior = NSInteger;
  UICollectionLayoutListAppearance = NSInteger;
  UICollectionLayoutListHeaderMode = NSInteger;
  UICollectionLayoutListFooterMode = NSInteger;
  UICollectionViewScrollPosition = NSInteger;
  UICollectionViewReorderingCadence = NSInteger;
  UICollectionViewDropIntent = NSInteger;
  UICollectionViewCellDragState = NSInteger;
  UICollectionViewFlowLayoutSectionInsetReference = NSInteger;
  UICollisionBehaviorMode = NSInteger;
  UIContextualActionStyle = NSInteger;
  UIDataDetectorTypes = NSInteger;
  UIDatePickerMode = NSInteger;
  UIDatePickerStyle = NSInteger;
  UIGestureRecognizerState = NSInteger;
  UISwipeGestureRecognizerDirection = NSInteger;
  UITableViewCellStyle = NSInteger;
  UITableViewCellSeparatorStyle = NSInteger;
  UITableViewCellSelectionStyle = NSInteger;
  UITableViewCellFocusStyle = NSInteger;
  UITableViewCellEditingStyle = NSInteger;
  UITableViewCellAccessoryType = NSInteger;
  UITableViewCellStateMask = NSInteger;
  UITableViewCellDragState = NSInteger;
  UITableViewStyle = NSInteger;
  UITableViewScrollPosition = NSInteger;
  UITableViewRowAnimation = NSInteger;
  UITableViewRowActionStyle = NSInteger;
  UITableViewSeparatorInsetReference = NSInteger;
  UITableViewDropIntent = NSInteger;
  UIDocumentChangeKind = NSInteger;
  UIDocumentSaveOperation = NSInteger;
  UIDocumentState = NSInteger;
  UIDocumentPickerMode = NSInteger;
  UIDocumentMenuOrder = NSInteger;
  UIDropSessionProgressIndicatorStyle = NSInteger;
  UIGraphicsImageRendererFormatRange = NSInteger;
  UIScrollType = NSInteger;
  UIScrollTypeMask = NSInteger;
  UINavigationControllerOperation = NSInteger;
  UIImagePickerControllerSourceType = NSInteger;
  UIImagePickerControllerQualityType = NSInteger;
  UIImagePickerControllerCameraCaptureMode = NSInteger;
  UIImagePickerControllerCameraDevice = NSInteger;
  UIImagePickerControllerCameraFlashMode = NSInteger;
  UIImagePickerControllerImageURLExportPreset = NSInteger;
  UIImagePickerControllerInfoKey = NSString;
  UIImageSymbolScale = NSInteger;
  UIImageSymbolWeight = NSInteger;
  UIImpactFeedbackStyle = NSInteger;
  UIScribbleElementIdentifier = Pointer;
  UIInputViewStyle = NSInteger;
  UIKeyboardHIDUsage = NSInteger;
  UIListContentTextAlignment = NSInteger;
  UIListContentTextTransform = NSInteger;
  UIVibrancyEffectStyle = NSInteger;
  UIUserNotificationType = NSInteger;
  UIUserNotificationActionBehavior = NSInteger;
  UIUserNotificationActivationMode = NSInteger;
  UIUserNotificationActionContext = NSInteger;
  UIMenuControllerArrowDirection = NSInteger;
  UIInterpolatingMotionEffectType = NSInteger;
  UINavigationItemLargeTitleDisplayMode = NSInteger;
  UINavigationItemBackButtonDisplayMode = NSInteger;
  UINibOptionsKey = NSString;
  UIPageControlInteractionState = NSInteger;
  UIPageControlBackgroundStyle = NSInteger;
  UIPageViewControllerNavigationOrientation = NSInteger;
  UIPageViewControllerSpineLocation = NSInteger;
  UIPageViewControllerNavigationDirection = NSInteger;
  UIPageViewControllerTransitionStyle = NSInteger;
  UIPageViewControllerOptionsKey = NSString;
  UIPasteboardName = NSString;
  UIPasteboardDetectionPattern = NSString;
  UIPasteboardOption = NSString;
  UIPopoverArrowDirection = NSInteger;
  UIPressPhase = NSInteger;
  UIPressType = NSInteger;
  UIPrinterJobTypes = NSInteger;
  UIPrintErrorCode = NSInteger;
  UIPrintInfoOutputType = NSInteger;
  UIPrintInfoOrientation = NSInteger;
  UIPrintInfoDuplex = NSInteger;
  UIPrinterCutterBehavior = NSInteger;
  UIProgressViewStyle = NSInteger;
  UIScreenOverscanCompensation = NSInteger;
  UISearchBarIcon = NSInteger;
  UISearchBarStyle = NSInteger;
  UISegmentedControlStyle = NSInteger;
  UISegmentedControlSegment = NSInteger;
  UISplitViewControllerDisplayMode = NSInteger;
  UISplitViewControllerPrimaryEdge = NSInteger;
  UISplitViewControllerBackgroundStyle = NSInteger;
  UISplitViewControllerStyle = NSInteger;
  UISplitViewControllerColumn = NSInteger;
  UISplitViewControllerSplitBehavior = NSInteger;
  UISwitchStyle = NSInteger;
  UITabBarItemPositioning = NSInteger;
  UITabBarSystemItem = NSInteger;
  UITextItemInteraction = NSInteger;
  UIWebViewNavigationType = NSInteger;
  UIWebPaginationMode = NSInteger;
  UIWebPaginationBreakingMode = NSInteger;
  UIWindowLevel = CGFloat;
  UISpringLoadedInteractionEffectState = NSInteger;
  UITabBarItemAppearanceStyle = NSInteger;
  UIStackViewDistribution = NSInteger;
  UIStackViewAlignment = NSInteger;
  UIPushBehaviorMode = NSInteger;
  UINotificationFeedbackType = NSInteger;
  UITextInteractionMode = NSInteger;
  UIPencilPreferredAction = NSInteger;
  UISceneCollectionJoinBehavior = NSInteger;
  UIWindowSceneDismissalAnimation = NSInteger;
  UIPointerEffectTintMode = NSInteger;

  NSDirectionalEdgeInsets = record
    top: CGFloat;
    leading: CGFloat;
    bottom: CGFloat;
    trailing: CGFloat;
  end;

  NSCollectionLayoutGroupCustomItemProvider = function(layoutEnvironment: Pointer): NSArray of object;
  NSCollectionLayoutSectionVisibleItemsInvalidationHandler = procedure(visibleItems: NSArray; contentOffset: CGPoint; layoutEnvironment: Pointer) of object;
  UIAccessibilityCustomActionHandler = function(customAction: UIAccessibilityCustomAction): Boolean of object;
  UIAccessibilityCustomRotorSearch = function(predicate: UIAccessibilityCustomRotorSearchPredicate): UIAccessibilityCustomRotorItemResult of object;
  UIActionHandler = procedure(action: UIAction) of object;
  UIActivityViewControllerCompletionHandler = procedure(activityType: UIActivityType; completed: Boolean) of object;
  UIActivityViewControllerCompletionWithItemsHandler = procedure(activityType: UIActivityType; completed: Boolean; returnedItems: NSArray; activityError: NSError) of object;
  UIButtonPointerStyleProvider = function(button: UIButton; proposedEffect: UIPointerEffect; proposedShape: UIPointerShape): UIPointerStyle of object;
  UICellAccessoryPosition = function(accessories: NSArray): NSUInteger of object;
  UICollectionLayoutListSwipeActionsConfigurationProvider = function(indexPath: NSIndexPath): UISwipeActionsConfiguration of object;
  UICollectionViewCellRegistrationConfigurationHandler = procedure(cell: UICollectionViewCell; indexPath: NSIndexPath; item: Pointer) of object;
  UICollectionViewCompositionalLayoutSectionProvider = function(section: NSInteger; p2: Pointer): NSCollectionLayoutSection of object;
  UICollectionViewLayoutInteractiveTransitionCompletion = procedure(completed: Boolean; finished: Boolean) of object;
  UICollectionViewSupplementaryRegistrationConfigurationHandler = procedure(supplementaryView: UICollectionReusableView; elementKind: NSString; indexPath: NSIndexPath) of object;
  UIConfigurationColorTransformer = function(color: UIColor): UIColor of object;
  UIContextMenuActionProvider = function(suggestedActions: NSArray): UIMenu of object;
  UIContextMenuContentPreviewProvider = function: UIViewController of object;
  UIContextualActionHandlerCompletionHandler = procedure(actionPerformed: Boolean) of object;
  UIContextualActionHandler = procedure(action: UIContextualAction; sourceView: UIView; completionHandler: UIContextualActionHandlerCompletionHandler) of object;
  UIGraphicsDrawingActions = procedure(rendererContext: UIGraphicsRendererContext) of object;
  UIGraphicsImageDrawingActions = procedure(rendererContext: UIGraphicsImageRendererContext) of object;
  UIGraphicsPDFDrawingActions = procedure(rendererContext: UIGraphicsPDFRendererContext) of object;
  UIPrinterPickerCompletionHandler = procedure(printerPickerController: UIPrinterPickerController; userDidSelect: Boolean; error: NSError) of object;
  UIPrintInteractionCompletionHandler = procedure(printInteractionController: UIPrintInteractionController; completed: Boolean; error: NSError) of object;
  UIStoryboardViewControllerCreator = function(coder: NSCoder): UIViewController of object;
  UITextAttributesConversionHandler = function(param1: NSDictionary): NSDictionary of object; // CAUTION: Declaration needs verification

  TUIAlertActionHandler = procedure(alertaction: Pointer) of object;
  TUIAlertControllerConfigurationHandler = procedure(textfield: UITextField) of object;
  TNSLayoutManagerBlockMethod1 = procedure(rect: CGRect; usedRect: CGRect; textContainer: NSTextContainer; glyphRange: NSRange; stop: PBoolean) of object;
  TNSLayoutManagerBlockMethod2 = procedure(rect: CGRect; stop: PBoolean) of object;
  TUIActivityItemsConfigurationBlockMethod1 = function(param1: UIActivityItemsConfigurationMetadataKey): Pointer of object;
  TUIActivityItemsConfigurationBlockMethod2 = procedure of object;
  TUIActivityItemsConfigurationBlockMethod3 = function(param1: NSInteger; param2: UIActivityItemsConfigurationMetadataKey): Pointer of object;
  // Foundation TUIActivityItemsConfigurationBlockMethod4 = function(param1: NSInteger; param2: UIActivityItemsConfigurationPreviewIntent; param3: CGSize): NSItemProvider of object;
  TUIActivityItemsConfigurationBlockMethod5 = function: NSArray of object;
  TUIActivityItemsConfigurationBlockMethod6 = procedure of object;
  TUIAlertActionBlockMethod1 = procedure(action: UIAlertAction) of object;
  TUIAlertControllerBlockMethod1 = procedure(textField: UITextField) of object;
  TUIApplicationBlockMethod1 = procedure(success: Boolean) of object;
  TUIApplicationBlockMethod2 = procedure of object;
  TUIApplicationBlockMethod3 = procedure(error: NSError) of object;
  TUIApplicationDelegateBlockMethod1 = procedure of object;
  TUIApplicationDelegateBlockMethod2 = procedure(result: UIBackgroundFetchResult) of object;
  TUIApplicationDelegateBlockMethod3 = procedure(succeeded: Boolean) of object;
  TUIApplicationDelegateBlockMethod4 = procedure(replyInfo: NSDictionary) of object;
  // Intents TUIApplicationDelegateBlockMethod5 = procedure(intentResponse: INIntentResponse) of object;
  TUIApplicationDelegateBlockMethod6 = procedure(restorableObjects: NSArray) of object;
  TUICellAccessoryDeleteBlockMethod1 = procedure of object;
  TUICellAccessoryInsertBlockMethod1 = procedure of object;
  TUICellAccessoryOutlineDisclosureBlockMethod1 = procedure of object;
  TUICloudSharingControllerBlockMethod1 = procedure of object;
  TUICloudSharingControllerBlockMethod2 = procedure(controller: UICloudSharingController;
    preparationCompletionHandler: TUICloudSharingControllerBlockMethod1) of object;
  TUICollectionViewBlockMethod1 = procedure(finished: Boolean) of object;
  TUICollectionViewBlockMethod2 = procedure of object;
  TUICollectionViewDropPlaceholderBlockMethod1 = function(param1: UICollectionViewCell): UIDragPreviewParameters of object;
  TUICollectionViewDropPlaceholderBlockMethod2 = procedure of object;
  TUICollectionViewDropPlaceholderContextBlockMethod1 = procedure(insertionIndexPath: NSIndexPath) of object;
  TUICollectionViewPlaceholderBlockMethod1 = procedure(param1: UICollectionViewCell) of object;
  TUICollectionViewPlaceholderBlockMethod2 = procedure of object;
  TUIColorBlockMethod1 = procedure(traitCollection: UITraitCollection) of object;
  TUIContextMenuInteractionAnimatingBlockMethod1 = procedure of object;
  TUIContextMenuInteractionBlockMethod1 = procedure(visibleMenu: UIMenu) of object;
  TUIControlBlockMethod1 = procedure(actionHandler: UIAction; target: Pointer; action: Pointer; controlEvents: UIControlEvents;
    stop: PBoolean) of object;
  TUIDataSourceTranslatingBlockMethod1 = procedure of object;
  TUIDeferredMenuElementBlockMethod1 = procedure of object;
  TUIDeferredMenuElementBlockMethod2 = procedure(completion: TUIDeferredMenuElementBlockMethod1) of object;
  TUIDocumentBlockMethod1 = procedure(success: Boolean) of object;
  TUIDocumentBlockMethod2 = procedure of object;
  TUIDocumentBrowserActionBlockMethod1 = procedure(param1: NSArray) of object;
  TUIDocumentBrowserViewControllerBlockMethod1 = procedure(revealedDocumentURL: NSURL; error: NSError) of object;
  TUIDocumentBrowserViewControllerBlockMethod2 = procedure(param1: NSURL; param2: NSError) of object;
  TUIDocumentBrowserViewControllerDelegateBlockMethod1 = procedure(urlToImport: NSURL; importMode: UIDocumentBrowserImportMode) of object;
  TUIDocumentMenuViewControllerBlockMethod1 = procedure of object;
  TUIDragAnimatingBlockMethod1 = procedure of object;
  TUIDragAnimatingBlockMethod2 = procedure(finalPosition: UIViewAnimatingPosition) of object;
  TUIDragItemBlockMethod1 = function: UIDragPreview of object;
  TUIDragItemBlockMethod2 = procedure of object;
  TUIDropSessionBlockMethod1 = procedure(objects: NSArray) of object;
  TUIDynamicBehaviorBlockMethod1 = procedure of object;
  TUIFieldBehaviorBlockMethod1 = procedure(field: UIFieldBehavior; position: CGPoint; velocity: CGVector; mass: CGFloat; charge: CGFloat;
    deltaTime: NSTimeInterval) of object;
  TUIFocusAnimationCoordinatorBlockMethod1 = procedure of object;
  TUIFocusAnimationCoordinatorBlockMethod2 = procedure(animationContext: Pointer) of object;
  TUIIndirectScribbleInteractionDelegateBlockMethod1 = procedure(elements: NSArray) of object;
  TUIIndirectScribbleInteractionDelegateBlockMethod2 = procedure(focusedInput: UIResponder) of object;
  TUIInputViewControllerBlockMethod1 = procedure(param1: UILexicon) of object;
  TUIMenuBuilderBlockMethod1 = procedure(param1: NSArray) of object;
  TUIPageViewControllerBlockMethod1 = procedure(finished: Boolean) of object;
  TUIPasteboardBlockMethod1 = procedure(param1: NSSet; param2: NSError) of object;
  TUIPasteboardBlockMethod2 = procedure(param1: NSArray; param2: NSError) of object;
  TUIPasteboardBlockMethod3 = procedure(param1: NSDictionary; param2: NSError) of object;
  TUIPointerInteractionAnimatingBlockMethod1 = procedure of object;
  TUIPointerInteractionAnimatingBlockMethod2 = procedure(finished: Boolean) of object;
  TUIPreviewActionBlockMethod1 = procedure(action: UIPreviewAction; previewViewController: UIViewController) of object;
  TUIPreviewActionBlockMethod2 = procedure(param1: Pointer; param2: UIViewController) of object;
  TUIPrinterBlockMethod1 = procedure(available: Boolean) of object;
  TUISceneBlockMethod1 = procedure(success: Boolean) of object;
  TUIScreenshotServiceDelegateBlockMethod1 = procedure(PDFData: NSData; indexOfCurrentPage: NSInteger; rectInCurrentPage: CGRect) of object;
  TUISpringLoadedInteractionBlockMethod1 = procedure(interaction: UISpringLoadedInteraction; context: Pointer) of object;
  TUIStoryboardSegueBlockMethod1 = procedure of object;
  TUITableViewBlockMethod1 = procedure of object;
  TUITableViewBlockMethod2 = procedure(finished: Boolean) of object;
  TUITableViewDropPlaceholderBlockMethod1 = function(param1: UITableViewCell): UIDragPreviewParameters of object;
  TUITableViewDropPlaceholderBlockMethod2 = procedure of object;
  TUITableViewDropPlaceholderContextBlockMethod1 = procedure(insertionIndexPath: NSIndexPath) of object;
  TUITableViewPlaceholderBlockMethod1 = procedure(param1: UITableViewCell) of object;
  TUITableViewPlaceholderBlockMethod2 = procedure of object;
  TUITableViewRowActionBlockMethod1 = procedure(action: UITableViewRowAction; indexPath: NSIndexPath) of object;
  TUITraitCollectionBlockMethod1 = procedure of object;
  TUIViewBlockMethod1 = procedure of object;
  TUIViewBlockMethod2 = procedure(finished: Boolean) of object;
  TUIViewControllerBlockMethod1 = procedure of object;
  TUIViewControllerBlockMethod2 = procedure(finished: Boolean) of object;
  TUIViewControllerTransitionCoordinatorBlockMethod1 = procedure(context: Pointer) of object;
  TUIViewImplicitlyAnimatingBlockMethod1 = procedure of object;
  TUIViewImplicitlyAnimatingBlockMethod2 = procedure(finalPosition: UIViewAnimatingPosition) of object;
  TUIViewPropertyAnimatorBlockMethod1 = procedure of object;
  TUIViewPropertyAnimatorBlockMethod2 = procedure(finalPosition: UIViewAnimatingPosition) of object;
  TUIWindowSceneDelegateBlockMethod1 = procedure(succeeded: Boolean) of object;

  UIAlertActionClass = interface(NSObjectClass)
    ['{953DC1CC-483A-451B-BB66-101217A9C6BF}']
    { class } function actionWithTitle(title: NSString; style: UIAlertActionStyle; handler: TUIAlertActionHandler): Pointer; cdecl;
  end;

  UIAlertAction = interface(NSObject)
    ['{D3CAF467-5D53-48A1-9507-7F934814E1AB}']
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setStyle(style: UIAlertActionStyle); cdecl;
    function style: UIAlertActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TUIAlertAction = class(TOCGenericImport<UIAlertActionClass, UIAlertAction>)
  end;

  UIAlertControllerClass = interface(NSObjectClass)
    ['{DA597BCE-6687-4BCC-8B3F-57205A141193}']
    { class } function alertControllerWithTitle(title: NSString; message: NSString; preferredStyle: UIAlertControllerStyle) : Pointer; cdecl;
  end;

  UIAlertController = interface(UIViewController)
    ['{68C4436A-A75E-4825-A578-8D26894B0469}']
    function actions: NSArray; cdecl;
    procedure addAction(action: UIAlertAction); cdecl;
    procedure addTextFieldWithConfigurationHandler(configurationHandler: TUIAlertControllerConfigurationHandler); cdecl;
    function message: NSString; cdecl;
    function preferredAction: UIAlertAction; cdecl;
    function preferredStyle: UIAlertControllerStyle; cdecl;
    procedure setMessage(message: NSString); cdecl;
    procedure setPreferredAction(preferredAction: UIAlertAction); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function textFields: NSArray; cdecl;
    function title: NSString; cdecl;
  end;
  TUIAlertController = class(TOCGenericImport<UIAlertControllerClass, UIAlertController>)
  end;

  TBackgroundTaskHandler = procedure of object;
  TOpenURLCompletionHandler = procedure(success: Boolean) of object;

  UIApplication = interface(iOSapi.UIKit.UIApplication)
    ['{EC184C87-9109-4D61-9BCA-291F70D90D15}']
    function beginBackgroundTaskWithExpirationHandler(handler: TBackgroundTaskHandler): UIBackgroundTaskIdentifier; cdecl;
    procedure endBackgroundTask(identifier: UIBackgroundTaskIdentifier); cdecl;
    function isRegisteredForRemoteNotifications: Boolean; cdecl;
    [MethodName('openURL:options:completionHandler:')]
    procedure openURLOptionsCompletionHandler(url: NSURL; options: NSDictionary; completionHandler: TOpenURLCompletionHandler); cdecl;
    procedure setMinimumBackgroundFetchInterval(minimumBackgroundFetchInterval: NSTimeInterval); cdecl;
  end;
  TUIApplication = class(TOCGenericImport<UIApplicationClass, UIApplication>) end;

  UILayoutGuideClass = interface(NSObjectClass)
    ['{AF677015-5698-4B26-A46E-147882759C1E}']
  end;

  UILayoutGuide = interface(NSObject)
    ['{AAA6A249-5597-412A-95E4-FC087F6794D7}']
    function bottomAnchor: NSLayoutYAxisAnchor; cdecl;
    function centerXAnchor: NSLayoutXAxisAnchor; cdecl;
    function centerYAnchor: NSLayoutYAxisAnchor; cdecl;
    function constraintsAffectingLayoutForAxis(axis: UILayoutConstraintAxis): NSArray; cdecl;
    function hasAmbiguousLayout: Boolean; cdecl;
    function heightAnchor: NSLayoutDimension; cdecl;
    function identifier: NSString; cdecl;
    function layoutFrame: CGRect; cdecl;
    function leadingAnchor: NSLayoutXAxisAnchor; cdecl;
    function leftAnchor: NSLayoutXAxisAnchor; cdecl;
    function owningView: UIView; cdecl;
    function rightAnchor: NSLayoutXAxisAnchor; cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setOwningView(owningView: UIView); cdecl;
    function topAnchor: NSLayoutYAxisAnchor; cdecl;
    function trailingAnchor: NSLayoutXAxisAnchor; cdecl;
    function widthAnchor: NSLayoutDimension; cdecl;
  end;
  TUILayoutGuide = class(TOCGenericImport<UILayoutGuideClass, UILayoutGuide>) end;

  UICommandAlternateClass = interface(NSObjectClass)
    ['{2DF7EBD5-10CB-4087-A995-70B019610D8C}']
    {class} function alternateWithTitle(title: NSString; action: Pointer; modifierFlags: UIKeyModifierFlags): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UICommandAlternate = interface(NSObject)
    ['{8C5D1945-4872-4A2F-A6D6-9B7B17AB9B27}']
    function action: Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function modifierFlags: UIKeyModifierFlags; cdecl;
    function title: NSString; cdecl;
  end;
  TUICommandAlternate = class(TOCGenericImport<UICommandAlternateClass, UICommandAlternate>) end;

  UICommandClass = interface(UIMenuElementClass)
    ['{61B8BE4C-B48E-44D1-812F-BC385C039517}']
    {class} function commandWithTitle(title: NSString; image: UIImage; action: Pointer; propertyList: Pointer; alternates: NSArray): Pointer; overload; cdecl;
    {class} function commandWithTitle(title: NSString; image: UIImage; action: Pointer; propertyList: Pointer): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UICommand = interface(UIMenuElement)
    ['{88316642-66AB-4A1E-A391-A6692B5620B1}']
    function action: Pointer; cdecl;
    function alternates: NSArray; cdecl;
    function attributes: UIMenuElementAttributes; cdecl;
    function discoverabilityTitle: NSString; cdecl;
    function image: UIImage; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function propertyList: Pointer; cdecl;
    procedure setAttributes(attributes: UIMenuElementAttributes); cdecl;
    procedure setDiscoverabilityTitle(discoverabilityTitle: NSString); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setState(state: UIMenuElementState); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function state: UIMenuElementState; cdecl;
    function title: NSString; cdecl;
  end;
  TUICommand = class(TOCGenericImport<UICommandClass, UICommand>) end;

  UIPasteConfigurationSupporting = interface(IObjectiveC)
    ['{E95042FB-ACD5-4E1A-A375-1D99E686FFEA}']
    function canPasteItemProviders(itemProviders: NSArray): Boolean; cdecl;
    function pasteConfiguration: UIPasteConfiguration; cdecl;
    procedure pasteItemProviders(itemProviders: NSArray); cdecl;
    procedure setPasteConfiguration(pasteConfiguration: UIPasteConfiguration); cdecl;
  end;

  UIUserActivityRestoring = interface(IObjectiveC)
    ['{81EDAF52-AF01-4C3F-A095-F6E216F73922}']
    procedure restoreUserActivityState(userActivity: NSUserActivity); cdecl;
  end;

  UIResponderStandardEditActions = interface(IObjectiveC)
    ['{1B52D6F6-3B6D-4E5E-B45E-5D880BC04164}']
    procedure copy(sender: Pointer); cdecl;
    procedure cut(sender: Pointer); cdecl;
    procedure decreaseSize(sender: Pointer); cdecl;
    procedure delete(sender: Pointer); cdecl;
    procedure increaseSize(sender: Pointer); cdecl;
    procedure makeTextWritingDirectionLeftToRight(sender: Pointer); cdecl;
    procedure makeTextWritingDirectionRightToLeft(sender: Pointer); cdecl;
    procedure paste(sender: Pointer); cdecl;
    procedure select(sender: Pointer); cdecl;
    procedure selectAll(sender: Pointer); cdecl;
    procedure toggleBoldface(sender: Pointer); cdecl;
    procedure toggleItalics(sender: Pointer); cdecl;
    procedure toggleUnderline(sender: Pointer); cdecl;
    procedure updateTextAttributesWithConversionHandler(conversionHandler: UITextAttributesConversionHandler); cdecl;
  end;

  UIDynamicItemGroupClass = interface(NSObjectClass)
    ['{FE5E30F3-838B-4730-AA94-ED2E2E53904F}']
  end;

  UIDynamicItemGroup = interface(NSObject)
    ['{0790CA30-F8B6-43F1-9F61-F198FF87D350}']
    function initWithItems(items: NSArray): Pointer; cdecl;
    function items: NSArray; cdecl;
  end;
  TUIDynamicItemGroup = class(TOCGenericImport<UIDynamicItemGroupClass, UIDynamicItemGroup>) end;

  UIFocusGuideClass = interface(UILayoutGuideClass)
    ['{D2165EB9-D13F-4D05-8419-F80A78621435}']
  end;

  UIFocusGuide = interface(UILayoutGuide)
    ['{2DED064F-7B67-4E1B-A536-D063B154BA77}']
    function isEnabled: Boolean; cdecl;
    function preferredFocusedView: UIView; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredFocusEnvironments", ios(9.0, 10.0))
    function preferredFocusEnvironments: NSArray; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setPreferredFocusedView(preferredFocusedView: UIView); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredFocusEnvironments", ios(9.0, 10.0))
    procedure setPreferredFocusEnvironments(preferredFocusEnvironments: NSArray); cdecl;
  end;
  TUIFocusGuide = class(TOCGenericImport<UIFocusGuideClass, UIFocusGuide>) end;

  UIFocusAnimationContext = interface(IObjectiveC)
    ['{B2E53ED1-C39D-44A1-8DF1-B86F7E7AEE28}']
    function duration: NSTimeInterval; cdecl;
  end;

  UIFocusAnimationCoordinatorClass = interface(NSObjectClass)
    ['{023CDC6F-562B-4A90-B3B5-1DD0593C52FA}']
  end;

  UIFocusAnimationCoordinator = interface(NSObject)
    ['{1DEAF6A4-2151-4B20-A6C7-F2B4ACC90B3C}']
    procedure addCoordinatedAnimations(animations: TUIFocusAnimationCoordinatorBlockMethod1; completion: TUIFocusAnimationCoordinatorBlockMethod1); cdecl;
    procedure addCoordinatedFocusingAnimations(animations: TUIFocusAnimationCoordinatorBlockMethod2; completion: TUIFocusAnimationCoordinatorBlockMethod1); cdecl;
    procedure addCoordinatedUnfocusingAnimations(animations: TUIFocusAnimationCoordinatorBlockMethod2; completion: TUIFocusAnimationCoordinatorBlockMethod1); cdecl;
  end;
  TUIFocusAnimationCoordinator = class(TOCGenericImport<UIFocusAnimationCoordinatorClass, UIFocusAnimationCoordinator>) end;

  UIFocusEnvironment = interface(IObjectiveC)
    ['{C0EABE0B-BC1F-4BA3-8E72-1F922C281DEE}']
    procedure didUpdateFocusInContext(context: UIFocusUpdateContext; withAnimationCoordinator: UIFocusAnimationCoordinator); cdecl;
    function focusGroupIdentifier: NSString; cdecl;
    function focusItemContainer: Pointer; cdecl;
    function parentFocusEnvironment: Pointer; cdecl;
    function preferredFocusedView: UIView; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredFocusEnvironments", ios(9.0, 10.0))
    function preferredFocusEnvironments: NSArray; cdecl;
    procedure setNeedsFocusUpdate; cdecl;
    function shouldUpdateFocusInContext(context: UIFocusUpdateContext): Boolean; cdecl;
    function soundIdentifierForFocusUpdateInContext(context: UIFocusUpdateContext): UIFocusSoundIdentifier; cdecl;
    procedure updateFocusIfNeeded; cdecl;
  end;

  UIFocusItem = interface(IObjectiveC)
    ['{CF9F8E97-03F4-441E-9EEA-96AC36D7BDE8}']
    function canBecomeFocused: Boolean; cdecl;
    procedure didHintFocusMovement(hint: UIFocusMovementHint); cdecl;
    function frame: CGRect; cdecl;
  end;

  UIFocusItemContainer = interface(IObjectiveC)
    ['{9BC65F91-F2A5-4446-BFE2-1583C1A71701}']
    function coordinateSpace: Pointer; cdecl;
    function focusItemsInRect(rect: CGRect): NSArray; cdecl;
  end;

  UIFocusItemScrollableContainer = interface(IObjectiveC)
    ['{651229C9-ACF0-47AE-BD60-BF292F0C8D82}']
    function contentOffset: CGPoint; cdecl;
    function contentSize: CGSize; cdecl;
    procedure setContentOffset(contentOffset: CGPoint); cdecl;
    function visibleSize: CGSize; cdecl;
  end;

  UIFocusUpdateContextClass = interface(NSObjectClass)
    ['{66E9CB97-886D-4232-89BC-0110023EBE16}']
  end;

  UIFocusUpdateContext = interface(NSObject)
    ['{B182F2A0-5F13-4AAC-A778-AA827E02100D}']
    function focusHeading: UIFocusHeading; cdecl;
    function nextFocusedItem: Pointer; cdecl;
    function nextFocusedView: UIView; cdecl;
    function previouslyFocusedItem: Pointer; cdecl;
    function previouslyFocusedView: UIView; cdecl;
  end;
  TUIFocusUpdateContext = class(TOCGenericImport<UIFocusUpdateContextClass, UIFocusUpdateContext>) end;

  UICoordinateSpace = interface(IObjectiveC)
    ['{8D706D21-4667-425D-BB9D-D0125B249CD7}']
    function bounds: CGRect; cdecl;
    [MethodName('convertPoint:fromCoordinateSpace:')]
    function convertPointFromCoordinateSpace(point: CGPoint; fromCoordinateSpace: Pointer): CGPoint; cdecl;
    [MethodName('convertPoint:toCoordinateSpace:')]
    function convertPointToCoordinateSpace(point: CGPoint; toCoordinateSpace: Pointer): CGPoint; cdecl;
    [MethodName('convertRect:fromCoordinateSpace:')]
    function convertRectFromCoordinateSpace(rect: CGRect; fromCoordinateSpace: Pointer): CGRect; cdecl;
    [MethodName('convertRect:toCoordinateSpace:')]
    function convertRectToCoordinateSpace(rect: CGRect; toCoordinateSpace: Pointer): CGRect; cdecl;
  end;

  UIInteraction = interface(IObjectiveC)
    ['{98A906FA-AD9C-480D-93C0-22F4C66F603A}']
    procedure didMoveToView(view: UIView); cdecl;
    function view: UIView; cdecl;
    procedure willMoveToView(view: UIView); cdecl;
  end;

  UIActionClass = interface(UIMenuElementClass)
    ['{84ADEF5C-9442-44F4-BFFF-565617972DFC}']
    {class} function actionWithHandler(handler: UIActionHandler): Pointer; cdecl;
    {class} function actionWithTitle(title: NSString; image: UIImage; identifier: UIActionIdentifier; handler: UIActionHandler): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIAction = interface(UIMenuElement)
    ['{929EB125-1C01-4DC0-8963-9834C0E8CDA5}']
    function attributes: UIMenuElementAttributes; cdecl;
    function discoverabilityTitle: NSString; cdecl;
    function identifier: UIActionIdentifier; cdecl;
    function image: UIImage; cdecl;
    function sender: Pointer; cdecl;
    procedure setAttributes(attributes: UIMenuElementAttributes); cdecl;
    procedure setDiscoverabilityTitle(discoverabilityTitle: NSString); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setState(state: UIMenuElementState); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function state: UIMenuElementState; cdecl;
    function title: NSString; cdecl;
  end;
  TUIAction = class(TOCGenericImport<UIActionClass, UIAction>) end;

  UIContextMenuInteractionClass = interface(NSObjectClass)
    ['{0DAE92AE-F0F5-45FE-A414-6B96F1BFDF1E}']
    {class} function new: Pointer; cdecl;
  end;

  UIContextMenuInteraction = interface(NSObject)
    ['{2FBB99E8-E25B-40A5-BC5F-EDF2F315244C}']
    function delegate: Pointer; cdecl;
    procedure dismissMenu; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function locationInView(view: UIView): CGPoint; cdecl;
    function menuAppearance: UIContextMenuInteractionAppearance; cdecl;
    procedure updateVisibleMenuWithBlock(block: TUIContextMenuInteractionBlockMethod1); cdecl;
  end;
  TUIContextMenuInteraction = class(TOCGenericImport<UIContextMenuInteractionClass, UIContextMenuInteraction>) end;

  UIContextMenuInteractionAnimating = interface(IObjectiveC)
    ['{CF911B70-C05A-4B9E-BA4C-2D33DAFA7B6C}']
    procedure addAnimations(animations: TUIContextMenuInteractionAnimatingBlockMethod1); cdecl;
    procedure addCompletion(completion: TUIContextMenuInteractionAnimatingBlockMethod1); cdecl;
    function previewViewController: UIViewController; cdecl;
  end;

  UIContextMenuInteractionCommitAnimating = interface(IObjectiveC)
    ['{8E16D6AE-57C6-4B89-BDCA-2E75A03B5EB9}']
    function preferredCommitStyle: UIContextMenuInteractionCommitStyle; cdecl;
    procedure setPreferredCommitStyle(preferredCommitStyle: UIContextMenuInteractionCommitStyle); cdecl;
  end;

  UIContextMenuInteractionDelegate = interface(IObjectiveC)
    ['{FCE6DD81-A7FB-4B17-8925-2FCFF65F6A26}']
    [MethodName('contextMenuInteraction:configurationForMenuAtLocation:')]
    function contextMenuInteractionConfigurationForMenuAtLocation(interaction: UIContextMenuInteraction; configurationForMenuAtLocation: CGPoint): UIContextMenuConfiguration; cdecl;
    [MethodName('contextMenuInteraction:previewForDismissingMenuWithConfiguration:')]
    function contextMenuInteractionPreviewForDismissingMenuWithConfiguration(interaction: UIContextMenuInteraction; previewForDismissingMenuWithConfiguration: UIContextMenuConfiguration): UITargetedPreview; cdecl;
    [MethodName('contextMenuInteraction:previewForHighlightingMenuWithConfiguration:')]
    function contextMenuInteractionPreviewForHighlightingMenuWithConfiguration(interaction: UIContextMenuInteraction; previewForHighlightingMenuWithConfiguration: UIContextMenuConfiguration): UITargetedPreview; cdecl;
    [MethodName('contextMenuInteraction:willDisplayMenuForConfiguration:animator:')]
    procedure contextMenuInteractionWillDisplayMenuForConfiguration(interaction: UIContextMenuInteraction; willDisplayMenuForConfiguration: UIContextMenuConfiguration; animator: Pointer); cdecl;
    [MethodName('contextMenuInteraction:willEndForConfiguration:animator:')]
    procedure contextMenuInteractionWillEndForConfiguration(interaction: UIContextMenuInteraction; willEndForConfiguration: UIContextMenuConfiguration; animator: Pointer); cdecl;
    [MethodName('contextMenuInteraction:willPerformPreviewActionForMenuWithConfiguration:animator:')]
    procedure contextMenuInteractionWillPerformPreviewActionForMenuWithConfiguration(interaction: UIContextMenuInteraction; willPerformPreviewActionForMenuWithConfiguration: UIContextMenuConfiguration; animator: Pointer); cdecl;
  end;

  UIDropInteractionClass = interface(NSObjectClass)
    ['{72F47CE3-E6D2-47EF-926F-3257451CB46E}']
    {class} function new: Pointer; cdecl;
  end;

  UIDropInteraction = interface(NSObject)
    ['{1D2F2FB4-830C-4218-B4B1-90E81B2EBEA2}']
    function allowsSimultaneousDropSessions: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    procedure setAllowsSimultaneousDropSessions(allowsSimultaneousDropSessions: Boolean); cdecl;
  end;
  TUIDropInteraction = class(TOCGenericImport<UIDropInteractionClass, UIDropInteraction>) end;

  UIDropProposalClass = interface(NSObjectClass)
    ['{D093F830-C7E2-4EB3-93EA-506B8E3EC3AA}']
    {class} function new: Pointer; cdecl;
  end;

  UIDropProposal = interface(NSObject)
    ['{2F0E6B61-9869-4841-B391-73B5FDF1DE2B}']
    function initWithDropOperation(operation: UIDropOperation): Pointer; cdecl;
    function isPrecise: Boolean; cdecl;
    function operation: UIDropOperation; cdecl;
    function prefersFullSizePreview: Boolean; cdecl;
    procedure setPrecise(precise: Boolean); cdecl;
    procedure setPrefersFullSizePreview(prefersFullSizePreview: Boolean); cdecl;
  end;
  TUIDropProposal = class(TOCGenericImport<UIDropProposalClass, UIDropProposal>) end;

  UIDropInteractionDelegate = interface(IObjectiveC)
    ['{2510D86F-A7CD-40C7-800B-A0250B5C7AB3}']
    [MethodName('dropInteraction:canHandleSession:')]
    function dropInteractionCanHandleSession(interaction: UIDropInteraction; canHandleSession: Pointer): Boolean; cdecl;
    [MethodName('dropInteraction:concludeDrop:')]
    procedure dropInteractionConcludeDrop(interaction: UIDropInteraction; concludeDrop: Pointer); cdecl;
    [MethodName('dropInteraction:item:willAnimateDropWithAnimator:')]
    procedure dropInteractionItem(interaction: UIDropInteraction; item: UIDragItem; willAnimateDropWithAnimator: Pointer); cdecl;
    [MethodName('dropInteraction:performDrop:')]
    procedure dropInteractionPerformDrop(interaction: UIDropInteraction; performDrop: Pointer); cdecl;
    [MethodName('dropInteraction:previewForDroppingItem:withDefault:')]
    function dropInteractionPreviewForDroppingItem(interaction: UIDropInteraction; previewForDroppingItem: UIDragItem; withDefault: UITargetedDragPreview): UITargetedDragPreview; cdecl;
    [MethodName('dropInteraction:sessionDidEnd:')]
    procedure dropInteractionSessionDidEnd(interaction: UIDropInteraction; sessionDidEnd: Pointer); cdecl;
    [MethodName('dropInteraction:sessionDidEnter:')]
    procedure dropInteractionSessionDidEnter(interaction: UIDropInteraction; sessionDidEnter: Pointer); cdecl;
    [MethodName('dropInteraction:sessionDidExit:')]
    procedure dropInteractionSessionDidExit(interaction: UIDropInteraction; sessionDidExit: Pointer); cdecl;
    [MethodName('dropInteraction:sessionDidUpdate:')]
    function dropInteractionSessionDidUpdate(interaction: UIDropInteraction; sessionDidUpdate: Pointer): UIDropProposal; cdecl;
  end;

  UIViewAnimating = interface(IObjectiveC)
    ['{AB1D42BC-CD7E-46DE-92A6-009908CEFFB7}']
    procedure finishAnimationAtPosition(finalPosition: UIViewAnimatingPosition); cdecl;
    function fractionComplete: CGFloat; cdecl;
    function isReversed: Boolean; cdecl;
    function isRunning: Boolean; cdecl;
    procedure pauseAnimation; cdecl;
    procedure setFractionComplete(fractionComplete: CGFloat); cdecl;
    procedure setReversed(reversed: Boolean); cdecl;
    procedure startAnimation; cdecl;
    procedure startAnimationAfterDelay(delay: NSTimeInterval); cdecl;
    function state: UIViewAnimatingState; cdecl;
    procedure stopAnimation(withoutFinishing: Boolean); cdecl;
  end;

  UIViewImplicitlyAnimating = interface(IObjectiveC)
    ['{1B736EF0-594B-4151-844A-229AF9B719F1}']
    procedure addAnimations(animation: TUIViewImplicitlyAnimatingBlockMethod1); overload; cdecl;
    procedure addAnimations(animation: TUIViewImplicitlyAnimatingBlockMethod1; delayFactor: CGFloat); overload; cdecl;
    procedure addCompletion(completion: TUIViewImplicitlyAnimatingBlockMethod2); cdecl;
    procedure continueAnimationWithTimingParameters(parameters: Pointer; durationFactor: CGFloat); cdecl;
  end;

  UIDragAnimating = interface(IObjectiveC)
    ['{E7BC85BE-A677-46FA-BEC3-7CEA4AB1EA42}']
    procedure addAnimations(animations: TUIDragAnimatingBlockMethod1); cdecl;
    procedure addCompletion(completion: TUIDragAnimatingBlockMethod2); cdecl;
  end;

  UIDragInteractionClass = interface(NSObjectClass)
    ['{C22D7CE0-1B31-4568-A2F7-1011E09CA380}']
    {class} function isEnabledByDefault: Boolean; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIDragInteraction = interface(NSObject)
    ['{A48BE24D-FCA9-4DB9-92BF-0C9B8F9DC3C6}']
    function allowsSimultaneousRecognitionDuringLift: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setAllowsSimultaneousRecognitionDuringLift(allowsSimultaneousRecognitionDuringLift: Boolean); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TUIDragInteraction = class(TOCGenericImport<UIDragInteractionClass, UIDragInteraction>) end;

  UIDragInteractionDelegate = interface(IObjectiveC)
    ['{8883EB5C-07BF-414F-B02C-45F0C6C78B28}']
    [MethodName('dragInteraction:item:willAnimateCancelWithAnimator:')]
    procedure dragInteractionItem(interaction: UIDragInteraction; item: UIDragItem; willAnimateCancelWithAnimator: Pointer); cdecl;
    [MethodName('dragInteraction:itemsForAddingToSession:withTouchAtPoint:')]
    function dragInteractionItemsForAddingToSession(interaction: UIDragInteraction; itemsForAddingToSession: Pointer; withTouchAtPoint: CGPoint): NSArray; cdecl;
    [MethodName('dragInteraction:itemsForBeginningSession:')]
    function dragInteractionItemsForBeginningSession(interaction: UIDragInteraction; itemsForBeginningSession: Pointer): NSArray; cdecl;
    [MethodName('dragInteraction:prefersFullSizePreviewsForSession:')]
    function dragInteractionPrefersFullSizePreviewsForSession(interaction: UIDragInteraction; prefersFullSizePreviewsForSession: Pointer): Boolean; cdecl;
    [MethodName('dragInteraction:previewForCancellingItem:withDefault:')]
    function dragInteractionPreviewForCancellingItem(interaction: UIDragInteraction; previewForCancellingItem: UIDragItem; withDefault: UITargetedDragPreview): UITargetedDragPreview; cdecl;
    [MethodName('dragInteraction:previewForLiftingItem:session:')]
    function dragInteractionPreviewForLiftingItem(interaction: UIDragInteraction; previewForLiftingItem: UIDragItem; session: Pointer): UITargetedDragPreview; cdecl;
    [MethodName('dragInteraction:session:willEndWithOperation:')]
    procedure dragInteractionSession(interaction: UIDragInteraction; session: Pointer; willEndWithOperation: UIDropOperation); overload; cdecl;
    [MethodName('dragInteraction:session:willAddItems:forInteraction:')]
    procedure dragInteractionSession(interaction: UIDragInteraction; session: Pointer; willAddItems: NSArray; forInteraction: UIDragInteraction); overload; cdecl;
    [MethodName('dragInteraction:session:didEndWithOperation:')]
    procedure dragInteractionSessionDidEndWithOperation(interaction: UIDragInteraction; session: Pointer; didEndWithOperation: UIDropOperation); cdecl;
    [MethodName('dragInteraction:sessionAllowsMoveOperation:')]
    function dragInteractionSessionAllowsMoveOperation(interaction: UIDragInteraction; sessionAllowsMoveOperation: Pointer): Boolean; cdecl;
    [MethodName('dragInteraction:sessionDidMove:')]
    procedure dragInteractionSessionDidMove(interaction: UIDragInteraction; sessionDidMove: Pointer); cdecl;
    [MethodName('dragInteraction:sessionDidTransferItems:')]
    procedure dragInteractionSessionDidTransferItems(interaction: UIDragInteraction; sessionDidTransferItems: Pointer); cdecl;
    [MethodName('dragInteraction:sessionForAddingItems:withTouchAtPoint:')]
    function dragInteractionSessionForAddingItems(interaction: UIDragInteraction; sessionForAddingItems: NSArray; withTouchAtPoint: CGPoint): Pointer; cdecl;
    [MethodName('dragInteraction:sessionIsRestrictedToDraggingApplication:')]
    function dragInteractionSessionIsRestrictedToDraggingApplication(interaction: UIDragInteraction; sessionIsRestrictedToDraggingApplication: Pointer): Boolean; cdecl;
    [MethodName('dragInteraction:sessionWillBegin:')]
    procedure dragInteractionSessionWillBegin(interaction: UIDragInteraction; sessionWillBegin: Pointer); cdecl;
    [MethodName('dragInteraction:willAnimateLiftWithAnimator:session:')]
    procedure dragInteractionWillAnimateLiftWithAnimator(interaction: UIDragInteraction; willAnimateLiftWithAnimator: Pointer; session: Pointer); cdecl;
  end;

  UITextInputPasswordRulesClass = interface(NSObjectClass)
    ['{70586C5E-30E2-4223-A8A2-93D6723CE64E}']
    {class} function new: Pointer; cdecl;
    {class} function passwordRulesWithDescriptor(passwordRulesDescriptor: NSString): Pointer; cdecl;
  end;

  UITextInputPasswordRules = interface(NSObject)
    ['{9C1E7BAD-3ED3-4C87-A938-F15B7C05A431}']
    function passwordRulesDescriptor: NSString; cdecl;
  end;
  TUITextInputPasswordRules = class(TOCGenericImport<UITextInputPasswordRulesClass, UITextInputPasswordRules>) end;

  UITextInputAssistantItemClass = interface(NSObjectClass)
    ['{2AAAE150-6C18-4127-A699-9BF825EDA72D}']
  end;

  UITextInputAssistantItem = interface(NSObject)
    ['{604C1706-BFB3-4014-A19C-049C67B58E5B}']
    function allowsHidingShortcuts: Boolean; cdecl;
    function leadingBarButtonGroups: NSArray; cdecl;
    procedure setAllowsHidingShortcuts(allowsHidingShortcuts: Boolean); cdecl;
    procedure setLeadingBarButtonGroups(leadingBarButtonGroups: NSArray); cdecl;
    procedure setTrailingBarButtonGroups(trailingBarButtonGroups: NSArray); cdecl;
    function trailingBarButtonGroups: NSArray; cdecl;
  end;
  TUITextInputAssistantItem = class(TOCGenericImport<UITextInputAssistantItemClass, UITextInputAssistantItem>) end;

  UITextPlaceholderClass = interface(NSObjectClass)
    ['{86554913-5FC7-43D9-AC9C-EA5F27D99EAD}']
  end;

  UITextPlaceholder = interface(NSObject)
    ['{98E8DB88-E7CD-49E9-8077-7292F6E36762}']
    function rects: NSArray; cdecl;
  end;
  TUITextPlaceholder = class(TOCGenericImport<UITextPlaceholderClass, UITextPlaceholder>) end;

  UITextDraggable = interface(IObjectiveC)
    ['{A74CBAB8-85E3-4501-AE3B-EA544FE108AC}']
    function isTextDragActive: Boolean; cdecl;
    procedure setTextDragDelegate(textDragDelegate: Pointer); cdecl;
    procedure setTextDragOptions(textDragOptions: UITextDragOptions); cdecl;
    function textDragDelegate: Pointer; cdecl;
    function textDragInteraction: UIDragInteraction; cdecl;
    function textDragOptions: UITextDragOptions; cdecl;
  end;

  UITextDragDelegate = interface(IObjectiveC)
    ['{AE2CC93F-9A69-4F42-BC49-09B63912991F}']
    [MethodName('textDraggableView:dragPreviewForLiftingItem:session:')]
    function textDraggableViewDragPreviewForLiftingItem(textDraggableView: UIView; dragPreviewForLiftingItem: UIDragItem; session: Pointer): UITargetedDragPreview; cdecl;
    [MethodName('textDraggableView:dragSessionDidEnd:withOperation:')]
    procedure textDraggableViewDragSessionDidEnd(textDraggableView: UIView; dragSessionDidEnd: Pointer; withOperation: UIDropOperation); cdecl;
    [MethodName('textDraggableView:dragSessionWillBegin:')]
    procedure textDraggableViewDragSessionWillBegin(textDraggableView: UIView; dragSessionWillBegin: Pointer); cdecl;
    [MethodName('textDraggableView:itemsForDrag:')]
    function textDraggableViewItemsForDrag(textDraggableView: UIView; itemsForDrag: Pointer): NSArray; cdecl;
    [MethodName('textDraggableView:willAnimateLiftWithAnimator:session:')]
    procedure textDraggableViewWillAnimateLiftWithAnimator(textDraggableView: UIView; willAnimateLiftWithAnimator: Pointer; session: Pointer); cdecl;
  end;

  UITextDragRequest = interface(IObjectiveC)
    ['{E3AA1312-7374-4C70-A7C2-296127E3788A}']
    function dragRange: UITextRange; cdecl;
    function dragSession: Pointer; cdecl;
    function existingItems: NSArray; cdecl;
    function isSelected: Boolean; cdecl;
    function suggestedItems: NSArray; cdecl;
  end;

  UITextDropProposalClass = interface(UIDropProposalClass)
    ['{1D07087B-5180-4089-B669-37869FBA07D9}']
  end;

  UITextDropProposal = interface(UIDropProposal)
    ['{FE1761C2-B5D4-4788-9F3A-1DFECD3375FD}']
    function dropAction: UITextDropAction; cdecl;
    function dropPerformer: UITextDropPerformer; cdecl;
    function dropProgressMode: UITextDropProgressMode; cdecl;
    procedure setDropAction(dropAction: UITextDropAction); cdecl;
    procedure setDropPerformer(dropPerformer: UITextDropPerformer); cdecl;
    procedure setDropProgressMode(dropProgressMode: UITextDropProgressMode); cdecl;
    procedure setUseFastSameViewOperations(useFastSameViewOperations: Boolean); cdecl;
    function useFastSameViewOperations: Boolean; cdecl;
  end;
  TUITextDropProposal = class(TOCGenericImport<UITextDropProposalClass, UITextDropProposal>) end;

  UIPasteConfigurationClass = interface(NSObjectClass)
    ['{6E055EC4-00E4-4EAB-A008-2B27C19F63DE}']
  end;

  UIPasteConfiguration = interface(NSObject)
    ['{C45298C0-E4B3-4D74-B782-4B3FFF85811B}']
    function acceptableTypeIdentifiers: NSArray; cdecl;
    procedure addAcceptableTypeIdentifiers(acceptableTypeIdentifiers: NSArray); cdecl;
    procedure addTypeIdentifiersForAcceptingClass(aClass: Pointer); cdecl;
    function initWithAcceptableTypeIdentifiers(acceptableTypeIdentifiers: NSArray): Pointer; cdecl;
    function initWithTypeIdentifiersForAcceptingClass(aClass: Pointer): Pointer; cdecl;
    procedure setAcceptableTypeIdentifiers(acceptableTypeIdentifiers: NSArray); cdecl;
  end;
  TUIPasteConfiguration = class(TOCGenericImport<UIPasteConfigurationClass, UIPasteConfiguration>) end;

  UITextPasteDelegate = interface(IObjectiveC)
    ['{F317869C-D465-4C7F-AE7D-D2581C046747}']
    [MethodName('textPasteConfigurationSupporting:combineItemAttributedStrings:forRange:')]
    function textPasteConfigurationSupportingCombineItemAttributedStrings(textPasteConfigurationSupporting: Pointer; combineItemAttributedStrings: NSArray; forRange: UITextRange): NSAttributedString; cdecl;
    [MethodName('textPasteConfigurationSupporting:performPasteOfAttributedString:toRange:')]
    function textPasteConfigurationSupportingPerformPasteOfAttributedString(textPasteConfigurationSupporting: Pointer; performPasteOfAttributedString: NSAttributedString; toRange: UITextRange): UITextRange; cdecl;
    [MethodName('textPasteConfigurationSupporting:shouldAnimatePasteOfAttributedString:toRange:')]
    function textPasteConfigurationSupportingShouldAnimatePasteOfAttributedString(textPasteConfigurationSupporting: Pointer; shouldAnimatePasteOfAttributedString: NSAttributedString; toRange: UITextRange): Boolean; cdecl;
    [MethodName('textPasteConfigurationSupporting:transformPasteItem:')]
    procedure textPasteConfigurationSupportingTransformPasteItem(textPasteConfigurationSupporting: Pointer; transformPasteItem: Pointer); cdecl;
  end;

  UITextPasteItem = interface(IObjectiveC)
    ['{AD4FD763-687F-4C28-A5D9-722210423F6B}']
    function defaultAttributes: NSDictionary; cdecl;
    // Foundation function itemProvider: NSItemProvider; cdecl;
    function localObject: Pointer; cdecl;
    procedure setAttachmentResult(textAttachment: NSTextAttachment); cdecl;
    procedure setAttributedStringResult(&string: NSAttributedString); cdecl;
    procedure setDefaultResult; cdecl;
    procedure setNoResult; cdecl;
    procedure setStringResult(&string: NSString); cdecl;
  end;

  UITextPasteConfigurationSupporting = interface(IObjectiveC)
    ['{A310699F-B730-4AF4-973A-C2D7E490E921}']
    function pasteDelegate: Pointer; cdecl;
    procedure setPasteDelegate(pasteDelegate: Pointer); cdecl;
  end;

  UITextDroppable = interface(IObjectiveC)
    ['{B442A157-C699-4849-A32F-EB1957617ABC}']
    function isTextDropActive: Boolean; cdecl;
    procedure setTextDropDelegate(textDropDelegate: Pointer); cdecl;
    function textDropDelegate: Pointer; cdecl;
    function textDropInteraction: UIDropInteraction; cdecl;
  end;

  UITextDropDelegate = interface(IObjectiveC)
    ['{2367275D-4236-4591-A625-C3B694B99030}']
    [MethodName('textDroppableView:dropSessionDidEnd:')]
    procedure textDroppableViewDropSessionDidEnd(textDroppableView: UIView; dropSessionDidEnd: Pointer); cdecl;
    [MethodName('textDroppableView:dropSessionDidEnter:')]
    procedure textDroppableViewDropSessionDidEnter(textDroppableView: UIView; dropSessionDidEnter: Pointer); cdecl;
    [MethodName('textDroppableView:dropSessionDidExit:')]
    procedure textDroppableViewDropSessionDidExit(textDroppableView: UIView; dropSessionDidExit: Pointer); cdecl;
    [MethodName('textDroppableView:dropSessionDidUpdate:')]
    procedure textDroppableViewDropSessionDidUpdate(textDroppableView: UIView; dropSessionDidUpdate: Pointer); cdecl;
    [MethodName('textDroppableView:previewForDroppingAllItemsWithDefault:')]
    function textDroppableViewPreviewForDroppingAllItemsWithDefault(textDroppableView: UIView; previewForDroppingAllItemsWithDefault: UITargetedDragPreview): UITargetedDragPreview; cdecl;
    [MethodName('textDroppableView:proposalForDrop:')]
    function textDroppableViewProposalForDrop(textDroppableView: UIView; proposalForDrop: Pointer): UITextDropProposal; cdecl;
    [MethodName('textDroppableView:willBecomeEditableForDrop:')]
    function textDroppableViewWillBecomeEditableForDrop(textDroppableView: UIView; willBecomeEditableForDrop: Pointer): UITextDropEditability; cdecl;
    [MethodName('textDroppableView:willPerformDrop:')]
    procedure textDroppableViewWillPerformDrop(textDroppableView: UIView; willPerformDrop: Pointer); cdecl;
  end;

  UITextDropRequest = interface(IObjectiveC)
    ['{A11008C8-7D18-409B-A79D-5C2BD9796256}']
    function dropPosition: UITextPosition; cdecl;
    function dropSession: Pointer; cdecl;
    function isSameView: Boolean; cdecl;
    function suggestedProposal: UITextDropProposal; cdecl;
  end;

  UIContentSizeCategoryAdjusting = interface(IObjectiveC)
    ['{04460536-6E77-4D85-B7B3-5D794DE3B41B}']
    function adjustsFontForContentSizeCategory: Boolean; cdecl;
    procedure setAdjustsFontForContentSizeCategory(adjustsFontForContentSizeCategory: Boolean); cdecl;
  end;

  UISceneClass = interface(UIResponderClass)
    ['{92D8AB19-0384-4D5E-BCA4-35D983FAA303}']
    {class} function new: Pointer; cdecl;
  end;

  UIScene = interface(UIResponder)
    ['{B30AC065-79DA-478F-89BA-307396102BEC}']
    function activationConditions: UISceneActivationConditions; cdecl;
    function activationState: UISceneActivationState; cdecl;
    function delegate: Pointer; cdecl;
    function initWithSession(session: UISceneSession; connectionOptions: UISceneConnectionOptions): Pointer; cdecl;
    procedure openURL(url: NSURL; options: UISceneOpenExternalURLOptions; completionHandler: TUISceneBlockMethod1); cdecl;
    function pointerLockState: UIPointerLockState; cdecl;
    function session: UISceneSession; cdecl;
    procedure setActivationConditions(activationConditions: UISceneActivationConditions); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TUIScene = class(TOCGenericImport<UISceneClass, UIScene>) end;

  UISceneDelegate = interface(IObjectiveC)
    ['{0D3433AF-19A7-4884-848F-E999787C6B96}']
    [MethodName('scene:continueUserActivity:')]
    procedure sceneContinueUserActivity(scene: UIScene; continueUserActivity: NSUserActivity); cdecl;
    procedure sceneDidBecomeActive(scene: UIScene); cdecl;
    procedure sceneDidDisconnect(scene: UIScene); cdecl;
    procedure sceneDidEnterBackground(scene: UIScene); cdecl;
    [MethodName('scene:didFailToContinueUserActivityWithType:error:')]
    procedure sceneDidFailToContinueUserActivityWithType(scene: UIScene; didFailToContinueUserActivityWithType: NSString; error: NSError); cdecl;
    [MethodName('scene:didUpdateUserActivity:')]
    procedure sceneDidUpdateUserActivity(scene: UIScene; didUpdateUserActivity: NSUserActivity); cdecl;
    [MethodName('scene:openURLContexts:')]
    procedure sceneOpenURLContexts(scene: UIScene; openURLContexts: NSSet); cdecl;
    [MethodName('scene:willConnectToSession:options:')]
    procedure sceneWillConnectToSession(scene: UIScene; willConnectToSession: UISceneSession; options: UISceneConnectionOptions); cdecl;
    [MethodName('scene:willContinueUserActivityWithType:')]
    procedure sceneWillContinueUserActivityWithType(scene: UIScene; willContinueUserActivityWithType: NSString); cdecl;
    procedure sceneWillEnterForeground(scene: UIScene); cdecl;
    procedure sceneWillResignActive(scene: UIScene); cdecl;
    function stateRestorationActivityForScene(scene: UIScene): NSUserActivity; cdecl;
  end;

  UIPointerLockStateClass = interface(NSObjectClass)
    ['{8B9AB950-5987-4924-AE92-A340F06757CF}']
    {class} function new: Pointer; cdecl;
  end;

  UIPointerLockState = interface(NSObject)
    ['{A3C30BDC-088E-4863-8D0A-844985EE68FC}']
    function isLocked: Boolean; cdecl;
  end;
  TUIPointerLockState = class(TOCGenericImport<UIPointerLockStateClass, UIPointerLockState>) end;

  UIViewControllerPreviewing = interface(IObjectiveC)
    ['{BA1A0231-29B1-42E6-B814-81016749387A}']
    function delegate: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
    function previewingGestureRecognizerForFailureRelationship: UIGestureRecognizer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
    procedure setSourceRect(sourceRect: CGRect); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
    function sourceRect: CGRect; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
    function sourceView: UIView; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
  end;

  UIViewControllerPreviewingDelegate = interface(IObjectiveC)
    ['{C9C3CB9E-BE3A-472D-ACD4-C39F2795F7FA}']
    function previewingContext(previewingContext: Pointer; viewControllerForLocation: CGPoint): UIViewController; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
    procedure previewingContext(previewingContext: Pointer; commitViewController: UIViewController); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("UIContextMenuInteraction", ios(9.0, 13.0))
  end;

  UIPreviewActionItem = interface(IObjectiveC)
    ['{79F588DC-1959-475D-AB60-18123C958509}']
    function title: NSString; cdecl;
  end;

  UIPreviewActionClass = interface(NSObjectClass)
    ['{66AC6FD2-24F6-4715-A8B8-133E9029A878}']
    {class} function actionWithTitle(title: NSString; style: UIPreviewActionStyle; handler: TUIPreviewActionBlockMethod1): Pointer; cdecl;
  end;

  UIPreviewAction = interface(NSObject)
    ['{C32FF5B1-9BBB-42F1-B03F-429D4FC48AA8}']
    function handler: TUIPreviewActionBlockMethod2; cdecl;
  end;
  TUIPreviewAction = class(TOCGenericImport<UIPreviewActionClass, UIPreviewAction>) end;

  UIPreviewActionGroupClass = interface(NSObjectClass)
    ['{956CAF8A-46FE-41A6-8409-8072BFC3A0DB}']
    {class} function actionGroupWithTitle(title: NSString; style: UIPreviewActionStyle; actions: NSArray): Pointer; cdecl;
  end;

  UIPreviewActionGroup = interface(NSObject)
    ['{E602BBF7-A0DB-4801-AEC1-583A19319D2A}']
  end;
  TUIPreviewActionGroup = class(TOCGenericImport<UIPreviewActionGroupClass, UIPreviewActionGroup>) end;

  UITimingCurveProvider = interface(IObjectiveC)
    ['{BEA68AE0-0EEF-4931-B9C2-2E3B828AE5C6}']
    function cubicTimingParameters: UICubicTimingParameters; cdecl;
    function springTimingParameters: UISpringTimingParameters; cdecl;
    function timingCurveType: UITimingCurveType; cdecl;
  end;

  UICubicTimingParametersClass = interface(NSObjectClass)
    ['{E76BAE80-7488-4804-A830-49D81FA3B0C1}']
  end;

  UICubicTimingParameters = interface(NSObject)
    ['{2CA0DC8B-91D0-49B8-81FA-1D4F21AAFF5C}']
    function animationCurve: UIViewAnimationCurve; cdecl;
    function controlPoint1: CGPoint; cdecl;
    function controlPoint2: CGPoint; cdecl;
    function initWithAnimationCurve(curve: UIViewAnimationCurve): Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithControlPoint1(point1: CGPoint; controlPoint2: CGPoint): Pointer; cdecl;
  end;
  TUICubicTimingParameters = class(TOCGenericImport<UICubicTimingParametersClass, UICubicTimingParameters>) end;

  UISpringTimingParametersClass = interface(NSObjectClass)
    ['{A53A9805-A66C-49E1-99F5-3CDA583CB420}']
  end;

  UISpringTimingParameters = interface(NSObject)
    ['{8ABB0CC8-DC79-45F2-B6FC-7E0244502AD5}']
    function initialVelocity: CGVector; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithDampingRatio(ratio: CGFloat): Pointer; overload; cdecl;
    function initWithDampingRatio(ratio: CGFloat; initialVelocity: CGVector): Pointer; overload; cdecl;
    function initWithMass(mass: CGFloat; stiffness: CGFloat; damping: CGFloat; initialVelocity: CGVector): Pointer; cdecl;
  end;
  TUISpringTimingParameters = class(TOCGenericImport<UISpringTimingParametersClass, UISpringTimingParameters>) end;

  UIDocumentBrowserViewControllerClass = interface(UIViewControllerClass)
    ['{3F8A26AC-EE26-46E8-B884-F1C09E51116D}']
  end;

  UIDocumentBrowserViewController = interface(UIViewController)
    ['{04C6A717-42DA-4BFB-96EF-D2259B49AF4F}']
    function additionalLeadingNavigationBarButtonItems: NSArray; cdecl;
    function additionalTrailingNavigationBarButtonItems: NSArray; cdecl;
    function allowedContentTypes: NSArray; cdecl; // API_DEPRECATED("allowedContentTypes is no longer supported", ios(11.0, 14.0))
    function allowsDocumentCreation: Boolean; cdecl;
    function allowsPickingMultipleItems: Boolean; cdecl;
    function browserUserInterfaceStyle: UIDocumentBrowserUserInterfaceStyle; cdecl;
    function contentTypesForRecentDocuments: NSArray; cdecl;
    function customActions: NSArray; cdecl;
    function defaultDocumentAspectRatio: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    procedure importDocumentAtURL(documentURL: NSURL; nextToDocumentAtURL: NSURL; mode: UIDocumentBrowserImportMode; completionHandler: TUIDocumentBrowserViewControllerBlockMethod2); cdecl;
    function initForOpeningContentTypes(contentTypes: NSArray): Pointer; cdecl;
    function initForOpeningFilesWithContentTypes(allowedContentTypes: NSArray): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("use initForOpeningContentTypes: instead", ios(11.0,14.0))
    function initWithNibName(nibName: NSString; bundle: NSBundle): Pointer; cdecl;
    function localizedCreateDocumentActionTitle: NSString; cdecl;
    function recentDocumentsContentTypes: NSArray; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("use contentTypesForRecentDocuments instead", ios(11.0,14.0))
    procedure revealDocumentAtURL(url: NSURL; importIfNeeded: Boolean; completion: TUIDocumentBrowserViewControllerBlockMethod1); cdecl;
    procedure setAdditionalLeadingNavigationBarButtonItems(additionalLeadingNavigationBarButtonItems: NSArray); cdecl;
    procedure setAdditionalTrailingNavigationBarButtonItems(additionalTrailingNavigationBarButtonItems: NSArray); cdecl;
    procedure setAllowsDocumentCreation(allowsDocumentCreation: Boolean); cdecl;
    procedure setAllowsPickingMultipleItems(allowsPickingMultipleItems: Boolean); cdecl;
    procedure setBrowserUserInterfaceStyle(browserUserInterfaceStyle: UIDocumentBrowserUserInterfaceStyle); cdecl;
    procedure setCustomActions(customActions: NSArray); cdecl;
    procedure setDefaultDocumentAspectRatio(defaultDocumentAspectRatio: CGFloat); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setLocalizedCreateDocumentActionTitle(localizedCreateDocumentActionTitle: NSString); cdecl;
    procedure setShouldShowFileExtensions(shouldShowFileExtensions: Boolean); cdecl;
    function shouldShowFileExtensions: Boolean; cdecl;
    function transitionControllerForDocumentAtURL(documentURL: NSURL): UIDocumentBrowserTransitionController; cdecl;
    function transitionControllerForDocumentURL(documentURL: NSURL): UIDocumentBrowserTransitionController; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("transitionControllerForDocumentAtURL:", ios(11.0,12.0))
  end;
  TUIDocumentBrowserViewController = class(TOCGenericImport<UIDocumentBrowserViewControllerClass, UIDocumentBrowserViewController>) end;

  UIDocumentBrowserViewControllerDelegate = interface(IObjectiveC)
    ['{E112E8CB-13C8-474F-AE7D-DC9E129BE4A8}']
    [MethodName('documentBrowser:applicationActivitiesForDocumentURLs:')]
    function documentBrowserApplicationActivitiesForDocumentURLs(controller: UIDocumentBrowserViewController; applicationActivitiesForDocumentURLs: NSArray): NSArray; cdecl;
    [MethodName('documentBrowser:didImportDocumentAtURL:toDestinationURL:')]
    procedure documentBrowserDidImportDocumentAtURL(controller: UIDocumentBrowserViewController; didImportDocumentAtURL: NSURL; toDestinationURL: NSURL); cdecl;
    [MethodName('documentBrowser:didPickDocumentsAtURLs:')]
    procedure documentBrowserDidPickDocumentsAtURLs(controller: UIDocumentBrowserViewController; didPickDocumentsAtURLs: NSArray); cdecl;
    [MethodName('documentBrowser:didPickDocumentURLs:')]
    procedure documentBrowserDidPickDocumentURLs(controller: UIDocumentBrowserViewController; didPickDocumentURLs: NSArray); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("documentBrowser:didPickDocumentsAtURLs:", ios(11.0, 12.0))
    [MethodName('documentBrowser:didRequestDocumentCreationWithHandler:')]
    procedure documentBrowserDidRequestDocumentCreationWithHandler(controller: UIDocumentBrowserViewController; didRequestDocumentCreationWithHandler: TUIDocumentBrowserViewControllerDelegateBlockMethod1); cdecl;
    [MethodName('documentBrowser:failedToImportDocumentAtURL:error:')]
    procedure documentBrowserFailedToImportDocumentAtURL(controller: UIDocumentBrowserViewController; failedToImportDocumentAtURL: NSURL; error: NSError); cdecl;
    [MethodName('documentBrowser:willPresentActivityViewController:')]
    procedure documentBrowserWillPresentActivityViewController(controller: UIDocumentBrowserViewController; willPresentActivityViewController: UIActivityViewController); cdecl;
  end;

  UIDocumentBrowserTransitionControllerClass = interface(NSObjectClass)
    ['{5B913FC1-150E-4C62-8859-94504BB9050A}']
  end;

  UIDocumentBrowserTransitionController = interface(NSObject)
    ['{DBCC9A48-B892-4C3B-A03D-4811383DFFE9}']
    function loadingProgress: NSProgress; cdecl;
    procedure setLoadingProgress(loadingProgress: NSProgress); cdecl;
    procedure setTargetView(targetView: UIView); cdecl;
    function targetView: UIView; cdecl;
  end;
  TUIDocumentBrowserTransitionController = class(TOCGenericImport<UIDocumentBrowserTransitionControllerClass, UIDocumentBrowserTransitionController>) end;

  UIDocumentBrowserActionClass = interface(NSObjectClass)
    ['{21C4A8D0-69CF-4B56-9097-142249D107C2}']
  end;

  UIDocumentBrowserAction = interface(NSObject)
    ['{7F5A4C7E-468C-40CE-9830-C00019629D18}']
    function availability: UIDocumentBrowserActionAvailability; cdecl;
    function identifier: NSString; cdecl;
    function image: UIImage; cdecl;
    function initWithIdentifier(identifier: NSString; localizedTitle: NSString; availability: UIDocumentBrowserActionAvailability; handler: TUIDocumentBrowserActionBlockMethod1): Pointer; cdecl;
    function localizedTitle: NSString; cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setSupportedContentTypes(supportedContentTypes: NSArray); cdecl;
    procedure setSupportsMultipleItems(supportsMultipleItems: Boolean); cdecl;
    function supportedContentTypes: NSArray; cdecl;
    function supportsMultipleItems: Boolean; cdecl;
  end;
  TUIDocumentBrowserAction = class(TOCGenericImport<UIDocumentBrowserActionClass, UIDocumentBrowserAction>) end;

  NSDataAssetClass = interface(NSObjectClass)
    ['{21674210-5BC3-4A14-9BE7-5B45A22DF69D}']
  end;

  NSDataAsset = interface(NSObject)
    ['{8641E40A-4627-41D6-876D-5007FA27976A}']
    function data: NSData; cdecl;
    function initWithName(name: NSDataAssetName; bundle: NSBundle): Pointer; overload; cdecl;
    function initWithName(name: NSDataAssetName): Pointer; overload; cdecl;
    function name: NSDataAssetName; cdecl;
    function typeIdentifier: NSString; cdecl;
  end;
  TNSDataAsset = class(TOCGenericImport<NSDataAssetClass, NSDataAsset>) end;

  UIItemProviderPresentationSizeProviding = interface(IObjectiveC)
    ['{2AD58580-80C1-4469-9274-B94EA6302C99}']
    function preferredPresentationSizeForItemProvider: CGSize; cdecl;
  end;

  NSLayoutAnchorClass = interface(NSObjectClass)
    ['{15597D3B-69BF-44FF-9C38-CD2A666A5BB0}']
  end;

  NSLayoutAnchor = interface(NSObject)
    ['{1102605C-0D8D-457D-8639-62AF1ECF35D6}']
    function constraintEqualToAnchor(anchor: NSLayoutAnchor): NSLayoutConstraint; overload; cdecl;
    function constraintEqualToAnchor(anchor: NSLayoutAnchor; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintGreaterThanOrEqualToAnchor(anchor: NSLayoutAnchor; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintGreaterThanOrEqualToAnchor(anchor: NSLayoutAnchor): NSLayoutConstraint; overload; cdecl;
    function constraintLessThanOrEqualToAnchor(anchor: NSLayoutAnchor; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintLessThanOrEqualToAnchor(anchor: NSLayoutAnchor): NSLayoutConstraint; overload; cdecl;
    function constraintsAffectingLayout: NSArray; cdecl;
    function hasAmbiguousLayout: Boolean; cdecl;
    function item: Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TNSLayoutAnchor = class(TOCGenericImport<NSLayoutAnchorClass, NSLayoutAnchor>) end;

  NSLayoutXAxisAnchorClass = interface(NSLayoutAnchorClass)
    ['{2505A4F9-8177-4D06-BA60-E23B8B8AE4F5}']
  end;

  NSLayoutXAxisAnchor = interface(NSLayoutAnchor)
    ['{F72B73E1-7AA2-40B2-9EDE-D2E50D029CAE}']
    function anchorWithOffsetToAnchor(otherAnchor: NSLayoutXAxisAnchor): NSLayoutDimension; cdecl;
    function constraintEqualToSystemSpacingAfterAnchor(anchor: NSLayoutXAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
    function constraintGreaterThanOrEqualToSystemSpacingAfterAnchor(anchor: NSLayoutXAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
    function constraintLessThanOrEqualToSystemSpacingAfterAnchor(anchor: NSLayoutXAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
  end;
  TNSLayoutXAxisAnchor = class(TOCGenericImport<NSLayoutXAxisAnchorClass, NSLayoutXAxisAnchor>) end;

  NSLayoutYAxisAnchorClass = interface(NSLayoutAnchorClass)
    ['{D155707F-6E87-400D-9B95-10EBE696DB40}']
  end;

  NSLayoutYAxisAnchor = interface(NSLayoutAnchor)
    ['{AF2388F2-8045-4BB8-9B8D-135F3DCB6DF0}']
    function anchorWithOffsetToAnchor(otherAnchor: NSLayoutYAxisAnchor): NSLayoutDimension; cdecl;
    function constraintEqualToSystemSpacingBelowAnchor(anchor: NSLayoutYAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
    function constraintGreaterThanOrEqualToSystemSpacingBelowAnchor(anchor: NSLayoutYAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
    function constraintLessThanOrEqualToSystemSpacingBelowAnchor(anchor: NSLayoutYAxisAnchor; multiplier: CGFloat): NSLayoutConstraint; cdecl;
  end;
  TNSLayoutYAxisAnchor = class(TOCGenericImport<NSLayoutYAxisAnchorClass, NSLayoutYAxisAnchor>) end;

  NSLayoutDimensionClass = interface(NSLayoutAnchorClass)
    ['{94D4D378-8589-4967-9C36-F57239113B5F}']
  end;

  NSLayoutDimension = interface(NSLayoutAnchor)
    ['{936E2797-1AE9-4D05-90F5-A10FB3B52C87}']
    function constraintEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintEqualToConstant(c: CGFloat): NSLayoutConstraint; cdecl;
    function constraintGreaterThanOrEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintGreaterThanOrEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintGreaterThanOrEqualToConstant(c: CGFloat): NSLayoutConstraint; cdecl;
    function constraintLessThanOrEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat; constant: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintLessThanOrEqualToAnchor(anchor: NSLayoutDimension; multiplier: CGFloat): NSLayoutConstraint; overload; cdecl;
    function constraintLessThanOrEqualToConstant(c: CGFloat): NSLayoutConstraint; cdecl;
  end;
  TNSLayoutDimension = class(TOCGenericImport<NSLayoutDimensionClass, NSLayoutDimension>) end;

  UIAccessibilityContainerDataTableCell = interface(IObjectiveC)
    ['{B98AA51B-0690-4C65-85CA-98A6EB908216}']
    function accessibilityColumnRange: NSRange; cdecl;
    function accessibilityRowRange: NSRange; cdecl;
  end;

  UIAccessibilityContainerDataTable = interface(IObjectiveC)
    ['{99CD1D4E-11B9-4B55-BE38-BEAED993D511}']
    function accessibilityColumnCount: NSUInteger; cdecl;
    function accessibilityDataTableCellElementForRow(row: NSUInteger; column: NSUInteger): Pointer; cdecl;
    function accessibilityHeaderElementsForColumn(column: NSUInteger): NSArray; cdecl;
    function accessibilityHeaderElementsForRow(row: NSUInteger): NSArray; cdecl;
    function accessibilityRowCount: NSUInteger; cdecl;
  end;

  UIAccessibilityCustomActionClass = interface(NSObjectClass)
    ['{2FFA0796-D4C7-4F19-A43F-EB2735AA9F94}']
  end;

  UIAccessibilityCustomAction = interface(NSObject)
    ['{C3AA8AFB-37BD-4BFF-8DB0-03E7A422B703}']
    function actionHandler: UIAccessibilityCustomActionHandler; cdecl;
    function attributedName: NSAttributedString; cdecl;
    function image: UIImage; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; image: UIImage; actionHandler: UIAccessibilityCustomActionHandler): Pointer; overload; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; actionHandler: UIAccessibilityCustomActionHandler): Pointer; overload; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; image: UIImage; target: Pointer; selector: Pointer): Pointer; overload; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; target: Pointer; selector: Pointer): Pointer; overload; cdecl;
    function initWithName(name: NSString; target: Pointer; selector: Pointer): Pointer; overload; cdecl;
    function initWithName(name: NSString; image: UIImage; target: Pointer; selector: Pointer): Pointer; overload; cdecl;
    function initWithName(name: NSString; actionHandler: UIAccessibilityCustomActionHandler): Pointer; overload; cdecl;
    function initWithName(name: NSString; image: UIImage; actionHandler: UIAccessibilityCustomActionHandler): Pointer; overload; cdecl;
    function name: NSString; cdecl;
    function selector: Pointer; cdecl;
    procedure setActionHandler(actionHandler: UIAccessibilityCustomActionHandler); cdecl;
    procedure setAttributedName(attributedName: NSAttributedString); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setSelector(selector: Pointer); cdecl;
    procedure setTarget(target: Pointer); cdecl;
    function target: Pointer; cdecl;
  end;
  TUIAccessibilityCustomAction = class(TOCGenericImport<UIAccessibilityCustomActionClass, UIAccessibilityCustomAction>) end;

  UIAccessibilityCustomRotorSearchPredicateClass = interface(NSObjectClass)
    ['{CC5A4A0F-961C-4B3D-94C3-B5E22046EA1F}']
  end;

  UIAccessibilityCustomRotorSearchPredicate = interface(NSObject)
    ['{6290980F-1456-4705-BE0E-C481BD2DDAF2}']
    function currentItem: UIAccessibilityCustomRotorItemResult; cdecl;
    function searchDirection: UIAccessibilityCustomRotorDirection; cdecl;
    procedure setCurrentItem(currentItem: UIAccessibilityCustomRotorItemResult); cdecl;
    procedure setSearchDirection(searchDirection: UIAccessibilityCustomRotorDirection); cdecl;
  end;
  TUIAccessibilityCustomRotorSearchPredicate = class(TOCGenericImport<UIAccessibilityCustomRotorSearchPredicateClass, UIAccessibilityCustomRotorSearchPredicate>) end;

  UIAccessibilityCustomRotorClass = interface(NSObjectClass)
    ['{6963F160-FF7B-4031-8B8E-8574A62854B2}']
  end;

  UIAccessibilityCustomRotor = interface(NSObject)
    ['{719C0EB1-BA24-44AF-BF16-86705D638111}']
    function attributedName: NSAttributedString; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; itemSearchBlock: UIAccessibilityCustomRotorSearch): Pointer; cdecl;
    function initWithName(name: NSString; itemSearchBlock: UIAccessibilityCustomRotorSearch): Pointer; cdecl;
    function initWithSystemType(&type: UIAccessibilityCustomSystemRotorType; itemSearchBlock: UIAccessibilityCustomRotorSearch): Pointer; cdecl;
    function itemSearchBlock: UIAccessibilityCustomRotorSearch; cdecl;
    function name: NSString; cdecl;
    procedure setAttributedName(attributedName: NSAttributedString); cdecl;
    procedure setItemSearchBlock(itemSearchBlock: UIAccessibilityCustomRotorSearch); cdecl;
    procedure setName(name: NSString); cdecl;
    function systemRotorType: UIAccessibilityCustomSystemRotorType; cdecl;
  end;
  TUIAccessibilityCustomRotor = class(TOCGenericImport<UIAccessibilityCustomRotorClass, UIAccessibilityCustomRotor>) end;

  UIAccessibilityCustomRotorItemResultClass = interface(NSObjectClass)
    ['{0963EAAF-FDC0-48DA-92C2-260E3C75C21D}']
  end;

  UIAccessibilityCustomRotorItemResult = interface(NSObject)
    ['{1CEA9D6E-9F59-49C9-A440-59DB713A0BBA}']
    function initWithTargetElement(targetElement: Pointer; targetRange: UITextRange): Pointer; cdecl;
    procedure setTargetElement(targetElement: Pointer); cdecl;
    procedure setTargetRange(targetRange: UITextRange); cdecl;
    function targetElement: Pointer; cdecl;
    function targetRange: UITextRange; cdecl;
  end;
  TUIAccessibilityCustomRotorItemResult = class(TOCGenericImport<UIAccessibilityCustomRotorItemResultClass, UIAccessibilityCustomRotorItemResult>) end;

  UISpringLoadedInteractionSupporting = interface(IObjectiveC)
    ['{B8EBA02E-C5DD-44D9-9391-1C992F80E9C4}']
    function isSpringLoaded: Boolean; cdecl;
    procedure setSpringLoaded(springLoaded: Boolean); cdecl;
  end;

  UIAccessibilityLocationDescriptorClass = interface(NSObjectClass)
    ['{86BDC669-31B2-41B4-8A6E-FCC6479C0EF8}']
    {class} function new: Pointer; cdecl;
  end;

  UIAccessibilityLocationDescriptor = interface(NSObject)
    ['{04A0A295-9165-49D9-8BB1-30A00F813E3E}']
    function attributedName: NSAttributedString; cdecl;
    function initWithAttributedName(attributedName: NSAttributedString; point: CGPoint; inView: UIView): Pointer; cdecl;
    function initWithName(name: NSString; view: UIView): Pointer; overload; cdecl;
    function initWithName(name: NSString; point: CGPoint; inView: UIView): Pointer; overload; cdecl;
    function name: NSString; cdecl;
    function point: CGPoint; cdecl;
    function view: UIView; cdecl;
  end;
  TUIAccessibilityLocationDescriptor = class(TOCGenericImport<UIAccessibilityLocationDescriptorClass, UIAccessibilityLocationDescriptor>) end;

  UIAccessibilityContentSizeCategoryImageAdjusting = interface(IObjectiveC)
    ['{4BF86AC0-42C4-46E9-9E6E-28936EDF6236}']
    function adjustsImageSizeForAccessibilityContentSizeCategory: Boolean; cdecl;
    procedure setAdjustsImageSizeForAccessibilityContentSizeCategory(adjustsImageSizeForAccessibilityContentSizeCategory: Boolean); cdecl;
  end;

  UIActivityItemsConfigurationReading = interface(IObjectiveC)
    ['{A4464FAF-7649-4F33-A80B-F622EDF397A6}']
    function activityItemsConfigurationMetadataForItemAtIndex(index: NSInteger; key: UIActivityItemsConfigurationMetadataKey): Pointer; cdecl;
    function activityItemsConfigurationMetadataForKey(key: UIActivityItemsConfigurationMetadataKey): Pointer; cdecl;
    // Foundation function activityItemsConfigurationPreviewForItemAtIndex(index: NSInteger; intent: UIActivityItemsConfigurationPreviewIntent; suggestedSize: CGSize): NSItemProvider; cdecl;
    function activityItemsConfigurationSupportsInteraction(interaction: UIActivityItemsConfigurationInteraction): Boolean; cdecl;
    function applicationActivitiesForActivityItemsConfiguration: NSArray; cdecl;
    function itemProvidersForActivityItemsConfiguration: NSArray; cdecl;
  end;

  UIActivityItemsConfigurationClass = interface(NSObjectClass)
    ['{C8CD8220-493E-4C91-BDED-90A47FF0DA72}']
    {class} function activityItemsConfigurationWithItemProviders(itemProviders: NSArray): Pointer; cdecl;
    {class} function activityItemsConfigurationWithObjects(objects: NSArray): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIActivityItemsConfiguration = interface(NSObject)
    ['{2F010CB2-B75E-4445-82E7-75D87CB2185C}']
    function applicationActivitiesProvider: TUIActivityItemsConfigurationBlockMethod5; cdecl;
    function initWithItemProviders(itemProviders: NSArray): Pointer; cdecl;
    function initWithObjects(objects: NSArray): Pointer; cdecl;
    function localObject: Pointer; cdecl;
    function metadataProvider: TUIActivityItemsConfigurationBlockMethod1; cdecl;
    function perItemMetadataProvider: TUIActivityItemsConfigurationBlockMethod3; cdecl;
    // Foundation function previewProvider: TUIActivityItemsConfigurationBlockMethod4; cdecl;
    procedure setApplicationActivitiesProvider(applicationActivitiesProvider: TUIActivityItemsConfigurationBlockMethod6); cdecl;
    procedure setLocalObject(localObject: Pointer); cdecl;
    procedure setMetadataProvider(metadataProvider: TUIActivityItemsConfigurationBlockMethod2); cdecl;
    procedure setPerItemMetadataProvider(perItemMetadataProvider: TUIActivityItemsConfigurationBlockMethod2); cdecl;
    procedure setPreviewProvider(previewProvider: TUIActivityItemsConfigurationBlockMethod2); cdecl;
    procedure setSupportedInteractions(supportedInteractions: NSArray); cdecl;
    function supportedInteractions: NSArray; cdecl;
  end;
  TUIActivityItemsConfiguration = class(TOCGenericImport<UIActivityItemsConfigurationClass, UIActivityItemsConfiguration>) end;

  UIApplicationShortcutIconClass = interface(NSObjectClass)
    ['{5D8D9C15-10A2-48BE-A5DE-96DD1477C009}']
    {class} function iconWithSystemImageName(systemImageName: NSString): Pointer; cdecl;
    {class} function iconWithTemplateImageName(templateImageName: NSString): Pointer; cdecl;
    {class} function iconWithType(&type: UIApplicationShortcutIconType): Pointer; cdecl;
  end;

  UIApplicationShortcutIcon = interface(NSObject)
    ['{9230DA3F-3601-4CBF-999F-ABA8657C799A}']
  end;
  TUIApplicationShortcutIcon = class(TOCGenericImport<UIApplicationShortcutIconClass, UIApplicationShortcutIcon>) end;

  UIApplicationShortcutItemClass = interface(NSObjectClass)
    ['{ED5F7B42-E1CF-4708-88D2-4A829E2B18FD}']
  end;

  UIApplicationShortcutItem = interface(NSObject)
    ['{1C4D4599-39AD-4F27-BCED-F441A55218A7}']
    [MethodName('type')]
    function &type: NSString; cdecl;
    function icon: UIApplicationShortcutIcon; cdecl;
    function initWithType(&type: NSString; localizedTitle: NSString; localizedSubtitle: NSString; icon: UIApplicationShortcutIcon; userInfo: NSDictionary): Pointer; overload; cdecl;
    function initWithType(&type: NSString; localizedTitle: NSString): Pointer; overload; cdecl;
    function localizedSubtitle: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    function targetContentIdentifier: Pointer; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUIApplicationShortcutItem = class(TOCGenericImport<UIApplicationShortcutItemClass, UIApplicationShortcutItem>) end;

  UIMutableApplicationShortcutItemClass = interface(UIApplicationShortcutItemClass)
    ['{DC731B7D-FCA7-406E-A7DD-D3DE609FA5EB}']
  end;

  UIMutableApplicationShortcutItem = interface(UIApplicationShortcutItem)
    ['{21AD0096-DA42-43E0-9733-D842BAFE5AFA}']
    [MethodName('type')]
    function &type: NSString; cdecl;
    function icon: UIApplicationShortcutIcon; cdecl;
    function localizedSubtitle: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    procedure setIcon(icon: UIApplicationShortcutIcon); cdecl;
    procedure setLocalizedSubtitle(localizedSubtitle: NSString); cdecl;
    procedure setLocalizedTitle(localizedTitle: NSString); cdecl;
    procedure setTargetContentIdentifier(targetContentIdentifier: Pointer); cdecl;
    procedure setType(&type: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function targetContentIdentifier: Pointer; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUIMutableApplicationShortcutItem = class(TOCGenericImport<UIMutableApplicationShortcutItemClass, UIMutableApplicationShortcutItem>) end;

  UIBackgroundConfigurationClass = interface(NSObjectClass)
    ['{6C5C6241-ABD7-4F41-9B8B-1C65C75AF560}']
    {class} function clearConfiguration: Pointer; cdecl;
    {class} function listAccompaniedSidebarCellConfiguration: Pointer; cdecl;
    {class} function listGroupedCellConfiguration: Pointer; cdecl;
    {class} function listGroupedHeaderFooterConfiguration: Pointer; cdecl;
    {class} function listPlainCellConfiguration: Pointer; cdecl;
    {class} function listPlainHeaderFooterConfiguration: Pointer; cdecl;
    {class} function listSidebarCellConfiguration: Pointer; cdecl;
    {class} function listSidebarHeaderConfiguration: Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIBackgroundConfiguration = interface(NSObject)
    ['{35939B49-FE98-4A03-9F92-02F5501C5329}']
    function backgroundColor: UIColor; cdecl;
    function backgroundColorTransformer: UIConfigurationColorTransformer; cdecl;
    function backgroundInsets: NSDirectionalEdgeInsets; cdecl;
    function cornerRadius: CGFloat; cdecl;
    function customView: UIView; cdecl;
    function edgesAddingLayoutMarginsToBackgroundInsets: NSDirectionalRectEdge; cdecl;
    function resolvedBackgroundColorForTintColor(tintColor: UIColor): UIColor; cdecl;
    function resolvedStrokeColorForTintColor(tintColor: UIColor): UIColor; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setBackgroundColorTransformer(backgroundColorTransformer: UIConfigurationColorTransformer); cdecl;
    procedure setBackgroundInsets(backgroundInsets: NSDirectionalEdgeInsets); cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setCustomView(customView: UIView); cdecl;
    procedure setEdgesAddingLayoutMarginsToBackgroundInsets(edgesAddingLayoutMarginsToBackgroundInsets: NSDirectionalRectEdge); cdecl;
    procedure setStrokeColor(strokeColor: UIColor); cdecl;
    procedure setStrokeColorTransformer(strokeColorTransformer: UIConfigurationColorTransformer); cdecl;
    procedure setStrokeOutset(strokeOutset: CGFloat); cdecl;
    procedure setStrokeWidth(strokeWidth: CGFloat); cdecl;
    procedure setVisualEffect(visualEffect: UIVisualEffect); cdecl;
    function strokeColor: UIColor; cdecl;
    function strokeColorTransformer: UIConfigurationColorTransformer; cdecl;
    function strokeOutset: CGFloat; cdecl;
    function strokeWidth: CGFloat; cdecl;
    function updatedConfigurationForState(state: Pointer): Pointer; cdecl;
    function visualEffect: UIVisualEffect; cdecl;
  end;
  TUIBackgroundConfiguration = class(TOCGenericImport<UIBackgroundConfigurationClass, UIBackgroundConfiguration>) end;

  UIBarAppearanceClass = interface(NSObjectClass)
    ['{6024C1EA-0CF4-41A7-A3D6-FA10E9D9B11B}']
  end;

  UIBarAppearance = interface(NSObject)
    ['{1D405815-22B7-4F91-A26D-A8B50A4972CB}']
    function backgroundColor: UIColor; cdecl;
    function backgroundEffect: UIBlurEffect; cdecl;
    function backgroundImage: UIImage; cdecl;
    function backgroundImageContentMode: UIViewContentMode; cdecl;
    procedure configureWithDefaultBackground; cdecl;
    procedure configureWithOpaqueBackground; cdecl;
    procedure configureWithTransparentBackground; cdecl;
    function copy: Pointer; cdecl;
    function idiom: UIUserInterfaceIdiom; cdecl;
    function initWithBarAppearance(barAppearance: UIBarAppearance): Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithIdiom(idiom: UIUserInterfaceIdiom): Pointer; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setBackgroundEffect(backgroundEffect: UIBlurEffect); cdecl;
    procedure setBackgroundImage(backgroundImage: UIImage); cdecl;
    procedure setBackgroundImageContentMode(backgroundImageContentMode: UIViewContentMode); cdecl;
    procedure setShadowColor(shadowColor: UIColor); cdecl;
    procedure setShadowImage(shadowImage: UIImage); cdecl;
    function shadowColor: UIColor; cdecl;
    function shadowImage: UIImage; cdecl;
  end;
  TUIBarAppearance = class(TOCGenericImport<UIBarAppearanceClass, UIBarAppearance>) end;

  UIBarButtonItemStateAppearanceClass = interface(NSObjectClass)
    ['{3B434174-5B61-41DA-AC03-F354C7B16B6A}']
    {class} function new: Pointer; cdecl;
  end;

  UIBarButtonItemStateAppearance = interface(NSObject)
    ['{DD8D0678-DF67-4A0E-A999-1F5D8410C0A0}']
    function backgroundImage: UIImage; cdecl;
    function backgroundImagePositionAdjustment: UIOffset; cdecl;
    procedure setBackgroundImage(backgroundImage: UIImage); cdecl;
    procedure setBackgroundImagePositionAdjustment(backgroundImagePositionAdjustment: UIOffset); cdecl;
    procedure setTitlePositionAdjustment(titlePositionAdjustment: UIOffset); cdecl;
    procedure setTitleTextAttributes(titleTextAttributes: NSDictionary); cdecl;
    function titlePositionAdjustment: UIOffset; cdecl;
    function titleTextAttributes: NSDictionary; cdecl;
  end;
  TUIBarButtonItemStateAppearance = class(TOCGenericImport<UIBarButtonItemStateAppearanceClass, UIBarButtonItemStateAppearance>) end;

  UIBarButtonItemAppearanceClass = interface(NSObjectClass)
    ['{BC28A52E-85A3-4FB2-B113-609DCF656482}']
  end;

  UIBarButtonItemAppearance = interface(NSObject)
    ['{EE012815-4961-4244-A30F-8B7C132B34BC}']
    procedure configureWithDefaultForStyle(style: UIBarButtonItemStyle); cdecl;
    function copy: Pointer; cdecl;
    function disabled: UIBarButtonItemStateAppearance; cdecl;
    function focused: UIBarButtonItemStateAppearance; cdecl;
    function highlighted: UIBarButtonItemStateAppearance; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithStyle(style: UIBarButtonItemStyle): Pointer; cdecl;
    function normal: UIBarButtonItemStateAppearance; cdecl;
  end;
  TUIBarButtonItemAppearance = class(TOCGenericImport<UIBarButtonItemAppearanceClass, UIBarButtonItemAppearance>) end;

  UIBarButtonItemGroupClass = interface(NSObjectClass)
    ['{1D104900-8786-4DFC-963F-0BFBE9460607}']
  end;

  UIBarButtonItemGroup = interface(NSObject)
    ['{43504A31-42AF-406A-99AB-699BD27F436A}']
    function barButtonItems: NSArray; cdecl;
    function initWithBarButtonItems(barButtonItems: NSArray; representativeItem: UIBarButtonItem): Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function isDisplayingRepresentativeItem: Boolean; cdecl;
    function representativeItem: UIBarButtonItem; cdecl;
    procedure setBarButtonItems(barButtonItems: NSArray); cdecl;
    procedure setRepresentativeItem(representativeItem: UIBarButtonItem); cdecl;
  end;
  TUIBarButtonItemGroup = class(TOCGenericImport<UIBarButtonItemGroupClass, UIBarButtonItemGroup>) end;

  UIVisualEffectClass = interface(NSObjectClass)
    ['{A7BDEFA8-8578-4A98-99D5-54E4A7AA05AE}']
  end;

  UIVisualEffect = interface(NSObject)
    ['{899FADBB-1A1C-4AA4-8469-023C7DF4784E}']
  end;
  TUIVisualEffect = class(TOCGenericImport<UIVisualEffectClass, UIVisualEffect>) end;

  UIBlurEffectClass = interface(UIVisualEffectClass)
    ['{9F1F64CA-870E-4D21-881E-0BC34F1CC695}']
    {class} function effectWithStyle(style: UIBlurEffectStyle): UIBlurEffect; cdecl;
  end;

  UIBlurEffect = interface(UIVisualEffect)
    ['{C80B1689-DDCE-4ACB-BABF-5080ACC9DEDE}']
  end;
  TUIBlurEffect = class(TOCGenericImport<UIBlurEffectClass, UIBlurEffect>) end;

  UICellAccessoryClass = interface(NSObjectClass)
    ['{5ACB23AA-AA91-4820-B3F1-B9DFDD92FBDA}']
  end;

  UICellAccessory = interface(NSObject)
    ['{BB6A036F-1D7C-4DB6-8D30-CEE4470AA530}']
    function displayedState: UICellAccessoryDisplayedState; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function isHidden: Boolean; cdecl;
    function reservedLayoutWidth: CGFloat; cdecl;
    procedure setDisplayedState(displayedState: UICellAccessoryDisplayedState); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setReservedLayoutWidth(reservedLayoutWidth: CGFloat); cdecl;
    procedure setTintColor(tintColor: UIColor); cdecl;
    function tintColor: UIColor; cdecl;
  end;
  TUICellAccessory = class(TOCGenericImport<UICellAccessoryClass, UICellAccessory>) end;

  UICellAccessoryDisclosureIndicatorClass = interface(UICellAccessoryClass)
    ['{D649AE2C-A2E6-4A52-A23C-21F1572B488D}']
  end;

  UICellAccessoryDisclosureIndicator = interface(UICellAccessory)
    ['{895C9105-D1CB-4EC0-ACF0-471D25CABFBD}']
  end;
  TUICellAccessoryDisclosureIndicator = class(TOCGenericImport<UICellAccessoryDisclosureIndicatorClass, UICellAccessoryDisclosureIndicator>) end;

  UICellAccessoryCheckmarkClass = interface(UICellAccessoryClass)
    ['{BD09493C-E57C-43C6-BEA0-CA5C943E1F52}']
  end;

  UICellAccessoryCheckmark = interface(UICellAccessory)
    ['{BD5154CE-42EC-4CC4-A84B-48A039F15E44}']
  end;
  TUICellAccessoryCheckmark = class(TOCGenericImport<UICellAccessoryCheckmarkClass, UICellAccessoryCheckmark>) end;

  UICellAccessoryDeleteClass = interface(UICellAccessoryClass)
    ['{5D712D0C-AB05-4041-9EDE-8CF4941AD8B4}']
  end;

  UICellAccessoryDelete = interface(UICellAccessory)
    ['{99F711CD-041F-4D17-BFF5-6D4776E84C74}']
    function actionHandler: TUICellAccessoryDeleteBlockMethod1; cdecl;
    function backgroundColor: UIColor; cdecl;
    procedure setActionHandler(actionHandler: TUICellAccessoryDeleteBlockMethod1); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
  end;
  TUICellAccessoryDelete = class(TOCGenericImport<UICellAccessoryDeleteClass, UICellAccessoryDelete>) end;

  UICellAccessoryInsertClass = interface(UICellAccessoryClass)
    ['{F198371B-1001-47BD-B675-E12A1CF6651B}']
  end;

  UICellAccessoryInsert = interface(UICellAccessory)
    ['{E2E63F27-ADB8-4806-80D8-8DA7212E9455}']
    function actionHandler: TUICellAccessoryInsertBlockMethod1; cdecl;
    function backgroundColor: UIColor; cdecl;
    procedure setActionHandler(actionHandler: TUICellAccessoryInsertBlockMethod1); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
  end;
  TUICellAccessoryInsert = class(TOCGenericImport<UICellAccessoryInsertClass, UICellAccessoryInsert>) end;

  UICellAccessoryReorderClass = interface(UICellAccessoryClass)
    ['{2196C652-BD57-47A6-826D-4C6209803A28}']
  end;

  UICellAccessoryReorder = interface(UICellAccessory)
    ['{4D0EBF09-159C-41E2-8B64-411A9A9C3F6E}']
    procedure setShowsVerticalSeparator(showsVerticalSeparator: Boolean); cdecl;
    function showsVerticalSeparator: Boolean; cdecl;
  end;
  TUICellAccessoryReorder = class(TOCGenericImport<UICellAccessoryReorderClass, UICellAccessoryReorder>) end;

  UICellAccessoryMultiselectClass = interface(UICellAccessoryClass)
    ['{86B742C1-E3C6-47D2-897B-7D6A02598F59}']
  end;

  UICellAccessoryMultiselect = interface(UICellAccessory)
    ['{E53410C5-3B5C-4433-A9A4-35C02D3F2EBC}']
    function backgroundColor: UIColor; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
  end;
  TUICellAccessoryMultiselect = class(TOCGenericImport<UICellAccessoryMultiselectClass, UICellAccessoryMultiselect>) end;

  UICellAccessoryOutlineDisclosureClass = interface(UICellAccessoryClass)
    ['{49799D45-3F52-41A5-9F5E-96B96180FD34}']
  end;

  UICellAccessoryOutlineDisclosure = interface(UICellAccessory)
    ['{E7FAE490-0936-48C5-A2DA-E698C6FA4E96}']
    function actionHandler: TUICellAccessoryOutlineDisclosureBlockMethod1; cdecl;
    procedure setActionHandler(actionHandler: TUICellAccessoryOutlineDisclosureBlockMethod1); cdecl;
    procedure setStyle(style: UICellAccessoryOutlineDisclosureStyle); cdecl;
    function style: UICellAccessoryOutlineDisclosureStyle; cdecl;
  end;
  TUICellAccessoryOutlineDisclosure = class(TOCGenericImport<UICellAccessoryOutlineDisclosureClass, UICellAccessoryOutlineDisclosure>) end;

  UICellAccessoryLabelClass = interface(UICellAccessoryClass)
    ['{508592E3-85AD-4EB0-AC4C-3C8C8BC895EF}']
    {class} function new: Pointer; cdecl;
  end;

  UICellAccessoryLabel = interface(UICellAccessory)
    ['{D78C1B0E-439D-4532-B5A9-7A1EE10A9DB7}']
    function adjustsFontForContentSizeCategory: Boolean; cdecl;
    function font: UIFont; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithText(text: NSString): Pointer; cdecl;
    procedure setAdjustsFontForContentSizeCategory(adjustsFontForContentSizeCategory: Boolean); cdecl;
    procedure setFont(font: UIFont); cdecl;
    function text: NSString; cdecl;
  end;
  TUICellAccessoryLabel = class(TOCGenericImport<UICellAccessoryLabelClass, UICellAccessoryLabel>) end;

  UICellAccessoryCustomViewClass = interface(UICellAccessoryClass)
    ['{7AB8AE67-A250-430D-B20A-0384E907F9D6}']
    {class} function new: Pointer; cdecl;
  end;

  UICellAccessoryCustomView = interface(UICellAccessory)
    ['{34DCFF3B-4FB8-4855-A781-B15E8B6E47ED}']
    function customView: UIView; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithCustomView(customView: UIView; placement: UICellAccessoryPlacement): Pointer; cdecl;
    function maintainsFixedSize: Boolean; cdecl;
    function placement: UICellAccessoryPlacement; cdecl;
    function position: UICellAccessoryPosition; cdecl;
    procedure setMaintainsFixedSize(maintainsFixedSize: Boolean); cdecl;
    procedure setPosition(position: UICellAccessoryPosition); cdecl;
  end;
  TUICellAccessoryCustomView = class(TOCGenericImport<UICellAccessoryCustomViewClass, UICellAccessoryCustomView>) end;

  UIConfigurationState = interface(IObjectiveC)
    ['{0C33D6E3-7C67-41B0-9319-E74B57BDB0D7}']
    function customStateForKey(key: UIConfigurationStateCustomKey): Pointer; cdecl;
    function initWithTraitCollection(traitCollection: UITraitCollection): Pointer; cdecl;
    function objectForKeyedSubscript(key: UIConfigurationStateCustomKey): Pointer; cdecl;
    procedure setCustomState(customState: Pointer; forKey: UIConfigurationStateCustomKey); cdecl;
    procedure setObject(obj: Pointer; forKeyedSubscript: UIConfigurationStateCustomKey); cdecl;
    procedure setTraitCollection(traitCollection: UITraitCollection); cdecl;
    function traitCollection: UITraitCollection; cdecl;
  end;

  UIViewConfigurationStateClass = interface(NSObjectClass)
    ['{1636665D-24CB-4584-B978-9E38739C2C05}']
    {class} function new: Pointer; cdecl;
  end;

  UIViewConfigurationState = interface(NSObject)
    ['{B468D57B-288A-46A9-B4EA-97B0830D70C2}']
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithTraitCollection(traitCollection: UITraitCollection): Pointer; cdecl;
    function isDisabled: Boolean; cdecl;
    function isFocused: Boolean; cdecl;
    function isHighlighted: Boolean; cdecl;
    function isSelected: Boolean; cdecl;
    procedure setDisabled(disabled: Boolean); cdecl;
    procedure setFocused(focused: Boolean); cdecl;
    procedure setHighlighted(highlighted: Boolean); cdecl;
    procedure setSelected(selected: Boolean); cdecl;
    procedure setTraitCollection(traitCollection: UITraitCollection); cdecl;
    function traitCollection: UITraitCollection; cdecl;
  end;
  TUIViewConfigurationState = class(TOCGenericImport<UIViewConfigurationStateClass, UIViewConfigurationState>) end;

  UICellConfigurationStateClass = interface(UIViewConfigurationStateClass)
    ['{6BF8FFBD-02DD-450C-9344-A0D6B5FD2FDD}']
  end;

  UICellConfigurationState = interface(UIViewConfigurationState)
    ['{7F839185-62A1-4E0D-BC42-EA0B429EEE98}']
    function cellDragState: UICellConfigurationDragState; cdecl;
    function cellDropState: UICellConfigurationDropState; cdecl;
    function isEditing: Boolean; cdecl;
    function isExpanded: Boolean; cdecl;
    function isReordering: Boolean; cdecl;
    function isSwiped: Boolean; cdecl;
    procedure setCellDragState(cellDragState: UICellConfigurationDragState); cdecl;
    procedure setCellDropState(cellDropState: UICellConfigurationDropState); cdecl;
    procedure setEditing(editing: Boolean); cdecl;
    procedure setExpanded(expanded: Boolean); cdecl;
    procedure setReordering(reordering: Boolean); cdecl;
    procedure setSwiped(swiped: Boolean); cdecl;
  end;
  TUICellConfigurationState = class(TOCGenericImport<UICellConfigurationStateClass, UICellConfigurationState>) end;

  UICloudSharingControllerDelegate = interface(IObjectiveC)
    ['{439AC756-ADC4-409C-8BF7-371AB4F2945E}']
    procedure cloudSharingController(csc: UICloudSharingController; failedToSaveShareWithError: NSError); cdecl;
    procedure cloudSharingControllerDidSaveShare(csc: UICloudSharingController); cdecl;
    procedure cloudSharingControllerDidStopSharing(csc: UICloudSharingController); cdecl;
    function itemThumbnailDataForCloudSharingController(csc: UICloudSharingController): NSData; cdecl;
    function itemTitleForCloudSharingController(csc: UICloudSharingController): NSString; cdecl;
    function itemTypeForCloudSharingController(csc: UICloudSharingController): NSString; cdecl;
  end;

  UICloudSharingControllerClass = interface(UIViewControllerClass)
    ['{71337635-DD88-4844-8076-74F65859AEDB}']
  end;

  UICloudSharingController = interface(UIViewController)
    ['{2B742BB0-AAAD-4974-B0BB-EF6996D4ABB4}']
    function activityItemSource: Pointer; cdecl;
    function availablePermissions: UICloudSharingPermissionOptions; cdecl;
    function delegate: Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    function initWithPreparationHandler(preparationHandler: TUICloudSharingControllerBlockMethod2): Pointer; cdecl;
    // CloudKit function initWithShare(share: CKShare; container: CKContainer): Pointer; cdecl;
    procedure setAvailablePermissions(availablePermissions: UICloudSharingPermissionOptions); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    // CloudKit function share: CKShare; cdecl;
  end;
  TUICloudSharingController = class(TOCGenericImport<UICloudSharingControllerClass, UICloudSharingController>) end;

  UICollectionViewCompositionalLayoutConfigurationClass = interface(NSObjectClass)
    ['{1A462D38-BBF3-48F2-B1C3-527C499D359A}']
  end;

  UICollectionViewCompositionalLayoutConfiguration = interface(NSObject)
    ['{5243DE50-FE9C-4D91-B1E2-62FB702B360B}']
    function boundarySupplementaryItems: NSArray; cdecl;
    function contentInsetsReference: UIContentInsetsReference; cdecl;
    function interSectionSpacing: CGFloat; cdecl;
    function scrollDirection: UICollectionViewScrollDirection; cdecl;
    procedure setBoundarySupplementaryItems(boundarySupplementaryItems: NSArray); cdecl;
    procedure setContentInsetsReference(contentInsetsReference: UIContentInsetsReference); cdecl;
    procedure setInterSectionSpacing(interSectionSpacing: CGFloat); cdecl;
    procedure setScrollDirection(scrollDirection: UICollectionViewScrollDirection); cdecl;
  end;
  TUICollectionViewCompositionalLayoutConfiguration = class(TOCGenericImport<UICollectionViewCompositionalLayoutConfigurationClass, UICollectionViewCompositionalLayoutConfiguration>) end;

  UICollectionViewCompositionalLayoutClass = interface(UICollectionViewLayoutClass)
    ['{36B06B9A-FE29-44CA-800C-5ECBD37FBE47}']
    {class} function layoutWithListConfiguration(configuration: UICollectionLayoutListConfiguration): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UICollectionViewCompositionalLayout = interface(UICollectionViewLayout)
    ['{FD7816EC-9814-438A-B60D-97C56C10A731}']
    function configuration: UICollectionViewCompositionalLayoutConfiguration; cdecl;
    function initWithSection(section: NSCollectionLayoutSection; configuration: UICollectionViewCompositionalLayoutConfiguration): Pointer; overload; cdecl;
    function initWithSection(section: NSCollectionLayoutSection): Pointer; overload; cdecl;
    function initWithSectionProvider(sectionProvider: UICollectionViewCompositionalLayoutSectionProvider; configuration: UICollectionViewCompositionalLayoutConfiguration): Pointer; overload; cdecl;
    function initWithSectionProvider(sectionProvider: UICollectionViewCompositionalLayoutSectionProvider): Pointer; overload; cdecl;
    procedure setConfiguration(configuration: UICollectionViewCompositionalLayoutConfiguration); cdecl;
  end;
  TUICollectionViewCompositionalLayout = class(TOCGenericImport<UICollectionViewCompositionalLayoutClass, UICollectionViewCompositionalLayout>) end;

  NSCollectionLayoutSectionClass = interface(NSObjectClass)
    ['{B0C795AA-CFA0-48A1-935F-F43FAB151CE7}']
    {class} function new: Pointer; cdecl;
    {class} function sectionWithGroup(group: NSCollectionLayoutGroup): Pointer; cdecl;
    {class} function sectionWithListConfiguration(configuration: UICollectionLayoutListConfiguration; layoutEnvironment: Pointer): Pointer; cdecl;
  end;

  NSCollectionLayoutSection = interface(NSObject)
    ['{A457E3D1-2B43-48F1-ACEF-4226BC83D54B}']
    function boundarySupplementaryItems: NSArray; cdecl;
    function contentInsets: NSDirectionalEdgeInsets; cdecl;
    function contentInsetsReference: UIContentInsetsReference; cdecl;
    function decorationItems: NSArray; cdecl;
    function interGroupSpacing: CGFloat; cdecl;
    function orthogonalScrollingBehavior: UICollectionLayoutSectionOrthogonalScrollingBehavior; cdecl;
    procedure setBoundarySupplementaryItems(boundarySupplementaryItems: NSArray); cdecl;
    procedure setContentInsets(contentInsets: NSDirectionalEdgeInsets); cdecl;
    procedure setContentInsetsReference(contentInsetsReference: UIContentInsetsReference); cdecl;
    procedure setDecorationItems(decorationItems: NSArray); cdecl;
    procedure setInterGroupSpacing(interGroupSpacing: CGFloat); cdecl;
    procedure setOrthogonalScrollingBehavior(orthogonalScrollingBehavior: UICollectionLayoutSectionOrthogonalScrollingBehavior); cdecl;
    procedure setSupplementariesFollowContentInsets(supplementariesFollowContentInsets: Boolean); cdecl;
    procedure setVisibleItemsInvalidationHandler(visibleItemsInvalidationHandler: NSCollectionLayoutSectionVisibleItemsInvalidationHandler); cdecl;
    function supplementariesFollowContentInsets: Boolean; cdecl;
    function visibleItemsInvalidationHandler: NSCollectionLayoutSectionVisibleItemsInvalidationHandler; cdecl;
  end;
  TNSCollectionLayoutSection = class(TOCGenericImport<NSCollectionLayoutSectionClass, NSCollectionLayoutSection>) end;

  NSCollectionLayoutItemClass = interface(NSObjectClass)
    ['{8EDB0D7D-CCF7-45D1-9F49-E86EEF91314E}']
    {class} function itemWithLayoutSize(layoutSize: NSCollectionLayoutSize; supplementaryItems: NSArray): Pointer; overload; cdecl;
    {class} function itemWithLayoutSize(layoutSize: NSCollectionLayoutSize): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutItem = interface(NSObject)
    ['{99CF3845-F999-4319-9BF5-812DBEA499F4}']
    function contentInsets: NSDirectionalEdgeInsets; cdecl;
    function edgeSpacing: NSCollectionLayoutEdgeSpacing; cdecl;
    function layoutSize: NSCollectionLayoutSize; cdecl;
    procedure setContentInsets(contentInsets: NSDirectionalEdgeInsets); cdecl;
    procedure setEdgeSpacing(edgeSpacing: NSCollectionLayoutEdgeSpacing); cdecl;
    function supplementaryItems: NSArray; cdecl;
  end;
  TNSCollectionLayoutItem = class(TOCGenericImport<NSCollectionLayoutItemClass, NSCollectionLayoutItem>) end;

  NSCollectionLayoutGroupCustomItemClass = interface(NSObjectClass)
    ['{36AC30A2-6919-4A36-972C-1BD8273E06B5}']
    {class} function customItemWithFrame(frame: CGRect; zIndex: NSInteger): Pointer; overload; cdecl;
    {class} function customItemWithFrame(frame: CGRect): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutGroupCustomItem = interface(NSObject)
    ['{A05FCB3C-9B5B-424F-8367-D8E6D795309B}']
    function frame: CGRect; cdecl;
    function zIndex: NSInteger; cdecl;
  end;
  TNSCollectionLayoutGroupCustomItem = class(TOCGenericImport<NSCollectionLayoutGroupCustomItemClass, NSCollectionLayoutGroupCustomItem>) end;

  NSCollectionLayoutGroupClass = interface(NSCollectionLayoutItemClass)
    ['{DEDC3C14-E61D-4C05-873B-7F20ED5F1449}']
    {class} function customGroupWithLayoutSize(layoutSize: NSCollectionLayoutSize; itemProvider: NSCollectionLayoutGroupCustomItemProvider): Pointer; cdecl;
    {class} function horizontalGroupWithLayoutSize(layoutSize: NSCollectionLayoutSize; subitems: NSArray): Pointer; overload; cdecl;
    {class} function horizontalGroupWithLayoutSize(layoutSize: NSCollectionLayoutSize; subitem: NSCollectionLayoutItem; count: NSInteger): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function verticalGroupWithLayoutSize(layoutSize: NSCollectionLayoutSize; subitems: NSArray): Pointer; overload; cdecl;
    {class} function verticalGroupWithLayoutSize(layoutSize: NSCollectionLayoutSize; subitem: NSCollectionLayoutItem; count: NSInteger): Pointer; overload; cdecl;
  end;

  NSCollectionLayoutGroup = interface(NSCollectionLayoutItem)
    ['{37777892-9711-4CE6-988A-8162C7EF2159}']
    function interItemSpacing: NSCollectionLayoutSpacing; cdecl;
    procedure setInterItemSpacing(interItemSpacing: NSCollectionLayoutSpacing); cdecl;
    procedure setSupplementaryItems(supplementaryItems: NSArray); cdecl;
    function subitems: NSArray; cdecl;
    function supplementaryItems: NSArray; cdecl;
    function visualDescription: NSString; cdecl;
  end;
  TNSCollectionLayoutGroup = class(TOCGenericImport<NSCollectionLayoutGroupClass, NSCollectionLayoutGroup>) end;

  NSCollectionLayoutDimensionClass = interface(NSObjectClass)
    ['{03613083-E812-4B2E-BFFD-C886A1A00920}']
    {class} function absoluteDimension(absoluteDimension: CGFloat): Pointer; cdecl;
    {class} function estimatedDimension(estimatedDimension: CGFloat): Pointer; cdecl;
    {class} function fractionalHeightDimension(fractionalHeight: CGFloat): Pointer; cdecl;
    {class} function fractionalWidthDimension(fractionalWidth: CGFloat): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutDimension = interface(NSObject)
    ['{4434BB59-F1A0-40C0-9742-0BAF7856ED0D}']
    function dimension: CGFloat; cdecl;
    function isAbsolute: Boolean; cdecl;
    function isEstimated: Boolean; cdecl;
    function isFractionalHeight: Boolean; cdecl;
    function isFractionalWidth: Boolean; cdecl;
  end;
  TNSCollectionLayoutDimension = class(TOCGenericImport<NSCollectionLayoutDimensionClass, NSCollectionLayoutDimension>) end;

  NSCollectionLayoutSizeClass = interface(NSObjectClass)
    ['{12D83A6E-E5EA-4B33-8793-2F232A570E0A}']
    {class} function new: Pointer; cdecl;
    {class} function sizeWithWidthDimension(width: NSCollectionLayoutDimension; heightDimension: NSCollectionLayoutDimension): Pointer; cdecl;
  end;

  NSCollectionLayoutSize = interface(NSObject)
    ['{A7F4FC75-50D9-42B0-B8D1-D1E3F29A45BA}']
    function heightDimension: NSCollectionLayoutDimension; cdecl;
    function widthDimension: NSCollectionLayoutDimension; cdecl;
  end;
  TNSCollectionLayoutSize = class(TOCGenericImport<NSCollectionLayoutSizeClass, NSCollectionLayoutSize>) end;

  NSCollectionLayoutSpacingClass = interface(NSObjectClass)
    ['{442E6A12-16F8-4047-AB0D-97B8A3335F1B}']
    {class} function fixedSpacing(fixedSpacing: CGFloat): Pointer; cdecl;
    {class} function flexibleSpacing(flexibleSpacing: CGFloat): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutSpacing = interface(NSObject)
    ['{7615DD01-1C39-4719-BADC-BF4619D46D66}']
    function isFixedSpacing: Boolean; cdecl;
    function isFlexibleSpacing: Boolean; cdecl;
    function spacing: CGFloat; cdecl;
  end;
  TNSCollectionLayoutSpacing = class(TOCGenericImport<NSCollectionLayoutSpacingClass, NSCollectionLayoutSpacing>) end;

  NSCollectionLayoutEdgeSpacingClass = interface(NSObjectClass)
    ['{2C3E8411-E5B5-4E8C-9137-C7F9EEA8B8DF}']
    {class} function new: Pointer; cdecl;
    {class} function spacingForLeading(leading: NSCollectionLayoutSpacing; top: NSCollectionLayoutSpacing; trailing: NSCollectionLayoutSpacing; bottom: NSCollectionLayoutSpacing): Pointer; cdecl;
  end;

  NSCollectionLayoutEdgeSpacing = interface(NSObject)
    ['{B88E23E8-6CB7-428A-98FE-F315D304CE45}']
    function bottom: NSCollectionLayoutSpacing; cdecl;
    function leading: NSCollectionLayoutSpacing; cdecl;
    function top: NSCollectionLayoutSpacing; cdecl;
    function trailing: NSCollectionLayoutSpacing; cdecl;
  end;
  TNSCollectionLayoutEdgeSpacing = class(TOCGenericImport<NSCollectionLayoutEdgeSpacingClass, NSCollectionLayoutEdgeSpacing>) end;

  NSCollectionLayoutSupplementaryItemClass = interface(NSCollectionLayoutItemClass)
    ['{14950A85-CA02-4C21-8014-4FBD0FFCCDC3}']
    {class} function new: Pointer; cdecl;
    {class} function supplementaryItemWithLayoutSize(layoutSize: NSCollectionLayoutSize; elementKind: NSString; containerAnchor: NSCollectionLayoutAnchor; itemAnchor: NSCollectionLayoutAnchor): Pointer; overload; cdecl;
    {class} function supplementaryItemWithLayoutSize(layoutSize: NSCollectionLayoutSize; elementKind: NSString; containerAnchor: NSCollectionLayoutAnchor): Pointer; overload; cdecl;
  end;

  NSCollectionLayoutSupplementaryItem = interface(NSCollectionLayoutItem)
    ['{A9E09328-42EC-483A-9F1F-39DCCBAB8FEC}']
    function containerAnchor: NSCollectionLayoutAnchor; cdecl;
    function elementKind: NSString; cdecl;
    function itemAnchor: NSCollectionLayoutAnchor; cdecl;
    procedure setZIndex(zIndex: NSInteger); cdecl;
    function zIndex: NSInteger; cdecl;
  end;
  TNSCollectionLayoutSupplementaryItem = class(TOCGenericImport<NSCollectionLayoutSupplementaryItemClass, NSCollectionLayoutSupplementaryItem>) end;

  NSCollectionLayoutBoundarySupplementaryItemClass = interface(NSCollectionLayoutSupplementaryItemClass)
    ['{2DFF96C5-9458-4E1C-BF65-BC976E9C645C}']
    {class} function boundarySupplementaryItemWithLayoutSize(layoutSize: NSCollectionLayoutSize; elementKind: NSString; alignment: NSRectAlignment; absoluteOffset: CGPoint): Pointer; overload; cdecl;
    {class} function boundarySupplementaryItemWithLayoutSize(layoutSize: NSCollectionLayoutSize; elementKind: NSString; alignment: NSRectAlignment): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutBoundarySupplementaryItem = interface(NSCollectionLayoutSupplementaryItem)
    ['{4B74BE2A-2FA4-4D50-A43D-B151B3C51A69}']
    function alignment: NSRectAlignment; cdecl;
    function extendsBoundary: Boolean; cdecl;
    function offset: CGPoint; cdecl;
    function pinToVisibleBounds: Boolean; cdecl;
    procedure setExtendsBoundary(extendsBoundary: Boolean); cdecl;
    procedure setPinToVisibleBounds(pinToVisibleBounds: Boolean); cdecl;
  end;
  TNSCollectionLayoutBoundarySupplementaryItem = class(TOCGenericImport<NSCollectionLayoutBoundarySupplementaryItemClass, NSCollectionLayoutBoundarySupplementaryItem>) end;

  NSCollectionLayoutDecorationItemClass = interface(NSCollectionLayoutItemClass)
    ['{53EDB013-FB8F-48EE-8551-78DF496E1D94}']
    {class} function backgroundDecorationItemWithElementKind(elementKind: NSString): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutDecorationItem = interface(NSCollectionLayoutItem)
    ['{007A1BCE-0EAE-4218-82D2-0C6E9C816C0A}']
    function elementKind: NSString; cdecl;
    procedure setZIndex(zIndex: NSInteger); cdecl;
    function zIndex: NSInteger; cdecl;
  end;
  TNSCollectionLayoutDecorationItem = class(TOCGenericImport<NSCollectionLayoutDecorationItemClass, NSCollectionLayoutDecorationItem>) end;

  NSCollectionLayoutAnchorClass = interface(NSObjectClass)
    ['{ED6A6569-1F73-4A14-B847-E2B50ED2B0F4}']
    {class} function layoutAnchorWithEdges(edges: NSDirectionalRectEdge): Pointer; cdecl;
    [MethodName('layoutAnchorWithEdges:absoluteOffset:')]
    {class} function layoutAnchorWithEdgesAbsoluteOffset(edges: NSDirectionalRectEdge; absoluteOffset: CGPoint): Pointer; cdecl;
    [MethodName('layoutAnchorWithEdges:fractionalOffset:')]
    {class} function layoutAnchorWithEdgesFractionalOffset(edges: NSDirectionalRectEdge; fractionalOffset: CGPoint): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  NSCollectionLayoutAnchor = interface(NSObject)
    ['{F8B0758E-1C19-4928-B7AB-BF4605C7E127}']
    function edges: NSDirectionalRectEdge; cdecl;
    function isAbsoluteOffset: Boolean; cdecl;
    function isFractionalOffset: Boolean; cdecl;
    function offset: CGPoint; cdecl;
  end;
  TNSCollectionLayoutAnchor = class(TOCGenericImport<NSCollectionLayoutAnchorClass, NSCollectionLayoutAnchor>) end;

  NSCollectionLayoutContainer = interface(IObjectiveC)
    ['{91657C31-CE70-476B-AAD7-E201C7FA192D}']
    function contentInsets: NSDirectionalEdgeInsets; cdecl;
    function contentSize: CGSize; cdecl;
    function effectiveContentInsets: NSDirectionalEdgeInsets; cdecl;
    function effectiveContentSize: CGSize; cdecl;
  end;

  NSCollectionLayoutEnvironment = interface(IObjectiveC)
    ['{6A00CF9F-6DC8-4EEC-BD2C-62A026A130BB}']
    function container: Pointer; cdecl;
    function traitCollection: UITraitCollection; cdecl;
  end;

  NSCollectionLayoutVisibleItem = interface(IObjectiveC)
    ['{1BAA9D6A-22F2-4117-9F89-EEB21C29E87F}']
    function alpha: CGFloat; cdecl;
    function bounds: CGRect; cdecl;
    function center: CGPoint; cdecl;
    function frame: CGRect; cdecl;
    function indexPath: NSIndexPath; cdecl;
    function isHidden: Boolean; cdecl;
    function name: NSString; cdecl;
    function representedElementCategory: UICollectionElementCategory; cdecl;
    function representedElementKind: NSString; cdecl;
    procedure setAlpha(alpha: CGFloat); cdecl;
    procedure setCenter(center: CGPoint); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setTransform(transform: CGAffineTransform); cdecl;
    procedure setTransform3D(transform3D: CATransform3D); cdecl;
    procedure setZIndex(zIndex: NSInteger); cdecl;
    function transform: CGAffineTransform; cdecl;
    function transform3D: CATransform3D; cdecl;
    function zIndex: NSInteger; cdecl;
  end;

  UICollectionLayoutListConfigurationClass = interface(NSObjectClass)
    ['{0D7B7999-DCBD-46FB-9DFC-C23C3AE29DA2}']
    {class} function new: Pointer; cdecl;
  end;

  UICollectionLayoutListConfiguration = interface(NSObject)
    ['{7CF1B805-47A7-40E6-900B-00005F49E04C}']
    function appearance: UICollectionLayoutListAppearance; cdecl;
    function backgroundColor: UIColor; cdecl;
    function footerMode: UICollectionLayoutListFooterMode; cdecl;
    function headerMode: UICollectionLayoutListHeaderMode; cdecl;
    function initWithAppearance(appearance: UICollectionLayoutListAppearance): Pointer; cdecl;
    function leadingSwipeActionsConfigurationProvider: UICollectionLayoutListSwipeActionsConfigurationProvider; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setFooterMode(footerMode: UICollectionLayoutListFooterMode); cdecl;
    procedure setHeaderMode(headerMode: UICollectionLayoutListHeaderMode); cdecl;
    procedure setLeadingSwipeActionsConfigurationProvider(leadingSwipeActionsConfigurationProvider: UICollectionLayoutListSwipeActionsConfigurationProvider); cdecl;
    procedure setShowsSeparators(showsSeparators: Boolean); cdecl;
    procedure setTrailingSwipeActionsConfigurationProvider(trailingSwipeActionsConfigurationProvider: UICollectionLayoutListSwipeActionsConfigurationProvider); cdecl;
    function showsSeparators: Boolean; cdecl;
    function trailingSwipeActionsConfigurationProvider: UICollectionLayoutListSwipeActionsConfigurationProvider; cdecl;
  end;
  TUICollectionLayoutListConfiguration = class(TOCGenericImport<UICollectionLayoutListConfigurationClass, UICollectionLayoutListConfiguration>) end;

  UIDataSourceTranslating = interface(IObjectiveC)
    ['{590DD309-5B53-430C-9CC0-605C2D1D8537}']
    function dataSourceIndexPathForPresentationIndexPath(presentationIndexPath: NSIndexPath): NSIndexPath; cdecl;
    function dataSourceSectionIndexForPresentationSectionIndex(presentationSectionIndex: NSInteger): NSInteger; cdecl;
    procedure performUsingPresentationValues(actionsToTranslate: TUIDataSourceTranslatingBlockMethod1); cdecl;
    function presentationIndexPathForDataSourceIndexPath(dataSourceIndexPath: NSIndexPath): NSIndexPath; cdecl;
    function presentationSectionIndexForDataSourceSectionIndex(dataSourceSectionIndex: NSInteger): NSInteger; cdecl;
  end;

  UICollectionViewFocusUpdateContextClass = interface(UIFocusUpdateContextClass)
    ['{E29950D5-0CEC-4273-8E2C-591486715A83}']
  end;

  UICollectionViewFocusUpdateContext = interface(UIFocusUpdateContext)
    ['{4D8709A6-8F65-4052-B7E4-CEAF25CCAA15}']
    function nextFocusedIndexPath: NSIndexPath; cdecl;
    function previouslyFocusedIndexPath: NSIndexPath; cdecl;
  end;
  TUICollectionViewFocusUpdateContext = class(TOCGenericImport<UICollectionViewFocusUpdateContextClass, UICollectionViewFocusUpdateContext>) end;

  UICollectionViewDataSourcePrefetching = interface(IObjectiveC)
    ['{B7ECA311-7408-46C7-B630-A67BBD9842D1}']
    [MethodName('collectionView:cancelPrefetchingForItemsAtIndexPaths:')]
    procedure collectionViewCancelPrefetchingForItemsAtIndexPaths(collectionView: UICollectionView; cancelPrefetchingForItemsAtIndexPaths: NSArray); cdecl;
    [MethodName('collectionView:prefetchItemsAtIndexPaths:')]
    procedure collectionViewPrefetchItemsAtIndexPaths(collectionView: UICollectionView; prefetchItemsAtIndexPaths: NSArray); cdecl;
  end;

  UICollectionViewDragDelegate = interface(IObjectiveC)
    ['{59BE9114-B633-4BBC-897C-DAC15B1CF32E}']
    [MethodName('collectionView:dragPreviewParametersForItemAtIndexPath:')]
    function collectionViewDragPreviewParametersForItemAtIndexPath(collectionView: UICollectionView; dragPreviewParametersForItemAtIndexPath: NSIndexPath): UIDragPreviewParameters; cdecl;
    [MethodName('collectionView:dragSessionAllowsMoveOperation:')]
    function collectionViewDragSessionAllowsMoveOperation(collectionView: UICollectionView; dragSessionAllowsMoveOperation: Pointer): Boolean; cdecl;
    [MethodName('collectionView:dragSessionDidEnd:')]
    procedure collectionViewDragSessionDidEnd(collectionView: UICollectionView; dragSessionDidEnd: Pointer); cdecl;
    [MethodName('collectionView:dragSessionIsRestrictedToDraggingApplication:')]
    function collectionViewDragSessionIsRestrictedToDraggingApplication(collectionView: UICollectionView; dragSessionIsRestrictedToDraggingApplication: Pointer): Boolean; cdecl;
    [MethodName('collectionView:dragSessionWillBegin:')]
    procedure collectionViewDragSessionWillBegin(collectionView: UICollectionView; dragSessionWillBegin: Pointer); cdecl;
    [MethodName('collectionView:itemsForAddingToDragSession:atIndexPath:point:')]
    function collectionViewItemsForAddingToDragSession(collectionView: UICollectionView; itemsForAddingToDragSession: Pointer; atIndexPath: NSIndexPath; point: CGPoint): NSArray; cdecl;
    [MethodName('collectionView:itemsForBeginningDragSession:atIndexPath:')]
    function collectionViewItemsForBeginningDragSession(collectionView: UICollectionView; itemsForBeginningDragSession: Pointer; atIndexPath: NSIndexPath): NSArray; cdecl;
  end;

  UICollectionViewDropDelegate = interface(IObjectiveC)
    ['{4A28695D-1144-464C-B3D9-440C1DF4A435}']
    [MethodName('collectionView:canHandleDropSession:')]
    function collectionViewCanHandleDropSession(collectionView: UICollectionView; canHandleDropSession: Pointer): Boolean; cdecl;
    [MethodName('collectionView:dropPreviewParametersForItemAtIndexPath:')]
    function collectionViewDropPreviewParametersForItemAtIndexPath(collectionView: UICollectionView; dropPreviewParametersForItemAtIndexPath: NSIndexPath): UIDragPreviewParameters; cdecl;
    [MethodName('collectionView:dropSessionDidEnd:')]
    procedure collectionViewDropSessionDidEnd(collectionView: UICollectionView; dropSessionDidEnd: Pointer); cdecl;
    [MethodName('collectionView:dropSessionDidEnter:')]
    procedure collectionViewDropSessionDidEnter(collectionView: UICollectionView; dropSessionDidEnter: Pointer); cdecl;
    [MethodName('collectionView:dropSessionDidExit:')]
    procedure collectionViewDropSessionDidExit(collectionView: UICollectionView; dropSessionDidExit: Pointer); cdecl;
    [MethodName('collectionView:dropSessionDidUpdate:withDestinationIndexPath:')]
    function collectionViewDropSessionDidUpdate(collectionView: UICollectionView; dropSessionDidUpdate: Pointer; withDestinationIndexPath: NSIndexPath): UICollectionViewDropProposal; cdecl;
    [MethodName('collectionView:performDropWithCoordinator:')]
    procedure collectionViewPerformDropWithCoordinator(collectionView: UICollectionView; performDropWithCoordinator: Pointer); cdecl;
  end;

  UICollectionViewDropProposalClass = interface(UIDropProposalClass)
    ['{74750222-4E26-4D74-B056-9BCC801AE8B1}']
  end;

  UICollectionViewDropProposal = interface(UIDropProposal)
    ['{F49A245E-0416-45BE-BD15-5BF001E3A5D7}']
    function initWithDropOperation(operation: UIDropOperation; intent: UICollectionViewDropIntent): Pointer; cdecl;
    function intent: UICollectionViewDropIntent; cdecl;
  end;
  TUICollectionViewDropProposal = class(TOCGenericImport<UICollectionViewDropProposalClass, UICollectionViewDropProposal>) end;

  UICollectionViewDropCoordinator = interface(IObjectiveC)
    ['{17E15BC0-D662-40B6-99E0-8AA622E7698C}']
    function destinationIndexPath: NSIndexPath; cdecl;
    function dropItem(dragItem: UIDragItem; toTarget: UIDragPreviewTarget): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; intoItemAtIndexPath: NSIndexPath; rect: CGRect): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; toPlaceholder: UICollectionViewDropPlaceholder): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; toItemAtIndexPath: NSIndexPath): Pointer; overload; cdecl;
    function items: NSArray; cdecl;
    function proposal: UICollectionViewDropProposal; cdecl;
    function session: Pointer; cdecl;
  end;

  UICollectionViewPlaceholderClass = interface(NSObjectClass)
    ['{4F76FF16-CAE0-4FA7-874B-A554AD2EF3B0}']
    {class} function new: Pointer; cdecl;
  end;

  UICollectionViewPlaceholder = interface(NSObject)
    ['{4F7C4318-60C8-4745-B406-9DE73AD9C03E}']
    function cellUpdateHandler: TUICollectionViewPlaceholderBlockMethod1; cdecl;
    function initWithInsertionIndexPath(insertionIndexPath: NSIndexPath; reuseIdentifier: NSString): Pointer; cdecl;
    procedure setCellUpdateHandler(cellUpdateHandler: TUICollectionViewPlaceholderBlockMethod2); cdecl;
  end;
  TUICollectionViewPlaceholder = class(TOCGenericImport<UICollectionViewPlaceholderClass, UICollectionViewPlaceholder>) end;

  UICollectionViewDropPlaceholderClass = interface(UICollectionViewPlaceholderClass)
    ['{0B9AD765-F09A-4E48-8DE5-6CC4A6F51FA7}']
  end;

  UICollectionViewDropPlaceholder = interface(UICollectionViewPlaceholder)
    ['{588DDE4C-B457-41AA-9F94-F57BB8CC3688}']
    function previewParametersProvider: TUICollectionViewDropPlaceholderBlockMethod1; cdecl;
    procedure setPreviewParametersProvider(previewParametersProvider: TUICollectionViewDropPlaceholderBlockMethod2); cdecl;
  end;
  TUICollectionViewDropPlaceholder = class(TOCGenericImport<UICollectionViewDropPlaceholderClass, UICollectionViewDropPlaceholder>) end;

  UICollectionViewDropItem = interface(IObjectiveC)
    ['{97F5D91A-11D9-44B0-8F92-74374970FFDA}']
    function dragItem: UIDragItem; cdecl;
    function previewSize: CGSize; cdecl;
    function sourceIndexPath: NSIndexPath; cdecl;
  end;

  UICollectionViewDropPlaceholderContext = interface(IObjectiveC)
    ['{DBABDCA4-DCB7-444B-9AC2-DEBCCC6E9C05}']
    function commitInsertionWithDataSourceUpdates(dataSourceUpdates: TUICollectionViewDropPlaceholderContextBlockMethod1): Boolean; cdecl;
    function deletePlaceholder: Boolean; cdecl;
    function dragItem: UIDragItem; cdecl;
    procedure setNeedsCellUpdate; cdecl;
  end;

  UICollectionViewCellRegistrationClass = interface(NSObjectClass)
    ['{0579D6AA-AF63-4A3E-B751-192927CFD1D2}']
    {class} function registrationWithCellClass(cellClass: Pointer; configurationHandler: UICollectionViewCellRegistrationConfigurationHandler): Pointer; cdecl;
    {class} function registrationWithCellNib(cellNib: UINib; configurationHandler: UICollectionViewCellRegistrationConfigurationHandler): Pointer; cdecl;
  end;

  UICollectionViewCellRegistration = interface(NSObject)
    ['{6E9B9E4A-8141-47DC-A0BE-160396DF73EB}']
    function cellClass: Pointer; cdecl;
    function cellNib: UINib; cdecl;
    function configurationHandler: UICollectionViewCellRegistrationConfigurationHandler; cdecl;
  end;
  TUICollectionViewCellRegistration = class(TOCGenericImport<UICollectionViewCellRegistrationClass, UICollectionViewCellRegistration>) end;

  UICollectionViewSupplementaryRegistrationClass = interface(NSObjectClass)
    ['{4E0FAA59-0482-4893-9A89-8AB077234056}']
    {class} function registrationWithSupplementaryClass(supplementaryClass: Pointer; elementKind: NSString; configurationHandler: UICollectionViewSupplementaryRegistrationConfigurationHandler): Pointer; cdecl;
    {class} function registrationWithSupplementaryNib(supplementaryNib: UINib; elementKind: NSString; configurationHandler: UICollectionViewSupplementaryRegistrationConfigurationHandler): Pointer; cdecl;
  end;

  UICollectionViewSupplementaryRegistration = interface(NSObject)
    ['{ED3F5336-DADC-4AD4-BAC4-ED9A95E2522F}']
    function configurationHandler: UICollectionViewSupplementaryRegistrationConfigurationHandler; cdecl;
    function elementKind: NSString; cdecl;
    function supplementaryClass: Pointer; cdecl;
    function supplementaryNib: UINib; cdecl;
  end;
  TUICollectionViewSupplementaryRegistration = class(TOCGenericImport<UICollectionViewSupplementaryRegistrationClass, UICollectionViewSupplementaryRegistration>) end;

  UICollectionViewListCellClass = interface(UICollectionViewCellClass)
    ['{66A6242B-BC33-4605-882F-F8A90854F572}']
  end;

  UICollectionViewListCell = interface(UICollectionViewCell)
    ['{746108B3-7C20-4CF4-9779-ACFE60D53D57}']
    function accessories: NSArray; cdecl;
    function defaultContentConfiguration: UIListContentConfiguration; cdecl;
    function indentationLevel: NSInteger; cdecl;
    function indentationWidth: CGFloat; cdecl;
    function indentsAccessories: Boolean; cdecl;
    function separatorLayoutGuide: UILayoutGuide; cdecl;
    procedure setAccessories(accessories: NSArray); cdecl;
    procedure setIndentationLevel(indentationLevel: NSInteger); cdecl;
    procedure setIndentationWidth(indentationWidth: CGFloat); cdecl;
    procedure setIndentsAccessories(indentsAccessories: Boolean); cdecl;
  end;
  TUICollectionViewListCell = class(TOCGenericImport<UICollectionViewListCellClass, UICollectionViewListCell>) end;

  UIColorPickerViewControllerDelegate = interface(IObjectiveC)
    ['{94189118-3C18-42EF-9808-ED013E0A396D}']
    procedure colorPickerViewControllerDidFinish(viewController: UIColorPickerViewController); cdecl;
    procedure colorPickerViewControllerDidSelectColor(viewController: UIColorPickerViewController); cdecl;
  end;

  UIColorPickerViewControllerClass = interface(UIViewControllerClass)
    ['{01BCDEDF-2DEE-4F58-A24F-98E85E3A54CA}']
  end;

  UIColorPickerViewController = interface(UIViewController)
    ['{42D44F8F-074A-45A0-B60B-40EA995C035B}']
    function delegate: Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    function selectedColor: UIColor; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setSelectedColor(selectedColor: UIColor); cdecl;
    procedure setSupportsAlpha(supportsAlpha: Boolean); cdecl;
    function supportsAlpha: Boolean; cdecl;
  end;
  TUIColorPickerViewController = class(TOCGenericImport<UIColorPickerViewControllerClass, UIColorPickerViewController>) end;

  UIColorWellClass = interface(UIControlClass)
    ['{A808CB13-A0F0-40E5-A8DF-5B6B39721556}']
  end;

  UIColorWell = interface(UIControl)
    ['{CB019241-CD57-4AF7-A1DC-E81CC730B876}']
    function selectedColor: UIColor; cdecl;
    procedure setSelectedColor(selectedColor: UIColor); cdecl;
    procedure setSupportsAlpha(supportsAlpha: Boolean); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function supportsAlpha: Boolean; cdecl;
    function title: NSString; cdecl;
  end;
  TUIColorWell = class(TOCGenericImport<UIColorWellClass, UIColorWell>) end;

  UIContentConfiguration = interface(IObjectiveC)
    ['{1C304783-C126-4BB8-ACB9-C2342F71B051}']
    function makeContentView: UIView; cdecl;
    function updatedConfigurationForState(state: Pointer): Pointer; cdecl;
  end;

  UIContentView = interface(IObjectiveC)
    ['{C6876C73-9C94-429F-B7A3-D7DC190FD52E}']
    function configuration: Pointer; cdecl;
    procedure setConfiguration(configuration: Pointer); cdecl;
  end;

  UIContextualActionClass = interface(NSObjectClass)
    ['{D33ED3EB-C608-460F-BE87-3FB2F95198EF}']
    {class} function contextualActionWithStyle(style: UIContextualActionStyle; title: NSString; handler: UIContextualActionHandler): Pointer; cdecl;
  end;

  UIContextualAction = interface(NSObject)
    ['{B2A28793-58F9-46BC-9A06-5D7E8E4BEC10}']
    function backgroundColor: UIColor; cdecl;
    function handler: UIContextualActionHandler; cdecl;
    function image: UIImage; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function style: UIContextualActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TUIContextualAction = class(TOCGenericImport<UIContextualActionClass, UIContextualAction>) end;

  UIDeferredMenuElementClass = interface(UIMenuElementClass)
    ['{63B5926F-1CA2-4930-87FC-B646463C09FB}']
    {class} function elementWithProvider(elementProvider: TUIDeferredMenuElementBlockMethod2): Pointer; cdecl;
  end;

  UIDeferredMenuElement = interface(UIMenuElement)
    ['{3AAC5D78-E3F3-401B-95DF-AAB627635CEB}']
  end;
  TUIDeferredMenuElement = class(TOCGenericImport<UIDeferredMenuElementClass, UIDeferredMenuElement>) end;

  UISwipeActionsConfigurationClass = interface(NSObjectClass)
    ['{15A87207-0E8E-46E1-9E7A-C2E91D41E28B}']
    {class} function configurationWithActions(actions: NSArray): Pointer; cdecl;
  end;

  UISwipeActionsConfiguration = interface(NSObject)
    ['{52227442-972D-4BE5-A64D-778D5918D3F0}']
    function actions: NSArray; cdecl;
    function performsFirstActionWithFullSwipe: Boolean; cdecl;
    procedure setPerformsFirstActionWithFullSwipe(performsFirstActionWithFullSwipe: Boolean); cdecl;
  end;
  TUISwipeActionsConfiguration = class(TOCGenericImport<UISwipeActionsConfigurationClass, UISwipeActionsConfiguration>) end;

  UITableViewRowActionClass = interface(NSObjectClass)
    ['{D6EEAFF9-628A-438A-869C-3EF495196C4E}']
    {class} function rowActionWithStyle(style: UITableViewRowActionStyle; title: NSString; handler: TUITableViewRowActionBlockMethod1): Pointer; cdecl;
  end;

  UITableViewRowAction = interface(NSObject)
    ['{988D4F80-DC98-463D-9140-500BC9B3882F}']
    function backgroundColor: UIColor; cdecl;
    function backgroundEffect: UIVisualEffect; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setBackgroundEffect(backgroundEffect: UIVisualEffect); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function style: UITableViewRowActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TUITableViewRowAction = class(TOCGenericImport<UITableViewRowActionClass, UITableViewRowAction>) end;

  UITableViewFocusUpdateContextClass = interface(UIFocusUpdateContextClass)
    ['{BCF816DE-CB3E-40DA-BC02-0D5C95DEA25E}']
  end;

  UITableViewFocusUpdateContext = interface(UIFocusUpdateContext)
    ['{A3B132CA-C46E-46B5-A82A-CB961C127CE4}']
    function nextFocusedIndexPath: NSIndexPath; cdecl;
    function previouslyFocusedIndexPath: NSIndexPath; cdecl;
  end;
  TUITableViewFocusUpdateContext = class(TOCGenericImport<UITableViewFocusUpdateContextClass, UITableViewFocusUpdateContext>) end;

  UITableViewDataSourcePrefetching = interface(IObjectiveC)
    ['{193112D8-C4EC-4753-995B-98FF0A215CA7}']
    [MethodName('tableView:cancelPrefetchingForRowsAtIndexPaths:')]
    procedure tableViewCancelPrefetchingForRowsAtIndexPaths(tableView: UITableView; cancelPrefetchingForRowsAtIndexPaths: NSArray); cdecl;
    [MethodName('tableView:prefetchRowsAtIndexPaths:')]
    procedure tableViewPrefetchRowsAtIndexPaths(tableView: UITableView; prefetchRowsAtIndexPaths: NSArray); cdecl;
  end;

  UITableViewDragDelegate = interface(IObjectiveC)
    ['{B2A04343-90DD-474F-B528-AEB8617E128B}']
    [MethodName('tableView:dragPreviewParametersForRowAtIndexPath:')]
    function tableViewDragPreviewParametersForRowAtIndexPath(tableView: UITableView; dragPreviewParametersForRowAtIndexPath: NSIndexPath): UIDragPreviewParameters; cdecl;
    [MethodName('tableView:dragSessionAllowsMoveOperation:')]
    function tableViewDragSessionAllowsMoveOperation(tableView: UITableView; dragSessionAllowsMoveOperation: Pointer): Boolean; cdecl;
    [MethodName('tableView:dragSessionDidEnd:')]
    procedure tableViewDragSessionDidEnd(tableView: UITableView; dragSessionDidEnd: Pointer); cdecl;
    [MethodName('tableView:dragSessionIsRestrictedToDraggingApplication:')]
    function tableViewDragSessionIsRestrictedToDraggingApplication(tableView: UITableView; dragSessionIsRestrictedToDraggingApplication: Pointer): Boolean; cdecl;
    [MethodName('tableView:dragSessionWillBegin:')]
    procedure tableViewDragSessionWillBegin(tableView: UITableView; dragSessionWillBegin: Pointer); cdecl;
    [MethodName('tableView:itemsForAddingToDragSession:atIndexPath:point:')]
    function tableViewItemsForAddingToDragSession(tableView: UITableView; itemsForAddingToDragSession: Pointer; atIndexPath: NSIndexPath; point: CGPoint): NSArray; cdecl;
    [MethodName('tableView:itemsForBeginningDragSession:atIndexPath:')]
    function tableViewItemsForBeginningDragSession(tableView: UITableView; itemsForBeginningDragSession: Pointer; atIndexPath: NSIndexPath): NSArray; cdecl;
  end;

  UITableViewDropDelegate = interface(IObjectiveC)
    ['{0477929E-B87C-41D7-9465-869720A85D0F}']
    [MethodName('tableView:canHandleDropSession:')]
    function tableViewCanHandleDropSession(tableView: UITableView; canHandleDropSession: Pointer): Boolean; cdecl;
    [MethodName('tableView:dropPreviewParametersForRowAtIndexPath:')]
    function tableViewDropPreviewParametersForRowAtIndexPath(tableView: UITableView; dropPreviewParametersForRowAtIndexPath: NSIndexPath): UIDragPreviewParameters; cdecl;
    [MethodName('tableView:dropSessionDidEnd:')]
    procedure tableViewDropSessionDidEnd(tableView: UITableView; dropSessionDidEnd: Pointer); cdecl;
    [MethodName('tableView:dropSessionDidEnter:')]
    procedure tableViewDropSessionDidEnter(tableView: UITableView; dropSessionDidEnter: Pointer); cdecl;
    [MethodName('tableView:dropSessionDidExit:')]
    procedure tableViewDropSessionDidExit(tableView: UITableView; dropSessionDidExit: Pointer); cdecl;
    [MethodName('tableView:dropSessionDidUpdate:withDestinationIndexPath:')]
    function tableViewDropSessionDidUpdate(tableView: UITableView; dropSessionDidUpdate: Pointer; withDestinationIndexPath: NSIndexPath): UITableViewDropProposal; cdecl;
    [MethodName('tableView:performDropWithCoordinator:')]
    procedure tableViewPerformDropWithCoordinator(tableView: UITableView; performDropWithCoordinator: Pointer); cdecl;
  end;

  UITableViewDropProposalClass = interface(UIDropProposalClass)
    ['{E8A7CEB4-4C72-4FF1-ABB6-0B8B0DC2ADC7}']
  end;

  UITableViewDropProposal = interface(UIDropProposal)
    ['{51CA5AE3-ECDC-46C6-B211-8026D11011CF}']
    function initWithDropOperation(operation: UIDropOperation; intent: UITableViewDropIntent): Pointer; cdecl;
    function intent: UITableViewDropIntent; cdecl;
  end;
  TUITableViewDropProposal = class(TOCGenericImport<UITableViewDropProposalClass, UITableViewDropProposal>) end;

  UITableViewDropCoordinator = interface(IObjectiveC)
    ['{C8F32891-CE1A-4A00-8DC6-A5C116A29B31}']
    function destinationIndexPath: NSIndexPath; cdecl;
    function dropItem(dragItem: UIDragItem; toTarget: UIDragPreviewTarget): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; intoRowAtIndexPath: NSIndexPath; rect: CGRect): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; toPlaceholder: UITableViewDropPlaceholder): Pointer; overload; cdecl;
    function dropItem(dragItem: UIDragItem; toRowAtIndexPath: NSIndexPath): Pointer; overload; cdecl;
    function items: NSArray; cdecl;
    function proposal: UITableViewDropProposal; cdecl;
    function session: Pointer; cdecl;
  end;

  UITableViewPlaceholderClass = interface(NSObjectClass)
    ['{1703460C-2EAC-4973-9881-A4F9C0CAA645}']
    {class} function new: Pointer; cdecl;
  end;

  UITableViewPlaceholder = interface(NSObject)
    ['{C255E0ED-1CF7-4A48-A133-DB0C2176BE78}']
    function cellUpdateHandler: TUITableViewPlaceholderBlockMethod1; cdecl;
    function initWithInsertionIndexPath(insertionIndexPath: NSIndexPath; reuseIdentifier: NSString; rowHeight: CGFloat): Pointer; cdecl;
    procedure setCellUpdateHandler(cellUpdateHandler: TUITableViewPlaceholderBlockMethod2); cdecl;
  end;
  TUITableViewPlaceholder = class(TOCGenericImport<UITableViewPlaceholderClass, UITableViewPlaceholder>) end;

  UITableViewDropPlaceholderClass = interface(UITableViewPlaceholderClass)
    ['{8AC418BE-4E17-4966-BE7B-9D7E736E41FF}']
  end;

  UITableViewDropPlaceholder = interface(UITableViewPlaceholder)
    ['{74FA11CC-A51B-421A-89CC-0229F7DCD8E9}']
    function previewParametersProvider: TUITableViewDropPlaceholderBlockMethod1; cdecl;
    procedure setPreviewParametersProvider(previewParametersProvider: TUITableViewDropPlaceholderBlockMethod2); cdecl;
  end;
  TUITableViewDropPlaceholder = class(TOCGenericImport<UITableViewDropPlaceholderClass, UITableViewDropPlaceholder>) end;

  UITableViewDropItem = interface(IObjectiveC)
    ['{38008A66-5D33-403D-A86F-B4E3A521FC62}']
    function dragItem: UIDragItem; cdecl;
    function previewSize: CGSize; cdecl;
    function sourceIndexPath: NSIndexPath; cdecl;
  end;

  UITableViewDropPlaceholderContext = interface(IObjectiveC)
    ['{370464A9-B989-4231-BB61-1F05F9DF482C}']
    function commitInsertionWithDataSourceUpdates(dataSourceUpdates: TUITableViewDropPlaceholderContextBlockMethod1): Boolean; cdecl;
    function deletePlaceholder: Boolean; cdecl;
    function dragItem: UIDragItem; cdecl;
  end;

(*
  NSDiffableDataSourceSnapshotClass = interface(NSObjectClass)
    ['{F384574D-745E-4ADF-B482-430354953CB2}']
  end;

  NSDiffableDataSourceSnapshot = interface(NSObject)
    ['{D98D423D-DE0F-440C-8ED6-324FA7D51908}']
    procedure appendItemsWithIdentifiers(identifiers: NSArray; intoSectionWithIdentifier: SectionIdentifierType); overload; cdecl;
    procedure appendItemsWithIdentifiers(identifiers: NSArray); overload; cdecl;
    procedure appendSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    procedure deleteAllItems; cdecl;
    procedure deleteItemsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure deleteSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    function indexOfItemIdentifier(itemIdentifier: ItemIdentifierType): NSInteger; cdecl;
    function indexOfSectionIdentifier(sectionIdentifier: SectionIdentifierType): NSInteger; cdecl;
    [MethodName('insertItemsWithIdentifiers:afterItemWithIdentifier:')]
    procedure insertItemsWithIdentifiersAfterItemWithIdentifier(identifiers: NSArray; afterItemWithIdentifier: ItemIdentifierType); cdecl;
    [MethodName('insertItemsWithIdentifiers:beforeItemWithIdentifier:')]
    procedure insertItemsWithIdentifiersBeforeItemWithIdentifier(identifiers: NSArray; beforeItemWithIdentifier: ItemIdentifierType); cdecl;
    [MethodName('insertSectionsWithIdentifiers:afterSectionWithIdentifier:')]
    procedure insertSectionsWithIdentifiersAfterSectionWithIdentifier(sectionIdentifiers: NSArray; afterSectionWithIdentifier: SectionIdentifierType); cdecl;
    [MethodName('insertSectionsWithIdentifiers:beforeSectionWithIdentifier:')]
    procedure insertSectionsWithIdentifiersBeforeSectionWithIdentifier(sectionIdentifiers: NSArray; beforeSectionWithIdentifier: SectionIdentifierType); cdecl;
    function itemIdentifiers: NSArray; cdecl;
    function itemIdentifiersInSectionWithIdentifier(sectionIdentifier: SectionIdentifierType): NSArray; cdecl;
    [MethodName('moveItemWithIdentifier:afterItemWithIdentifier:')]
    procedure moveItemWithIdentifierAfterItemWithIdentifier(fromIdentifier: ItemIdentifierType; afterItemWithIdentifier: ItemIdentifierType); cdecl;
    [MethodName('moveItemWithIdentifier:beforeItemWithIdentifier:')]
    procedure moveItemWithIdentifierBeforeItemWithIdentifier(fromIdentifier: ItemIdentifierType; beforeItemWithIdentifier: ItemIdentifierType); cdecl;
    [MethodName('moveSectionWithIdentifier:afterSectionWithIdentifier:')]
    procedure moveSectionWithIdentifierAfterSectionWithIdentifier(fromSectionIdentifier: SectionIdentifierType; afterSectionWithIdentifier: SectionIdentifierType); cdecl;
    [MethodName('moveSectionWithIdentifier:beforeSectionWithIdentifier:')]
    procedure moveSectionWithIdentifierBeforeSectionWithIdentifier(fromSectionIdentifier: SectionIdentifierType; beforeSectionWithIdentifier: SectionIdentifierType); cdecl;
    function numberOfItems: NSInteger; cdecl;
    function numberOfItemsInSection(sectionIdentifier: SectionIdentifierType): NSInteger; cdecl;
    function numberOfSections: NSInteger; cdecl;
    procedure reloadItemsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure reloadSectionsWithIdentifiers(sectionIdentifiers: NSArray); cdecl;
    function sectionIdentifierForSectionContainingItemIdentifier(itemIdentifier: ItemIdentifierType): SectionIdentifierType; cdecl;
    function sectionIdentifiers: NSArray; cdecl;
  end;
  TNSDiffableDataSourceSnapshot = class(TOCGenericImport<NSDiffableDataSourceSnapshotClass, NSDiffableDataSourceSnapshot>) end;

  NSDiffableDataSourceSectionTransactionClass = interface(NSObjectClass)
    ['{A54ED13A-7C99-415E-88C8-04DD8A2561D2}']
  end;

  NSDiffableDataSourceSectionTransaction = interface(NSObject)
    ['{FE05579A-B933-48B0-A33C-48F4FE613E99}']
    function difference: NSOrderedCollectionDifference; cdecl;
    function finalSnapshot: NSDiffableDataSourceSectionSnapshot; cdecl;
    function initialSnapshot: NSDiffableDataSourceSectionSnapshot; cdecl;
    function sectionIdentifier: SectionIdentifierType; cdecl;
  end;
  TNSDiffableDataSourceSectionTransaction = class(TOCGenericImport<NSDiffableDataSourceSectionTransactionClass, NSDiffableDataSourceSectionTransaction>) end;

  NSDiffableDataSourceTransactionClass = interface(NSObjectClass)
    ['{016C8204-A2F8-4C06-8ACC-D6D5DEFF1B2F}']
  end;

  NSDiffableDataSourceTransaction = interface(NSObject)
    ['{4D2A76B2-8339-47FA-B1DD-7DB16EF4F417}']
    function difference: NSOrderedCollectionDifference; cdecl;
    function finalSnapshot: NSDiffableDataSourceSnapshot; cdecl;
    function initialSnapshot: NSDiffableDataSourceSnapshot; cdecl;
    function sectionTransactions: NSArray; cdecl;
  end;
  TNSDiffableDataSourceTransaction = class(TOCGenericImport<NSDiffableDataSourceTransactionClass, NSDiffableDataSourceTransaction>) end;

  UICollectionViewDiffableDataSourceReorderingHandlersClass = interface(NSObjectClass)
    ['{1340B794-8813-464D-9651-90DF664FB148}']
  end;

  UICollectionViewDiffableDataSourceReorderingHandlers = interface(NSObject)
    ['{0E0BAD6A-9926-47A7-BD93-251CDBA0E3EB}']
    function canReorderItemHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod1; cdecl;
    function didReorderHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod3; cdecl;
    procedure setCanReorderItemHandler(canReorderItemHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod2); cdecl;
    procedure setDidReorderHandler(didReorderHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod2); cdecl;
    procedure setWillReorderHandler(willReorderHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod2); cdecl;
    function willReorderHandler: TUICollectionViewDiffableDataSourceReorderingHandlersBlockMethod3; cdecl;
  end;
  TUICollectionViewDiffableDataSourceReorderingHandlers = class(TOCGenericImport<UICollectionViewDiffableDataSourceReorderingHandlersClass, UICollectionViewDiffableDataSourceReorderingHandlers>) end;

  UICollectionViewDiffableDataSourceSectionSnapshotHandlersClass = interface(NSObjectClass)
    ['{C3C70FF1-EE19-4B56-99C7-C431ED90997C}']
  end;

  UICollectionViewDiffableDataSourceSectionSnapshotHandlers = interface(NSObject)
    ['{E54B1310-75F4-4DC4-BBF8-BEB41098131C}']
    procedure setShouldCollapseItemHandler(shouldCollapseItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod2); cdecl;
    procedure setShouldExpandItemHandler(shouldExpandItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod2); cdecl;
    procedure setSnapshotForExpandingParentItemHandler(snapshotForExpandingParentItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod2); cdecl;
    procedure setWillCollapseItemHandler(willCollapseItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod2); cdecl;
    procedure setWillExpandItemHandler(willExpandItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod2); cdecl;
    function shouldCollapseItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod1; cdecl;
    function shouldExpandItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod1; cdecl;
    function snapshotForExpandingParentItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod6; cdecl;
    function willCollapseItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod3; cdecl;
    function willExpandItemHandler: TUICollectionViewDiffableDataSourceSectionSnapshotHandlersBlockMethod3; cdecl;
  end;
  TUICollectionViewDiffableDataSourceSectionSnapshotHandlers = class(TOCGenericImport<UICollectionViewDiffableDataSourceSectionSnapshotHandlersClass, UICollectionViewDiffableDataSourceSectionSnapshotHandlers>) end;

  UICollectionViewDiffableDataSourceClass = interface(NSObjectClass)
    ['{A0071539-A70F-4522-B55F-661C427F465B}']
    {class} function new: Pointer; cdecl;
  end;

  UICollectionViewDiffableDataSource = interface(NSObject)
    ['{F64D7C21-84B8-42FF-BB80-3090BFB8C97F}']
    procedure applySnapshot(snapshot: NSDiffableDataSourceSnapshot; animatingDifferences: Boolean; completion: TUICollectionViewDiffableDataSourceBlockMethod1); overload; cdecl;
    procedure applySnapshot(snapshot: NSDiffableDataSourceSectionSnapshot; toSection: SectionIdentifierType; animatingDifferences: Boolean); overload; cdecl;
    procedure applySnapshot(snapshot: NSDiffableDataSourceSectionSnapshot; toSection: SectionIdentifierType; animatingDifferences: Boolean; completion: TUICollectionViewDiffableDataSourceBlockMethod1); overload; cdecl;
    procedure applySnapshot(snapshot: NSDiffableDataSourceSnapshot; animatingDifferences: Boolean); overload; cdecl;
    function indexPathForItemIdentifier(identifier: ItemIdentifierType): NSIndexPath; cdecl;
    function initWithCollectionView(collectionView: UICollectionView; cellProvider: UICollectionViewDiffableDataSourceCellProvider): Pointer; cdecl;
    function itemIdentifierForIndexPath(indexPath: NSIndexPath): ItemIdentifierType; cdecl;
    function reorderingHandlers: UICollectionViewDiffableDataSourceReorderingHandlers; cdecl;
    function sectionSnapshotHandlers: UICollectionViewDiffableDataSourceSectionSnapshotHandlers; cdecl;
    procedure setReorderingHandlers(reorderingHandlers: UICollectionViewDiffableDataSourceReorderingHandlers); cdecl;
    procedure setSectionSnapshotHandlers(sectionSnapshotHandlers: UICollectionViewDiffableDataSourceSectionSnapshotHandlers); cdecl;
    procedure setSupplementaryViewProvider(supplementaryViewProvider: UICollectionViewDiffableDataSourceSupplementaryViewProvider); cdecl;
    function snapshot: NSDiffableDataSourceSnapshot; cdecl;
    function snapshotForSection(section: SectionIdentifierType): NSDiffableDataSourceSectionSnapshot; cdecl;
    function supplementaryViewProvider: UICollectionViewDiffableDataSourceSupplementaryViewProvider; cdecl;
  end;
  TUICollectionViewDiffableDataSource = class(TOCGenericImport<UICollectionViewDiffableDataSourceClass, UICollectionViewDiffableDataSource>) end;

  UITableViewDiffableDataSourceClass = interface(NSObjectClass)
    ['{98D2850D-6B7D-4A63-A4DC-ED8FF4C3F327}']
    {class} function new: Pointer; cdecl;
  end;

  UITableViewDiffableDataSource = interface(NSObject)
    ['{ED0909BB-33A3-499A-9C5E-EE4DC53A63D8}']
    procedure applySnapshot(snapshot: NSDiffableDataSourceSnapshot; animatingDifferences: Boolean; completion: TUITableViewDiffableDataSourceBlockMethod1); overload; cdecl;
    procedure applySnapshot(snapshot: NSDiffableDataSourceSnapshot; animatingDifferences: Boolean); overload; cdecl;
    function defaultRowAnimation: UITableViewRowAnimation; cdecl;
    function indexPathForItemIdentifier(identifier: ItemIdentifierType): NSIndexPath; cdecl;
    function initWithTableView(tableView: UITableView; cellProvider: UITableViewDiffableDataSourceCellProvider): Pointer; cdecl;
    function itemIdentifierForIndexPath(indexPath: NSIndexPath): ItemIdentifierType; cdecl;
    procedure setDefaultRowAnimation(defaultRowAnimation: UITableViewRowAnimation); cdecl;
    function snapshot: NSDiffableDataSourceSnapshot; cdecl;
  end;
  TUITableViewDiffableDataSource = class(TOCGenericImport<UITableViewDiffableDataSourceClass, UITableViewDiffableDataSource>) end;

*)
  UIDocumentPickerDelegate = interface(IObjectiveC)
    ['{4F94C432-A848-4294-A617-F79304AE0B09}']
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentsAtURLs: NSArray); overload; cdecl;
    procedure documentPicker(controller: UIDocumentPickerViewController; didPickDocumentAtURL: NSURL); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("documentPicker:didPickDocumentsAtURLs:", ios(8.0, 11.0))
    procedure documentPickerWasCancelled(controller: UIDocumentPickerViewController); cdecl;
  end;

  UIDocumentPickerViewControllerClass = interface(UIViewControllerClass)
    ['{B60505B9-62AF-482B-B06B-B20F8651E6EE}']
  end;

  UIDocumentPickerViewController = interface(UIViewController)
    ['{A011C35F-028C-4713-A02A-BC6725C866A8}']
    function allowsMultipleSelection: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function directoryURL: NSURL; cdecl;
    function documentPickerMode: UIDocumentPickerMode; cdecl; // API_DEPRECATED("Use appropriate initializers instead",ios(8.0,14.0))
    function initForExportingURLs(urls: NSArray; asCopy: Boolean): Pointer; overload; cdecl;
    function initForExportingURLs(urls: NSArray): Pointer; overload; cdecl;
    function initForOpeningContentTypes(contentTypes: NSArray): Pointer; overload; cdecl;
    function initForOpeningContentTypes(contentTypes: NSArray; asCopy: Boolean): Pointer; overload; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithDocumentTypes(allowedUTIs: NSArray; inMode: UIDocumentPickerMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("use initForOpeningContentTypes:asCopy: or initForOpeningContentTypes: instead", ios(8.0, 14.0))
    function initWithURL(url: NSURL; inMode: UIDocumentPickerMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("use initForExportingURLs:asCopy: or initForExportingURLs: instead", ios(8.0, 14.0))
    function initWithURLs(urls: NSArray; inMode: UIDocumentPickerMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("use initForExportinitForExportingURLsingURLs:asCopy: or initForExportingURLs: instead", ios(11.0, 14.0))
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDirectoryURL(directoryURL: NSURL); cdecl;
    procedure setShouldShowFileExtensions(shouldShowFileExtensions: Boolean); cdecl;
    function shouldShowFileExtensions: Boolean; cdecl;
  end;
  TUIDocumentPickerViewController = class(TOCGenericImport<UIDocumentPickerViewControllerClass, UIDocumentPickerViewController>) end;

  UIDocumentMenuDelegate = interface(IObjectiveC)
    ['{3AB3B8F9-FA0B-492C-969D-AA59A45AD78D}']
    procedure documentMenu(documentMenu: UIDocumentMenuViewController; didPickDocumentPicker: UIDocumentPickerViewController); cdecl;
    procedure documentMenuWasCancelled(documentMenu: UIDocumentMenuViewController); cdecl;
  end;

  UIDocumentMenuViewControllerClass = interface(UIViewControllerClass)
    ['{1C79E97F-5F3A-4C66-BBE9-63B99D59AC41}']
  end;

  UIDocumentMenuViewController = interface(UIViewController)
    ['{DD4C8989-9AC5-4196-9BAF-47948012A82F}']
    procedure addOptionWithTitle(title: NSString; image: UIImage; order: UIDocumentMenuOrder; handler: TUIDocumentMenuViewControllerBlockMethod1); cdecl;
    function delegate: Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithDocumentTypes(allowedUTIs: NSArray; inMode: UIDocumentPickerMode): Pointer; cdecl;
    function initWithURL(url: NSURL; inMode: UIDocumentPickerMode): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TUIDocumentMenuViewController = class(TOCGenericImport<UIDocumentMenuViewControllerClass, UIDocumentMenuViewController>) end;

  UIDocumentPickerExtensionViewControllerClass = interface(UIViewControllerClass)
    ['{C99D1FE4-174D-4882-8128-FA3560883B74}']
  end;

  UIDocumentPickerExtensionViewController = interface(UIViewController)
    ['{C064225F-2DF2-4781-A80F-2725E8CA9444}']
    procedure dismissGrantingAccessToURL(url: NSURL); cdecl;
    function documentPickerMode: UIDocumentPickerMode; cdecl;
    function documentStorageURL: NSURL; cdecl;
    function originalURL: NSURL; cdecl;
    procedure prepareForPresentationInMode(mode: UIDocumentPickerMode); cdecl;
    function providerIdentifier: NSString; cdecl;
    function validTypes: NSArray; cdecl;
  end;
  TUIDocumentPickerExtensionViewController = class(TOCGenericImport<UIDocumentPickerExtensionViewControllerClass, UIDocumentPickerExtensionViewController>) end;

  UIDragItemClass = interface(NSObjectClass)
    ['{ED5DAFF9-2385-45DE-ABA3-4574FD67C0D6}']
    {class} function new: Pointer; cdecl;
  end;

  UIDragItem = interface(NSObject)
    ['{AF05584C-95E8-4D00-89C2-25EABE90BE54}']
    // Foundation function initWithItemProvider(itemProvider: NSItemProvider): Pointer; cdecl;
    // Foundation function itemProvider: NSItemProvider; cdecl;
    function localObject: Pointer; cdecl;
    function previewProvider: TUIDragItemBlockMethod1; cdecl;
    procedure setLocalObject(localObject: Pointer); cdecl;
    procedure setPreviewProvider(previewProvider: TUIDragItemBlockMethod2); cdecl;
  end;
  TUIDragItem = class(TOCGenericImport<UIDragItemClass, UIDragItem>) end;

  UIDragPreviewClass = interface(NSObjectClass)
    ['{600400E5-0101-4484-B9B5-61964013662A}']
    {class} function new: Pointer; cdecl;
    {class} function previewForURL(url: NSURL; title: NSString): Pointer; overload; cdecl;
    {class} function previewForURL(url: NSURL): Pointer; overload; cdecl;
  end;

  UIDragPreview = interface(NSObject)
    ['{34DE1A31-E7BB-4E1C-9A9C-E4DFE744522C}']
    function initWithView(view: UIView): Pointer; overload; cdecl;
    function initWithView(view: UIView; parameters: UIDragPreviewParameters): Pointer; overload; cdecl;
    function parameters: UIDragPreviewParameters; cdecl;
    function view: UIView; cdecl;
  end;
  TUIDragPreview = class(TOCGenericImport<UIDragPreviewClass, UIDragPreview>) end;

  UIPreviewParametersClass = interface(NSObjectClass)
    ['{D149E2A7-6354-4181-8D5C-BEEC86B5A24A}']
  end;

  UIPreviewParameters = interface(NSObject)
    ['{E74EEC4A-6F18-4BC7-B223-2DDA2B783010}']
    function backgroundColor: UIColor; cdecl;
    function initWithTextLineRects(textLineRects: NSArray): Pointer; cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setShadowPath(shadowPath: UIBezierPath); cdecl;
    procedure setVisiblePath(visiblePath: UIBezierPath); cdecl;
    function shadowPath: UIBezierPath; cdecl;
    function visiblePath: UIBezierPath; cdecl;
  end;
  TUIPreviewParameters = class(TOCGenericImport<UIPreviewParametersClass, UIPreviewParameters>) end;

  UIDragPreviewParametersClass = interface(UIPreviewParametersClass)
    ['{796A647C-40F7-4037-AFB1-10E478EC1911}']
  end;

  UIDragPreviewParameters = interface(UIPreviewParameters)
    ['{277024D1-2657-4207-AB21-E92D19D8DF42}']
  end;
  TUIDragPreviewParameters = class(TOCGenericImport<UIDragPreviewParametersClass, UIDragPreviewParameters>) end;

  UIDragDropSession = interface(IObjectiveC)
    ['{255C689D-B74F-4F37-9C3C-0469B3B9645B}']
    function allowsMoveOperation: Boolean; cdecl;
    function canLoadObjectsOfClass(aClass: Pointer): Boolean; cdecl;
    function hasItemsConformingToTypeIdentifiers(typeIdentifiers: NSArray): Boolean; cdecl;
    function isRestrictedToDraggingApplication: Boolean; cdecl;
    function items: NSArray; cdecl;
    function locationInView(view: UIView): CGPoint; cdecl;
  end;

  UIDragSession = interface(IObjectiveC)
    ['{AF6E08F0-4695-4293-BA6A-867D1BA98CCD}']
    function localContext: Pointer; cdecl;
    procedure setLocalContext(localContext: Pointer); cdecl;
  end;

  UIDropSession = interface(IObjectiveC)
    ['{6C386E9C-C2D7-4FCE-A172-41EAB1D29AE0}']
    function loadObjectsOfClass(aClass: Pointer; completion: TUIDropSessionBlockMethod1): NSProgress; cdecl;
    function localDragSession: Pointer; cdecl;
    function progressIndicatorStyle: UIDropSessionProgressIndicatorStyle; cdecl;
    procedure setProgressIndicatorStyle(progressIndicatorStyle: UIDropSessionProgressIndicatorStyle); cdecl;
  end;

  UIFeedbackGeneratorClass = interface(NSObjectClass)
    ['{5ABBD6EA-4AC1-4BBC-B62A-C8A871984CE7}']
  end;

  UIFeedbackGenerator = interface(NSObject)
    ['{A59C613B-943A-4B57-AB37-B41F99C8A8A8}']
    procedure prepare; cdecl;
  end;
  TUIFeedbackGenerator = class(TOCGenericImport<UIFeedbackGeneratorClass, UIFeedbackGenerator>) end;

  UIFieldBehaviorClass = interface(UIDynamicBehaviorClass)
    ['{097FF08E-4E4F-46C2-99F8-A198F58D871E}']
    {class} function dragField: Pointer; cdecl;
    {class} function electricField: Pointer; cdecl;
    {class} function fieldWithEvaluationBlock(block: TUIFieldBehaviorBlockMethod1): Pointer; cdecl;
    {class} function linearGravityFieldWithVector(direction: CGVector): Pointer; cdecl;
    {class} function magneticField: Pointer; cdecl;
    {class} function noiseFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): Pointer; cdecl;
    {class} function radialGravityFieldWithPosition(position: CGPoint): Pointer; cdecl;
    {class} function springField: Pointer; cdecl;
    {class} function turbulenceFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): Pointer; cdecl;
    {class} function velocityFieldWithVector(direction: CGVector): Pointer; cdecl;
    {class} function vortexField: Pointer; cdecl;
  end;

  UIFieldBehavior = interface(UIDynamicBehavior)
    ['{3D193DEE-C756-4455-BA3B-4D20BD9324E5}']
    procedure addItem(item: Pointer); cdecl;
    function animationSpeed: CGFloat; cdecl;
    function direction: CGVector; cdecl;
    function falloff: CGFloat; cdecl;
    function items: NSArray; cdecl;
    function minimumRadius: CGFloat; cdecl;
    function position: CGPoint; cdecl;
    function region: UIRegion; cdecl;
    procedure removeItem(item: Pointer); cdecl;
    procedure setAnimationSpeed(animationSpeed: CGFloat); cdecl;
    procedure setDirection(direction: CGVector); cdecl;
    procedure setFalloff(falloff: CGFloat); cdecl;
    procedure setMinimumRadius(minimumRadius: CGFloat); cdecl;
    procedure setPosition(position: CGPoint); cdecl;
    procedure setRegion(region: UIRegion); cdecl;
    procedure setSmoothness(smoothness: CGFloat); cdecl;
    procedure setStrength(strength: CGFloat); cdecl;
    function smoothness: CGFloat; cdecl;
    function strength: CGFloat; cdecl;
  end;
  TUIFieldBehavior = class(TOCGenericImport<UIFieldBehaviorClass, UIFieldBehavior>) end;

  UIFocusDebuggerClass = interface(NSObjectClass)
    ['{89ACE262-B6CA-4891-A377-ACBC0C10F2FA}']
    {class} function checkFocusabilityForItem(item: Pointer): Pointer; cdecl;
    {class} function help: Pointer; cdecl;
    {class} function simulateFocusUpdateRequestFromEnvironment(environment: Pointer): Pointer; cdecl;
    {class} function status: Pointer; cdecl;
  end;

  UIFocusDebugger = interface(NSObject)
    ['{1B4FE090-5283-4BD4-B915-65C12097ADCC}']
  end;
  TUIFocusDebugger = class(TOCGenericImport<UIFocusDebuggerClass, UIFocusDebugger>) end;

  UIFocusDebuggerOutput = interface(IObjectiveC)
    ['{83414711-3568-40C2-94D0-18E584C69F2F}']
  end;

  UIFocusMovementHintClass = interface(NSObjectClass)
    ['{BA50EEFE-3BB7-4F98-B05B-5ED6209FF2FE}']
    {class} function new: Pointer; cdecl;
  end;

  UIFocusMovementHint = interface(NSObject)
    ['{62CB899F-63C6-4474-A8FC-B9D9AA847D69}']
    function interactionTransform: CATransform3D; cdecl;
    function movementDirection: CGVector; cdecl;
    function perspectiveTransform: CATransform3D; cdecl;
    function rotation: CGVector; cdecl;
    function translation: CGVector; cdecl;
  end;
  TUIFocusMovementHint = class(TOCGenericImport<UIFocusMovementHintClass, UIFocusMovementHint>) end;

  UIFocusSystemClass = interface(NSObjectClass)
    ['{361F4264-A6BE-4FBA-AF1B-5D1957301026}']
    {class} function environment(environment: Pointer; containsEnvironment: Pointer): Boolean; cdecl;
    {class} function focusSystemForEnvironment(environment: Pointer): UIFocusSystem; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure registerURL(soundFileURL: NSURL; forSoundIdentifier: UIFocusSoundIdentifier); cdecl;
  end;

  UIFocusSystem = interface(NSObject)
    ['{62394A47-D933-4834-945A-9D0A5A1C2249}']
    function focusedItem: Pointer; cdecl;
    procedure requestFocusUpdateToEnvironment(environment: Pointer); cdecl;
    procedure updateFocusIfNeeded; cdecl;
  end;
  TUIFocusSystem = class(TOCGenericImport<UIFocusSystemClass, UIFocusSystem>) end;

  UIFontMetricsClass = interface(NSObjectClass)
    ['{C2385852-96CE-4F7C-900B-E71AD6B566F2}']
    {class} function defaultMetrics: UIFontMetrics; cdecl;
    {class} function metricsForTextStyle(textStyle: UIFontTextStyle): Pointer; cdecl;
  end;

  UIFontMetrics = interface(NSObject)
    ['{069A47AA-888C-408D-BA3D-041A80B44556}']
    function initForTextStyle(textStyle: UIFontTextStyle): Pointer; cdecl;
    function scaledFontForFont(font: UIFont; compatibleWithTraitCollection: UITraitCollection): UIFont; overload; cdecl;
    function scaledFontForFont(font: UIFont; maximumPointSize: CGFloat; compatibleWithTraitCollection: UITraitCollection): UIFont; overload; cdecl;
    function scaledFontForFont(font: UIFont): UIFont; overload; cdecl;
    function scaledFontForFont(font: UIFont; maximumPointSize: CGFloat): UIFont; overload; cdecl;
    function scaledValueForValue(value: CGFloat; compatibleWithTraitCollection: UITraitCollection): CGFloat; overload; cdecl;
    function scaledValueForValue(value: CGFloat): CGFloat; overload; cdecl;
  end;
  TUIFontMetrics = class(TOCGenericImport<UIFontMetricsClass, UIFontMetrics>) end;

  UIFontPickerViewControllerConfigurationClass = interface(NSObjectClass)
    ['{102D2552-0E78-4222-90AC-A7D52ADFC2E1}']
    {class} function filterPredicateForFilteredLanguages(filteredLanguages: NSArray): NSPredicate; cdecl;
  end;

  UIFontPickerViewControllerConfiguration = interface(NSObject)
    ['{C835A62C-7209-4AA8-B7E6-BA7D043393EF}']
    function displayUsingSystemFont: Boolean; cdecl;
    function filteredLanguagesPredicate: NSPredicate; cdecl;
    function filteredTraits: UIFontDescriptorSymbolicTraits; cdecl;
    function includeFaces: Boolean; cdecl;
    procedure setDisplayUsingSystemFont(displayUsingSystemFont: Boolean); cdecl;
    procedure setFilteredLanguagesPredicate(filteredLanguagesPredicate: NSPredicate); cdecl;
    procedure setFilteredTraits(filteredTraits: UIFontDescriptorSymbolicTraits); cdecl;
    procedure setIncludeFaces(includeFaces: Boolean); cdecl;
  end;
  TUIFontPickerViewControllerConfiguration = class(TOCGenericImport<UIFontPickerViewControllerConfigurationClass, UIFontPickerViewControllerConfiguration>) end;

  UIFontPickerViewControllerDelegate = interface(IObjectiveC)
    ['{8A9831A7-CBEA-4FAA-A37F-2A4A26CBF8BD}']
    procedure fontPickerViewControllerDidCancel(viewController: UIFontPickerViewController); cdecl;
    procedure fontPickerViewControllerDidPickFont(viewController: UIFontPickerViewController); cdecl;
  end;

  UIFontPickerViewControllerClass = interface(UIViewControllerClass)
    ['{34A4A795-0EB8-443E-B524-892DF874BB83}']
  end;

  UIFontPickerViewController = interface(UIViewController)
    ['{5132F685-76BB-4442-90FD-11E304EBF84C}']
    function configuration: UIFontPickerViewControllerConfiguration; cdecl;
    function delegate: Pointer; cdecl;
    function initWithConfiguration(configuration: UIFontPickerViewControllerConfiguration): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    function selectedFontDescriptor: UIFontDescriptor; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setSelectedFontDescriptor(selectedFontDescriptor: UIFontDescriptor); cdecl;
  end;
  TUIFontPickerViewController = class(TOCGenericImport<UIFontPickerViewControllerClass, UIFontPickerViewController>) end;

  UIGraphicsRendererFormatClass = interface(NSObjectClass)
    ['{E806D5B8-82F5-4730-A10C-F46BCDB7AE41}']
    {class} function defaultFormat: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredFormat", tvos(10.0, 11.0))
    {class} function preferredFormat: Pointer; cdecl;
  end;

  UIGraphicsRendererFormat = interface(NSObject)
    ['{BEBC5491-7326-4F23-950A-81E60ECEF61C}']
    function bounds: CGRect; cdecl;
  end;
  TUIGraphicsRendererFormat = class(TOCGenericImport<UIGraphicsRendererFormatClass, UIGraphicsRendererFormat>) end;

  UIGraphicsRendererContextClass = interface(NSObjectClass)
    ['{2CDBF740-63FE-4D22-882B-674A106BB8D8}']
  end;

  UIGraphicsRendererContext = interface(NSObject)
    ['{59A7044A-4158-476C-9789-190C894EBE4A}']
    function CGContext: CGContextRef; cdecl;
    procedure clipToRect(rect: CGRect); cdecl;
    procedure fillRect(rect: CGRect; blendMode: CGBlendMode); overload; cdecl;
    procedure fillRect(rect: CGRect); overload; cdecl;
    function format: UIGraphicsRendererFormat; cdecl;
    procedure strokeRect(rect: CGRect; blendMode: CGBlendMode); overload; cdecl;
    procedure strokeRect(rect: CGRect); overload; cdecl;
  end;
  TUIGraphicsRendererContext = class(TOCGenericImport<UIGraphicsRendererContextClass, UIGraphicsRendererContext>) end;

  UIGraphicsRendererClass = interface(NSObjectClass)
    ['{5AF797CF-74E7-40B7-9B33-EB08CF86C154}']
    {class} function contextWithFormat(format: UIGraphicsRendererFormat): CGContextRef; cdecl;
    {class} procedure prepareCGContext(context: CGContextRef; withRendererContext: UIGraphicsRendererContext); cdecl;
    {class} function rendererContextClass: Pointer; cdecl;
  end;

  UIGraphicsRenderer = interface(NSObject)
    ['{F7688B9B-C617-4C64-811C-3E8B5F45CFE7}']
    function allowsImageOutput: Boolean; cdecl;
    function format: UIGraphicsRendererFormat; cdecl;
    function initWithBounds(bounds: CGRect): Pointer; overload; cdecl;
    function initWithBounds(bounds: CGRect; format: UIGraphicsRendererFormat): Pointer; overload; cdecl;
    function runDrawingActions(drawingActions: UIGraphicsDrawingActions; completionActions: UIGraphicsDrawingActions; error: PPointer): Boolean; cdecl;
  end;
  TUIGraphicsRenderer = class(TOCGenericImport<UIGraphicsRendererClass, UIGraphicsRenderer>) end;

  UIGraphicsImageRendererFormatClass = interface(UIGraphicsRendererFormatClass)
    ['{B205686B-BB9A-48FF-8B21-42F55D0B164A}']
    {class} function formatForTraitCollection(traitCollection: UITraitCollection): Pointer; cdecl;
  end;

  UIGraphicsImageRendererFormat = interface(UIGraphicsRendererFormat)
    ['{3B7951E9-7A37-4313-9769-03F2C16AC4CA}']
    function opaque: Boolean; cdecl;
    function preferredRange: UIGraphicsImageRendererFormatRange; cdecl;
    function prefersExtendedRange: Boolean; cdecl; // API_DEPRECATED("Use the preferredRange property instead", ios(10.0, 12.0))
    function scale: CGFloat; cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setPreferredRange(preferredRange: UIGraphicsImageRendererFormatRange); cdecl;
    procedure setPrefersExtendedRange(prefersExtendedRange: Boolean); cdecl; // API_DEPRECATED("Use the preferredRange property instead", ios(10.0, 12.0))
    procedure setScale(scale: CGFloat); cdecl;
  end;
  TUIGraphicsImageRendererFormat = class(TOCGenericImport<UIGraphicsImageRendererFormatClass, UIGraphicsImageRendererFormat>) end;

  UIGraphicsImageRendererContextClass = interface(UIGraphicsRendererContextClass)
    ['{636D08C5-12BE-4D2E-B887-723B054FA242}']
  end;

  UIGraphicsImageRendererContext = interface(UIGraphicsRendererContext)
    ['{DFC3CA6A-9BC9-41B3-B208-9E06CB3871B5}']
    function currentImage: UIImage; cdecl;
  end;
  TUIGraphicsImageRendererContext = class(TOCGenericImport<UIGraphicsImageRendererContextClass, UIGraphicsImageRendererContext>) end;

  UIGraphicsImageRendererClass = interface(UIGraphicsRendererClass)
    ['{FC8B1784-0E99-402E-9965-B10FD8C3C07C}']
  end;

  UIGraphicsImageRenderer = interface(UIGraphicsRenderer)
    ['{9256453F-6E47-4F22-88C0-E5AFD4E88378}']
    function imageWithActions(actions: UIGraphicsImageDrawingActions): UIImage; cdecl;
    function initWithBounds(bounds: CGRect; format: UIGraphicsImageRendererFormat): Pointer; cdecl;
    function initWithSize(size: CGSize): Pointer; overload; cdecl;
    function initWithSize(size: CGSize; format: UIGraphicsImageRendererFormat): Pointer; overload; cdecl;
    function JPEGDataWithCompressionQuality(compressionQuality: CGFloat; actions: UIGraphicsImageDrawingActions): NSData; cdecl;
    function PNGDataWithActions(actions: UIGraphicsImageDrawingActions): NSData; cdecl;
  end;
  TUIGraphicsImageRenderer = class(TOCGenericImport<UIGraphicsImageRendererClass, UIGraphicsImageRenderer>) end;

  UIGraphicsPDFRendererFormatClass = interface(UIGraphicsRendererFormatClass)
    ['{F1CF9D6B-8755-438E-BE07-DD0638475485}']
  end;

  UIGraphicsPDFRendererFormat = interface(UIGraphicsRendererFormat)
    ['{D51AC03B-6EF2-4397-85D5-76AC43D2E56B}']
    function documentInfo: NSDictionary; cdecl;
    procedure setDocumentInfo(documentInfo: NSDictionary); cdecl;
  end;
  TUIGraphicsPDFRendererFormat = class(TOCGenericImport<UIGraphicsPDFRendererFormatClass, UIGraphicsPDFRendererFormat>) end;

  UIGraphicsPDFRendererContextClass = interface(UIGraphicsRendererContextClass)
    ['{E09FEF9F-F5F8-4E8C-9360-D78FE922F423}']
  end;

  UIGraphicsPDFRendererContext = interface(UIGraphicsRendererContext)
    ['{9B4246C1-9471-496D-8226-81D9483D8B51}']
    procedure addDestinationWithName(name: NSString; atPoint: CGPoint); cdecl;
    procedure beginPage; cdecl;
    procedure beginPageWithBounds(bounds: CGRect; pageInfo: NSDictionary); cdecl;
    function pdfContextBounds: CGRect; cdecl;
    procedure setDestinationWithName(name: NSString; forRect: CGRect); cdecl;
    procedure setURL(url: NSURL; forRect: CGRect); cdecl;
  end;
  TUIGraphicsPDFRendererContext = class(TOCGenericImport<UIGraphicsPDFRendererContextClass, UIGraphicsPDFRendererContext>) end;

  UIGraphicsPDFRendererClass = interface(UIGraphicsRendererClass)
    ['{EFCBC85C-727C-4580-B967-F5446699777D}']
  end;

  UIGraphicsPDFRenderer = interface(UIGraphicsRenderer)
    ['{F2660ECE-D415-45A8-B545-F39870137408}']
    function initWithBounds(bounds: CGRect; format: UIGraphicsPDFRendererFormat): Pointer; cdecl;
    function PDFDataWithActions(actions: UIGraphicsPDFDrawingActions): NSData; cdecl;
    function writePDFToURL(url: NSURL; withActions: UIGraphicsPDFDrawingActions; error: PPointer): Boolean; cdecl;
  end;
  TUIGraphicsPDFRenderer = class(TOCGenericImport<UIGraphicsPDFRendererClass, UIGraphicsPDFRenderer>) end;

  UIHoverGestureRecognizerClass = interface(UIGestureRecognizerClass)
    ['{6B45B13E-4250-444E-BEF7-407C88672877}']
  end;

  UIHoverGestureRecognizer = interface(UIGestureRecognizer)
    ['{82F27FB5-FC38-47D0-B4B6-A6068E134EB4}']
  end;
  TUIHoverGestureRecognizer = class(TOCGenericImport<UIHoverGestureRecognizerClass, UIHoverGestureRecognizer>) end;

  UIImageAssetClass = interface(NSObjectClass)
    ['{D7CE8E44-D8B0-484A-A6BC-79455656F00B}']
  end;

  UIImageAsset = interface(NSObject)
    ['{F253C1B2-1F52-4486-93F7-D65FC0C1B7F2}']
    function imageWithConfiguration(configuration: UIImageConfiguration): UIImage; cdecl;
    function imageWithTraitCollection(traitCollection: UITraitCollection): UIImage; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    procedure registerImage(image: UIImage; withTraitCollection: UITraitCollection); overload; cdecl;
    procedure registerImage(image: UIImage; withConfiguration: UIImageConfiguration); overload; cdecl;
    procedure unregisterImageWithConfiguration(configuration: UIImageConfiguration); cdecl;
    procedure unregisterImageWithTraitCollection(traitCollection: UITraitCollection); cdecl;
  end;
  TUIImageAsset = class(TOCGenericImport<UIImageAssetClass, UIImageAsset>) end;

  UIImageConfigurationClass = interface(NSObjectClass)
    ['{BA214DB5-B624-4D09-9B70-BE26969C05D7}']
    {class} function new: Pointer; cdecl;
  end;

  UIImageConfiguration = interface(NSObject)
    ['{C373995C-7F7C-4B87-87FB-B2DA6CB305EE}']
    function configurationByApplyingConfiguration(otherConfiguration: UIImageConfiguration): Pointer; cdecl;
    function configurationWithTraitCollection(traitCollection: UITraitCollection): Pointer; cdecl;
    function traitCollection: UITraitCollection; cdecl;
  end;
  TUIImageConfiguration = class(TOCGenericImport<UIImageConfigurationClass, UIImageConfiguration>) end;

  UIImageSymbolConfigurationClass = interface(UIImageConfigurationClass)
    ['{E88A219A-AD45-41F4-BD6B-13BA7297C4BC}']
    {class} function configurationWithFont(font: UIFont): Pointer; overload; cdecl;
    {class} function configurationWithFont(font: UIFont; scale: UIImageSymbolScale): Pointer; overload; cdecl;
    {class} function configurationWithPointSize(pointSize: CGFloat; weight: UIImageSymbolWeight; scale: UIImageSymbolScale): Pointer; overload; cdecl;
    {class} function configurationWithPointSize(pointSize: CGFloat; weight: UIImageSymbolWeight): Pointer; overload; cdecl;
    {class} function configurationWithPointSize(pointSize: CGFloat): Pointer; overload; cdecl;
    {class} function configurationWithScale(scale: UIImageSymbolScale): Pointer; cdecl;
    {class} function configurationWithTextStyle(textStyle: UIFontTextStyle; scale: UIImageSymbolScale): Pointer; overload; cdecl;
    {class} function configurationWithTextStyle(textStyle: UIFontTextStyle): Pointer; overload; cdecl;
    {class} function configurationWithWeight(weight: UIImageSymbolWeight): Pointer; cdecl;
    {class} function unspecifiedConfiguration: UIImageSymbolConfiguration; cdecl;
  end;

  UIImageSymbolConfiguration = interface(UIImageConfiguration)
    ['{4115FE31-5D17-41EF-85FE-8D7213D76393}']
    function configurationWithoutPointSizeAndWeight: Pointer; cdecl;
    function configurationWithoutScale: Pointer; cdecl;
    function configurationWithoutTextStyle: Pointer; cdecl;
    function configurationWithoutWeight: Pointer; cdecl;
    function isEqualToConfiguration(otherConfiguration: UIImageSymbolConfiguration): Boolean; cdecl;
  end;
  TUIImageSymbolConfiguration = class(TOCGenericImport<UIImageSymbolConfigurationClass, UIImageSymbolConfiguration>) end;

  UIImpactFeedbackGeneratorClass = interface(UIFeedbackGeneratorClass)
    ['{E6209EB8-1C3E-4E90-87FA-52A2AF2986C6}']
  end;

  UIImpactFeedbackGenerator = interface(UIFeedbackGenerator)
    ['{F56279C0-654A-498A-9E70-50572793600A}']
    procedure impactOccurred; cdecl;
    procedure impactOccurredWithIntensity(intensity: CGFloat); cdecl;
    function initWithStyle(style: UIImpactFeedbackStyle): Pointer; cdecl;
  end;
  TUIImpactFeedbackGenerator = class(TOCGenericImport<UIImpactFeedbackGeneratorClass, UIImpactFeedbackGenerator>) end;

  UIIndirectScribbleInteractionClass = interface(NSObjectClass)
    ['{AD8A0AC9-F468-4DFD-8BAB-66100B486DA3}']
    {class} function new: Pointer; cdecl;
  end;

  UIIndirectScribbleInteraction = interface(NSObject)
    ['{50DF758B-6F64-4FE7-BE4B-D9B480AA1E6B}']
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function isHandlingWriting: Boolean; cdecl;
  end;
  TUIIndirectScribbleInteraction = class(TOCGenericImport<UIIndirectScribbleInteractionClass, UIIndirectScribbleInteraction>) end;

  UIIndirectScribbleInteractionDelegate = interface(IObjectiveC)
    ['{BF4C7494-E3BE-48FA-99B3-84B5DD1B85E7}']
    [MethodName('indirectScribbleInteraction:didFinishWritingInElement:')]
    procedure indirectScribbleInteractionDidFinishWritingInElement(interaction: UIIndirectScribbleInteraction; didFinishWritingInElement: UIScribbleElementIdentifier); cdecl;
    [MethodName('indirectScribbleInteraction:focusElementIfNeeded:referencePoint:completion:')]
    procedure indirectScribbleInteractionFocusElementIfNeeded(interaction: UIIndirectScribbleInteraction; focusElementIfNeeded: UIScribbleElementIdentifier; referencePoint: CGPoint; completion: TUIIndirectScribbleInteractionDelegateBlockMethod2); cdecl;
    [MethodName('indirectScribbleInteraction:frameForElement:')]
    function indirectScribbleInteractionFrameForElement(interaction: UIIndirectScribbleInteraction; frameForElement: UIScribbleElementIdentifier): CGRect; cdecl;
    [MethodName('indirectScribbleInteraction:isElementFocused:')]
    function indirectScribbleInteractionIsElementFocused(interaction: UIIndirectScribbleInteraction; isElementFocused: UIScribbleElementIdentifier): Boolean; cdecl;
    [MethodName('indirectScribbleInteraction:requestElementsInRect:completion:')]
    procedure indirectScribbleInteractionRequestElementsInRect(interaction: UIIndirectScribbleInteraction; requestElementsInRect: CGRect; completion: TUIIndirectScribbleInteractionDelegateBlockMethod1); cdecl;
    [MethodName('indirectScribbleInteraction:shouldDelayFocusForElement:')]
    function indirectScribbleInteractionShouldDelayFocusForElement(interaction: UIIndirectScribbleInteraction; shouldDelayFocusForElement: UIScribbleElementIdentifier): Boolean; cdecl;
    [MethodName('indirectScribbleInteraction:willBeginWritingInElement:')]
    procedure indirectScribbleInteractionWillBeginWritingInElement(interaction: UIIndirectScribbleInteraction; willBeginWritingInElement: UIScribbleElementIdentifier); cdecl;
  end;

  UIInputViewControllerClass = interface(UIViewControllerClass)
    ['{AC4CD913-3712-4D5C-81B4-5D0EA1A4B979}']
  end;

  UIInputViewController = interface(UIViewController)
    ['{6D2F0353-E09A-4687-BBDD-30014199F187}']
    procedure advanceToNextInputMode; cdecl;
    procedure dismissKeyboard; cdecl;
    procedure handleInputModeListFromView(view: UIView; withEvent: UIEvent); cdecl;
    function hasDictationKey: Boolean; cdecl;
    function hasFullAccess: Boolean; cdecl;
    function inputView: UIInputView; cdecl;
    function needsInputModeSwitchKey: Boolean; cdecl;
    function primaryLanguage: NSString; cdecl;
    procedure requestSupplementaryLexiconWithCompletion(completionHandler: TUIInputViewControllerBlockMethod1); cdecl;
    procedure setHasDictationKey(hasDictationKey: Boolean); cdecl;
    procedure setInputView(inputView: UIInputView); cdecl;
    procedure setPrimaryLanguage(primaryLanguage: NSString); cdecl;
    function textDocumentProxy: Pointer; cdecl;
  end;
  TUIInputViewController = class(TOCGenericImport<UIInputViewControllerClass, UIInputViewController>) end;

  UIKeyClass = interface(NSObjectClass)
    ['{0ADC6ED7-403F-4B0B-8AD6-71AF33634A6B}']
  end;

  UIKey = interface(NSObject)
    ['{D0E4DFFC-C3BE-4964-8ADA-7F2E7BF549D0}']
    function characters: NSString; cdecl;
    function charactersIgnoringModifiers: NSString; cdecl;
    function keyCode: UIKeyboardHIDUsage; cdecl;
    function modifierFlags: UIKeyModifierFlags; cdecl;
  end;
  TUIKey = class(TOCGenericImport<UIKeyClass, UIKey>) end;

  UIListContentImagePropertiesClass = interface(NSObjectClass)
    ['{141B7EB1-928B-4855-B1E8-B10309DA0959}']
  end;

  UIListContentImageProperties = interface(NSObject)
    ['{FA5CCB6E-D6CE-4B65-95E2-5E75BDD8FF6A}']
    function accessibilityIgnoresInvertColors: Boolean; cdecl;
    function cornerRadius: CGFloat; cdecl;
    function maximumSize: CGSize; cdecl;
    function preferredSymbolConfiguration: UIImageSymbolConfiguration; cdecl;
    function reservedLayoutSize: CGSize; cdecl;
    function resolvedTintColorForTintColor(tintColor: UIColor): UIColor; cdecl;
    procedure setAccessibilityIgnoresInvertColors(accessibilityIgnoresInvertColors: Boolean); cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setMaximumSize(maximumSize: CGSize); cdecl;
    procedure setPreferredSymbolConfiguration(preferredSymbolConfiguration: UIImageSymbolConfiguration); cdecl;
    procedure setReservedLayoutSize(reservedLayoutSize: CGSize); cdecl;
    procedure setTintColor(tintColor: UIColor); cdecl;
    procedure setTintColorTransformer(tintColorTransformer: UIConfigurationColorTransformer); cdecl;
    function tintColor: UIColor; cdecl;
    function tintColorTransformer: UIConfigurationColorTransformer; cdecl;
  end;
  TUIListContentImageProperties = class(TOCGenericImport<UIListContentImagePropertiesClass, UIListContentImageProperties>) end;

  UIListContentTextPropertiesClass = interface(NSObjectClass)
    ['{9920BCDC-ECBF-41B5-BBBE-489FEE004165}']
  end;

  UIListContentTextProperties = interface(NSObject)
    ['{A0E5B4F3-F0B9-4762-BA6C-87AA872BE5BE}']
    function adjustsFontForContentSizeCategory: Boolean; cdecl;
    function adjustsFontSizeToFitWidth: Boolean; cdecl;
    function alignment: UIListContentTextAlignment; cdecl;
    function allowsDefaultTighteningForTruncation: Boolean; cdecl;
    function color: UIColor; cdecl;
    function colorTransformer: UIConfigurationColorTransformer; cdecl;
    function font: UIFont; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    function minimumScaleFactor: CGFloat; cdecl;
    function numberOfLines: NSInteger; cdecl;
    function resolvedColor: UIColor; cdecl;
    procedure setAdjustsFontForContentSizeCategory(adjustsFontForContentSizeCategory: Boolean); cdecl;
    procedure setAdjustsFontSizeToFitWidth(adjustsFontSizeToFitWidth: Boolean); cdecl;
    procedure setAlignment(alignment: UIListContentTextAlignment); cdecl;
    procedure setAllowsDefaultTighteningForTruncation(allowsDefaultTighteningForTruncation: Boolean); cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setColorTransformer(colorTransformer: UIConfigurationColorTransformer); cdecl;
    procedure setFont(font: UIFont); cdecl;
    procedure setLineBreakMode(lineBreakMode: NSLineBreakMode); cdecl;
    procedure setMinimumScaleFactor(minimumScaleFactor: CGFloat); cdecl;
    procedure setNumberOfLines(numberOfLines: NSInteger); cdecl;
    procedure setTransform(transform: UIListContentTextTransform); cdecl;
    function transform: UIListContentTextTransform; cdecl;
  end;
  TUIListContentTextProperties = class(TOCGenericImport<UIListContentTextPropertiesClass, UIListContentTextProperties>) end;

  UIListContentConfigurationClass = interface(NSObjectClass)
    ['{B0B0D587-CE14-4494-9831-902BC091BC42}']
    {class} function accompaniedSidebarCellConfiguration: Pointer; cdecl;
    {class} function accompaniedSidebarSubtitleCellConfiguration: Pointer; cdecl;
    {class} function cellConfiguration: Pointer; cdecl;
    {class} function groupedFooterConfiguration: Pointer; cdecl;
    {class} function groupedHeaderConfiguration: Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function plainFooterConfiguration: Pointer; cdecl;
    {class} function plainHeaderConfiguration: Pointer; cdecl;
    {class} function sidebarCellConfiguration: Pointer; cdecl;
    {class} function sidebarHeaderConfiguration: Pointer; cdecl;
    {class} function sidebarSubtitleCellConfiguration: Pointer; cdecl;
    {class} function subtitleCellConfiguration: Pointer; cdecl;
    {class} function valueCellConfiguration: Pointer; cdecl;
  end;

  UIListContentConfiguration = interface(NSObject)
    ['{39BB5D66-2967-4F8C-8B72-8CDD64D30BD3}']
    function attributedText: NSAttributedString; cdecl;
    function axesPreservingSuperviewLayoutMargins: UIAxis; cdecl;
    function directionalLayoutMargins: NSDirectionalEdgeInsets; cdecl;
    function image: UIImage; cdecl;
    function imageProperties: UIListContentImageProperties; cdecl;
    function imageToTextPadding: CGFloat; cdecl;
    function prefersSideBySideTextAndSecondaryText: Boolean; cdecl;
    function secondaryAttributedText: NSAttributedString; cdecl;
    function secondaryText: NSString; cdecl;
    function secondaryTextProperties: UIListContentTextProperties; cdecl;
    procedure setAttributedText(attributedText: NSAttributedString); cdecl;
    procedure setAxesPreservingSuperviewLayoutMargins(axesPreservingSuperviewLayoutMargins: UIAxis); cdecl;
    procedure setDirectionalLayoutMargins(directionalLayoutMargins: NSDirectionalEdgeInsets); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setImageToTextPadding(imageToTextPadding: CGFloat); cdecl;
    procedure setPrefersSideBySideTextAndSecondaryText(prefersSideBySideTextAndSecondaryText: Boolean); cdecl;
    procedure setSecondaryAttributedText(secondaryAttributedText: NSAttributedString); cdecl;
    procedure setSecondaryText(secondaryText: NSString); cdecl;
    procedure setText(text: NSString); cdecl;
    procedure setTextToSecondaryTextHorizontalPadding(textToSecondaryTextHorizontalPadding: CGFloat); cdecl;
    procedure setTextToSecondaryTextVerticalPadding(textToSecondaryTextVerticalPadding: CGFloat); cdecl;
    function text: NSString; cdecl;
    function textProperties: UIListContentTextProperties; cdecl;
    function textToSecondaryTextHorizontalPadding: CGFloat; cdecl;
    function textToSecondaryTextVerticalPadding: CGFloat; cdecl;
  end;
  TUIListContentConfiguration = class(TOCGenericImport<UIListContentConfigurationClass, UIListContentConfiguration>) end;

  UIListContentViewClass = interface(UIViewClass)
    ['{B0F27FB7-2F12-443F-BF80-1E894FF7DA88}']
    {class} function new: Pointer; cdecl;
  end;

  UIListContentView = interface(UIView)
    ['{DC88A4A7-05B4-47D4-82E9-9B06A6170D40}']
    function configuration: UIListContentConfiguration; cdecl;
    function imageLayoutGuide: UILayoutGuide; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithConfiguration(configuration: UIListContentConfiguration): Pointer; cdecl;
    function initWithFrame(frame: CGRect): Pointer; cdecl;
    function secondaryTextLayoutGuide: UILayoutGuide; cdecl;
    procedure setConfiguration(configuration: UIListContentConfiguration); cdecl;
    function textLayoutGuide: UILayoutGuide; cdecl;
  end;
  TUIListContentView = class(TOCGenericImport<UIListContentViewClass, UIListContentView>) end;

  UIVibrancyEffectClass = interface(UIVisualEffectClass)
    ['{61C15133-1A73-44E4-AE0C-E5CAA544F461}']
    {class} function effectForBlurEffect(blurEffect: UIBlurEffect): UIVibrancyEffect; overload; cdecl;
    {class} function effectForBlurEffect(blurEffect: UIBlurEffect; style: UIVibrancyEffectStyle): UIVibrancyEffect; overload; cdecl;
  end;

  UIVibrancyEffect = interface(UIVisualEffect)
    ['{0627572A-4BE0-449C-BE18-C136B014516A}']
  end;
  TUIVibrancyEffect = class(TOCGenericImport<UIVibrancyEffectClass, UIVibrancyEffect>) end;

  UIVisualEffectViewClass = interface(UIViewClass)
    ['{4AE2BC00-AC45-4DAE-95B8-96C87FC77FBC}']
  end;

  UIVisualEffectView = interface(UIView)
    ['{5E696C0B-5090-49BB-B8B2-497461CACB58}']
    function contentView: UIView; cdecl;
    function effect: UIVisualEffect; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithEffect(effect: UIVisualEffect): Pointer; cdecl;
    procedure setEffect(effect: UIVisualEffect); cdecl;
  end;
  TUIVisualEffectView = class(TOCGenericImport<UIVisualEffectViewClass, UIVisualEffectView>) end;

  UILexiconEntryClass = interface(NSObjectClass)
    ['{AA8590C9-FA75-4A80-B8B7-1513B8DFFFAB}']
  end;

  UILexiconEntry = interface(NSObject)
    ['{AE078675-BC80-4317-B6A5-34912C92F415}']
    function documentText: NSString; cdecl;
    function userInput: NSString; cdecl;
  end;
  TUILexiconEntry = class(TOCGenericImport<UILexiconEntryClass, UILexiconEntry>) end;

  UILexiconClass = interface(NSObjectClass)
    ['{2F2C8E43-AE86-43FA-81C0-16767DDD5848}']
  end;

  UILexicon = interface(NSObject)
    ['{5937277A-9BFA-4C86-8F11-7EAD8E2BB971}']
    function entries: NSArray; cdecl;
  end;
  TUILexicon = class(TOCGenericImport<UILexiconClass, UILexicon>) end;

  UILargeContentViewerItem = interface(IObjectiveC)
    ['{5E102BE5-2D6E-4157-A2CB-D2BB67062C3C}']
    function largeContentImage: UIImage; cdecl;
    function largeContentImageInsets: UIEdgeInsets; cdecl;
    function largeContentTitle: NSString; cdecl;
    function scalesLargeContentImage: Boolean; cdecl;
    function showsLargeContentViewer: Boolean; cdecl;
  end;

  UILargeContentViewerInteractionClass = interface(NSObjectClass)
    ['{EE275842-CC88-4BF8-8CC2-1A1142F5F28A}']
    {class} function isEnabled: Boolean; cdecl;
  end;

  UILargeContentViewerInteraction = interface(NSObject)
    ['{5897FD4D-FC65-4639-AA51-DBD5A3AF1465}']
    function delegate: Pointer; cdecl;
    function gestureRecognizerForExclusionRelationship: UIGestureRecognizer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
  end;
  TUILargeContentViewerInteraction = class(TOCGenericImport<UILargeContentViewerInteractionClass, UILargeContentViewerInteraction>) end;

  UILargeContentViewerInteractionDelegate = interface(IObjectiveC)
    ['{B07049CB-4476-4F9A-B81C-53E4CE0A428E}']
    function largeContentViewerInteraction(interaction: UILargeContentViewerInteraction; itemAtPoint: CGPoint): Pointer; overload; cdecl;
    procedure largeContentViewerInteraction(interaction: UILargeContentViewerInteraction; didEndOnItem: Pointer; atPoint: CGPoint); overload; cdecl;
    function viewControllerForLargeContentViewerInteraction(interaction: UILargeContentViewerInteraction): UIViewController; cdecl;
  end;

  UIUserNotificationCategoryClass = interface(NSObjectClass)
    ['{FE10940B-BA4A-4D00-A707-89C8577A200F}']
  end;

  UIUserNotificationCategory = interface(NSObject)
    ['{CCEE0B68-0B72-4A86-8842-5995FFF3CFA0}']
    function actionsForContext(context: UIUserNotificationActionContext): NSArray; cdecl;
    function identifier: NSString; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
  end;
  TUIUserNotificationCategory = class(TOCGenericImport<UIUserNotificationCategoryClass, UIUserNotificationCategory>) end;

  UIMutableUserNotificationCategoryClass = interface(UIUserNotificationCategoryClass)
    ['{731E241B-97A9-46EE-B1CA-50470BD2A556}']
  end;

  UIMutableUserNotificationCategory = interface(UIUserNotificationCategory)
    ['{3E36C5EA-88A2-4B56-8F4D-C59F6C406AAE}']
    function identifier: NSString; cdecl;
    procedure setActions(actions: NSArray; forContext: UIUserNotificationActionContext); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
  end;
  TUIMutableUserNotificationCategory = class(TOCGenericImport<UIMutableUserNotificationCategoryClass, UIMutableUserNotificationCategory>) end;

  UIUserNotificationActionClass = interface(NSObjectClass)
    ['{B9AE7ED7-6779-4F91-B9F0-47936EDF5F52}']
  end;

  UIUserNotificationAction = interface(NSObject)
    ['{5B59EA04-5047-4A2F-BBD8-2E4211FD7662}']
    function activationMode: UIUserNotificationActivationMode; cdecl;
    function behavior: UIUserNotificationActionBehavior; cdecl;
    function identifier: NSString; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function isAuthenticationRequired: Boolean; cdecl;
    function isDestructive: Boolean; cdecl;
    function parameters: NSDictionary; cdecl;
    function title: NSString; cdecl;
  end;
  TUIUserNotificationAction = class(TOCGenericImport<UIUserNotificationActionClass, UIUserNotificationAction>) end;

  UIMutableUserNotificationActionClass = interface(UIUserNotificationActionClass)
    ['{1B654F63-7906-49DC-8891-B6B304C928A6}']
  end;

  UIMutableUserNotificationAction = interface(UIUserNotificationAction)
    ['{BF9ED985-549C-412B-B57F-75D1334D96C5}']
    function activationMode: UIUserNotificationActivationMode; cdecl;
    function behavior: UIUserNotificationActionBehavior; cdecl;
    function identifier: NSString; cdecl;
    function isAuthenticationRequired: Boolean; cdecl;
    function isDestructive: Boolean; cdecl;
    function parameters: NSDictionary; cdecl;
    procedure setActivationMode(activationMode: UIUserNotificationActivationMode); cdecl;
    procedure setAuthenticationRequired(authenticationRequired: Boolean); cdecl;
    procedure setBehavior(behavior: UIUserNotificationActionBehavior); cdecl;
    procedure setDestructive(destructive: Boolean); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setParameters(parameters: NSDictionary); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;
  end;
  TUIMutableUserNotificationAction = class(TOCGenericImport<UIMutableUserNotificationActionClass, UIMutableUserNotificationAction>) end;

  UIPressClass = interface(NSObjectClass)
    ['{BE8AB240-A68C-4FB6-9F9F-44537759999A}']
  end;

  UIPress = interface(NSObject)
    ['{86A7AB30-3642-4A33-B45B-6E1A5752B8F5}']
    [MethodName('type')]
    function &type: UIPressType; cdecl;
    function force: CGFloat; cdecl;
    function gestureRecognizers: NSArray; cdecl;
    function key: UIKey; cdecl;
    function phase: UIPressPhase; cdecl;
    function responder: UIResponder; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function window: UIWindow; cdecl;
  end;
  TUIPress = class(TOCGenericImport<UIPressClass, UIPress>) end;

  UIPressesEventClass = interface(UIEventClass)
    ['{00F12498-64A5-4F28-8813-FFB46B7D756F}']
  end;

  UIPressesEvent = interface(UIEvent)
    ['{ADFDF9FF-0A7F-4D68-8406-CDC3950CBD51}']
    function allPresses: NSSet; cdecl;
    function pressesForGestureRecognizer(gesture: UIGestureRecognizer): NSSet; cdecl;
  end;
  TUIPressesEvent = class(TOCGenericImport<UIPressesEventClass, UIPressesEvent>) end;

  UISearchContainerViewControllerClass = interface(UIViewControllerClass)
    ['{13A940D2-538C-486C-9F38-082357E8D486}']
  end;

  UISearchContainerViewController = interface(UIViewController)
    ['{EEFBD972-AF05-4F64-9DFF-408E57492F98}']
    function initWithSearchController(searchController: UISearchController): Pointer; cdecl;
    function searchController: UISearchController; cdecl;
  end;
  TUISearchContainerViewController = class(TOCGenericImport<UISearchContainerViewControllerClass, UISearchContainerViewController>) end;

  UISearchTextFieldClass = interface(UITextFieldClass)
    ['{D06EBC8B-1DEB-4283-B92B-283B2F378030}']
  end;

  UISearchTextField = interface(UITextField)
    ['{F3266D50-F218-487D-8828-FBF16AD86277}']
    function allowsCopyingTokens: Boolean; cdecl;
    function allowsDeletingTokens: Boolean; cdecl;
    procedure insertToken(token: UISearchToken; atIndex: NSInteger); cdecl;
    function positionOfTokenAtIndex(tokenIndex: NSInteger): UITextPosition; cdecl;
    procedure removeTokenAtIndex(tokenIndex: NSInteger); cdecl;
    procedure replaceTextualPortionOfRange(textRange: UITextRange; withToken: UISearchToken; atIndex: NSUInteger); cdecl;
    procedure setAllowsCopyingTokens(allowsCopyingTokens: Boolean); cdecl;
    procedure setAllowsDeletingTokens(allowsDeletingTokens: Boolean); cdecl;
    procedure setTokenBackgroundColor(tokenBackgroundColor: UIColor); cdecl;
    procedure setTokens(tokens: NSArray); cdecl;
    function textualRange: UITextRange; cdecl;
    function tokenBackgroundColor: UIColor; cdecl;
    function tokens: NSArray; cdecl;
    function tokensInRange(textRange: UITextRange): NSArray; cdecl;
  end;
  TUISearchTextField = class(TOCGenericImport<UISearchTextFieldClass, UISearchTextField>) end;

  UISearchTokenClass = interface(NSObjectClass)
    ['{CD4F1E39-A47D-4AD1-89E4-437BF9F939C1}']
    {class} function new: Pointer; cdecl;
    {class} function tokenWithIcon(icon: UIImage; text: NSString): UISearchToken; cdecl;
  end;

  UISearchToken = interface(NSObject)
    ['{7FA78B61-D014-4263-8560-CC198F27A284}']
    function representedObject: Pointer; cdecl;
    procedure setRepresentedObject(representedObject: Pointer); cdecl;
  end;
  TUISearchToken = class(TOCGenericImport<UISearchTokenClass, UISearchToken>) end;

  UISearchTextFieldDelegate = interface(IObjectiveC)
    ['{0A537A94-5D8F-4A53-AD88-EAA307A582CE}']
    // Foundation function searchTextField(searchTextField: UISearchTextField; itemProviderForCopyingToken: UISearchToken): NSItemProvider; cdecl;
  end;

  UISearchTextFieldPasteItem = interface(IObjectiveC)
    ['{9FA6BCD4-9261-4B49-AF1E-0E38DF3B4E84}']
    procedure setSearchTokenResult(token: UISearchToken); cdecl;
  end;

  UIStoryboardUnwindSegueSourceClass = interface(NSObjectClass)
    ['{3E6D3F8D-C50A-43F3-8919-4F5C3B2716F4}']
  end;

  UIStoryboardUnwindSegueSource = interface(NSObject)
    ['{4639AB35-6A7A-411D-8B8E-1C91A15C6CC7}']
    function sender: Pointer; cdecl;
    function sourceViewController: UIViewController; cdecl;
    function unwindAction: Pointer; cdecl;
  end;
  TUIStoryboardUnwindSegueSource = class(TOCGenericImport<UIStoryboardUnwindSegueSourceClass, UIStoryboardUnwindSegueSource>) end;

  UIPreviewTargetClass = interface(NSObjectClass)
    ['{737AB6C1-4323-41C2-A18B-D189DA7CF92C}']
    {class} function new: Pointer; cdecl;
  end;

  UIPreviewTarget = interface(NSObject)
    ['{CCA45CF8-3ABB-473A-9696-DB499FB630C1}']
    function center: CGPoint; cdecl;
    function container: UIView; cdecl;
    function initWithContainer(container: UIView; center: CGPoint; transform: CGAffineTransform): Pointer; overload; cdecl;
    function initWithContainer(container: UIView; center: CGPoint): Pointer; overload; cdecl;
    function transform: CGAffineTransform; cdecl;
  end;
  TUIPreviewTarget = class(TOCGenericImport<UIPreviewTargetClass, UIPreviewTarget>) end;

  UITargetedPreviewClass = interface(NSObjectClass)
    ['{6A936100-67B2-45CC-83F6-5AB81ADC3785}']
    {class} function new: Pointer; cdecl;
  end;

  UITargetedPreview = interface(NSObject)
    ['{1B68D420-070A-477D-A622-47A6C8DB1A15}']
    function initWithView(view: UIView): Pointer; overload; cdecl;
    function initWithView(view: UIView; parameters: UIPreviewParameters): Pointer; overload; cdecl;
    function initWithView(view: UIView; parameters: UIPreviewParameters; target: UIPreviewTarget): Pointer; overload; cdecl;
    function parameters: UIPreviewParameters; cdecl;
    function retargetedPreviewWithTarget(newTarget: UIPreviewTarget): UITargetedPreview; cdecl;
    function size: CGSize; cdecl;
    function target: UIPreviewTarget; cdecl;
    function view: UIView; cdecl;
  end;
  TUITargetedPreview = class(TOCGenericImport<UITargetedPreviewClass, UITargetedPreview>) end;

  UIDragPreviewTargetClass = interface(UIPreviewTargetClass)
    ['{41DBD1EB-2D57-48CC-84CF-FC02F8342891}']
  end;

  UIDragPreviewTarget = interface(UIPreviewTarget)
    ['{10893AC4-A3FD-40B6-8C8E-58D6D18A5F38}']
  end;
  TUIDragPreviewTarget = class(TOCGenericImport<UIDragPreviewTargetClass, UIDragPreviewTarget>) end;

  UITargetedDragPreviewClass = interface(UITargetedPreviewClass)
    ['{59CB49E1-FC45-41C8-9FD9-0871249A5F0B}']
    {class} function previewForURL(url: NSURL; target: UIDragPreviewTarget): Pointer; overload; cdecl;
    {class} function previewForURL(url: NSURL; title: NSString; target: UIDragPreviewTarget): Pointer; overload; cdecl;
  end;

  UITargetedDragPreview = interface(UITargetedPreview)
    ['{E7ACED1C-2B3B-4115-AC3B-084DCD4AF104}']
    function retargetedPreviewWithTarget(newTarget: UIDragPreviewTarget): UITargetedDragPreview; cdecl;
  end;
  TUITargetedDragPreview = class(TOCGenericImport<UITargetedDragPreviewClass, UITargetedDragPreview>) end;

  UISpringLoadedInteractionClass = interface(NSObjectClass)
    ['{88B0D3E2-3DF2-4FA3-A386-53F2A3F1C686}']
    {class} function new: Pointer; cdecl;
  end;

  UISpringLoadedInteraction = interface(NSObject)
    ['{9E580022-7FD5-4AE1-B6B5-5870DAA401C2}']
    function initWithActivationHandler(handler: TUISpringLoadedInteractionBlockMethod1): Pointer; cdecl;
    function initWithInteractionBehavior(interactionBehavior: Pointer; interactionEffect: Pointer; activationHandler: TUISpringLoadedInteractionBlockMethod1): Pointer; cdecl;
    function interactionBehavior: Pointer; cdecl;
    function interactionEffect: Pointer; cdecl;
  end;
  TUISpringLoadedInteraction = class(TOCGenericImport<UISpringLoadedInteractionClass, UISpringLoadedInteraction>) end;

  UISpringLoadedInteractionBehavior = interface(IObjectiveC)
    ['{1AC70EA3-6427-4583-BF05-9B84E3F647A9}']
    procedure interactionDidFinish(interaction: UISpringLoadedInteraction); cdecl;
    function shouldAllowInteraction(interaction: UISpringLoadedInteraction; withContext: Pointer): Boolean; cdecl;
  end;

  UISpringLoadedInteractionEffect = interface(IObjectiveC)
    ['{B2786F7C-C72A-4FCF-9F75-004BF929667C}']
    procedure interaction(interaction: UISpringLoadedInteraction; didChangeWithContext: Pointer); cdecl;
  end;

  UISpringLoadedInteractionContext = interface(IObjectiveC)
    ['{BC606D91-F3F4-46AC-A8E5-7B13493B68A9}']
    function locationInView(view: UIView): CGPoint; cdecl;
    procedure setTargetItem(targetItem: Pointer); cdecl;
    procedure setTargetView(targetView: UIView); cdecl;
    function state: UISpringLoadedInteractionEffectState; cdecl;
    function targetItem: Pointer; cdecl;
    function targetView: UIView; cdecl;
  end;

  UINavigationBarAppearanceClass = interface(UIBarAppearanceClass)
    ['{C925FE7C-AD5A-481C-9E51-018F32FAE5B9}']
  end;

  UINavigationBarAppearance = interface(UIBarAppearance)
    ['{340785BC-B462-450E-8937-A2A3B63F8FB6}']
    function backButtonAppearance: UIBarButtonItemAppearance; cdecl;
    function backIndicatorImage: UIImage; cdecl;
    function backIndicatorTransitionMaskImage: UIImage; cdecl;
    function buttonAppearance: UIBarButtonItemAppearance; cdecl;
    function doneButtonAppearance: UIBarButtonItemAppearance; cdecl;
    function largeTitleTextAttributes: NSDictionary; cdecl;
    procedure setBackButtonAppearance(backButtonAppearance: UIBarButtonItemAppearance); cdecl;
    procedure setBackIndicatorImage(backIndicatorImage: UIImage; transitionMaskImage: UIImage); cdecl;
    procedure setButtonAppearance(buttonAppearance: UIBarButtonItemAppearance); cdecl;
    procedure setDoneButtonAppearance(doneButtonAppearance: UIBarButtonItemAppearance); cdecl;
    procedure setLargeTitleTextAttributes(largeTitleTextAttributes: NSDictionary); cdecl;
    procedure setTitlePositionAdjustment(titlePositionAdjustment: UIOffset); cdecl;
    procedure setTitleTextAttributes(titleTextAttributes: NSDictionary); cdecl;
    function titlePositionAdjustment: UIOffset; cdecl;
    function titleTextAttributes: NSDictionary; cdecl;
  end;
  TUINavigationBarAppearance = class(TOCGenericImport<UINavigationBarAppearanceClass, UINavigationBarAppearance>) end;

  UIToolbarAppearanceClass = interface(UIBarAppearanceClass)
    ['{D91493B0-91F6-43A7-857A-A572B27D4B34}']
  end;

  UIToolbarAppearance = interface(UIBarAppearance)
    ['{17290DE8-3027-4BF5-88C8-A8CF5AE5AAA2}']
    function buttonAppearance: UIBarButtonItemAppearance; cdecl;
    function doneButtonAppearance: UIBarButtonItemAppearance; cdecl;
    procedure setButtonAppearance(buttonAppearance: UIBarButtonItemAppearance); cdecl;
    procedure setDoneButtonAppearance(doneButtonAppearance: UIBarButtonItemAppearance); cdecl;
  end;
  TUIToolbarAppearance = class(TOCGenericImport<UIToolbarAppearanceClass, UIToolbarAppearance>) end;

  UITabBarItemStateAppearanceClass = interface(NSObjectClass)
    ['{617DBFBE-0D99-43C7-A7CA-7D2485E0BCFD}']
    {class} function new: Pointer; cdecl;
  end;

  UITabBarItemStateAppearance = interface(NSObject)
    ['{5545598F-8175-495C-9594-57CF4CF3DE78}']
    function badgeBackgroundColor: UIColor; cdecl;
    function badgePositionAdjustment: UIOffset; cdecl;
    function badgeTextAttributes: NSDictionary; cdecl;
    function badgeTitlePositionAdjustment: UIOffset; cdecl;
    function iconColor: UIColor; cdecl;
    procedure setBadgeBackgroundColor(badgeBackgroundColor: UIColor); cdecl;
    procedure setBadgePositionAdjustment(badgePositionAdjustment: UIOffset); cdecl;
    procedure setBadgeTextAttributes(badgeTextAttributes: NSDictionary); cdecl;
    procedure setBadgeTitlePositionAdjustment(badgeTitlePositionAdjustment: UIOffset); cdecl;
    procedure setIconColor(iconColor: UIColor); cdecl;
    procedure setTitlePositionAdjustment(titlePositionAdjustment: UIOffset); cdecl;
    procedure setTitleTextAttributes(titleTextAttributes: NSDictionary); cdecl;
    function titlePositionAdjustment: UIOffset; cdecl;
    function titleTextAttributes: NSDictionary; cdecl;
  end;
  TUITabBarItemStateAppearance = class(TOCGenericImport<UITabBarItemStateAppearanceClass, UITabBarItemStateAppearance>) end;

  UITabBarItemAppearanceClass = interface(NSObjectClass)
    ['{A45B24B6-8133-46BB-8A76-15BFBD65446C}']
  end;

  UITabBarItemAppearance = interface(NSObject)
    ['{9BBA7805-59DA-4BD5-AB7C-8A25B687435B}']
    procedure configureWithDefaultForStyle(style: UITabBarItemAppearanceStyle); cdecl;
    function copy: Pointer; cdecl;
    function disabled: UITabBarItemStateAppearance; cdecl;
    function focused: UITabBarItemStateAppearance; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithStyle(style: UITabBarItemAppearanceStyle): Pointer; cdecl;
    function normal: UITabBarItemStateAppearance; cdecl;
    function selected: UITabBarItemStateAppearance; cdecl;
  end;
  TUITabBarItemAppearance = class(TOCGenericImport<UITabBarItemAppearanceClass, UITabBarItemAppearance>) end;

  UITabBarAppearanceClass = interface(UIBarAppearanceClass)
    ['{D82AE79D-3AF6-4616-BDCB-165F27E092BB}']
  end;

  UITabBarAppearance = interface(UIBarAppearance)
    ['{A30C6893-937D-423E-AF66-E2C80B906863}']
    function compactInlineLayoutAppearance: UITabBarItemAppearance; cdecl;
    function inlineLayoutAppearance: UITabBarItemAppearance; cdecl;
    function selectionIndicatorImage: UIImage; cdecl;
    function selectionIndicatorTintColor: UIColor; cdecl;
    procedure setCompactInlineLayoutAppearance(compactInlineLayoutAppearance: UITabBarItemAppearance); cdecl;
    procedure setInlineLayoutAppearance(inlineLayoutAppearance: UITabBarItemAppearance); cdecl;
    procedure setSelectionIndicatorImage(selectionIndicatorImage: UIImage); cdecl;
    procedure setSelectionIndicatorTintColor(selectionIndicatorTintColor: UIColor); cdecl;
    procedure setStackedItemPositioning(stackedItemPositioning: UITabBarItemPositioning); cdecl;
    procedure setStackedItemSpacing(stackedItemSpacing: CGFloat); cdecl;
    procedure setStackedItemWidth(stackedItemWidth: CGFloat); cdecl;
    procedure setStackedLayoutAppearance(stackedLayoutAppearance: UITabBarItemAppearance); cdecl;
    function stackedItemPositioning: UITabBarItemPositioning; cdecl;
    function stackedItemSpacing: CGFloat; cdecl;
    function stackedItemWidth: CGFloat; cdecl;
    function stackedLayoutAppearance: UITabBarItemAppearance; cdecl;
  end;
  TUITabBarAppearance = class(TOCGenericImport<UITabBarAppearanceClass, UITabBarAppearance>) end;

  UISearchSuggestion = interface(IObjectiveC)
    ['{706AE7BB-7F80-498C-BE94-DC6FBE4C5A5F}']
    function iconImage: UIImage; cdecl;
    function localizedDescription: NSString; cdecl;
    function localizedSuggestion: NSString; cdecl;
  end;

  UISearchSuggestionItemClass = interface(NSObjectClass)
    ['{3C4ADC0D-4929-4812-9A4B-FDDA5F473D8D}']
    {class} function suggestionWithLocalizedSuggestion(suggestion: NSString; descriptionString: NSString; iconImage: UIImage): Pointer; overload; cdecl;
    {class} function suggestionWithLocalizedSuggestion(suggestion: NSString; descriptionString: NSString): Pointer; overload; cdecl;
    {class} function suggestionWithLocalizedSuggestion(suggestion: NSString): Pointer; overload; cdecl;
  end;

  UISearchSuggestionItem = interface(NSObject)
    ['{061F70B8-FCD4-4D4D-A2A5-CF7FB82C47E4}']
    function iconImage: UIImage; cdecl;
    function initWithLocalizedSuggestion(suggestion: NSString; localizedDescription: NSString; iconImage: UIImage): Pointer; overload; cdecl;
    function initWithLocalizedSuggestion(suggestion: NSString): Pointer; overload; cdecl;
    function initWithLocalizedSuggestion(suggestion: NSString; localizedDescription: NSString): Pointer; overload; cdecl;
    function localizedDescription: NSString; cdecl;
    function localizedSuggestion: NSString; cdecl;
  end;
  TUISearchSuggestionItem = class(TOCGenericImport<UISearchSuggestionItemClass, UISearchSuggestionItem>) end;

  UIScribbleInteractionClass = interface(NSObjectClass)
    ['{1DA6554A-1E84-4778-815F-18191A79EAF0}']
    {class} function isPencilInputExpected: Boolean; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIScribbleInteraction = interface(NSObject)
    ['{3DBCAFE4-8030-435F-8774-29DCBF259DD1}']
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function isHandlingWriting: Boolean; cdecl;
  end;
  TUIScribbleInteraction = class(TOCGenericImport<UIScribbleInteractionClass, UIScribbleInteraction>) end;

  UIScribbleInteractionDelegate = interface(IObjectiveC)
    ['{7C65EE51-AB71-46E2-B1A2-6BD53D3DBD01}']
    function scribbleInteraction(interaction: UIScribbleInteraction; shouldBeginAtLocation: CGPoint): Boolean; cdecl;
    procedure scribbleInteractionDidFinishWriting(interaction: UIScribbleInteraction); cdecl;
    function scribbleInteractionShouldDelayFocus(interaction: UIScribbleInteraction): Boolean; cdecl;
    procedure scribbleInteractionWillBeginWriting(interaction: UIScribbleInteraction); cdecl;
  end;

  UIStackViewClass = interface(UIViewClass)
    ['{042BA81A-7E93-403F-AC9E-413A9D9F5E65}']
  end;

  UIStackView = interface(UIView)
    ['{3550CDB0-DDBA-41F9-A4B4-A9BB4AD4D392}']
    procedure addArrangedSubview(view: UIView); cdecl;
    function alignment: UIStackViewAlignment; cdecl;
    function arrangedSubviews: NSArray; cdecl;
    function axis: UILayoutConstraintAxis; cdecl;
    function customSpacingAfterView(arrangedSubview: UIView): CGFloat; cdecl;
    function distribution: UIStackViewDistribution; cdecl;
    function initWithArrangedSubviews(views: NSArray): Pointer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithFrame(frame: CGRect): Pointer; cdecl;
    procedure insertArrangedSubview(view: UIView; atIndex: NSUInteger); cdecl;
    function isBaselineRelativeArrangement: Boolean; cdecl;
    function isLayoutMarginsRelativeArrangement: Boolean; cdecl;
    procedure removeArrangedSubview(view: UIView); cdecl;
    procedure setAlignment(alignment: UIStackViewAlignment); cdecl;
    procedure setAxis(axis: UILayoutConstraintAxis); cdecl;
    procedure setBaselineRelativeArrangement(baselineRelativeArrangement: Boolean); cdecl;
    procedure setCustomSpacing(spacing: CGFloat; afterView: UIView); cdecl;
    procedure setDistribution(distribution: UIStackViewDistribution); cdecl;
    procedure setLayoutMarginsRelativeArrangement(layoutMarginsRelativeArrangement: Boolean); cdecl;
    procedure setSpacing(spacing: CGFloat); cdecl;
    function spacing: CGFloat; cdecl;
  end;
  TUIStackView = class(TOCGenericImport<UIStackViewClass, UIStackView>) end;

  UIPreviewInteractionClass = interface(NSObjectClass)
    ['{47043878-A157-4F78-B0C9-128F530981C7}']
  end;

  UIPreviewInteraction = interface(NSObject)
    ['{824DD880-4405-4856-BAD5-E0A660A95B9F}']
    procedure cancelInteraction; cdecl;
    function delegate: Pointer; cdecl;
    function initWithView(view: UIView): Pointer; cdecl;
    function locationInCoordinateSpace(coordinateSpace: Pointer): CGPoint; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function view: UIView; cdecl;
  end;
  TUIPreviewInteraction = class(TOCGenericImport<UIPreviewInteractionClass, UIPreviewInteraction>) end;

  UIPreviewInteractionDelegate = interface(IObjectiveC)
    ['{2F0834FD-BFEA-46CA-9A67-46A9D807A2A7}']
    procedure previewInteractionDidCancel(previewInteraction: UIPreviewInteraction); cdecl;
    [MethodName('previewInteraction:didUpdateCommitTransition:ended:')]
    procedure previewInteractionDidUpdateCommitTransition(previewInteraction: UIPreviewInteraction; didUpdateCommitTransition: CGFloat; ended: Boolean); cdecl;
    [MethodName('previewInteraction:didUpdatePreviewTransition:ended:')]
    procedure previewInteractionDidUpdatePreviewTransition(previewInteraction: UIPreviewInteraction; didUpdatePreviewTransition: CGFloat; ended: Boolean); cdecl;
    function previewInteractionShouldBegin(previewInteraction: UIPreviewInteraction): Boolean; cdecl;
  end;

  UIRegionClass = interface(NSObjectClass)
    ['{5C241644-B465-4122-9E0C-B605A5652D77}']
    {class} function infiniteRegion: UIRegion; cdecl;
  end;

  UIRegion = interface(NSObject)
    ['{940F4C6B-E692-44F7-BE9F-C6330BC0810E}']
    function containsPoint(point: CGPoint): Boolean; cdecl;
    function initWithRadius(radius: CGFloat): Pointer; cdecl;
    function initWithSize(size: CGSize): Pointer; cdecl;
    function inverseRegion: Pointer; cdecl;
    function regionByDifferenceFromRegion(region: UIRegion): Pointer; cdecl;
    function regionByIntersectionWithRegion(region: UIRegion): Pointer; cdecl;
    function regionByUnionWithRegion(region: UIRegion): Pointer; cdecl;
  end;
  TUIRegion = class(TOCGenericImport<UIRegionClass, UIRegion>) end;

  UITextDragPreviewRendererClass = interface(NSObjectClass)
    ['{7356D141-BB6F-43E9-88FA-CBDBD5F7B275}']
    {class} function new: Pointer; cdecl;
  end;

  UITextDragPreviewRenderer = interface(NSObject)
    ['{16AE0B48-0009-4B25-9FD7-F7C5EE7E1408}']
    procedure adjustFirstLineRect(firstLineRect: PCGRect; bodyRect: PCGRect; lastLineRect: PCGRect; textOrigin: CGPoint); cdecl;
    function bodyRect: CGRect; cdecl;
    function firstLineRect: CGRect; cdecl;
    function image: UIImage; cdecl;
    function initWithLayoutManager(layoutManager: NSLayoutManager; range: NSRange; unifyRects: Boolean): Pointer; overload; cdecl;
    function initWithLayoutManager(layoutManager: NSLayoutManager; range: NSRange): Pointer; overload; cdecl;
    function lastLineRect: CGRect; cdecl;
    function layoutManager: NSLayoutManager; cdecl;
  end;
  TUITextDragPreviewRenderer = class(TOCGenericImport<UITextDragPreviewRendererClass, UITextDragPreviewRenderer>) end;

  UIViewPropertyAnimatorClass = interface(NSObjectClass)
    ['{CEF3FAF8-580D-436F-8099-9B801D0469DA}']
    {class} function runningPropertyAnimatorWithDuration(duration: NSTimeInterval; delay: NSTimeInterval; options: UIViewAnimationOptions; animations: TUIViewPropertyAnimatorBlockMethod1; completion: TUIViewPropertyAnimatorBlockMethod2): Pointer; cdecl;
  end;

  UIViewPropertyAnimator = interface(NSObject)
    ['{37AAA28E-3A1D-471A-947B-2DD77A8609BB}']
    procedure addAnimations(animation: TUIViewPropertyAnimatorBlockMethod1; delayFactor: CGFloat); overload; cdecl;
    procedure addAnimations(animation: TUIViewPropertyAnimatorBlockMethod1); overload; cdecl;
    procedure addCompletion(completion: TUIViewPropertyAnimatorBlockMethod2); cdecl;
    procedure continueAnimationWithTimingParameters(parameters: Pointer; durationFactor: CGFloat); cdecl;
    function delay: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function initWithDuration(duration: NSTimeInterval; timingParameters: Pointer): Pointer; overload; cdecl;
    function initWithDuration(duration: NSTimeInterval; curve: UIViewAnimationCurve; animations: TUIViewPropertyAnimatorBlockMethod1): Pointer; overload; cdecl;
    function initWithDuration(duration: NSTimeInterval; controlPoint1: CGPoint; controlPoint2: CGPoint; animations: TUIViewPropertyAnimatorBlockMethod1): Pointer; overload; cdecl;
    function initWithDuration(duration: NSTimeInterval; dampingRatio: CGFloat; animations: TUIViewPropertyAnimatorBlockMethod1): Pointer; overload; cdecl;
    function isInterruptible: Boolean; cdecl;
    function isManualHitTestingEnabled: Boolean; cdecl;
    function isUserInteractionEnabled: Boolean; cdecl;
    function pausesOnCompletion: Boolean; cdecl;
    function scrubsLinearly: Boolean; cdecl;
    procedure setInterruptible(interruptible: Boolean); cdecl;
    procedure setManualHitTestingEnabled(manualHitTestingEnabled: Boolean); cdecl;
    procedure setPausesOnCompletion(pausesOnCompletion: Boolean); cdecl;
    procedure setScrubsLinearly(scrubsLinearly: Boolean); cdecl;
    procedure setUserInteractionEnabled(userInteractionEnabled: Boolean); cdecl;
    function timingParameters: Pointer; cdecl;
  end;
  TUIViewPropertyAnimator = class(TOCGenericImport<UIViewPropertyAnimatorClass, UIViewPropertyAnimator>) end;

  UISelectionFeedbackGeneratorClass = interface(UIFeedbackGeneratorClass)
    ['{E6A67C65-CEFF-4184-B507-2BCBF0530F2E}']
  end;

  UISelectionFeedbackGenerator = interface(UIFeedbackGenerator)
    ['{D7AAB8F0-A9D9-4F04-BA24-23BA7247BA13}']
    procedure selectionChanged; cdecl;
  end;
  TUISelectionFeedbackGenerator = class(TOCGenericImport<UISelectionFeedbackGeneratorClass, UISelectionFeedbackGenerator>) end;

  UINotificationFeedbackGeneratorClass = interface(UIFeedbackGeneratorClass)
    ['{8B2E458F-7B73-43B0-87E1-D0DBC17A7429}']
  end;

  UINotificationFeedbackGenerator = interface(UIFeedbackGenerator)
    ['{ADB40311-CB2E-401A-AD7E-585B0DF4D1EC}']
    procedure notificationOccurred(notificationType: UINotificationFeedbackType); cdecl;
  end;
  TUINotificationFeedbackGenerator = class(TOCGenericImport<UINotificationFeedbackGeneratorClass, UINotificationFeedbackGenerator>) end;

  UITextInteractionDelegate = interface(IObjectiveC)
    ['{5BFD68F3-7B39-49A3-AFF5-A01ADCAFC8C6}']
    procedure interactionDidEnd(interaction: UITextInteraction); cdecl;
    function interactionShouldBegin(interaction: UITextInteraction; atPoint: CGPoint): Boolean; cdecl;
    procedure interactionWillBegin(interaction: UITextInteraction); cdecl;
  end;

  UITextInteractionClass = interface(NSObjectClass)
    ['{102EAD6F-9D8C-4D9C-9130-54C3B909172C}']
    {class} function textInteractionForMode(mode: UITextInteractionMode): Pointer; cdecl;
  end;

  UITextInteraction = interface(NSObject)
    ['{870C90F1-8666-4048-8CC5-098EA17BD173}']
    function delegate: Pointer; cdecl;
    function gesturesForFailureRequirements: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setTextInput(textInput: UIResponder); cdecl;
    function textInput: UIResponder; cdecl;
    function textInteractionMode: UITextInteractionMode; cdecl;
  end;
  TUITextInteraction = class(TOCGenericImport<UITextInteractionClass, UITextInteraction>) end;

  UIPencilInteractionClass = interface(NSObjectClass)
    ['{DFAB593E-72AD-4DE3-84A9-78D4CA3995BB}']
    {class} function preferredTapAction: UIPencilPreferredAction; cdecl;
    {class} function prefersPencilOnlyDrawing: Boolean; cdecl;
  end;

  UIPencilInteraction = interface(NSObject)
    ['{09062B88-9E55-4156-83B2-1563E5118C8F}']
    function delegate: Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TUIPencilInteraction = class(TOCGenericImport<UIPencilInteractionClass, UIPencilInteraction>) end;

  UIPencilInteractionDelegate = interface(IObjectiveC)
    ['{59FA447B-A3E8-4DF9-9FF6-54B05FEFEC8A}']
    procedure pencilInteractionDidTap(interaction: UIPencilInteraction); cdecl;
  end;

  UISceneConnectionOptionsClass = interface(NSObjectClass)
    ['{8EF05D9B-C5E6-4988-9999-AB9620771E1A}']
    {class} function new: Pointer; cdecl;
  end;

  UISceneConnectionOptions = interface(NSObject)
    ['{2ECE6710-1217-4E1B-9CCC-75FE52E31CD8}']
    // CloudKit function cloudKitShareMetadata: CKShareMetadata; cdecl;
    function handoffUserActivityType: NSString; cdecl;
    function notificationResponse: UNNotificationResponse; cdecl;
    function shortcutItem: UIApplicationShortcutItem; cdecl;
    function sourceApplication: NSString; cdecl;
    function URLContexts: NSSet; cdecl;
    function userActivities: NSSet; cdecl;
  end;
  TUISceneConnectionOptions = class(TOCGenericImport<UISceneConnectionOptionsClass, UISceneConnectionOptions>) end;

  UISceneOpenURLOptionsClass = interface(NSObjectClass)
    ['{F316A0FA-0AA0-4852-9CCA-CC285A203BE5}']
    {class} function new: Pointer; cdecl;
  end;

  UISceneOpenURLOptions = interface(NSObject)
    ['{D657892D-B894-44E9-A8C8-433982E1A106}']
    function annotation: Pointer; cdecl;
    function openInPlace: Boolean; cdecl;
    function sourceApplication: NSString; cdecl;
  end;
  TUISceneOpenURLOptions = class(TOCGenericImport<UISceneOpenURLOptionsClass, UISceneOpenURLOptions>) end;

  UISceneOpenExternalURLOptionsClass = interface(NSObjectClass)
    ['{BAEF92FA-D3F3-45FC-8D40-6406F3839557}']
  end;

  UISceneOpenExternalURLOptions = interface(NSObject)
    ['{9969EDE2-1E08-4E01-8DB8-C3626B300428}']
    procedure setUniversalLinksOnly(universalLinksOnly: Boolean); cdecl;
    function universalLinksOnly: Boolean; cdecl;
  end;
  TUISceneOpenExternalURLOptions = class(TOCGenericImport<UISceneOpenExternalURLOptionsClass, UISceneOpenExternalURLOptions>) end;

  UISceneActivationRequestOptionsClass = interface(NSObjectClass)
    ['{10DECCE1-DED4-43BA-881E-96B6DCC99D8A}']
  end;

  UISceneActivationRequestOptions = interface(NSObject)
    ['{3B17C287-E62A-465E-B6B4-0A29B588C72D}']
    function collectionJoinBehavior: UISceneCollectionJoinBehavior; cdecl;
    function requestingScene: UIScene; cdecl;
    procedure setCollectionJoinBehavior(collectionJoinBehavior: UISceneCollectionJoinBehavior); cdecl;
    procedure setRequestingScene(requestingScene: UIScene); cdecl;
  end;
  TUISceneActivationRequestOptions = class(TOCGenericImport<UISceneActivationRequestOptionsClass, UISceneActivationRequestOptions>) end;

  UISceneDestructionRequestOptionsClass = interface(NSObjectClass)
    ['{A9BD8065-48DB-4C9B-9640-9E0AE25E9E50}']
  end;

  UISceneDestructionRequestOptions = interface(NSObject)
    ['{D51FE2B2-7819-4CCF-845C-8CD568808AE0}']
  end;
  TUISceneDestructionRequestOptions = class(TOCGenericImport<UISceneDestructionRequestOptionsClass, UISceneDestructionRequestOptions>) end;

  UIWindowSceneClass = interface(UISceneClass)
    ['{275826AC-0E53-408F-A1D1-EB79D99462BA}']
  end;

  UIWindowScene = interface(UIScene)
    ['{7E1AC388-7373-490B-91CB-7088111B98BE}']
    function coordinateSpace: Pointer; cdecl;
    function interfaceOrientation: UIInterfaceOrientation; cdecl;
    function screen: UIScreen; cdecl;
    function screenshotService: UIScreenshotService; cdecl;
    function sizeRestrictions: UISceneSizeRestrictions; cdecl;
    function statusBarManager: UIStatusBarManager; cdecl;
    function traitCollection: UITraitCollection; cdecl;
    function windows: NSArray; cdecl;
  end;
  TUIWindowScene = class(TOCGenericImport<UIWindowSceneClass, UIWindowScene>) end;

  UIWindowSceneDelegate = interface(IObjectiveC)
    ['{04E56960-3B5C-4A48-AE77-3311C2EAD35D}']
    procedure setWindow(window: UIWindow); cdecl;
    function window: UIWindow; cdecl;
    // CloudKit procedure windowScene(windowScene: UIWindowScene; userDidAcceptCloudKitShareWithMetadata: CKShareMetadata); overload; cdecl;
    procedure windowScene(windowScene: UIWindowScene; performActionForShortcutItem: UIApplicationShortcutItem; completionHandler: TUIWindowSceneDelegateBlockMethod1); overload; cdecl;
    procedure windowScene(windowScene: UIWindowScene; didUpdateCoordinateSpace: Pointer; interfaceOrientation: UIInterfaceOrientation; traitCollection: UITraitCollection); overload; cdecl;
  end;

  UIWindowSceneDestructionRequestOptionsClass = interface(UISceneDestructionRequestOptionsClass)
    ['{BA102D42-2369-4B44-B26B-F762CF2B61CF}']
  end;

  UIWindowSceneDestructionRequestOptions = interface(UISceneDestructionRequestOptions)
    ['{A66A5BAE-31C3-4546-92A0-A40C1494AB59}']
    procedure setWindowDismissalAnimation(windowDismissalAnimation: UIWindowSceneDismissalAnimation); cdecl;
    function windowDismissalAnimation: UIWindowSceneDismissalAnimation; cdecl;
  end;
  TUIWindowSceneDestructionRequestOptions = class(TOCGenericImport<UIWindowSceneDestructionRequestOptionsClass, UIWindowSceneDestructionRequestOptions>) end;

  UISceneSizeRestrictionsClass = interface(NSObjectClass)
    ['{94BDB972-4650-4A0E-8273-914EA4C317D5}']
    {class} function new: Pointer; cdecl;
  end;

  UISceneSizeRestrictions = interface(NSObject)
    ['{A6F65184-502B-4DB2-B5F3-479BBF37F482}']
    function maximumSize: CGSize; cdecl;
    function minimumSize: CGSize; cdecl;
    procedure setMaximumSize(maximumSize: CGSize); cdecl;
    procedure setMinimumSize(minimumSize: CGSize); cdecl;
  end;
  TUISceneSizeRestrictions = class(TOCGenericImport<UISceneSizeRestrictionsClass, UISceneSizeRestrictions>) end;

  UISceneConfigurationClass = interface(NSObjectClass)
    ['{D9901A53-C82B-45C3-B3B7-7B2304665D51}']
    {class} function configurationWithName(name: NSString; sessionRole: UISceneSessionRole): Pointer; cdecl;
  end;

  UISceneConfiguration = interface(NSObject)
    ['{C071A362-BA75-43D7-B0EA-1768D4E62408}']
    function delegateClass: Pointer; cdecl;
    function initWithName(name: NSString; sessionRole: UISceneSessionRole): Pointer; cdecl;
    function name: NSString; cdecl;
    function role: UISceneSessionRole; cdecl;
    function sceneClass: Pointer; cdecl;
    procedure setDelegateClass(delegateClass: Pointer); cdecl;
    procedure setSceneClass(sceneClass: Pointer); cdecl;
    procedure setStoryboard(storyboard: UIStoryboard); cdecl;
    function storyboard: UIStoryboard; cdecl;
  end;
  TUISceneConfiguration = class(TOCGenericImport<UISceneConfigurationClass, UISceneConfiguration>) end;

  UISceneSessionClass = interface(NSObjectClass)
    ['{C4363534-266D-49E8-8D93-D6C73C90D215}']
    {class} function new: Pointer; cdecl;
  end;

  UISceneSession = interface(NSObject)
    ['{BA79EFC3-B99C-4DDB-8457-145036D23186}']
    function configuration: UISceneConfiguration; cdecl;
    function persistentIdentifier: NSString; cdecl;
    function role: UISceneSessionRole; cdecl;
    function scene: UIScene; cdecl;
    procedure setStateRestorationActivity(stateRestorationActivity: NSUserActivity); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function stateRestorationActivity: NSUserActivity; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUISceneSession = class(TOCGenericImport<UISceneSessionClass, UISceneSession>) end;

  UISceneActivationConditionsClass = interface(NSObjectClass)
    ['{E55B0178-F2D3-4F66-855C-D1150C1488B4}']
  end;

  UISceneActivationConditions = interface(NSObject)
    ['{5753A318-23F4-428C-8033-7816B8CD759D}']
    function canActivateForTargetContentIdentifierPredicate: NSPredicate; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function prefersToActivateForTargetContentIdentifierPredicate: NSPredicate; cdecl;
    procedure setCanActivateForTargetContentIdentifierPredicate(canActivateForTargetContentIdentifierPredicate: NSPredicate); cdecl;
    procedure setPrefersToActivateForTargetContentIdentifierPredicate(prefersToActivateForTargetContentIdentifierPredicate: NSPredicate); cdecl;
  end;
  TUISceneActivationConditions = class(TOCGenericImport<UISceneActivationConditionsClass, UISceneActivationConditions>) end;

  UIOpenURLContextClass = interface(NSObjectClass)
    ['{9E7F5FB4-A069-445D-823F-63C39C6E8AFC}']
    {class} function new: Pointer; cdecl;
  end;

  UIOpenURLContext = interface(NSObject)
    ['{B8E23CD8-2AF8-4CE5-AB9F-1946B5D33D17}']
    function options: UISceneOpenURLOptions; cdecl;
    function URL: NSURL; cdecl;
  end;
  TUIOpenURLContext = class(TOCGenericImport<UIOpenURLContextClass, UIOpenURLContext>) end;

  UIStatusBarManagerClass = interface(NSObjectClass)
    ['{71F33528-88AC-4052-9B76-3A4A38327FA7}']
    {class} function new: Pointer; cdecl;
  end;

  UIStatusBarManager = interface(NSObject)
    ['{8A94F14E-C287-40A0-B80F-7D03BC6FF0EC}']
    function isStatusBarHidden: Boolean; cdecl;
    function statusBarFrame: CGRect; cdecl;
    function statusBarStyle: UIStatusBarStyle; cdecl;
  end;
  TUIStatusBarManager = class(TOCGenericImport<UIStatusBarManagerClass, UIStatusBarManager>) end;

  UIScreenshotServiceClass = interface(NSObjectClass)
    ['{C366A7C4-6445-428A-873D-09159C539781}']
    {class} function new: Pointer; cdecl;
  end;

  UIScreenshotService = interface(NSObject)
    ['{E0CEFFEA-986C-4FB4-9CE8-65BBC17B2743}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function windowScene: UIWindowScene; cdecl;
  end;
  TUIScreenshotService = class(TOCGenericImport<UIScreenshotServiceClass, UIScreenshotService>) end;

  UIScreenshotServiceDelegate = interface(IObjectiveC)
    ['{A590941F-587D-4D60-9C96-60F79EAB4D30}']
    procedure screenshotService(screenshotService: UIScreenshotService; generatePDFRepresentationWithCompletion: TUIScreenshotServiceDelegateBlockMethod1); cdecl;
  end;

  UIMenuBuilder = interface(IObjectiveC)
    ['{24149815-3B4A-4EEC-BA35-E3BBB6930E3B}']
    function actionForIdentifier(identifier: UIActionIdentifier): UIAction; cdecl;
    function commandForAction(action: Pointer; propertyList: Pointer): UICommand; cdecl;
    [MethodName('insertChildMenu:atEndOfMenuForIdentifier:')]
    procedure insertChildMenuAtEndOfMenuForIdentifier(childMenu: UIMenu; atEndOfMenuForIdentifier: UIMenuIdentifier); cdecl;
    [MethodName('insertChildMenu:atStartOfMenuForIdentifier:')]
    procedure insertChildMenuAtStartOfMenuForIdentifier(childMenu: UIMenu; atStartOfMenuForIdentifier: UIMenuIdentifier); cdecl;
    [MethodName('insertSiblingMenu:afterMenuForIdentifier:')]
    procedure insertSiblingMenuAfterMenuForIdentifier(siblingMenu: UIMenu; afterMenuForIdentifier: UIMenuIdentifier); cdecl;
    [MethodName('insertSiblingMenu:beforeMenuForIdentifier:')]
    procedure insertSiblingMenuBeforeMenuForIdentifier(siblingMenu: UIMenu; beforeMenuForIdentifier: UIMenuIdentifier); cdecl;
    function menuForIdentifier(identifier: UIMenuIdentifier): UIMenu; cdecl;
    procedure removeMenuForIdentifier(removedIdentifier: UIMenuIdentifier); cdecl;
    procedure replaceChildrenOfMenuForIdentifier(parentIdentifier: UIMenuIdentifier; fromChildrenBlock: TUIMenuBuilderBlockMethod1); cdecl;
    procedure replaceMenuForIdentifier(replacedIdentifier: UIMenuIdentifier; withMenu: UIMenu); cdecl;
    function system: UIMenuSystem; cdecl;
  end;

  UIMenuSystemClass = interface(NSObjectClass)
    ['{633DDEF9-61D8-4E3C-B7C3-249702D6D870}']
    {class} function contextSystem: UIMenuSystem; cdecl;
    {class} function mainSystem: UIMenuSystem; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIMenuSystem = interface(NSObject)
    ['{8CE12537-BF72-43FE-998E-DB7BB884E5BF}']
    procedure setNeedsRebuild; cdecl;
    procedure setNeedsRevalidate; cdecl;
  end;
  TUIMenuSystem = class(TOCGenericImport<UIMenuSystemClass, UIMenuSystem>) end;

  UITextFormattingCoordinatorDelegate = interface(IObjectiveC)
    ['{E0BC0066-D7FA-4EF3-9846-E1EED56000D4}']
    procedure updateTextAttributesWithConversionHandler(conversionHandler: UITextAttributesConversionHandler); cdecl;
  end;

  UITextFormattingCoordinatorClass = interface(NSObjectClass)
    ['{2979CF4B-62AD-460B-A49F-17A7F74517D2}']
    {class} function isFontPanelVisible: Boolean; cdecl;
    {class} function textFormattingCoordinatorForWindowScene(windowScene: UIWindowScene): Pointer; cdecl;
    {class} procedure toggleFontPanel(sender: Pointer); cdecl;
  end;

  UITextFormattingCoordinator = interface(NSObject)
    ['{7F9A2DA1-7656-4F7D-9382-F3F59C1DC232}']
    function delegate: Pointer; cdecl;
    function initWithWindowScene(windowScene: UIWindowScene): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setSelectedAttributes(attributes: NSDictionary; isMultiple: Boolean); cdecl;
  end;
  TUITextFormattingCoordinator = class(TOCGenericImport<UITextFormattingCoordinatorClass, UITextFormattingCoordinator>) end;

  UIPointerRegionClass = interface(NSObjectClass)
    ['{611E6E93-88D8-4F52-85E6-F10DE6F4A8C8}']
    {class} function new: Pointer; cdecl;
    {class} function regionWithRect(rect: CGRect; identifier: Pointer): Pointer; cdecl;
  end;

  UIPointerRegion = interface(NSObject)
    ['{34DC9711-676D-472F-B7A7-229C9B69E2FF}']
    function identifier: Pointer; cdecl;
    function rect: CGRect; cdecl;
  end;
  TUIPointerRegion = class(TOCGenericImport<UIPointerRegionClass, UIPointerRegion>) end;

  UIPointerStyleClass = interface(NSObjectClass)
    ['{B47A1AAC-91C9-4093-8E7F-4F1FFDDF66B8}']
    {class} function hiddenPointerStyle: Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function styleWithEffect(effect: UIPointerEffect; shape: UIPointerShape): Pointer; cdecl;
    {class} function styleWithShape(shape: UIPointerShape; constrainedAxes: UIAxis): Pointer; cdecl;
  end;

  UIPointerStyle = interface(NSObject)
    ['{C7D20FA4-2314-4ED0-9FA0-483FFDC4606F}']
  end;
  TUIPointerStyle = class(TOCGenericImport<UIPointerStyleClass, UIPointerStyle>) end;

  UIPointerEffectClass = interface(NSObjectClass)
    ['{F309DAFF-170F-4A7C-AEDB-4093EDE934FA}']
    {class} function effectWithPreview(preview: UITargetedPreview): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  UIPointerEffect = interface(NSObject)
    ['{A65EBB1A-055F-4B0D-8167-325CC708D716}']
    function preview: UITargetedPreview; cdecl;
  end;
  TUIPointerEffect = class(TOCGenericImport<UIPointerEffectClass, UIPointerEffect>) end;

  UIPointerHighlightEffectClass = interface(UIPointerEffectClass)
    ['{02A89837-C921-4856-8CE3-BDDA19449276}']
  end;

  UIPointerHighlightEffect = interface(UIPointerEffect)
    ['{64EB97A0-B46A-4C6B-ADB3-18D5F035E5F5}']
  end;
  TUIPointerHighlightEffect = class(TOCGenericImport<UIPointerHighlightEffectClass, UIPointerHighlightEffect>) end;

  UIPointerLiftEffectClass = interface(UIPointerEffectClass)
    ['{C96A0FB0-BF8F-49F1-B404-7BFD85EE9255}']
  end;

  UIPointerLiftEffect = interface(UIPointerEffect)
    ['{C502DAC8-BD0B-4FBF-9E98-11CA062D10DC}']
  end;
  TUIPointerLiftEffect = class(TOCGenericImport<UIPointerLiftEffectClass, UIPointerLiftEffect>) end;

  UIPointerHoverEffectClass = interface(UIPointerEffectClass)
    ['{DC5927BA-E9FE-4BC5-BDB9-D4F8779BCD84}']
  end;

  UIPointerHoverEffect = interface(UIPointerEffect)
    ['{4008DC61-5C90-4B3E-8806-9DE1A7684334}']
    function preferredTintMode: UIPointerEffectTintMode; cdecl;
    function prefersScaledContent: Boolean; cdecl;
    function prefersShadow: Boolean; cdecl;
    procedure setPreferredTintMode(preferredTintMode: UIPointerEffectTintMode); cdecl;
    procedure setPrefersScaledContent(prefersScaledContent: Boolean); cdecl;
    procedure setPrefersShadow(prefersShadow: Boolean); cdecl;
  end;
  TUIPointerHoverEffect = class(TOCGenericImport<UIPointerHoverEffectClass, UIPointerHoverEffect>) end;

  UIPointerShapeClass = interface(NSObjectClass)
    ['{78D7FD28-B527-49FB-B053-0AFAC6F6F578}']
    {class} function beamWithPreferredLength(length: CGFloat; axis: UIAxis): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function shapeWithPath(path: UIBezierPath): Pointer; cdecl;
    {class} function shapeWithRoundedRect(rect: CGRect; cornerRadius: CGFloat): Pointer; overload; cdecl;
    {class} function shapeWithRoundedRect(rect: CGRect): Pointer; overload; cdecl;
  end;

  UIPointerShape = interface(NSObject)
    ['{63E9E550-4845-4A06-BB81-65083B827D2D}']
  end;
  TUIPointerShape = class(TOCGenericImport<UIPointerShapeClass, UIPointerShape>) end;

  UIPointerInteractionClass = interface(NSObjectClass)
    ['{7849BBE3-2C76-46A9-A81B-3A13E858D311}']
  end;

  UIPointerInteraction = interface(NSObject)
    ['{3E79065D-6332-4768-A517-43CEB1512885}']
    function delegate: Pointer; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    procedure invalidate; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TUIPointerInteraction = class(TOCGenericImport<UIPointerInteractionClass, UIPointerInteraction>) end;

  UIPointerInteractionDelegate = interface(IObjectiveC)
    ['{04C51066-3FED-4392-98D7-8868CC8E2314}']
    [MethodName('pointerInteraction:regionForRequest:defaultRegion:')]
    function pointerInteractionRegionForRequest(interaction: UIPointerInteraction; regionForRequest: UIPointerRegionRequest; defaultRegion: UIPointerRegion): UIPointerRegion; cdecl;
    [MethodName('pointerInteraction:styleForRegion:')]
    function pointerInteractionStyleForRegion(interaction: UIPointerInteraction; styleForRegion: UIPointerRegion): UIPointerStyle; cdecl;
    [MethodName('pointerInteraction:willEnterRegion:animator:')]
    procedure pointerInteractionWillEnterRegion(interaction: UIPointerInteraction; willEnterRegion: UIPointerRegion; animator: Pointer); cdecl;
    [MethodName('pointerInteraction:willExitRegion:animator:')]
    procedure pointerInteractionWillExitRegion(interaction: UIPointerInteraction; willExitRegion: UIPointerRegion; animator: Pointer); cdecl;
  end;

  UIPointerRegionRequestClass = interface(NSObjectClass)
    ['{6F70D2D3-CB26-4877-8794-253642C98DC0}']
  end;

  UIPointerRegionRequest = interface(NSObject)
    ['{91E63B37-D805-4633-BCC9-4985A2A142A2}']
    function location: CGPoint; cdecl;
    function modifiers: UIKeyModifierFlags; cdecl;
  end;
  TUIPointerRegionRequest = class(TOCGenericImport<UIPointerRegionRequestClass, UIPointerRegionRequest>) end;

  UIPointerInteractionAnimating = interface(IObjectiveC)
    ['{DD808724-D9A2-4C1F-8973-2F9E6E0E9241}']
    procedure addAnimations(animations: TUIPointerInteractionAnimatingBlockMethod1); cdecl;
    procedure addCompletion(completion: TUIPointerInteractionAnimatingBlockMethod2); cdecl;
  end;

implementation

end.
