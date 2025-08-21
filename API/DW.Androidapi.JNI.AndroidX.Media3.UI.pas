unit DW.Androidapi.JNI.AndroidX.Media3.UI;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.JNI.Util,
  Androidapi.JNI.App,
  // DW
  DW.Androidapi.JNI.AndroidX.Media3.Common;

type
  JAspectRatioFrameLayout_AspectRatioListener = interface;
  JCaptionStyleCompat = interface;
  JPlayerControlView = interface;
  JPlayerControlView_OnFullScreenModeChangedListener = interface;
  JPlayerControlView_ProgressUpdateListener = interface;
  JPlayerControlView_VisibilityListener = interface;
  JPlayerNotificationManager = interface;
  JPlayerNotificationManager_BitmapCallback = interface;
  JPlayerNotificationManager_Builder = interface;
  JPlayerNotificationManager_CustomActionReceiver = interface;
  JPlayerNotificationManager_MediaDescriptionAdapter = interface;
  JPlayerNotificationManager_NotificationListener = interface;
  JPlayerNotificationManager_Priority = interface;
  JPlayerNotificationManager_Visibility = interface;
  JPlayerView = interface;
  JPlayerView_ControllerVisibilityListener = interface;
  JPlayerView_FullscreenButtonClickListener = interface;
  JSubtitleView = interface;

  JPlayerNotificationManagerClass = interface(JObjectClass)
    ['{8468AD30-39D3-4083-96DE-533C9700D5E4}']
    {class} function _GetACTION_FAST_FORWARD: JString; cdecl;
    {class} function _GetACTION_NEXT: JString; cdecl;
    {class} function _GetACTION_PAUSE: JString; cdecl;
    {class} function _GetACTION_PLAY: JString; cdecl;
    {class} function _GetACTION_PREVIOUS: JString; cdecl;
    {class} function _GetACTION_REWIND: JString; cdecl;
    {class} function _GetACTION_STOP: JString; cdecl;
    {class} function _GetEXTRA_INSTANCE_ID: JString; cdecl;
    {class} property ACTION_FAST_FORWARD: JString read _GetACTION_FAST_FORWARD;
    {class} property ACTION_NEXT: JString read _GetACTION_NEXT;
    {class} property ACTION_PAUSE: JString read _GetACTION_PAUSE;
    {class} property ACTION_PLAY: JString read _GetACTION_PLAY;
    {class} property ACTION_PREVIOUS: JString read _GetACTION_PREVIOUS;
    {class} property ACTION_REWIND: JString read _GetACTION_REWIND;
    {class} property ACTION_STOP: JString read _GetACTION_STOP;
    {class} property EXTRA_INSTANCE_ID: JString read _GetEXTRA_INSTANCE_ID;
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager')]
  JPlayerNotificationManager = interface(JObject)
    ['{B94BE2F0-2682-4223-8756-14E95BFE2EEF}']
    procedure invalidate; cdecl;
    procedure setBadgeIconType(int: Integer); cdecl;
    procedure setColor(int: Integer); cdecl;
    procedure setColorized(boolean: Boolean); cdecl;
    procedure setDefaults(int: Integer); cdecl;
    // procedure setMediaSessionToken(token: JMediaSessionCompat_Token); cdecl; // Deprecated anyway
    procedure setPlayer(player: JPlayer); cdecl;
    procedure setPriority(int: Integer); cdecl;
    procedure setShowPlayButtonIfPlaybackIsSuppressed(boolean: Boolean); cdecl;
    procedure setSmallIcon(int: Integer); cdecl;
    procedure setUseChronometer(boolean: Boolean); cdecl;
    procedure setUseFastForwardAction(boolean: Boolean); cdecl;
    procedure setUseFastForwardActionInCompactView(boolean: Boolean); cdecl;
    procedure setUseNextAction(boolean: Boolean); cdecl;
    procedure setUseNextActionInCompactView(boolean: Boolean); cdecl;
    procedure setUsePlayPauseActions(boolean: Boolean); cdecl;
    procedure setUsePreviousAction(boolean: Boolean); cdecl;
    procedure setUsePreviousActionInCompactView(boolean: Boolean); cdecl;
    procedure setUseRewindAction(boolean: Boolean); cdecl;
    procedure setUseRewindActionInCompactView(boolean: Boolean); cdecl;
    procedure setUseStopAction(boolean: Boolean); cdecl;
    procedure setVisibility(int: Integer); cdecl;
  end;
  TJPlayerNotificationManager = class(TJavaGenericImport<JPlayerNotificationManagerClass, JPlayerNotificationManager>) end;

  JPlayerNotificationManager_VisibilityClass = interface(JAnnotationClass)
    ['{D6A1820C-62D8-4858-9C72-606FA0A57F73}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$Visibility')]
  JPlayerNotificationManager_Visibility = interface(JAnnotation)
    ['{2DD4BB13-888D-44FD-BF14-CF6435565304}']
  end;
  TJPlayerNotificationManager_Visibility = class(TJavaGenericImport<JPlayerNotificationManager_VisibilityClass, JPlayerNotificationManager_Visibility>) end;

  JPlayerNotificationManager_PriorityClass = interface(JAnnotationClass)
    ['{405F9597-FDA3-4E20-A3CD-D630D84ACCEF}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$Priority')]
  JPlayerNotificationManager_Priority = interface(JAnnotation)
    ['{9BD654A9-FA5C-4DA4-9CAE-C7A698342008}']
  end;
  TJPlayerNotificationManager_Priority = class(TJavaGenericImport<JPlayerNotificationManager_PriorityClass, JPlayerNotificationManager_Priority>) end;

  JPlayerNotificationManager_NotificationListenerClass = interface(IJavaClass)
    ['{9344992E-93ED-408A-8DA6-63CF42D023A8}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$NotificationListener')]
  JPlayerNotificationManager_NotificationListener = interface(IJavaInstance)
    ['{D0C03015-6789-4F94-AB39-3E528B2DD41F}']
    procedure onNotificationCancelled(int: Integer; boolean: Boolean); cdecl;
    procedure onNotificationPosted(int: Integer; notification: JNotification; boolean: Boolean); cdecl;
  end;
  TJPlayerNotificationManager_NotificationListener = class(TJavaGenericImport<JPlayerNotificationManager_NotificationListenerClass,
    JPlayerNotificationManager_NotificationListener>) end;

  JPlayerNotificationManager_MediaDescriptionAdapterClass = interface(IJavaClass)
    ['{2FCA8182-A641-4240-8511-31F37340CFA0}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$MediaDescriptionAdapter')]
  JPlayerNotificationManager_MediaDescriptionAdapter = interface(IJavaInstance)
    ['{45A6646D-80C6-4509-A851-A19806BBC776}']
    function createCurrentContentIntent(player: JPlayer): JPendingIntent; cdecl;
    function getCurrentContentText(player: JPlayer): JCharSequence; cdecl;
    function getCurrentContentTitle(player: JPlayer): JCharSequence; cdecl;
    function getCurrentLargeIcon(player: JPlayer; bitmapCallback: JPlayerNotificationManager_BitmapCallback): JBitmap; cdecl;
    function getCurrentSubText(player: JPlayer): JCharSequence; cdecl;
  end;
  TJPlayerNotificationManager_MediaDescriptionAdapter = class(TJavaGenericImport<JPlayerNotificationManager_MediaDescriptionAdapterClass,
    JPlayerNotificationManager_MediaDescriptionAdapter>) end;

  JPlayerNotificationManager_CustomActionReceiverClass = interface(IJavaClass)
    ['{E5036534-7A48-4952-B3AC-4C0992A55A23}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$CustomActionReceiver')]
  JPlayerNotificationManager_CustomActionReceiver = interface(IJavaInstance)
    ['{54C4BF83-6D8D-427D-9511-0D731B3BC0A4}']
    function createCustomActions(context: JContext; int: Integer): JMap; cdecl;
    function getCustomActions(player: JPlayer): JList; cdecl;
    procedure onCustomAction(player: JPlayer; string_1: JString; intent: JIntent); cdecl;
  end;
  TJPlayerNotificationManager_CustomActionReceiver = class(TJavaGenericImport<JPlayerNotificationManager_CustomActionReceiverClass,
    JPlayerNotificationManager_CustomActionReceiver>) end;

  JPlayerNotificationManager_BuilderClass = interface(JObjectClass)
    ['{9170D7AB-EFE9-4A73-8BC9-70DEC61811CD}']
    {class} function init(context: JContext; int: Integer; string_1: JString;
      mediaDescriptionAdapter: JPlayerNotificationManager_MediaDescriptionAdapter): JPlayerNotificationManager_Builder; overload; cdecl;
    {class} function init(context: JContext; int: Integer; string_1: JString): JPlayerNotificationManager_Builder; overload; cdecl;
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$Builder')]
  JPlayerNotificationManager_Builder = interface(JObject)
    ['{A9C9593E-D8AB-4FCD-A743-FE7CE27AA358}']
    function build: JPlayerNotificationManager; cdecl;
    function setChannelDescriptionResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setChannelImportance(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setChannelNameResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setCustomActionReceiver(customActionReceiver: JPlayerNotificationManager_CustomActionReceiver): JPlayerNotificationManager_Builder; cdecl;
    function setFastForwardActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setGroup(string_1: JString): JPlayerNotificationManager_Builder; cdecl;
    function setMediaDescriptionAdapter(mediaDescriptionAdapter: JPlayerNotificationManager_MediaDescriptionAdapter): JPlayerNotificationManager_Builder; cdecl;
    function setNextActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setNotificationListener(notificationListener: JPlayerNotificationManager_NotificationListener): JPlayerNotificationManager_Builder; cdecl;
    function setPauseActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setPlayActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setPreviousActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setRewindActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setSmallIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
    function setStopActionIconResourceId(int: Integer): JPlayerNotificationManager_Builder; cdecl;
  end;
  TJPlayerNotificationManager_Builder = class(TJavaGenericImport<JPlayerNotificationManager_BuilderClass, JPlayerNotificationManager_Builder>) end;

  JPlayerNotificationManager_BitmapCallbackClass = interface(JObjectClass)
    ['{C006CB89-54F7-48B0-9465-12C5E1605490}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerNotificationManager$BitmapCallback')]
  JPlayerNotificationManager_BitmapCallback = interface(JObject)
    ['{B9FCA935-C3BE-43E0-BD65-019B0B7128A7}']
    procedure onBitmap(bitmap: JBitmap); cdecl;
  end;
  TJPlayerNotificationManager_BitmapCallback = class(TJavaGenericImport<JPlayerNotificationManager_BitmapCallbackClass, JPlayerNotificationManager_BitmapCallback>) end;

  JSubtitleViewClass = interface(JFrameLayoutClass)
    ['{0C297B81-6526-4DC7-8BCD-B7F5BFF6A5B3}']
    {class} function _GetDEFAULT_BOTTOM_PADDING_FRACTION: Single; cdecl;
    {class} function _GetDEFAULT_TEXT_SIZE_FRACTION: Single; cdecl;
    {class} function _GetVIEW_TYPE_CANVAS: Integer; cdecl;
    {class} function _GetVIEW_TYPE_WEB: Integer; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet): JSubtitleView; cdecl; overload;
    {class} function init(context: JContext): JSubtitleView; cdecl; overload;
    {class} property DEFAULT_BOTTOM_PADDING_FRACTION: Single read _GetDEFAULT_BOTTOM_PADDING_FRACTION;
    {class} property DEFAULT_TEXT_SIZE_FRACTION: Single read _GetDEFAULT_TEXT_SIZE_FRACTION;
    {class} property VIEW_TYPE_CANVAS: Integer read _GetVIEW_TYPE_CANVAS;
    {class} property VIEW_TYPE_WEB: Integer read _GetVIEW_TYPE_WEB;
  end;

  [JavaSignature('androidx/media3/ui/SubtitleView')]
  JSubtitleView = interface(JFrameLayout)
    ['{3D123B60-94B1-459A-B10A-C9F124CF1553}']
    procedure setApplyEmbeddedFontSizes(boolean: Boolean); cdecl;
    procedure setApplyEmbeddedStyles(boolean: Boolean); cdecl;
    procedure setBottomPaddingFraction(float: Single); cdecl;
    procedure setCues(list: JList); cdecl;
    procedure setFixedTextSize(int: Integer; float: Single); cdecl;
    procedure setFractionalTextSize(float: Single); cdecl; overload;
    procedure setFractionalTextSize(float: Single; boolean: Boolean); cdecl; overload;
    procedure setStyle(captionstylecompat: JCaptionStyleCompat); cdecl;
    procedure setUserDefaultStyle; cdecl;
    procedure setUserDefaultTextSize; cdecl;
    procedure setViewType(int: Integer); cdecl;
  end;
  TJSubtitleView = class(TJavaGenericImport<JSubtitleViewClass, JSubtitleView>) end;

  JPlayerViewClass = interface(JFrameLayoutClass)
    ['{6AAFAA9B-266C-435C-A047-CFB1273A068A}']
    {class} function _GetARTWORK_DISPLAY_MODE_FILL: Integer; cdecl;
    {class} function _GetARTWORK_DISPLAY_MODE_FIT: Integer; cdecl;
    {class} function _GetARTWORK_DISPLAY_MODE_OFF: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_ALWAYS: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_NEVER: Integer; cdecl;
    {class} function _GetSHOW_BUFFERING_WHEN_PLAYING: Integer; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet; int: Integer): JPlayerView; cdecl; overload;
    {class} function init(context: JContext; attributeset: JAttributeSet): JPlayerView; cdecl; overload;
    {class} function init(context: JContext): JPlayerView; cdecl; overload;
    {class} procedure switchTargetView(player: JPlayer; playerview: JPlayerView; playerview_1: JPlayerView); cdecl;
    {class} property ARTWORK_DISPLAY_MODE_FILL: Integer read _GetARTWORK_DISPLAY_MODE_FILL;
    {class} property ARTWORK_DISPLAY_MODE_FIT: Integer read _GetARTWORK_DISPLAY_MODE_FIT;
    {class} property ARTWORK_DISPLAY_MODE_OFF: Integer read _GetARTWORK_DISPLAY_MODE_OFF;
    {class} property SHOW_BUFFERING_ALWAYS: Integer read _GetSHOW_BUFFERING_ALWAYS;
    {class} property SHOW_BUFFERING_NEVER: Integer read _GetSHOW_BUFFERING_NEVER;
    {class} property SHOW_BUFFERING_WHEN_PLAYING: Integer read _GetSHOW_BUFFERING_WHEN_PLAYING;
  end;

  [JavaSignature('androidx/media3/ui/PlayerView')]
  JPlayerView = interface(JFrameLayout)
    ['{EED204DB-BD0C-4344-AD55-ADF3870EA5F2}']
    function dispatchKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function dispatchMediaKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function getAdOverlayInfos: JList; cdecl;
    function getAdViewGroup: JViewGroup; cdecl;
    function getArtworkDisplayMode: Integer; cdecl;
    function getControllerAutoShow: Boolean; cdecl;
    function getControllerHideOnTouch: Boolean; cdecl;
    function getControllerShowTimeoutMs: Integer; cdecl;
    function getDefaultArtwork: JDrawable; cdecl;
    function getOverlayFrameLayout: JFrameLayout; cdecl;
    function getPlayer: JPlayer; cdecl;
    function getResizeMode: Integer; cdecl;
    function getSubtitleView: JSubtitleView; cdecl;
    function getUseArtwork: Boolean; cdecl;
    function getUseController: Boolean; cdecl;
    function getVideoSurfaceView: JView; cdecl;
    procedure hideController; cdecl;
    function isControllerFullyVisible: Boolean; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    function onTrackballEvent(motionevent: JMotionEvent): Boolean; cdecl;
    function performClick: Boolean; cdecl;
    procedure setArtworkDisplayMode(int: Integer); cdecl;
    procedure setAspectRatioListener(aspectratiolistener: JAspectRatioFrameLayout_AspectRatioListener); cdecl;
    procedure setControllerAutoShow(boolean: Boolean); cdecl;
    procedure setControllerHideDuringAds(boolean: Boolean); cdecl;
    procedure setControllerHideOnTouch(boolean: Boolean); cdecl;
    procedure setControllerOnFullScreenModeChangedListener(onfullscreenmodechangedlistener: JPlayerControlView_OnFullScreenModeChangedListener); cdecl;
    procedure setControllerShowTimeoutMs(int: Integer); cdecl;
    procedure setControllerVisibilityListener(controllervisibilitylistener: JPlayerView_ControllerVisibilityListener); cdecl; overload;
    procedure setControllerVisibilityListener(visibilitylistener: JPlayerControlView_VisibilityListener); cdecl; overload;
    procedure setCustomErrorMessage(charsequence: JCharSequence); cdecl;
    procedure setDefaultArtwork(drawable: JDrawable); cdecl;
    procedure setErrorMessageProvider(errormessageprovider: JErrorMessageProvider); cdecl;
    procedure setExtraAdGroupMarkers(longs: TJavaArray<Int64>; booleans: TJavaArray<Boolean>); cdecl;
    procedure setFullscreenButtonClickListener(fullscreenbuttonclicklistener: JPlayerView_FullscreenButtonClickListener); cdecl;
    procedure setKeepContentOnPlayerReset(boolean: Boolean); cdecl;
    procedure setPlayer(player: JPlayer); cdecl;
    procedure setRepeatToggleModes(int: Integer); cdecl;
    procedure setResizeMode(int: Integer); cdecl;
    procedure setShowBuffering(int: Integer); cdecl;
    procedure setShowFastForwardButton(boolean: Boolean); cdecl;
    procedure setShowMultiWindowTimeBar(boolean: Boolean); cdecl;
    procedure setShowNextButton(boolean: Boolean); cdecl;
    procedure setShowPlayButtonIfPlaybackIsSuppressed(boolean: Boolean); cdecl;
    procedure setShowPreviousButton(boolean: Boolean); cdecl;
    procedure setShowRewindButton(boolean: Boolean); cdecl;
    procedure setShowShuffleButton(boolean: Boolean); cdecl;
    procedure setShowSubtitleButton(boolean: Boolean); cdecl;
    procedure setShowVrButton(boolean: Boolean); cdecl;
    procedure setShutterBackgroundColor(int: Integer); cdecl;
    procedure setUseArtwork(boolean: Boolean); cdecl;
    procedure setUseController(boolean: Boolean); cdecl;
    procedure setVisibility(int: Integer); cdecl;
    procedure showController; cdecl;
  end;
  TJPlayerView = class(TJavaGenericImport<JPlayerViewClass, JPlayerView>) end;

  JPlayerView_FullscreenButtonClickListenerClass = interface(IJavaClass)
    ['{A0E6331C-807D-4986-86EE-2FCA73472B27}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerView$FullscreenButtonClickListener')]
  JPlayerView_FullscreenButtonClickListener = interface(IJavaInstance)
    ['{4D1C500A-B46E-4559-A03C-1EA1F2AF8D60}']
    procedure onFullscreenButtonClick(boolean: Boolean); cdecl;
  end;
  TJPlayerView_FullscreenButtonClickListener = class(TJavaGenericImport<JPlayerView_FullscreenButtonClickListenerClass, JPlayerView_FullscreenButtonClickListener>) end;

  JPlayerView_ControllerVisibilityListenerClass = interface(IJavaClass)
    ['{20955CE7-0A1A-44C9-9CBE-39C5E1343348}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerView$ControllerVisibilityListener')]
  JPlayerView_ControllerVisibilityListener = interface(IJavaInstance)
    ['{D744AED8-7071-42E3-8082-843BA4E42629}']
    procedure onVisibilityChanged(int: Integer); cdecl;
  end;
  TJPlayerView_ControllerVisibilityListener = class(TJavaGenericImport<JPlayerView_ControllerVisibilityListenerClass, JPlayerView_ControllerVisibilityListener>) end;

  JPlayerControlViewClass = interface(JFrameLayoutClass)
    ['{34A5F6D1-3F29-485B-ACCE-4E50ACF63197}']
    {class} function _GetDEFAULT_REPEAT_TOGGLE_MODES: Integer; cdecl;
    {class} function _GetDEFAULT_SHOW_TIMEOUT_MS: Integer; cdecl;
    {class} function _GetDEFAULT_TIME_BAR_MIN_UPDATE_INTERVAL_MS: Integer; cdecl;
    {class} function _GetMAX_WINDOWS_FOR_MULTI_WINDOW_TIME_BAR: Integer; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet): JPlayerControlView; overload; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet; int: Integer): JPlayerControlView; overload; cdecl;
    {class} function init(context: JContext; attributeset: JAttributeSet; int: Integer; attributeset_1: JAttributeSet): JPlayerControlView; overload; cdecl;
    {class} function init(context: JContext): JPlayerControlView; overload; cdecl;
    {class} property DEFAULT_REPEAT_TOGGLE_MODES: Integer read _GetDEFAULT_REPEAT_TOGGLE_MODES;
    {class} property DEFAULT_SHOW_TIMEOUT_MS: Integer read _GetDEFAULT_SHOW_TIMEOUT_MS;
    {class} property DEFAULT_TIME_BAR_MIN_UPDATE_INTERVAL_MS: Integer read _GetDEFAULT_TIME_BAR_MIN_UPDATE_INTERVAL_MS;
    {class} property MAX_WINDOWS_FOR_MULTI_WINDOW_TIME_BAR: Integer read _GetMAX_WINDOWS_FOR_MULTI_WINDOW_TIME_BAR;
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView')]
  JPlayerControlView = interface(JFrameLayout)
    ['{7D21C814-FB6A-4C44-8128-008D488A7DA7}']
    procedure addVisibilityListener(visibilitylistener: JPlayerControlView_VisibilityListener); cdecl;
    function dispatchKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function dispatchMediaKeyEvent(keyevent: JKeyEvent): Boolean; cdecl;
    function getPlayer: JPlayer; cdecl;
    function getRepeatToggleModes: Integer; cdecl;
    function getShowShuffleButton: Boolean; cdecl;
    function getShowSubtitleButton: Boolean; cdecl;
    function getShowTimeoutMs: Integer; cdecl;
    function getShowVrButton: Boolean; cdecl;
    procedure hide; cdecl;
    procedure hideImmediately; cdecl;
    function isAnimationEnabled: Boolean; cdecl;
    function isFullyVisible: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure onAttachedToWindow; cdecl;
    procedure onDetachedFromWindow; cdecl;
    procedure removeVisibilityListener(visibilitylistener: JPlayerControlView_VisibilityListener); cdecl;
    procedure setAnimationEnabled(boolean: Boolean); cdecl;
    procedure setExtraAdGroupMarkers(longs: TJavaArray<Int64>; booleans: TJavaArray<Boolean>); cdecl;
    procedure setOnFullScreenModeChangedListener(onfullscreenmodechangedlistener: JPlayerControlView_OnFullScreenModeChangedListener); cdecl;
    procedure setPlayer(player: JPlayer); cdecl;
    procedure setProgressUpdateListener(progressupdatelistener: JPlayerControlView_ProgressUpdateListener); cdecl;
    procedure setRepeatToggleModes(int: Integer); cdecl;
    procedure setShowFastForwardButton(boolean: Boolean); cdecl;
    procedure setShowMultiWindowTimeBar(boolean: Boolean); cdecl;
    procedure setShowNextButton(boolean: Boolean); cdecl;
    procedure setShowPlayButtonIfPlaybackIsSuppressed(boolean: Boolean); cdecl;
    procedure setShowPreviousButton(boolean: Boolean); cdecl;
    procedure setShowRewindButton(boolean: Boolean); cdecl;
    procedure setShowShuffleButton(boolean: Boolean); cdecl;
    procedure setShowSubtitleButton(boolean: Boolean); cdecl;
    procedure setShowTimeoutMs(int: Integer); cdecl;
    procedure setShowVrButton(boolean: Boolean); cdecl;
    procedure setTimeBarMinUpdateInterval(int: Integer); cdecl;
    procedure setVrButtonListener(onclicklistener: JView_OnClickListener); cdecl;
    procedure show; cdecl;
  end;
  TJPlayerControlView = class(TJavaGenericImport<JPlayerControlViewClass, JPlayerControlView>) end;

  JPlayerControlView_VisibilityListenerClass = interface(IJavaClass)
    ['{16F03EBB-8CFC-4032-836E-C8BC96A25225}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView$VisibilityListener')]
  JPlayerControlView_VisibilityListener = interface(IJavaInstance)
    ['{9C957FA1-F38C-4588-8833-BD6755824C33}']
    procedure onVisibilityChange(int: Integer); cdecl;
  end;
  TJPlayerControlView_VisibilityListener = class(TJavaGenericImport<JPlayerControlView_VisibilityListenerClass, JPlayerControlView_VisibilityListener>) end;

  JPlayerControlView_OnFullScreenModeChangedListenerClass = interface(IJavaClass)
    ['{C2910F02-5A8B-4614-89A8-73C27312B21B}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView$OnFullScreenModeChangedListener')]
  JPlayerControlView_OnFullScreenModeChangedListener = interface(IJavaInstance)
    ['{D80AF4EF-29B2-43E2-B8B0-DDDF2D319947}']
    procedure onFullScreenModeChanged(boolean: Boolean); cdecl;
  end;
  TJPlayerControlView_OnFullScreenModeChangedListener = class(TJavaGenericImport<JPlayerControlView_OnFullScreenModeChangedListenerClass, JPlayerControlView_OnFullScreenModeChangedListener>) end;

  JPlayerControlView_ProgressUpdateListenerClass = interface(IJavaClass)
    ['{1FEF8744-3ABF-4D6C-A738-FACB5EB509E9}']
  end;

  [JavaSignature('androidx/media3/ui/PlayerControlView$ProgressUpdateListener')]
  JPlayerControlView_ProgressUpdateListener = interface(IJavaInstance)
    ['{E1739926-3039-45A7-BA5F-504C10920B89}']
    procedure onProgressUpdate(long: Int64; long_1: Int64); cdecl;
  end;
  TJPlayerControlView_ProgressUpdateListener = class(TJavaGenericImport<JPlayerControlView_ProgressUpdateListenerClass, JPlayerControlView_ProgressUpdateListener>) end;

  JCaptionStyleCompatClass = interface(JObjectClass)
    ['{FA96F5FE-32B9-4E10-9B24-5FA540DA6BDE}']
    {class} function _GetDEFAULT: JCaptionStyleCompat; cdecl;
    {class} function _GetEDGE_TYPE_DEPRESSED: Integer; cdecl;
    {class} function _GetEDGE_TYPE_DROP_SHADOW: Integer; cdecl;
    {class} function _GetEDGE_TYPE_NONE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_OUTLINE: Integer; cdecl;
    {class} function _GetEDGE_TYPE_RAISED: Integer; cdecl;
    {class} function _GetUSE_TRACK_COLOR_SETTINGS: Integer; cdecl;
    // {class} function createFromCaptionStyle(captionstyle: JCaptioningManager_CaptionStyle): JCaptionStyleCompat; cdecl;
    {class} function init(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; typeface: JTypeface): JCaptionStyleCompat; cdecl;
    {class} property &DEFAULT: JCaptionStyleCompat read _GetDEFAULT;
    {class} property EDGE_TYPE_DEPRESSED: Integer read _GetEDGE_TYPE_DEPRESSED;
    {class} property EDGE_TYPE_DROP_SHADOW: Integer read _GetEDGE_TYPE_DROP_SHADOW;
    {class} property EDGE_TYPE_NONE: Integer read _GetEDGE_TYPE_NONE;
    {class} property EDGE_TYPE_OUTLINE: Integer read _GetEDGE_TYPE_OUTLINE;
    {class} property EDGE_TYPE_RAISED: Integer read _GetEDGE_TYPE_RAISED;
    {class} property USE_TRACK_COLOR_SETTINGS: Integer read _GetUSE_TRACK_COLOR_SETTINGS;
  end;

  [JavaSignature('androidx/media3/ui/CaptionStyleCompat')]
  JCaptionStyleCompat = interface(JObject)
    ['{814A150F-2830-437F-B784-AA38BBCC82FE}']
    function _GetbackgroundColor: Integer; cdecl;
    function _GetedgeColor: Integer; cdecl;
    function _GetedgeType: Integer; cdecl;
    function _GetforegroundColor: Integer; cdecl;
    function _Gettypeface: JTypeface; cdecl;
    function _GetwindowColor: Integer; cdecl;
    property backgroundColor: Integer read _GetbackgroundColor;
    property edgeColor: Integer read _GetedgeColor;
    property edgeType: Integer read _GetedgeType;
    property foregroundColor: Integer read _GetforegroundColor;
    property typeface: JTypeface read _Gettypeface;
    property windowColor: Integer read _GetwindowColor;
  end;
  TJCaptionStyleCompat = class(TJavaGenericImport<JCaptionStyleCompatClass, JCaptionStyleCompat>) end;

  JAspectRatioFrameLayout_AspectRatioListenerClass = interface(IJavaClass)
    ['{CB837FB1-E42D-49D3-A3D6-37D446CD30A8}']
  end;

  [JavaSignature('androidx/media3/ui/AspectRatioFrameLayout$AspectRatioListener')]
  JAspectRatioFrameLayout_AspectRatioListener = interface(IJavaInstance)
    ['{469BE5FB-4B50-4CB9-9F50-77BE2488A2B7}']
    procedure onAspectRatioUpdated(float: Single; float_1: Single; boolean: Boolean); cdecl;
  end;
  TJAspectRatioFrameLayout_AspectRatioListener = class(TJavaGenericImport<JAspectRatioFrameLayout_AspectRatioListenerClass, JAspectRatioFrameLayout_AspectRatioListener>) end;

implementation

end.
