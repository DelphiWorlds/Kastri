unit DW.Androidapi.JNI.AndroidX.App;

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
  // Android
  Androidapi.JNIBridge, Androidapi.Jni.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Os,
  Androidapi.JNI.Widget, Androidapi.JNI.Net,
  // DW
  DW.Androidapi.JNI.AndroidX.Content;

type
  JNotificationBuilderWithBuilderAccessor = interface;
  JNotificationCompat = interface;
  JNotificationCompat_Action = interface;
  JNotificationCompat_Action_Builder = interface;
  JNotificationCompat_BigPictureStyle = interface;
  JNotificationCompat_BigTextStyle = interface;
  JNotificationCompat_BubbleMetadata = interface;
  JNotificationCompat_Builder = interface;
  JNotificationCompat_DecoratedCustomViewStyle = interface;
  JNotificationCompat_Extender = interface;
  JNotificationCompat_Style = interface;
  JRemoteInput = interface;

  JRemoteInputClass = interface(JObjectClass)
    ['{4AFD7682-C801-4A28-A256-4A1990F73FFE}']
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_AUTO: Integer; cdecl;
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_DISABLED: Integer; cdecl;
    {class} function _GetEDIT_CHOICES_BEFORE_SENDING_ENABLED: Integer; cdecl;
    {class} function _GetEXTRA_RESULTS_DATA: JString; cdecl;
    {class} function _GetRESULTS_CLIP_LABEL: JString; cdecl;
    {class} function _GetSOURCE_CHOICE: Integer; cdecl;
    {class} function _GetSOURCE_FREE_FORM_INPUT: Integer; cdecl;
    {class} procedure addDataResultToIntent(remoteInput: JRemoteInput; intent: JIntent; map: JMap); cdecl;
    {class} procedure addResultsToIntent(remoteInput: TJavaObjectArray<JRemoteInput>; intent: JIntent; bundle: JBundle); cdecl;
    {class} function getDataResultsFromIntent(intent: JIntent; string_: JString): JMap; cdecl;
    {class} function getResultsFromIntent(intent: JIntent): JBundle; cdecl;
    {class} function getResultsSource(intent: JIntent): Integer; cdecl;
    {class} procedure setResultsSource(intent: JIntent; i: Integer); cdecl;
    {class} property EDIT_CHOICES_BEFORE_SENDING_AUTO: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_AUTO;
    {class} property EDIT_CHOICES_BEFORE_SENDING_DISABLED: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_DISABLED;
    {class} property EDIT_CHOICES_BEFORE_SENDING_ENABLED: Integer read _GetEDIT_CHOICES_BEFORE_SENDING_ENABLED;
    {class} property EXTRA_RESULTS_DATA: JString read _GetEXTRA_RESULTS_DATA;
    {class} property RESULTS_CLIP_LABEL: JString read _GetRESULTS_CLIP_LABEL;
    {class} property SOURCE_CHOICE: Integer read _GetSOURCE_CHOICE;
    {class} property SOURCE_FREE_FORM_INPUT: Integer read _GetSOURCE_FREE_FORM_INPUT;
  end;

  [JavaSignature('androidx/core/app/RemoteInput')]
  JRemoteInput = interface(JObject)
    ['{734D869C-48D1-4828-81DF-2EDBBB88F105}']
    function getAllowFreeFormInput: Boolean; cdecl;
    function getAllowedDataTypes: JSet; cdecl;
    function getChoices: TJavaObjectArray<JCharSequence>; cdecl;
    function getEditChoicesBeforeSending: Integer; cdecl;
    function getExtras: JBundle; cdecl;
    function getLabel: JCharSequence; cdecl;
    function getResultKey: JString; cdecl;
    function isDataOnly: Boolean; cdecl;
  end;
  TJRemoteInput = class(TJavaGenericImport<JRemoteInputClass, JRemoteInput>) end;

  JNotificationCompatClass = interface(JObjectClass)
    ['{0FC7B36E-EE79-4878-9154-05C8A4D59C40}']
    {class} function _GetBADGE_ICON_LARGE: Integer; cdecl;
    {class} function _GetBADGE_ICON_NONE: Integer; cdecl;
    {class} function _GetBADGE_ICON_SMALL: Integer; cdecl;
    {class} function _GetCATEGORY_ALARM: JString; cdecl;
    {class} function _GetCATEGORY_CALL: JString; cdecl;
    {class} function _GetCATEGORY_EMAIL: JString; cdecl;
    {class} function _GetCATEGORY_ERROR: JString; cdecl;
    {class} function _GetCATEGORY_EVENT: JString; cdecl;
    {class} function _GetCATEGORY_LOCATION_SHARING: JString; cdecl;
    {class} function _GetCATEGORY_MESSAGE: JString; cdecl;
    {class} function _GetCATEGORY_MISSED_CALL: JString; cdecl;
    {class} function _GetCATEGORY_NAVIGATION: JString; cdecl;
    {class} function _GetCATEGORY_PROGRESS: JString; cdecl;
    {class} function _GetCATEGORY_PROMO: JString; cdecl;
    {class} function _GetCATEGORY_RECOMMENDATION: JString; cdecl;
    {class} function _GetCATEGORY_REMINDER: JString; cdecl;
    {class} function _GetCATEGORY_SERVICE: JString; cdecl;
    {class} function _GetCATEGORY_SOCIAL: JString; cdecl;
    {class} function _GetCATEGORY_STATUS: JString; cdecl;
    {class} function _GetCATEGORY_STOPWATCH: JString; cdecl;
    {class} function _GetCATEGORY_SYSTEM: JString; cdecl;
    {class} function _GetCATEGORY_TRANSPORT: JString; cdecl;
    {class} function _GetCATEGORY_VOICEMAIL: JString; cdecl;
    {class} function _GetCATEGORY_WORKOUT: JString; cdecl;
    {class} function _GetCOLOR_DEFAULT: Integer; cdecl;
    {class} function _GetDEFAULT_ALL: Integer; cdecl;
    {class} function _GetDEFAULT_LIGHTS: Integer; cdecl;
    {class} function _GetDEFAULT_SOUND: Integer; cdecl;
    {class} function _GetDEFAULT_VIBRATE: Integer; cdecl;
    {class} function _GetEXTRA_ANSWER_COLOR: JString; cdecl;
    {class} function _GetEXTRA_ANSWER_INTENT: JString; cdecl;
    {class} function _GetEXTRA_AUDIO_CONTENTS_URI: JString; cdecl;
    {class} function _GetEXTRA_BACKGROUND_IMAGE_URI: JString; cdecl;
    {class} function _GetEXTRA_BIG_TEXT: JString; cdecl;
    {class} function _GetEXTRA_CALL_IS_VIDEO: JString; cdecl;
    {class} function _GetEXTRA_CALL_PERSON: JString; cdecl;
    {class} function _GetEXTRA_CALL_PERSON_COMPAT: JString; cdecl;
    {class} function _GetEXTRA_CALL_TYPE: JString; cdecl;
    {class} function _GetEXTRA_CHANNEL_GROUP_ID: JString; cdecl;
    {class} function _GetEXTRA_CHANNEL_ID: JString; cdecl;
    {class} function _GetEXTRA_CHRONOMETER_COUNT_DOWN: JString; cdecl;
    {class} function _GetEXTRA_COLORIZED: JString; cdecl;
    {class} function _GetEXTRA_COMPACT_ACTIONS: JString; cdecl;
    {class} function _GetEXTRA_COMPAT_TEMPLATE: JString; cdecl;
    {class} function _GetEXTRA_CONVERSATION_TITLE: JString; cdecl;
    {class} function _GetEXTRA_DECLINE_COLOR: JString; cdecl;
    {class} function _GetEXTRA_DECLINE_INTENT: JString; cdecl;
    {class} function _GetEXTRA_HANG_UP_INTENT: JString; cdecl;
    {class} function _GetEXTRA_HIDDEN_CONVERSATION_TITLE: JString; cdecl;
    {class} function _GetEXTRA_HISTORIC_MESSAGES: JString; cdecl;
    {class} function _GetEXTRA_INFO_TEXT: JString; cdecl;
    {class} function _GetEXTRA_IS_GROUP_CONVERSATION: JString; cdecl;
    {class} function _GetEXTRA_LARGE_ICON: JString; cdecl;
    {class} function _GetEXTRA_LARGE_ICON_BIG: JString; cdecl;
    {class} function _GetEXTRA_MEDIA_SESSION: JString; cdecl;
    {class} function _GetEXTRA_MESSAGES: JString; cdecl;
    {class} function _GetEXTRA_MESSAGING_STYLE_USER: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_ID: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_TAG: JString; cdecl;
    {class} function _GetEXTRA_PEOPLE: JString; cdecl;
    {class} function _GetEXTRA_PEOPLE_LIST: JString; cdecl;
    {class} function _GetEXTRA_PICTURE: JString; cdecl;
    {class} function _GetEXTRA_PICTURE_CONTENT_DESCRIPTION: JString; cdecl;
    {class} function _GetEXTRA_PICTURE_ICON: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS_INDETERMINATE: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS_MAX: JString; cdecl;
    {class} function _GetEXTRA_REMOTE_INPUT_HISTORY: JString; cdecl;
    {class} function _GetEXTRA_SELF_DISPLAY_NAME: JString; cdecl;
    {class} function _GetEXTRA_SHOW_BIG_PICTURE_WHEN_COLLAPSED: JString; cdecl;
    {class} function _GetEXTRA_SHOW_CHRONOMETER: JString; cdecl;
    {class} function _GetEXTRA_SHOW_WHEN: JString; cdecl;
    {class} function _GetEXTRA_SMALL_ICON: JString; cdecl;
    {class} function _GetEXTRA_SUB_TEXT: JString; cdecl;
    {class} function _GetEXTRA_SUMMARY_TEXT: JString; cdecl;
    {class} function _GetEXTRA_TEMPLATE: JString; cdecl;
    {class} function _GetEXTRA_TEXT: JString; cdecl;
    {class} function _GetEXTRA_TEXT_LINES: JString; cdecl;
    {class} function _GetEXTRA_TITLE: JString; cdecl;
    {class} function _GetEXTRA_TITLE_BIG: JString; cdecl;
    {class} function _GetEXTRA_VERIFICATION_ICON: JString; cdecl;
    {class} function _GetEXTRA_VERIFICATION_ICON_COMPAT: JString; cdecl;
    {class} function _GetEXTRA_VERIFICATION_TEXT: JString; cdecl;
    {class} function _GetFLAG_AUTO_CANCEL: Integer; cdecl;
    {class} function _GetFLAG_BUBBLE: Integer; cdecl;
    {class} function _GetFLAG_FOREGROUND_SERVICE: Integer; cdecl;
    {class} function _GetFLAG_GROUP_SUMMARY: Integer; cdecl;
    {class} function _GetFLAG_HIGH_PRIORITY: Integer; cdecl;
    {class} function _GetFLAG_INSISTENT: Integer; cdecl;
    {class} function _GetFLAG_LOCAL_ONLY: Integer; cdecl;
    {class} function _GetFLAG_NO_CLEAR: Integer; cdecl;
    {class} function _GetFLAG_ONGOING_EVENT: Integer; cdecl;
    {class} function _GetFLAG_ONLY_ALERT_ONCE: Integer; cdecl;
    {class} function _GetFLAG_SHOW_LIGHTS: Integer; cdecl;
    {class} function _GetFOREGROUND_SERVICE_DEFAULT: Integer; cdecl;
    {class} function _GetFOREGROUND_SERVICE_DEFERRED: Integer; cdecl;
    {class} function _GetFOREGROUND_SERVICE_IMMEDIATE: Integer; cdecl;
    {class} function _GetGROUP_ALERT_ALL: Integer; cdecl;
    {class} function _GetGROUP_ALERT_CHILDREN: Integer; cdecl;
    {class} function _GetGROUP_ALERT_SUMMARY: Integer; cdecl;
    {class} function _GetGROUP_KEY_SILENT: JString; cdecl;
    {class} function _GetINTENT_CATEGORY_NOTIFICATION_PREFERENCES: JString; cdecl;
    {class} function _GetMAX_ACTION_BUTTONS: Integer; cdecl;
    {class} function _GetPRIORITY_DEFAULT: Integer; cdecl;
    {class} function _GetPRIORITY_HIGH: Integer; cdecl;
    {class} function _GetPRIORITY_LOW: Integer; cdecl;
    {class} function _GetPRIORITY_MAX: Integer; cdecl;
    {class} function _GetPRIORITY_MIN: Integer; cdecl;
    {class} function _GetSTREAM_DEFAULT: Integer; cdecl;
    {class} function _GetVISIBILITY_PRIVATE: Integer; cdecl;
    {class} function _GetVISIBILITY_PUBLIC: Integer; cdecl;
    {class} function _GetVISIBILITY_SECRET: Integer; cdecl;
    {class} function init: JNotificationCompat; cdecl;
    {class} function getAction(notification: JNotification; i: Integer): JNotificationCompat_Action; cdecl;
    {class} function getActionCount(notification: JNotification): Integer; cdecl;
    {class} function getAllowSystemGeneratedContextualActions(notification: JNotification): Boolean; cdecl;
    {class} function getAutoCancel(notification: JNotification): Boolean; cdecl;
    {class} function getBadgeIconType(notification: JNotification): Integer; cdecl;
    {class} function getBubbleMetadata(notification: JNotification): JNotificationCompat_BubbleMetadata; cdecl;
    {class} function getCategory(notification: JNotification): JString; cdecl;
    {class} function getChannelId(notification: JNotification): JString; cdecl;
    {class} function getColor(notification: JNotification): Integer; cdecl;
    {class} function getContentInfo(notification: JNotification): JCharSequence; cdecl;
    {class} function getContentText(notification: JNotification): JCharSequence; cdecl;
    {class} function getContentTitle(notification: JNotification): JCharSequence; cdecl;
    {class} function getExtras(notification: JNotification): JBundle; cdecl;
    {class} function getGroup(notification: JNotification): JString; cdecl;
    {class} function getGroupAlertBehavior(notification: JNotification): Integer; cdecl;
    {class} function getInvisibleActions(notification: JNotification): JList; cdecl;
    {class} function getLocalOnly(notification: JNotification): Boolean; cdecl;
    {class} function getLocusId(notification: JNotification): JLocusIdCompat; cdecl;
    {class} function getOngoing(notification: JNotification): Boolean; cdecl;
    {class} function getOnlyAlertOnce(notification: JNotification): Boolean; cdecl;
    {class} function getPeople(notification: JNotification): JList; cdecl;
    {class} function getPublicVersion(notification: JNotification): JNotification; cdecl;
    {class} function getSettingsText(notification: JNotification): JCharSequence; cdecl;
    {class} function getShortcutId(notification: JNotification): JString; cdecl;
    {class} function getShowWhen(notification: JNotification): Boolean; cdecl;
    {class} function getSortKey(notification: JNotification): JString; cdecl;
    {class} function getSubText(notification: JNotification): JCharSequence; cdecl;
    {class} function getTimeoutAfter(notification: JNotification): Int64; cdecl;
    {class} function getUsesChronometer(notification: JNotification): Boolean; cdecl;
    {class} function getVisibility(notification: JNotification): Integer; cdecl;
    {class} function isGroupSummary(notification: JNotification): Boolean; cdecl;
    {class} function reduceLargeIconSize(context: JContext; bitmap: JBitmap): JBitmap; cdecl;
    {class} property BADGE_ICON_LARGE: Integer read _GetBADGE_ICON_LARGE;
    {class} property BADGE_ICON_NONE: Integer read _GetBADGE_ICON_NONE;
    {class} property BADGE_ICON_SMALL: Integer read _GetBADGE_ICON_SMALL;
    {class} property CATEGORY_ALARM: JString read _GetCATEGORY_ALARM;
    {class} property CATEGORY_CALL: JString read _GetCATEGORY_CALL;
    {class} property CATEGORY_EMAIL: JString read _GetCATEGORY_EMAIL;
    {class} property CATEGORY_ERROR: JString read _GetCATEGORY_ERROR;
    {class} property CATEGORY_EVENT: JString read _GetCATEGORY_EVENT;
    {class} property CATEGORY_LOCATION_SHARING: JString read _GetCATEGORY_LOCATION_SHARING;
    {class} property CATEGORY_MESSAGE: JString read _GetCATEGORY_MESSAGE;
    {class} property CATEGORY_MISSED_CALL: JString read _GetCATEGORY_MISSED_CALL;
    {class} property CATEGORY_NAVIGATION: JString read _GetCATEGORY_NAVIGATION;
    {class} property CATEGORY_PROGRESS: JString read _GetCATEGORY_PROGRESS;
    {class} property CATEGORY_PROMO: JString read _GetCATEGORY_PROMO;
    {class} property CATEGORY_RECOMMENDATION: JString read _GetCATEGORY_RECOMMENDATION;
    {class} property CATEGORY_REMINDER: JString read _GetCATEGORY_REMINDER;
    {class} property CATEGORY_SERVICE: JString read _GetCATEGORY_SERVICE;
    {class} property CATEGORY_SOCIAL: JString read _GetCATEGORY_SOCIAL;
    {class} property CATEGORY_STATUS: JString read _GetCATEGORY_STATUS;
    {class} property CATEGORY_STOPWATCH: JString read _GetCATEGORY_STOPWATCH;
    {class} property CATEGORY_SYSTEM: JString read _GetCATEGORY_SYSTEM;
    {class} property CATEGORY_TRANSPORT: JString read _GetCATEGORY_TRANSPORT;
    {class} property CATEGORY_VOICEMAIL: JString read _GetCATEGORY_VOICEMAIL;
    {class} property CATEGORY_WORKOUT: JString read _GetCATEGORY_WORKOUT;
    {class} property COLOR_DEFAULT: Integer read _GetCOLOR_DEFAULT;
    {class} property DEFAULT_ALL: Integer read _GetDEFAULT_ALL;
    {class} property DEFAULT_LIGHTS: Integer read _GetDEFAULT_LIGHTS;
    {class} property DEFAULT_SOUND: Integer read _GetDEFAULT_SOUND;
    {class} property DEFAULT_VIBRATE: Integer read _GetDEFAULT_VIBRATE;
    {class} property EXTRA_ANSWER_COLOR: JString read _GetEXTRA_ANSWER_COLOR;
    {class} property EXTRA_ANSWER_INTENT: JString read _GetEXTRA_ANSWER_INTENT;
    {class} property EXTRA_AUDIO_CONTENTS_URI: JString read _GetEXTRA_AUDIO_CONTENTS_URI;
    {class} property EXTRA_BACKGROUND_IMAGE_URI: JString read _GetEXTRA_BACKGROUND_IMAGE_URI;
    {class} property EXTRA_BIG_TEXT: JString read _GetEXTRA_BIG_TEXT;
    {class} property EXTRA_CALL_IS_VIDEO: JString read _GetEXTRA_CALL_IS_VIDEO;
    {class} property EXTRA_CALL_PERSON: JString read _GetEXTRA_CALL_PERSON;
    {class} property EXTRA_CALL_PERSON_COMPAT: JString read _GetEXTRA_CALL_PERSON_COMPAT;
    {class} property EXTRA_CALL_TYPE: JString read _GetEXTRA_CALL_TYPE;
    {class} property EXTRA_CHANNEL_GROUP_ID: JString read _GetEXTRA_CHANNEL_GROUP_ID;
    {class} property EXTRA_CHANNEL_ID: JString read _GetEXTRA_CHANNEL_ID;
    {class} property EXTRA_CHRONOMETER_COUNT_DOWN: JString read _GetEXTRA_CHRONOMETER_COUNT_DOWN;
    {class} property EXTRA_COLORIZED: JString read _GetEXTRA_COLORIZED;
    {class} property EXTRA_COMPACT_ACTIONS: JString read _GetEXTRA_COMPACT_ACTIONS;
    {class} property EXTRA_COMPAT_TEMPLATE: JString read _GetEXTRA_COMPAT_TEMPLATE;
    {class} property EXTRA_CONVERSATION_TITLE: JString read _GetEXTRA_CONVERSATION_TITLE;
    {class} property EXTRA_DECLINE_COLOR: JString read _GetEXTRA_DECLINE_COLOR;
    {class} property EXTRA_DECLINE_INTENT: JString read _GetEXTRA_DECLINE_INTENT;
    {class} property EXTRA_HANG_UP_INTENT: JString read _GetEXTRA_HANG_UP_INTENT;
    {class} property EXTRA_HIDDEN_CONVERSATION_TITLE: JString read _GetEXTRA_HIDDEN_CONVERSATION_TITLE;
    {class} property EXTRA_HISTORIC_MESSAGES: JString read _GetEXTRA_HISTORIC_MESSAGES;
    {class} property EXTRA_INFO_TEXT: JString read _GetEXTRA_INFO_TEXT;
    {class} property EXTRA_IS_GROUP_CONVERSATION: JString read _GetEXTRA_IS_GROUP_CONVERSATION;
    {class} property EXTRA_LARGE_ICON: JString read _GetEXTRA_LARGE_ICON;
    {class} property EXTRA_LARGE_ICON_BIG: JString read _GetEXTRA_LARGE_ICON_BIG;
    {class} property EXTRA_MEDIA_SESSION: JString read _GetEXTRA_MEDIA_SESSION;
    {class} property EXTRA_MESSAGES: JString read _GetEXTRA_MESSAGES;
    {class} property EXTRA_MESSAGING_STYLE_USER: JString read _GetEXTRA_MESSAGING_STYLE_USER;
    {class} property EXTRA_NOTIFICATION_ID: JString read _GetEXTRA_NOTIFICATION_ID;
    {class} property EXTRA_NOTIFICATION_TAG: JString read _GetEXTRA_NOTIFICATION_TAG;
    {class} property EXTRA_PEOPLE: JString read _GetEXTRA_PEOPLE;
    {class} property EXTRA_PEOPLE_LIST: JString read _GetEXTRA_PEOPLE_LIST;
    {class} property EXTRA_PICTURE: JString read _GetEXTRA_PICTURE;
    {class} property EXTRA_PICTURE_CONTENT_DESCRIPTION: JString read _GetEXTRA_PICTURE_CONTENT_DESCRIPTION;
    {class} property EXTRA_PICTURE_ICON: JString read _GetEXTRA_PICTURE_ICON;
    {class} property EXTRA_PROGRESS: JString read _GetEXTRA_PROGRESS;
    {class} property EXTRA_PROGRESS_INDETERMINATE: JString read _GetEXTRA_PROGRESS_INDETERMINATE;
    {class} property EXTRA_PROGRESS_MAX: JString read _GetEXTRA_PROGRESS_MAX;
    {class} property EXTRA_REMOTE_INPUT_HISTORY: JString read _GetEXTRA_REMOTE_INPUT_HISTORY;
    {class} property EXTRA_SELF_DISPLAY_NAME: JString read _GetEXTRA_SELF_DISPLAY_NAME;
    {class} property EXTRA_SHOW_BIG_PICTURE_WHEN_COLLAPSED: JString read _GetEXTRA_SHOW_BIG_PICTURE_WHEN_COLLAPSED;
    {class} property EXTRA_SHOW_CHRONOMETER: JString read _GetEXTRA_SHOW_CHRONOMETER;
    {class} property EXTRA_SHOW_WHEN: JString read _GetEXTRA_SHOW_WHEN;
    {class} property EXTRA_SMALL_ICON: JString read _GetEXTRA_SMALL_ICON;
    {class} property EXTRA_SUB_TEXT: JString read _GetEXTRA_SUB_TEXT;
    {class} property EXTRA_SUMMARY_TEXT: JString read _GetEXTRA_SUMMARY_TEXT;
    {class} property EXTRA_TEMPLATE: JString read _GetEXTRA_TEMPLATE;
    {class} property EXTRA_TEXT: JString read _GetEXTRA_TEXT;
    {class} property EXTRA_TEXT_LINES: JString read _GetEXTRA_TEXT_LINES;
    {class} property EXTRA_TITLE: JString read _GetEXTRA_TITLE;
    {class} property EXTRA_TITLE_BIG: JString read _GetEXTRA_TITLE_BIG;
    {class} property EXTRA_VERIFICATION_ICON: JString read _GetEXTRA_VERIFICATION_ICON;
    {class} property EXTRA_VERIFICATION_ICON_COMPAT: JString read _GetEXTRA_VERIFICATION_ICON_COMPAT;
    {class} property EXTRA_VERIFICATION_TEXT: JString read _GetEXTRA_VERIFICATION_TEXT;
    {class} property FLAG_AUTO_CANCEL: Integer read _GetFLAG_AUTO_CANCEL;
    {class} property FLAG_BUBBLE: Integer read _GetFLAG_BUBBLE;
    {class} property FLAG_FOREGROUND_SERVICE: Integer read _GetFLAG_FOREGROUND_SERVICE;
    {class} property FLAG_GROUP_SUMMARY: Integer read _GetFLAG_GROUP_SUMMARY;
    {class} property FLAG_HIGH_PRIORITY: Integer read _GetFLAG_HIGH_PRIORITY;
    {class} property FLAG_INSISTENT: Integer read _GetFLAG_INSISTENT;
    {class} property FLAG_LOCAL_ONLY: Integer read _GetFLAG_LOCAL_ONLY;
    {class} property FLAG_NO_CLEAR: Integer read _GetFLAG_NO_CLEAR;
    {class} property FLAG_ONGOING_EVENT: Integer read _GetFLAG_ONGOING_EVENT;
    {class} property FLAG_ONLY_ALERT_ONCE: Integer read _GetFLAG_ONLY_ALERT_ONCE;
    {class} property FLAG_SHOW_LIGHTS: Integer read _GetFLAG_SHOW_LIGHTS;
    {class} property FOREGROUND_SERVICE_DEFAULT: Integer read _GetFOREGROUND_SERVICE_DEFAULT;
    {class} property FOREGROUND_SERVICE_DEFERRED: Integer read _GetFOREGROUND_SERVICE_DEFERRED;
    {class} property FOREGROUND_SERVICE_IMMEDIATE: Integer read _GetFOREGROUND_SERVICE_IMMEDIATE;
    {class} property GROUP_ALERT_ALL: Integer read _GetGROUP_ALERT_ALL;
    {class} property GROUP_ALERT_CHILDREN: Integer read _GetGROUP_ALERT_CHILDREN;
    {class} property GROUP_ALERT_SUMMARY: Integer read _GetGROUP_ALERT_SUMMARY;
    {class} property GROUP_KEY_SILENT: JString read _GetGROUP_KEY_SILENT;
    {class} property INTENT_CATEGORY_NOTIFICATION_PREFERENCES: JString read _GetINTENT_CATEGORY_NOTIFICATION_PREFERENCES;
    {class} property MAX_ACTION_BUTTONS: Integer read _GetMAX_ACTION_BUTTONS;
    {class} property PRIORITY_DEFAULT: Integer read _GetPRIORITY_DEFAULT;
    {class} property PRIORITY_HIGH: Integer read _GetPRIORITY_HIGH;
    {class} property PRIORITY_LOW: Integer read _GetPRIORITY_LOW;
    {class} property PRIORITY_MAX: Integer read _GetPRIORITY_MAX;
    {class} property PRIORITY_MIN: Integer read _GetPRIORITY_MIN;
    {class} property STREAM_DEFAULT: Integer read _GetSTREAM_DEFAULT;
    {class} property VISIBILITY_PRIVATE: Integer read _GetVISIBILITY_PRIVATE;
    {class} property VISIBILITY_PUBLIC: Integer read _GetVISIBILITY_PUBLIC;
    {class} property VISIBILITY_SECRET: Integer read _GetVISIBILITY_SECRET;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat')]
  JNotificationCompat = interface(JObject)
    ['{DD6DDFDB-5E4F-474C-A5FF-0C85AB1DE424}']
  end;
  TJNotificationCompat = class(TJavaGenericImport<JNotificationCompatClass, JNotificationCompat>) end;

  JNotificationCompat_BubbleMetadataClass = interface(JObjectClass)
    ['{33DACB07-1885-4B88-8DF1-C32E27638D14}']
    {class} // function fromPlatform(bubbleMetadata: JNotification_BubbleMetadata): JNotificationCompat_BubbleMetadata; cdecl;
    {class} // function toPlatform(bubbleMetadata: JNotificationCompat_BubbleMetadata): JNotification_BubbleMetadata; cdecl;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$BubbleMetadata')]
  JNotificationCompat_BubbleMetadata = interface(JObject)
    ['{D9B394E6-0C5F-4CEE-91DA-64ADF5B4C9AF}']
    function getAutoExpandBubble: Boolean; cdecl;
    function getDeleteIntent: JPendingIntent; cdecl;
    function getDesiredHeight: Integer; cdecl;
    function getDesiredHeightResId: Integer; cdecl;
    // function getIcon: Jdrawable_IconCompat; cdecl;
    function getIntent: JPendingIntent; cdecl;
    function getShortcutId: JString; cdecl;
    function isNotificationSuppressed: Boolean; cdecl;
    procedure setFlags(i: Integer); cdecl;
  end;
  TJNotificationCompat_BubbleMetadata = class(TJavaGenericImport<JNotificationCompat_BubbleMetadataClass, JNotificationCompat_BubbleMetadata>) end;

  JNotificationCompat_ActionClass = interface(JObjectClass)
    ['{069FC3BA-E978-4659-A8D7-7EEC6D4F9652}']
    {class} function _GetSEMANTIC_ACTION_ARCHIVE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_CALL: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_DELETE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MARK_AS_READ: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MARK_AS_UNREAD: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_MUTE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_NONE: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_REPLY: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_THUMBS_DOWN: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_THUMBS_UP: Integer; cdecl;
    {class} function _GetSEMANTIC_ACTION_UNMUTE: Integer; cdecl;
    {class} function init(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action; cdecl; overload;
    {class} // function init(iconCompat: Jdrawable_IconCompat; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action; cdecl; overload;
    {class} property SEMANTIC_ACTION_ARCHIVE: Integer read _GetSEMANTIC_ACTION_ARCHIVE;
    {class} property SEMANTIC_ACTION_CALL: Integer read _GetSEMANTIC_ACTION_CALL;
    {class} property SEMANTIC_ACTION_DELETE: Integer read _GetSEMANTIC_ACTION_DELETE;
    {class} property SEMANTIC_ACTION_MARK_AS_READ: Integer read _GetSEMANTIC_ACTION_MARK_AS_READ;
    {class} property SEMANTIC_ACTION_MARK_AS_UNREAD: Integer read _GetSEMANTIC_ACTION_MARK_AS_UNREAD;
    {class} property SEMANTIC_ACTION_MUTE: Integer read _GetSEMANTIC_ACTION_MUTE;
    {class} property SEMANTIC_ACTION_NONE: Integer read _GetSEMANTIC_ACTION_NONE;
    {class} property SEMANTIC_ACTION_REPLY: Integer read _GetSEMANTIC_ACTION_REPLY;
    {class} property SEMANTIC_ACTION_THUMBS_DOWN: Integer read _GetSEMANTIC_ACTION_THUMBS_DOWN;
    {class} property SEMANTIC_ACTION_THUMBS_UP: Integer read _GetSEMANTIC_ACTION_THUMBS_UP;
    {class} property SEMANTIC_ACTION_UNMUTE: Integer read _GetSEMANTIC_ACTION_UNMUTE;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Action')]
  JNotificationCompat_Action = interface(JObject)
    ['{57074D9D-6157-4142-A2C1-D61ED3F54404}']
    function _GetactionIntent: JPendingIntent; cdecl;
    procedure _SetactionIntent(Value: JPendingIntent); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _Gettitle: JCharSequence; cdecl;
    procedure _Settitle(Value: JCharSequence); cdecl;
    function getActionIntent: JPendingIntent; cdecl;
    function getAllowGeneratedReplies: Boolean; cdecl;
    function getDataOnlyRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    function getExtras: JBundle; cdecl;
    function getIcon: Integer; cdecl;
    // function getIconCompat: Jdrawable_IconCompat; cdecl;
    function getRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    function getSemanticAction: Integer; cdecl;
    function getShowsUserInterface: Boolean; cdecl;
    function getTitle: JCharSequence; cdecl;
    function isContextual: Boolean; cdecl;
    property actionIntent: JPendingIntent read _GetactionIntent write _SetactionIntent;
    property icon: Integer read _Geticon write _Seticon;
    property title: JCharSequence read _Gettitle write _Settitle;
  end;
  TJNotificationCompat_Action = class(TJavaGenericImport<JNotificationCompat_ActionClass, JNotificationCompat_Action>) end;

  JNotificationCompat_Action_BuilderClass = interface(JObjectClass)
    ['{F0E1B9CC-F8E5-4E2C-A1C9-933B3C61D3C8}']
    // {class} function init(iconCompat: JIconCompat; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action_Builder; cdecl; overload;
    {class} function init(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Action_Builder; cdecl; overload;
    {class} function init(action: JNotificationCompat_Action): JNotificationCompat_Action_Builder; cdecl; overload;
    {class} function fromAndroidAction(action: JNotification_Action): JNotificationCompat_Action_Builder; cdecl;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Action$Builder')]
  JNotificationCompat_Action_Builder = interface(JObject)
    ['{A190E5D6-BBDC-4B20-8456-30C7C48E6FAD}']
    function addExtras(bundle: JBundle): JNotificationCompat_Action_Builder; cdecl;
    function addRemoteInput(remoteInput: JRemoteInput): JNotificationCompat_Action_Builder; cdecl;
    function build: JNotificationCompat_Action; cdecl;
    // function extend(extender: JNotificationCompat_Action_Extender): JNotificationCompat_Action_Builder; cdecl;
    function getExtras: JBundle; cdecl;
    function setAllowGeneratedReplies(b: Boolean): JNotificationCompat_Action_Builder; cdecl;
    function setAuthenticationRequired(b: Boolean): JNotificationCompat_Action_Builder; cdecl;
    function setContextual(b: Boolean): JNotificationCompat_Action_Builder; cdecl;
    function setSemanticAction(i: Integer): JNotificationCompat_Action_Builder; cdecl;
    function setShowsUserInterface(b: Boolean): JNotificationCompat_Action_Builder; cdecl;
  end;
  TJNotificationCompat_Action_Builder = class(TJavaGenericImport<JNotificationCompat_Action_BuilderClass, JNotificationCompat_Action_Builder>) end;

  JNotificationCompat_BuilderClass = interface(JObjectClass)
    ['{BF06ED8F-16CF-4D2D-A029-4A43FD71907F}']
    {class} function init(context: JContext): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext; notification: JNotification): JNotificationCompat_Builder; cdecl; overload;
    {class} function init(context: JContext; string_: JString): JNotificationCompat_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Builder')]
  JNotificationCompat_Builder = interface(JObject)
    ['{86BE52A4-03F6-4537-98A9-F94A2266274F}']
    function _GetmActions: JArrayList; cdecl;
    procedure _SetmActions(Value: JArrayList); cdecl;
    function _GetmContext: JContext; cdecl;
    procedure _SetmContext(Value: JContext); cdecl;
    function _GetmPeople: JArrayList; cdecl;
    procedure _SetmPeople(Value: JArrayList); cdecl;
    function _GetmPersonList: JArrayList; cdecl;
    procedure _SetmPersonList(Value: JArrayList); cdecl;
    function addAction(action: JNotificationCompat_Action): JNotificationCompat_Builder; cdecl; overload;
    function addAction(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl; overload;
    function addExtras(bundle: JBundle): JNotificationCompat_Builder; cdecl;
    function addInvisibleAction(action: JNotificationCompat_Action): JNotificationCompat_Builder; cdecl; overload;
    function addInvisibleAction(i: Integer; charSequence: JCharSequence; pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl; overload;
    // function addPerson(person: JPerson): JNotificationCompat_Builder; cdecl; overload;
    function addPerson(string_: JString): JNotificationCompat_Builder; cdecl; overload;
    function build: JNotification; cdecl;
    function clearActions: JNotificationCompat_Builder; cdecl;
    function clearInvisibleActions: JNotificationCompat_Builder; cdecl;
    function clearPeople: JNotificationCompat_Builder; cdecl;
    function createBigContentView: JRemoteViews; cdecl;
    function createContentView: JRemoteViews; cdecl;
    function createHeadsUpContentView: JRemoteViews; cdecl;
    function extend(extender: JNotificationCompat_Extender): JNotificationCompat_Builder; cdecl;
    function getBigContentView: JRemoteViews; cdecl;
    function getBubbleMetadata: JNotificationCompat_BubbleMetadata; cdecl;
    function getColor: Integer; cdecl;
    function getContentView: JRemoteViews; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadsUpContentView: JRemoteViews; cdecl;
    function getNotification: JNotification; cdecl;
    function getPriority: Integer; cdecl;
    function getWhenIfShowing: Int64; cdecl;
    function setAllowSystemGeneratedContextualActions(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setAutoCancel(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setBadgeIconType(i: Integer): JNotificationCompat_Builder; cdecl;
    function setBubbleMetadata(bubbleMetadata: JNotificationCompat_BubbleMetadata): JNotificationCompat_Builder; cdecl;
    function setCategory(string_: JString): JNotificationCompat_Builder; cdecl;
    function setChannelId(string_: JString): JNotificationCompat_Builder; cdecl;
    function setChronometerCountDown(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setColor(i: Integer): JNotificationCompat_Builder; cdecl;
    function setColorized(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setContent(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setContentInfo(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentIntent(pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setContentText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentTitle(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setCustomBigContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setCustomHeadsUpContentView(remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setDefaults(i: Integer): JNotificationCompat_Builder; cdecl;
    function setDeleteIntent(pendingIntent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setExtras(bundle: JBundle): JNotificationCompat_Builder; cdecl;
    function setFullScreenIntent(pendingIntent: JPendingIntent; b: Boolean): JNotificationCompat_Builder; cdecl;
    function setGroup(string_: JString): JNotificationCompat_Builder; cdecl;
    function setGroupAlertBehavior(i: Integer): JNotificationCompat_Builder; cdecl;
    function setGroupSummary(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setLargeIcon(bitmap: JBitmap): JNotificationCompat_Builder; cdecl;
    function setLights(i: Integer; i1: Integer; i2: Integer): JNotificationCompat_Builder; cdecl;
    function setLocalOnly(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setLocusId(locusIdCompat: JLocusIdCompat): JNotificationCompat_Builder; cdecl;
    function setNotificationSilent: JNotificationCompat_Builder; cdecl;
    function setNumber(i: Integer): JNotificationCompat_Builder; cdecl;
    function setOngoing(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setOnlyAlertOnce(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setPriority(i: Integer): JNotificationCompat_Builder; cdecl;
    function setProgress(i: Integer; i1: Integer; b: Boolean): JNotificationCompat_Builder; cdecl;
    function setPublicVersion(notification: JNotification): JNotificationCompat_Builder; cdecl;
    function setRemoteInputHistory(charSequence: TJavaObjectArray<JCharSequence>): JNotificationCompat_Builder; cdecl;
    function setSettingsText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setShortcutId(string_: JString): JNotificationCompat_Builder; cdecl;
    function setShortcutInfo(shortcutInfoCompat: JShortcutInfoCompat): JNotificationCompat_Builder; cdecl;
    function setShowWhen(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setSilent(b: Boolean): JNotificationCompat_Builder; cdecl;
    // function setSmallIcon(iconCompat: Jdrawable_IconCompat): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(i: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(i: Integer; i1: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSortKey(string_: JString): JNotificationCompat_Builder; cdecl;
    function setSound(uri: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
    function setSound(uri: Jnet_Uri; i: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
    function setSubText(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setTicker(charSequence: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
    function setTicker(charSequence: JCharSequence; remoteViews: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
    function setTimeoutAfter(l: Int64): JNotificationCompat_Builder; cdecl;
    function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setVibrate(l: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
    function setVisibility(i: Integer): JNotificationCompat_Builder; cdecl;
    function setWhen(l: Int64): JNotificationCompat_Builder; cdecl;
    property mActions: JArrayList read _GetmActions write _SetmActions;
    property mContext: JContext read _GetmContext write _SetmContext;
    property mPeople: JArrayList read _GetmPeople write _SetmPeople;
    property mPersonList: JArrayList read _GetmPersonList write _SetmPersonList;
  end;
  TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;

  JNotificationCompat_ExtenderClass = interface(IJavaClass)
    ['{E526C472-F346-4A76-A119-1213F3C14C3E}']
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Extender')]
  JNotificationCompat_Extender = interface(IJavaInstance)
    ['{49AAF7F2-478D-49AE-B03C-B03C07E46B1F}']
    function extend(builder: JNotificationCompat_Builder): JNotificationCompat_Builder; cdecl;
  end;
  TJNotificationCompat_Extender = class(TJavaGenericImport<JNotificationCompat_ExtenderClass, JNotificationCompat_Extender>) end;

  JNotificationCompat_StyleClass = interface(JObjectClass)
    ['{57D9C489-B5D5-466A-99BE-38DF51239448}']
    {class} function extractStyleFromNotification(notification: JNotification): JNotificationCompat_Style; cdecl;
    {class} function init: JNotificationCompat_Style; cdecl;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$Style')]
  JNotificationCompat_Style = interface(JObject)
    ['{89931428-DFD9-4BD3-8C5A-80B6E446ECD6}']
    procedure addCompatExtras(bundle: JBundle); cdecl;
    // procedure apply(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor); cdecl;
    function applyStandardTemplate(b: Boolean; i: Integer; b1: Boolean): JRemoteViews; cdecl;
    function build: JNotification; cdecl;
    procedure buildIntoRemoteViews(remoteViews: JRemoteViews; remoteViews1: JRemoteViews); cdecl;
    function createColoredBitmap(i: Integer; i1: Integer): JBitmap; cdecl; overload;
    function displayCustomViewInline: Boolean; cdecl;
    // function makeBigContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    // function makeContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    // function makeHeadsUpContentView(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor): JRemoteViews; cdecl;
    procedure setBuilder(builder: JNotificationCompat_Builder); cdecl;
  end;
  TJNotificationCompat_Style = class(TJavaGenericImport<JNotificationCompat_StyleClass, JNotificationCompat_Style>) end;

  // API 24+
  JNotificationCompat_DecoratedCustomViewStyleClass = interface(JNotificationCompat_StyleClass)
    ['{136C847E-D749-437D-8EE1-06D6DB9B856F}']
    function init: JNotificationCompat_DecoratedCustomViewStyle; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$DecoratedCustomViewStyle')]
  JNotificationCompat_DecoratedCustomViewStyle = interface(JNotificationCompat_Style)
    ['{50371CBC-753D-4C0C-9D87-9F178AA42F06}']
  end;
  TJNotificationCompat_DecoratedCustomViewStyle = class(TJavaGenericImport<JNotificationCompat_DecoratedCustomViewStyleClass,
    JNotificationCompat_DecoratedCustomViewStyle>)
  end;

  JNotificationBuilderWithBuilderAccessorClass = interface(IJavaClass)
    ['{AA7D94CC-8903-4E5B-A33F-8CB915D35674}']
  end;

  [JavaSignature('androidx/core/app/NotificationBuilderWithBuilderAccessor')]
  JNotificationBuilderWithBuilderAccessor = interface(IJavaInstance)
    ['{D832FC4F-095F-41F9-B83A-ED0A57D7687C}']
    function getBuilder: JNotification_Builder; cdecl;
  end;
  TJNotificationBuilderWithBuilderAccessor = class(TJavaGenericImport<JNotificationBuilderWithBuilderAccessorClass, JNotificationBuilderWithBuilderAccessor>) end;

  JNotificationCompat_BigPictureStyleClass = interface(JNotificationCompat_StyleClass)
    ['{7391005B-039A-47EA-9053-4F84A9BA51A1}']
    {class} function init: JNotificationCompat_BigPictureStyle; cdecl; overload;
    {class} function init(builder: JNotificationCompat_Builder): JNotificationCompat_BigPictureStyle; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$BigPictureStyle')]
  JNotificationCompat_BigPictureStyle = interface(JNotificationCompat_Style)
    ['{FD5C00B8-47BE-4E9B-A9D6-6696F571164F}']
    function bigLargeIcon(bitmap: JBitmap): JNotificationCompat_BigPictureStyle; cdecl; overload;
    function bigLargeIcon(icon: JIcon): JNotificationCompat_BigPictureStyle; cdecl; overload;  // API 23
    function bigPicture(bitmap: JBitmap): JNotificationCompat_BigPictureStyle; cdecl; overload;
    function bigPicture(icon: JIcon): JNotificationCompat_BigPictureStyle; cdecl; overload;  // API 31
    function setBigContentTitle(title: JCharSequence): JNotificationCompat_BigPictureStyle; cdecl;
    function setContentDescription(contentDescription: JCharSequence): JNotificationCompat_BigPictureStyle; cdecl; // API 31
    function setSummaryText(charSequence: JCharSequence): JNotificationCompat_BigPictureStyle; cdecl;
    function showBigPictureWhenCollapsed(show: Boolean): JNotificationCompat_BigPictureStyle; cdecl; // API 31
  end;
  TJNotificationCompat_BigPictureStyle = class(TJavaGenericImport<JNotificationCompat_BigPictureStyleClass, JNotificationCompat_BigPictureStyle>) end;


  JNotificationCompat_BigTextStyleClass = interface(JNotificationCompat_StyleClass)
    ['{5ABAD2AD-4BB1-4CDA-A578-CE13EAC1BBE7}']
    {class} function init: JNotificationCompat_BigTextStyle; cdecl; overload;
    {class} function init(builder: JNotificationCompat_Builder): JNotificationCompat_BigTextStyle; cdecl; overload;
  end;

  [JavaSignature('androidx/core/app/NotificationCompat$BigTextStyle')]
  JNotificationCompat_BigTextStyle = interface(JNotificationCompat_Style)
    ['{5164736D-3021-44AE-9218-19B700D4B805}']
    procedure addCompatExtras(bundle: JBundle); cdecl;
    procedure apply(notificationBuilderWithBuilderAccessor: JNotificationBuilderWithBuilderAccessor); cdecl;
    function bigText(charSequence: JCharSequence): JNotificationCompat_BigTextStyle; cdecl;
    procedure restoreFromCompatExtras(bundle: JBundle); cdecl;
    function setBigContentTitle(charSequence: JCharSequence): JNotificationCompat_BigTextStyle; cdecl;
    function setSummaryText(charSequence: JCharSequence): JNotificationCompat_BigTextStyle; cdecl;
  end;
  TJNotificationCompat_BigTextStyle = class(TJavaGenericImport<JNotificationCompat_BigTextStyleClass, JNotificationCompat_BigTextStyle>) end;

implementation

end.
