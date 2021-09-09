unit DW.Androidapi.JNI.AndroidX.Activity;

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
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.Util, DW.Androidapi.JNI.AndroidX.Lifecycle;

type
  JActivityOptionsCompat = interface;
  JActivityResult = interface;
  JActivityResultCallback = interface;
  JActivityResultContract = interface;
  JActivityResultContract_SynchronousResult = interface;
  JActivityResultLauncher = interface;
  JActivityResultRegistry = interface;
  JCancellable = interface;
  JComponentActivity = interface;
  JOnBackPressedCallback = interface;
  JOnBackPressedDispatcher = interface;
  JOnContextAvailableListener = interface;
  JContextAware = interface;
  JContextAwareHelper = interface;
  JFragmentActivity = interface;

  JContextAwareClass = interface(IJavaClass)
    ['{0A0825FB-ADEB-40AA-BA90-C5B7F200EB32}']
  end;

  [JavaSignature('androidx/activity/contextaware/ContextAware')]
  JContextAware = interface(IJavaInstance)
    ['{6E113B74-9255-40C9-BE7D-4E46F5F4E8AE}']
    procedure addOnContextAvailableListener(onContextAvailableListener: JOnContextAvailableListener); cdecl;
    function peekAvailableContext: JContext; cdecl;
    procedure removeOnContextAvailableListener(onContextAvailableListener: JOnContextAvailableListener); cdecl;
  end;
  TJContextAware = class(TJavaGenericImport<JContextAwareClass, JContextAware>) end;

  JContextAwareHelperClass = interface(JObjectClass)
    ['{7470D7A3-57D0-477F-ABC7-91E91D2E9EE8}']
    {class} function init: JContextAwareHelper; cdecl;
  end;

  [JavaSignature('androidx/activity/contextaware/ContextAwareHelper')]
  JContextAwareHelper = interface(JObject)
    ['{012B54E0-B64C-4261-A785-75A840DC37D9}']
    procedure addOnContextAvailableListener(onContextAvailableListener: JOnContextAvailableListener); cdecl;
    procedure clearAvailableContext; cdecl;
    procedure dispatchOnContextAvailable(context: JContext); cdecl;
    function peekAvailableContext: JContext; cdecl;
    procedure removeOnContextAvailableListener(onContextAvailableListener: JOnContextAvailableListener); cdecl;
  end;
  TJContextAwareHelper = class(TJavaGenericImport<JContextAwareHelperClass, JContextAwareHelper>) end;

  JOnContextAvailableListenerClass = interface(IJavaClass)
    ['{222E49E9-AF4E-46FC-8903-E26CF885F883}']
  end;

  [JavaSignature('androidx/activity/contextaware/OnContextAvailableListener')]
  JOnContextAvailableListener = interface(IJavaInstance)
    ['{43546D9C-1A7A-48D7-B18E-37F0E4DBD9A7}']
    procedure onContextAvailable(context: JContext); cdecl;
  end;
  TJOnContextAvailableListener = class(TJavaGenericImport<JOnContextAvailableListenerClass, JOnContextAvailableListener>) end;

  JActivityOptionsCompatClass = interface(JObjectClass)
    ['{C926C5A5-26C0-44A5-A4C9-730F4D928973}']
    {class} function _GetEXTRA_USAGE_TIME_REPORT: JString; cdecl;
    {class} function _GetEXTRA_USAGE_TIME_REPORT_PACKAGES: JString; cdecl;
    {class} function init: JActivityOptionsCompat; cdecl;
    {class} function makeBasic: JActivityOptionsCompat; cdecl;
    {class} function makeClipRevealAnimation(source: JView; startX: Integer; startY: Integer; width: Integer;
      height: Integer): JActivityOptionsCompat; cdecl;
    {class} function makeCustomAnimation(context: JContext; enterResId: Integer; exitResId: Integer): JActivityOptionsCompat; cdecl;
    {class} function makeScaleUpAnimation(source: JView; startX: Integer; startY: Integer; startWidth: Integer;
      startHeight: Integer): JActivityOptionsCompat; cdecl;
    {class} function makeSceneTransitionAnimation(activity: JActivity; sharedElement: JView;
      sharedElementName: JString): JActivityOptionsCompat; cdecl; overload;
    {class} function makeTaskLaunchBehind: JActivityOptionsCompat; cdecl;
    {class} function makeThumbnailScaleUpAnimation(source: JView; thumbnail: JBitmap; startX: Integer; startY: Integer): JActivityOptionsCompat; cdecl;
    {class} property EXTRA_USAGE_TIME_REPORT: JString read _GetEXTRA_USAGE_TIME_REPORT;
    {class} property EXTRA_USAGE_TIME_REPORT_PACKAGES: JString read _GetEXTRA_USAGE_TIME_REPORT_PACKAGES;
  end;

  [JavaSignature('androidx/core/app/ActivityOptionsCompat')]
  JActivityOptionsCompat = interface(JObject)
    ['{EE62E67D-F02B-440E-8EEC-20069940FB7B}']
    function getLaunchBounds: JRect; cdecl;
    procedure requestUsageTimeReport(receiver: JPendingIntent); cdecl;
    function setLaunchBounds(screenSpacePixelRect: JRect): JActivityOptionsCompat; cdecl;
    function toBundle: JBundle; cdecl;
    procedure update(otherOptions: JActivityOptionsCompat); cdecl;
  end;
  TJActivityOptionsCompat = class(TJavaGenericImport<JActivityOptionsCompatClass, JActivityOptionsCompat>) end;

  JActivityResultClass = interface(JParcelableClass)
    ['{F37BA94A-5C1B-4442-AD9F-3EAE40AF8824}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(resultCode: Integer; data: JIntent): JActivityResult; cdecl; overload;
    {class} function resultCodeToString(resultCode: Integer): JString; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/activity/result/ActivityResult')]
  JActivityResult = interface(JParcelable)
    ['{23EA3369-3222-418A-AD4E-AB3525C986DA}']
    function describeContents: Integer; cdecl;
    function getData: JIntent; cdecl;
    function getResultCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJActivityResult = class(TJavaGenericImport<JActivityResultClass, JActivityResult>) end;

  JActivityResultContractClass = interface(JObjectClass)
    ['{9056B145-A942-4656-9A7F-1B506C59E2B9}']
    {class} function init: JActivityResultContract; cdecl;
  end;

  [JavaSignature('androidx/activity/result/contract/ActivityResultContract')]
  JActivityResultContract = interface(JObject)
    ['{6BE88C31-AC15-40CC-90ED-579F61CB4C44}']
    function createIntent(context: JContext; input: JObject): JIntent; cdecl;
    function getSynchronousResult(context: JContext; input: JObject): JActivityResultContract_SynchronousResult; cdecl;
    function parseResult(resultCode: Integer; intent: JIntent): JObject; cdecl;
  end;
  TJActivityResultContract = class(TJavaGenericImport<JActivityResultContractClass, JActivityResultContract>) end;

  JActivityResultContract_SynchronousResultClass = interface(JObjectClass)
    ['{25F71A16-7B03-4D75-8A1E-CD95C861E2A8}']
    {class} function init(value: JObject): JActivityResultContract_SynchronousResult; cdecl;
  end;

  [JavaSignature('androidx/activity/result/contract/ActivityResultContract$SynchronousResult')]
  JActivityResultContract_SynchronousResult = interface(JObject)
    ['{C7DBC49D-649E-4BBD-A909-B13F109FEE7D}']
    function getValue: JObject; cdecl;
  end;
  TJActivityResultContract_SynchronousResult = class(TJavaGenericImport<JActivityResultContract_SynchronousResultClass,
    JActivityResultContract_SynchronousResult>) end;

  JActivityResultCallbackClass = interface(IJavaClass)
    ['{8C4CA5E3-DDC2-4CC8-B492-317A9A75A7B8}']
  end;

  [JavaSignature('androidx/activity/result/ActivityResultCallback')]
  JActivityResultCallback = interface(IJavaInstance)
    ['{3E162F99-47C0-42A7-97B0-20C700E5A3D5}']
    procedure onActivityResult(result: JObject); cdecl;
  end;
  TJActivityResultCallback = class(TJavaGenericImport<JActivityResultCallbackClass, JActivityResultCallback>) end;

  JActivityResultLauncherClass = interface(JObjectClass)
    ['{A394ADCB-1108-4A3B-88CC-5FCBCC510EE0}']
    {class} function init: JActivityResultLauncher; cdecl;
  end;

  [JavaSignature('androidx/activity/result/ActivityResultLauncher')]
  JActivityResultLauncher = interface(JObject)
    ['{7355C197-FF0A-45BB-87AA-FA3F0EF9E1B5}']
    function getContract: JActivityResultContract; cdecl;
    procedure launch(input: JObject); cdecl; overload;
    procedure launch(input: JObject; options: JActivityOptionsCompat); cdecl; overload;
    procedure unregister; cdecl;
  end;
  TJActivityResultLauncher = class(TJavaGenericImport<JActivityResultLauncherClass, JActivityResultLauncher>) end;

  JActivityResultRegistryClass = interface(JObjectClass)
    ['{03C3FA1A-F234-4BD3-8C28-D31FCA79A417}']
    {class} function _GetmKeyToCallback: JMap; cdecl;
    {class} function _GetmParsedPendingResults: JMap; cdecl;
    {class} function _GetmPendingResults: JBundle; cdecl;
    {class} function init: JActivityResultRegistry; cdecl;
    {class} property mKeyToCallback: JMap read _GetmKeyToCallback;
    {class} property mParsedPendingResults: JMap read _GetmParsedPendingResults;
    {class} property mPendingResults: JBundle read _GetmPendingResults;
  end;

  [JavaSignature('androidx/activity/result/ActivityResultRegistry')]
  JActivityResultRegistry = interface(JObject)
    ['{64DDB128-F9F5-4AC0-8CEE-DE606898AB2B}']
    function dispatchResult(requestCode: Integer; result: JObject): Boolean; cdecl; overload;
    function dispatchResult(requestCode: Integer; resultCode: Integer; data: JIntent): Boolean; cdecl; overload;
    procedure onLaunch(requestCode: Integer; contract: JActivityResultContract; input: JObject; options: JActivityOptionsCompat); cdecl;
    procedure onRestoreInstanceState(savedInstanceState: JBundle); cdecl;
    procedure onSaveInstanceState(outState: JBundle); cdecl;
    function register(key: JString; contract: JActivityResultContract; callback: JActivityResultCallback): JActivityResultLauncher; cdecl; overload;
    function register(key: JString; lifecycleOwner: JLifecycleOwner; contract: JActivityResultContract;
      callback: JActivityResultCallback): JActivityResultLauncher; cdecl; overload;
    procedure unregister(key: JString); cdecl;
  end;
  TJActivityResultRegistry = class(TJavaGenericImport<JActivityResultRegistryClass, JActivityResultRegistry>) end;

  JCancellableClass = interface(IJavaClass)
    ['{4F20D6CF-B912-4322-BC01-EC7915A07C86}']
  end;

  [JavaSignature('androidx/activity/Cancellable')]
  JCancellable = interface(IJavaInstance)
    ['{564D2FD4-CE4F-4DE5-B927-B8C576403C9E}']
    procedure cancel; cdecl;
  end;
  TJCancellable = class(TJavaGenericImport<JCancellableClass, JCancellable>) end;

  JOnBackPressedCallbackClass = interface(JObjectClass)
    ['{BF715EE7-E624-4342-91EA-C15CF7BD9FB4}']
    {class} function init(enabled: Boolean): JOnBackPressedCallback; cdecl;
  end;

  [JavaSignature('androidx/activity/OnBackPressedCallback')]
  JOnBackPressedCallback = interface(JObject)
    ['{F42D539B-DE2F-45E7-9952-7C68034FBB74}']
    procedure handleOnBackPressed; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure remove; cdecl;
    procedure removeCancellable(cancellable: JCancellable); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TJOnBackPressedCallback = class(TJavaGenericImport<JOnBackPressedCallbackClass, JOnBackPressedCallback>) end;

  JOnBackPressedDispatcherClass = interface(JObjectClass)
    ['{A5B64034-14AA-4CF6-98F7-03A3F39D6AD7}']
    {class} function init: JOnBackPressedDispatcher; cdecl; overload;
    {class} function init(fallbackOnBackPressed: JRunnable): JOnBackPressedDispatcher; cdecl; overload;
  end;

  [JavaSignature('androidx/activity/OnBackPressedDispatcher')]
  JOnBackPressedDispatcher = interface(JObject)
    ['{D0B8FA64-D40E-4418-AC6D-4E98C09559E6}']
    procedure addCallback(onBackPressedCallback: JOnBackPressedCallback); cdecl; overload;
    procedure addCallback(owner: JLifecycleOwner; onBackPressedCallback: JOnBackPressedCallback); cdecl; overload;
    function addCancellableCallback(onBackPressedCallback: JOnBackPressedCallback): JCancellable; cdecl;
    function hasEnabledCallbacks: Boolean; cdecl;
    procedure onBackPressed; cdecl;
  end;
  TJOnBackPressedDispatcher = class(TJavaGenericImport<JOnBackPressedDispatcherClass, JOnBackPressedDispatcher>) end;

  JComponentActivityClass = interface(JActivityClass)
    ['{C1A53046-84B2-4355-83DC-EC3AB8B0D0CD}']
    {class} function init: JComponentActivity; cdecl; overload;
    {class} function init(contentLayoutId: Integer): JComponentActivity; cdecl; overload;
  end;

  [JavaSignature('androidx/activity/ComponentActivity')]
  JComponentActivity = interface(JActivity)
    ['{2A4AD04A-BC6A-4C34-8AD9-4DA5CE1E7F56}']
    procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
    procedure addOnContextAvailableListener(listener: JOnContextAvailableListener); cdecl;
    procedure ensureViewModelStore; cdecl;
    function getActivityResultRegistry: JActivityResultRegistry; cdecl;
    function getDefaultViewModelProviderFactory: JViewModelProvider_Factory; cdecl;
    function getLastCustomNonConfigurationInstance: JObject; cdecl;
    function getLifecycle: JLifecycle; cdecl;
    function getOnBackPressedDispatcher: JOnBackPressedDispatcher; cdecl;
    // function getSavedStateRegistry: JSavedStateRegistry; cdecl;
    function getViewModelStore: JViewModelStore; cdecl;
    procedure onBackPressed; cdecl;
    procedure onCreate(bundle: JBundle); cdecl;
    procedure onRequestPermissionsResult(requestCode: Integer; permissions: TJavaObjectArray<JString>; grantResults: TJavaArray<Integer>); cdecl;
    function onRetainCustomNonConfigurationInstance: JObject; cdecl;
    function onRetainNonConfigurationInstance: JObject; cdecl;
    function peekAvailableContext: JContext; cdecl;
    function registerForActivityResult(contract: JActivityResultContract; callback: JActivityResultCallback): JActivityResultLauncher; cdecl; overload;
    function registerForActivityResult(contract: JActivityResultContract; registry: JActivityResultRegistry;
      callback: JActivityResultCallback): JActivityResultLauncher; cdecl; overload;
    procedure removeOnContextAvailableListener(listener: JOnContextAvailableListener); cdecl;
    procedure reportFullyDrawn; cdecl;
    procedure setContentView(layoutResID: Integer); cdecl; overload;
    procedure setContentView(view: JView); cdecl; overload;
    procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer;
      extraFlags: Integer); cdecl; overload;
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer;
      extraFlags: Integer; options: JBundle); cdecl; overload;
  end;
  TJComponentActivity = class(TJavaGenericImport<JComponentActivityClass, JComponentActivity>) end;

  JFragmentActivityClass = interface(JComponentActivityClass)
    ['{82CF6B78-3FBE-455C-91C8-A0E3D274288D}']
  end;

  [JavaSignature('androidx/activity/ComponentActivity')]
  JFragmentActivity = interface(JComponentActivity)
    ['{E20CFB15-223E-4E7D-AC3A-058F00BB72A4}']
  end;
  TJFragmentActivity = class(TJavaGenericImport<JFragmentActivityClass, JFragmentActivity>) end;

implementation

end.

