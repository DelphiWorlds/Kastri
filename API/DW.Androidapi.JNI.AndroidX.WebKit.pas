unit DW.Androidapi.JNI.AndroidX.WebKit;

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
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Java.Security, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Util, Androidapi.JNI.Webkit,
  // DW
  DW.Androidapi.JNI.JavaTypes, DW.Androidapi.JNI.Util;

type
  JBackForwardCacheSettings = interface;//androidx.webkit.BackForwardCacheSettings
  JBackForwardCacheSettings_Builder = interface;//androidx.webkit.BackForwardCacheSettings$Builder
  JCookieManagerCompat = interface;//androidx.webkit.CookieManagerCompat
  JCustomHeader = interface;//androidx.webkit.CustomHeader
  JDropDataContentProvider = interface;//androidx.webkit.DropDataContentProvider
  JJavaScriptReplyProxy = interface;//androidx.webkit.JavaScriptReplyProxy
  JNavigation = interface;//androidx.webkit.Navigation
  JNavigationListener = interface;//androidx.webkit.NavigationListener
  JNoVarySearchHeader = interface;//androidx.webkit.NoVarySearchHeader
  JOutcomeReceiverCompat = interface;//androidx.webkit.OutcomeReceiverCompat
  JPage = interface;//androidx.webkit.Page
  JPrefetchException = interface;//androidx.webkit.PrefetchException
  JPrefetchNetworkException = interface;//androidx.webkit.PrefetchNetworkException
  JPrerenderException = interface;//androidx.webkit.PrerenderException
  JPrerenderOperationCallback = interface;//androidx.webkit.PrerenderOperationCallback
  JProcessGlobalConfig = interface;//androidx.webkit.ProcessGlobalConfig
  JProcessGlobalConfig_UiThreadStartupMode = interface;//androidx.webkit.ProcessGlobalConfig$UiThreadStartupMode
  JProfile = interface;//androidx.webkit.Profile
  JProfile_ExperimentalAddQuicHints = interface;//androidx.webkit.Profile$ExperimentalAddQuicHints
  JProfile_ExperimentalOriginMatchedHeader = interface;//androidx.webkit.Profile$ExperimentalOriginMatchedHeader
  JProfile_ExperimentalPreconnect = interface;//androidx.webkit.Profile$ExperimentalPreconnect
  JProfile_ExperimentalUrlPrefetch = interface;//androidx.webkit.Profile$ExperimentalUrlPrefetch
  JProfile_ExperimentalWarmUpRendererProcess = interface;//androidx.webkit.Profile$ExperimentalWarmUpRendererProcess
  JProfileStore = interface;//androidx.webkit.ProfileStore
  JProxyConfig = interface;//androidx.webkit.ProxyConfig
  JProxyConfig_Builder = interface;//androidx.webkit.ProxyConfig$Builder
  JProxyConfig_ProxyRule = interface;//androidx.webkit.ProxyConfig$ProxyRule
  JProxyConfig_ProxyScheme = interface;//androidx.webkit.ProxyConfig$ProxyScheme
  JProxyController = interface;//androidx.webkit.ProxyController
  JRestrictionAllowlist = interface;//androidx.webkit.RestrictionAllowlist
  JRestrictionAllowlist_Builder = interface;//androidx.webkit.RestrictionAllowlist$Builder
  JSafeBrowsingResponseCompat = interface;//androidx.webkit.SafeBrowsingResponseCompat
  JScriptHandler = interface;//androidx.webkit.ScriptHandler
  JServiceWorkerClientCompat = interface;//androidx.webkit.ServiceWorkerClientCompat
  JServiceWorkerControllerCompat = interface;//androidx.webkit.ServiceWorkerControllerCompat
  JServiceWorkerWebSettingsCompat = interface;//androidx.webkit.ServiceWorkerWebSettingsCompat
  JServiceWorkerWebSettingsCompat_CacheMode = interface;//androidx.webkit.ServiceWorkerWebSettingsCompat$CacheMode
  JSpeculativeLoadingConfig = interface;//androidx.webkit.SpeculativeLoadingConfig
  JSpeculativeLoadingConfig_Builder = interface;//androidx.webkit.SpeculativeLoadingConfig$Builder
  JSpeculativeLoadingParameters = interface;//androidx.webkit.SpeculativeLoadingParameters
  JSpeculativeLoadingParameters_Builder = interface;//androidx.webkit.SpeculativeLoadingParameters$Builder
  JStartUpLocation = interface;//androidx.webkit.StartUpLocation
  Jwebkit_TracingConfig = interface;//androidx.webkit.TracingConfig
  Jwebkit_TracingConfig_Builder = interface;//androidx.webkit.TracingConfig$Builder
  JTracingConfig_PredefinedCategories = interface;//androidx.webkit.TracingConfig$PredefinedCategories
  JTracingConfig_TracingMode = interface;//androidx.webkit.TracingConfig$TracingMode
  Jwebkit_TracingController = interface;//androidx.webkit.TracingController
  JURLUtilCompat = interface;//androidx.webkit.URLUtilCompat
  JUserAgentMetadata = interface;//androidx.webkit.UserAgentMetadata
  JUserAgentMetadata_BrandVersion = interface;//androidx.webkit.UserAgentMetadata$BrandVersion
  JBrandVersion_Builder = interface;//androidx.webkit.UserAgentMetadata$BrandVersion$Builder
  JUserAgentMetadata_Builder = interface;//androidx.webkit.UserAgentMetadata$Builder
  JUserAgentMetadata_FormFactors = interface;//androidx.webkit.UserAgentMetadata$FormFactors
  JWebMessageCompat = interface;//androidx.webkit.WebMessageCompat
  JWebMessageCompat_Type = interface;//androidx.webkit.WebMessageCompat$Type
  JWebMessagePortCompat = interface;//androidx.webkit.WebMessagePortCompat
  JWebMessagePortCompat_WebMessageCallbackCompat = interface;//androidx.webkit.WebMessagePortCompat$WebMessageCallbackCompat
  JWebNavigationClient = interface;//androidx.webkit.WebNavigationClient
  JWebNavigationClient_ExperimentalNavigationCallback = interface;//androidx.webkit.WebNavigationClient$ExperimentalNavigationCallback
  JWebResourceErrorCompat = interface;//androidx.webkit.WebResourceErrorCompat
  JWebResourceErrorCompat_NetErrorCode = interface;//androidx.webkit.WebResourceErrorCompat$NetErrorCode
  JWebResourceRequestCompat = interface;//androidx.webkit.WebResourceRequestCompat
  JWebResourceResponseCompat = interface;//androidx.webkit.WebResourceResponseCompat
  JWebSettingsCompat = interface;//androidx.webkit.WebSettingsCompat
  JWebSettingsCompat_ExperimentalBackForwardCacheSettings = interface;//androidx.webkit.WebSettingsCompat$ExperimentalBackForwardCacheSettings
  JWebSettingsCompat_ExperimentalSpeculativeLoading = interface;//androidx.webkit.WebSettingsCompat$ExperimentalSpeculativeLoading
  JWebSettingsCompat_ForceDark = interface;//androidx.webkit.WebSettingsCompat$ForceDark
  JWebSettingsCompat_ForceDarkStrategy = interface;//androidx.webkit.WebSettingsCompat$ForceDarkStrategy
  JWebSettingsCompat_HyperlinkContextMenuItems = interface;//androidx.webkit.WebSettingsCompat$HyperlinkContextMenuItems
  JWebSettingsCompat_MenuItemFlags = interface;//androidx.webkit.WebSettingsCompat$MenuItemFlags
  JWebStorageCompat = interface;//androidx.webkit.WebStorageCompat
  JWebViewAssetLoader = interface;//androidx.webkit.WebViewAssetLoader
  JWebViewAssetLoader_AssetsPathHandler = interface;//androidx.webkit.WebViewAssetLoader$AssetsPathHandler
  JWebViewAssetLoader_Builder = interface;//androidx.webkit.WebViewAssetLoader$Builder
  JWebViewAssetLoader_InternalStoragePathHandler = interface;//androidx.webkit.WebViewAssetLoader$InternalStoragePathHandler
  JWebViewAssetLoader_PathHandler = interface;//androidx.webkit.WebViewAssetLoader$PathHandler
  JWebViewAssetLoader_ResourcesPathHandler = interface;//androidx.webkit.WebViewAssetLoader$ResourcesPathHandler
  JWebViewBuilder = interface;//androidx.webkit.WebViewBuilder
  JWebViewBuilder_Experimental = interface;//androidx.webkit.WebViewBuilder$Experimental
  JWebViewBuilder_Preset = interface;//androidx.webkit.WebViewBuilder$Preset
  JWebViewBuilderException = interface;//androidx.webkit.WebViewBuilderException
  JWebViewClientCompat = interface;//androidx.webkit.WebViewClientCompat
  JWebViewClientCompat_SafeBrowsingThreat = interface;//androidx.webkit.WebViewClientCompat$SafeBrowsingThreat
  JWebViewCompat = interface;//androidx.webkit.WebViewCompat
  JWebViewCompat_ExperimentalAsyncStartUp = interface;//androidx.webkit.WebViewCompat$ExperimentalAsyncStartUp
  JWebViewCompat_ExperimentalSaveState = interface;//androidx.webkit.WebViewCompat$ExperimentalSaveState
  JWebViewCompat_VisualStateCallback = interface;//androidx.webkit.WebViewCompat$VisualStateCallback
  JWebViewCompat_WebMessageListener = interface;//androidx.webkit.WebViewCompat$WebMessageListener
  JWebViewCompat_WebViewStartUpCallback = interface;//androidx.webkit.WebViewCompat$WebViewStartUpCallback
  JWebViewFeature = interface;//androidx.webkit.WebViewFeature
  JWebViewFeature_WebViewStartupFeature = interface;//androidx.webkit.WebViewFeature$WebViewStartupFeature
  JWebViewFeature_WebViewSupportFeature = interface;//androidx.webkit.WebViewFeature$WebViewSupportFeature
  JWebViewMediaIntegrityApiStatusConfig = interface;//androidx.webkit.WebViewMediaIntegrityApiStatusConfig
  JWebViewMediaIntegrityApiStatusConfig_Builder = interface;//androidx.webkit.WebViewMediaIntegrityApiStatusConfig$Builder
  Jwebkit_WebViewRenderProcess = interface;//androidx.webkit.WebViewRenderProcess
  Jwebkit_WebViewRenderProcessClient = interface;//androidx.webkit.WebViewRenderProcessClient
  JWebViewStartUpConfig = interface;//androidx.webkit.WebViewStartUpConfig
  JWebViewStartUpConfig_Builder = interface;//androidx.webkit.WebViewStartUpConfig$Builder
  JWebViewStartUpResult = interface;//androidx.webkit.WebViewStartUpResult
  JApiFeature = interface;//androidx.webkit.internal.ApiFeature
  JApiFeature_M = interface;//androidx.webkit.internal.ApiFeature$M
  JApiFeature_N = interface;//androidx.webkit.internal.ApiFeature$N
  JApiFeature_NoFramework = interface;//androidx.webkit.internal.ApiFeature$NoFramework
  JApiFeature_O = interface;//androidx.webkit.internal.ApiFeature$O
  JApiFeature_O_MR1 = interface;//androidx.webkit.internal.ApiFeature$O_MR1
  JApiFeature_P = interface;//androidx.webkit.internal.ApiFeature$P
  JApiFeature_Q = interface;//androidx.webkit.internal.ApiFeature$Q
  JApiFeature_T = interface;//androidx.webkit.internal.ApiFeature$T
  JApiHelperForN = interface;//androidx.webkit.internal.ApiHelperForN
  JApiHelperForO = interface;//androidx.webkit.internal.ApiHelperForO
  JApiHelperForOMR1 = interface;//androidx.webkit.internal.ApiHelperForOMR1
  JApiHelperForP = interface;//androidx.webkit.internal.ApiHelperForP
  JApiHelperForQ = interface;//androidx.webkit.internal.ApiHelperForQ
  JApiHelperForTiramisu = interface;//androidx.webkit.internal.ApiHelperForTiramisu
  JAssetHelper = interface;//androidx.webkit.internal.AssetHelper
  JBackForwardCacheSettingsImpl = interface;//androidx.webkit.internal.BackForwardCacheSettingsImpl
  JConditionallySupportedFeature = interface;//androidx.webkit.internal.ConditionallySupportedFeature
  JCookieManagerAdapter = interface;//androidx.webkit.internal.CookieManagerAdapter
  JFrameworkServiceWorkerClient = interface;//androidx.webkit.internal.FrameworkServiceWorkerClient
  JIncompatibleApkWebViewProviderFactory = interface;//androidx.webkit.internal.IncompatibleApkWebViewProviderFactory
  JJavaScriptReplyProxyImpl = interface;//androidx.webkit.internal.JavaScriptReplyProxyImpl
  JNavigationImpl = interface;//androidx.webkit.internal.NavigationImpl
  JNavigationListenerAdapter = interface;//androidx.webkit.internal.NavigationListenerAdapter
  JNoVarySearchHeaderAdapter = interface;//androidx.webkit.internal.NoVarySearchHeaderAdapter
  JPageImpl = interface;//androidx.webkit.internal.PageImpl
  JPrefetchOperationCallbackAdapter = interface;//androidx.webkit.internal.PrefetchOperationCallbackAdapter
  JProfileImpl = interface;//androidx.webkit.internal.ProfileImpl
  JProfileStoreImpl = interface;//androidx.webkit.internal.ProfileStoreImpl
  JProxyControllerImpl = interface;//androidx.webkit.internal.ProxyControllerImpl
  JSafeBrowsingResponseImpl = interface;//androidx.webkit.internal.SafeBrowsingResponseImpl
  JScriptHandlerImpl = interface;//androidx.webkit.internal.ScriptHandlerImpl
  JServiceWorkerClientAdapter = interface;//androidx.webkit.internal.ServiceWorkerClientAdapter
  JServiceWorkerControllerImpl = interface;//androidx.webkit.internal.ServiceWorkerControllerImpl
  JServiceWorkerWebSettingsImpl = interface;//androidx.webkit.internal.ServiceWorkerWebSettingsImpl
  JSpeculativeLoadingConfigAdapter = interface;//androidx.webkit.internal.SpeculativeLoadingConfigAdapter
  JSpeculativeLoadingParametersAdapter = interface;//androidx.webkit.internal.SpeculativeLoadingParametersAdapter
  JStartupApiFeature = interface;//androidx.webkit.internal.StartupApiFeature
  JStartupApiFeature_NoFramework = interface;//androidx.webkit.internal.StartupApiFeature$NoFramework
  JStartupApiFeature_P = interface;//androidx.webkit.internal.StartupApiFeature$P
  JStartupFeatures = interface;//androidx.webkit.internal.StartupFeatures
  JTracingControllerImpl = interface;//androidx.webkit.internal.TracingControllerImpl
  JUserAgentMetadataInternal = interface;//androidx.webkit.internal.UserAgentMetadataInternal
  JVisualStateCallbackAdapter = interface;//androidx.webkit.internal.VisualStateCallbackAdapter
  JWebMessageAdapter = interface;//androidx.webkit.internal.WebMessageAdapter
  JWebMessageCallbackAdapter = interface;//androidx.webkit.internal.WebMessageCallbackAdapter
  JWebMessageListenerAdapter = interface;//androidx.webkit.internal.WebMessageListenerAdapter
  JWebMessagePayloadAdapter = interface;//androidx.webkit.internal.WebMessagePayloadAdapter
  JWebMessagePortImpl = interface;//androidx.webkit.internal.WebMessagePortImpl
  JWebNavigationClientAdapter = interface;//androidx.webkit.internal.WebNavigationClientAdapter
  JWebResourceErrorImpl = interface;//androidx.webkit.internal.WebResourceErrorImpl
  JWebResourceRequestAdapter = interface;//androidx.webkit.internal.WebResourceRequestAdapter
  JWebSettingsAdapter = interface;//androidx.webkit.internal.WebSettingsAdapter
  JWebSettingsNoOpAdapter = interface;//androidx.webkit.internal.WebSettingsNoOpAdapter
  JWebStorageAdapter = interface;//androidx.webkit.internal.WebStorageAdapter
  JWebViewFeatureInternal = interface;//androidx.webkit.internal.WebViewFeatureInternal
  JWebViewGlueCommunicator = interface;//androidx.webkit.internal.WebViewGlueCommunicator
  JWebViewProviderAdapter = interface;//androidx.webkit.internal.WebViewProviderAdapter
  JWebViewProviderFactory = interface;//androidx.webkit.internal.WebViewProviderFactory
  JWebViewProviderFactoryAdapter = interface;//androidx.webkit.internal.WebViewProviderFactoryAdapter
  JWebViewRenderProcessClientAdapter = interface;//androidx.webkit.internal.WebViewRenderProcessClientAdapter
  JWebViewRenderProcessClientFrameworkAdapter = interface;//androidx.webkit.internal.WebViewRenderProcessClientFrameworkAdapter
  JWebViewRenderProcessImpl = interface;//androidx.webkit.internal.WebViewRenderProcessImpl
  JWebViewStartUpCallbackAdapter = interface;//androidx.webkit.internal.WebViewStartUpCallbackAdapter
  JWebViewStartUpConfigAdapter = interface;//androidx.webkit.internal.WebViewStartUpConfigAdapter
  JWebkitToCompatConverter = interface;//androidx.webkit.internal.WebkitToCompatConverter
  // JUnsupportedOperationException = interface;//java.lang.UnsupportedOperationException
  JDropDataContentProviderBoundaryInterface = interface;//org.chromium.support_lib_boundary.DropDataContentProviderBoundaryInterface
  JFeatureFlagHolderBoundaryInterface = interface;//org.chromium.support_lib_boundary.FeatureFlagHolderBoundaryInterface
  JIsomorphicObjectBoundaryInterface = interface;//org.chromium.support_lib_boundary.IsomorphicObjectBoundaryInterface
  JJsReplyProxyBoundaryInterface = interface;//org.chromium.support_lib_boundary.JsReplyProxyBoundaryInterface
  JNoVarySearchDataBoundaryInterface = interface;//org.chromium.support_lib_boundary.NoVarySearchDataBoundaryInterface
  JOriginMatchedHeaderBoundaryInterface = interface;//org.chromium.support_lib_boundary.OriginMatchedHeaderBoundaryInterface
  JPrefetchOperationCallbackBoundaryInterface = interface;//org.chromium.support_lib_boundary.PrefetchOperationCallbackBoundaryInterface
  JPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterface = interface;//org.chromium.support_lib_boundary.PrefetchOperationCallbackBoundaryInterface$PrefetchExceptionTypeBoundaryInterface
  JProcessGlobalConfigConstants = interface;//org.chromium.support_lib_boundary.ProcessGlobalConfigConstants
  JProcessGlobalConfigConstants_ProcessGlobalConfigMapKey = interface;//org.chromium.support_lib_boundary.ProcessGlobalConfigConstants$ProcessGlobalConfigMapKey
  JProfileBoundaryInterface = interface;//org.chromium.support_lib_boundary.ProfileBoundaryInterface
  JProfileStoreBoundaryInterface = interface;//org.chromium.support_lib_boundary.ProfileStoreBoundaryInterface
  JProxyControllerBoundaryInterface = interface;//org.chromium.support_lib_boundary.ProxyControllerBoundaryInterface
  JSafeBrowsingResponseBoundaryInterface = interface;//org.chromium.support_lib_boundary.SafeBrowsingResponseBoundaryInterface
  JScriptHandlerBoundaryInterface = interface;//org.chromium.support_lib_boundary.ScriptHandlerBoundaryInterface
  JServiceWorkerClientBoundaryInterface = interface;//org.chromium.support_lib_boundary.ServiceWorkerClientBoundaryInterface
  JServiceWorkerControllerBoundaryInterface = interface;//org.chromium.support_lib_boundary.ServiceWorkerControllerBoundaryInterface
  JServiceWorkerWebSettingsBoundaryInterface = interface;//org.chromium.support_lib_boundary.ServiceWorkerWebSettingsBoundaryInterface
  JSpeculativeLoadingConfigBoundaryInterface = interface;//org.chromium.support_lib_boundary.SpeculativeLoadingConfigBoundaryInterface
  JSpeculativeLoadingParametersBoundaryInterface = interface;//org.chromium.support_lib_boundary.SpeculativeLoadingParametersBoundaryInterface
  JStaticsBoundaryInterface = interface;//org.chromium.support_lib_boundary.StaticsBoundaryInterface
  JTracingControllerBoundaryInterface = interface;//org.chromium.support_lib_boundary.TracingControllerBoundaryInterface
  JVisualStateCallbackBoundaryInterface = interface;//org.chromium.support_lib_boundary.VisualStateCallbackBoundaryInterface
  JWebMessageBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebMessageBoundaryInterface
  JWebMessageCallbackBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebMessageCallbackBoundaryInterface
  JWebMessageListenerBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebMessageListenerBoundaryInterface
  JWebMessagePayloadBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebMessagePayloadBoundaryInterface
  JWebMessagePayloadBoundaryInterface_WebMessagePayloadType = interface;//org.chromium.support_lib_boundary.WebMessagePayloadBoundaryInterface$WebMessagePayloadType
  JWebMessagePortBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebMessagePortBoundaryInterface
  JWebResourceErrorBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebResourceErrorBoundaryInterface
  JWebResourceRequestBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebResourceRequestBoundaryInterface
  JWebSettingsBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface
  JWebSettingsBoundaryInterface_AttributionBehavior = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$AttributionBehavior
  JWebSettingsBoundaryInterface_ForceDarkBehavior = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$ForceDarkBehavior
  JWebSettingsBoundaryInterface_HyperlinkContextMenuItems = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$HyperlinkContextMenuItems
  JWebSettingsBoundaryInterface_SpeculativeLoadingStatus = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$SpeculativeLoadingStatus
  JWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatus = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$WebViewMediaIntegrityApiStatus
  JWebSettingsBoundaryInterface_WebauthnSupport = interface;//org.chromium.support_lib_boundary.WebSettingsBoundaryInterface$WebauthnSupport
  JWebStorageBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebStorageBoundaryInterface
  JWebViewBackForwardCacheSettingsBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewBackForwardCacheSettingsBoundaryInterface
  JWebViewBuilderBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewBuilderBoundaryInterface
  JWebViewBuilderBoundaryInterface_Baseline = interface;//org.chromium.support_lib_boundary.WebViewBuilderBoundaryInterface$Baseline
  JWebViewBuilderBoundaryInterface_Config = interface;//org.chromium.support_lib_boundary.WebViewBuilderBoundaryInterface$Config
  JWebViewBuilderBoundaryInterface_ConfigField = interface;//org.chromium.support_lib_boundary.WebViewBuilderBoundaryInterface$ConfigField
  JWebViewClientBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewClientBoundaryInterface
  JWebViewCookieManagerBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewCookieManagerBoundaryInterface
  JWebViewNavigationBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewNavigationBoundaryInterface
  JWebViewNavigationClientBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewNavigationClientBoundaryInterface
  JWebViewNavigationListenerBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewNavigationListenerBoundaryInterface
  JWebViewPageBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewPageBoundaryInterface
  JWebViewProviderBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewProviderBoundaryInterface
  JWebViewProviderFactoryBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewProviderFactoryBoundaryInterface
  JWebViewRendererBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewRendererBoundaryInterface
  JWebViewRendererClientBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewRendererClientBoundaryInterface
  JWebViewStartUpCallbackBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewStartUpCallbackBoundaryInterface
  JWebViewStartUpConfigBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewStartUpConfigBoundaryInterface
  JWebViewStartUpResultBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebViewStartUpResultBoundaryInterface
  JWebkitToCompatConverterBoundaryInterface = interface;//org.chromium.support_lib_boundary.WebkitToCompatConverterBoundaryInterface
  JBoundaryInterfaceReflectionUtil = interface;//org.chromium.support_lib_boundary.util.BoundaryInterfaceReflectionUtil
  JFeatures = interface;//org.chromium.support_lib_boundary.util.Features

  JBackForwardCacheSettingsClass = interface(JObjectClass)
    ['{89A0421D-4E04-4138-B88F-BEF4F64DDD71}']
  end;

  [JavaSignature('androidx/webkit/BackForwardCacheSettings')]
  JBackForwardCacheSettings = interface(JObject)
    ['{7B237C45-8659-4EB6-8E11-28C98071B8DF}']
    function getMaxPagesInCache: Integer; cdecl;
    function getTimeoutSeconds: Int64; cdecl;
  end;
  TJBackForwardCacheSettings = class(TJavaGenericImport<JBackForwardCacheSettingsClass, JBackForwardCacheSettings>) end;

  JBackForwardCacheSettings_BuilderClass = interface(JObjectClass)
    ['{F423CBF7-CA27-4843-8F12-C8BE75BACA63}']
    {class} function init: JBackForwardCacheSettings_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/BackForwardCacheSettings$Builder')]
  JBackForwardCacheSettings_Builder = interface(JObject)
    ['{3D746AFF-B944-4680-88FA-6ACCECA6AE0B}']
    function build: JBackForwardCacheSettings; cdecl;
    function setMaxPagesInCache(i: Integer): JBackForwardCacheSettings_Builder; cdecl;
    function setTimeoutSeconds(l: Int64): JBackForwardCacheSettings_Builder; cdecl;
  end;
  TJBackForwardCacheSettings_Builder = class(TJavaGenericImport<JBackForwardCacheSettings_BuilderClass, JBackForwardCacheSettings_Builder>) end;

  JCookieManagerCompatClass = interface(JObjectClass)
    ['{392EF8B5-D2EC-4745-A87C-5E69AA26145F}']
    {class} function getCookieInfo(cookieManager: JCookieManager; string_: JString): JList; cdecl;
  end;

  [JavaSignature('androidx/webkit/CookieManagerCompat')]
  JCookieManagerCompat = interface(JObject)
    ['{6477766C-E612-42B1-ACAD-F41002B31EB5}']
  end;
  TJCookieManagerCompat = class(TJavaGenericImport<JCookieManagerCompatClass, JCookieManagerCompat>) end;

  JCustomHeaderClass = interface(JObjectClass)
    ['{9642863E-12BE-4B05-867A-E995CCCDF446}']
    {class} function init(string_: JString; string_1: JString; set_: JSet): JCustomHeader; cdecl;
  end;

  [JavaSignature('androidx/webkit/CustomHeader')]
  JCustomHeader = interface(JObject)
    ['{2B8ED7C9-89E7-4095-BFDB-D1BFD20C3D10}']
    function equals(object_: JObject): Boolean; cdecl;
    function getName: JString; cdecl;
    function getRules: JSet; cdecl;
    function getValue: JString; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJCustomHeader = class(TJavaGenericImport<JCustomHeaderClass, JCustomHeader>) end;

  JDropDataContentProviderClass = interface(JContentProviderClass)
    ['{DDCB7442-66F5-4C84-BB8D-089EBF11CE4D}']
    {class} function init: JDropDataContentProvider; cdecl;
  end;

  [JavaSignature('androidx/webkit/DropDataContentProvider')]
  JDropDataContentProvider = interface(JContentProvider)
    ['{2E3ADD51-E7CB-4F9B-9A42-5D1A1FE718AE}']
    function call(string_: JString; string_1: JString; bundle: JBundle): JBundle; cdecl;
    function delete(uri: Jnet_Uri; string_: JString; string_1: TJavaObjectArray<JString>): Integer; cdecl;
    function getType(uri: Jnet_Uri): JString; cdecl;
    function insert(uri: Jnet_Uri; contentValues: JContentValues): Jnet_Uri; cdecl;
    function onCreate: Boolean; cdecl;
    function openFile(uri: Jnet_Uri; string_: JString): JParcelFileDescriptor; cdecl;
    function query(uri: Jnet_Uri; string_: TJavaObjectArray<JString>; string_1: JString; string_2: TJavaObjectArray<JString>; string_3: JString): JCursor; cdecl;
    function update(uri: Jnet_Uri; contentValues: JContentValues; string_: JString; string_1: TJavaObjectArray<JString>): Integer; cdecl;
  end;
  TJDropDataContentProvider = class(TJavaGenericImport<JDropDataContentProviderClass, JDropDataContentProvider>) end;

  JJavaScriptReplyProxyClass = interface(JObjectClass)
    ['{FD995F32-B7EC-484B-B115-5EE175E571C2}']
    {class} function init: JJavaScriptReplyProxy; cdecl;
  end;

  [JavaSignature('androidx/webkit/JavaScriptReplyProxy')]
  JJavaScriptReplyProxy = interface(JObject)
    ['{E5C074D3-4E52-4FA9-B69F-06A061966420}']
    procedure postMessage(string_: JString); cdecl; overload;
    procedure postMessage(b: TJavaArray<Byte>); cdecl; overload;
  end;
  TJJavaScriptReplyProxy = class(TJavaGenericImport<JJavaScriptReplyProxyClass, JJavaScriptReplyProxy>) end;

  JNavigationClass = interface(IJavaClass)
    ['{597618B8-49E9-4331-96BD-1793F35682B9}']
  end;

  [JavaSignature('androidx/webkit/Navigation')]
  JNavigation = interface(IJavaInstance)
    ['{4C57632A-D109-46A1-A9C0-2A5C97EDFC96}']
    function didCommit: Boolean; cdecl;
    function didCommitErrorPage: Boolean; cdecl;
    function getPage: JPage; cdecl;
    function getStatusCode: Integer; cdecl;
    function getUrl: JString; cdecl;
    function isBack: Boolean; cdecl;
    function isForward: Boolean; cdecl;
    function isHistory: Boolean; cdecl;
    function isReload: Boolean; cdecl;
    function isRestore: Boolean; cdecl;
    function isSameDocument: Boolean; cdecl;
    function wasInitiatedByPage: Boolean; cdecl;
  end;
  TJNavigation = class(TJavaGenericImport<JNavigationClass, JNavigation>) end;

  JNavigationListenerClass = interface(IJavaClass)
    ['{D785FC0F-AF8B-4734-AD8F-E34AE8D8A19E}']
  end;

  [JavaSignature('androidx/webkit/NavigationListener')]
  JNavigationListener = interface(IJavaInstance)
    ['{DB284A86-CF5B-49DC-BC96-FBE3D60C9F2F}']
    procedure onFirstContentfulPaint(page: JPage; l: Int64); cdecl;
    procedure onNavigationCompleted(navigation: JNavigation); cdecl;
    procedure onNavigationRedirected(navigation: JNavigation); cdecl;
    procedure onNavigationStarted(navigation: JNavigation); cdecl;
    procedure onPageDeleted(page: JPage); cdecl;
    procedure onPageDomContentLoadedEvent(page: JPage); cdecl;
    procedure onPageLoadEvent(page: JPage); cdecl;
  end;
  TJNavigationListener = class(TJavaGenericImport<JNavigationListenerClass, JNavigationListener>) end;

  JNoVarySearchHeaderClass = interface(JObjectClass)
    ['{5F6819ED-7980-45BE-97B9-B5437CCEAD98}']
    {class} function alwaysVaryHeader: JNoVarySearchHeader; cdecl;
    {class} function neverVaryExcept(b: Boolean; list: JList): JNoVarySearchHeader; cdecl;
    {class} function neverVaryHeader: JNoVarySearchHeader; cdecl;
    {class} function varyExcept(b: Boolean; list: JList): JNoVarySearchHeader; cdecl;
  end;

  [JavaSignature('androidx/webkit/NoVarySearchHeader')]
  JNoVarySearchHeader = interface(JObject)
    ['{317E3D6B-1224-4D65-9C5D-F2ACD0C3038F}']
    function _GetconsideredQueryParameters: JList; cdecl;
    function _GetignoreDifferencesInParameters: Boolean; cdecl;
    function _GetignoredQueryParameters: JList; cdecl;
    function _GetvaryOnKeyOrder: Boolean; cdecl;
    property consideredQueryParameters: JList read _GetconsideredQueryParameters;
    property ignoreDifferencesInParameters: Boolean read _GetignoreDifferencesInParameters;
    property ignoredQueryParameters: JList read _GetignoredQueryParameters;
    property varyOnKeyOrder: Boolean read _GetvaryOnKeyOrder;
  end;
  TJNoVarySearchHeader = class(TJavaGenericImport<JNoVarySearchHeaderClass, JNoVarySearchHeader>) end;

  JOutcomeReceiverCompatClass = interface(IJavaClass)
    ['{2FA8BD09-BF16-4D85-9909-BC3B886C9CC4}']
  end;

  [JavaSignature('androidx/webkit/OutcomeReceiverCompat')]
  JOutcomeReceiverCompat = interface(IJavaInstance)
    ['{AA8A4D1D-7303-4E30-A8E3-BC48AA10D3DE}']
    procedure onError(throwable: JThrowable); cdecl;
    procedure onResult(object_: JObject); cdecl;
  end;
  TJOutcomeReceiverCompat = class(TJavaGenericImport<JOutcomeReceiverCompatClass, JOutcomeReceiverCompat>) end;

  JPageClass = interface(IJavaClass)
    ['{CAAA5D20-8F83-47D9-B3AF-3F3354F1358A}']
  end;

  [JavaSignature('androidx/webkit/Page')]
  JPage = interface(IJavaInstance)
    ['{2EE5FA29-B252-400B-A4F1-EFE2D64D1D49}']
  end;
  TJPage = class(TJavaGenericImport<JPageClass, JPage>) end;

  JPrefetchExceptionClass = interface(JExceptionClass)
    ['{06E08E24-92D8-4727-BDC7-BE7B4E832406}']
    {class} function init(string_: JString): JPrefetchException; cdecl; overload;
    {class} function init: JPrefetchException; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/PrefetchException')]
  JPrefetchException = interface(JException)
    ['{59CC2F85-410A-4F01-9854-17D6D9638194}']
  end;
  TJPrefetchException = class(TJavaGenericImport<JPrefetchExceptionClass, JPrefetchException>) end;

  JPrefetchNetworkExceptionClass = interface(JPrefetchExceptionClass)
    ['{D5655248-FCAB-4BE7-87B3-7C0558A22A57}']
    {class} function _GetNO_HTTP_RESPONSE_STATUS_CODE: Integer; cdecl;
    {class} function init(string_: JString): JPrefetchNetworkException; cdecl; overload;
    {class} function init(string_: JString; i: Integer): JPrefetchNetworkException; cdecl; overload;
    {class} function init(i: Integer): JPrefetchNetworkException; cdecl; overload;
    {class} function init: JPrefetchNetworkException; cdecl; overload;
    {class} property NO_HTTP_RESPONSE_STATUS_CODE: Integer read _GetNO_HTTP_RESPONSE_STATUS_CODE;
  end;

  [JavaSignature('androidx/webkit/PrefetchNetworkException')]
  JPrefetchNetworkException = interface(JPrefetchException)
    ['{96586BC5-C9DD-48B0-B5AB-99930046F3CD}']
    function _GethttpResponseStatusCode: Integer; cdecl;
    property httpResponseStatusCode: Integer read _GethttpResponseStatusCode;
  end;
  TJPrefetchNetworkException = class(TJavaGenericImport<JPrefetchNetworkExceptionClass, JPrefetchNetworkException>) end;

  JPrerenderExceptionClass = interface(JExceptionClass)
    ['{18AEB33C-8E59-4E1C-A9C0-A7EA56674959}']
    {class} function init(string_: JString; throwable: JThrowable): JPrerenderException; cdecl;
  end;

  [JavaSignature('androidx/webkit/PrerenderException')]
  JPrerenderException = interface(JException)
    ['{3CFDB5B2-E52D-4B17-B4D2-170624E85891}']
  end;
  TJPrerenderException = class(TJavaGenericImport<JPrerenderExceptionClass, JPrerenderException>) end;

  JPrerenderOperationCallbackClass = interface(IJavaClass)
    ['{3259EB3E-F0E1-428F-B9F4-F7149F39F490}']
  end;

  [JavaSignature('androidx/webkit/PrerenderOperationCallback')]
  JPrerenderOperationCallback = interface(IJavaInstance)
    ['{BECEFDBE-0C98-417C-A735-EA0C01FFBB78}']
    procedure onError(prerenderException: JPrerenderException); cdecl;
    procedure onPrerenderActivated; cdecl;
  end;
  TJPrerenderOperationCallback = class(TJavaGenericImport<JPrerenderOperationCallbackClass, JPrerenderOperationCallback>) end;

  JProcessGlobalConfigClass = interface(JObjectClass)
    ['{ECCFC435-F2A2-4720-9197-6EFE6302E27D}']
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_WITHOUT_MULTI_PROCESS_STARTUP: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_DEFAULT: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_SYNC: Integer; cdecl;
    {class} function init: JProcessGlobalConfig; cdecl;
    {class} procedure apply(processGlobalConfig: JProcessGlobalConfig); cdecl;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_WITHOUT_MULTI_PROCESS_STARTUP: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_WITHOUT_MULTI_PROCESS_STARTUP;
    {class} property UI_THREAD_STARTUP_MODE_DEFAULT: Integer read _GetUI_THREAD_STARTUP_MODE_DEFAULT;
    {class} property UI_THREAD_STARTUP_MODE_SYNC: Integer read _GetUI_THREAD_STARTUP_MODE_SYNC;
  end;

  [JavaSignature('androidx/webkit/ProcessGlobalConfig')]
  JProcessGlobalConfig = interface(JObject)
    ['{FE32F482-1F34-407E-B7E2-A42B67134069}']
    function setDataDirectorySuffix(context: JContext; string_: JString): JProcessGlobalConfig; cdecl;
    function setDirectoryBasePaths(context: JContext; file_: JFile; file_1: JFile): JProcessGlobalConfig; cdecl;
    function setPartitionedCookiesEnabled(context: JContext; b: Boolean): JProcessGlobalConfig; cdecl;
    function setUiThreadStartupMode(context: JContext; i: Integer): JProcessGlobalConfig; cdecl;
    function setUiThreadStartupModeV2(context: JContext; i: Integer): JProcessGlobalConfig; cdecl;
  end;
  TJProcessGlobalConfig = class(TJavaGenericImport<JProcessGlobalConfigClass, JProcessGlobalConfig>) end;

  JProcessGlobalConfig_UiThreadStartupModeClass = interface(JAnnotationClass)
    ['{817A4D7F-7CFB-41B6-B1CD-B8FE43A34092}']
  end;

  [JavaSignature('androidx/webkit/ProcessGlobalConfig$UiThreadStartupMode')]
  JProcessGlobalConfig_UiThreadStartupMode = interface(JAnnotation)
    ['{7C7439A9-C379-4D10-92A5-B33FB68795FA}']
  end;
  TJProcessGlobalConfig_UiThreadStartupMode = class(TJavaGenericImport<JProcessGlobalConfig_UiThreadStartupModeClass, JProcessGlobalConfig_UiThreadStartupMode>) end;

  JProfileClass = interface(IJavaClass)
    ['{1C956522-7E23-4A81-9A36-17A593A540C4}']
    {class} function _GetDEFAULT_PROFILE_NAME: JString; cdecl;
    {class} property DEFAULT_PROFILE_NAME: JString read _GetDEFAULT_PROFILE_NAME;
  end;

  [JavaSignature('androidx/webkit/Profile')]
  JProfile = interface(IJavaInstance)
    ['{12A2A76C-DAE9-47D2-9369-CB15A99340C1}']
    procedure addCustomHeader(customHeader: JCustomHeader); cdecl;
    procedure addQuicHints(set_: JSet); cdecl;
    procedure clearAllCustomHeaders; cdecl;
    procedure clearAllOriginMatchedHeaders; cdecl;
    procedure clearCustomHeader(string_: JString); cdecl; overload;
    procedure clearCustomHeader(string_: JString; string_1: JString); cdecl; overload;
    procedure clearOriginMatchedHeader(string_: JString); cdecl;
    procedure clearPrefetchAsync(string_: JString; executor: JExecutor; outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl;
    function getCookieManager: JCookieManager; cdecl;
    function getCustomHeaders: JSet; cdecl; overload;
    function getCustomHeaders(string_: JString): JSet; cdecl; overload;
    function getCustomHeaders(string_: JString; string_1: JString): JSet; cdecl; overload;
    function getGeolocationPermissions: JGeolocationPermissions; cdecl;
    function getName: JString; cdecl;
    function getServiceWorkerController: JServiceWorkerController; cdecl;
    function getWebStorage: JWebStorage; cdecl;
    function hasCustomHeader(string_: JString): Boolean; cdecl;
    function hasOriginMatchedHeader(string_: JString): Boolean; cdecl;
    procedure preconnect(string_: JString); cdecl;
    procedure prefetchUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl; overload;
    procedure prefetchUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      speculativeLoadingParameters: JSpeculativeLoadingParameters; outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl; overload;
    procedure setOriginMatchedHeader(string_: JString; string_1: JString; set_: JSet); cdecl;
    procedure setSpeculativeLoadingConfig(speculativeLoadingConfig: JSpeculativeLoadingConfig); cdecl;
    procedure warmUpRendererProcess; cdecl;
  end;
  TJProfile = class(TJavaGenericImport<JProfileClass, JProfile>) end;

  JProfile_ExperimentalAddQuicHintsClass = interface(JAnnotationClass)
    ['{D39C11B5-B6E0-4501-9728-0CD528292AA8}']
  end;

  [JavaSignature('androidx/webkit/Profile$ExperimentalAddQuicHints')]
  JProfile_ExperimentalAddQuicHints = interface(JAnnotation)
    ['{4D1E5716-686D-47DA-8C2C-E0A07BDA441E}']
  end;
  TJProfile_ExperimentalAddQuicHints = class(TJavaGenericImport<JProfile_ExperimentalAddQuicHintsClass, JProfile_ExperimentalAddQuicHints>) end;

  JProfile_ExperimentalOriginMatchedHeaderClass = interface(JAnnotationClass)
    ['{44220761-B48A-43F1-87EB-5E3EB485F920}']
  end;

  [JavaSignature('androidx/webkit/Profile$ExperimentalOriginMatchedHeader')]
  JProfile_ExperimentalOriginMatchedHeader = interface(JAnnotation)
    ['{E459A7AB-61C5-4249-A39F-D75FD0747CC8}']
  end;
  TJProfile_ExperimentalOriginMatchedHeader = class(TJavaGenericImport<JProfile_ExperimentalOriginMatchedHeaderClass, JProfile_ExperimentalOriginMatchedHeader>) end;

  JProfile_ExperimentalPreconnectClass = interface(JAnnotationClass)
    ['{23DCF4DD-96BD-4E39-A5B4-26465947CB3F}']
  end;

  [JavaSignature('androidx/webkit/Profile$ExperimentalPreconnect')]
  JProfile_ExperimentalPreconnect = interface(JAnnotation)
    ['{EFEBCB29-4603-4704-BF9A-712C73D071C4}']
  end;
  TJProfile_ExperimentalPreconnect = class(TJavaGenericImport<JProfile_ExperimentalPreconnectClass, JProfile_ExperimentalPreconnect>) end;

  JProfile_ExperimentalUrlPrefetchClass = interface(JAnnotationClass)
    ['{78985339-F483-43F8-A2D0-01E0853745D6}']
  end;

  [JavaSignature('androidx/webkit/Profile$ExperimentalUrlPrefetch')]
  JProfile_ExperimentalUrlPrefetch = interface(JAnnotation)
    ['{D0025310-6D5E-4329-B5E5-A2C146676AB7}']
  end;
  TJProfile_ExperimentalUrlPrefetch = class(TJavaGenericImport<JProfile_ExperimentalUrlPrefetchClass, JProfile_ExperimentalUrlPrefetch>) end;

  JProfile_ExperimentalWarmUpRendererProcessClass = interface(JAnnotationClass)
    ['{098467E8-BE4A-4653-8009-F4C979A1D2ED}']
  end;

  [JavaSignature('androidx/webkit/Profile$ExperimentalWarmUpRendererProcess')]
  JProfile_ExperimentalWarmUpRendererProcess = interface(JAnnotation)
    ['{CF48B5F6-55C9-4E4C-B7D2-1D1EEE89A4BE}']
  end;
  TJProfile_ExperimentalWarmUpRendererProcess = class(TJavaGenericImport<JProfile_ExperimentalWarmUpRendererProcessClass, JProfile_ExperimentalWarmUpRendererProcess>) end;

  JProfileStoreClass = interface(IJavaClass)
    ['{5A3537AE-31C0-41E6-8020-057BAF97DDFB}']
    {class} function getInstance: JProfileStore; cdecl;
  end;

  [JavaSignature('androidx/webkit/ProfileStore')]
  JProfileStore = interface(IJavaInstance)
    ['{D0280973-E35D-472E-99B2-F9DCCD2BD414}']
    function deleteProfile(string_: JString): Boolean; cdecl;
    function getAllProfileNames: JList; cdecl;
    function getOrCreateProfile(string_: JString): JProfile; cdecl;
    function getProfile(string_: JString): JProfile; cdecl;
  end;
  TJProfileStore = class(TJavaGenericImport<JProfileStoreClass, JProfileStore>) end;

  JProxyConfigClass = interface(JObjectClass)
    ['{BA98D8F4-2539-4137-A3B3-C9ACB592EF6D}']
    {class} function _GetMATCH_ALL_SCHEMES: JString; cdecl;
    {class} function _GetMATCH_HTTP: JString; cdecl;
    {class} function _GetMATCH_HTTPS: JString; cdecl;
    {class} function init(list: JList; list1: JList; b: Boolean): JProxyConfig; cdecl;
    {class} property MATCH_ALL_SCHEMES: JString read _GetMATCH_ALL_SCHEMES;
    {class} property MATCH_HTTP: JString read _GetMATCH_HTTP;
    {class} property MATCH_HTTPS: JString read _GetMATCH_HTTPS;
  end;

  [JavaSignature('androidx/webkit/ProxyConfig')]
  JProxyConfig = interface(JObject)
    ['{C3AE7130-33F1-493D-A947-E788C808CE3A}']
    function getBypassRules: JList; cdecl;
    function getProxyRules: JList; cdecl;
    function isReverseBypassEnabled: Boolean; cdecl;
  end;
  TJProxyConfig = class(TJavaGenericImport<JProxyConfigClass, JProxyConfig>) end;

  JProxyConfig_BuilderClass = interface(JObjectClass)
    ['{640BFCA1-974D-4B4F-861D-9FC21B162160}']
    {class} function init: JProxyConfig_Builder; cdecl; overload;
    {class} function init(proxyConfig: JProxyConfig): JProxyConfig_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/ProxyConfig$Builder')]
  JProxyConfig_Builder = interface(JObject)
    ['{0441D0BA-B980-4DBD-89AA-73ADA3DB0429}']
    function addBypassRule(string_: JString): JProxyConfig_Builder; cdecl;
    function addDirect(string_: JString): JProxyConfig_Builder; cdecl; overload;
    function addDirect: JProxyConfig_Builder; cdecl; overload;
    function addProxyRule(string_: JString): JProxyConfig_Builder; cdecl; overload;
    function addProxyRule(string_: JString; string_1: JString): JProxyConfig_Builder; cdecl; overload;
    function build: JProxyConfig; cdecl;
    function bypassSimpleHostnames: JProxyConfig_Builder; cdecl;
    function removeImplicitRules: JProxyConfig_Builder; cdecl;
    function setReverseBypassEnabled(b: Boolean): JProxyConfig_Builder; cdecl;
  end;
  TJProxyConfig_Builder = class(TJavaGenericImport<JProxyConfig_BuilderClass, JProxyConfig_Builder>) end;

  JProxyConfig_ProxyRuleClass = interface(JObjectClass)
    ['{9312B9AC-8D65-45D7-83AC-29D9A82D8E40}']
    {class} function init(string_: JString; string_1: JString): JProxyConfig_ProxyRule; cdecl; overload;
    {class} function init(string_: JString): JProxyConfig_ProxyRule; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/ProxyConfig$ProxyRule')]
  JProxyConfig_ProxyRule = interface(JObject)
    ['{F7DB49D4-2CBB-4E6C-A4FB-7E35A4306D65}']
    function getSchemeFilter: JString; cdecl;
    function getUrl: JString; cdecl;
  end;
  TJProxyConfig_ProxyRule = class(TJavaGenericImport<JProxyConfig_ProxyRuleClass, JProxyConfig_ProxyRule>) end;

  JProxyConfig_ProxySchemeClass = interface(JAnnotationClass)
    ['{F2C2BD45-A207-4D43-8DEC-AFF9116FBAA2}']
  end;

  [JavaSignature('androidx/webkit/ProxyConfig$ProxyScheme')]
  JProxyConfig_ProxyScheme = interface(JAnnotation)
    ['{443454A9-B139-464E-8175-8FD7B908945F}']
  end;
  TJProxyConfig_ProxyScheme = class(TJavaGenericImport<JProxyConfig_ProxySchemeClass, JProxyConfig_ProxyScheme>) end;

  JProxyControllerClass = interface(JObjectClass)
    ['{08A3AAF1-6565-4C1E-9FD0-9256C16E0608}']
    {class} function init: JProxyController; cdecl;
    {class} function getInstance: JProxyController; cdecl;
  end;

  [JavaSignature('androidx/webkit/ProxyController')]
  JProxyController = interface(JObject)
    ['{DEB6A3F6-D729-4286-8A9C-D72CD4F99DA1}']
    procedure clearProxyOverride(executor: JExecutor; runnable: JRunnable); cdecl;
    procedure setProxyOverride(proxyConfig: JProxyConfig; executor: JExecutor; runnable: JRunnable); cdecl;
  end;
  TJProxyController = class(TJavaGenericImport<JProxyControllerClass, JProxyController>) end;

  JRestrictionAllowlistClass = interface(JObjectClass)
    ['{DE51B63D-C198-426D-AFE7-CBE892B62873}']
  end;

  [JavaSignature('androidx/webkit/RestrictionAllowlist')]
  JRestrictionAllowlist = interface(JObject)
    ['{AA7804B7-07A5-45EE-B22B-DB30E653E2D4}']
  end;
  TJRestrictionAllowlist = class(TJavaGenericImport<JRestrictionAllowlistClass, JRestrictionAllowlist>) end;

  JRestrictionAllowlist_BuilderClass = interface(JObjectClass)
    ['{1981E1D7-831D-453F-BA2B-2F60BD0E5B91}']
    {class} function init(set_: JSet): JRestrictionAllowlist_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/RestrictionAllowlist$Builder')]
  JRestrictionAllowlist_Builder = interface(JObject)
    ['{A3D3B7A9-1AAB-4930-9422-8D5B6ACDB362}']
    function addJavaScriptInterface(object_: JObject; string_: JString): JRestrictionAllowlist_Builder; cdecl;
    function build: JRestrictionAllowlist; cdecl;
  end;
  TJRestrictionAllowlist_Builder = class(TJavaGenericImport<JRestrictionAllowlist_BuilderClass, JRestrictionAllowlist_Builder>) end;

  JSafeBrowsingResponseCompatClass = interface(JObjectClass)
    ['{7BC8035F-7EC6-4023-9F7E-976ECBC8042D}']
    {class} function init: JSafeBrowsingResponseCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/SafeBrowsingResponseCompat')]
  JSafeBrowsingResponseCompat = interface(JObject)
    ['{177D841D-5FB1-4697-883D-E25CC8F032D0}']
    procedure backToSafety(b: Boolean); cdecl;
    procedure proceed(b: Boolean); cdecl;
    procedure showInterstitial(b: Boolean); cdecl;
  end;
  TJSafeBrowsingResponseCompat = class(TJavaGenericImport<JSafeBrowsingResponseCompatClass, JSafeBrowsingResponseCompat>) end;

  JScriptHandlerClass = interface(IJavaClass)
    ['{E748AB02-61B0-4CBD-8086-3DBDFE683E40}']
  end;

  [JavaSignature('androidx/webkit/ScriptHandler')]
  JScriptHandler = interface(IJavaInstance)
    ['{B8F333DE-C1D2-4258-B7C8-062F4A4B6D61}']
    procedure remove; cdecl;
  end;
  TJScriptHandler = class(TJavaGenericImport<JScriptHandlerClass, JScriptHandler>) end;

  JServiceWorkerClientCompatClass = interface(JObjectClass)
    ['{BA9CEAA6-D8E8-46C8-9AD5-F219BA894DD1}']
    {class} function init: JServiceWorkerClientCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/ServiceWorkerClientCompat')]
  JServiceWorkerClientCompat = interface(JObject)
    ['{3BEE2A98-89F0-4C91-A51C-5C426ECA9147}']
    function shouldInterceptRequest(webResourceRequest: JWebResourceRequest): JWebResourceResponse; cdecl;
  end;
  TJServiceWorkerClientCompat = class(TJavaGenericImport<JServiceWorkerClientCompatClass, JServiceWorkerClientCompat>) end;

  JServiceWorkerControllerCompatClass = interface(JObjectClass)
    ['{14B90FB0-552E-43B7-8A99-90B1E6A4A5E5}']
    {class} function init: JServiceWorkerControllerCompat; cdecl;
    {class} function getInstance: JServiceWorkerControllerCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/ServiceWorkerControllerCompat')]
  JServiceWorkerControllerCompat = interface(JObject)
    ['{18A7F1AA-CFD5-45A0-B697-3BD8AA71CB7D}']
    function getServiceWorkerWebSettings: JServiceWorkerWebSettingsCompat; cdecl;
    procedure setServiceWorkerClient(serviceWorkerClientCompat: JServiceWorkerClientCompat); cdecl;
  end;
  TJServiceWorkerControllerCompat = class(TJavaGenericImport<JServiceWorkerControllerCompatClass, JServiceWorkerControllerCompat>) end;

  JServiceWorkerWebSettingsCompatClass = interface(JObjectClass)
    ['{4AA7771E-1E08-4CB7-A771-AE8BFFD314C9}']
    {class} function init: JServiceWorkerWebSettingsCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/ServiceWorkerWebSettingsCompat')]
  JServiceWorkerWebSettingsCompat = interface(JObject)
    ['{53ABF921-6716-4CEE-93B8-F55CB67B250A}']
    function getAllowContentAccess: Boolean; cdecl;
    function getAllowFileAccess: Boolean; cdecl;
    function getBlockNetworkLoads: Boolean; cdecl;
    function getCacheMode: Integer; cdecl;
    function getRequestedWithHeaderOriginAllowList: JSet; cdecl;
    function isIncludeCookiesOnShouldInterceptRequestEnabled: Boolean; cdecl;
    procedure setAllowContentAccess(b: Boolean); cdecl;
    procedure setAllowFileAccess(b: Boolean); cdecl;
    procedure setBlockNetworkLoads(b: Boolean); cdecl;
    procedure setCacheMode(i: Integer); cdecl;
    procedure setIncludeCookiesOnShouldInterceptRequestEnabled(b: Boolean); cdecl;
    procedure setRequestedWithHeaderOriginAllowList(set_: JSet); cdecl;
  end;
  TJServiceWorkerWebSettingsCompat = class(TJavaGenericImport<JServiceWorkerWebSettingsCompatClass, JServiceWorkerWebSettingsCompat>) end;

  JServiceWorkerWebSettingsCompat_CacheModeClass = interface(JAnnotationClass)
    ['{6AB21C26-7F38-4D6C-8FD5-E5EE457DE7F0}']
  end;

  [JavaSignature('androidx/webkit/ServiceWorkerWebSettingsCompat$CacheMode')]
  JServiceWorkerWebSettingsCompat_CacheMode = interface(JAnnotation)
    ['{A4205249-BC7F-414D-9565-7B8A8C4A7681}']
  end;
  TJServiceWorkerWebSettingsCompat_CacheMode = class(TJavaGenericImport<JServiceWorkerWebSettingsCompat_CacheModeClass, JServiceWorkerWebSettingsCompat_CacheMode>) end;

  JSpeculativeLoadingConfigClass = interface(JObjectClass)
    ['{0D9768AC-D841-4360-A036-CC252C158F4B}']
  end;

  [JavaSignature('androidx/webkit/SpeculativeLoadingConfig')]
  JSpeculativeLoadingConfig = interface(JObject)
    ['{B0F57CE4-5E9C-484A-A907-63022A013F36}']
    function getMaxPrefetches: Integer; cdecl;
    function getMaxPrerenders: Integer; cdecl;
    function getPrefetchTtlSeconds: Integer; cdecl;
  end;
  TJSpeculativeLoadingConfig = class(TJavaGenericImport<JSpeculativeLoadingConfigClass, JSpeculativeLoadingConfig>) end;

  JSpeculativeLoadingConfig_BuilderClass = interface(JObjectClass)
    ['{64ADAA82-14AC-4050-A960-9FB629143381}']
    {class} function init: JSpeculativeLoadingConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/SpeculativeLoadingConfig$Builder')]
  JSpeculativeLoadingConfig_Builder = interface(JObject)
    ['{B934DF10-2FF6-4449-8249-21C02830C59B}']
    function build: JSpeculativeLoadingConfig; cdecl;
    function setMaxPrefetches(i: Integer): JSpeculativeLoadingConfig_Builder; cdecl;
    function setMaxPrerenders(i: Integer): JSpeculativeLoadingConfig_Builder; cdecl;
    function setPrefetchTtlSeconds(i: Integer): JSpeculativeLoadingConfig_Builder; cdecl;
  end;
  TJSpeculativeLoadingConfig_Builder = class(TJavaGenericImport<JSpeculativeLoadingConfig_BuilderClass, JSpeculativeLoadingConfig_Builder>) end;

  JSpeculativeLoadingParametersClass = interface(JObjectClass)
    ['{C18D7A6E-2FDC-418D-8A4F-14C3BB066BF2}']
  end;

  [JavaSignature('androidx/webkit/SpeculativeLoadingParameters')]
  JSpeculativeLoadingParameters = interface(JObject)
    ['{C017A254-2409-4159-8F7E-94DB96109ED1}']
    function getAdditionalHeaders: JMap; cdecl;
    function getExpectedNoVarySearchData: JNoVarySearchHeader; cdecl;
    function isJavaScriptEnabled: Boolean; cdecl;
  end;
  TJSpeculativeLoadingParameters = class(TJavaGenericImport<JSpeculativeLoadingParametersClass, JSpeculativeLoadingParameters>) end;

  JSpeculativeLoadingParameters_BuilderClass = interface(JObjectClass)
    ['{BDDB48E3-A40D-466D-8EDC-3AD4743418E3}']
    {class} function init: JSpeculativeLoadingParameters_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/SpeculativeLoadingParameters$Builder')]
  JSpeculativeLoadingParameters_Builder = interface(JObject)
    ['{F95163E6-B91B-43F5-9993-F8D8DE1868EB}']
    function addAdditionalHeader(string_: JString; string_1: JString): JSpeculativeLoadingParameters_Builder; cdecl;
    function addAdditionalHeaders(map: JMap): JSpeculativeLoadingParameters_Builder; cdecl;
    function build: JSpeculativeLoadingParameters; cdecl;
    function setExpectedNoVarySearchData(noVarySearchHeader: JNoVarySearchHeader): JSpeculativeLoadingParameters_Builder; cdecl;
    function setJavaScriptEnabled(b: Boolean): JSpeculativeLoadingParameters_Builder; cdecl;
  end;
  TJSpeculativeLoadingParameters_Builder = class(TJavaGenericImport<JSpeculativeLoadingParameters_BuilderClass, JSpeculativeLoadingParameters_Builder>) end;

  JStartUpLocationClass = interface(IJavaClass)
    ['{6D43BA0F-FBD6-4278-B2AE-EDA6ADC2C60B}']
  end;

  [JavaSignature('androidx/webkit/StartUpLocation')]
  JStartUpLocation = interface(IJavaInstance)
    ['{F99E639B-2E57-4FF2-9995-0965F47E0DE6}']
    function getStackInformation: JString; cdecl;
  end;
  TJStartUpLocation = class(TJavaGenericImport<JStartUpLocationClass, JStartUpLocation>) end;

  Jwebkit_TracingConfigClass = interface(JObjectClass)
    ['{370BF595-AEA6-498D-BD2A-7BD023785E07}']
    {class} function _GetCATEGORIES_ALL: Integer; cdecl;
    {class} function _GetCATEGORIES_ANDROID_WEBVIEW: Integer; cdecl;
    {class} function _GetCATEGORIES_FRAME_VIEWER: Integer; cdecl;
    {class} function _GetCATEGORIES_INPUT_LATENCY: Integer; cdecl;
    {class} function _GetCATEGORIES_JAVASCRIPT_AND_RENDERING: Integer; cdecl;
    {class} function _GetCATEGORIES_NONE: Integer; cdecl;
    {class} function _GetCATEGORIES_RENDERING: Integer; cdecl;
    {class} function _GetCATEGORIES_WEB_DEVELOPER: Integer; cdecl;
    {class} function _GetRECORD_CONTINUOUSLY: Integer; cdecl;
    {class} function _GetRECORD_UNTIL_FULL: Integer; cdecl;
    {class} function init(i: Integer; list: JList; i1: Integer): Jwebkit_TracingConfig; cdecl;
    {class} property CATEGORIES_ALL: Integer read _GetCATEGORIES_ALL;
    {class} property CATEGORIES_ANDROID_WEBVIEW: Integer read _GetCATEGORIES_ANDROID_WEBVIEW;
    {class} property CATEGORIES_FRAME_VIEWER: Integer read _GetCATEGORIES_FRAME_VIEWER;
    {class} property CATEGORIES_INPUT_LATENCY: Integer read _GetCATEGORIES_INPUT_LATENCY;
    {class} property CATEGORIES_JAVASCRIPT_AND_RENDERING: Integer read _GetCATEGORIES_JAVASCRIPT_AND_RENDERING;
    {class} property CATEGORIES_NONE: Integer read _GetCATEGORIES_NONE;
    {class} property CATEGORIES_RENDERING: Integer read _GetCATEGORIES_RENDERING;
    {class} property CATEGORIES_WEB_DEVELOPER: Integer read _GetCATEGORIES_WEB_DEVELOPER;
    {class} property RECORD_CONTINUOUSLY: Integer read _GetRECORD_CONTINUOUSLY;
    {class} property RECORD_UNTIL_FULL: Integer read _GetRECORD_UNTIL_FULL;
  end;

  [JavaSignature('androidx/webkit/TracingConfig')]
  Jwebkit_TracingConfig = interface(JObject)
    ['{DE4723CF-6EFD-4A23-A743-358892F35F3F}']
    function getCustomIncludedCategories: JList; cdecl;
    function getPredefinedCategories: Integer; cdecl;
    function getTracingMode: Integer; cdecl;
  end;
  TJwebkit_TracingConfig = class(TJavaGenericImport<Jwebkit_TracingConfigClass, Jwebkit_TracingConfig>) end;

  Jwebkit_TracingConfig_BuilderClass = interface(JObjectClass)
    ['{7C32F395-8049-4154-9B43-BEEC8BA598D9}']
    {class} function init: Jwebkit_TracingConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/TracingConfig$Builder')]
  Jwebkit_TracingConfig_Builder = interface(JObject)
    ['{52200DD9-0B1A-4DF2-8E97-D67841D69BF1}']
    function addCategories(i: TJavaArray<Integer>): Jwebkit_TracingConfig_Builder; cdecl; overload;
    function addCategories(string_: TJavaObjectArray<JString>): Jwebkit_TracingConfig_Builder; cdecl; overload;
    function addCategories(collection: JCollection): Jwebkit_TracingConfig_Builder; cdecl; overload;
    function build: Jwebkit_TracingConfig; cdecl;
    function setTracingMode(i: Integer): Jwebkit_TracingConfig_Builder; cdecl;
  end;
  TJwebkit_TracingConfig_Builder = class(TJavaGenericImport<Jwebkit_TracingConfig_BuilderClass, Jwebkit_TracingConfig_Builder>) end;

  JTracingConfig_PredefinedCategoriesClass = interface(JAnnotationClass)
    ['{29459817-D64D-4FE1-A7F6-47199CF5CDC9}']
  end;

  [JavaSignature('androidx/webkit/TracingConfig$PredefinedCategories')]
  JTracingConfig_PredefinedCategories = interface(JAnnotation)
    ['{739D4242-AE7D-4757-A8B9-D50DB2F762E4}']
  end;
  TJTracingConfig_PredefinedCategories = class(TJavaGenericImport<JTracingConfig_PredefinedCategoriesClass, JTracingConfig_PredefinedCategories>) end;

  JTracingConfig_TracingModeClass = interface(JAnnotationClass)
    ['{566D8433-D4FD-49D0-B05B-6BB99AFA53D1}']
  end;

  [JavaSignature('androidx/webkit/TracingConfig$TracingMode')]
  JTracingConfig_TracingMode = interface(JAnnotation)
    ['{2BA0E2A1-6FFD-41FB-8DC2-383EFF6BE115}']
  end;
  TJTracingConfig_TracingMode = class(TJavaGenericImport<JTracingConfig_TracingModeClass, JTracingConfig_TracingMode>) end;

  Jwebkit_TracingControllerClass = interface(JObjectClass)
    ['{E4CC1019-D26B-4296-8D85-8AE1CFEF6E1D}']
    {class} function init: Jwebkit_TracingController; cdecl;
    {class} function getInstance: Jwebkit_TracingController; cdecl;
  end;

  [JavaSignature('androidx/webkit/TracingController')]
  Jwebkit_TracingController = interface(JObject)
    ['{D00F3C48-8C70-4F31-9FCA-9E344D07ECA7}']
    function isTracing: Boolean; cdecl;
    procedure start(tracingConfig: Jwebkit_TracingConfig); cdecl;
    function stop(outputStream: JOutputStream; executor: JExecutor): Boolean; cdecl;
  end;
  TJwebkit_TracingController = class(TJavaGenericImport<Jwebkit_TracingControllerClass, Jwebkit_TracingController>) end;

  JURLUtilCompatClass = interface(JObjectClass)
    ['{73D5BD59-4D40-4534-A255-68E2B96553D8}']
    {class} function getFilenameFromContentDisposition(string_: JString): JString; cdecl;
    {class} function guessFileName(string_: JString; string_1: JString; string_2: JString): JString; cdecl;
  end;

  [JavaSignature('androidx/webkit/URLUtilCompat')]
  JURLUtilCompat = interface(JObject)
    ['{90213ABE-17E5-4B80-804F-F58C669449D6}']
  end;
  TJURLUtilCompat = class(TJavaGenericImport<JURLUtilCompatClass, JURLUtilCompat>) end;

  JUserAgentMetadataClass = interface(JObjectClass)
    ['{9FA629A1-4375-4A8D-8690-5D91E43902C1}']
    {class} function _GetBITNESS_DEFAULT: Integer; cdecl;
    {class} function _GetFORM_FACTOR_AUTOMOTIVE: JString; cdecl;
    {class} function _GetFORM_FACTOR_DESKTOP: JString; cdecl;
    {class} function _GetFORM_FACTOR_EINK: JString; cdecl;
    {class} function _GetFORM_FACTOR_MOBILE: JString; cdecl;
    {class} function _GetFORM_FACTOR_TABLET: JString; cdecl;
    {class} function _GetFORM_FACTOR_WATCH: JString; cdecl;
    {class} function _GetFORM_FACTOR_XR: JString; cdecl;
    {class} property BITNESS_DEFAULT: Integer read _GetBITNESS_DEFAULT;
    {class} property FORM_FACTOR_AUTOMOTIVE: JString read _GetFORM_FACTOR_AUTOMOTIVE;
    {class} property FORM_FACTOR_DESKTOP: JString read _GetFORM_FACTOR_DESKTOP;
    {class} property FORM_FACTOR_EINK: JString read _GetFORM_FACTOR_EINK;
    {class} property FORM_FACTOR_MOBILE: JString read _GetFORM_FACTOR_MOBILE;
    {class} property FORM_FACTOR_TABLET: JString read _GetFORM_FACTOR_TABLET;
    {class} property FORM_FACTOR_WATCH: JString read _GetFORM_FACTOR_WATCH;
    {class} property FORM_FACTOR_XR: JString read _GetFORM_FACTOR_XR;
  end;

  [JavaSignature('androidx/webkit/UserAgentMetadata')]
  JUserAgentMetadata = interface(JObject)
    ['{95AC618B-DB1D-42A0-A9EC-B646A74E3393}']
    function equals(object_: JObject): Boolean; cdecl;
    function getArchitecture: JString; cdecl;
    function getBitness: Integer; cdecl;
    function getBrandVersionList: JList; cdecl;
    function getFormFactors: JList; cdecl;
    function getFullVersion: JString; cdecl;
    function getModel: JString; cdecl;
    function getPlatform: JString; cdecl;
    function getPlatformVersion: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isMobile: Boolean; cdecl;
    function isWow64: Boolean; cdecl;
  end;
  TJUserAgentMetadata = class(TJavaGenericImport<JUserAgentMetadataClass, JUserAgentMetadata>) end;

  JUserAgentMetadata_BrandVersionClass = interface(JObjectClass)
    ['{CD31579C-5887-46F7-BB74-A2855C1A1C00}']
  end;

  [JavaSignature('androidx/webkit/UserAgentMetadata$BrandVersion')]
  JUserAgentMetadata_BrandVersion = interface(JObject)
    ['{99F8917B-2EF0-450C-99DB-9E25444946E1}']
    function equals(object_: JObject): Boolean; cdecl;
    function getBrand: JString; cdecl;
    function getFullVersion: JString; cdecl;
    function getMajorVersion: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJUserAgentMetadata_BrandVersion = class(TJavaGenericImport<JUserAgentMetadata_BrandVersionClass, JUserAgentMetadata_BrandVersion>) end;

  JBrandVersion_BuilderClass = interface(JObjectClass)
    ['{17A4C232-E7E5-4FE9-AD10-6957DD03459C}']
    {class} function init: JBrandVersion_Builder; cdecl; overload;
    {class} function init(brandVersion: JUserAgentMetadata_BrandVersion): JBrandVersion_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/UserAgentMetadata$BrandVersion$Builder')]
  JBrandVersion_Builder = interface(JObject)
    ['{34D2AD62-F265-4AF8-9089-D013EB313FBC}']
    function build: JUserAgentMetadata_BrandVersion; cdecl;
    function setBrand(string_: JString): JBrandVersion_Builder; cdecl;
    function setFullVersion(string_: JString): JBrandVersion_Builder; cdecl;
    function setMajorVersion(string_: JString): JBrandVersion_Builder; cdecl;
  end;
  TJBrandVersion_Builder = class(TJavaGenericImport<JBrandVersion_BuilderClass, JBrandVersion_Builder>) end;

  JUserAgentMetadata_BuilderClass = interface(JObjectClass)
    ['{EF4196DE-4564-4646-99F5-E61E569A3254}']
    {class} function init: JUserAgentMetadata_Builder; cdecl; overload;
    {class} function init(userAgentMetadata: JUserAgentMetadata): JUserAgentMetadata_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/UserAgentMetadata$Builder')]
  JUserAgentMetadata_Builder = interface(JObject)
    ['{59EEE457-F71D-4944-84A3-603FD7A0E2CB}']
    function build: JUserAgentMetadata; cdecl;
    function setArchitecture(string_: JString): JUserAgentMetadata_Builder; cdecl;
    function setBitness(i: Integer): JUserAgentMetadata_Builder; cdecl;
    function setBrandVersionList(list: JList): JUserAgentMetadata_Builder; cdecl;
    function setFormFactors(list: JList): JUserAgentMetadata_Builder; cdecl;
    function setFullVersion(string_: JString): JUserAgentMetadata_Builder; cdecl;
    function setMobile(b: Boolean): JUserAgentMetadata_Builder; cdecl;
    function setModel(string_: JString): JUserAgentMetadata_Builder; cdecl;
    function setPlatform(string_: JString): JUserAgentMetadata_Builder; cdecl;
    function setPlatformVersion(string_: JString): JUserAgentMetadata_Builder; cdecl;
    function setWow64(b: Boolean): JUserAgentMetadata_Builder; cdecl;
  end;
  TJUserAgentMetadata_Builder = class(TJavaGenericImport<JUserAgentMetadata_BuilderClass, JUserAgentMetadata_Builder>) end;

  JUserAgentMetadata_FormFactorsClass = interface(JAnnotationClass)
    ['{86CB0AB8-70E4-4E7B-A855-1ADB03790B5D}']
  end;

  [JavaSignature('androidx/webkit/UserAgentMetadata$FormFactors')]
  JUserAgentMetadata_FormFactors = interface(JAnnotation)
    ['{EDF7EAFF-50AB-4C60-B171-D6D82A10934B}']
  end;
  TJUserAgentMetadata_FormFactors = class(TJavaGenericImport<JUserAgentMetadata_FormFactorsClass, JUserAgentMetadata_FormFactors>) end;

  JWebMessageCompatClass = interface(JObjectClass)
    ['{C2A4FB69-4F15-473C-ACF6-89952AE9B834}']
    {class} function _GetTYPE_ARRAY_BUFFER: Integer; cdecl;
    {class} function _GetTYPE_STRING: Integer; cdecl;
    {class} function init(string_: JString): JWebMessageCompat; cdecl; overload;
    {class} function init(string_: JString; webMessagePortCompat: TJavaObjectArray<JWebMessagePortCompat>): JWebMessageCompat; cdecl; overload;
    {class} function init(b: TJavaArray<Byte>): JWebMessageCompat; cdecl; overload;
    {class} function init(b: TJavaArray<Byte>; webMessagePortCompat: TJavaObjectArray<JWebMessagePortCompat>): JWebMessageCompat; cdecl; overload;
    {class} property TYPE_ARRAY_BUFFER: Integer read _GetTYPE_ARRAY_BUFFER;
    {class} property TYPE_STRING: Integer read _GetTYPE_STRING;
  end;

  [JavaSignature('androidx/webkit/WebMessageCompat')]
  JWebMessageCompat = interface(JObject)
    ['{5A5AFA86-3D4A-48C6-8062-195A336A6E34}']
    function getArrayBuffer: TJavaArray<Byte>; cdecl;
    function getData: JString; cdecl;
    function getPorts: TJavaObjectArray<JWebMessagePortCompat>; cdecl;
    function getType: Integer; cdecl;
  end;
  TJWebMessageCompat = class(TJavaGenericImport<JWebMessageCompatClass, JWebMessageCompat>) end;

  JWebMessageCompat_TypeClass = interface(JAnnotationClass)
    ['{C4CFE4C1-2B66-45F4-A24E-C4E266D2D1BA}']
  end;

  [JavaSignature('androidx/webkit/WebMessageCompat$Type')]
  JWebMessageCompat_Type = interface(JAnnotation)
    ['{9627767D-83D3-4CFD-94F9-CC3A6F2C7464}']
  end;
  TJWebMessageCompat_Type = class(TJavaGenericImport<JWebMessageCompat_TypeClass, JWebMessageCompat_Type>) end;

  JWebMessagePortCompatClass = interface(JObjectClass)
    ['{F5501FC6-34FE-47EE-9B1C-7878094449CD}']
    {class} function init: JWebMessagePortCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebMessagePortCompat')]
  JWebMessagePortCompat = interface(JObject)
    ['{D87B0870-6254-4141-9D63-EAC4CF011082}']
    procedure close; cdecl;
    function getFrameworkPort: JWebMessagePort; cdecl;
    function getInvocationHandler: JInvocationHandler; cdecl;
    procedure postMessage(webMessageCompat: JWebMessageCompat); cdecl;
    procedure setWebMessageCallback(webMessageCallbackCompat: JWebMessagePortCompat_WebMessageCallbackCompat); cdecl; overload;
    procedure setWebMessageCallback(handler: JHandler; webMessageCallbackCompat: JWebMessagePortCompat_WebMessageCallbackCompat); cdecl; overload;
  end;
  TJWebMessagePortCompat = class(TJavaGenericImport<JWebMessagePortCompatClass, JWebMessagePortCompat>) end;

  JWebMessagePortCompat_WebMessageCallbackCompatClass = interface(JObjectClass)
    ['{0A596526-672B-4E94-90C8-24EC1FEB2388}']
    {class} function init: JWebMessagePortCompat_WebMessageCallbackCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebMessagePortCompat$WebMessageCallbackCompat')]
  JWebMessagePortCompat_WebMessageCallbackCompat = interface(JObject)
    ['{AC47094F-0354-4110-AFC5-B165FD86A701}']
    procedure onMessage(webMessagePortCompat: JWebMessagePortCompat; webMessageCompat: JWebMessageCompat); cdecl;
  end;
  TJWebMessagePortCompat_WebMessageCallbackCompat = class(TJavaGenericImport<JWebMessagePortCompat_WebMessageCallbackCompatClass, JWebMessagePortCompat_WebMessageCallbackCompat>) end;

  JWebNavigationClientClass = interface(IJavaClass)
    ['{ABDD2BF6-EFCD-4E03-B623-2070D123D81B}']
  end;

  [JavaSignature('androidx/webkit/WebNavigationClient')]
  JWebNavigationClient = interface(IJavaInstance)
    ['{7FE2DBBE-93CB-43B6-B250-507A9837182F}']
    procedure onFirstContentfulPaint(page: JPage); cdecl;
    procedure onNavigationCompleted(navigation: JNavigation); cdecl;
    procedure onNavigationRedirected(navigation: JNavigation); cdecl;
    procedure onNavigationStarted(navigation: JNavigation); cdecl;
    procedure onPageDeleted(page: JPage); cdecl;
    procedure onPageDomContentLoadedEventFired(page: JPage); cdecl;
    procedure onPageLoadEventFired(page: JPage); cdecl;
  end;
  TJWebNavigationClient = class(TJavaGenericImport<JWebNavigationClientClass, JWebNavigationClient>) end;

  JWebNavigationClient_ExperimentalNavigationCallbackClass = interface(JAnnotationClass)
    ['{4DFD811F-DC49-40E4-8303-6F7F19E77FBE}']
  end;

  [JavaSignature('androidx/webkit/WebNavigationClient$ExperimentalNavigationCallback')]
  JWebNavigationClient_ExperimentalNavigationCallback = interface(JAnnotation)
    ['{D97F5A5C-8366-48EB-A913-275A296F8339}']
  end;
  TJWebNavigationClient_ExperimentalNavigationCallback = class(TJavaGenericImport<JWebNavigationClient_ExperimentalNavigationCallbackClass, JWebNavigationClient_ExperimentalNavigationCallback>) end;

  JWebResourceErrorCompatClass = interface(JObjectClass)
    ['{6F80A9BD-8E0D-4306-BAEF-383B6F6225CE}']
    {class} function init: JWebResourceErrorCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebResourceErrorCompat')]
  JWebResourceErrorCompat = interface(JObject)
    ['{CDC7E882-3C4A-4185-B964-44E47F98029A}']
    function getDescription: JCharSequence; cdecl;
    function getErrorCode: Integer; cdecl;
  end;
  TJWebResourceErrorCompat = class(TJavaGenericImport<JWebResourceErrorCompatClass, JWebResourceErrorCompat>) end;

  JWebResourceErrorCompat_NetErrorCodeClass = interface(JAnnotationClass)
    ['{2914556E-9BB4-4DA1-9425-95613ED5A8B4}']
  end;

  [JavaSignature('androidx/webkit/WebResourceErrorCompat$NetErrorCode')]
  JWebResourceErrorCompat_NetErrorCode = interface(JAnnotation)
    ['{E8E8B506-63C7-4D04-B57B-4684BB2A29FD}']
  end;
  TJWebResourceErrorCompat_NetErrorCode = class(TJavaGenericImport<JWebResourceErrorCompat_NetErrorCodeClass, JWebResourceErrorCompat_NetErrorCode>) end;

  JWebResourceRequestCompatClass = interface(JObjectClass)
    ['{03592EE6-6264-44D8-8919-10A8D27234C0}']
    {class} function isRedirect(webResourceRequest: JWebResourceRequest): Boolean; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebResourceRequestCompat')]
  JWebResourceRequestCompat = interface(JObject)
    ['{B2B2DFAE-0594-4ED1-B1FA-F2C427D9E376}']
  end;
  TJWebResourceRequestCompat = class(TJavaGenericImport<JWebResourceRequestCompatClass, JWebResourceRequestCompat>) end;

  JWebResourceResponseCompatClass = interface(JObjectClass)
    ['{1ABE916C-2DB0-4373-BB8B-1ACA7917E6E3}']
    {class} function init(string_: JString; string_1: JString; inputStream: JInputStream): JWebResourceResponseCompat; cdecl; overload;
    {class} function init(string_: JString; string_1: JString; i: Integer; string_2: JString; map: JMap;
      inputStream: JInputStream): JWebResourceResponseCompat; cdecl; overload;
    {class} function toWebResourceResponseCompat(webResourceResponse: JWebResourceResponse): JWebResourceResponseCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebResourceResponseCompat')]
  JWebResourceResponseCompat = interface(JObject)
    ['{36E83DCD-4ED1-47A0-9559-67319266E65E}']
    function getCookies: JList; cdecl;
    function getData: JInputStream; cdecl;
    function getEncoding: JString; cdecl;
    function getMimeType: JString; cdecl;
    function getReasonPhrase: JString; cdecl;
    function getResponseHeaders: JMap; cdecl;
    function getStatusCode: Integer; cdecl;
    procedure setCookies(list: JList); cdecl;
    procedure setData(inputStream: JInputStream); cdecl;
    procedure setEncoding(string_: JString); cdecl;
    procedure setMimeType(string_: JString); cdecl;
    procedure setResponseHeaders(map: JMap); cdecl;
    procedure setStatusCodeAndReasonPhrase(i: Integer; string_: JString); cdecl;
    function toWebResourceResponse: JWebResourceResponse; cdecl;
  end;
  TJWebResourceResponseCompat = class(TJavaGenericImport<JWebResourceResponseCompatClass, JWebResourceResponseCompat>) end;

  JWebSettingsCompatClass = interface(JObjectClass)
    ['{C2ABBE86-4D8A-431C-8945-0C470744C380}']
    {class} function _GetATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_APP_TRIGGER: Integer; cdecl;
    {class} function _GetATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_WEB_TRIGGER: Integer; cdecl;
    {class} function _GetATTRIBUTION_BEHAVIOR_DISABLED: Integer; cdecl;
    {class} function _GetATTRIBUTION_BEHAVIOR_WEB_SOURCE_AND_WEB_TRIGGER: Integer; cdecl;
    {class} function _GetDARK_STRATEGY_PREFER_WEB_THEME_OVER_USER_AGENT_DARKENING: Integer; cdecl;
    {class} function _GetDARK_STRATEGY_USER_AGENT_DARKENING_ONLY: Integer; cdecl;
    {class} function _GetDARK_STRATEGY_WEB_THEME_DARKENING_ONLY: Integer; cdecl;
    {class} function _GetFORCE_DARK_AUTO: Integer; cdecl;
    {class} function _GetFORCE_DARK_OFF: Integer; cdecl;
    {class} function _GetFORCE_DARK_ON: Integer; cdecl;
    {class} function _GetSPECULATIVE_LOADING_DISABLED: Integer; cdecl;
    {class} function _GetSPECULATIVE_LOADING_PRERENDER_ENABLED: Integer; cdecl;
    {class} function _GetWEB_AUTHENTICATION_SUPPORT_FOR_APP: Integer; cdecl;
    {class} function _GetWEB_AUTHENTICATION_SUPPORT_FOR_BROWSER: Integer; cdecl;
    {class} function _GetWEB_AUTHENTICATION_SUPPORT_NONE: Integer; cdecl;
    {class} function areCookiesIncludedInShouldInterceptRequest(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getAttributionRegistrationBehavior(webSettings: JWebSettings): Integer; cdecl;
    {class} function getBackForwardCacheEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getBackForwardCacheSettings(webSettings: JWebSettings): JBackForwardCacheSettings; cdecl;
    {class} function getDisabledActionModeMenuItems(webSettings: JWebSettings): Integer; cdecl;
    {class} function getEnterpriseAuthenticationAppLinkPolicyEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getForceDark(webSettings: JWebSettings): Integer; cdecl;
    {class} function getForceDarkStrategy(webSettings: JWebSettings): Integer; cdecl;
    {class} function getHasEnrolledInstrumentEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getOffscreenPreRaster(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getPaymentRequestEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getRequestedWithHeaderOriginAllowList(webSettings: JWebSettings): JSet; cdecl;
    {class} function getSafeBrowsingEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getSpeculativeLoadingStatus(webSettings: JWebSettings): Integer; cdecl;
    {class} function getUserAgentMetadata(webSettings: JWebSettings): JUserAgentMetadata; cdecl;
    {class} function getWebAuthenticationSupport(webSettings: JWebSettings): Integer; cdecl;
    {class} function getWebViewMediaIntegrityApiStatus(webSettings: JWebSettings): JWebViewMediaIntegrityApiStatusConfig; cdecl;
    {class} function isAlgorithmicDarkeningAllowed(webSettings: JWebSettings): Boolean; cdecl;
    {class} procedure setAlgorithmicDarkeningAllowed(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setAttributionRegistrationBehavior(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setBackForwardCacheEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setBackForwardCacheSettings(webSettings: JWebSettings; backForwardCacheSettings: JBackForwardCacheSettings); cdecl;
    {class} procedure setCookiesIncludedInShouldInterceptRequest(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setDisabledActionModeMenuItems(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setEnterpriseAuthenticationAppLinkPolicyEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setForceDark(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setForceDarkStrategy(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setHasEnrolledInstrumentEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setHyperlinkContextMenuItems(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setOffscreenPreRaster(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setPaymentRequestEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setRequestedWithHeaderOriginAllowList(webSettings: JWebSettings; set_: JSet); cdecl;
    {class} procedure setSafeBrowsingEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
    {class} procedure setSpeculativeLoadingStatus(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setUserAgentMetadata(webSettings: JWebSettings; userAgentMetadata: JUserAgentMetadata); cdecl;
    {class} procedure setWebAuthenticationSupport(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setWebViewMediaIntegrityApiStatus(webSettings: JWebSettings; webViewMediaIntegrityApiStatusConfig: JWebViewMediaIntegrityApiStatusConfig); cdecl;
    {class} property ATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_APP_TRIGGER: Integer read _GetATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_APP_TRIGGER;
    {class} property ATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_WEB_TRIGGER: Integer read _GetATTRIBUTION_BEHAVIOR_APP_SOURCE_AND_WEB_TRIGGER;
    {class} property ATTRIBUTION_BEHAVIOR_DISABLED: Integer read _GetATTRIBUTION_BEHAVIOR_DISABLED;
    {class} property ATTRIBUTION_BEHAVIOR_WEB_SOURCE_AND_WEB_TRIGGER: Integer read _GetATTRIBUTION_BEHAVIOR_WEB_SOURCE_AND_WEB_TRIGGER;
    {class} property DARK_STRATEGY_PREFER_WEB_THEME_OVER_USER_AGENT_DARKENING: Integer read _GetDARK_STRATEGY_PREFER_WEB_THEME_OVER_USER_AGENT_DARKENING;
    {class} property DARK_STRATEGY_USER_AGENT_DARKENING_ONLY: Integer read _GetDARK_STRATEGY_USER_AGENT_DARKENING_ONLY;
    {class} property DARK_STRATEGY_WEB_THEME_DARKENING_ONLY: Integer read _GetDARK_STRATEGY_WEB_THEME_DARKENING_ONLY;
    {class} property FORCE_DARK_AUTO: Integer read _GetFORCE_DARK_AUTO;
    {class} property FORCE_DARK_OFF: Integer read _GetFORCE_DARK_OFF;
    {class} property FORCE_DARK_ON: Integer read _GetFORCE_DARK_ON;
    {class} property SPECULATIVE_LOADING_DISABLED: Integer read _GetSPECULATIVE_LOADING_DISABLED;
    {class} property SPECULATIVE_LOADING_PRERENDER_ENABLED: Integer read _GetSPECULATIVE_LOADING_PRERENDER_ENABLED;
    {class} property WEB_AUTHENTICATION_SUPPORT_FOR_APP: Integer read _GetWEB_AUTHENTICATION_SUPPORT_FOR_APP;
    {class} property WEB_AUTHENTICATION_SUPPORT_FOR_BROWSER: Integer read _GetWEB_AUTHENTICATION_SUPPORT_FOR_BROWSER;
    {class} property WEB_AUTHENTICATION_SUPPORT_NONE: Integer read _GetWEB_AUTHENTICATION_SUPPORT_NONE;
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat')]
  JWebSettingsCompat = interface(JObject)
    ['{DA8F0EF9-72D6-4508-B2AB-2017A263489F}']
  end;
  TJWebSettingsCompat = class(TJavaGenericImport<JWebSettingsCompatClass, JWebSettingsCompat>) end;

  JWebSettingsCompat_ExperimentalBackForwardCacheSettingsClass = interface(JAnnotationClass)
    ['{D229E2D1-A53A-4987-8A5B-D78E8F51FF5A}']
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$ExperimentalBackForwardCacheSettings')]
  JWebSettingsCompat_ExperimentalBackForwardCacheSettings = interface(JAnnotation)
    ['{A3CDA943-40AA-4869-B7F2-D2E4844B8AB6}']
  end;
  TJWebSettingsCompat_ExperimentalBackForwardCacheSettings = class(TJavaGenericImport<JWebSettingsCompat_ExperimentalBackForwardCacheSettingsClass, JWebSettingsCompat_ExperimentalBackForwardCacheSettings>) end;

  JWebSettingsCompat_ExperimentalSpeculativeLoadingClass = interface(JAnnotationClass)
    ['{57A9631A-6507-492C-BD95-576175EFD98F}']
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$ExperimentalSpeculativeLoading')]
  JWebSettingsCompat_ExperimentalSpeculativeLoading = interface(JAnnotation)
    ['{18709DE7-C09B-4F9A-A4DB-1CBE5664F5BD}']
  end;
  TJWebSettingsCompat_ExperimentalSpeculativeLoading = class(TJavaGenericImport<JWebSettingsCompat_ExperimentalSpeculativeLoadingClass, JWebSettingsCompat_ExperimentalSpeculativeLoading>) end;

  JWebSettingsCompat_ForceDarkClass = interface(JAnnotationClass)
    ['{8C3FE85C-8172-46FD-8FFE-E35FF1C29389}']
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$ForceDark')]
  JWebSettingsCompat_ForceDark = interface(JAnnotation)
    ['{B24760CD-4B12-4E56-8EF6-543B2822AE93}']
  end;
  TJWebSettingsCompat_ForceDark = class(TJavaGenericImport<JWebSettingsCompat_ForceDarkClass, JWebSettingsCompat_ForceDark>) end;

  JWebSettingsCompat_ForceDarkStrategyClass = interface(JAnnotationClass)
    ['{E86FCB12-86E4-4B20-858F-3863D6D65E83}']
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$ForceDarkStrategy')]
  JWebSettingsCompat_ForceDarkStrategy = interface(JAnnotation)
    ['{08899ABF-11A2-4F5C-948A-6F0BF6BB8A4B}']
  end;
  TJWebSettingsCompat_ForceDarkStrategy = class(TJavaGenericImport<JWebSettingsCompat_ForceDarkStrategyClass, JWebSettingsCompat_ForceDarkStrategy>) end;

  JWebSettingsCompat_HyperlinkContextMenuItemsClass = interface(JAnnotationClass)
    ['{5C34921F-A73C-487A-8AF6-7913832C9D8B}']
    {class} function _GetCOPY_LINK_ADDRESS: Integer; cdecl;
    {class} function _GetCOPY_LINK_TEXT: Integer; cdecl;
    {class} function _GetDISABLED: Integer; cdecl;
    {class} function _GetOPEN_LINK: Integer; cdecl;
    {class} property COPY_LINK_ADDRESS: Integer read _GetCOPY_LINK_ADDRESS;
    {class} property COPY_LINK_TEXT: Integer read _GetCOPY_LINK_TEXT;
    {class} property DISABLED: Integer read _GetDISABLED;
    {class} property OPEN_LINK: Integer read _GetOPEN_LINK;
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$HyperlinkContextMenuItems')]
  JWebSettingsCompat_HyperlinkContextMenuItems = interface(JAnnotation)
    ['{D7B1348A-D60C-4ACC-8C73-77AAE7378F6A}']
  end;
  TJWebSettingsCompat_HyperlinkContextMenuItems = class(TJavaGenericImport<JWebSettingsCompat_HyperlinkContextMenuItemsClass, JWebSettingsCompat_HyperlinkContextMenuItems>) end;

  JWebSettingsCompat_MenuItemFlagsClass = interface(JAnnotationClass)
    ['{357699FF-3C84-413A-916A-B36DF19C6A96}']
  end;

  [JavaSignature('androidx/webkit/WebSettingsCompat$MenuItemFlags')]
  JWebSettingsCompat_MenuItemFlags = interface(JAnnotation)
    ['{66E15466-900A-475C-98C7-DB704DD5D866}']
  end;
  TJWebSettingsCompat_MenuItemFlags = class(TJavaGenericImport<JWebSettingsCompat_MenuItemFlagsClass, JWebSettingsCompat_MenuItemFlags>) end;

  JWebStorageCompatClass = interface(JObjectClass)
    ['{18C89EB8-3BED-4DF4-A9CE-EA388BD00A53}']
    {class} procedure deleteBrowsingData(webStorage: JWebStorage; executor: JExecutor; runnable: JRunnable); cdecl; overload;
    {class} procedure deleteBrowsingData(webStorage: JWebStorage; runnable: JRunnable); cdecl; overload;
    {class} function deleteBrowsingDataForSite(webStorage: JWebStorage; string_: JString; executor: JExecutor; runnable: JRunnable): JString; cdecl; overload;
    {class} function deleteBrowsingDataForSite(webStorage: JWebStorage; string_: JString; runnable: JRunnable): JString; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/WebStorageCompat')]
  JWebStorageCompat = interface(JObject)
    ['{F57D271E-696C-4764-BD0D-78F946BDDE06}']
  end;
  TJWebStorageCompat = class(TJavaGenericImport<JWebStorageCompatClass, JWebStorageCompat>) end;

  JWebViewAssetLoaderClass = interface(JObjectClass)
    ['{56A00E05-2127-4304-8B34-1A7E02AB83C4}']
    {class} function _GetDEFAULT_DOMAIN: JString; cdecl;
    {class} property DEFAULT_DOMAIN: JString read _GetDEFAULT_DOMAIN;
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader')]
  JWebViewAssetLoader = interface(JObject)
    ['{CD5D98B7-78A8-456C-AC24-3BC79D0F58B1}']
    function shouldInterceptRequest(uri: Jnet_Uri): JWebResourceResponse; cdecl;
  end;
  TJWebViewAssetLoader = class(TJavaGenericImport<JWebViewAssetLoaderClass, JWebViewAssetLoader>) end;

  JWebViewAssetLoader_AssetsPathHandlerClass = interface(JObjectClass)
    ['{38AF8622-B9B2-4DBD-B7D6-7E3E60FB0F21}']
    {class} function init(context: JContext): JWebViewAssetLoader_AssetsPathHandler; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader$AssetsPathHandler')]
  JWebViewAssetLoader_AssetsPathHandler = interface(JObject)
    ['{A0540AC0-967E-4114-A2DD-8B0D3954F870}']
    function handle(string_: JString): JWebResourceResponse; cdecl;
  end;
  TJWebViewAssetLoader_AssetsPathHandler = class(TJavaGenericImport<JWebViewAssetLoader_AssetsPathHandlerClass, JWebViewAssetLoader_AssetsPathHandler>) end;

  JWebViewAssetLoader_BuilderClass = interface(JObjectClass)
    ['{09CFF241-1DD8-46B8-93B6-7CFB4A0E282D}']
    {class} function init: JWebViewAssetLoader_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader$Builder')]
  JWebViewAssetLoader_Builder = interface(JObject)
    ['{4C70C4AB-5B2F-4F14-A13E-72CCA07809E1}']
    function addPathHandler(string_: JString; pathHandler: JWebViewAssetLoader_PathHandler): JWebViewAssetLoader_Builder; cdecl;
    function build: JWebViewAssetLoader; cdecl;
    function setDomain(string_: JString): JWebViewAssetLoader_Builder; cdecl;
    function setHttpAllowed(b: Boolean): JWebViewAssetLoader_Builder; cdecl;
  end;
  TJWebViewAssetLoader_Builder = class(TJavaGenericImport<JWebViewAssetLoader_BuilderClass, JWebViewAssetLoader_Builder>) end;

  JWebViewAssetLoader_InternalStoragePathHandlerClass = interface(JObjectClass)
    ['{6F348CA7-D585-4733-9EB4-449B46C8041A}']
    {class} function init(context: JContext; file_: JFile): JWebViewAssetLoader_InternalStoragePathHandler; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader$InternalStoragePathHandler')]
  JWebViewAssetLoader_InternalStoragePathHandler = interface(JObject)
    ['{F473B938-810C-443B-8347-BF84113470FE}']
    function handle(string_: JString): JWebResourceResponse; cdecl;
  end;
  TJWebViewAssetLoader_InternalStoragePathHandler = class(TJavaGenericImport<JWebViewAssetLoader_InternalStoragePathHandlerClass, JWebViewAssetLoader_InternalStoragePathHandler>) end;

  JWebViewAssetLoader_PathHandlerClass = interface(IJavaClass)
    ['{98D6B79D-083D-402F-8799-22D7A2E0B5AC}']
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader$PathHandler')]
  JWebViewAssetLoader_PathHandler = interface(IJavaInstance)
    ['{25B64916-FEE4-4A3E-98D7-C4303FC8C662}']
    function handle(string_: JString): JWebResourceResponse; cdecl;
  end;
  TJWebViewAssetLoader_PathHandler = class(TJavaGenericImport<JWebViewAssetLoader_PathHandlerClass, JWebViewAssetLoader_PathHandler>) end;

  JWebViewAssetLoader_ResourcesPathHandlerClass = interface(JObjectClass)
    ['{52A878A8-CD61-4BF9-A568-3E062AD5B72C}']
    {class} function init(context: JContext): JWebViewAssetLoader_ResourcesPathHandler; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewAssetLoader$ResourcesPathHandler')]
  JWebViewAssetLoader_ResourcesPathHandler = interface(JObject)
    ['{52279B6D-5788-4350-80EA-DB7596E7A32A}']
    function handle(string_: JString): JWebResourceResponse; cdecl;
  end;
  TJWebViewAssetLoader_ResourcesPathHandler = class(TJavaGenericImport<JWebViewAssetLoader_ResourcesPathHandlerClass, JWebViewAssetLoader_ResourcesPathHandler>) end;

  JWebViewBuilderClass = interface(JObjectClass)
    ['{D9AFC7E5-5EF9-46BD-A20C-E51B3DD71942}']
    {class} function _GetPRESET_LEGACY: Integer; cdecl;
    {class} function init(i: Integer): JWebViewBuilder; cdecl;
    {class} property PRESET_LEGACY: Integer read _GetPRESET_LEGACY;
  end;

  [JavaSignature('androidx/webkit/WebViewBuilder')]
  JWebViewBuilder = interface(JObject)
    ['{4D34D5F8-EA6B-4EC2-97A1-2F64A4E46DFC}']
    function addAllowlist(restrictionAllowlist: JRestrictionAllowlist): JWebViewBuilder; cdecl;
    function build(context: JContext): JWebView; cdecl;
    function restrictJavaScriptInterfaces: JWebViewBuilder; cdecl;
    function setProfile(string_: JString): JWebViewBuilder; cdecl;
  end;
  TJWebViewBuilder = class(TJavaGenericImport<JWebViewBuilderClass, JWebViewBuilder>) end;

  JWebViewBuilder_ExperimentalClass = interface(JAnnotationClass)
    ['{E2345A02-0E3A-4F35-BF2B-FCCF542396C7}']
  end;

  [JavaSignature('androidx/webkit/WebViewBuilder$Experimental')]
  JWebViewBuilder_Experimental = interface(JAnnotation)
    ['{9EDC9980-60E3-4436-81A6-4BA141470783}']
  end;
  TJWebViewBuilder_Experimental = class(TJavaGenericImport<JWebViewBuilder_ExperimentalClass, JWebViewBuilder_Experimental>) end;

  JWebViewBuilder_PresetClass = interface(JAnnotationClass)
    ['{63677D7D-2387-404A-AD25-D8AC14FC9A5F}']
  end;

  [JavaSignature('androidx/webkit/WebViewBuilder$Preset')]
  JWebViewBuilder_Preset = interface(JAnnotation)
    ['{24F3B792-33E9-4489-BF97-D4DEC8CBED45}']
  end;
  TJWebViewBuilder_Preset = class(TJavaGenericImport<JWebViewBuilder_PresetClass, JWebViewBuilder_Preset>) end;

  JWebViewBuilderExceptionClass = interface(JRuntimeExceptionClass)
    ['{EC4322AE-CEE7-4948-9770-0ACF0BE619CA}']
  end;

  [JavaSignature('androidx/webkit/WebViewBuilderException')]
  JWebViewBuilderException = interface(JRuntimeException)
    ['{4712E5FE-265E-49C0-B7FC-8B4E054D91CF}']
  end;
  TJWebViewBuilderException = class(TJavaGenericImport<JWebViewBuilderExceptionClass, JWebViewBuilderException>) end;

  JWebViewClientCompatClass = interface(JWebViewClientClass)
    ['{5B55538E-7BBE-4FB0-96D8-00048551652A}']
    {class} function init: JWebViewClientCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewClientCompat')]
  JWebViewClientCompat = interface(JWebViewClient)
    ['{32AB9325-8EA9-4F5B-BD8C-005CCD0FA21B}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    procedure onPageCommitVisible(webView: JWebView; string_: JString); cdecl;
    procedure onReceivedError(webView: JWebView; webResourceRequest: JWebResourceRequest; invocationHandler: JInvocationHandler); cdecl; overload;
    procedure onReceivedError(webView: JWebView; webResourceRequest: JWebResourceRequest; webResourceError: JWebResourceError); cdecl; overload;
    procedure onReceivedError(webView: JWebView; webResourceRequest: JWebResourceRequest; webResourceErrorCompat: JWebResourceErrorCompat); cdecl; overload;
    procedure onReceivedHttpError(webView: JWebView; webResourceRequest: JWebResourceRequest; webResourceResponse: JWebResourceResponse); cdecl;
    procedure onSafeBrowsingHit(webView: JWebView; webResourceRequest: JWebResourceRequest; i: Integer; invocationHandler: JInvocationHandler); cdecl; overload;
    procedure onSafeBrowsingHit(webView: JWebView; webResourceRequest: JWebResourceRequest; i: Integer; safeBrowsingResponse: JSafeBrowsingResponse); cdecl; overload;
    procedure onSafeBrowsingHit(webView: JWebView; webResourceRequest: JWebResourceRequest; i: Integer; safeBrowsingResponseCompat: JSafeBrowsingResponseCompat); cdecl; overload;
    function onWebAuthnIntent(webView: JWebView; pendingIntent: JPendingIntent; invocationHandler: JInvocationHandler): Boolean; cdecl;
    function shouldOverrideUrlLoading(webView: JWebView; webResourceRequest: JWebResourceRequest): Boolean; cdecl;
  end;
  TJWebViewClientCompat = class(TJavaGenericImport<JWebViewClientCompatClass, JWebViewClientCompat>) end;

  JWebViewClientCompat_SafeBrowsingThreatClass = interface(JAnnotationClass)
    ['{AE6A156B-FD06-4B5A-8EDA-46DD3AD883A0}']
  end;

  [JavaSignature('androidx/webkit/WebViewClientCompat$SafeBrowsingThreat')]
  JWebViewClientCompat_SafeBrowsingThreat = interface(JAnnotation)
    ['{0F831784-6981-4B6D-8529-2B183FC98356}']
  end;
  TJWebViewClientCompat_SafeBrowsingThreat = class(TJavaGenericImport<JWebViewClientCompat_SafeBrowsingThreatClass, JWebViewClientCompat_SafeBrowsingThreat>) end;

  JWebViewCompatClass = interface(JObjectClass)
    ['{81F64320-924B-48C1-A609-BBB47677CC05}']
    {class} function addDocumentStartJavaScript(webView: JWebView; string_: JString; set_: JSet): JScriptHandler; cdecl;
    {class} procedure addNavigationListener(webView: JWebView; executor: JExecutor; navigationListener: JNavigationListener); cdecl; overload;
    {class} procedure addNavigationListener(webView: JWebView; navigationListener: JNavigationListener); cdecl; overload;
    {class} procedure addWebMessageListener(webView: JWebView; string_: JString; set_: JSet; webMessageListener: JWebViewCompat_WebMessageListener); cdecl;
    {class} function createWebMessageChannel(webView: JWebView): TJavaObjectArray<JWebMessagePortCompat>; cdecl;
    {class} function getCurrentLoadedWebViewPackage: JPackageInfo; cdecl;
    {class} function getCurrentWebViewPackage(context: JContext): JPackageInfo; cdecl;
    {class} function getProfile(webView: JWebView): JProfile; cdecl;
    {class} function getSafeBrowsingPrivacyPolicyUrl: Jnet_Uri; cdecl;
    {class} function getVariationsHeader: JString; cdecl;
    {class} function getWebChromeClient(webView: JWebView): JWebChromeClient; cdecl;
    {class} function getWebNavigationClient(webView: JWebView): JWebNavigationClient; cdecl;
    {class} function getWebViewClient(webView: JWebView): JWebViewClient; cdecl;
    {class} function getWebViewRenderProcess(webView: JWebView): Jwebkit_WebViewRenderProcess; cdecl;
    {class} function getWebViewRenderProcessClient(webView: JWebView): Jwebkit_WebViewRenderProcessClient; cdecl;
    {class} function isAudioMuted(webView: JWebView): Boolean; cdecl;
    {class} function isMultiProcessEnabled: Boolean; cdecl;
    {class} procedure postVisualStateCallback(webView: JWebView; l: Int64; visualStateCallback: JWebViewCompat_VisualStateCallback); cdecl;
    {class} procedure postWebMessage(webView: JWebView; webMessageCompat: JWebMessageCompat; uri: Jnet_Uri); cdecl;
    {class} procedure prerenderUrlAsync(webView: JWebView; string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      prerenderOperationCallback: JPrerenderOperationCallback); cdecl; overload;
    {class} procedure prerenderUrlAsync(webView: JWebView; string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      speculativeLoadingParameters: JSpeculativeLoadingParameters; prerenderOperationCallback: JPrerenderOperationCallback); cdecl; overload;
    {class} procedure removeNavigationListener(webView: JWebView; navigationListener: JNavigationListener); cdecl;
    {class} procedure removeWebMessageListener(webView: JWebView; string_: JString); cdecl;
    {class} procedure saveState(webView: JWebView; bundle: JBundle; i: Integer; b: Boolean); cdecl;
    {class} procedure setAudioMuted(webView: JWebView; b: Boolean); cdecl;
    {class} procedure setDefaultTrafficStatsTag(i: Integer); cdecl;
    {class} procedure setProfile(webView: JWebView; string_: JString); cdecl;
    {class} procedure setSafeBrowsingAllowlist(set_: JSet; valueCallback: JValueCallback); cdecl;
    {class} procedure setSafeBrowsingWhitelist(list: JList; valueCallback: JValueCallback); cdecl;
    {class} procedure setWebNavigationClient(webView: JWebView; webNavigationClient: JWebNavigationClient); cdecl;
    {class} procedure setWebViewRenderProcessClient(webView: JWebView; executor: JExecutor;
      webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient); cdecl; overload;
    {class} procedure setWebViewRenderProcessClient(webView: JWebView; webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient); cdecl; overload;
    {class} procedure startSafeBrowsing(context: JContext; valueCallback: JValueCallback); cdecl;
    {class} procedure startUpWebView(context: JContext; webViewStartUpConfig: JWebViewStartUpConfig;
      webViewStartUpCallback: JWebViewCompat_WebViewStartUpCallback); cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewCompat')]
  JWebViewCompat = interface(JObject)
    ['{730A0D3A-0F82-415B-B59B-F3A0484B8410}']
  end;
  TJWebViewCompat = class(TJavaGenericImport<JWebViewCompatClass, JWebViewCompat>) end;

  JWebViewCompat_ExperimentalAsyncStartUpClass = interface(JAnnotationClass)
    ['{27FA93AD-AA79-41F9-A686-1DEB74B9A01A}']
  end;

  [JavaSignature('androidx/webkit/WebViewCompat$ExperimentalAsyncStartUp')]
  JWebViewCompat_ExperimentalAsyncStartUp = interface(JAnnotation)
    ['{55C75822-8034-4853-9186-4EF63A83A92D}']
  end;
  TJWebViewCompat_ExperimentalAsyncStartUp = class(TJavaGenericImport<JWebViewCompat_ExperimentalAsyncStartUpClass, JWebViewCompat_ExperimentalAsyncStartUp>) end;

  JWebViewCompat_ExperimentalSaveStateClass = interface(JAnnotationClass)
    ['{9A95EC29-EA5C-4EAC-A73F-8E86151AACA7}']
  end;

  [JavaSignature('androidx/webkit/WebViewCompat$ExperimentalSaveState')]
  JWebViewCompat_ExperimentalSaveState = interface(JAnnotation)
    ['{5613AA63-A0E6-478F-B449-6E7863EF6AF8}']
  end;
  TJWebViewCompat_ExperimentalSaveState = class(TJavaGenericImport<JWebViewCompat_ExperimentalSaveStateClass, JWebViewCompat_ExperimentalSaveState>) end;

  JWebViewCompat_VisualStateCallbackClass = interface(IJavaClass)
    ['{39051CDC-A7F5-4AAE-9B33-03DEF31274A1}']
  end;

  [JavaSignature('androidx/webkit/WebViewCompat$VisualStateCallback')]
  JWebViewCompat_VisualStateCallback = interface(IJavaInstance)
    ['{693E0587-54D1-4B96-8BDF-D25647FC4E31}']
    procedure onComplete(l: Int64); cdecl;
  end;
  TJWebViewCompat_VisualStateCallback = class(TJavaGenericImport<JWebViewCompat_VisualStateCallbackClass, JWebViewCompat_VisualStateCallback>) end;

  JWebViewCompat_WebMessageListenerClass = interface(IJavaClass)
    ['{1B8CB9A9-9E8C-4A0A-935F-96D29B1DC59B}']
  end;

  [JavaSignature('androidx/webkit/WebViewCompat$WebMessageListener')]
  JWebViewCompat_WebMessageListener = interface(IJavaInstance)
    ['{6489C4A1-8968-4C1A-A3AA-10B11AFA8039}']
    procedure onPostMessage(webView: JWebView; webMessageCompat: JWebMessageCompat; uri: Jnet_Uri; b: Boolean; javaScriptReplyProxy: JJavaScriptReplyProxy); cdecl;
  end;
  TJWebViewCompat_WebMessageListener = class(TJavaGenericImport<JWebViewCompat_WebMessageListenerClass, JWebViewCompat_WebMessageListener>) end;

  JWebViewCompat_WebViewStartUpCallbackClass = interface(IJavaClass)
    ['{814318E3-7D7C-4FA5-869B-3CED5C2A140F}']
  end;

  [JavaSignature('androidx/webkit/WebViewCompat$WebViewStartUpCallback')]
  JWebViewCompat_WebViewStartUpCallback = interface(IJavaInstance)
    ['{6557DB74-207A-4A04-ADB0-E48638B2B21F}']
    procedure onSuccess(webViewStartUpResult: JWebViewStartUpResult); cdecl;
  end;
  TJWebViewCompat_WebViewStartUpCallback = class(TJavaGenericImport<JWebViewCompat_WebViewStartUpCallbackClass, JWebViewCompat_WebViewStartUpCallback>) end;

  JWebViewFeatureClass = interface(JObjectClass)
    ['{F62267B9-F0C0-4A14-8741-F07B0D396233}']
    {class} function _GetADD_QUIC_HINTS_V1: JString; cdecl;
    {class} function _GetALGORITHMIC_DARKENING: JString; cdecl;
    {class} function _GetATTRIBUTION_REGISTRATION_BEHAVIOR: JString; cdecl;
    {class} function _GetBACK_FORWARD_CACHE: JString; cdecl;
    {class} function _GetBACK_FORWARD_CACHE_SETTINGS: JString; cdecl;
    {class} function _GetCOOKIE_INTERCEPT: JString; cdecl;
    {class} function _GetCREATE_WEB_MESSAGE_CHANNEL: JString; cdecl;
    {class} function _GetCUSTOM_REQUEST_HEADERS: JString; cdecl;
    {class} function _GetDEFAULT_TRAFFICSTATS_TAGGING: JString; cdecl;
    {class} function _GetDELETE_BROWSING_DATA: JString; cdecl;
    {class} function _GetDISABLED_ACTION_MODE_MENU_ITEMS: JString; cdecl;
    {class} function _GetDOCUMENT_START_SCRIPT: JString; cdecl;
    {class} function _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JString; cdecl;
    {class} function _GetFORCE_DARK: JString; cdecl;
    {class} function _GetFORCE_DARK_STRATEGY: JString; cdecl;
    {class} function _GetGET_COOKIE_INFO: JString; cdecl;
    {class} function _GetGET_VARIATIONS_HEADER: JString; cdecl;
    {class} function _GetGET_WEB_CHROME_CLIENT: JString; cdecl;
    {class} function _GetGET_WEB_VIEW_CLIENT: JString; cdecl;
    {class} function _GetGET_WEB_VIEW_RENDERER: JString; cdecl;
    {class} function _GetHYPERLINK_CONTEXT_MENU_ITEMS: JString; cdecl;
    {class} function _GetMULTI_PROCESS: JString; cdecl;
    {class} function _GetMULTI_PROFILE: JString; cdecl;
    {class} function _GetMUTE_AUDIO: JString; cdecl;
    {class} function _GetNAVIGATION_CALLBACK_BASIC: JString; cdecl;
    {class} function _GetNAVIGATION_LISTENER_V1: JString; cdecl;
    {class} function _GetOFF_SCREEN_PRERASTER: JString; cdecl;
    {class} function _GetORIGIN_MATCHED_HEADERS: JString; cdecl;
    {class} function _GetPAYMENT_REQUEST: JString; cdecl;
    {class} function _GetPOST_WEB_MESSAGE: JString; cdecl;
    {class} function _GetPRECONNECT: JString; cdecl;
    {class} function _GetPRERENDER_WITH_URL: JString; cdecl;
    {class} function _GetPROFILE_URL_PREFETCH: JString; cdecl;
    {class} function _GetPROVIDER_WEAKLY_REF_WEBVIEW: JString; cdecl;
    {class} function _GetPROXY_OVERRIDE: JString; cdecl;
    {class} function _GetPROXY_OVERRIDE_REVERSE_BYPASS: JString; cdecl;
    {class} function _GetRECEIVE_HTTP_ERROR: JString; cdecl;
    {class} function _GetRECEIVE_WEB_RESOURCE_ERROR: JString; cdecl;
    {class} function _GetREQUESTED_WITH_HEADER_ALLOW_LIST: JString; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST: JString; cdecl;
    {class} function _GetSAFE_BROWSING_ENABLE: JString; cdecl;
    {class} function _GetSAFE_BROWSING_HIT: JString; cdecl;
    {class} function _GetSAFE_BROWSING_PRIVACY_POLICY_URL: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_PROCEED: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JString; cdecl;
    {class} function _GetSAFE_BROWSING_WHITELIST: JString; cdecl;
    {class} function _GetSAVE_STATE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_BASIC_USAGE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_CACHE_MODE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_CONTENT_ACCESS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_FILE_ACCESS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JString; cdecl;
    {class} function _GetSHOULD_OVERRIDE_WITH_REDIRECTS: JString; cdecl;
    {class} function _GetSPECULATIVE_LOADING: JString; cdecl;
    {class} function _GetSPECULATIVE_LOADING_CONFIG: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATHS: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JString; cdecl;
    {class} function _GetSTART_SAFE_BROWSING: JString; cdecl;
    {class} function _GetTRACING_CONTROLLER_BASIC_USAGE: JString; cdecl;
    {class} function _GetUSER_AGENT_METADATA: JString; cdecl;
    {class} function _GetUSER_AGENT_METADATA_FORM_FACTORS: JString; cdecl;
    {class} function _GetVISUAL_STATE_CALLBACK: JString; cdecl;
    {class} function _GetWARM_UP_RENDERER_PROCESS: JString; cdecl;
    {class} function _GetWEBVIEW_BUILDER_EXPERIMENTAL_V1: JString; cdecl;
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS: JString; cdecl;
    {class} function _GetWEB_AUTHENTICATION: JString; cdecl;
    {class} function _GetWEB_MESSAGE_ARRAY_BUFFER: JString; cdecl;
    {class} function _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_LISTENER: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_CLOSE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_POST_MESSAGE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JString; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_CODE: JString; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION: JString; cdecl;
    {class} function _GetWEB_RESOURCE_REQUEST_IS_REDIRECT: JString; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JString; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_TERMINATE: JString; cdecl;
    {class} function isFeatureSupported(string_: JString): Boolean; cdecl;
    {class} function isStartupFeatureSupported(context: JContext; string_: JString): Boolean; cdecl;
    {class} property ADD_QUIC_HINTS_V1: JString read _GetADD_QUIC_HINTS_V1;
    {class} property ALGORITHMIC_DARKENING: JString read _GetALGORITHMIC_DARKENING;
    {class} property ATTRIBUTION_REGISTRATION_BEHAVIOR: JString read _GetATTRIBUTION_REGISTRATION_BEHAVIOR;
    {class} property BACK_FORWARD_CACHE: JString read _GetBACK_FORWARD_CACHE;
    {class} property BACK_FORWARD_CACHE_SETTINGS: JString read _GetBACK_FORWARD_CACHE_SETTINGS;
    {class} property COOKIE_INTERCEPT: JString read _GetCOOKIE_INTERCEPT;
    {class} property CREATE_WEB_MESSAGE_CHANNEL: JString read _GetCREATE_WEB_MESSAGE_CHANNEL;
    {class} property CUSTOM_REQUEST_HEADERS: JString read _GetCUSTOM_REQUEST_HEADERS;
    {class} property DEFAULT_TRAFFICSTATS_TAGGING: JString read _GetDEFAULT_TRAFFICSTATS_TAGGING;
    {class} property DELETE_BROWSING_DATA: JString read _GetDELETE_BROWSING_DATA;
    {class} property DISABLED_ACTION_MODE_MENU_ITEMS: JString read _GetDISABLED_ACTION_MODE_MENU_ITEMS;
    {class} property DOCUMENT_START_SCRIPT: JString read _GetDOCUMENT_START_SCRIPT;
    {class} property ENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JString read _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY;
    {class} property FORCE_DARK: JString read _GetFORCE_DARK;
    {class} property FORCE_DARK_STRATEGY: JString read _GetFORCE_DARK_STRATEGY;
    {class} property GET_COOKIE_INFO: JString read _GetGET_COOKIE_INFO;
    {class} property GET_VARIATIONS_HEADER: JString read _GetGET_VARIATIONS_HEADER;
    {class} property GET_WEB_CHROME_CLIENT: JString read _GetGET_WEB_CHROME_CLIENT;
    {class} property GET_WEB_VIEW_CLIENT: JString read _GetGET_WEB_VIEW_CLIENT;
    {class} property GET_WEB_VIEW_RENDERER: JString read _GetGET_WEB_VIEW_RENDERER;
    {class} property HYPERLINK_CONTEXT_MENU_ITEMS: JString read _GetHYPERLINK_CONTEXT_MENU_ITEMS;
    {class} property MULTI_PROCESS: JString read _GetMULTI_PROCESS;
    {class} property MULTI_PROFILE: JString read _GetMULTI_PROFILE;
    {class} property MUTE_AUDIO: JString read _GetMUTE_AUDIO;
    {class} property NAVIGATION_CALLBACK_BASIC: JString read _GetNAVIGATION_CALLBACK_BASIC;
    {class} property NAVIGATION_LISTENER_V1: JString read _GetNAVIGATION_LISTENER_V1;
    {class} property OFF_SCREEN_PRERASTER: JString read _GetOFF_SCREEN_PRERASTER;
    {class} property ORIGIN_MATCHED_HEADERS: JString read _GetORIGIN_MATCHED_HEADERS;
    {class} property PAYMENT_REQUEST: JString read _GetPAYMENT_REQUEST;
    {class} property POST_WEB_MESSAGE: JString read _GetPOST_WEB_MESSAGE;
    {class} property PRECONNECT: JString read _GetPRECONNECT;
    {class} property PRERENDER_WITH_URL: JString read _GetPRERENDER_WITH_URL;
    {class} property PROFILE_URL_PREFETCH: JString read _GetPROFILE_URL_PREFETCH;
    {class} property PROVIDER_WEAKLY_REF_WEBVIEW: JString read _GetPROVIDER_WEAKLY_REF_WEBVIEW;
    {class} property PROXY_OVERRIDE: JString read _GetPROXY_OVERRIDE;
    {class} property PROXY_OVERRIDE_REVERSE_BYPASS: JString read _GetPROXY_OVERRIDE_REVERSE_BYPASS;
    {class} property RECEIVE_HTTP_ERROR: JString read _GetRECEIVE_HTTP_ERROR;
    {class} property RECEIVE_WEB_RESOURCE_ERROR: JString read _GetRECEIVE_WEB_RESOURCE_ERROR;
    {class} property REQUESTED_WITH_HEADER_ALLOW_LIST: JString read _GetREQUESTED_WITH_HEADER_ALLOW_LIST;
    {class} property SAFE_BROWSING_ALLOWLIST: JString read _GetSAFE_BROWSING_ALLOWLIST;
    {class} property SAFE_BROWSING_ENABLE: JString read _GetSAFE_BROWSING_ENABLE;
    {class} property SAFE_BROWSING_HIT: JString read _GetSAFE_BROWSING_HIT;
    {class} property SAFE_BROWSING_PRIVACY_POLICY_URL: JString read _GetSAFE_BROWSING_PRIVACY_POLICY_URL;
    {class} property SAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JString read _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY;
    {class} property SAFE_BROWSING_RESPONSE_PROCEED: JString read _GetSAFE_BROWSING_RESPONSE_PROCEED;
    {class} property SAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JString read _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL;
    {class} property SAFE_BROWSING_WHITELIST: JString read _GetSAFE_BROWSING_WHITELIST;
    {class} property SAVE_STATE: JString read _GetSAVE_STATE;
    {class} property SERVICE_WORKER_BASIC_USAGE: JString read _GetSERVICE_WORKER_BASIC_USAGE;
    {class} property SERVICE_WORKER_BLOCK_NETWORK_LOADS: JString read _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS;
    {class} property SERVICE_WORKER_CACHE_MODE: JString read _GetSERVICE_WORKER_CACHE_MODE;
    {class} property SERVICE_WORKER_CONTENT_ACCESS: JString read _GetSERVICE_WORKER_CONTENT_ACCESS;
    {class} property SERVICE_WORKER_FILE_ACCESS: JString read _GetSERVICE_WORKER_FILE_ACCESS;
    {class} property SERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JString read _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST;
    {class} property SHOULD_OVERRIDE_WITH_REDIRECTS: JString read _GetSHOULD_OVERRIDE_WITH_REDIRECTS;
    {class} property SPECULATIVE_LOADING: JString read _GetSPECULATIVE_LOADING;
    {class} property SPECULATIVE_LOADING_CONFIG: JString read _GetSPECULATIVE_LOADING_CONFIG;
    {class} property STARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JString read _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES;
    {class} property STARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JString read _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX;
    {class} property STARTUP_FEATURE_SET_DIRECTORY_BASE_PATHS: JString read _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATHS;
    {class} property STARTUP_FEATURE_SET_PROFILES_TO_LOAD: JString read _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JString read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JString read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2;
    {class} property START_SAFE_BROWSING: JString read _GetSTART_SAFE_BROWSING;
    {class} property TRACING_CONTROLLER_BASIC_USAGE: JString read _GetTRACING_CONTROLLER_BASIC_USAGE;
    {class} property USER_AGENT_METADATA: JString read _GetUSER_AGENT_METADATA;
    {class} property USER_AGENT_METADATA_FORM_FACTORS: JString read _GetUSER_AGENT_METADATA_FORM_FACTORS;
    {class} property VISUAL_STATE_CALLBACK: JString read _GetVISUAL_STATE_CALLBACK;
    {class} property WARM_UP_RENDERER_PROCESS: JString read _GetWARM_UP_RENDERER_PROCESS;
    {class} property WEBVIEW_BUILDER_EXPERIMENTAL_V1: JString read _GetWEBVIEW_BUILDER_EXPERIMENTAL_V1;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_STATUS: JString read _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS;
    {class} property WEB_AUTHENTICATION: JString read _GetWEB_AUTHENTICATION;
    {class} property WEB_MESSAGE_ARRAY_BUFFER: JString read _GetWEB_MESSAGE_ARRAY_BUFFER;
    {class} property WEB_MESSAGE_CALLBACK_ON_MESSAGE: JString read _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE;
    {class} property WEB_MESSAGE_LISTENER: JString read _GetWEB_MESSAGE_LISTENER;
    {class} property WEB_MESSAGE_PORT_CLOSE: JString read _GetWEB_MESSAGE_PORT_CLOSE;
    {class} property WEB_MESSAGE_PORT_POST_MESSAGE: JString read _GetWEB_MESSAGE_PORT_POST_MESSAGE;
    {class} property WEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JString read _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK;
    {class} property WEB_RESOURCE_ERROR_GET_CODE: JString read _GetWEB_RESOURCE_ERROR_GET_CODE;
    {class} property WEB_RESOURCE_ERROR_GET_DESCRIPTION: JString read _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION;
    {class} property WEB_RESOURCE_REQUEST_IS_REDIRECT: JString read _GetWEB_RESOURCE_REQUEST_IS_REDIRECT;
    {class} property WEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JString read _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE;
    {class} property WEB_VIEW_RENDERER_TERMINATE: JString read _GetWEB_VIEW_RENDERER_TERMINATE;
  end;

  [JavaSignature('androidx/webkit/WebViewFeature')]
  JWebViewFeature = interface(JObject)
    ['{A4C0F7C5-661E-479E-981C-B6991F7E5238}']
  end;
  TJWebViewFeature = class(TJavaGenericImport<JWebViewFeatureClass, JWebViewFeature>) end;

  JWebViewFeature_WebViewStartupFeatureClass = interface(JAnnotationClass)
    ['{4D737405-B9F9-4BE1-8BF5-29F9F10AD13E}']
  end;

  [JavaSignature('androidx/webkit/WebViewFeature$WebViewStartupFeature')]
  JWebViewFeature_WebViewStartupFeature = interface(JAnnotation)
    ['{12247501-4476-4B9D-8AD7-EE3185779C48}']
  end;
  TJWebViewFeature_WebViewStartupFeature = class(TJavaGenericImport<JWebViewFeature_WebViewStartupFeatureClass, JWebViewFeature_WebViewStartupFeature>) end;

  JWebViewFeature_WebViewSupportFeatureClass = interface(JAnnotationClass)
    ['{056BF53F-56F6-4B8F-AFE6-4398F3275ECB}']
  end;

  [JavaSignature('androidx/webkit/WebViewFeature$WebViewSupportFeature')]
  JWebViewFeature_WebViewSupportFeature = interface(JAnnotation)
    ['{539D64A7-8E14-454F-B3F8-7CBAE5C12B0A}']
  end;
  TJWebViewFeature_WebViewSupportFeature = class(TJavaGenericImport<JWebViewFeature_WebViewSupportFeatureClass, JWebViewFeature_WebViewSupportFeature>) end;

  JWebViewMediaIntegrityApiStatusConfigClass = interface(JObjectClass)
    ['{5AC14633-B4D2-4FB6-B629-EE9FC11D59C3}']
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_DISABLED: Integer; cdecl;
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_ENABLED: Integer; cdecl;
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_ENABLED_WITHOUT_APP_IDENTITY: Integer; cdecl;
    {class} function init(builder: JWebViewMediaIntegrityApiStatusConfig_Builder): JWebViewMediaIntegrityApiStatusConfig; cdecl;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_DISABLED: Integer read _GetWEBVIEW_MEDIA_INTEGRITY_API_DISABLED;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_ENABLED: Integer read _GetWEBVIEW_MEDIA_INTEGRITY_API_ENABLED;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_ENABLED_WITHOUT_APP_IDENTITY: Integer read _GetWEBVIEW_MEDIA_INTEGRITY_API_ENABLED_WITHOUT_APP_IDENTITY;
  end;

  [JavaSignature('androidx/webkit/WebViewMediaIntegrityApiStatusConfig')]
  JWebViewMediaIntegrityApiStatusConfig = interface(JObject)
    ['{7D3F27A5-45CD-4899-954E-4A08801DF710}']
    function getDefaultStatus: Integer; cdecl;
    function getOverrideRules: JMap; cdecl;
  end;
  TJWebViewMediaIntegrityApiStatusConfig = class(TJavaGenericImport<JWebViewMediaIntegrityApiStatusConfigClass, JWebViewMediaIntegrityApiStatusConfig>) end;

  JWebViewMediaIntegrityApiStatusConfig_BuilderClass = interface(JObjectClass)
    ['{24C0473D-AB29-4DA5-89F4-2E81B6E8F0A2}']
    {class} function init(i: Integer): JWebViewMediaIntegrityApiStatusConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewMediaIntegrityApiStatusConfig$Builder')]
  JWebViewMediaIntegrityApiStatusConfig_Builder = interface(JObject)
    ['{C844F084-B6A7-48B4-89D1-92A1DEC0F935}']
    function addOverrideRule(string_: JString; i: Integer): JWebViewMediaIntegrityApiStatusConfig_Builder; cdecl;
    function build: JWebViewMediaIntegrityApiStatusConfig; cdecl;
    function setOverrideRules(map: JMap): JWebViewMediaIntegrityApiStatusConfig_Builder; cdecl;
  end;
  TJWebViewMediaIntegrityApiStatusConfig_Builder = class(TJavaGenericImport<JWebViewMediaIntegrityApiStatusConfig_BuilderClass, JWebViewMediaIntegrityApiStatusConfig_Builder>) end;

  Jwebkit_WebViewRenderProcessClass = interface(JObjectClass)
    ['{E2220FD5-ECF2-4DE8-A2AB-732184E0E2AE}']
    {class} function init: Jwebkit_WebViewRenderProcess; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewRenderProcess')]
  Jwebkit_WebViewRenderProcess = interface(JObject)
    ['{695BC5B1-EFFF-48A9-8E1D-5ED21E9D645C}']
    function terminate: Boolean; cdecl;
  end;
  TJwebkit_WebViewRenderProcess = class(TJavaGenericImport<Jwebkit_WebViewRenderProcessClass, Jwebkit_WebViewRenderProcess>) end;

  Jwebkit_WebViewRenderProcessClientClass = interface(JObjectClass)
    ['{FB8B4DEA-611D-4C38-B145-3A182C1FD529}']
    {class} function init: Jwebkit_WebViewRenderProcessClient; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewRenderProcessClient')]
  Jwebkit_WebViewRenderProcessClient = interface(JObject)
    ['{CE9CBC53-43F1-4961-977D-2C38F20736BD}']
    procedure onRenderProcessResponsive(webView: JWebView; webViewRenderProcess: Jwebkit_WebViewRenderProcess); cdecl;
    procedure onRenderProcessUnresponsive(webView: JWebView; webViewRenderProcess: Jwebkit_WebViewRenderProcess); cdecl;
  end;
  TJwebkit_WebViewRenderProcessClient = class(TJavaGenericImport<Jwebkit_WebViewRenderProcessClientClass, Jwebkit_WebViewRenderProcessClient>) end;

  JWebViewStartUpConfigClass = interface(JObjectClass)
    ['{F5143AB8-8D28-48ED-82A2-32A2C7D7D852}']
  end;

  [JavaSignature('androidx/webkit/WebViewStartUpConfig')]
  JWebViewStartUpConfig = interface(JObject)
    ['{600BC554-DE03-4BED-A1A4-923FAB0F6A36}']
    function getBackgroundExecutor: JExecutor; cdecl;
    function getProfilesToLoadDuringStartup: JSet; cdecl;
    function shouldRunUiThreadStartUpTasks: Boolean; cdecl;
  end;
  TJWebViewStartUpConfig = class(TJavaGenericImport<JWebViewStartUpConfigClass, JWebViewStartUpConfig>) end;

  JWebViewStartUpConfig_BuilderClass = interface(JObjectClass)
    ['{347FF772-8D29-40D9-A968-C8ED5E996CC1}']
    {class} function init(executor: JExecutor): JWebViewStartUpConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/webkit/WebViewStartUpConfig$Builder')]
  JWebViewStartUpConfig_Builder = interface(JObject)
    ['{B27CCA09-A69E-42E0-832F-0713E07339E8}']
    function build: JWebViewStartUpConfig; cdecl;
    function setProfilesToLoadDuringStartup(set_: JSet): JWebViewStartUpConfig_Builder; cdecl;
    function setShouldRunUiThreadStartUpTasks(b: Boolean): JWebViewStartUpConfig_Builder; cdecl;
  end;
  TJWebViewStartUpConfig_Builder = class(TJavaGenericImport<JWebViewStartUpConfig_BuilderClass, JWebViewStartUpConfig_Builder>) end;

  JWebViewStartUpResultClass = interface(IJavaClass)
    ['{56D47686-1E9B-4E43-BCFA-16928A6FCBFD}']
  end;

  [JavaSignature('androidx/webkit/WebViewStartUpResult')]
  JWebViewStartUpResult = interface(IJavaInstance)
    ['{529EB5AF-E826-4CB4-93BC-C1B4C4E32C14}']
    function getMaxTimePerTaskInUiThreadMillis: JLong; cdecl;
    function getNonUiThreadBlockingStartUpLocations: JList; cdecl;
    function getTotalTimeInUiThreadMillis: JLong; cdecl;
    function getUiThreadBlockingStartUpLocations: JList; cdecl;
  end;
  TJWebViewStartUpResult = class(TJavaGenericImport<JWebViewStartUpResultClass, JWebViewStartUpResult>) end;

  JApiFeatureClass = interface(JObjectClass)
    ['{7D46C6CB-BF94-4EF9-AD90-E14273A64632}']
    {class} function getWebViewApkFeaturesForTesting: JSet; cdecl;
    {class} function values: JSet; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature')]
  JApiFeature = interface(JObject)
    ['{7D6E9F9C-AAEB-458D-BA43-1293FA444338}']
    function getPublicFeatureName: JString; cdecl;
    function isSupported: Boolean; cdecl;
    function isSupportedByFramework: Boolean; cdecl;
    function isSupportedByWebView: Boolean; cdecl;
  end;
  TJApiFeature = class(TJavaGenericImport<JApiFeatureClass, JApiFeature>) end;

  JApiFeature_MClass = interface(JApiFeatureClass)
    ['{89EEB6D5-8D58-4392-BDFC-D4A3EFB78ED1}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$M')]
  JApiFeature_M = interface(JApiFeature)
    ['{C1362D38-CB96-47B9-87BA-765E669EAC83}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_M = class(TJavaGenericImport<JApiFeature_MClass, JApiFeature_M>) end;

  JApiFeature_NClass = interface(JApiFeatureClass)
    ['{26347084-1BB7-4A8C-8C40-0F6A255959C0}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$N')]
  JApiFeature_N = interface(JApiFeature)
    ['{237E2473-29B6-4794-A1F8-3ACAD5E8DE6A}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_N = class(TJavaGenericImport<JApiFeature_NClass, JApiFeature_N>) end;

  JApiFeature_NoFrameworkClass = interface(JApiFeatureClass)
    ['{CE69B4F7-62A4-4AF8-97A4-E4D471B5CB86}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$NoFramework')]
  JApiFeature_NoFramework = interface(JApiFeature)
    ['{9A3F876C-ADC8-4E11-9221-483A843DC0DF}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_NoFramework = class(TJavaGenericImport<JApiFeature_NoFrameworkClass, JApiFeature_NoFramework>) end;

  JApiFeature_OClass = interface(JApiFeatureClass)
    ['{BB2E6411-65DA-4029-8D71-31605F2FE69B}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$O')]
  JApiFeature_O = interface(JApiFeature)
    ['{D04B4574-0715-43EE-A5F2-2D5B9AF748A3}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_O = class(TJavaGenericImport<JApiFeature_OClass, JApiFeature_O>) end;

  JApiFeature_O_MR1Class = interface(JApiFeatureClass)
    ['{C3AD242F-2C96-43A1-9AC2-8CA8A4A4B612}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$O_MR1')]
  JApiFeature_O_MR1 = interface(JApiFeature)
    ['{1BC05F3C-B4DE-4C4F-AA50-90BD9658826A}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_O_MR1 = class(TJavaGenericImport<JApiFeature_O_MR1Class, JApiFeature_O_MR1>) end;

  JApiFeature_PClass = interface(JApiFeatureClass)
    ['{1C818B28-72AF-4222-A6F4-87A6765A0C59}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$P')]
  JApiFeature_P = interface(JApiFeature)
    ['{4D5BC7D4-2305-48FB-A8DE-62943F1D5220}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_P = class(TJavaGenericImport<JApiFeature_PClass, JApiFeature_P>) end;

  JApiFeature_QClass = interface(JApiFeatureClass)
    ['{FB390C7E-695F-41EC-AFCD-A69F17FE324D}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$Q')]
  JApiFeature_Q = interface(JApiFeature)
    ['{64D67CCA-E854-4367-B5A6-F6B59BEF07A5}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_Q = class(TJavaGenericImport<JApiFeature_QClass, JApiFeature_Q>) end;

  JApiFeature_TClass = interface(JApiFeatureClass)
    ['{366C1633-86C5-4AC5-B21C-EB44B04824DB}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiFeature$T')]
  JApiFeature_T = interface(JApiFeature)
    ['{354DF246-AE45-421C-858D-A282B13E6757}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJApiFeature_T = class(TJavaGenericImport<JApiFeature_TClass, JApiFeature_T>) end;

  JApiHelperForNClass = interface(JObjectClass)
    ['{F426DAF5-B1F6-4951-B03B-443EED111A44}']
    {class} function getAllowContentAccess(serviceWorkerWebSettings: JServiceWorkerWebSettings): Boolean; cdecl;
    {class} function getAllowFileAccess(serviceWorkerWebSettings: JServiceWorkerWebSettings): Boolean; cdecl;
    {class} function getBlockNetworkLoads(serviceWorkerWebSettings: JServiceWorkerWebSettings): Boolean; cdecl;
    {class} function getCacheMode(serviceWorkerWebSettings: JServiceWorkerWebSettings): Integer; cdecl;
    {class} function getDataDir(context: JContext): JFile; cdecl;
    {class} function getDisabledActionModeMenuItems(webSettings: JWebSettings): Integer; cdecl;
    {class} function getServiceWorkerControllerInstance: JServiceWorkerController; cdecl;
    {class} function getServiceWorkerWebSettings(serviceWorkerController: JServiceWorkerController): JServiceWorkerWebSettings; cdecl;
    {class} function getServiceWorkerWebSettingsImpl(serviceWorkerController: JServiceWorkerController): JServiceWorkerWebSettingsImpl; cdecl;
    {class} function isRedirect(webResourceRequest: JWebResourceRequest): Boolean; cdecl;
    {class} procedure setAllowContentAccess(serviceWorkerWebSettings: JServiceWorkerWebSettings; b: Boolean); cdecl;
    {class} procedure setAllowFileAccess(serviceWorkerWebSettings: JServiceWorkerWebSettings; b: Boolean); cdecl;
    {class} procedure setBlockNetworkLoads(serviceWorkerWebSettings: JServiceWorkerWebSettings; b: Boolean); cdecl;
    {class} procedure setCacheMode(serviceWorkerWebSettings: JServiceWorkerWebSettings; i: Integer); cdecl;
    {class} procedure setDisabledActionModeMenuItems(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setServiceWorkerClient(serviceWorkerController: JServiceWorkerController; serviceWorkerClient: JServiceWorkerClient); cdecl;
    {class} procedure setServiceWorkerClientCompat(serviceWorkerController: JServiceWorkerController;
      serviceWorkerClientCompat: JServiceWorkerClientCompat); cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForN')]
  JApiHelperForN = interface(JObject)
    ['{94967F1F-334A-4114-B390-3C807FAF6022}']
  end;
  TJApiHelperForN = class(TJavaGenericImport<JApiHelperForNClass, JApiHelperForN>) end;

  JApiHelperForOClass = interface(JObjectClass)
    ['{09E7A8B5-FBFC-4276-A8F9-D78BE3D19BED}']
    {class} function getCurrentWebViewPackage: JPackageInfo; cdecl;
    {class} function getSafeBrowsingEnabled(webSettings: JWebSettings): Boolean; cdecl;
    {class} function getWebChromeClient(webView: JWebView): JWebChromeClient; cdecl;
    {class} function getWebViewClient(webView: JWebView): JWebViewClient; cdecl;
    {class} procedure setSafeBrowsingEnabled(webSettings: JWebSettings; b: Boolean); cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForO')]
  JApiHelperForO = interface(JObject)
    ['{B480EC52-02C2-4BF5-AD13-840565554E89}']
  end;
  TJApiHelperForO = class(TJavaGenericImport<JApiHelperForOClass, JApiHelperForO>) end;

  JApiHelperForOMR1Class = interface(JObjectClass)
    ['{95ABF772-212A-4651-9813-5268B8CCC91F}']
    {class} procedure backToSafety(safeBrowsingResponse: JSafeBrowsingResponse; b: Boolean); cdecl;
    {class} function getSafeBrowsingPrivacyPolicyUrl: Jnet_Uri; cdecl;
    {class} procedure proceed(safeBrowsingResponse: JSafeBrowsingResponse; b: Boolean); cdecl;
    {class} procedure setSafeBrowsingWhitelist(list: JList; valueCallback: JValueCallback); cdecl;
    {class} procedure showInterstitial(safeBrowsingResponse: JSafeBrowsingResponse; b: Boolean); cdecl;
    {class} procedure startSafeBrowsing(context: JContext; valueCallback: JValueCallback); cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForOMR1')]
  JApiHelperForOMR1 = interface(JObject)
    ['{C9D1F926-B43C-4A41-8660-5CE1CC18C1C7}']
  end;
  TJApiHelperForOMR1 = class(TJavaGenericImport<JApiHelperForOMR1Class, JApiHelperForOMR1>) end;

  JApiHelperForPClass = interface(JObjectClass)
    ['{96129F9E-A5FE-479E-A239-C523DE704296}']
    {class} function getTracingControllerInstance: JTracingController; cdecl;
    {class} function getWebViewClassLoader: JClassLoader; cdecl;
    {class} function getWebViewLooper(webView: JWebView): JLooper; cdecl;
    {class} function isTracing(tracingController: JTracingController): Boolean; cdecl;
    {class} procedure setDataDirectorySuffix(string_: JString); cdecl;
    {class} procedure start(tracingController: JTracingController; tracingConfig: Jwebkit_TracingConfig); cdecl;
    {class} function stop(tracingController: JTracingController; outputStream: JOutputStream; executor: JExecutor): Boolean; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForP')]
  JApiHelperForP = interface(JObject)
    ['{3189AAE7-49CD-4D84-93CB-017009117171}']
  end;
  TJApiHelperForP = class(TJavaGenericImport<JApiHelperForPClass, JApiHelperForP>) end;

  JApiHelperForQClass = interface(JObjectClass)
    ['{6AD5C35E-FB87-4FAB-A426-3F8BAB506AFC}']
    {class} function getForceDark(webSettings: JWebSettings): Integer; cdecl;
    {class} function getWebViewRenderProcess(webView: JWebView): JWebViewRenderProcess; cdecl;
    {class} function getWebViewRenderProcessClient(webView: JWebView): JWebViewRenderProcessClient; cdecl;
    {class} procedure setForceDark(webSettings: JWebSettings; i: Integer); cdecl;
    {class} procedure setWebViewRenderProcessClient(webView: JWebView; executor: JExecutor;
      webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient); cdecl; overload;
    {class} procedure setWebViewRenderProcessClient(webView: JWebView; webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient); cdecl; overload;
    {class} function terminate(webViewRenderProcess: JWebViewRenderProcess): Boolean; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForQ')]
  JApiHelperForQ = interface(JObject)
    ['{CC6A4A17-5835-442C-A8A6-A2121E9E419B}']
  end;
  TJApiHelperForQ = class(TJavaGenericImport<JApiHelperForQClass, JApiHelperForQ>) end;

  JApiHelperForTiramisuClass = interface(JObjectClass)
    ['{D92B3BAC-C422-4D1E-8B74-AE49638EA229}']
  end;

  [JavaSignature('androidx/webkit/internal/ApiHelperForTiramisu')]
  JApiHelperForTiramisu = interface(JObject)
    ['{F8009576-145D-4283-AC85-CAB0DE37C982}']
  end;
  TJApiHelperForTiramisu = class(TJavaGenericImport<JApiHelperForTiramisuClass, JApiHelperForTiramisu>) end;

  JAssetHelperClass = interface(JObjectClass)
    ['{B1652263-84F1-499B-8FD2-0BEF839FF78B}']
    {class} function _GetDEFAULT_MIME_TYPE: JString; cdecl;
    {class} function init(context: JContext): JAssetHelper; cdecl;
    {class} function getCanonicalDirPath(file_: JFile): JString; cdecl;
    {class} function getCanonicalFileIfChild(file_: JFile; string_: JString): JFile; cdecl;
    {class} function getDataDir(context: JContext): JFile; cdecl;
    {class} function guessMimeType(string_: JString): JString; cdecl;
    {class} function openFile(file_: JFile): JInputStream; cdecl;
    {class} property DEFAULT_MIME_TYPE: JString read _GetDEFAULT_MIME_TYPE;
  end;

  [JavaSignature('androidx/webkit/internal/AssetHelper')]
  JAssetHelper = interface(JObject)
    ['{BC725698-90F8-4A07-86F8-C0DF6E1CD3A9}']
    function openAsset(string_: JString): JInputStream; cdecl;
    function openResource(string_: JString): JInputStream; cdecl;
  end;
  TJAssetHelper = class(TJavaGenericImport<JAssetHelperClass, JAssetHelper>) end;

  JBackForwardCacheSettingsImplClass = interface(JObjectClass)
    ['{6966EFFF-38FA-4BC5-9AC9-67F9BC0C3EF9}']
    {class} function init(backForwardCacheSettings: JBackForwardCacheSettings): JBackForwardCacheSettingsImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/BackForwardCacheSettingsImpl')]
  JBackForwardCacheSettingsImpl = interface(JObject)
    ['{74FDBFB4-CA6E-48EB-AC55-C0B157090DD0}']
    function getMaxPagesInCache: Integer; cdecl;
    function getOrCreatePeer(callable: JCallable): JObject; cdecl;
    function getTimeoutInSeconds: Integer; cdecl;
  end;
  TJBackForwardCacheSettingsImpl = class(TJavaGenericImport<JBackForwardCacheSettingsImplClass, JBackForwardCacheSettingsImpl>) end;

  JConditionallySupportedFeatureClass = interface(IJavaClass)
    ['{AD4370CE-9D7E-40CF-ABAE-D4EAFCE47139}']
  end;

  [JavaSignature('androidx/webkit/internal/ConditionallySupportedFeature')]
  JConditionallySupportedFeature = interface(IJavaInstance)
    ['{A2C66506-764C-4241-BFB3-AB61A831869B}']
    function getPublicFeatureName: JString; cdecl;
    function isSupported: Boolean; cdecl;
  end;
  TJConditionallySupportedFeature = class(TJavaGenericImport<JConditionallySupportedFeatureClass, JConditionallySupportedFeature>) end;

  JCookieManagerAdapterClass = interface(JObjectClass)
    ['{FEE01B19-6372-4C8A-B16D-460F46F53E1F}']
    {class} function init(webViewCookieManagerBoundaryInterface: JWebViewCookieManagerBoundaryInterface): JCookieManagerAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/CookieManagerAdapter')]
  JCookieManagerAdapter = interface(JObject)
    ['{62623BE4-2EBB-4BCA-93A4-37C35E3DBA00}']
    function getCookieInfo(string_: JString): JList; cdecl;
  end;
  TJCookieManagerAdapter = class(TJavaGenericImport<JCookieManagerAdapterClass, JCookieManagerAdapter>) end;

  JFrameworkServiceWorkerClientClass = interface(JServiceWorkerClientClass)
    ['{3ACAA5DE-0CEE-40AF-9078-609C48683A02}']
    {class} function init(serviceWorkerClientCompat: JServiceWorkerClientCompat): JFrameworkServiceWorkerClient; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/FrameworkServiceWorkerClient')]
  JFrameworkServiceWorkerClient = interface(JServiceWorkerClient)
    ['{86F62B5E-1662-4E8E-9D4E-CAC7D3E766A2}']
    function shouldInterceptRequest(webResourceRequest: JWebResourceRequest): JWebResourceResponse; cdecl;
  end;
  TJFrameworkServiceWorkerClient = class(TJavaGenericImport<JFrameworkServiceWorkerClientClass, JFrameworkServiceWorkerClient>) end;

  JIncompatibleApkWebViewProviderFactoryClass = interface(JObjectClass)
    ['{B66F989E-AB44-42AB-B9F9-F7990C5AE478}']
    {class} function init: JIncompatibleApkWebViewProviderFactory; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/IncompatibleApkWebViewProviderFactory')]
  JIncompatibleApkWebViewProviderFactory = interface(JObject)
    ['{5EE3457D-399D-46E7-AC24-BF26A193766F}']
    function createWebView(webView: JWebView): JWebViewProviderBoundaryInterface; cdecl;
    function getDropDataProvider: JDropDataContentProviderBoundaryInterface; cdecl;
    function getProfileStore: JProfileStoreBoundaryInterface; cdecl;
    function getProxyController: JProxyControllerBoundaryInterface; cdecl;
    function getServiceWorkerController: JServiceWorkerControllerBoundaryInterface; cdecl;
    function getStatics: JStaticsBoundaryInterface; cdecl;
    function getTracingController: JTracingControllerBoundaryInterface; cdecl;
    function getWebViewBuilder: JWebViewBuilderBoundaryInterface; cdecl;
    function getWebViewFeatures: TJavaObjectArray<JString>; cdecl;
    function getWebkitToCompatConverter: JWebkitToCompatConverterBoundaryInterface; cdecl;
    procedure startUpWebView(webViewStartUpConfig: JWebViewStartUpConfig; webViewStartUpCallback: JWebViewCompat_WebViewStartUpCallback); cdecl;
  end;
  TJIncompatibleApkWebViewProviderFactory = class(TJavaGenericImport<JIncompatibleApkWebViewProviderFactoryClass, JIncompatibleApkWebViewProviderFactory>) end;

  JJavaScriptReplyProxyImplClass = interface(JJavaScriptReplyProxyClass)
    ['{FB7DF079-2D4F-4223-AEA0-FD369B7827DE}']
    {class} function init(jsReplyProxyBoundaryInterface: JJsReplyProxyBoundaryInterface): JJavaScriptReplyProxyImpl; cdecl;
    {class} function forInvocationHandler(invocationHandler: JInvocationHandler): JJavaScriptReplyProxyImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/JavaScriptReplyProxyImpl')]
  JJavaScriptReplyProxyImpl = interface(JJavaScriptReplyProxy)
    ['{21D16267-883D-4442-882B-3FCAFD1A21D7}']
    procedure postMessage(string_: JString); cdecl; overload;
    procedure postMessage(b: TJavaArray<Byte>); cdecl; overload;
  end;
  TJJavaScriptReplyProxyImpl = class(TJavaGenericImport<JJavaScriptReplyProxyImplClass, JJavaScriptReplyProxyImpl>) end;

  JNavigationImplClass = interface(JObjectClass)
    ['{65CB603D-7D07-4744-A83C-8F0EA875F50E}']
    {class} function forInvocationHandler(invocationHandler: JInvocationHandler): JNavigation; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/NavigationImpl')]
  JNavigationImpl = interface(JObject)
    ['{AB685C63-2A10-4127-88E2-7952FCA9F6BA}']
    function didCommit: Boolean; cdecl;
    function didCommitErrorPage: Boolean; cdecl;
    function getPage: JPage; cdecl;
    function getStatusCode: Integer; cdecl;
    function getUrl: JString; cdecl;
    function isBack: Boolean; cdecl;
    function isForward: Boolean; cdecl;
    function isHistory: Boolean; cdecl;
    function isReload: Boolean; cdecl;
    function isRestore: Boolean; cdecl;
    function isSameDocument: Boolean; cdecl;
    function wasInitiatedByPage: Boolean; cdecl;
  end;
  TJNavigationImpl = class(TJavaGenericImport<JNavigationImplClass, JNavigationImpl>) end;

  JNavigationListenerAdapterClass = interface(JObjectClass)
    ['{2D0CE203-5315-4673-B331-35334EB9A9E6}']
    {class} function init(navigationListener: JNavigationListener): JNavigationListenerAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/NavigationListenerAdapter')]
  JNavigationListenerAdapter = interface(JObject)
    ['{6BEE4FCC-2435-439F-8EEE-55BADA41FDED}']
    function equals(object_: JObject): Boolean; cdecl;
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function hashCode: Integer; cdecl;
    procedure onFirstContentfulPaint(invocationHandler: JInvocationHandler; l: Int64); cdecl;
    procedure onNavigationCompleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationRedirected(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationStarted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDOMContentLoadedEventFired(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDeleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageLoadEventFired(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJNavigationListenerAdapter = class(TJavaGenericImport<JNavigationListenerAdapterClass, JNavigationListenerAdapter>) end;

  JNoVarySearchHeaderAdapterClass = interface(JObjectClass)
    ['{9EEDE1A8-4FF0-4842-9269-5F9AED9631B5}']
    {class} function init(noVarySearchHeader: JNoVarySearchHeader): JNoVarySearchHeaderAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/NoVarySearchHeaderAdapter')]
  JNoVarySearchHeaderAdapter = interface(JObject)
    ['{F1090EFC-5BAD-47BB-B9CE-B6831562FE66}']
    function getConsideredQueryParameters: JList; cdecl;
    function getIgnoreDifferencesInParameters: Boolean; cdecl;
    function getIgnoredQueryParameters: JList; cdecl;
    function getVaryOnKeyOrder: Boolean; cdecl;
  end;
  TJNoVarySearchHeaderAdapter = class(TJavaGenericImport<JNoVarySearchHeaderAdapterClass, JNoVarySearchHeaderAdapter>) end;

  JPageImplClass = interface(JObjectClass)
    ['{E6D826F3-2D3D-4D2C-956B-F3346F5269C0}']
    {class} function forInvocationHandler(invocationHandler: JInvocationHandler): JPage; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/PageImpl')]
  JPageImpl = interface(JObject)
    ['{1D6AE679-110A-46B9-A071-78D4AB2D9C86}']
  end;
  TJPageImpl = class(TJavaGenericImport<JPageImplClass, JPageImpl>) end;

  JPrefetchOperationCallbackAdapterClass = interface(JObjectClass)
    ['{A4E7EAF3-2F72-4A72-AF22-D530D1394040}']
    {class} function buildInvocationHandler(outcomeReceiverCompat: JOutcomeReceiverCompat): JInvocationHandler; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/PrefetchOperationCallbackAdapter')]
  JPrefetchOperationCallbackAdapter = interface(JObject)
    ['{8974D416-C7F0-4606-96EA-4AFDF5AE4433}']
  end;
  TJPrefetchOperationCallbackAdapter = class(TJavaGenericImport<JPrefetchOperationCallbackAdapterClass, JPrefetchOperationCallbackAdapter>) end;

  JProfileImplClass = interface(JObjectClass)
    ['{CBCB97ED-A24E-4FD2-9183-43DE547EE0CE}']
  end;

  [JavaSignature('androidx/webkit/internal/ProfileImpl')]
  JProfileImpl = interface(JObject)
    ['{637C82F0-BCA0-4360-B610-586893584337}']
    procedure addCustomHeader(customHeader: JCustomHeader); cdecl;
    procedure addQuicHints(set_: JSet); cdecl;
    procedure clearAllCustomHeaders; cdecl;
    procedure clearAllOriginMatchedHeaders; cdecl;
    procedure clearCustomHeader(string_: JString); cdecl; overload;
    procedure clearCustomHeader(string_: JString; string_1: JString); cdecl; overload;
    procedure clearOriginMatchedHeader(string_: JString); cdecl;
    procedure clearPrefetchAsync(string_: JString; executor: JExecutor; outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl;
    function getCookieManager: JCookieManager; cdecl;
    function getCustomHeaders: JSet; cdecl; overload;
    function getCustomHeaders(string_: JString): JSet; cdecl; overload;
    function getCustomHeaders(string_: JString; string_1: JString): JSet; cdecl; overload;
    function getGeolocationPermissions: JGeolocationPermissions; cdecl;
    function getName: JString; cdecl;
    function getServiceWorkerController: JServiceWorkerController; cdecl;
    function getWebStorage: JWebStorage; cdecl;
    function hasCustomHeader(string_: JString): Boolean; cdecl;
    function hasOriginMatchedHeader(string_: JString): Boolean; cdecl;
    procedure preconnect(string_: JString); cdecl;
    procedure prefetchUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      speculativeLoadingParameters: JSpeculativeLoadingParameters; outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl; overload;
    procedure prefetchUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      outcomeReceiverCompat: JOutcomeReceiverCompat); cdecl; overload;
    procedure setOriginMatchedHeader(string_: JString; string_1: JString; set_: JSet); cdecl;
    procedure setSpeculativeLoadingConfig(speculativeLoadingConfig: JSpeculativeLoadingConfig); cdecl;
    procedure warmUpRendererProcess; cdecl;
  end;
  TJProfileImpl = class(TJavaGenericImport<JProfileImplClass, JProfileImpl>) end;

  JProfileStoreImplClass = interface(JObjectClass)
    ['{4B159CDF-5058-4062-81F4-E434070A9BB0}']
    {class} function getInstance: JProfileStore; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ProfileStoreImpl')]
  JProfileStoreImpl = interface(JObject)
    ['{6C0E5E59-B2DB-404C-9535-1C52000A3A0A}']
    function deleteProfile(string_: JString): Boolean; cdecl;
    function getAllProfileNames: JList; cdecl;
    function getOrCreateProfile(string_: JString): JProfile; cdecl;
    function getProfile(string_: JString): JProfile; cdecl;
  end;
  TJProfileStoreImpl = class(TJavaGenericImport<JProfileStoreImplClass, JProfileStoreImpl>) end;

  JProxyControllerImplClass = interface(JProxyControllerClass)
    ['{7EA0C8EA-C403-4734-94A2-F892DDEAA387}']
    {class} function init: JProxyControllerImpl; cdecl;
    {class} function proxyRulesToStringArray(list: JList): TJavaObjectBiArray<JString>; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ProxyControllerImpl')]
  JProxyControllerImpl = interface(JProxyController)
    ['{D8E7581D-FDD2-409E-9C2A-243D89150A4D}']
    procedure clearProxyOverride(executor: JExecutor; runnable: JRunnable); cdecl;
    procedure setProxyOverride(proxyConfig: JProxyConfig; executor: JExecutor; runnable: JRunnable); cdecl;
  end;
  TJProxyControllerImpl = class(TJavaGenericImport<JProxyControllerImplClass, JProxyControllerImpl>) end;

  JSafeBrowsingResponseImplClass = interface(JSafeBrowsingResponseCompatClass)
    ['{7445C9D3-9D38-4647-BB32-7B179FC9FE05}']
    {class} function init(invocationHandler: JInvocationHandler): JSafeBrowsingResponseImpl; cdecl; overload;
    {class} function init(safeBrowsingResponse: JSafeBrowsingResponse): JSafeBrowsingResponseImpl; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/internal/SafeBrowsingResponseImpl')]
  JSafeBrowsingResponseImpl = interface(JSafeBrowsingResponseCompat)
    ['{44DCC0FB-15A5-4543-9BFB-4F1A8DE7A869}']
    procedure backToSafety(b: Boolean); cdecl;
    procedure proceed(b: Boolean); cdecl;
    procedure showInterstitial(b: Boolean); cdecl;
  end;
  TJSafeBrowsingResponseImpl = class(TJavaGenericImport<JSafeBrowsingResponseImplClass, JSafeBrowsingResponseImpl>) end;

  JScriptHandlerImplClass = interface(JObjectClass)
    ['{6C0C84BD-2704-4CDB-951D-576D0B6EC9C9}']
    {class} function toScriptHandler(invocationHandler: JInvocationHandler): JScriptHandlerImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ScriptHandlerImpl')]
  JScriptHandlerImpl = interface(JObject)
    ['{39A6454C-2F44-4B34-B3B9-7BF859D11B0A}']
    procedure remove; cdecl;
  end;
  TJScriptHandlerImpl = class(TJavaGenericImport<JScriptHandlerImplClass, JScriptHandlerImpl>) end;

  JServiceWorkerClientAdapterClass = interface(JObjectClass)
    ['{E8D52860-297D-4D61-B7F5-B8A0254EADD0}']
    {class} function init(serviceWorkerClientCompat: JServiceWorkerClientCompat): JServiceWorkerClientAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ServiceWorkerClientAdapter')]
  JServiceWorkerClientAdapter = interface(JObject)
    ['{2F817961-5883-44B0-AB59-480F652260E1}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function shouldInterceptRequest(webResourceRequest: JWebResourceRequest): JWebResourceResponse; cdecl;
  end;
  TJServiceWorkerClientAdapter = class(TJavaGenericImport<JServiceWorkerClientAdapterClass, JServiceWorkerClientAdapter>) end;

  JServiceWorkerControllerImplClass = interface(JServiceWorkerControllerCompatClass)
    ['{4E6787FA-16F6-46CA-BCE0-A5B40F40A30B}']
    {class} function init: JServiceWorkerControllerImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/ServiceWorkerControllerImpl')]
  JServiceWorkerControllerImpl = interface(JServiceWorkerControllerCompat)
    ['{1606FFCA-AE15-407E-B9F8-4A5D97C498C7}']
    function getServiceWorkerWebSettings: JServiceWorkerWebSettingsCompat; cdecl;
    procedure setServiceWorkerClient(serviceWorkerClientCompat: JServiceWorkerClientCompat); cdecl;
  end;
  TJServiceWorkerControllerImpl = class(TJavaGenericImport<JServiceWorkerControllerImplClass, JServiceWorkerControllerImpl>) end;

  JServiceWorkerWebSettingsImplClass = interface(JServiceWorkerWebSettingsCompatClass)
    ['{02A378F9-38A3-4348-9087-7406FD9DE5DC}']
    {class} function init(serviceWorkerWebSettings: JServiceWorkerWebSettings): JServiceWorkerWebSettingsImpl; cdecl; overload;
    {class} function init(invocationHandler: JInvocationHandler): JServiceWorkerWebSettingsImpl; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/internal/ServiceWorkerWebSettingsImpl')]
  JServiceWorkerWebSettingsImpl = interface(JServiceWorkerWebSettingsCompat)
    ['{73B6A716-C60F-470D-86A2-605188939900}']
    function getAllowContentAccess: Boolean; cdecl;
    function getAllowFileAccess: Boolean; cdecl;
    function getBlockNetworkLoads: Boolean; cdecl;
    function getCacheMode: Integer; cdecl;
    function getRequestedWithHeaderOriginAllowList: JSet; cdecl;
    function isIncludeCookiesOnShouldInterceptRequestEnabled: Boolean; cdecl;
    procedure setAllowContentAccess(b: Boolean); cdecl;
    procedure setAllowFileAccess(b: Boolean); cdecl;
    procedure setBlockNetworkLoads(b: Boolean); cdecl;
    procedure setCacheMode(i: Integer); cdecl;
    procedure setIncludeCookiesOnShouldInterceptRequestEnabled(b: Boolean); cdecl;
    procedure setRequestedWithHeaderOriginAllowList(set_: JSet); cdecl;
  end;
  TJServiceWorkerWebSettingsImpl = class(TJavaGenericImport<JServiceWorkerWebSettingsImplClass, JServiceWorkerWebSettingsImpl>) end;

  JSpeculativeLoadingConfigAdapterClass = interface(JObjectClass)
    ['{62DA0136-DADF-4EEB-A736-9C719C8388EB}']
    {class} function init(speculativeLoadingConfig: JSpeculativeLoadingConfig): JSpeculativeLoadingConfigAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/SpeculativeLoadingConfigAdapter')]
  JSpeculativeLoadingConfigAdapter = interface(JObject)
    ['{9FD7515B-546A-426F-8EE4-40AE7B5DDE64}']
    function getMaxPrefetches: Integer; cdecl;
    function getMaxPrerenders: Integer; cdecl;
    function getPrefetchTTLSeconds: Integer; cdecl;
  end;
  TJSpeculativeLoadingConfigAdapter = class(TJavaGenericImport<JSpeculativeLoadingConfigAdapterClass, JSpeculativeLoadingConfigAdapter>) end;

  JSpeculativeLoadingParametersAdapterClass = interface(JObjectClass)
    ['{153B2DBA-0182-4BEF-AE11-866D26DF2277}']
    {class} function init(speculativeLoadingParameters: JSpeculativeLoadingParameters): JSpeculativeLoadingParametersAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/SpeculativeLoadingParametersAdapter')]
  JSpeculativeLoadingParametersAdapter = interface(JObject)
    ['{8F6DD29B-FC3A-4CAA-B5F2-040CBB01937C}']
    function getAdditionalHeaders: JMap; cdecl;
    function getNoVarySearchData: JInvocationHandler; cdecl;
    function isJavaScriptEnabled: Boolean; cdecl;
  end;
  TJSpeculativeLoadingParametersAdapter = class(TJavaGenericImport<JSpeculativeLoadingParametersAdapterClass, JSpeculativeLoadingParametersAdapter>) end;

  JStartupApiFeatureClass = interface(JObjectClass)
    ['{A8D9ECA6-9CAC-4395-AE9B-CC8FB2662AE7}']
    {class} function _GetMETADATA_HOLDER_SERVICE_NAME: JString; cdecl;
    {class} function values: JSet; cdecl;
    {class} property METADATA_HOLDER_SERVICE_NAME: JString read _GetMETADATA_HOLDER_SERVICE_NAME;
  end;

  [JavaSignature('androidx/webkit/internal/StartupApiFeature')]
  JStartupApiFeature = interface(JObject)
    ['{5BF12532-9E7D-496C-8BD2-49537E14C88B}']
    function getPublicFeatureName: JString; cdecl;
    function isSupported(context: JContext): Boolean; cdecl;
    function isSupportedByFramework: Boolean; cdecl;
    function isSupportedByWebView(context: JContext): Boolean; cdecl;
  end;
  TJStartupApiFeature = class(TJavaGenericImport<JStartupApiFeatureClass, JStartupApiFeature>) end;

  JStartupApiFeature_NoFrameworkClass = interface(JStartupApiFeatureClass)
    ['{0D3D7D20-BEDE-46C8-9981-C052EFAA1B4E}']
  end;

  [JavaSignature('androidx/webkit/internal/StartupApiFeature$NoFramework')]
  JStartupApiFeature_NoFramework = interface(JStartupApiFeature)
    ['{B2EC003F-FA1D-4341-8745-6BD3CA493A47}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJStartupApiFeature_NoFramework = class(TJavaGenericImport<JStartupApiFeature_NoFrameworkClass, JStartupApiFeature_NoFramework>) end;

  JStartupApiFeature_PClass = interface(JStartupApiFeatureClass)
    ['{DE7021A0-CD38-4D43-BCAD-1FE073BD0873}']
  end;

  [JavaSignature('androidx/webkit/internal/StartupApiFeature$P')]
  JStartupApiFeature_P = interface(JStartupApiFeature)
    ['{1C6163DB-C0FF-4661-A238-528DE89E22C1}']
    function isSupportedByFramework: Boolean; cdecl;
  end;
  TJStartupApiFeature_P = class(TJavaGenericImport<JStartupApiFeature_PClass, JStartupApiFeature_P>) end;

  JStartupFeaturesClass = interface(JObjectClass)
    ['{75E69A9C-C156-4B11-BAA5-5E2ACF6DAB97}']
    {class} function _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATH: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JString; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JString; cdecl;
    {class} property STARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JString read _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES;
    {class} property STARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JString read _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX;
    {class} property STARTUP_FEATURE_SET_DIRECTORY_BASE_PATH: JString read _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATH;
    {class} property STARTUP_FEATURE_SET_PROFILES_TO_LOAD: JString read _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JString read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JString read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2;
  end;

  [JavaSignature('androidx/webkit/internal/StartupFeatures')]
  JStartupFeatures = interface(JObject)
    ['{B5F594ED-6DDF-47D0-A434-D50E591F9C64}']
  end;
  TJStartupFeatures = class(TJavaGenericImport<JStartupFeaturesClass, JStartupFeatures>) end;

  JTracingControllerImplClass = interface(Jwebkit_TracingControllerClass)
    ['{2B2CC9D0-FD26-4B06-A0D8-800855E89661}']
    {class} function init: JTracingControllerImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/TracingControllerImpl')]
  JTracingControllerImpl = interface(Jwebkit_TracingController)
    ['{8457691C-2CA8-4B11-B6CB-EDD325E1DD03}']
    function isTracing: Boolean; cdecl;
    procedure start(tracingConfig: Jwebkit_TracingConfig); cdecl;
    function stop(outputStream: JOutputStream; executor: JExecutor): Boolean; cdecl;
  end;
  TJTracingControllerImpl = class(TJavaGenericImport<JTracingControllerImplClass, JTracingControllerImpl>) end;

  JUserAgentMetadataInternalClass = interface(JObjectClass)
    ['{29A20429-2460-49FC-8C01-C847DA536683}']
  end;

  [JavaSignature('androidx/webkit/internal/UserAgentMetadataInternal')]
  JUserAgentMetadataInternal = interface(JObject)
    ['{BC799770-D62A-4497-97A1-5F7468282E07}']
  end;
  TJUserAgentMetadataInternal = class(TJavaGenericImport<JUserAgentMetadataInternalClass, JUserAgentMetadataInternal>) end;

  JVisualStateCallbackAdapterClass = interface(JObjectClass)
    ['{6DBADBE9-A427-476E-A279-930D7BF15CA5}']
    {class} function init(visualStateCallback: JWebViewCompat_VisualStateCallback): JVisualStateCallbackAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/VisualStateCallbackAdapter')]
  JVisualStateCallbackAdapter = interface(JObject)
    ['{07F22965-3E1F-44D9-A70A-05F647779F74}']
    procedure onComplete(l: Int64); cdecl;
  end;
  TJVisualStateCallbackAdapter = class(TJavaGenericImport<JVisualStateCallbackAdapterClass, JVisualStateCallbackAdapter>) end;

  JWebMessageAdapterClass = interface(JObjectClass)
    ['{AB61950A-B73D-4397-98A2-CB541D5AAFE1}']
    {class} function init(webMessageCompat: JWebMessageCompat): JWebMessageAdapter; cdecl;
    {class} function isMessagePayloadTypeSupportedByWebView(i: Integer): Boolean; cdecl;
    {class} function webMessageCompatFromBoundaryInterface(webMessageBoundaryInterface: JWebMessageBoundaryInterface): JWebMessageCompat; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebMessageAdapter')]
  JWebMessageAdapter = interface(JObject)
    ['{91F9D75F-4126-4EB9-AE1B-EF8AA3D92539}']
    function getData: JString; cdecl;
    function getMessagePayload: JInvocationHandler; cdecl;
    function getPorts: TJavaObjectArray<JInvocationHandler>; cdecl;
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
  end;
  TJWebMessageAdapter = class(TJavaGenericImport<JWebMessageAdapterClass, JWebMessageAdapter>) end;

  JWebMessageCallbackAdapterClass = interface(JObjectClass)
    ['{213121E7-2AA7-4097-BF66-7B19BFBC3AC4}']
    {class} function init(webMessageCallbackCompat: JWebMessagePortCompat_WebMessageCallbackCompat): JWebMessageCallbackAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebMessageCallbackAdapter')]
  JWebMessageCallbackAdapter = interface(JObject)
    ['{05FE3E49-9E55-4AA3-BDA3-55C2F07E3EAD}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    procedure onMessage(invocationHandler: JInvocationHandler; invocationHandler1: JInvocationHandler); cdecl;
  end;
  TJWebMessageCallbackAdapter = class(TJavaGenericImport<JWebMessageCallbackAdapterClass, JWebMessageCallbackAdapter>) end;

  JWebMessageListenerAdapterClass = interface(JObjectClass)
    ['{DADDD833-FB0C-4F1F-A051-09DBAE22F447}']
    {class} function init(webMessageListener: JWebViewCompat_WebMessageListener): JWebMessageListenerAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebMessageListenerAdapter')]
  JWebMessageListenerAdapter = interface(JObject)
    ['{BB471504-E504-4B8D-89C5-78F037378272}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    procedure onPostMessage(webView: JWebView; invocationHandler: JInvocationHandler; uri: Jnet_Uri; b: Boolean; invocationHandler1: JInvocationHandler); cdecl;
  end;
  TJWebMessageListenerAdapter = class(TJavaGenericImport<JWebMessageListenerAdapterClass, JWebMessageListenerAdapter>) end;

  JWebMessagePayloadAdapterClass = interface(JObjectClass)
    ['{E7DA91EE-934B-44BF-A971-CD596237DD40}']
    {class} function init(string_: JString): JWebMessagePayloadAdapter; cdecl; overload;
    {class} function init(b: TJavaArray<Byte>): JWebMessagePayloadAdapter; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/internal/WebMessagePayloadAdapter')]
  JWebMessagePayloadAdapter = interface(JObject)
    ['{C42D191B-5DA5-43EF-98A0-E1C303BDA6CC}']
    function getAsArrayBuffer: TJavaArray<Byte>; cdecl;
    function getAsString: JString; cdecl;
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function getType: Integer; cdecl;
  end;
  TJWebMessagePayloadAdapter = class(TJavaGenericImport<JWebMessagePayloadAdapterClass, JWebMessagePayloadAdapter>) end;

  JWebMessagePortImplClass = interface(JWebMessagePortCompatClass)
    ['{2C409526-DF0A-4804-912F-9FD3045583D6}']
    {class} function init(webMessagePort: JWebMessagePort): JWebMessagePortImpl; cdecl; overload;
    {class} function init(invocationHandler: JInvocationHandler): JWebMessagePortImpl; cdecl; overload;
    {class} function compatToFrameworkMessage(webMessageCompat: JWebMessageCompat): JWebMessage; cdecl;
    {class} function compatToPorts(webMessagePortCompat: TJavaObjectArray<JWebMessagePortCompat>): TJavaObjectArray<JWebMessagePort>; cdecl;
    {class} function frameworkMessageToCompat(webMessage: JWebMessage): JWebMessageCompat; cdecl;
    {class} function portsToCompat(webMessagePort: TJavaObjectArray<JWebMessagePort>): TJavaObjectArray<JWebMessagePortCompat>; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebMessagePortImpl')]
  JWebMessagePortImpl = interface(JWebMessagePortCompat)
    ['{6F84EBA7-43B6-4FBD-85E1-486BD09696FC}']
    procedure close; cdecl;
    function getFrameworkPort: JWebMessagePort; cdecl;
    function getInvocationHandler: JInvocationHandler; cdecl;
    procedure postMessage(webMessageCompat: JWebMessageCompat); cdecl;
    procedure setWebMessageCallback(webMessageCallbackCompat: JWebMessagePortCompat_WebMessageCallbackCompat); cdecl; overload;
    procedure setWebMessageCallback(handler: JHandler; webMessageCallbackCompat: JWebMessagePortCompat_WebMessageCallbackCompat); cdecl; overload;
  end;
  TJWebMessagePortImpl = class(TJavaGenericImport<JWebMessagePortImplClass, JWebMessagePortImpl>) end;

  JWebNavigationClientAdapterClass = interface(JObjectClass)
    ['{6DF53133-4B47-4373-B3BB-4B8D093576D0}']
    {class} function init(webNavigationClient: JWebNavigationClient): JWebNavigationClientAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebNavigationClientAdapter')]
  JWebNavigationClientAdapter = interface(JObject)
    ['{3A503B86-F9BF-4D14-A231-F9FEA62F9051}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function getWebNavigationClient: JWebNavigationClient; cdecl;
    procedure onFirstContentfulPaint(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationCompleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationRedirected(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationStarted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDOMContentLoadedEventFired(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDeleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageLoadEventFired(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebNavigationClientAdapter = class(TJavaGenericImport<JWebNavigationClientAdapterClass, JWebNavigationClientAdapter>) end;

  JWebResourceErrorImplClass = interface(JWebResourceErrorCompatClass)
    ['{C3BDC278-FBEF-47E3-932C-9C1DAB98A233}']
    {class} function init(invocationHandler: JInvocationHandler): JWebResourceErrorImpl; cdecl; overload;
    {class} function init(webResourceError: JWebResourceError): JWebResourceErrorImpl; cdecl; overload;
  end;

  [JavaSignature('androidx/webkit/internal/WebResourceErrorImpl')]
  JWebResourceErrorImpl = interface(JWebResourceErrorCompat)
    ['{1EFE20E9-7160-484B-A8D8-B02528D4C826}']
    function getDescription: JCharSequence; cdecl;
    function getErrorCode: Integer; cdecl;
  end;
  TJWebResourceErrorImpl = class(TJavaGenericImport<JWebResourceErrorImplClass, JWebResourceErrorImpl>) end;

  JWebResourceRequestAdapterClass = interface(JObjectClass)
    ['{578DCE02-AD38-4FEE-A372-9D158658E8CC}']
    {class} function init(webResourceRequestBoundaryInterface: JWebResourceRequestBoundaryInterface): JWebResourceRequestAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebResourceRequestAdapter')]
  JWebResourceRequestAdapter = interface(JObject)
    ['{E2120CC7-09B3-49ED-A39D-50D74FB4AA04}']
    function isRedirect: Boolean; cdecl;
  end;
  TJWebResourceRequestAdapter = class(TJavaGenericImport<JWebResourceRequestAdapterClass, JWebResourceRequestAdapter>) end;

  JWebSettingsAdapterClass = interface(JObjectClass)
    ['{A4B8A11B-7214-4D45-B718-DBF3763B3141}']
    {class} function init(webSettingsBoundaryInterface: JWebSettingsBoundaryInterface): JWebSettingsAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebSettingsAdapter')]
  JWebSettingsAdapter = interface(JObject)
    ['{63BF7DDE-B776-4E22-9616-B9E588E93DDD}']
    function getAttributionRegistrationBehavior: Integer; cdecl;
    function getBackForwardCacheEnabled: Boolean; cdecl;
    function getBackForwardCacheSettings: JBackForwardCacheSettings; cdecl;
    function getCookieAccessForShouldInterceptRequestEnabled: Boolean; cdecl;
    function getDisabledActionModeMenuItems: Integer; cdecl;
    function getEnterpriseAuthenticationAppLinkPolicyEnabled: Boolean; cdecl;
    function getForceDark: Integer; cdecl;
    function getForceDarkStrategy: Integer; cdecl;
    function getHasEnrolledInstrumentEnabled: Boolean; cdecl;
    function getOffscreenPreRaster: Boolean; cdecl;
    function getPaymentRequestEnabled: Boolean; cdecl;
    function getSafeBrowsingEnabled: Boolean; cdecl;
    function getSpeculativeLoadingStatus: Integer; cdecl;
    function getUserAgentMetadata: JUserAgentMetadata; cdecl;
    function getWebAuthenticationSupport: Integer; cdecl;
    function getWebViewMediaIntegrityApiStatus: JWebViewMediaIntegrityApiStatusConfig; cdecl;
    function isAlgorithmicDarkeningAllowed: Boolean; cdecl;
    procedure setAlgorithmicDarkeningAllowed(b: Boolean); cdecl;
    procedure setAttributionRegistrationBehavior(i: Integer); cdecl;
    procedure setBackForwardCacheEnabled(b: Boolean); cdecl;
    procedure setBackForwardCacheSettings(backForwardCacheSettings: JBackForwardCacheSettings); cdecl;
    procedure setCookieAccessForShouldInterceptRequestEnabled(b: Boolean); cdecl;
    procedure setDisabledActionModeMenuItems(i: Integer); cdecl;
    procedure setEnterpriseAuthenticationAppLinkPolicyEnabled(b: Boolean); cdecl;
    procedure setForceDark(i: Integer); cdecl;
    procedure setForceDarkStrategy(i: Integer); cdecl;
    procedure setHasEnrolledInstrumentEnabled(b: Boolean); cdecl;
    procedure setHyperlinkContextMenuItems(i: Integer); cdecl;
    procedure setOffscreenPreRaster(b: Boolean); cdecl;
    procedure setPaymentRequestEnabled(b: Boolean); cdecl;
    procedure setSafeBrowsingEnabled(b: Boolean); cdecl;
    procedure setSpeculativeLoadingStatus(i: Integer); cdecl;
    procedure setUserAgentMetadata(userAgentMetadata: JUserAgentMetadata); cdecl;
    procedure setWebAuthenticationSupport(i: Integer); cdecl;
    procedure setWebViewMediaIntegrityApiStatus(webViewMediaIntegrityApiStatusConfig: JWebViewMediaIntegrityApiStatusConfig); cdecl;
  end;
  TJWebSettingsAdapter = class(TJavaGenericImport<JWebSettingsAdapterClass, JWebSettingsAdapter>) end;

  JWebSettingsNoOpAdapterClass = interface(JWebSettingsAdapterClass)
    ['{14FE2A9B-6D87-4267-AD5D-EC79558FFC40}']
    {class} function init: JWebSettingsNoOpAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebSettingsNoOpAdapter')]
  JWebSettingsNoOpAdapter = interface(JWebSettingsAdapter)
    ['{102B784E-844A-48F1-8EB2-788DB21DFC3A}']
    function getAttributionRegistrationBehavior: Integer; cdecl;
    function getBackForwardCacheEnabled: Boolean; cdecl;
    function getBackForwardCacheSettings: JBackForwardCacheSettings; cdecl;
    function getCookieAccessForShouldInterceptRequestEnabled: Boolean; cdecl;
    function getDisabledActionModeMenuItems: Integer; cdecl;
    function getEnterpriseAuthenticationAppLinkPolicyEnabled: Boolean; cdecl;
    function getForceDark: Integer; cdecl;
    function getForceDarkStrategy: Integer; cdecl;
    function getHasEnrolledInstrumentEnabled: Boolean; cdecl;
    function getOffscreenPreRaster: Boolean; cdecl;
    function getPaymentRequestEnabled: Boolean; cdecl;
    function getSafeBrowsingEnabled: Boolean; cdecl;
    function getSpeculativeLoadingStatus: Integer; cdecl;
    function getUserAgentMetadata: JUserAgentMetadata; cdecl;
    function getWebAuthenticationSupport: Integer; cdecl;
    function getWebViewMediaIntegrityApiStatus: JWebViewMediaIntegrityApiStatusConfig; cdecl;
    function isAlgorithmicDarkeningAllowed: Boolean; cdecl;
    procedure setAlgorithmicDarkeningAllowed(b: Boolean); cdecl;
    procedure setAttributionRegistrationBehavior(i: Integer); cdecl;
    procedure setBackForwardCacheEnabled(b: Boolean); cdecl;
    procedure setBackForwardCacheSettings(backForwardCacheSettings: JBackForwardCacheSettings); cdecl;
    procedure setCookieAccessForShouldInterceptRequestEnabled(b: Boolean); cdecl;
    procedure setDisabledActionModeMenuItems(i: Integer); cdecl;
    procedure setEnterpriseAuthenticationAppLinkPolicyEnabled(b: Boolean); cdecl;
    procedure setForceDark(i: Integer); cdecl;
    procedure setForceDarkStrategy(i: Integer); cdecl;
    procedure setHasEnrolledInstrumentEnabled(b: Boolean); cdecl;
    procedure setHyperlinkContextMenuItems(i: Integer); cdecl;
    procedure setOffscreenPreRaster(b: Boolean); cdecl;
    procedure setPaymentRequestEnabled(b: Boolean); cdecl;
    procedure setSafeBrowsingEnabled(b: Boolean); cdecl;
    procedure setSpeculativeLoadingStatus(i: Integer); cdecl;
    procedure setUserAgentMetadata(userAgentMetadata: JUserAgentMetadata); cdecl;
    procedure setWebAuthenticationSupport(i: Integer); cdecl;
    procedure setWebViewMediaIntegrityApiStatus(webViewMediaIntegrityApiStatusConfig: JWebViewMediaIntegrityApiStatusConfig); cdecl;
  end;
  TJWebSettingsNoOpAdapter = class(TJavaGenericImport<JWebSettingsNoOpAdapterClass, JWebSettingsNoOpAdapter>) end;

  JWebStorageAdapterClass = interface(JObjectClass)
    ['{C35A3D94-0702-48B1-886C-A906BD6BB50E}']
    {class} function init(webStorageBoundaryInterface: JWebStorageBoundaryInterface): JWebStorageAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebStorageAdapter')]
  JWebStorageAdapter = interface(JObject)
    ['{DC040530-C6EB-4580-B2EE-205C2CD3661F}']
    procedure deleteBrowsingData(executor: JExecutor; runnable: JRunnable); cdecl;
    function deleteBrowsingDataForSite(string_: JString; executor: JExecutor; runnable: JRunnable): JString; cdecl;
  end;
  TJWebStorageAdapter = class(TJavaGenericImport<JWebStorageAdapterClass, JWebStorageAdapter>) end;

  JWebViewFeatureInternalClass = interface(JObjectClass)
    ['{A165FCBC-8F93-42D2-A5BB-994B4BC3C97B}']
    {class} function _GetADD_QUIC_HINTS_V1: JApiFeature_NoFramework; cdecl;
    {class} function _GetALGORITHMIC_DARKENING: JApiFeature_T; cdecl;
    {class} function _GetASYNC_WEBVIEW_STARTUP: JApiFeature_NoFramework; cdecl;
    {class} function _GetASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS: JApiFeature_NoFramework; cdecl;
    {class} function _GetATTRIBUTION_REGISTRATION_BEHAVIOR: JApiFeature_NoFramework; cdecl;
    {class} function _GetBACK_FORWARD_CACHE: JApiFeature_NoFramework; cdecl;
    {class} function _GetBACK_FORWARD_CACHE_SETTINGS: JApiFeature_NoFramework; cdecl;
    {class} function _GetCOOKIE_INTERCEPT: JApiFeature_NoFramework; cdecl;
    {class} function _GetCREATE_WEB_MESSAGE_CHANNEL: JApiFeature_M; cdecl;
    {class} function _GetCUSTOM_REQUEST_HEADERS: JApiFeature_NoFramework; cdecl;
    {class} function _GetDEFAULT_TRAFFICSTATS_TAGGING: JApiFeature_NoFramework; cdecl;
    {class} function _GetDELETE_BROWSING_DATA: JApiFeature_NoFramework; cdecl;
    {class} function _GetDISABLED_ACTION_MODE_MENU_ITEMS: JApiFeature_N; cdecl;
    {class} function _GetDOCUMENT_START_SCRIPT: JApiFeature_NoFramework; cdecl;
    {class} function _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JApiFeature_NoFramework; cdecl;
    {class} function _GetFORCE_DARK: JApiFeature_Q; cdecl;
    {class} function _GetFORCE_DARK_STRATEGY: JApiFeature_NoFramework; cdecl;
    {class} function _GetGET_COOKIE_INFO: JApiFeature_NoFramework; cdecl;
    {class} function _GetGET_VARIATIONS_HEADER: JApiFeature_NoFramework; cdecl;
    {class} function _GetGET_WEB_CHROME_CLIENT: JApiFeature_O; cdecl;
    {class} function _GetGET_WEB_VIEW_CLIENT: JApiFeature_O; cdecl;
    {class} function _GetGET_WEB_VIEW_RENDERER: JApiFeature_Q; cdecl;
    {class} function _GetHYPERLINK_CONTEXT_MENU_ITEMS: JApiFeature_NoFramework; cdecl;
    {class} function _GetMULTI_PROCESS: JApiFeature_NoFramework; cdecl;
    {class} function _GetMULTI_PROFILE: JApiFeature_NoFramework; cdecl;
    {class} function _GetMUTE_AUDIO: JApiFeature_NoFramework; cdecl;
    {class} function _GetNAVIGATION_CALLBACK_BASIC: JApiFeature_NoFramework; cdecl;
    {class} function _GetNAVIGATION_LISTENER_V1: JApiFeature_NoFramework; cdecl;
    {class} function _GetOFF_SCREEN_PRERASTER: JApiFeature_M; cdecl;
    {class} function _GetORIGIN_MATCHED_HEADERS: JApiFeature_NoFramework; cdecl;
    {class} function _GetPAYMENT_REQUEST: JApiFeature_NoFramework; cdecl;
    {class} function _GetPOST_WEB_MESSAGE: JApiFeature_M; cdecl;
    {class} function _GetPRECONNECT: JApiFeature_NoFramework; cdecl;
    {class} function _GetPRERENDER_WITH_URL: JApiFeature_NoFramework; cdecl;
    {class} function _GetPROFILE_URL_PREFETCH: JApiFeature_NoFramework; cdecl;
    {class} function _GetPROVIDER_WEAKLY_REF_WEBVIEW: JApiFeature_NoFramework; cdecl;
    {class} function _GetPROXY_OVERRIDE: JApiFeature_NoFramework; cdecl;
    {class} function _GetPROXY_OVERRIDE_REVERSE_BYPASS: JApiFeature_NoFramework; cdecl;
    {class} function _GetRECEIVE_HTTP_ERROR: JApiFeature_M; cdecl;
    {class} function _GetRECEIVE_WEB_RESOURCE_ERROR: JApiFeature_M; cdecl;
    {class} function _GetREQUESTED_WITH_HEADER_ALLOW_LIST: JApiFeature_NoFramework; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_DEPRECATED: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_PREFERRED: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST_PREFERRED_TO_DEPRECATED: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST_PREFERRED_TO_PREFERRED: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_ENABLE: JApiFeature_O; cdecl;
    {class} function _GetSAFE_BROWSING_HIT: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_PRIVACY_POLICY_URL: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_PROCEED: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JApiFeature_O_MR1; cdecl;
    {class} function _GetSAVE_STATE: JApiFeature_NoFramework; cdecl;
    {class} function _GetSERVICE_WORKER_BASIC_USAGE: JApiFeature_N; cdecl;
    {class} function _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS: JApiFeature_N; cdecl;
    {class} function _GetSERVICE_WORKER_CACHE_MODE: JApiFeature_N; cdecl;
    {class} function _GetSERVICE_WORKER_CONTENT_ACCESS: JApiFeature_N; cdecl;
    {class} function _GetSERVICE_WORKER_FILE_ACCESS: JApiFeature_N; cdecl;
    {class} function _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JApiFeature_N; cdecl;
    {class} function _GetSHOULD_OVERRIDE_WITH_REDIRECTS: JApiFeature_N; cdecl;
    {class} function _GetSPECULATIVE_LOADING: JApiFeature_NoFramework; cdecl;
    {class} function _GetSPECULATIVE_LOADING_CONFIG: JApiFeature_NoFramework; cdecl;
    {class} function _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JStartupApiFeature_NoFramework; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JStartupApiFeature_P; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATH: JStartupApiFeature_NoFramework; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD: JStartupApiFeature_NoFramework; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JStartupApiFeature_NoFramework; cdecl;
    {class} function _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JStartupApiFeature_NoFramework; cdecl;
    {class} function _GetSTART_SAFE_BROWSING: JApiFeature_O_MR1; cdecl;
    {class} function _GetTRACING_CONTROLLER_BASIC_USAGE: JApiFeature_P; cdecl;
    {class} function _GetUSER_AGENT_METADATA: JApiFeature_NoFramework; cdecl;
    {class} function _GetUSER_AGENT_METADATA_FORM_FACTORS: JApiFeature_NoFramework; cdecl;
    {class} function _GetVISUAL_STATE_CALLBACK: JApiFeature_M; cdecl;
    {class} function _GetWARM_UP_RENDERER_PROCESS: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEBVIEW_BUILDER_V1: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEB_AUTHENTICATION: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEB_MESSAGE_ARRAY_BUFFER: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE: JApiFeature_M; cdecl;
    {class} function _GetWEB_MESSAGE_LISTENER: JApiFeature_NoFramework; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_CLOSE: JApiFeature_M; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_POST_MESSAGE: JApiFeature_M; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JApiFeature_M; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_CODE: JApiFeature_M; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION: JApiFeature_M; cdecl;
    {class} function _GetWEB_RESOURCE_REQUEST_IS_REDIRECT: JApiFeature_N; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JApiFeature_Q; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_TERMINATE: JApiFeature_Q; cdecl;
    {class} function getUnsupportedOperationException: JUnsupportedOperationException; cdecl;
    {class} function isStartupFeatureSupported(string_: JString; context: JContext): Boolean; cdecl; overload;
    {class} function isStartupFeatureSupported(string_: JString; collection: JCollection; context: JContext): Boolean; cdecl; overload;
    {class} function isSupported(string_: JString): Boolean; cdecl; overload;
    {class} function isSupported(string_: JString; collection: JCollection): Boolean; cdecl; overload;
    {class} property ADD_QUIC_HINTS_V1: JApiFeature_NoFramework read _GetADD_QUIC_HINTS_V1;
    {class} property ALGORITHMIC_DARKENING: JApiFeature_T read _GetALGORITHMIC_DARKENING;
    {class} property ASYNC_WEBVIEW_STARTUP: JApiFeature_NoFramework read _GetASYNC_WEBVIEW_STARTUP;
    {class} property ASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS: JApiFeature_NoFramework read _GetASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS;
    {class} property ATTRIBUTION_REGISTRATION_BEHAVIOR: JApiFeature_NoFramework read _GetATTRIBUTION_REGISTRATION_BEHAVIOR;
    {class} property BACK_FORWARD_CACHE: JApiFeature_NoFramework read _GetBACK_FORWARD_CACHE;
    {class} property BACK_FORWARD_CACHE_SETTINGS: JApiFeature_NoFramework read _GetBACK_FORWARD_CACHE_SETTINGS;
    {class} property COOKIE_INTERCEPT: JApiFeature_NoFramework read _GetCOOKIE_INTERCEPT;
    {class} property CREATE_WEB_MESSAGE_CHANNEL: JApiFeature_M read _GetCREATE_WEB_MESSAGE_CHANNEL;
    {class} property CUSTOM_REQUEST_HEADERS: JApiFeature_NoFramework read _GetCUSTOM_REQUEST_HEADERS;
    {class} property DEFAULT_TRAFFICSTATS_TAGGING: JApiFeature_NoFramework read _GetDEFAULT_TRAFFICSTATS_TAGGING;
    {class} property DELETE_BROWSING_DATA: JApiFeature_NoFramework read _GetDELETE_BROWSING_DATA;
    {class} property DISABLED_ACTION_MODE_MENU_ITEMS: JApiFeature_N read _GetDISABLED_ACTION_MODE_MENU_ITEMS;
    {class} property DOCUMENT_START_SCRIPT: JApiFeature_NoFramework read _GetDOCUMENT_START_SCRIPT;
    {class} property ENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JApiFeature_NoFramework read _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY;
    {class} property FORCE_DARK: JApiFeature_Q read _GetFORCE_DARK;
    {class} property FORCE_DARK_STRATEGY: JApiFeature_NoFramework read _GetFORCE_DARK_STRATEGY;
    {class} property GET_COOKIE_INFO: JApiFeature_NoFramework read _GetGET_COOKIE_INFO;
    {class} property GET_VARIATIONS_HEADER: JApiFeature_NoFramework read _GetGET_VARIATIONS_HEADER;
    {class} property GET_WEB_CHROME_CLIENT: JApiFeature_O read _GetGET_WEB_CHROME_CLIENT;
    {class} property GET_WEB_VIEW_CLIENT: JApiFeature_O read _GetGET_WEB_VIEW_CLIENT;
    {class} property GET_WEB_VIEW_RENDERER: JApiFeature_Q read _GetGET_WEB_VIEW_RENDERER;
    {class} property HYPERLINK_CONTEXT_MENU_ITEMS: JApiFeature_NoFramework read _GetHYPERLINK_CONTEXT_MENU_ITEMS;
    {class} property MULTI_PROCESS: JApiFeature_NoFramework read _GetMULTI_PROCESS;
    {class} property MULTI_PROFILE: JApiFeature_NoFramework read _GetMULTI_PROFILE;
    {class} property MUTE_AUDIO: JApiFeature_NoFramework read _GetMUTE_AUDIO;
    {class} property NAVIGATION_CALLBACK_BASIC: JApiFeature_NoFramework read _GetNAVIGATION_CALLBACK_BASIC;
    {class} property NAVIGATION_LISTENER_V1: JApiFeature_NoFramework read _GetNAVIGATION_LISTENER_V1;
    {class} property OFF_SCREEN_PRERASTER: JApiFeature_M read _GetOFF_SCREEN_PRERASTER;
    {class} property ORIGIN_MATCHED_HEADERS: JApiFeature_NoFramework read _GetORIGIN_MATCHED_HEADERS;
    {class} property PAYMENT_REQUEST: JApiFeature_NoFramework read _GetPAYMENT_REQUEST;
    {class} property POST_WEB_MESSAGE: JApiFeature_M read _GetPOST_WEB_MESSAGE;
    {class} property PRECONNECT: JApiFeature_NoFramework read _GetPRECONNECT;
    {class} property PRERENDER_WITH_URL: JApiFeature_NoFramework read _GetPRERENDER_WITH_URL;
    {class} property PROFILE_URL_PREFETCH: JApiFeature_NoFramework read _GetPROFILE_URL_PREFETCH;
    {class} property PROVIDER_WEAKLY_REF_WEBVIEW: JApiFeature_NoFramework read _GetPROVIDER_WEAKLY_REF_WEBVIEW;
    {class} property PROXY_OVERRIDE: JApiFeature_NoFramework read _GetPROXY_OVERRIDE;
    {class} property PROXY_OVERRIDE_REVERSE_BYPASS: JApiFeature_NoFramework read _GetPROXY_OVERRIDE_REVERSE_BYPASS;
    {class} property RECEIVE_HTTP_ERROR: JApiFeature_M read _GetRECEIVE_HTTP_ERROR;
    {class} property RECEIVE_WEB_RESOURCE_ERROR: JApiFeature_M read _GetRECEIVE_WEB_RESOURCE_ERROR;
    {class} property REQUESTED_WITH_HEADER_ALLOW_LIST: JApiFeature_NoFramework read _GetREQUESTED_WITH_HEADER_ALLOW_LIST;
    {class} property SAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_DEPRECATED: JApiFeature_O_MR1 read _GetSAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_DEPRECATED;
    {class} property SAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_PREFERRED: JApiFeature_O_MR1 read _GetSAFE_BROWSING_ALLOWLIST_DEPRECATED_TO_PREFERRED;
    {class} property SAFE_BROWSING_ALLOWLIST_PREFERRED_TO_DEPRECATED: JApiFeature_O_MR1 read _GetSAFE_BROWSING_ALLOWLIST_PREFERRED_TO_DEPRECATED;
    {class} property SAFE_BROWSING_ALLOWLIST_PREFERRED_TO_PREFERRED: JApiFeature_O_MR1 read _GetSAFE_BROWSING_ALLOWLIST_PREFERRED_TO_PREFERRED;
    {class} property SAFE_BROWSING_ENABLE: JApiFeature_O read _GetSAFE_BROWSING_ENABLE;
    {class} property SAFE_BROWSING_HIT: JApiFeature_O_MR1 read _GetSAFE_BROWSING_HIT;
    {class} property SAFE_BROWSING_PRIVACY_POLICY_URL: JApiFeature_O_MR1 read _GetSAFE_BROWSING_PRIVACY_POLICY_URL;
    {class} property SAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JApiFeature_O_MR1 read _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY;
    {class} property SAFE_BROWSING_RESPONSE_PROCEED: JApiFeature_O_MR1 read _GetSAFE_BROWSING_RESPONSE_PROCEED;
    {class} property SAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JApiFeature_O_MR1 read _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL;
    {class} property SAVE_STATE: JApiFeature_NoFramework read _GetSAVE_STATE;
    {class} property SERVICE_WORKER_BASIC_USAGE: JApiFeature_N read _GetSERVICE_WORKER_BASIC_USAGE;
    {class} property SERVICE_WORKER_BLOCK_NETWORK_LOADS: JApiFeature_N read _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS;
    {class} property SERVICE_WORKER_CACHE_MODE: JApiFeature_N read _GetSERVICE_WORKER_CACHE_MODE;
    {class} property SERVICE_WORKER_CONTENT_ACCESS: JApiFeature_N read _GetSERVICE_WORKER_CONTENT_ACCESS;
    {class} property SERVICE_WORKER_FILE_ACCESS: JApiFeature_N read _GetSERVICE_WORKER_FILE_ACCESS;
    {class} property SERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JApiFeature_N read _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST;
    {class} property SHOULD_OVERRIDE_WITH_REDIRECTS: JApiFeature_N read _GetSHOULD_OVERRIDE_WITH_REDIRECTS;
    {class} property SPECULATIVE_LOADING: JApiFeature_NoFramework read _GetSPECULATIVE_LOADING;
    {class} property SPECULATIVE_LOADING_CONFIG: JApiFeature_NoFramework read _GetSPECULATIVE_LOADING_CONFIG;
    {class} property STARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES: JStartupApiFeature_NoFramework read _GetSTARTUP_FEATURE_CONFIGURE_PARTITIONED_COOKIES;
    {class} property STARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX: JStartupApiFeature_P read _GetSTARTUP_FEATURE_SET_DATA_DIRECTORY_SUFFIX;
    {class} property STARTUP_FEATURE_SET_DIRECTORY_BASE_PATH: JStartupApiFeature_NoFramework read _GetSTARTUP_FEATURE_SET_DIRECTORY_BASE_PATH;
    {class} property STARTUP_FEATURE_SET_PROFILES_TO_LOAD: JStartupApiFeature_NoFramework read _GetSTARTUP_FEATURE_SET_PROFILES_TO_LOAD;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE: JStartupApiFeature_NoFramework read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE;
    {class} property STARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2: JStartupApiFeature_NoFramework read _GetSTARTUP_FEATURE_SET_UI_THREAD_STARTUP_MODE_V2;
    {class} property START_SAFE_BROWSING: JApiFeature_O_MR1 read _GetSTART_SAFE_BROWSING;
    {class} property TRACING_CONTROLLER_BASIC_USAGE: JApiFeature_P read _GetTRACING_CONTROLLER_BASIC_USAGE;
    {class} property USER_AGENT_METADATA: JApiFeature_NoFramework read _GetUSER_AGENT_METADATA;
    {class} property USER_AGENT_METADATA_FORM_FACTORS: JApiFeature_NoFramework read _GetUSER_AGENT_METADATA_FORM_FACTORS;
    {class} property VISUAL_STATE_CALLBACK: JApiFeature_M read _GetVISUAL_STATE_CALLBACK;
    {class} property WARM_UP_RENDERER_PROCESS: JApiFeature_NoFramework read _GetWARM_UP_RENDERER_PROCESS;
    {class} property WEBVIEW_BUILDER_V1: JApiFeature_NoFramework read _GetWEBVIEW_BUILDER_V1;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_STATUS: JApiFeature_NoFramework read _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS;
    {class} property WEB_AUTHENTICATION: JApiFeature_NoFramework read _GetWEB_AUTHENTICATION;
    {class} property WEB_MESSAGE_ARRAY_BUFFER: JApiFeature_NoFramework read _GetWEB_MESSAGE_ARRAY_BUFFER;
    {class} property WEB_MESSAGE_CALLBACK_ON_MESSAGE: JApiFeature_M read _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE;
    {class} property WEB_MESSAGE_LISTENER: JApiFeature_NoFramework read _GetWEB_MESSAGE_LISTENER;
    {class} property WEB_MESSAGE_PORT_CLOSE: JApiFeature_M read _GetWEB_MESSAGE_PORT_CLOSE;
    {class} property WEB_MESSAGE_PORT_POST_MESSAGE: JApiFeature_M read _GetWEB_MESSAGE_PORT_POST_MESSAGE;
    {class} property WEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JApiFeature_M read _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK;
    {class} property WEB_RESOURCE_ERROR_GET_CODE: JApiFeature_M read _GetWEB_RESOURCE_ERROR_GET_CODE;
    {class} property WEB_RESOURCE_ERROR_GET_DESCRIPTION: JApiFeature_M read _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION;
    {class} property WEB_RESOURCE_REQUEST_IS_REDIRECT: JApiFeature_N read _GetWEB_RESOURCE_REQUEST_IS_REDIRECT;
    {class} property WEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JApiFeature_Q read _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE;
    {class} property WEB_VIEW_RENDERER_TERMINATE: JApiFeature_Q read _GetWEB_VIEW_RENDERER_TERMINATE;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewFeatureInternal')]
  JWebViewFeatureInternal = interface(JObject)
    ['{200EF1EF-FD78-4A31-B096-C60E5C58C643}']
  end;
  TJWebViewFeatureInternal = class(TJavaGenericImport<JWebViewFeatureInternalClass, JWebViewFeatureInternal>) end;

  JWebViewGlueCommunicatorClass = interface(JObjectClass)
    ['{82AE28C6-D8E8-43C0-AF22-150B41B299E3}']
    {class} function getCompatConverter: JWebkitToCompatConverter; cdecl;
    {class} function getFactory: JWebViewProviderFactory; cdecl;
    {class} function getWebViewClassLoader: JClassLoader; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewGlueCommunicator')]
  JWebViewGlueCommunicator = interface(JObject)
    ['{E8E01711-A8DE-4543-B184-8AA69FA94D44}']
  end;
  TJWebViewGlueCommunicator = class(TJavaGenericImport<JWebViewGlueCommunicatorClass, JWebViewGlueCommunicator>) end;

  JWebViewProviderAdapterClass = interface(JObjectClass)
    ['{D945F202-03FE-4803-B6D3-955049696EFF}']
    {class} function init(webViewProviderBoundaryInterface: JWebViewProviderBoundaryInterface): JWebViewProviderAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewProviderAdapter')]
  JWebViewProviderAdapter = interface(JObject)
    ['{84938633-B818-4E84-8A46-9051BF356A68}']
    function addDocumentStartJavaScript(string_: JString; string_1: TJavaObjectArray<JString>): JScriptHandlerImpl; cdecl;
    procedure addNavigationListener(executor: JExecutor; navigationListener: JNavigationListener); cdecl;
    procedure addWebMessageListener(string_: JString; string_1: TJavaObjectArray<JString>; webMessageListener: JWebViewCompat_WebMessageListener); cdecl;
    function createWebMessageChannel: TJavaObjectArray<JWebMessagePortCompat>; cdecl;
    function getProfile: JProfile; cdecl;
    function getWebChromeClient: JWebChromeClient; cdecl;
    function getWebNavigationClient: JWebNavigationClient; cdecl;
    function getWebViewClient: JWebViewClient; cdecl;
    function getWebViewRenderProcess: Jwebkit_WebViewRenderProcess; cdecl;
    function getWebViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient; cdecl;
    procedure insertVisualStateCallback(l: Int64; visualStateCallback: JWebViewCompat_VisualStateCallback); cdecl;
    function isAudioMuted: Boolean; cdecl;
    procedure postWebMessage(webMessageCompat: JWebMessageCompat; uri: Jnet_Uri); cdecl;
    procedure prerenderUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      prerenderOperationCallback: JPrerenderOperationCallback); cdecl; overload;
    procedure prerenderUrlAsync(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      speculativeLoadingParameters: JSpeculativeLoadingParameters; prerenderOperationCallback: JPrerenderOperationCallback); cdecl; overload;
    procedure removeNavigationListener(navigationListener: JNavigationListener); cdecl;
    procedure removeWebMessageListener(string_: JString); cdecl;
    procedure saveState(bundle: JBundle; i: Integer; b: Boolean); cdecl;
    procedure setAudioMuted(b: Boolean); cdecl;
    procedure setProfileWithName(string_: JString); cdecl;
    procedure setWebNavigationClient(webNavigationClient: JWebNavigationClient); cdecl;
    procedure setWebViewRenderProcessClient(executor: JExecutor; webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient); cdecl;
  end;
  TJWebViewProviderAdapter = class(TJavaGenericImport<JWebViewProviderAdapterClass, JWebViewProviderAdapter>) end;

  JWebViewProviderFactoryClass = interface(IJavaClass)
    ['{77FB0378-8CB1-4154-9E31-5FE1868E44C5}']
  end;

  [JavaSignature('androidx/webkit/internal/WebViewProviderFactory')]
  JWebViewProviderFactory = interface(IJavaInstance)
    ['{42E9CC52-88CB-457E-9489-B7D967918894}']
    function createWebView(webView: JWebView): JWebViewProviderBoundaryInterface; cdecl;
    function getDropDataProvider: JDropDataContentProviderBoundaryInterface; cdecl;
    function getProfileStore: JProfileStoreBoundaryInterface; cdecl;
    function getProxyController: JProxyControllerBoundaryInterface; cdecl;
    function getServiceWorkerController: JServiceWorkerControllerBoundaryInterface; cdecl;
    function getStatics: JStaticsBoundaryInterface; cdecl;
    function getTracingController: JTracingControllerBoundaryInterface; cdecl;
    function getWebViewBuilder: JWebViewBuilderBoundaryInterface; cdecl;
    function getWebViewFeatures: TJavaObjectArray<JString>; cdecl;
    function getWebkitToCompatConverter: JWebkitToCompatConverterBoundaryInterface; cdecl;
    procedure startUpWebView(webViewStartUpConfig: JWebViewStartUpConfig; webViewStartUpCallback: JWebViewCompat_WebViewStartUpCallback); cdecl;
  end;
  TJWebViewProviderFactory = class(TJavaGenericImport<JWebViewProviderFactoryClass, JWebViewProviderFactory>) end;

  JWebViewProviderFactoryAdapterClass = interface(JObjectClass)
    ['{590753DC-47F0-48EF-8C49-DC9C9CB4A661}']
    {class} function init(webViewProviderFactoryBoundaryInterface: JWebViewProviderFactoryBoundaryInterface): JWebViewProviderFactoryAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewProviderFactoryAdapter')]
  JWebViewProviderFactoryAdapter = interface(JObject)
    ['{17056765-B054-4B98-9C25-95B814B7A4B2}']
    function createWebView(webView: JWebView): JWebViewProviderBoundaryInterface; cdecl;
    function getDropDataProvider: JDropDataContentProviderBoundaryInterface; cdecl;
    function getProfileStore: JProfileStoreBoundaryInterface; cdecl;
    function getProxyController: JProxyControllerBoundaryInterface; cdecl;
    function getServiceWorkerController: JServiceWorkerControllerBoundaryInterface; cdecl;
    function getStatics: JStaticsBoundaryInterface; cdecl;
    function getTracingController: JTracingControllerBoundaryInterface; cdecl;
    function getWebViewBuilder: JWebViewBuilderBoundaryInterface; cdecl;
    function getWebViewFeatures: TJavaObjectArray<JString>; cdecl;
    function getWebkitToCompatConverter: JWebkitToCompatConverterBoundaryInterface; cdecl;
    procedure startUpWebView(webViewStartUpConfig: JWebViewStartUpConfig; webViewStartUpCallback: JWebViewCompat_WebViewStartUpCallback); cdecl;
  end;
  TJWebViewProviderFactoryAdapter = class(TJavaGenericImport<JWebViewProviderFactoryAdapterClass, JWebViewProviderFactoryAdapter>) end;

  JWebViewRenderProcessClientAdapterClass = interface(JObjectClass)
    ['{D01CA221-3A57-4458-ABF6-024A2BB499EE}']
    {class} function init(executor: JExecutor; webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient): JWebViewRenderProcessClientAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewRenderProcessClientAdapter')]
  JWebViewRenderProcessClientAdapter = interface(JObject)
    ['{07D11C01-CD0F-4A69-8DCE-BF5E0BC0B438}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function getWebViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient; cdecl;
    procedure onRendererResponsive(webView: JWebView; invocationHandler: JInvocationHandler); cdecl;
    procedure onRendererUnresponsive(webView: JWebView; invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewRenderProcessClientAdapter = class(TJavaGenericImport<JWebViewRenderProcessClientAdapterClass, JWebViewRenderProcessClientAdapter>) end;

  JWebViewRenderProcessClientFrameworkAdapterClass = interface(JWebViewRenderProcessClientClass)
    ['{EC266E28-FEDC-4BBA-9441-1027A3062A54}']
    {class} function init(webViewRenderProcessClient: Jwebkit_WebViewRenderProcessClient): JWebViewRenderProcessClientFrameworkAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewRenderProcessClientFrameworkAdapter')]
  JWebViewRenderProcessClientFrameworkAdapter = interface(JWebViewRenderProcessClient)
    ['{08CAAC35-1007-469B-BB67-526133DB0910}']
    function getFrameworkRenderProcessClient: Jwebkit_WebViewRenderProcessClient; cdecl;
    procedure onRenderProcessResponsive(webView: JWebView; webViewRenderProcess: JWebViewRenderProcess); cdecl;
    procedure onRenderProcessUnresponsive(webView: JWebView; webViewRenderProcess: JWebViewRenderProcess); cdecl;
  end;
  TJWebViewRenderProcessClientFrameworkAdapter = class(TJavaGenericImport<JWebViewRenderProcessClientFrameworkAdapterClass, JWebViewRenderProcessClientFrameworkAdapter>) end;

  JWebViewRenderProcessImplClass = interface(Jwebkit_WebViewRenderProcessClass)
    ['{AB156EF5-35E6-44F9-BB73-C0A29B4E1B0C}']
    {class} function init(webViewRendererBoundaryInterface: JWebViewRendererBoundaryInterface): JWebViewRenderProcessImpl; cdecl; overload;
    {class} function init(webViewRenderProcess: JWebViewRenderProcess): JWebViewRenderProcessImpl; cdecl; overload;
    {class} function forFrameworkObject(webViewRenderProcess: JWebViewRenderProcess): JWebViewRenderProcessImpl; cdecl;
    {class} function forInvocationHandler(invocationHandler: JInvocationHandler): JWebViewRenderProcessImpl; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewRenderProcessImpl')]
  JWebViewRenderProcessImpl = interface(Jwebkit_WebViewRenderProcess)
    ['{D41D9051-E25B-416F-ACEB-58A921F2F649}']
    function terminate: Boolean; cdecl;
  end;
  TJWebViewRenderProcessImpl = class(TJavaGenericImport<JWebViewRenderProcessImplClass, JWebViewRenderProcessImpl>) end;

  JWebViewStartUpCallbackAdapterClass = interface(JObjectClass)
    ['{E6804006-4229-47FD-B064-6EC6F5F43802}']
    {class} function init(webViewStartUpCallback: JWebViewCompat_WebViewStartUpCallback): JWebViewStartUpCallbackAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewStartUpCallbackAdapter')]
  JWebViewStartUpCallbackAdapter = interface(JObject)
    ['{D3281855-55DC-42CA-920A-7D1A31B3A28E}']
    procedure onSuccess(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewStartUpCallbackAdapter = class(TJavaGenericImport<JWebViewStartUpCallbackAdapterClass, JWebViewStartUpCallbackAdapter>) end;

  JWebViewStartUpConfigAdapterClass = interface(JObjectClass)
    ['{84149AA8-E676-46FC-9E08-2ADC1FD3E781}']
    {class} function init(webViewStartUpConfig: JWebViewStartUpConfig): JWebViewStartUpConfigAdapter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebViewStartUpConfigAdapter')]
  JWebViewStartUpConfigAdapter = interface(JObject)
    ['{4E10EC12-9CFA-494C-8854-D08546644EAA}']
    function getBackgroundExecutor: JExecutor; cdecl;
    function getProfileNamesToLoad: JSet; cdecl;
    function shouldRunUiThreadStartUpTasks: Boolean; cdecl;
  end;
  TJWebViewStartUpConfigAdapter = class(TJavaGenericImport<JWebViewStartUpConfigAdapterClass, JWebViewStartUpConfigAdapter>) end;

  JWebkitToCompatConverterClass = interface(JObjectClass)
    ['{E37FD339-4F01-451A-8CB3-F84AFFFC8B36}']
    {class} function init(webkitToCompatConverterBoundaryInterface: JWebkitToCompatConverterBoundaryInterface): JWebkitToCompatConverter; cdecl;
  end;

  [JavaSignature('androidx/webkit/internal/WebkitToCompatConverter')]
  JWebkitToCompatConverter = interface(JObject)
    ['{EC1E4BAF-0503-4C5B-A539-6D9BAAEC8C32}']
    function convertCookieManager(cookieManager: JCookieManager): JCookieManagerAdapter; cdecl;
    function convertSafeBrowsingResponse(safeBrowsingResponse: JSafeBrowsingResponse): JInvocationHandler; cdecl; overload;
    function convertSafeBrowsingResponse(invocationHandler: JInvocationHandler): JSafeBrowsingResponse; cdecl; overload;
    function convertServiceWorkerSettings(serviceWorkerWebSettings: JServiceWorkerWebSettings): JInvocationHandler; cdecl; overload;
    function convertServiceWorkerSettings(invocationHandler: JInvocationHandler): JServiceWorkerWebSettings; cdecl; overload;
    function convertSettings(webSettings: JWebSettings): JWebSettingsAdapter; cdecl;
    function convertWebMessagePort(webMessagePort: JWebMessagePort): JInvocationHandler; cdecl; overload;
    function convertWebMessagePort(invocationHandler: JInvocationHandler): JWebMessagePort; cdecl; overload;
    function convertWebResourceError(webResourceError: JWebResourceError): JInvocationHandler; cdecl; overload;
    function convertWebResourceError(invocationHandler: JInvocationHandler): JWebResourceError; cdecl; overload;
    function convertWebResourceRequest(webResourceRequest: JWebResourceRequest): JWebResourceRequestAdapter; cdecl;
    function convertWebStorage(webStorage: JWebStorage): JWebStorageAdapter; cdecl;
  end;
  TJWebkitToCompatConverter = class(TJavaGenericImport<JWebkitToCompatConverterClass, JWebkitToCompatConverter>) end;

  JDropDataContentProviderBoundaryInterfaceClass = interface(IJavaClass)
    ['{C4A7469C-49D4-493D-B61D-E94F4F8319C7}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/DropDataContentProviderBoundaryInterface')]
  JDropDataContentProviderBoundaryInterface = interface(IJavaInstance)
    ['{D9C81FC5-1493-4862-9F6A-58D852B862DE}']
    function cache(b: TJavaArray<Byte>; string_: JString; string_1: JString): Jnet_Uri; cdecl;
    function call(string_: JString; string_1: JString; bundle: JBundle): JBundle; cdecl;
    function getStreamTypes(uri: Jnet_Uri; string_: JString): TJavaObjectArray<JString>; cdecl;
    function getType(uri: Jnet_Uri): JString; cdecl;
    function onCreate: Boolean; cdecl;
    procedure onDragEnd(b: Boolean); cdecl;
    function openFile(contentProvider: JContentProvider; uri: Jnet_Uri): JParcelFileDescriptor; cdecl;
    function query(uri: Jnet_Uri; string_: TJavaObjectArray<JString>; string_1: JString; string_2: TJavaObjectArray<JString>; string_3: JString): JCursor; cdecl;
    procedure setClearCachedDataIntervalMs(i: Integer); cdecl;
  end;
  TJDropDataContentProviderBoundaryInterface = class(TJavaGenericImport<JDropDataContentProviderBoundaryInterfaceClass, JDropDataContentProviderBoundaryInterface>) end;

  JFeatureFlagHolderBoundaryInterfaceClass = interface(IJavaClass)
    ['{10120F96-49FB-4B3F-80F8-4EB040711006}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/FeatureFlagHolderBoundaryInterface')]
  JFeatureFlagHolderBoundaryInterface = interface(IJavaInstance)
    ['{59CD3B0A-786B-4DF6-8B94-AC6E714CED15}']
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
  end;
  TJFeatureFlagHolderBoundaryInterface = class(TJavaGenericImport<JFeatureFlagHolderBoundaryInterfaceClass, JFeatureFlagHolderBoundaryInterface>) end;

  JIsomorphicObjectBoundaryInterfaceClass = interface(IJavaClass)
    ['{BC3C4DDA-4E5E-4F4D-899B-5C3B8637C58C}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/IsomorphicObjectBoundaryInterface')]
  JIsomorphicObjectBoundaryInterface = interface(IJavaInstance)
    ['{31EC609D-DA4A-44B1-A42E-BC1C9000E7F4}']
    function getOrCreatePeer(callable: JCallable): JObject; cdecl;
  end;
  TJIsomorphicObjectBoundaryInterface = class(TJavaGenericImport<JIsomorphicObjectBoundaryInterfaceClass, JIsomorphicObjectBoundaryInterface>) end;

  JJsReplyProxyBoundaryInterfaceClass = interface(JIsomorphicObjectBoundaryInterfaceClass)
    ['{491C81F1-B16A-4ED9-9298-62F646B2D957}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/JsReplyProxyBoundaryInterface')]
  JJsReplyProxyBoundaryInterface = interface(JIsomorphicObjectBoundaryInterface)
    ['{4AEC8A7F-450D-46BC-8BA2-125C714FAB77}']
    procedure postMessage(string_: JString); cdecl;
    procedure postMessageWithPayload(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJJsReplyProxyBoundaryInterface = class(TJavaGenericImport<JJsReplyProxyBoundaryInterfaceClass, JJsReplyProxyBoundaryInterface>) end;

  JNoVarySearchDataBoundaryInterfaceClass = interface(IJavaClass)
    ['{CF1E9961-C154-4B45-82FF-210D39462A87}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/NoVarySearchDataBoundaryInterface')]
  JNoVarySearchDataBoundaryInterface = interface(IJavaInstance)
    ['{4CF17A52-F65C-47FC-BEA5-8D7A50F7628E}']
    function getConsideredQueryParameters: JList; cdecl;
    function getIgnoreDifferencesInParameters: Boolean; cdecl;
    function getIgnoredQueryParameters: JList; cdecl;
    function getVaryOnKeyOrder: Boolean; cdecl;
  end;
  TJNoVarySearchDataBoundaryInterface = class(TJavaGenericImport<JNoVarySearchDataBoundaryInterfaceClass, JNoVarySearchDataBoundaryInterface>) end;

  JOriginMatchedHeaderBoundaryInterfaceClass = interface(IJavaClass)
    ['{4CED1231-FF92-4FFE-9D88-782302FB424E}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/OriginMatchedHeaderBoundaryInterface')]
  JOriginMatchedHeaderBoundaryInterface = interface(IJavaInstance)
    ['{D0ABF200-859A-45C0-9CA4-D9BFEA30253E}']
    function getName: JString; cdecl;
    function getRules: JSet; cdecl;
    function getValue: JString; cdecl;
  end;
  TJOriginMatchedHeaderBoundaryInterface = class(TJavaGenericImport<JOriginMatchedHeaderBoundaryInterfaceClass, JOriginMatchedHeaderBoundaryInterface>) end;

  JPrefetchOperationCallbackBoundaryInterfaceClass = interface(IJavaClass)
    ['{BCCC8FA8-3582-4D6E-98ED-B1C3151DDA36}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/PrefetchOperationCallbackBoundaryInterface')]
  JPrefetchOperationCallbackBoundaryInterface = interface(IJavaInstance)
    ['{DCFCECB3-DBCB-4162-A962-B569964571E0}']
    procedure onFailure(i: Integer; string_: JString; i1: Integer); cdecl;
    procedure onSuccess; cdecl;
  end;
  TJPrefetchOperationCallbackBoundaryInterface = class(TJavaGenericImport<JPrefetchOperationCallbackBoundaryInterfaceClass, JPrefetchOperationCallbackBoundaryInterface>) end;

  JPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterfaceClass = interface(JAnnotationClass)
    ['{2C982729-0A28-4A37-AFA3-BFF866EFA94E}']
    {class} function _GetDUPLICATE: Integer; cdecl;
    {class} function _GetGENERIC: Integer; cdecl;
    {class} function _GetNETWORK: Integer; cdecl;
    {class} property DUPLICATE: Integer read _GetDUPLICATE;
    {class} property GENERIC: Integer read _GetGENERIC;
    {class} property NETWORK: Integer read _GetNETWORK;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/PrefetchOperationCallbackBoundaryInterface$PrefetchExceptionTypeBoundaryInterface')]
  JPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterface = interface(JAnnotation)
    ['{EAE33CE1-E186-47F0-BA58-4E459AB4D87B}']
  end;
  TJPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterface = class(TJavaGenericImport<JPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterfaceClass, JPrefetchOperationCallbackBoundaryInterface_PrefetchExceptionTypeBoundaryInterface>) end;

  JProcessGlobalConfigConstantsClass = interface(JObjectClass)
    ['{6BB1FC93-1A38-454D-A5B9-8D32FCC99FBE}']
    {class} function _GetCACHE_DIRECTORY_BASE_PATH: JString; cdecl;
    {class} function _GetCONFIGURE_PARTITIONED_COOKIES: JString; cdecl;
    {class} function _GetDATA_DIRECTORY_BASE_PATH: JString; cdecl;
    {class} function _GetDATA_DIRECTORY_SUFFIX: JString; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE: JString; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_PLUS_MULTI_PROCESS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_DEFAULT: Integer; cdecl;
    {class} function _GetUI_THREAD_STARTUP_MODE_SYNC: Integer; cdecl;
    {class} property CACHE_DIRECTORY_BASE_PATH: JString read _GetCACHE_DIRECTORY_BASE_PATH;
    {class} property CONFIGURE_PARTITIONED_COOKIES: JString read _GetCONFIGURE_PARTITIONED_COOKIES;
    {class} property DATA_DIRECTORY_BASE_PATH: JString read _GetDATA_DIRECTORY_BASE_PATH;
    {class} property DATA_DIRECTORY_SUFFIX: JString read _GetDATA_DIRECTORY_SUFFIX;
    {class} property UI_THREAD_STARTUP_MODE: JString read _GetUI_THREAD_STARTUP_MODE;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_LONG_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_PLUS_MULTI_PROCESS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_PLUS_MULTI_PROCESS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_SHORT_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS: Integer read _GetUI_THREAD_STARTUP_MODE_ASYNC_VERY_SHORT_TASKS;
    {class} property UI_THREAD_STARTUP_MODE_DEFAULT: Integer read _GetUI_THREAD_STARTUP_MODE_DEFAULT;
    {class} property UI_THREAD_STARTUP_MODE_SYNC: Integer read _GetUI_THREAD_STARTUP_MODE_SYNC;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ProcessGlobalConfigConstants')]
  JProcessGlobalConfigConstants = interface(JObject)
    ['{ACACF9BB-6427-4B78-95CA-43386B27DDBD}']
  end;
  TJProcessGlobalConfigConstants = class(TJavaGenericImport<JProcessGlobalConfigConstantsClass, JProcessGlobalConfigConstants>) end;

  JProcessGlobalConfigConstants_ProcessGlobalConfigMapKeyClass = interface(JAnnotationClass)
    ['{8F9315FF-3541-4945-B3F9-BCCEFEDD476D}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ProcessGlobalConfigConstants$ProcessGlobalConfigMapKey')]
  JProcessGlobalConfigConstants_ProcessGlobalConfigMapKey = interface(JAnnotation)
    ['{C7A67159-A4AB-4D4F-A2B3-63EC17FB7896}']
  end;
  TJProcessGlobalConfigConstants_ProcessGlobalConfigMapKey = class(TJavaGenericImport<JProcessGlobalConfigConstants_ProcessGlobalConfigMapKeyClass, JProcessGlobalConfigConstants_ProcessGlobalConfigMapKey>) end;

  JProfileBoundaryInterfaceClass = interface(IJavaClass)
    ['{D7FC6C96-16E1-4037-8A4B-9A6987CBA161}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ProfileBoundaryInterface')]
  JProfileBoundaryInterface = interface(IJavaInstance)
    ['{52DB1F98-DBA4-488D-9B6C-C6172713427B}']
    procedure addOriginMatchedHeader(string_: JString; string_1: JString; set_: JSet); cdecl;
    procedure addQuicHints(set_: JSet); cdecl;
    procedure clearAllOriginMatchedHeaders; cdecl;
    procedure clearOriginMatchedHeader(string_: JString); cdecl; overload;
    procedure clearOriginMatchedHeader(string_: JString; string_1: JString); cdecl; overload;
    procedure clearPrefetch(string_: JString; executor: JExecutor; invocationHandler: JInvocationHandler); cdecl;
    function getCookieManager: JCookieManager; cdecl;
    function getGeoLocationPermissions: JGeolocationPermissions; cdecl;
    function getName: JString; cdecl;
    function getOriginMatchedHeaders(string_: JString; string_1: JString): JList; cdecl;
    function getServiceWorkerController: JServiceWorkerController; cdecl;
    function getWebStorage: JWebStorage; cdecl;
    function hasOriginMatchedHeader(string_: JString): Boolean; cdecl;
    procedure preconnect(string_: JString); cdecl;
    procedure prefetchUrl(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor;
      invocationHandler: JInvocationHandler); cdecl; overload;
    procedure prefetchUrl(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor; invocationHandler: JInvocationHandler;
      invocationHandler1: JInvocationHandler); cdecl; overload;
    procedure setOriginMatchedHeader(string_: JString; string_1: JString; set_: JSet); cdecl;
    procedure setSpeculativeLoadingConfig(invocationHandler: JInvocationHandler); cdecl;
    procedure warmUpRendererProcess; cdecl;
  end;
  TJProfileBoundaryInterface = class(TJavaGenericImport<JProfileBoundaryInterfaceClass, JProfileBoundaryInterface>) end;

  JProfileStoreBoundaryInterfaceClass = interface(IJavaClass)
    ['{4C672E37-608E-41CE-BF6E-563DF7D0618B}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ProfileStoreBoundaryInterface')]
  JProfileStoreBoundaryInterface = interface(IJavaInstance)
    ['{F91FDC26-6678-4007-AEE3-B3370136A383}']
    function deleteProfile(string_: JString): Boolean; cdecl;
    function getAllProfileNames: JList; cdecl;
    function getOrCreateProfile(string_: JString): JInvocationHandler; cdecl;
    function getProfile(string_: JString): JInvocationHandler; cdecl;
  end;
  TJProfileStoreBoundaryInterface = class(TJavaGenericImport<JProfileStoreBoundaryInterfaceClass, JProfileStoreBoundaryInterface>) end;

  JProxyControllerBoundaryInterfaceClass = interface(IJavaClass)
    ['{7640198C-2C54-475D-B125-F441CDBBD20F}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ProxyControllerBoundaryInterface')]
  JProxyControllerBoundaryInterface = interface(IJavaInstance)
    ['{82E42052-F015-469C-9E49-28EBF7A4EE74}']
    procedure clearProxyOverride(runnable: JRunnable; executor: JExecutor); cdecl;
    procedure setProxyOverride(string_: TJavaObjectBiArray<JString>; string_1: TJavaObjectArray<JString>; runnable: JRunnable; executor: JExecutor); cdecl; overload;
    procedure setProxyOverride(string_: TJavaObjectBiArray<JString>; string_1: TJavaObjectArray<JString>; runnable: JRunnable; executor: JExecutor;
      b: Boolean); cdecl; overload;
  end;
  TJProxyControllerBoundaryInterface = class(TJavaGenericImport<JProxyControllerBoundaryInterfaceClass, JProxyControllerBoundaryInterface>) end;

  JSafeBrowsingResponseBoundaryInterfaceClass = interface(IJavaClass)
    ['{2217F9C4-7EA5-46F9-8C27-85AACDEEF858}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/SafeBrowsingResponseBoundaryInterface')]
  JSafeBrowsingResponseBoundaryInterface = interface(IJavaInstance)
    ['{B46590E1-60F1-4E32-A339-4A6166680E5D}']
    procedure backToSafety(b: Boolean); cdecl;
    procedure proceed(b: Boolean); cdecl;
    procedure showInterstitial(b: Boolean); cdecl;
  end;
  TJSafeBrowsingResponseBoundaryInterface = class(TJavaGenericImport<JSafeBrowsingResponseBoundaryInterfaceClass, JSafeBrowsingResponseBoundaryInterface>) end;

  JScriptHandlerBoundaryInterfaceClass = interface(IJavaClass)
    ['{3A215031-CD76-42DC-877C-81CB450579EE}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ScriptHandlerBoundaryInterface')]
  JScriptHandlerBoundaryInterface = interface(IJavaInstance)
    ['{0AF06662-4863-4AB1-9A53-9C080635C458}']
    procedure remove; cdecl;
  end;
  TJScriptHandlerBoundaryInterface = class(TJavaGenericImport<JScriptHandlerBoundaryInterfaceClass, JScriptHandlerBoundaryInterface>) end;

  JServiceWorkerClientBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{B292A568-36B2-44A3-9CDB-101CE88DDBDF}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ServiceWorkerClientBoundaryInterface')]
  JServiceWorkerClientBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{58CF478F-2A10-419A-B210-F66C6A056A72}']
    function shouldInterceptRequest(webResourceRequest: JWebResourceRequest): JWebResourceResponse; cdecl;
  end;
  TJServiceWorkerClientBoundaryInterface = class(TJavaGenericImport<JServiceWorkerClientBoundaryInterfaceClass, JServiceWorkerClientBoundaryInterface>) end;

  JServiceWorkerControllerBoundaryInterfaceClass = interface(IJavaClass)
    ['{C7317227-AC24-47A4-BD95-E3595EECB2AE}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ServiceWorkerControllerBoundaryInterface')]
  JServiceWorkerControllerBoundaryInterface = interface(IJavaInstance)
    ['{53F8A4F9-7A88-44A9-B2C8-ECE90A7BA050}']
    function getServiceWorkerWebSettings: JInvocationHandler; cdecl;
    procedure setServiceWorkerClient(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJServiceWorkerControllerBoundaryInterface = class(TJavaGenericImport<JServiceWorkerControllerBoundaryInterfaceClass, JServiceWorkerControllerBoundaryInterface>) end;

  JServiceWorkerWebSettingsBoundaryInterfaceClass = interface(IJavaClass)
    ['{6BECF968-0738-427E-92ED-278600C12147}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/ServiceWorkerWebSettingsBoundaryInterface')]
  JServiceWorkerWebSettingsBoundaryInterface = interface(IJavaInstance)
    ['{248A3245-BA62-4DF9-ADFF-AA2AE0AC7CDC}']
    function getAllowContentAccess: Boolean; cdecl;
    function getAllowFileAccess: Boolean; cdecl;
    function getBlockNetworkLoads: Boolean; cdecl;
    function getCacheMode: Integer; cdecl;
    function getIncludeCookiesOnIntercept: Boolean; cdecl;
    procedure setAllowContentAccess(b: Boolean); cdecl;
    procedure setAllowFileAccess(b: Boolean); cdecl;
    procedure setBlockNetworkLoads(b: Boolean); cdecl;
    procedure setCacheMode(i: Integer); cdecl;
    procedure setIncludeCookiesOnIntercept(b: Boolean); cdecl;
  end;
  TJServiceWorkerWebSettingsBoundaryInterface = class(TJavaGenericImport<JServiceWorkerWebSettingsBoundaryInterfaceClass, JServiceWorkerWebSettingsBoundaryInterface>) end;

  JSpeculativeLoadingConfigBoundaryInterfaceClass = interface(IJavaClass)
    ['{1E43CDDE-B6E2-48C5-94C9-E47E18405451}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/SpeculativeLoadingConfigBoundaryInterface')]
  JSpeculativeLoadingConfigBoundaryInterface = interface(IJavaInstance)
    ['{D09F49CB-FFDC-40D2-B648-753C9D78A573}']
    function getMaxPrefetches: Integer; cdecl;
    function getMaxPrerenders: Integer; cdecl;
    function getPrefetchTTLSeconds: Integer; cdecl;
  end;
  TJSpeculativeLoadingConfigBoundaryInterface = class(TJavaGenericImport<JSpeculativeLoadingConfigBoundaryInterfaceClass, JSpeculativeLoadingConfigBoundaryInterface>) end;

  JSpeculativeLoadingParametersBoundaryInterfaceClass = interface(IJavaClass)
    ['{1D4FECBB-B89E-4662-9E6E-BAC798E05B03}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/SpeculativeLoadingParametersBoundaryInterface')]
  JSpeculativeLoadingParametersBoundaryInterface = interface(IJavaInstance)
    ['{D5D42DC6-2CD8-47C1-8F06-5544445F4908}']
    function getAdditionalHeaders: JMap; cdecl;
    function getNoVarySearchData: JInvocationHandler; cdecl;
    function isJavaScriptEnabled: Boolean; cdecl;
  end;
  TJSpeculativeLoadingParametersBoundaryInterface = class(TJavaGenericImport<JSpeculativeLoadingParametersBoundaryInterfaceClass, JSpeculativeLoadingParametersBoundaryInterface>) end;

  JStaticsBoundaryInterfaceClass = interface(IJavaClass)
    ['{0F341C15-C391-4747-8A3B-EFAF82BBEF36}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/StaticsBoundaryInterface')]
  JStaticsBoundaryInterface = interface(IJavaInstance)
    ['{16F3133B-CC3B-4043-BA07-CEE0DC475021}']
    function getRendererLibraryPrefetchMode: Integer; cdecl;
    function getSafeBrowsingPrivacyPolicyUrl: Jnet_Uri; cdecl;
    function getVariationsHeader: JString; cdecl;
    procedure initSafeBrowsing(context: JContext; valueCallback: JValueCallback); cdecl;
    function isMultiProcessEnabled: Boolean; cdecl;
    procedure setDefaultTrafficStatsTag(i: Integer); cdecl;
    procedure setDefaultTrafficStatsUid(i: Integer); cdecl;
    procedure setRendererLibraryPrefetchMode(i: Integer); cdecl;
    procedure setSafeBrowsingAllowlist(set_: JSet; valueCallback: JValueCallback); cdecl;
    procedure setSafeBrowsingWhitelist(list: JList; valueCallback: JValueCallback); cdecl;
  end;
  TJStaticsBoundaryInterface = class(TJavaGenericImport<JStaticsBoundaryInterfaceClass, JStaticsBoundaryInterface>) end;

  JTracingControllerBoundaryInterfaceClass = interface(IJavaClass)
    ['{D5124EEF-F314-4565-A554-30FE9FC25C88}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/TracingControllerBoundaryInterface')]
  JTracingControllerBoundaryInterface = interface(IJavaInstance)
    ['{B9AE332E-EB84-404E-8F90-830A9DA44859}']
    function isTracing: Boolean; cdecl;
    procedure start(i: Integer; collection: JCollection; i1: Integer); cdecl;
    function stop(outputStream: JOutputStream; executor: JExecutor): Boolean; cdecl;
  end;
  TJTracingControllerBoundaryInterface = class(TJavaGenericImport<JTracingControllerBoundaryInterfaceClass, JTracingControllerBoundaryInterface>) end;

  JVisualStateCallbackBoundaryInterfaceClass = interface(IJavaClass)
    ['{056F1F26-0AEB-4550-B7D2-F43377010F36}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/VisualStateCallbackBoundaryInterface')]
  JVisualStateCallbackBoundaryInterface = interface(IJavaInstance)
    ['{5A621C2B-E974-498A-A8F5-16328426282E}']
    procedure onComplete(l: Int64); cdecl;
  end;
  TJVisualStateCallbackBoundaryInterface = class(TJavaGenericImport<JVisualStateCallbackBoundaryInterfaceClass, JVisualStateCallbackBoundaryInterface>) end;

  JWebMessageBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{E9CE94F0-202E-4F3B-8652-220753010040}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessageBoundaryInterface')]
  JWebMessageBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{A0809895-D719-4AB6-8793-703775EDF5B6}']
    function getData: JString; cdecl;
    function getMessagePayload: JInvocationHandler; cdecl;
    function getPorts: TJavaObjectArray<JInvocationHandler>; cdecl;
  end;
  TJWebMessageBoundaryInterface = class(TJavaGenericImport<JWebMessageBoundaryInterfaceClass, JWebMessageBoundaryInterface>) end;

  JWebMessageCallbackBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{A27AE020-3922-463D-A071-6E8EFF4B178F}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessageCallbackBoundaryInterface')]
  JWebMessageCallbackBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{410E2FF2-3A1F-48B3-914F-F3F9556E4997}']
    procedure onMessage(invocationHandler: JInvocationHandler; invocationHandler1: JInvocationHandler); cdecl;
  end;
  TJWebMessageCallbackBoundaryInterface = class(TJavaGenericImport<JWebMessageCallbackBoundaryInterfaceClass, JWebMessageCallbackBoundaryInterface>) end;

  JWebMessageListenerBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{BF4F2F50-6F8C-4C24-8C9B-BDB92A0BF3DA}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessageListenerBoundaryInterface')]
  JWebMessageListenerBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{0D4C23FC-6233-4ACB-980E-A9179D3178BF}']
    procedure onPostMessage(webView: JWebView; invocationHandler: JInvocationHandler; uri: Jnet_Uri; b: Boolean; invocationHandler1: JInvocationHandler); cdecl;
  end;
  TJWebMessageListenerBoundaryInterface = class(TJavaGenericImport<JWebMessageListenerBoundaryInterfaceClass, JWebMessageListenerBoundaryInterface>) end;

  JWebMessagePayloadBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{2D5274A2-64EF-4C1D-9DF4-60F4E56A6E3E}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessagePayloadBoundaryInterface')]
  JWebMessagePayloadBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{D7277A83-5EAB-43BE-83F2-3C579D18181C}']
    function getAsArrayBuffer: TJavaArray<Byte>; cdecl;
    function getAsString: JString; cdecl;
    function getType: Integer; cdecl;
  end;
  TJWebMessagePayloadBoundaryInterface = class(TJavaGenericImport<JWebMessagePayloadBoundaryInterfaceClass, JWebMessagePayloadBoundaryInterface>) end;

  JWebMessagePayloadBoundaryInterface_WebMessagePayloadTypeClass = interface(JAnnotationClass)
    ['{2BCE1461-352D-4D90-96AC-3E462931B3AB}']
    {class} function _GetTYPE_ARRAY_BUFFER: Integer; cdecl;
    {class} function _GetTYPE_STRING: Integer; cdecl;
    {class} property TYPE_ARRAY_BUFFER: Integer read _GetTYPE_ARRAY_BUFFER;
    {class} property TYPE_STRING: Integer read _GetTYPE_STRING;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessagePayloadBoundaryInterface$WebMessagePayloadType')]
  JWebMessagePayloadBoundaryInterface_WebMessagePayloadType = interface(JAnnotation)
    ['{391E95D8-F3DB-441B-BB0B-8833CE115372}']
  end;
  TJWebMessagePayloadBoundaryInterface_WebMessagePayloadType = class(TJavaGenericImport<JWebMessagePayloadBoundaryInterface_WebMessagePayloadTypeClass, JWebMessagePayloadBoundaryInterface_WebMessagePayloadType>) end;

  JWebMessagePortBoundaryInterfaceClass = interface(IJavaClass)
    ['{EAC071B8-02B8-4BC4-9840-62DB06977422}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebMessagePortBoundaryInterface')]
  JWebMessagePortBoundaryInterface = interface(IJavaInstance)
    ['{D9EF639A-5CAE-479C-9AE3-984B46DAF6EA}']
    procedure close; cdecl;
    procedure postMessage(invocationHandler: JInvocationHandler); cdecl;
    procedure setWebMessageCallback(invocationHandler: JInvocationHandler); cdecl; overload;
    procedure setWebMessageCallback(invocationHandler: JInvocationHandler; handler: JHandler); cdecl; overload;
  end;
  TJWebMessagePortBoundaryInterface = class(TJavaGenericImport<JWebMessagePortBoundaryInterfaceClass, JWebMessagePortBoundaryInterface>) end;

  JWebResourceErrorBoundaryInterfaceClass = interface(IJavaClass)
    ['{59FD1E1E-B383-45C4-B92E-3841CE2FE875}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebResourceErrorBoundaryInterface')]
  JWebResourceErrorBoundaryInterface = interface(IJavaInstance)
    ['{D9B9CA52-A7DA-4E49-995D-A0061C9D7874}']
    function getDescription: JCharSequence; cdecl;
    function getErrorCode: Integer; cdecl;
  end;
  TJWebResourceErrorBoundaryInterface = class(TJavaGenericImport<JWebResourceErrorBoundaryInterfaceClass, JWebResourceErrorBoundaryInterface>) end;

  JWebResourceRequestBoundaryInterfaceClass = interface(IJavaClass)
    ['{5B26C421-CAAE-4E08-84B7-D3432DE7FFA4}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebResourceRequestBoundaryInterface')]
  JWebResourceRequestBoundaryInterface = interface(IJavaInstance)
    ['{B4EF369D-0669-4D5F-99BC-4E2F2A595774}']
    function isRedirect: Boolean; cdecl;
  end;
  TJWebResourceRequestBoundaryInterface = class(TJavaGenericImport<JWebResourceRequestBoundaryInterfaceClass, JWebResourceRequestBoundaryInterface>) end;

  JWebSettingsBoundaryInterfaceClass = interface(IJavaClass)
    ['{D51E07FB-8608-45D7-97B4-7128BD7F10B4}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface')]
  JWebSettingsBoundaryInterface = interface(IJavaInstance)
    ['{B21948D2-1D24-4E74-B8D1-71D2758CBB19}']
    function getAttributionBehavior: Integer; cdecl;
    function getBackForwardCacheEnabled: Boolean; cdecl;
    function getBackForwardCacheSettings: JInvocationHandler; cdecl;
    function getDisabledActionModeMenuItems: Integer; cdecl;
    function getEnterpriseAuthenticationAppLinkPolicyEnabled: Boolean; cdecl;
    function getForceDark: Integer; cdecl;
    function getForceDarkBehavior: Integer; cdecl;
    function getHasEnrolledInstrumentEnabled: Boolean; cdecl;
    function getIncludeCookiesOnIntercept: Boolean; cdecl;
    function getOffscreenPreRaster: Boolean; cdecl;
    function getPaymentRequestEnabled: Boolean; cdecl;
    function getSafeBrowsingEnabled: Boolean; cdecl;
    function getSpeculativeLoadingStatus: Integer; cdecl;
    function getUserAgentMetadataMap: JMap; cdecl;
    function getWebViewMediaIntegrityApiDefaultStatus: Integer; cdecl;
    function getWebViewMediaIntegrityApiOverrideRules: JMap; cdecl;
    function getWebauthnSupport: Integer; cdecl;
    function getWillSuppressErrorPage: Boolean; cdecl;
    function isAlgorithmicDarkeningAllowed: Boolean; cdecl;
    procedure setAlgorithmicDarkeningAllowed(b: Boolean); cdecl;
    procedure setAttributionBehavior(i: Integer); cdecl;
    procedure setBackForwardCacheEnabled(b: Boolean); cdecl;
    procedure setBackForwardCacheSettings(invocationHandler: JInvocationHandler); cdecl;
    procedure setDisabledActionModeMenuItems(i: Integer); cdecl;
    procedure setEnterpriseAuthenticationAppLinkPolicyEnabled(b: Boolean); cdecl;
    procedure setForceDark(i: Integer); cdecl;
    procedure setForceDarkBehavior(i: Integer); cdecl;
    procedure setHasEnrolledInstrumentEnabled(b: Boolean); cdecl;
    procedure setHyperlinkContextMenuItems(i: Integer); cdecl;
    procedure setIncludeCookiesOnIntercept(b: Boolean); cdecl;
    procedure setOffscreenPreRaster(b: Boolean); cdecl;
    procedure setPaymentRequestEnabled(b: Boolean); cdecl;
    procedure setSafeBrowsingEnabled(b: Boolean); cdecl;
    procedure setSpeculativeLoadingStatus(i: Integer); cdecl;
    procedure setUserAgentMetadataFromMap(map: JMap); cdecl;
    procedure setWebViewMediaIntegrityApiStatus(i: Integer; map: JMap); cdecl;
    procedure setWebauthnSupport(i: Integer); cdecl;
    procedure setWillSuppressErrorPage(b: Boolean); cdecl;
  end;
  TJWebSettingsBoundaryInterface = class(TJavaGenericImport<JWebSettingsBoundaryInterfaceClass, JWebSettingsBoundaryInterface>) end;

  JWebSettingsBoundaryInterface_AttributionBehaviorClass = interface(JAnnotationClass)
    ['{A9D8E2C0-278B-4511-9DB0-F01AAA6C35E3}']
    {class} function _GetAPP_SOURCE_AND_APP_TRIGGER: Integer; cdecl;
    {class} function _GetAPP_SOURCE_AND_WEB_TRIGGER: Integer; cdecl;
    {class} function _GetDISABLED: Integer; cdecl;
    {class} function _GetWEB_SOURCE_AND_WEB_TRIGGER: Integer; cdecl;
    {class} property APP_SOURCE_AND_APP_TRIGGER: Integer read _GetAPP_SOURCE_AND_APP_TRIGGER;
    {class} property APP_SOURCE_AND_WEB_TRIGGER: Integer read _GetAPP_SOURCE_AND_WEB_TRIGGER;
    {class} property DISABLED: Integer read _GetDISABLED;
    {class} property WEB_SOURCE_AND_WEB_TRIGGER: Integer read _GetWEB_SOURCE_AND_WEB_TRIGGER;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$AttributionBehavior')]
  JWebSettingsBoundaryInterface_AttributionBehavior = interface(JAnnotation)
    ['{1ACCBF83-D933-4299-A7BD-140A1D55457B}']
  end;
  TJWebSettingsBoundaryInterface_AttributionBehavior = class(TJavaGenericImport<JWebSettingsBoundaryInterface_AttributionBehaviorClass, JWebSettingsBoundaryInterface_AttributionBehavior>) end;

  JWebSettingsBoundaryInterface_ForceDarkBehaviorClass = interface(JAnnotationClass)
    ['{035E62D6-E86F-466F-830A-6292FAD035FB}']
    {class} function _GetFORCE_DARK_ONLY: Integer; cdecl;
    {class} function _GetMEDIA_QUERY_ONLY: Integer; cdecl;
    {class} function _GetPREFER_MEDIA_QUERY_OVER_FORCE_DARK: Integer; cdecl;
    {class} property FORCE_DARK_ONLY: Integer read _GetFORCE_DARK_ONLY;
    {class} property MEDIA_QUERY_ONLY: Integer read _GetMEDIA_QUERY_ONLY;
    {class} property PREFER_MEDIA_QUERY_OVER_FORCE_DARK: Integer read _GetPREFER_MEDIA_QUERY_OVER_FORCE_DARK;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$ForceDarkBehavior')]
  JWebSettingsBoundaryInterface_ForceDarkBehavior = interface(JAnnotation)
    ['{3B1099B8-F082-4627-A0A9-48A7B1EA9EAB}']
  end;
  TJWebSettingsBoundaryInterface_ForceDarkBehavior = class(TJavaGenericImport<JWebSettingsBoundaryInterface_ForceDarkBehaviorClass, JWebSettingsBoundaryInterface_ForceDarkBehavior>) end;

  JWebSettingsBoundaryInterface_HyperlinkContextMenuItemsClass = interface(JAnnotationClass)
    ['{2701CEFB-3738-4FDF-9F88-75AAADD843C4}']
    {class} function _GetCOPY_LINK_ADDRESS: Integer; cdecl;
    {class} function _GetCOPY_LINK_TEXT: Integer; cdecl;
    {class} function _GetDISABLED: Integer; cdecl;
    {class} function _GetOPEN_LINK: Integer; cdecl;
    {class} property COPY_LINK_ADDRESS: Integer read _GetCOPY_LINK_ADDRESS;
    {class} property COPY_LINK_TEXT: Integer read _GetCOPY_LINK_TEXT;
    {class} property DISABLED: Integer read _GetDISABLED;
    {class} property OPEN_LINK: Integer read _GetOPEN_LINK;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$HyperlinkContextMenuItems')]
  JWebSettingsBoundaryInterface_HyperlinkContextMenuItems = interface(JAnnotation)
    ['{8A1D8CEF-AA8B-4526-B907-16D5484BD57A}']
  end;
  TJWebSettingsBoundaryInterface_HyperlinkContextMenuItems = class(TJavaGenericImport<JWebSettingsBoundaryInterface_HyperlinkContextMenuItemsClass, JWebSettingsBoundaryInterface_HyperlinkContextMenuItems>) end;

  JWebSettingsBoundaryInterface_SpeculativeLoadingStatusClass = interface(JAnnotationClass)
    ['{54CF4649-90D0-4B46-BE82-A793D63E8DC4}']
    {class} function _GetDISABLED: Integer; cdecl;
    {class} function _GetPRERENDER_ENABLED: Integer; cdecl;
    {class} property DISABLED: Integer read _GetDISABLED;
    {class} property PRERENDER_ENABLED: Integer read _GetPRERENDER_ENABLED;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$SpeculativeLoadingStatus')]
  JWebSettingsBoundaryInterface_SpeculativeLoadingStatus = interface(JAnnotation)
    ['{E0E648D7-5418-4F95-83A4-3AD7FF90ABC5}']
  end;
  TJWebSettingsBoundaryInterface_SpeculativeLoadingStatus = class(TJavaGenericImport<JWebSettingsBoundaryInterface_SpeculativeLoadingStatusClass, JWebSettingsBoundaryInterface_SpeculativeLoadingStatus>) end;

  JWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatusClass = interface(JAnnotationClass)
    ['{0A1468E6-854A-4AF0-BC0A-9F874A716694}']
    {class} function _GetDISABLED: Integer; cdecl;
    {class} function _GetENABLED: Integer; cdecl;
    {class} function _GetENABLED_WITHOUT_APP_IDENTITY: Integer; cdecl;
    {class} property DISABLED: Integer read _GetDISABLED;
    {class} property ENABLED: Integer read _GetENABLED;
    {class} property ENABLED_WITHOUT_APP_IDENTITY: Integer read _GetENABLED_WITHOUT_APP_IDENTITY;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$WebViewMediaIntegrityApiStatus')]
  JWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatus = interface(JAnnotation)
    ['{BC9806CC-D4EF-4FFB-99DA-C384C59D3C98}']
  end;
  TJWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatus = class(TJavaGenericImport<JWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatusClass, JWebSettingsBoundaryInterface_WebViewMediaIntegrityApiStatus>) end;

  JWebSettingsBoundaryInterface_WebauthnSupportClass = interface(JAnnotationClass)
    ['{5EBB4EBB-4063-4224-BEE1-949A2115D32A}']
    {class} function _GetAPP: Integer; cdecl;
    {class} function _GetBROWSER: Integer; cdecl;
    {class} function _GetNONE: Integer; cdecl;
    {class} property APP: Integer read _GetAPP;
    {class} property BROWSER: Integer read _GetBROWSER;
    {class} property NONE: Integer read _GetNONE;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebSettingsBoundaryInterface$WebauthnSupport')]
  JWebSettingsBoundaryInterface_WebauthnSupport = interface(JAnnotation)
    ['{810A4D04-333D-4E84-B215-2D23F97BEA1F}']
  end;
  TJWebSettingsBoundaryInterface_WebauthnSupport = class(TJavaGenericImport<JWebSettingsBoundaryInterface_WebauthnSupportClass, JWebSettingsBoundaryInterface_WebauthnSupport>) end;

  JWebStorageBoundaryInterfaceClass = interface(IJavaClass)
    ['{D949DA61-C022-4C62-A5AE-AF0133AA23E6}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebStorageBoundaryInterface')]
  JWebStorageBoundaryInterface = interface(IJavaInstance)
    ['{6548353B-C7F1-4A03-A864-BA90E5E30616}']
    procedure deleteBrowsingData(executor: JExecutor; runnable: JRunnable); cdecl;
    function deleteBrowsingDataForSite(string_: JString; executor: JExecutor; runnable: JRunnable): JString; cdecl;
  end;
  TJWebStorageBoundaryInterface = class(TJavaGenericImport<JWebStorageBoundaryInterfaceClass, JWebStorageBoundaryInterface>) end;

  JWebViewBackForwardCacheSettingsBoundaryInterfaceClass = interface(JIsomorphicObjectBoundaryInterfaceClass)
    ['{E9D7133F-4DB9-4415-8B82-8506786D2D02}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewBackForwardCacheSettingsBoundaryInterface')]
  JWebViewBackForwardCacheSettingsBoundaryInterface = interface(JIsomorphicObjectBoundaryInterface)
    ['{7CB30AF6-E39F-4ADB-9C5A-4E4B4837B485}']
    function getMaxPagesInCache: Integer; cdecl;
    function getTimeoutInSeconds: Integer; cdecl;
  end;
  TJWebViewBackForwardCacheSettingsBoundaryInterface = class(TJavaGenericImport<JWebViewBackForwardCacheSettingsBoundaryInterfaceClass, JWebViewBackForwardCacheSettingsBoundaryInterface>) end;

  JWebViewBuilderBoundaryInterfaceClass = interface(IJavaClass)
    ['{7709BF24-AAC6-4F45-91B8-5A71D1F8711A}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewBuilderBoundaryInterface')]
  JWebViewBuilderBoundaryInterface = interface(IJavaInstance)
    ['{940EA2D3-F2C4-4FB1-8AD4-8B1A74425DB9}']
    function build(context: JContext; consumer: JConsumer): JWebView; cdecl;
  end;
  TJWebViewBuilderBoundaryInterface = class(TJavaGenericImport<JWebViewBuilderBoundaryInterfaceClass, JWebViewBuilderBoundaryInterface>) end;

  JWebViewBuilderBoundaryInterface_BaselineClass = interface(JAnnotationClass)
    ['{91CEF447-4274-4B38-8B2B-05E651FC4F0D}']
    {class} function _GetDEFAULT: Integer; cdecl;
    {class} property DEFAULT: Integer read _GetDEFAULT;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewBuilderBoundaryInterface$Baseline')]
  JWebViewBuilderBoundaryInterface_Baseline = interface(JAnnotation)
    ['{4DFBA510-B0A2-4B5F-817A-87B25D6CDA06}']
  end;
  TJWebViewBuilderBoundaryInterface_Baseline = class(TJavaGenericImport<JWebViewBuilderBoundaryInterface_BaselineClass, JWebViewBuilderBoundaryInterface_Baseline>) end;

  JWebViewBuilderBoundaryInterface_ConfigClass = interface(JObjectClass)
    ['{D3609B95-67A1-47A4-8FB3-086128EE336A}']
    {class} function init: JWebViewBuilderBoundaryInterface_Config; cdecl;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewBuilderBoundaryInterface$Config')]
  JWebViewBuilderBoundaryInterface_Config = interface(JObject)
    ['{A31D1C0D-F9FD-42CA-9FCE-49280F043904}']
    function _Getbaseline: Integer; cdecl;
    procedure _Setbaseline(Value: Integer); cdecl;
    function _GetprofileName: JString; cdecl;
    procedure _SetprofileName(Value: JString); cdecl;
    function _GetrestrictJavascriptInterface: Boolean; cdecl;
    procedure _SetrestrictJavascriptInterface(Value: Boolean); cdecl;
    procedure accept(biConsumer: JBiConsumer); cdecl;
    procedure addJavascriptInterface(object_: JObject; string_: JString; list: JList); cdecl;
    property baseline: Integer read _Getbaseline write _Setbaseline;
    property profileName: JString read _GetprofileName write _SetprofileName;
    property restrictJavascriptInterface: Boolean read _GetrestrictJavascriptInterface write _SetrestrictJavascriptInterface;
  end;
  TJWebViewBuilderBoundaryInterface_Config = class(TJavaGenericImport<JWebViewBuilderBoundaryInterface_ConfigClass, JWebViewBuilderBoundaryInterface_Config>) end;

  JWebViewBuilderBoundaryInterface_ConfigFieldClass = interface(JAnnotationClass)
    ['{8F50727E-4C35-43EB-B5B1-B06DA99A3404}']
    {class} function _GetBASELINE: Integer; cdecl;
    {class} function _GetJAVASCRIPT_INTERFACE: Integer; cdecl;
    {class} function _GetPROFILE_NAME: Integer; cdecl;
    {class} function _GetRESTRICT_JAVASCRIPT_INTERFACE: Integer; cdecl;
    {class} property BASELINE: Integer read _GetBASELINE;
    {class} property JAVASCRIPT_INTERFACE: Integer read _GetJAVASCRIPT_INTERFACE;
    {class} property PROFILE_NAME: Integer read _GetPROFILE_NAME;
    {class} property RESTRICT_JAVASCRIPT_INTERFACE: Integer read _GetRESTRICT_JAVASCRIPT_INTERFACE;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewBuilderBoundaryInterface$ConfigField')]
  JWebViewBuilderBoundaryInterface_ConfigField = interface(JAnnotation)
    ['{FB4FD030-0B08-40B7-A342-FAB969ACD92E}']
  end;
  TJWebViewBuilderBoundaryInterface_ConfigField = class(TJavaGenericImport<JWebViewBuilderBoundaryInterface_ConfigFieldClass, JWebViewBuilderBoundaryInterface_ConfigField>) end;

  JWebViewClientBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{70EE1D7F-D6C8-47EA-957D-4D1986326A62}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewClientBoundaryInterface')]
  JWebViewClientBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{1FF85158-3498-475E-B406-AF6147409DD2}']
    procedure onPageCommitVisible(webView: JWebView; string_: JString); cdecl;
    procedure onReceivedError(webView: JWebView; webResourceRequest: JWebResourceRequest; invocationHandler: JInvocationHandler); cdecl;
    procedure onReceivedHttpError(webView: JWebView; webResourceRequest: JWebResourceRequest; webResourceResponse: JWebResourceResponse); cdecl;
    procedure onSafeBrowsingHit(webView: JWebView; webResourceRequest: JWebResourceRequest; i: Integer; invocationHandler: JInvocationHandler); cdecl;
    function shouldOverrideUrlLoading(webView: JWebView; webResourceRequest: JWebResourceRequest): Boolean; cdecl;
  end;
  TJWebViewClientBoundaryInterface = class(TJavaGenericImport<JWebViewClientBoundaryInterfaceClass, JWebViewClientBoundaryInterface>) end;

  JWebViewCookieManagerBoundaryInterfaceClass = interface(IJavaClass)
    ['{449D8520-9240-46C6-81C8-54A9F3AFBF6D}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewCookieManagerBoundaryInterface')]
  JWebViewCookieManagerBoundaryInterface = interface(IJavaInstance)
    ['{95C63E43-EA09-4513-927E-074A2B427600}']
    function getCookieInfo(string_: JString): JList; cdecl;
  end;
  TJWebViewCookieManagerBoundaryInterface = class(TJavaGenericImport<JWebViewCookieManagerBoundaryInterfaceClass, JWebViewCookieManagerBoundaryInterface>) end;

  JWebViewNavigationBoundaryInterfaceClass = interface(JIsomorphicObjectBoundaryInterfaceClass)
    ['{E90B273A-18F6-43C4-9EAE-7E6684C88B21}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewNavigationBoundaryInterface')]
  JWebViewNavigationBoundaryInterface = interface(JIsomorphicObjectBoundaryInterface)
    ['{51EA4F83-A6BB-41FC-88CD-336E11D0E26B}']
    function didCommit: Boolean; cdecl;
    function didCommitErrorPage: Boolean; cdecl;
    function getPage: JInvocationHandler; cdecl;
    function getStatusCode: Integer; cdecl;
    function getUrl: JString; cdecl;
    function isBack: Boolean; cdecl;
    function isForward: Boolean; cdecl;
    function isHistory: Boolean; cdecl;
    function isReload: Boolean; cdecl;
    function isRestore: Boolean; cdecl;
    function isSameDocument: Boolean; cdecl;
    function wasInitiatedByPage: Boolean; cdecl;
  end;
  TJWebViewNavigationBoundaryInterface = class(TJavaGenericImport<JWebViewNavigationBoundaryInterfaceClass, JWebViewNavigationBoundaryInterface>) end;

  JWebViewNavigationClientBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{1CC4E03B-F73C-4E35-BF15-BE310F4C9FA4}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewNavigationClientBoundaryInterface')]
  JWebViewNavigationClientBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{EC730CBF-1850-413C-922C-8F83F04C1DCB}']
    procedure onFirstContentfulPaint(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationCompleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationRedirected(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationStarted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDOMContentLoadedEventFired(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDeleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageLoadEventFired(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewNavigationClientBoundaryInterface = class(TJavaGenericImport<JWebViewNavigationClientBoundaryInterfaceClass, JWebViewNavigationClientBoundaryInterface>) end;

  JWebViewNavigationListenerBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{8207105F-923E-4C8B-8146-ADCBE9CCD2E1}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewNavigationListenerBoundaryInterface')]
  JWebViewNavigationListenerBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{961CF14B-1407-4A4E-8C61-C2EF2DB1DA36}']
    procedure onFirstContentfulPaint(invocationHandler: JInvocationHandler; l: Int64); cdecl;
    procedure onNavigationCompleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationRedirected(invocationHandler: JInvocationHandler); cdecl;
    procedure onNavigationStarted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDOMContentLoadedEventFired(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageDeleted(invocationHandler: JInvocationHandler); cdecl;
    procedure onPageLoadEventFired(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewNavigationListenerBoundaryInterface = class(TJavaGenericImport<JWebViewNavigationListenerBoundaryInterfaceClass, JWebViewNavigationListenerBoundaryInterface>) end;

  JWebViewPageBoundaryInterfaceClass = interface(JIsomorphicObjectBoundaryInterfaceClass)
    ['{B66E638C-3007-4522-9E78-56377CC85BF6}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewPageBoundaryInterface')]
  JWebViewPageBoundaryInterface = interface(JIsomorphicObjectBoundaryInterface)
    ['{25F18ED5-9462-4C34-A34F-24939852B7FA}']
    function isPrerendering: Boolean; cdecl;
  end;
  TJWebViewPageBoundaryInterface = class(TJavaGenericImport<JWebViewPageBoundaryInterfaceClass, JWebViewPageBoundaryInterface>) end;

  JWebViewProviderBoundaryInterfaceClass = interface(IJavaClass)
    ['{9D7091C7-560A-46A2-BF4E-0C1981AD0F50}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewProviderBoundaryInterface')]
  JWebViewProviderBoundaryInterface = interface(IJavaInstance)
    ['{4F17E2AB-1398-482A-BB92-6C8F48430ACB}']
    function addDocumentStartJavaScript(string_: JString; string_1: TJavaObjectArray<JString>): JInvocationHandler; cdecl;
    procedure addWebMessageListener(string_: JString; string_1: TJavaObjectArray<JString>; invocationHandler: JInvocationHandler); cdecl;
    procedure addWebViewNavigationListener(executor: JExecutor; invocationHandler: JInvocationHandler); cdecl;
    function createWebMessageChannel: TJavaObjectArray<JInvocationHandler>; cdecl;
    function getProfile: JInvocationHandler; cdecl;
    function getWebChromeClient: JWebChromeClient; cdecl;
    function getWebViewClient: JWebViewClient; cdecl;
    function getWebViewNavigationClient: JInvocationHandler; cdecl;
    function getWebViewRenderer: JInvocationHandler; cdecl;
    function getWebViewRendererClient: JInvocationHandler; cdecl;
    procedure insertVisualStateCallback(l: Int64; invocationHandler: JInvocationHandler); cdecl;
    function isAudioMuted: Boolean; cdecl;
    procedure postMessageToMainFrame(invocationHandler: JInvocationHandler; uri: Jnet_Uri); cdecl;
    procedure prerenderUrl(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor; valueCallback: JValueCallback;
      valueCallback1: JValueCallback); cdecl; overload;
    procedure prerenderUrl(string_: JString; cancellationSignal: JCancellationSignal; executor: JExecutor; invocationHandler: JInvocationHandler;
      valueCallback: JValueCallback; valueCallback1: JValueCallback); cdecl; overload;
    procedure removeWebMessageListener(string_: JString); cdecl;
    procedure removeWebViewNavigationListener(invocationHandler: JInvocationHandler); cdecl;
    procedure saveState(bundle: JBundle; i: Integer; b: Boolean); cdecl;
    procedure setAudioMuted(b: Boolean); cdecl;
    procedure setProfile(string_: JString); cdecl;
    procedure setWebViewNavigationClient(invocationHandler: JInvocationHandler); cdecl;
    procedure setWebViewRendererClient(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewProviderBoundaryInterface = class(TJavaGenericImport<JWebViewProviderBoundaryInterfaceClass, JWebViewProviderBoundaryInterface>) end;

  JWebViewProviderFactoryBoundaryInterfaceClass = interface(IJavaClass)
    ['{1D99FDBA-DD28-4EE2-9581-F080FCE3C547}']
    {class} function _GetMULTI_COOKIE_HEADER_NAME: JString; cdecl;
    {class} function _GetMULTI_COOKIE_VALUE_SEPARATOR: JString; cdecl;
    {class} property MULTI_COOKIE_HEADER_NAME: JString read _GetMULTI_COOKIE_HEADER_NAME;
    {class} property MULTI_COOKIE_VALUE_SEPARATOR: JString read _GetMULTI_COOKIE_VALUE_SEPARATOR;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewProviderFactoryBoundaryInterface')]
  JWebViewProviderFactoryBoundaryInterface = interface(IJavaInstance)
    ['{3D4C4BB2-6B1C-43CB-ADC7-E47DD23F42C6}']
    function createWebView(webView: JWebView): JInvocationHandler; cdecl;
    function getDropDataProvider: JInvocationHandler; cdecl;
    function getProfileStore: JInvocationHandler; cdecl;
    function getProxyController: JInvocationHandler; cdecl;
    function getServiceWorkerController: JInvocationHandler; cdecl;
    function getStatics: JInvocationHandler; cdecl;
    function getSupportedFeatures: TJavaObjectArray<JString>; cdecl;
    function getTracingController: JInvocationHandler; cdecl;
    function getWebViewBuilder: JInvocationHandler; cdecl;
    function getWebkitToCompatConverter: JInvocationHandler; cdecl;
    procedure startUpWebView(invocationHandler: JInvocationHandler; invocationHandler1: JInvocationHandler); cdecl;
  end;
  TJWebViewProviderFactoryBoundaryInterface = class(TJavaGenericImport<JWebViewProviderFactoryBoundaryInterfaceClass, JWebViewProviderFactoryBoundaryInterface>) end;

  JWebViewRendererBoundaryInterfaceClass = interface(JIsomorphicObjectBoundaryInterfaceClass)
    ['{4E4C11A6-AA7A-4AA5-9F8E-203183F3A908}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewRendererBoundaryInterface')]
  JWebViewRendererBoundaryInterface = interface(JIsomorphicObjectBoundaryInterface)
    ['{B7CFDED8-55E5-480B-BD90-76DC296F2333}']
    function terminate: Boolean; cdecl;
  end;
  TJWebViewRendererBoundaryInterface = class(TJavaGenericImport<JWebViewRendererBoundaryInterfaceClass, JWebViewRendererBoundaryInterface>) end;

  JWebViewRendererClientBoundaryInterfaceClass = interface(JFeatureFlagHolderBoundaryInterfaceClass)
    ['{85824558-8434-4A12-A67F-9554B4462402}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewRendererClientBoundaryInterface')]
  JWebViewRendererClientBoundaryInterface = interface(JFeatureFlagHolderBoundaryInterface)
    ['{AA59627C-F24C-4C7E-ADAF-999DD42EDBEB}']
    procedure onRendererResponsive(webView: JWebView; invocationHandler: JInvocationHandler); cdecl;
    procedure onRendererUnresponsive(webView: JWebView; invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewRendererClientBoundaryInterface = class(TJavaGenericImport<JWebViewRendererClientBoundaryInterfaceClass, JWebViewRendererClientBoundaryInterface>) end;

  JWebViewStartUpCallbackBoundaryInterfaceClass = interface(IJavaClass)
    ['{8968F4D1-7BF7-44B1-8752-99F474E1AD82}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewStartUpCallbackBoundaryInterface')]
  JWebViewStartUpCallbackBoundaryInterface = interface(IJavaInstance)
    ['{396D431F-5305-4071-9373-E17725947D45}']
    procedure onSuccess(invocationHandler: JInvocationHandler); cdecl;
  end;
  TJWebViewStartUpCallbackBoundaryInterface = class(TJavaGenericImport<JWebViewStartUpCallbackBoundaryInterfaceClass, JWebViewStartUpCallbackBoundaryInterface>) end;

  JWebViewStartUpConfigBoundaryInterfaceClass = interface(IJavaClass)
    ['{867720EE-8FCA-4E8F-90C6-E530DD013C7F}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewStartUpConfigBoundaryInterface')]
  JWebViewStartUpConfigBoundaryInterface = interface(IJavaInstance)
    ['{7454DD59-8DC3-4CB1-8EFA-2A65F60FFE46}']
    function getBackgroundExecutor: JExecutor; cdecl;
    function getProfileNamesToLoad: JSet; cdecl;
    function shouldRunUiThreadStartUpTasks: Boolean; cdecl;
  end;
  TJWebViewStartUpConfigBoundaryInterface = class(TJavaGenericImport<JWebViewStartUpConfigBoundaryInterfaceClass, JWebViewStartUpConfigBoundaryInterface>) end;

  JWebViewStartUpResultBoundaryInterfaceClass = interface(IJavaClass)
    ['{15627DEE-A922-4B20-A147-34EC758DED01}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebViewStartUpResultBoundaryInterface')]
  JWebViewStartUpResultBoundaryInterface = interface(IJavaInstance)
    ['{E3C4997E-50E9-481E-A385-144C8B0DA423}']
    function getAsyncStartUpLocations: JList; cdecl;
    function getBlockingStartUpLocations: JList; cdecl;
    function getMaxTimePerTaskInUiThreadMillis: JLong; cdecl;
    function getTotalTimeInUiThreadMillis: JLong; cdecl;
  end;
  TJWebViewStartUpResultBoundaryInterface = class(TJavaGenericImport<JWebViewStartUpResultBoundaryInterfaceClass, JWebViewStartUpResultBoundaryInterface>) end;

  JWebkitToCompatConverterBoundaryInterfaceClass = interface(IJavaClass)
    ['{9D2346E9-185D-432A-A55F-26FFEDC3AA22}']
  end;

  [JavaSignature('org/chromium/support_lib_boundary/WebkitToCompatConverterBoundaryInterface')]
  JWebkitToCompatConverterBoundaryInterface = interface(IJavaInstance)
    ['{D9202893-0981-43F9-A3BB-2849E1C3E0C1}']
    function convertCookieManager(object_: JObject): JInvocationHandler; cdecl;
    function convertSafeBrowsingResponse(object_: JObject): JInvocationHandler; cdecl; overload;
    function convertSafeBrowsingResponse(invocationHandler: JInvocationHandler): JObject; cdecl; overload;
    function convertServiceWorkerSettings(object_: JObject): JInvocationHandler; cdecl; overload;
    function convertServiceWorkerSettings(invocationHandler: JInvocationHandler): JObject; cdecl; overload;
    function convertSettings(webSettings: JWebSettings): JInvocationHandler; cdecl;
    function convertWebMessagePort(object_: JObject): JInvocationHandler; cdecl; overload;
    function convertWebMessagePort(invocationHandler: JInvocationHandler): JObject; cdecl; overload;
    function convertWebResourceError(object_: JObject): JInvocationHandler; cdecl; overload;
    function convertWebResourceError(invocationHandler: JInvocationHandler): JObject; cdecl; overload;
    function convertWebResourceRequest(webResourceRequest: JWebResourceRequest): JInvocationHandler; cdecl;
    function convertWebStorage(object_: JObject): JInvocationHandler; cdecl;
  end;
  TJWebkitToCompatConverterBoundaryInterface = class(TJavaGenericImport<JWebkitToCompatConverterBoundaryInterfaceClass, JWebkitToCompatConverterBoundaryInterface>) end;

  JBoundaryInterfaceReflectionUtilClass = interface(JObjectClass)
    ['{C606AB6C-6BB3-4BC2-B670-2EE6F889E378}']
    {class} function init: JBoundaryInterfaceReflectionUtil; cdecl;
    {class} function castToSuppLibClass(class_: Jlang_Class; invocationHandler: JInvocationHandler): JObject; cdecl;
    {class} function containsFeature(collection: JCollection; string_: JString): Boolean; cdecl; overload;
    {class} function containsFeature(string_: TJavaObjectArray<JString>; string_1: JString): Boolean; cdecl; overload;
    {class} function createInvocationHandlerFor(object_: JObject): JInvocationHandler; cdecl;
    {class} function createInvocationHandlersForArray(object_: TJavaObjectArray<JObject>): TJavaObjectArray<JInvocationHandler>; cdecl;
    {class} function dupeMethod(method: JMethod; classLoader: JClassLoader): JMethod; cdecl;
    {class} function getDelegateFromInvocationHandler(invocationHandler: JInvocationHandler): JObject; cdecl;
    {class} function instanceOfInOwnClassLoader(object_: JObject; string_: JString): Boolean; cdecl;
    {class} procedure setMethodCache(lruCache: JLruCache; consumer: JConsumer); cdecl;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/util/BoundaryInterfaceReflectionUtil')]
  JBoundaryInterfaceReflectionUtil = interface(JObject)
    ['{349204D1-284A-4BDD-81E7-79360816B788}']
  end;
  TJBoundaryInterfaceReflectionUtil = class(TJavaGenericImport<JBoundaryInterfaceReflectionUtilClass, JBoundaryInterfaceReflectionUtil>) end;

  JFeaturesClass = interface(JObjectClass)
    ['{3D23B392-7412-4557-9665-437329A5C2A2}']
    {class} function _GetADD_QUIC_HINTS_V1: JString; cdecl;
    {class} function _GetALGORITHMIC_DARKENING: JString; cdecl;
    {class} function _GetASYNC_WEBVIEW_STARTUP: JString; cdecl;
    {class} function _GetASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS: JString; cdecl;
    {class} function _GetATTRIBUTION_BEHAVIOR: JString; cdecl;
    {class} function _GetBACK_FORWARD_CACHE: JString; cdecl;
    {class} function _GetBACK_FORWARD_CACHE_SETTINGS: JString; cdecl;
    {class} function _GetBACK_FORWARD_CACHE_SETTINGS_V2: JString; cdecl;
    {class} function _GetCOOKIE_INTERCEPT: JString; cdecl;
    {class} function _GetCREATE_WEB_MESSAGE_CHANNEL: JString; cdecl;
    {class} function _GetCUSTOM_REQUEST_HEADERS: JString; cdecl;
    {class} function _GetDEFAULT_TRAFFICSTATS_TAGGING: JString; cdecl;
    {class} function _GetDEV_SUFFIX: JString; cdecl;
    {class} function _GetDISABLED_ACTION_MODE_MENU_ITEMS: JString; cdecl;
    {class} function _GetDOCUMENT_START_SCRIPT: JString; cdecl;
    {class} function _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JString; cdecl;
    {class} function _GetEXTRA_HEADER_FOR_ORIGINS: JString; cdecl;
    {class} function _GetFORCE_DARK: JString; cdecl;
    {class} function _GetFORCE_DARK_BEHAVIOR: JString; cdecl;
    {class} function _GetGET_COOKIE_INFO: JString; cdecl;
    {class} function _GetGET_VARIATIONS_HEADER: JString; cdecl;
    {class} function _GetGET_WEB_CHROME_CLIENT: JString; cdecl;
    {class} function _GetGET_WEB_VIEW_CLIENT: JString; cdecl;
    {class} function _GetGET_WEB_VIEW_RENDERER: JString; cdecl;
    {class} function _GetHYPERLINK_CONTEXT_MENU_ITEMS: JString; cdecl;
    {class} function _GetIMAGE_DRAG_DROP: JString; cdecl;
    {class} function _GetMULTI_PROCESS_QUERY: JString; cdecl;
    {class} function _GetMULTI_PROFILE: JString; cdecl;
    {class} function _GetMUTE_AUDIO: JString; cdecl;
    {class} function _GetOFF_SCREEN_PRERASTER: JString; cdecl;
    {class} function _GetON_NAVIGATION_COMPLETED_NON_COMMITTED: JString; cdecl;
    {class} function _GetPAGE_IS_PRERENDERING: JString; cdecl;
    {class} function _GetPAYMENT_REQUEST: JString; cdecl;
    {class} function _GetPOST_WEB_MESSAGE: JString; cdecl;
    {class} function _GetPRECONNECT: JString; cdecl;
    {class} function _GetPREFETCH_WITH_URL: JString; cdecl;
    {class} function _GetPRERENDER_WITH_URL: JString; cdecl;
    {class} function _GetPROVIDER_WEAKLY_REF_WEBVIEW: JString; cdecl;
    {class} function _GetPROXY_OVERRIDE: JString; cdecl;
    {class} function _GetPROXY_OVERRIDE_REVERSE_BYPASS: JString; cdecl;
    {class} function _GetRECEIVE_HTTP_ERROR: JString; cdecl;
    {class} function _GetRECEIVE_WEB_RESOURCE_ERROR: JString; cdecl;
    {class} function _GetRENDERER_LIBRARY_PREFETCH_MODE: JString; cdecl;
    {class} function _GetREQUESTED_WITH_HEADER_ALLOW_LIST: JString; cdecl;
    {class} function _GetREQUESTED_WITH_HEADER_CONTROL: JString; cdecl;
    {class} function _GetRESTRICT_SENSITIVE_WEB_CONTENT: JString; cdecl;
    {class} function _GetSAFE_BROWSING_ALLOWLIST: JString; cdecl;
    {class} function _GetSAFE_BROWSING_ENABLE: JString; cdecl;
    {class} function _GetSAFE_BROWSING_HIT: JString; cdecl;
    {class} function _GetSAFE_BROWSING_PRIVACY_POLICY_URL: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_PROCEED: JString; cdecl;
    {class} function _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JString; cdecl;
    {class} function _GetSAFE_BROWSING_WHITELIST: JString; cdecl;
    {class} function _GetSAVE_STATE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_BASIC_USAGE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_CACHE_MODE: JString; cdecl;
    {class} function _GetSERVICE_WORKER_CONTENT_ACCESS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_FILE_ACCESS: JString; cdecl;
    {class} function _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JString; cdecl;
    {class} function _GetSHOULD_OVERRIDE_WITH_REDIRECTS: JString; cdecl;
    {class} function _GetSPECULATIVE_LOADING: JString; cdecl;
    {class} function _GetSPECULATIVE_LOADING_CONFIG: JString; cdecl;
    {class} function _GetSTART_SAFE_BROWSING: JString; cdecl;
    {class} function _GetSUPPRESS_ERROR_PAGE: JString; cdecl;
    {class} function _GetTRACING_CONTROLLER_BASIC_USAGE: JString; cdecl;
    {class} function _GetUSER_AGENT_METADATA: JString; cdecl;
    {class} function _GetVISUAL_STATE_CALLBACK: JString; cdecl;
    {class} function _GetWARM_UP_RENDERER_PROCESS: JString; cdecl;
    {class} function _GetWEBVIEW_BUILDER: JString; cdecl;
    {class} function _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS: JString; cdecl;
    {class} function _GetWEB_AUTHENTICATION: JString; cdecl;
    {class} function _GetWEB_MESSAGE_ARRAY_BUFFER: JString; cdecl;
    {class} function _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_GET_MESSAGE_PAYLOAD: JString; cdecl;
    {class} function _GetWEB_MESSAGE_LISTENER: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_CLOSE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_POST_MESSAGE: JString; cdecl;
    {class} function _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JString; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_CODE: JString; cdecl;
    {class} function _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION: JString; cdecl;
    {class} function _GetWEB_RESOURCE_REQUEST_IS_REDIRECT: JString; cdecl;
    {class} function _GetWEB_STORAGE_DELETE_BROWSING_DATA: JString; cdecl;
    {class} function _GetWEB_VIEW_NAVIGATION_CLIENT_BASIC_USAGE: JString; cdecl;
    {class} function _GetWEB_VIEW_NAVIGATION_LISTENER_V1: JString; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JString; cdecl;
    {class} function _GetWEB_VIEW_RENDERER_TERMINATE: JString; cdecl;
    {class} property ADD_QUIC_HINTS_V1: JString read _GetADD_QUIC_HINTS_V1;
    {class} property ALGORITHMIC_DARKENING: JString read _GetALGORITHMIC_DARKENING;
    {class} property ASYNC_WEBVIEW_STARTUP: JString read _GetASYNC_WEBVIEW_STARTUP;
    {class} property ASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS: JString read _GetASYNC_WEBVIEW_STARTUP_ASYNC_STARTUP_LOCATIONS;
    {class} property ATTRIBUTION_BEHAVIOR: JString read _GetATTRIBUTION_BEHAVIOR;
    {class} property BACK_FORWARD_CACHE: JString read _GetBACK_FORWARD_CACHE;
    {class} property BACK_FORWARD_CACHE_SETTINGS: JString read _GetBACK_FORWARD_CACHE_SETTINGS;
    {class} property BACK_FORWARD_CACHE_SETTINGS_V2: JString read _GetBACK_FORWARD_CACHE_SETTINGS_V2;
    {class} property COOKIE_INTERCEPT: JString read _GetCOOKIE_INTERCEPT;
    {class} property CREATE_WEB_MESSAGE_CHANNEL: JString read _GetCREATE_WEB_MESSAGE_CHANNEL;
    {class} property CUSTOM_REQUEST_HEADERS: JString read _GetCUSTOM_REQUEST_HEADERS;
    {class} property DEFAULT_TRAFFICSTATS_TAGGING: JString read _GetDEFAULT_TRAFFICSTATS_TAGGING;
    {class} property DEV_SUFFIX: JString read _GetDEV_SUFFIX;
    {class} property DISABLED_ACTION_MODE_MENU_ITEMS: JString read _GetDISABLED_ACTION_MODE_MENU_ITEMS;
    {class} property DOCUMENT_START_SCRIPT: JString read _GetDOCUMENT_START_SCRIPT;
    {class} property ENTERPRISE_AUTHENTICATION_APP_LINK_POLICY: JString read _GetENTERPRISE_AUTHENTICATION_APP_LINK_POLICY;
    {class} property EXTRA_HEADER_FOR_ORIGINS: JString read _GetEXTRA_HEADER_FOR_ORIGINS;
    {class} property FORCE_DARK: JString read _GetFORCE_DARK;
    {class} property FORCE_DARK_BEHAVIOR: JString read _GetFORCE_DARK_BEHAVIOR;
    {class} property GET_COOKIE_INFO: JString read _GetGET_COOKIE_INFO;
    {class} property GET_VARIATIONS_HEADER: JString read _GetGET_VARIATIONS_HEADER;
    {class} property GET_WEB_CHROME_CLIENT: JString read _GetGET_WEB_CHROME_CLIENT;
    {class} property GET_WEB_VIEW_CLIENT: JString read _GetGET_WEB_VIEW_CLIENT;
    {class} property GET_WEB_VIEW_RENDERER: JString read _GetGET_WEB_VIEW_RENDERER;
    {class} property HYPERLINK_CONTEXT_MENU_ITEMS: JString read _GetHYPERLINK_CONTEXT_MENU_ITEMS;
    {class} property IMAGE_DRAG_DROP: JString read _GetIMAGE_DRAG_DROP;
    {class} property MULTI_PROCESS_QUERY: JString read _GetMULTI_PROCESS_QUERY;
    {class} property MULTI_PROFILE: JString read _GetMULTI_PROFILE;
    {class} property MUTE_AUDIO: JString read _GetMUTE_AUDIO;
    {class} property OFF_SCREEN_PRERASTER: JString read _GetOFF_SCREEN_PRERASTER;
    {class} property ON_NAVIGATION_COMPLETED_NON_COMMITTED: JString read _GetON_NAVIGATION_COMPLETED_NON_COMMITTED;
    {class} property PAGE_IS_PRERENDERING: JString read _GetPAGE_IS_PRERENDERING;
    {class} property PAYMENT_REQUEST: JString read _GetPAYMENT_REQUEST;
    {class} property POST_WEB_MESSAGE: JString read _GetPOST_WEB_MESSAGE;
    {class} property PRECONNECT: JString read _GetPRECONNECT;
    {class} property PREFETCH_WITH_URL: JString read _GetPREFETCH_WITH_URL;
    {class} property PRERENDER_WITH_URL: JString read _GetPRERENDER_WITH_URL;
    {class} property PROVIDER_WEAKLY_REF_WEBVIEW: JString read _GetPROVIDER_WEAKLY_REF_WEBVIEW;
    {class} property PROXY_OVERRIDE: JString read _GetPROXY_OVERRIDE;
    {class} property PROXY_OVERRIDE_REVERSE_BYPASS: JString read _GetPROXY_OVERRIDE_REVERSE_BYPASS;
    {class} property RECEIVE_HTTP_ERROR: JString read _GetRECEIVE_HTTP_ERROR;
    {class} property RECEIVE_WEB_RESOURCE_ERROR: JString read _GetRECEIVE_WEB_RESOURCE_ERROR;
    {class} property RENDERER_LIBRARY_PREFETCH_MODE: JString read _GetRENDERER_LIBRARY_PREFETCH_MODE;
    {class} property REQUESTED_WITH_HEADER_ALLOW_LIST: JString read _GetREQUESTED_WITH_HEADER_ALLOW_LIST;
    {class} property REQUESTED_WITH_HEADER_CONTROL: JString read _GetREQUESTED_WITH_HEADER_CONTROL;
    {class} property RESTRICT_SENSITIVE_WEB_CONTENT: JString read _GetRESTRICT_SENSITIVE_WEB_CONTENT;
    {class} property SAFE_BROWSING_ALLOWLIST: JString read _GetSAFE_BROWSING_ALLOWLIST;
    {class} property SAFE_BROWSING_ENABLE: JString read _GetSAFE_BROWSING_ENABLE;
    {class} property SAFE_BROWSING_HIT: JString read _GetSAFE_BROWSING_HIT;
    {class} property SAFE_BROWSING_PRIVACY_POLICY_URL: JString read _GetSAFE_BROWSING_PRIVACY_POLICY_URL;
    {class} property SAFE_BROWSING_RESPONSE_BACK_TO_SAFETY: JString read _GetSAFE_BROWSING_RESPONSE_BACK_TO_SAFETY;
    {class} property SAFE_BROWSING_RESPONSE_PROCEED: JString read _GetSAFE_BROWSING_RESPONSE_PROCEED;
    {class} property SAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL: JString read _GetSAFE_BROWSING_RESPONSE_SHOW_INTERSTITIAL;
    {class} property SAFE_BROWSING_WHITELIST: JString read _GetSAFE_BROWSING_WHITELIST;
    {class} property SAVE_STATE: JString read _GetSAVE_STATE;
    {class} property SERVICE_WORKER_BASIC_USAGE: JString read _GetSERVICE_WORKER_BASIC_USAGE;
    {class} property SERVICE_WORKER_BLOCK_NETWORK_LOADS: JString read _GetSERVICE_WORKER_BLOCK_NETWORK_LOADS;
    {class} property SERVICE_WORKER_CACHE_MODE: JString read _GetSERVICE_WORKER_CACHE_MODE;
    {class} property SERVICE_WORKER_CONTENT_ACCESS: JString read _GetSERVICE_WORKER_CONTENT_ACCESS;
    {class} property SERVICE_WORKER_FILE_ACCESS: JString read _GetSERVICE_WORKER_FILE_ACCESS;
    {class} property SERVICE_WORKER_SHOULD_INTERCEPT_REQUEST: JString read _GetSERVICE_WORKER_SHOULD_INTERCEPT_REQUEST;
    {class} property SHOULD_OVERRIDE_WITH_REDIRECTS: JString read _GetSHOULD_OVERRIDE_WITH_REDIRECTS;
    {class} property SPECULATIVE_LOADING: JString read _GetSPECULATIVE_LOADING;
    {class} property SPECULATIVE_LOADING_CONFIG: JString read _GetSPECULATIVE_LOADING_CONFIG;
    {class} property START_SAFE_BROWSING: JString read _GetSTART_SAFE_BROWSING;
    {class} property SUPPRESS_ERROR_PAGE: JString read _GetSUPPRESS_ERROR_PAGE;
    {class} property TRACING_CONTROLLER_BASIC_USAGE: JString read _GetTRACING_CONTROLLER_BASIC_USAGE;
    {class} property USER_AGENT_METADATA: JString read _GetUSER_AGENT_METADATA;
    {class} property VISUAL_STATE_CALLBACK: JString read _GetVISUAL_STATE_CALLBACK;
    {class} property WARM_UP_RENDERER_PROCESS: JString read _GetWARM_UP_RENDERER_PROCESS;
    {class} property WEBVIEW_BUILDER: JString read _GetWEBVIEW_BUILDER;
    {class} property WEBVIEW_MEDIA_INTEGRITY_API_STATUS: JString read _GetWEBVIEW_MEDIA_INTEGRITY_API_STATUS;
    {class} property WEB_AUTHENTICATION: JString read _GetWEB_AUTHENTICATION;
    {class} property WEB_MESSAGE_ARRAY_BUFFER: JString read _GetWEB_MESSAGE_ARRAY_BUFFER;
    {class} property WEB_MESSAGE_CALLBACK_ON_MESSAGE: JString read _GetWEB_MESSAGE_CALLBACK_ON_MESSAGE;
    {class} property WEB_MESSAGE_GET_MESSAGE_PAYLOAD: JString read _GetWEB_MESSAGE_GET_MESSAGE_PAYLOAD;
    {class} property WEB_MESSAGE_LISTENER: JString read _GetWEB_MESSAGE_LISTENER;
    {class} property WEB_MESSAGE_PORT_CLOSE: JString read _GetWEB_MESSAGE_PORT_CLOSE;
    {class} property WEB_MESSAGE_PORT_POST_MESSAGE: JString read _GetWEB_MESSAGE_PORT_POST_MESSAGE;
    {class} property WEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK: JString read _GetWEB_MESSAGE_PORT_SET_MESSAGE_CALLBACK;
    {class} property WEB_RESOURCE_ERROR_GET_CODE: JString read _GetWEB_RESOURCE_ERROR_GET_CODE;
    {class} property WEB_RESOURCE_ERROR_GET_DESCRIPTION: JString read _GetWEB_RESOURCE_ERROR_GET_DESCRIPTION;
    {class} property WEB_RESOURCE_REQUEST_IS_REDIRECT: JString read _GetWEB_RESOURCE_REQUEST_IS_REDIRECT;
    {class} property WEB_STORAGE_DELETE_BROWSING_DATA: JString read _GetWEB_STORAGE_DELETE_BROWSING_DATA;
    {class} property WEB_VIEW_NAVIGATION_CLIENT_BASIC_USAGE: JString read _GetWEB_VIEW_NAVIGATION_CLIENT_BASIC_USAGE;
    {class} property WEB_VIEW_NAVIGATION_LISTENER_V1: JString read _GetWEB_VIEW_NAVIGATION_LISTENER_V1;
    {class} property WEB_VIEW_RENDERER_CLIENT_BASIC_USAGE: JString read _GetWEB_VIEW_RENDERER_CLIENT_BASIC_USAGE;
    {class} property WEB_VIEW_RENDERER_TERMINATE: JString read _GetWEB_VIEW_RENDERER_TERMINATE;
  end;

  [JavaSignature('org/chromium/support_lib_boundary/util/Features')]
  JFeatures = interface(JObject)
    ['{509BE426-D23D-4B94-A114-AB39E5B55ACB}']
  end;
  TJFeatures = class(TJavaGenericImport<JFeaturesClass, JFeatures>) end;

implementation

end.
