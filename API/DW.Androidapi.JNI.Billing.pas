unit DW.Androidapi.JNI.Billing;

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
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os, Androidapi.JNI.Util;

type
  Jbillingclient_BuildConfig = interface;
  JAccountIdentifiers = interface;
  JAcknowledgePurchaseParams = interface;
  JAcknowledgePurchaseParams_Builder = interface;
  JAcknowledgePurchaseResponseListener = interface;
  JBillingClient = interface;
  JBillingClient_BillingResponseCode = interface;
  JBillingClient_Builder = interface;
  JBillingClient_FeatureType = interface;
  JBillingClient_SkuType = interface;
  JBillingClientImpl = interface;
  JBillingClientStateListener = interface;
  JBillingFlowParams = interface;
  JBillingFlowParams_Builder = interface;
  JBillingFlowParams_ProrationMode = interface;
  JBillingResult = interface;
  JBillingResult_Builder = interface;
  JConsumeParams = interface;
  JConsumeParams_Builder = interface;
  JConsumeResponseListener = interface;
  JPriceChangeConfirmationListener = interface;
  JPriceChangeFlowParams = interface;
  JPriceChangeFlowParams_Builder = interface;
  JProxyBillingActivity = interface;
  JPurchase = interface;
  JPurchase_PurchaseState = interface;
  JPurchase_PurchasesResult = interface;
  JPurchaseHistoryRecord = interface;
  JPurchaseHistoryResponseListener = interface;
  JPurchasesUpdatedListener = interface;
  JSkuDetails = interface;
  JSkuDetailsParams = interface;
  JSkuDetailsParams_Builder = interface;
  JSkuDetailsResponseListener = interface;

  Jbillingclient_BuildConfigClass = interface(JObjectClass)
    ['{C8231255-5EDC-4869-BC3D-6C6358BF6E61}']
    {class} function _GetAPPLICATION_ID: JString; cdecl;
    {class} function _GetVERSION_NAME: JString; cdecl;
    {class} function init: Jbillingclient_BuildConfig; cdecl;
    {class} property APPLICATION_ID: JString read _GetAPPLICATION_ID;
    {class} property VERSION_NAME: JString read _GetVERSION_NAME;
  end;

  [JavaSignature('com/android/billingclient/BuildConfig')]
  Jbillingclient_BuildConfig = interface(JObject)
    ['{34F57370-E492-4AA5-AF8C-6B0C07A057DA}']
  end;
  TJbillingclient_BuildConfig = class(TJavaGenericImport<Jbillingclient_BuildConfigClass, Jbillingclient_BuildConfig>) end;

  JAccountIdentifiersClass = interface(JObjectClass)
    ['{A59C2E59-6E7A-4D74-ADA1-901F697E89B5}']
  end;

  [JavaSignature('com/android/billingclient/api/AccountIdentifiers')]
  JAccountIdentifiers = interface(JObject)
    ['{76D9AF10-25DC-40E7-A0BD-3A3504E64C72}']
    function getObfuscatedAccountId: JString; cdecl;
    function getObfuscatedProfileId: JString; cdecl;
  end;
  TJAccountIdentifiers = class(TJavaGenericImport<JAccountIdentifiersClass, JAccountIdentifiers>) end;

  JAcknowledgePurchaseParamsClass = interface(JObjectClass)
    ['{EACE3BD6-7A8D-4E97-9A06-5F3908436FCB}']
    {class} function newBuilder: JAcknowledgePurchaseParams_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseParams')]
  JAcknowledgePurchaseParams = interface(JObject)
    ['{AB430116-6FDA-4F48-A551-7BDFED00EE23}']
    function getPurchaseToken: JString; cdecl;
  end;
  TJAcknowledgePurchaseParams = class(TJavaGenericImport<JAcknowledgePurchaseParamsClass, JAcknowledgePurchaseParams>) end;

  JAcknowledgePurchaseParams_BuilderClass = interface(JObjectClass)
    ['{B82636C2-2FF5-443A-AA4E-B3159AC7AB42}']
  end;

  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseParams$Builder')]
  JAcknowledgePurchaseParams_Builder = interface(JObject)
    ['{D418A8A0-41EF-42C1-BC29-17FD1431D13D}']
    function build: JAcknowledgePurchaseParams; cdecl;
    function setPurchaseToken(string_: JString): JAcknowledgePurchaseParams_Builder; cdecl;
  end;
  TJAcknowledgePurchaseParams_Builder = class(TJavaGenericImport<JAcknowledgePurchaseParams_BuilderClass, JAcknowledgePurchaseParams_Builder>) end;

  JAcknowledgePurchaseResponseListenerClass = interface(IJavaClass)
    ['{CEFE1F25-CD2A-43F9-81A6-8DB399DEE51B}']
  end;

  [JavaSignature('com/android/billingclient/api/AcknowledgePurchaseResponseListener')]
  JAcknowledgePurchaseResponseListener = interface(IJavaInstance)
    ['{33D63561-4D42-4E76-806D-1AB0AC37C2D0}']
    procedure onAcknowledgePurchaseResponse(billingResult: JBillingResult); cdecl;
  end;
  TJAcknowledgePurchaseResponseListener = class(TJavaGenericImport<JAcknowledgePurchaseResponseListenerClass, JAcknowledgePurchaseResponseListener>) end;

  JBillingClientClass = interface(JObjectClass)
    ['{9952204A-4163-432C-92E4-065322838025}']
    {class} function init: JBillingClient; cdecl;
    {class} function newBuilder(context: JContext): JBillingClient_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/BillingClient')]
  JBillingClient = interface(JObject)
    ['{6C358167-958C-4CF1-B1DC-6020EF7C4B44}']
    procedure acknowledgePurchase(acknowledgePurchaseParams: JAcknowledgePurchaseParams; acknowledgePurchaseResponseListener: JAcknowledgePurchaseResponseListener); cdecl;
    procedure consumeAsync(consumeParams: JConsumeParams; consumeResponseListener: JConsumeResponseListener); cdecl;
    procedure endConnection; cdecl;
    function isFeatureSupported(string_: JString): JBillingResult; cdecl;
    function isReady: Boolean; cdecl;
    function launchBillingFlow(activity: JActivity; billingFlowParams: JBillingFlowParams): JBillingResult; cdecl;
    procedure launchPriceChangeConfirmationFlow(activity: JActivity; priceChangeFlowParams: JPriceChangeFlowParams; priceChangeConfirmationListener: JPriceChangeConfirmationListener); cdecl;
    procedure queryPurchaseHistoryAsync(string_: JString; purchaseHistoryResponseListener: JPurchaseHistoryResponseListener); cdecl;
    function queryPurchases(string_: JString): JPurchase_PurchasesResult; cdecl;
    procedure querySkuDetailsAsync(skuDetailsParams: JSkuDetailsParams; skuDetailsResponseListener: JSkuDetailsResponseListener); cdecl;
    procedure startConnection(billingClientStateListener: JBillingClientStateListener); cdecl;
  end;
  TJBillingClient = class(TJavaGenericImport<JBillingClientClass, JBillingClient>) end;

  JBillingClient_BillingResponseCodeClass = interface(JAnnotationClass)
    ['{A27CEAC7-02C8-42F4-9753-69F8EF375F50}']
    {class} function _GetBILLING_UNAVAILABLE: Integer; cdecl;
    {class} function _GetDEVELOPER_ERROR: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetFEATURE_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetITEM_ALREADY_OWNED: Integer; cdecl;
    {class} function _GetITEM_NOT_OWNED: Integer; cdecl;
    {class} function _GetITEM_UNAVAILABLE: Integer; cdecl;
    {class} function _GetOK: Integer; cdecl;
    {class} function _GetSERVICE_DISCONNECTED: Integer; cdecl;
    {class} function _GetSERVICE_TIMEOUT: Integer; cdecl;
    {class} function _GetSERVICE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetUSER_CANCELED: Integer; cdecl;
    {class} property BILLING_UNAVAILABLE: Integer read _GetBILLING_UNAVAILABLE;
    {class} property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR;
    {class} property ERROR: Integer read _GetERROR;
    {class} property FEATURE_NOT_SUPPORTED: Integer read _GetFEATURE_NOT_SUPPORTED;
    {class} property ITEM_ALREADY_OWNED: Integer read _GetITEM_ALREADY_OWNED;
    {class} property ITEM_NOT_OWNED: Integer read _GetITEM_NOT_OWNED;
    {class} property ITEM_UNAVAILABLE: Integer read _GetITEM_UNAVAILABLE;
    {class} property OK: Integer read _GetOK;
    {class} property SERVICE_DISCONNECTED: Integer read _GetSERVICE_DISCONNECTED;
    {class} property SERVICE_TIMEOUT: Integer read _GetSERVICE_TIMEOUT;
    {class} property SERVICE_UNAVAILABLE: Integer read _GetSERVICE_UNAVAILABLE;
    {class} property USER_CANCELED: Integer read _GetUSER_CANCELED;
  end;

  [JavaSignature('com/android/billingclient/api/BillingClient$BillingResponseCode')]
  JBillingClient_BillingResponseCode = interface(JAnnotation)
    ['{F4CC536E-C5E4-481B-B737-42D762034812}']
  end;
  TJBillingClient_BillingResponseCode = class(TJavaGenericImport<JBillingClient_BillingResponseCodeClass, JBillingClient_BillingResponseCode>) end;

  JBillingClient_BuilderClass = interface(JObjectClass)
    ['{E4BD2A29-6B9D-4079-812A-5C53DD929663}']
  end;

  [JavaSignature('com/android/billingclient/api/BillingClient$Builder')]
  JBillingClient_Builder = interface(JObject)
    ['{65BD7CEC-54DD-4FE5-BAC2-CB134337A615}']
    function build: JBillingClient; cdecl;
    function enablePendingPurchases: JBillingClient_Builder; cdecl;
    function setListener(purchasesUpdatedListener: JPurchasesUpdatedListener): JBillingClient_Builder; cdecl;
  end;
  TJBillingClient_Builder = class(TJavaGenericImport<JBillingClient_BuilderClass, JBillingClient_Builder>) end;

  JBillingClient_FeatureTypeClass = interface(JAnnotationClass)
    ['{C93FA776-DEE2-4152-BE8B-04BBF0D248BB}']
    {class} function _GetIN_APP_ITEMS_ON_VR: JString; cdecl;
    {class} function _GetPRICE_CHANGE_CONFIRMATION: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS_ON_VR: JString; cdecl;
    {class} function _GetSUBSCRIPTIONS_UPDATE: JString; cdecl;
    {class} property IN_APP_ITEMS_ON_VR: JString read _GetIN_APP_ITEMS_ON_VR;
    {class} property PRICE_CHANGE_CONFIRMATION: JString read _GetPRICE_CHANGE_CONFIRMATION;
    {class} property SUBSCRIPTIONS: JString read _GetSUBSCRIPTIONS;
    {class} property SUBSCRIPTIONS_ON_VR: JString read _GetSUBSCRIPTIONS_ON_VR;
    {class} property SUBSCRIPTIONS_UPDATE: JString read _GetSUBSCRIPTIONS_UPDATE;
  end;

  [JavaSignature('com/android/billingclient/api/BillingClient$FeatureType')]
  JBillingClient_FeatureType = interface(JAnnotation)
    ['{CCC364EB-5FBC-4471-B6C3-C798FA829AFD}']
  end;
  TJBillingClient_FeatureType = class(TJavaGenericImport<JBillingClient_FeatureTypeClass, JBillingClient_FeatureType>) end;

  JBillingClient_SkuTypeClass = interface(JAnnotationClass)
    ['{479B9F58-CB7D-402E-A256-414CBE69CF49}']
    {class} function _GetINAPP: JString; cdecl;
    {class} function _GetSUBS: JString; cdecl;
    {class} property INAPP: JString read _GetINAPP;
    {class} property SUBS: JString read _GetSUBS;
  end;

  [JavaSignature('com/android/billingclient/api/BillingClient$SkuType')]
  JBillingClient_SkuType = interface(JAnnotation)
    ['{C364C1E1-1204-40F3-BCE0-1DBB463F4B73}']
  end;
  TJBillingClient_SkuType = class(TJavaGenericImport<JBillingClient_SkuTypeClass, JBillingClient_SkuType>) end;

  JBillingClientImplClass = interface(JBillingClientClass)
    ['{349BD5E1-9836-4797-97F7-A5B652C36290}']
  end;

  [JavaSignature('com/android/billingclient/api/BillingClientImpl')]
  JBillingClientImpl = interface(JBillingClient)
    ['{28C11DB1-69AD-4B80-9CBA-CF5F46C4C0D0}']
    procedure acknowledgePurchase(acknowledgePurchaseParams: JAcknowledgePurchaseParams; acknowledgePurchaseResponseListener: JAcknowledgePurchaseResponseListener); cdecl;
    procedure consumeAsync(consumeParams: JConsumeParams; consumeResponseListener: JConsumeResponseListener); cdecl;
    procedure endConnection; cdecl;
    function isFeatureSupported(string_: JString): JBillingResult; cdecl;
    function isReady: Boolean; cdecl;
    function launchBillingFlow(activity: JActivity; billingFlowParams: JBillingFlowParams): JBillingResult; cdecl;
    procedure launchPriceChangeConfirmationFlow(activity: JActivity; priceChangeFlowParams: JPriceChangeFlowParams; priceChangeConfirmationListener: JPriceChangeConfirmationListener); cdecl;
    procedure queryPurchaseHistoryAsync(string_: JString; purchaseHistoryResponseListener: JPurchaseHistoryResponseListener); cdecl;
    function queryPurchases(string_: JString): JPurchase_PurchasesResult; cdecl;
    procedure querySkuDetailsAsync(skuDetailsParams: JSkuDetailsParams; skuDetailsResponseListener: JSkuDetailsResponseListener); cdecl;
    procedure startConnection(billingClientStateListener: JBillingClientStateListener); cdecl;
  end;
  TJBillingClientImpl = class(TJavaGenericImport<JBillingClientImplClass, JBillingClientImpl>) end;

  JBillingClientStateListenerClass = interface(IJavaClass)
    ['{377D4EBF-C3F6-46D7-A8FB-630B487ACB49}']
  end;

  [JavaSignature('com/android/billingclient/api/BillingClientStateListener')]
  JBillingClientStateListener = interface(IJavaInstance)
    ['{B8F40932-1BAA-4B21-8859-2BA5B99571BF}']
    procedure onBillingServiceDisconnected; cdecl;
    procedure onBillingSetupFinished(billingResult: JBillingResult); cdecl;
  end;
  TJBillingClientStateListener = class(TJavaGenericImport<JBillingClientStateListenerClass, JBillingClientStateListener>) end;

  JBillingFlowParamsClass = interface(JObjectClass)
    ['{D13F2ECA-D784-46C6-92FD-F89E5937FEC0}']
    {class} function _GetEXTRA_PARAM_KEY_ACCOUNT_ID: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_OLD_SKUS: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE: JString; cdecl;
    {class} function _GetEXTRA_PARAM_KEY_VR: JString; cdecl;
    {class} function newBuilder: JBillingFlowParams_Builder; cdecl;
    {class} property EXTRA_PARAM_KEY_ACCOUNT_ID: JString read _GetEXTRA_PARAM_KEY_ACCOUNT_ID;
    {class} property EXTRA_PARAM_KEY_OLD_SKUS: JString read _GetEXTRA_PARAM_KEY_OLD_SKUS;
    {class} property EXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN: JString read _GetEXTRA_PARAM_KEY_OLD_SKU_PURCHASE_TOKEN;
    {class} property EXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE: JString read _GetEXTRA_PARAM_KEY_REPLACE_SKUS_PRORATION_MODE;
    {class} property EXTRA_PARAM_KEY_VR: JString read _GetEXTRA_PARAM_KEY_VR;
  end;

  [JavaSignature('com/android/billingclient/api/BillingFlowParams')]
  JBillingFlowParams = interface(JObject)
    ['{0A4B7AC3-2F63-4B2E-A96B-FCA0D9FC349F}']
    function getOldSku: JString; cdecl;
    function getOldSkuPurchaseToken: JString; cdecl;
    function getReplaceSkusProrationMode: Integer; cdecl;
    function getSku: JString; cdecl;
    function getSkuDetails: JSkuDetails; cdecl;
    function getSkuType: JString; cdecl;
    function getVrPurchaseFlow: Boolean; cdecl;
  end;
  TJBillingFlowParams = class(TJavaGenericImport<JBillingFlowParamsClass, JBillingFlowParams>) end;

  JBillingFlowParams_BuilderClass = interface(JObjectClass)
    ['{E298BE18-49A2-46F2-BA82-114652E97983}']
  end;

  [JavaSignature('com/android/billingclient/api/BillingFlowParams$Builder')]
  JBillingFlowParams_Builder = interface(JObject)
    ['{18CA6FF0-9AFF-4B47-9BE9-8F37EC1C13CF}']
    function build: JBillingFlowParams; cdecl;
    function setObfuscatedAccountId(string_: JString): JBillingFlowParams_Builder; cdecl;
    function setObfuscatedProfileId(string_: JString): JBillingFlowParams_Builder; cdecl;
    function setOldSku(string_: JString; string_1: JString): JBillingFlowParams_Builder; cdecl;
    function setReplaceSkusProrationMode(i: Integer): JBillingFlowParams_Builder; cdecl;
    function setSkuDetails(skuDetails: JSkuDetails): JBillingFlowParams_Builder; cdecl;
    function setVrPurchaseFlow(b: Boolean): JBillingFlowParams_Builder; cdecl;
  end;
  TJBillingFlowParams_Builder = class(TJavaGenericImport<JBillingFlowParams_BuilderClass, JBillingFlowParams_Builder>) end;

  JBillingFlowParams_ProrationModeClass = interface(JAnnotationClass)
    ['{32B58E25-DA30-49C8-AB22-4359ADDCB581}']
    {class} function _GetDEFERRED: Integer; cdecl;
    {class} function _GetIMMEDIATE_AND_CHARGE_PRORATED_PRICE: Integer; cdecl;
    {class} function _GetIMMEDIATE_WITHOUT_PRORATION: Integer; cdecl;
    {class} function _GetIMMEDIATE_WITH_TIME_PRORATION: Integer; cdecl;
    {class} function _GetUNKNOWN_SUBSCRIPTION_UPGRADE_DOWNGRADE_POLICY: Integer; cdecl;
    {class} property DEFERRED: Integer read _GetDEFERRED;
    {class} property IMMEDIATE_AND_CHARGE_PRORATED_PRICE: Integer read _GetIMMEDIATE_AND_CHARGE_PRORATED_PRICE;
    {class} property IMMEDIATE_WITHOUT_PRORATION: Integer read _GetIMMEDIATE_WITHOUT_PRORATION;
    {class} property IMMEDIATE_WITH_TIME_PRORATION: Integer read _GetIMMEDIATE_WITH_TIME_PRORATION;
    {class} property UNKNOWN_SUBSCRIPTION_UPGRADE_DOWNGRADE_POLICY: Integer read _GetUNKNOWN_SUBSCRIPTION_UPGRADE_DOWNGRADE_POLICY;
  end;

  [JavaSignature('com/android/billingclient/api/BillingFlowParams$ProrationMode')]
  JBillingFlowParams_ProrationMode = interface(JAnnotation)
    ['{35FBF44A-306C-4C7A-B61F-B96AF4E2333E}']
  end;
  TJBillingFlowParams_ProrationMode = class(TJavaGenericImport<JBillingFlowParams_ProrationModeClass, JBillingFlowParams_ProrationMode>) end;

  JBillingResultClass = interface(JObjectClass)
    ['{298535BD-0F4D-4086-9811-EDEADB32D89B}']
    {class} function init: JBillingResult; cdecl;
    {class} function newBuilder: JBillingResult_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/BillingResult')]
  JBillingResult = interface(JObject)
    ['{A3CEE490-1517-4A2D-AB92-92D3A0A87985}']
    function getDebugMessage: JString; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJBillingResult = class(TJavaGenericImport<JBillingResultClass, JBillingResult>) end;

  JBillingResult_BuilderClass = interface(JObjectClass)
    ['{CEA0997A-A70D-439F-9D1F-28AAF985071E}']
  end;

  [JavaSignature('com/android/billingclient/api/BillingResult$Builder')]
  JBillingResult_Builder = interface(JObject)
    ['{D9F9281B-3608-4347-88F4-375E90A93CBB}']
    function build: JBillingResult; cdecl;
    function setDebugMessage(string_: JString): JBillingResult_Builder; cdecl;
    function setResponseCode(i: Integer): JBillingResult_Builder; cdecl;
  end;
  TJBillingResult_Builder = class(TJavaGenericImport<JBillingResult_BuilderClass, JBillingResult_Builder>) end;

  JConsumeParamsClass = interface(JObjectClass)
    ['{EBE277CF-1F38-46D2-BEAF-4FDBC5D6992A}']
    {class} function newBuilder: JConsumeParams_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/ConsumeParams')]
  JConsumeParams = interface(JObject)
    ['{C6EE280D-301C-4765-9528-B293BBEF40DC}']
    function getPurchaseToken: JString; cdecl;
  end;
  TJConsumeParams = class(TJavaGenericImport<JConsumeParamsClass, JConsumeParams>) end;

  JConsumeParams_BuilderClass = interface(JObjectClass)
    ['{9AB2CA2C-B385-4F3F-86CD-14D4380D83AF}']
  end;

  [JavaSignature('com/android/billingclient/api/ConsumeParams$Builder')]
  JConsumeParams_Builder = interface(JObject)
    ['{A8D78B2E-9F8B-49D6-BA4D-E97031E25B4C}']
    function build: JConsumeParams; cdecl;
    function setPurchaseToken(string_: JString): JConsumeParams_Builder; cdecl;
  end;
  TJConsumeParams_Builder = class(TJavaGenericImport<JConsumeParams_BuilderClass, JConsumeParams_Builder>) end;

  JConsumeResponseListenerClass = interface(IJavaClass)
    ['{55A036A6-0380-471D-99B6-102755FA93E2}']
  end;

  [JavaSignature('com/android/billingclient/api/ConsumeResponseListener')]
  JConsumeResponseListener = interface(IJavaInstance)
    ['{C4686AC5-451C-4EA0-8ACB-92FD4A9BD15A}']
    procedure onConsumeResponse(billingResult: JBillingResult; purchaseToken: JString); cdecl;
  end;
  TJConsumeResponseListener = class(TJavaGenericImport<JConsumeResponseListenerClass, JConsumeResponseListener>) end;

  JPriceChangeConfirmationListenerClass = interface(IJavaClass)
    ['{E95EB189-0542-4D64-AF0D-DDE7A7BC66DB}']
  end;

  [JavaSignature('com/android/billingclient/api/PriceChangeConfirmationListener')]
  JPriceChangeConfirmationListener = interface(IJavaInstance)
    ['{41163E1C-A5A6-410F-BC32-E7CF9E02B042}']
    procedure onPriceChangeConfirmationResult(billingResult: JBillingResult); cdecl;
  end;
  TJPriceChangeConfirmationListener = class(TJavaGenericImport<JPriceChangeConfirmationListenerClass, JPriceChangeConfirmationListener>) end;

  JPriceChangeFlowParamsClass = interface(JObjectClass)
    ['{4DC1B40D-2E4F-476A-9F99-8E48651A753E}']
    {class} function init: JPriceChangeFlowParams; cdecl;
    {class} function newBuilder: JPriceChangeFlowParams_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/PriceChangeFlowParams')]
  JPriceChangeFlowParams = interface(JObject)
    ['{03819180-27AC-4638-959F-9F7E1DAE1379}']
    function getSkuDetails: JSkuDetails; cdecl;
  end;
  TJPriceChangeFlowParams = class(TJavaGenericImport<JPriceChangeFlowParamsClass, JPriceChangeFlowParams>) end;

  JPriceChangeFlowParams_BuilderClass = interface(JObjectClass)
    ['{6F66A7AA-E7DA-46CD-AB1B-57B6504A316D}']
    {class} function init: JPriceChangeFlowParams_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/PriceChangeFlowParams$Builder')]
  JPriceChangeFlowParams_Builder = interface(JObject)
    ['{781A12F0-ADF4-4DAB-A5FB-E7121708E834}']
    function build: JPriceChangeFlowParams; cdecl;
    function setSkuDetails(skuDetails: JSkuDetails): JPriceChangeFlowParams_Builder; cdecl;
  end;
  TJPriceChangeFlowParams_Builder = class(TJavaGenericImport<JPriceChangeFlowParams_BuilderClass, JPriceChangeFlowParams_Builder>) end;

  JProxyBillingActivityClass = interface(JActivityClass)
    ['{31307946-C565-467B-A379-F3694F71EAAB}']
    {class} function init: JProxyBillingActivity; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/ProxyBillingActivity')]
  JProxyBillingActivity = interface(JActivity)
    ['{1C1CA8DE-8DEB-4B82-985B-0BE0EE2CF786}']
  end;
  TJProxyBillingActivity = class(TJavaGenericImport<JProxyBillingActivityClass, JProxyBillingActivity>) end;

  JPurchaseClass = interface(JObjectClass)
    ['{C35ACB42-9480-4BB9-B836-AF652E229EF4}']
    {class} function init(string_: JString; string_1: JString): JPurchase; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/Purchase')]
  JPurchase = interface(JObject)
    ['{4B3113D3-8BA6-4C17-82F8-083926944F78}']
    function equals(object_: JObject): Boolean; cdecl;
    function getAccountIdentifiers: JAccountIdentifiers; cdecl;
    function getDeveloperPayload: JString; cdecl;
    function getOrderId: JString; cdecl;
    function getOriginalJson: JString; cdecl;
    function getPackageName: JString; cdecl;
    function getPurchaseState: Integer; cdecl;
    function getPurchaseTime: Int64; cdecl;
    function getPurchaseToken: JString; cdecl;
    function getSignature: JString; cdecl;
    function getSku: JString; cdecl;
    function hashCode: Integer; cdecl;
    function isAcknowledged: Boolean; cdecl;
    function isAutoRenewing: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJPurchase = class(TJavaGenericImport<JPurchaseClass, JPurchase>) end;

  JPurchase_PurchaseStateClass = interface(JAnnotationClass)
    ['{37CBCCBB-4B8B-4F66-AC86-F2DEDB4FD575}']
    {class} function _GetPENDING: Integer; cdecl;
    {class} function _GetPURCHASED: Integer; cdecl;
    {class} function _GetUNSPECIFIED_STATE: Integer; cdecl;
    {class} property PENDING: Integer read _GetPENDING;
    {class} property PURCHASED: Integer read _GetPURCHASED;
    {class} property UNSPECIFIED_STATE: Integer read _GetUNSPECIFIED_STATE;
  end;

  [JavaSignature('com/android/billingclient/api/Purchase$PurchaseState')]
  JPurchase_PurchaseState = interface(JAnnotation)
    ['{C44F186C-C782-4DFE-A0BA-D905DB7C6CFE}']
  end;
  TJPurchase_PurchaseState = class(TJavaGenericImport<JPurchase_PurchaseStateClass, JPurchase_PurchaseState>) end;

  JPurchase_PurchasesResultClass = interface(JObjectClass)
    ['{F25EA884-EFC4-4D8F-BB91-F379815F4167}']
    {class} function init(billingResult: JBillingResult; list: JList): JPurchase_PurchasesResult; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/Purchase$PurchasesResult')]
  JPurchase_PurchasesResult = interface(JObject)
    ['{90939AB3-1516-45B8-834D-A492A9179A8B}']
    function getBillingResult: JBillingResult; cdecl;
    function getPurchasesList: JList; cdecl;
    function getResponseCode: Integer; cdecl;
  end;
  TJPurchase_PurchasesResult = class(TJavaGenericImport<JPurchase_PurchasesResultClass, JPurchase_PurchasesResult>) end;

  JPurchaseHistoryRecordClass = interface(JObjectClass)
    ['{AA8275E2-8281-4148-96EE-86668F60F9C3}']
    {class} function init(string_: JString; string_1: JString): JPurchaseHistoryRecord; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/PurchaseHistoryRecord')]
  JPurchaseHistoryRecord = interface(JObject)
    ['{FFB6ECDE-971E-4DAD-BD98-3E0E9ED22DB3}']
    function equals(object_: JObject): Boolean; cdecl;
    function getDeveloperPayload: JString; cdecl;
    function getOriginalJson: JString; cdecl;
    function getPurchaseTime: Int64; cdecl;
    function getPurchaseToken: JString; cdecl;
    function getSignature: JString; cdecl;
    function getSku: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJPurchaseHistoryRecord = class(TJavaGenericImport<JPurchaseHistoryRecordClass, JPurchaseHistoryRecord>) end;

  JPurchaseHistoryResponseListenerClass = interface(IJavaClass)
    ['{89D88A27-619A-4877-A1DE-4B517E22C857}']
  end;

  [JavaSignature('com/android/billingclient/api/PurchaseHistoryResponseListener')]
  JPurchaseHistoryResponseListener = interface(IJavaInstance)
    ['{E6A30037-A974-436D-A76E-C6E0E59D142C}']
    procedure onPurchaseHistoryResponse(billingResult: JBillingResult; list: JList); cdecl;
  end;
  TJPurchaseHistoryResponseListener = class(TJavaGenericImport<JPurchaseHistoryResponseListenerClass, JPurchaseHistoryResponseListener>) end;

  JPurchasesUpdatedListenerClass = interface(IJavaClass)
    ['{BA86FD20-7696-4AB0-85F4-A51FD4E3F11D}']
  end;

  [JavaSignature('com/android/billingclient/api/PurchasesUpdatedListener')]
  JPurchasesUpdatedListener = interface(IJavaInstance)
    ['{B7D764EA-0342-47E5-ACE7-27B66AC9655A}']
    procedure onPurchasesUpdated(billingResult: JBillingResult; list: JList); cdecl;
  end;
  TJPurchasesUpdatedListener = class(TJavaGenericImport<JPurchasesUpdatedListenerClass, JPurchasesUpdatedListener>) end;

  JSkuDetailsClass = interface(JObjectClass)
    ['{5CA1FC3F-47E8-4890-AE2D-8328C2986964}']
    {class} function init(string_: JString): JSkuDetails; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/SkuDetails')]
  JSkuDetails = interface(JObject)
    ['{94D3A6CE-0093-48DC-BFA9-FD97DE4882F0}']
    function equals(object_: JObject): Boolean; cdecl;
    function getDescription: JString; cdecl;
    function getFreeTrialPeriod: JString; cdecl;
    function getIconUrl: JString; cdecl;
    function getIntroductoryPrice: JString; cdecl;
    function getIntroductoryPriceAmountMicros: Int64; cdecl;
    function getIntroductoryPriceCycles: Integer; cdecl;
    function getIntroductoryPricePeriod: JString; cdecl;
    function getOriginalJson: JString; cdecl;
    function getOriginalPrice: JString; cdecl;
    function getOriginalPriceAmountMicros: Int64; cdecl;
    function getPrice: JString; cdecl;
    function getPriceAmountMicros: Int64; cdecl;
    function getPriceCurrencyCode: JString; cdecl;
    function getSku: JString; cdecl;
    function getSubscriptionPeriod: JString; cdecl;
    function getTitle: JString; cdecl;
    function getType: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJSkuDetails = class(TJavaGenericImport<JSkuDetailsClass, JSkuDetails>) end;

  JSkuDetailsParamsClass = interface(JObjectClass)
    ['{1B7EDC93-79F6-4F8A-9743-8CD68F0A1D21}']
    {class} function init: JSkuDetailsParams; cdecl;
    {class} function newBuilder: JSkuDetailsParams_Builder; cdecl;
  end;

  [JavaSignature('com/android/billingclient/api/SkuDetailsParams')]
  JSkuDetailsParams = interface(JObject)
    ['{87437956-F64C-4EF5-A482-E6401A0744ED}']
    function getSkuType: JString; cdecl;
    function getSkusList: JList; cdecl;
  end;
  TJSkuDetailsParams = class(TJavaGenericImport<JSkuDetailsParamsClass, JSkuDetailsParams>) end;

  JSkuDetailsParams_BuilderClass = interface(JObjectClass)
    ['{0EF8DF09-EAE2-4D93-AE77-19B6FBF38E5A}']
  end;

  [JavaSignature('com/android/billingclient/api/SkuDetailsParams$Builder')]
  JSkuDetailsParams_Builder = interface(JObject)
    ['{4F55EEC3-7AED-41E1-A78B-546BCCC12EBB}']
    function build: JSkuDetailsParams; cdecl;
    function setSkusList(list: JList): JSkuDetailsParams_Builder; cdecl;
    function setType(string_: JString): JSkuDetailsParams_Builder; cdecl;
  end;
  TJSkuDetailsParams_Builder = class(TJavaGenericImport<JSkuDetailsParams_BuilderClass, JSkuDetailsParams_Builder>) end;

  JSkuDetailsResponseListenerClass = interface(IJavaClass)
    ['{D4397961-1F6A-42DC-ADB7-0A6A75F848C7}']
  end;

  [JavaSignature('com/android/billingclient/api/SkuDetailsResponseListener')]
  JSkuDetailsResponseListener = interface(IJavaInstance)
    ['{A534B67F-9084-451B-B777-0288F397BD1F}']
    procedure onSkuDetailsResponse(billingResult: JBillingResult; list: JList); cdecl;
  end;
  TJSkuDetailsResponseListener = class(TJavaGenericImport<JSkuDetailsResponseListenerClass, JSkuDetailsResponseListener>) end;

implementation

end.
