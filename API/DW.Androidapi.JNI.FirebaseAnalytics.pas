unit DW.Androidapi.JNI.FirebaseAnalytics;

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
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.Util, Androidapi.JNI.PlayServices.Tasks;

type
  JFirebaseAnalytics = interface;
  JFirebaseAnalytics_ConsentStatus = interface;
  JFirebaseAnalytics_ConsentType = interface;
  JFirebaseAnalytics_Event = interface;
  JFirebaseAnalytics_Param = interface;
  JFirebaseAnalytics_UserProperty = interface;

  JFirebaseAnalyticsClass = interface(JObjectClass)
    ['{407A6397-B8CC-4865-A51C-4FE784356701}']
    {class} function getInstance(context: JContext): JFirebaseAnalytics; cdecl;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics')]
  JFirebaseAnalytics = interface(JObject)
    ['{B76138D5-E2E8-47E8-814D-D749B0B8026A}']
    function getAppInstanceId: JTask; cdecl;
    function getFirebaseInstanceId: JString; cdecl;
    procedure logEvent(name: JString; params: JBundle); cdecl;
    procedure resetAnalyticsData; cdecl;
    procedure setAnalyticsCollectionEnabled(enabled: Boolean); cdecl;
    procedure setConsent(consentSettings: JMap); cdecl;
    procedure setCurrentScreen(activity: JActivity; screenName: JString; screenClassOverride: JString); cdecl;
    procedure setDefaultEventParameters(parameters: JBundle); cdecl;
    procedure setSessionTimeoutDuration(milliseconds: Int64); cdecl;
    procedure setUserId(id: JString); cdecl;
    procedure setUserProperty(name: JString; value: JString); cdecl;
  end;
  TJFirebaseAnalytics = class(TJavaGenericImport<JFirebaseAnalyticsClass, JFirebaseAnalytics>) end;

  JFirebaseAnalytics_ConsentStatusClass = interface(JEnumClass)
    ['{76BDFE84-8A7D-4664-A284-B0F1A11E3E83}']
    {class} function _GetDENIED: JFirebaseAnalytics_ConsentStatus; cdecl;
    {class} function _GetGRANTED: JFirebaseAnalytics_ConsentStatus; cdecl;
    {class} property DENIED: JFirebaseAnalytics_ConsentStatus read _GetDENIED;
    {class} property GRANTED: JFirebaseAnalytics_ConsentStatus read _GetGRANTED;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics$ConsentStatus')]
  JFirebaseAnalytics_ConsentStatus = interface(JEnum)
    ['{B7703ACB-31D4-45B4-9FF6-0F898FB93B77}']
  end;
  TJFirebaseAnalytics_ConsentStatus = class(TJavaGenericImport<JFirebaseAnalytics_ConsentStatusClass, JFirebaseAnalytics_ConsentStatus>) end;

  JFirebaseAnalytics_ConsentTypeClass = interface(JEnumClass)
    ['{6A1390C3-5837-492B-9DA3-78D9B727EDFE}']
    {class} function _GetAD_STORAGE: JFirebaseAnalytics_ConsentType; cdecl;
    {class} function _GetANALYTICS_STORAGE: JFirebaseAnalytics_ConsentType; cdecl;
    {class} property AD_STORAGE: JFirebaseAnalytics_ConsentType read _GetAD_STORAGE;
    {class} property ANALYTICS_STORAGE: JFirebaseAnalytics_ConsentType read _GetANALYTICS_STORAGE;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics$ConsentType')]
  JFirebaseAnalytics_ConsentType = interface(JEnum)
    ['{2605D2FE-8984-4DA6-A4E8-DCF3707101B5}']
  end;
  TJFirebaseAnalytics_ConsentType = class(TJavaGenericImport<JFirebaseAnalytics_ConsentTypeClass, JFirebaseAnalytics_ConsentType>) end;

  JFirebaseAnalytics_EventClass = interface(JObjectClass)
    ['{94632D17-F4F1-4804-8742-36D4A14545B0}']
    {class} function _GetADD_PAYMENT_INFO: JString; cdecl;
    {class} function _GetADD_SHIPPING_INFO: JString; cdecl;
    {class} function _GetADD_TO_CART: JString; cdecl;
    {class} function _GetADD_TO_WISHLIST: JString; cdecl;
    {class} function _GetAD_IMPRESSION: JString; cdecl;
    {class} function _GetAPP_OPEN: JString; cdecl;
    {class} function _GetBEGIN_CHECKOUT: JString; cdecl;
    {class} function _GetCAMPAIGN_DETAILS: JString; cdecl;
    {class} function _GetCHECKOUT_PROGRESS: JString; cdecl;
    {class} function _GetEARN_VIRTUAL_CURRENCY: JString; cdecl;
    {class} function _GetECOMMERCE_PURCHASE: JString; cdecl;
    {class} function _GetGENERATE_LEAD: JString; cdecl;
    {class} function _GetJOIN_GROUP: JString; cdecl;
    {class} function _GetLEVEL_END: JString; cdecl;
    {class} function _GetLEVEL_START: JString; cdecl;
    {class} function _GetLEVEL_UP: JString; cdecl;
    {class} function _GetLOGIN: JString; cdecl;
    {class} function _GetPOST_SCORE: JString; cdecl;
    {class} function _GetPRESENT_OFFER: JString; cdecl;
    {class} function _GetPURCHASE: JString; cdecl;
    {class} function _GetPURCHASE_REFUND: JString; cdecl;
    {class} function _GetREFUND: JString; cdecl;
    {class} function _GetREMOVE_FROM_CART: JString; cdecl;
    {class} function _GetSCREEN_VIEW: JString; cdecl;
    {class} function _GetSEARCH: JString; cdecl;
    {class} function _GetSELECT_CONTENT: JString; cdecl;
    {class} function _GetSELECT_ITEM: JString; cdecl;
    {class} function _GetSELECT_PROMOTION: JString; cdecl;
    {class} function _GetSET_CHECKOUT_OPTION: JString; cdecl;
    {class} function _GetSHARE: JString; cdecl;
    {class} function _GetSIGN_UP: JString; cdecl;
    {class} function _GetSPEND_VIRTUAL_CURRENCY: JString; cdecl;
    {class} function _GetTUTORIAL_BEGIN: JString; cdecl;
    {class} function _GetTUTORIAL_COMPLETE: JString; cdecl;
    {class} function _GetUNLOCK_ACHIEVEMENT: JString; cdecl;
    {class} function _GetVIEW_CART: JString; cdecl;
    {class} function _GetVIEW_ITEM: JString; cdecl;
    {class} function _GetVIEW_ITEM_LIST: JString; cdecl;
    {class} function _GetVIEW_PROMOTION: JString; cdecl;
    {class} function _GetVIEW_SEARCH_RESULTS: JString; cdecl;
    {class} property ADD_PAYMENT_INFO: JString read _GetADD_PAYMENT_INFO;
    {class} property ADD_SHIPPING_INFO: JString read _GetADD_SHIPPING_INFO;
    {class} property ADD_TO_CART: JString read _GetADD_TO_CART;
    {class} property ADD_TO_WISHLIST: JString read _GetADD_TO_WISHLIST;
    {class} property AD_IMPRESSION: JString read _GetAD_IMPRESSION;
    {class} property APP_OPEN: JString read _GetAPP_OPEN;
    {class} property BEGIN_CHECKOUT: JString read _GetBEGIN_CHECKOUT;
    {class} property CAMPAIGN_DETAILS: JString read _GetCAMPAIGN_DETAILS;
    {class} property CHECKOUT_PROGRESS: JString read _GetCHECKOUT_PROGRESS;
    {class} property EARN_VIRTUAL_CURRENCY: JString read _GetEARN_VIRTUAL_CURRENCY;
    {class} property ECOMMERCE_PURCHASE: JString read _GetECOMMERCE_PURCHASE;
    {class} property GENERATE_LEAD: JString read _GetGENERATE_LEAD;
    {class} property JOIN_GROUP: JString read _GetJOIN_GROUP;
    {class} property LEVEL_END: JString read _GetLEVEL_END;
    {class} property LEVEL_START: JString read _GetLEVEL_START;
    {class} property LEVEL_UP: JString read _GetLEVEL_UP;
    {class} property LOGIN: JString read _GetLOGIN;
    {class} property POST_SCORE: JString read _GetPOST_SCORE;
    {class} property PRESENT_OFFER: JString read _GetPRESENT_OFFER;
    {class} property PURCHASE: JString read _GetPURCHASE;
    {class} property PURCHASE_REFUND: JString read _GetPURCHASE_REFUND;
    {class} property REFUND: JString read _GetREFUND;
    {class} property REMOVE_FROM_CART: JString read _GetREMOVE_FROM_CART;
    {class} property SCREEN_VIEW: JString read _GetSCREEN_VIEW;
    {class} property SEARCH: JString read _GetSEARCH;
    {class} property SELECT_CONTENT: JString read _GetSELECT_CONTENT;
    {class} property SELECT_ITEM: JString read _GetSELECT_ITEM;
    {class} property SELECT_PROMOTION: JString read _GetSELECT_PROMOTION;
    {class} property SET_CHECKOUT_OPTION: JString read _GetSET_CHECKOUT_OPTION;
    {class} property SHARE: JString read _GetSHARE;
    {class} property SIGN_UP: JString read _GetSIGN_UP;
    {class} property SPEND_VIRTUAL_CURRENCY: JString read _GetSPEND_VIRTUAL_CURRENCY;
    {class} property TUTORIAL_BEGIN: JString read _GetTUTORIAL_BEGIN;
    {class} property TUTORIAL_COMPLETE: JString read _GetTUTORIAL_COMPLETE;
    {class} property UNLOCK_ACHIEVEMENT: JString read _GetUNLOCK_ACHIEVEMENT;
    {class} property VIEW_CART: JString read _GetVIEW_CART;
    {class} property VIEW_ITEM: JString read _GetVIEW_ITEM;
    {class} property VIEW_ITEM_LIST: JString read _GetVIEW_ITEM_LIST;
    {class} property VIEW_PROMOTION: JString read _GetVIEW_PROMOTION;
    {class} property VIEW_SEARCH_RESULTS: JString read _GetVIEW_SEARCH_RESULTS;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics$Event')]
  JFirebaseAnalytics_Event = interface(JObject)
    ['{E827E80C-AF83-4C44-A2E2-71169E84D5CC}']
  end;
  TJFirebaseAnalytics_Event = class(TJavaGenericImport<JFirebaseAnalytics_EventClass, JFirebaseAnalytics_Event>) end;

  JFirebaseAnalytics_ParamClass = interface(JObjectClass)
    ['{67681227-1C4D-4251-BEDF-F3DCBF976CA2}']
    {class} function _GetACHIEVEMENT_ID: JString; cdecl;
    {class} function _GetACLID: JString; cdecl;
    {class} function _GetAD_FORMAT: JString; cdecl;
    {class} function _GetAD_PLATFORM: JString; cdecl;
    {class} function _GetAD_SOURCE: JString; cdecl;
    {class} function _GetAD_UNIT_NAME: JString; cdecl;
    {class} function _GetAFFILIATION: JString; cdecl;
    {class} function _GetCAMPAIGN: JString; cdecl;
    {class} function _GetCHARACTER: JString; cdecl;
    {class} function _GetCHECKOUT_OPTION: JString; cdecl;
    {class} function _GetCHECKOUT_STEP: JString; cdecl;
    {class} function _GetCONTENT: JString; cdecl;
    {class} function _GetCONTENT_TYPE: JString; cdecl;
    {class} function _GetCOUPON: JString; cdecl;
    {class} function _GetCP1: JString; cdecl;
    {class} function _GetCREATIVE_NAME: JString; cdecl;
    {class} function _GetCREATIVE_SLOT: JString; cdecl;
    {class} function _GetCURRENCY: JString; cdecl;
    {class} function _GetDESTINATION: JString; cdecl;
    {class} function _GetDISCOUNT: JString; cdecl;
    {class} function _GetEND_DATE: JString; cdecl;
    {class} function _GetEXTEND_SESSION: JString; cdecl;
    {class} function _GetFLIGHT_NUMBER: JString; cdecl;
    {class} function _GetGROUP_ID: JString; cdecl;
    {class} function _GetINDEX: JString; cdecl;
    {class} function _GetITEMS: JString; cdecl;
    {class} function _GetITEM_BRAND: JString; cdecl;
    {class} function _GetITEM_CATEGORY: JString; cdecl;
    {class} function _GetITEM_CATEGORY2: JString; cdecl;
    {class} function _GetITEM_CATEGORY3: JString; cdecl;
    {class} function _GetITEM_CATEGORY4: JString; cdecl;
    {class} function _GetITEM_CATEGORY5: JString; cdecl;
    {class} function _GetITEM_ID: JString; cdecl;
    {class} function _GetITEM_LIST: JString; cdecl;
    {class} function _GetITEM_LIST_ID: JString; cdecl;
    {class} function _GetITEM_LIST_NAME: JString; cdecl;
    {class} function _GetITEM_LOCATION_ID: JString; cdecl;
    {class} function _GetITEM_NAME: JString; cdecl;
    {class} function _GetITEM_VARIANT: JString; cdecl;
    {class} function _GetLEVEL: JString; cdecl;
    {class} function _GetLEVEL_NAME: JString; cdecl;
    {class} function _GetLOCATION: JString; cdecl;
    {class} function _GetLOCATION_ID: JString; cdecl;
    {class} function _GetMEDIUM: JString; cdecl;
    {class} function _GetMETHOD: JString; cdecl;
    {class} function _GetNUMBER_OF_NIGHTS: JString; cdecl;
    {class} function _GetNUMBER_OF_PASSENGERS: JString; cdecl;
    {class} function _GetNUMBER_OF_ROOMS: JString; cdecl;
    {class} function _GetORIGIN: JString; cdecl;
    {class} function _GetPAYMENT_TYPE: JString; cdecl;
    {class} function _GetPRICE: JString; cdecl;
    {class} function _GetPROMOTION_ID: JString; cdecl;
    {class} function _GetPROMOTION_NAME: JString; cdecl;
    {class} function _GetQUANTITY: JString; cdecl;
    {class} function _GetSCORE: JString; cdecl;
    {class} function _GetSCREEN_CLASS: JString; cdecl;
    {class} function _GetSCREEN_NAME: JString; cdecl;
    {class} function _GetSEARCH_TERM: JString; cdecl;
    {class} function _GetSHIPPING: JString; cdecl;
    {class} function _GetSHIPPING_TIER: JString; cdecl;
    {class} function _GetSIGN_UP_METHOD: JString; cdecl;
    {class} function _GetSOURCE: JString; cdecl;
    {class} function _GetSTART_DATE: JString; cdecl;
    {class} function _GetSUCCESS: JString; cdecl;
    {class} function _GetTAX: JString; cdecl;
    {class} function _GetTERM: JString; cdecl;
    {class} function _GetTRANSACTION_ID: JString; cdecl;
    {class} function _GetTRAVEL_CLASS: JString; cdecl;
    {class} function _GetVALUE: JString; cdecl;
    {class} function _GetVIRTUAL_CURRENCY_NAME: JString; cdecl;
    {class} property ACHIEVEMENT_ID: JString read _GetACHIEVEMENT_ID;
    {class} property ACLID: JString read _GetACLID;
    {class} property AD_FORMAT: JString read _GetAD_FORMAT;
    {class} property AD_PLATFORM: JString read _GetAD_PLATFORM;
    {class} property AD_SOURCE: JString read _GetAD_SOURCE;
    {class} property AD_UNIT_NAME: JString read _GetAD_UNIT_NAME;
    {class} property AFFILIATION: JString read _GetAFFILIATION;
    {class} property CAMPAIGN: JString read _GetCAMPAIGN;
    {class} property CHARACTER: JString read _GetCHARACTER;
    {class} property CHECKOUT_OPTION: JString read _GetCHECKOUT_OPTION;
    {class} property CHECKOUT_STEP: JString read _GetCHECKOUT_STEP;
    {class} property CONTENT: JString read _GetCONTENT;
    {class} property CONTENT_TYPE: JString read _GetCONTENT_TYPE;
    {class} property COUPON: JString read _GetCOUPON;
    {class} property CP1: JString read _GetCP1;
    {class} property CREATIVE_NAME: JString read _GetCREATIVE_NAME;
    {class} property CREATIVE_SLOT: JString read _GetCREATIVE_SLOT;
    {class} property CURRENCY: JString read _GetCURRENCY;
    {class} property DESTINATION: JString read _GetDESTINATION;
    {class} property DISCOUNT: JString read _GetDISCOUNT;
    {class} property END_DATE: JString read _GetEND_DATE;
    {class} property EXTEND_SESSION: JString read _GetEXTEND_SESSION;
    {class} property FLIGHT_NUMBER: JString read _GetFLIGHT_NUMBER;
    {class} property GROUP_ID: JString read _GetGROUP_ID;
    {class} property INDEX: JString read _GetINDEX;
    {class} property ITEMS: JString read _GetITEMS;
    {class} property ITEM_BRAND: JString read _GetITEM_BRAND;
    {class} property ITEM_CATEGORY: JString read _GetITEM_CATEGORY;
    {class} property ITEM_CATEGORY2: JString read _GetITEM_CATEGORY2;
    {class} property ITEM_CATEGORY3: JString read _GetITEM_CATEGORY3;
    {class} property ITEM_CATEGORY4: JString read _GetITEM_CATEGORY4;
    {class} property ITEM_CATEGORY5: JString read _GetITEM_CATEGORY5;
    {class} property ITEM_ID: JString read _GetITEM_ID;
    {class} property ITEM_LIST: JString read _GetITEM_LIST;
    {class} property ITEM_LIST_ID: JString read _GetITEM_LIST_ID;
    {class} property ITEM_LIST_NAME: JString read _GetITEM_LIST_NAME;
    {class} property ITEM_LOCATION_ID: JString read _GetITEM_LOCATION_ID;
    {class} property ITEM_NAME: JString read _GetITEM_NAME;
    {class} property ITEM_VARIANT: JString read _GetITEM_VARIANT;
    {class} property LEVEL: JString read _GetLEVEL;
    {class} property LEVEL_NAME: JString read _GetLEVEL_NAME;
    {class} property LOCATION: JString read _GetLOCATION;
    {class} property LOCATION_ID: JString read _GetLOCATION_ID;
    {class} property MEDIUM: JString read _GetMEDIUM;
    {class} property METHOD: JString read _GetMETHOD;
    {class} property NUMBER_OF_NIGHTS: JString read _GetNUMBER_OF_NIGHTS;
    {class} property NUMBER_OF_PASSENGERS: JString read _GetNUMBER_OF_PASSENGERS;
    {class} property NUMBER_OF_ROOMS: JString read _GetNUMBER_OF_ROOMS;
    {class} property ORIGIN: JString read _GetORIGIN;
    {class} property PAYMENT_TYPE: JString read _GetPAYMENT_TYPE;
    {class} property PRICE: JString read _GetPRICE;
    {class} property PROMOTION_ID: JString read _GetPROMOTION_ID;
    {class} property PROMOTION_NAME: JString read _GetPROMOTION_NAME;
    {class} property QUANTITY: JString read _GetQUANTITY;
    {class} property SCORE: JString read _GetSCORE;
    {class} property SCREEN_CLASS: JString read _GetSCREEN_CLASS;
    {class} property SCREEN_NAME: JString read _GetSCREEN_NAME;
    {class} property SEARCH_TERM: JString read _GetSEARCH_TERM;
    {class} property SHIPPING: JString read _GetSHIPPING;
    {class} property SHIPPING_TIER: JString read _GetSHIPPING_TIER;
    {class} property SIGN_UP_METHOD: JString read _GetSIGN_UP_METHOD;
    {class} property SOURCE: JString read _GetSOURCE;
    {class} property START_DATE: JString read _GetSTART_DATE;
    {class} property SUCCESS: JString read _GetSUCCESS;
    {class} property TAX: JString read _GetTAX;
    {class} property TERM: JString read _GetTERM;
    {class} property TRANSACTION_ID: JString read _GetTRANSACTION_ID;
    {class} property TRAVEL_CLASS: JString read _GetTRAVEL_CLASS;
    {class} property VALUE: JString read _GetVALUE;
    {class} property VIRTUAL_CURRENCY_NAME: JString read _GetVIRTUAL_CURRENCY_NAME;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics$Param')]
  JFirebaseAnalytics_Param = interface(JObject)
    ['{8EC15A4D-579E-4C3C-9B19-20B001CBF110}']
  end;
  TJFirebaseAnalytics_Param = class(TJavaGenericImport<JFirebaseAnalytics_ParamClass, JFirebaseAnalytics_Param>) end;

  JFirebaseAnalytics_UserPropertyClass = interface(JObjectClass)
    ['{CAF4819B-DD6D-4670-9804-C589928E4F15}']
    {class} function _GetALLOW_AD_PERSONALIZATION_SIGNALS: JString; cdecl;
    {class} function _GetSIGN_UP_METHOD: JString; cdecl;
    {class} property ALLOW_AD_PERSONALIZATION_SIGNALS: JString read _GetALLOW_AD_PERSONALIZATION_SIGNALS;
    {class} property SIGN_UP_METHOD: JString read _GetSIGN_UP_METHOD;
  end;

  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics$UserProperty')]
  JFirebaseAnalytics_UserProperty = interface(JObject)
    ['{9DC1EE70-6C93-4559-B3C7-7DA99CCD6B27}']
  end;
  TJFirebaseAnalytics_UserProperty = class(TJavaGenericImport<JFirebaseAnalytics_UserPropertyClass, JFirebaseAnalytics_UserProperty>) end;

implementation

end.
