unit DW.Firebase.Analytics.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  // DW
  DW.Firebase.Analytics, DW.Androidapi.JNI.FirebaseAnalytics;

type
  TPlatformFirebaseAnalytics = class(TCustomPlatformFirebaseAnalytics)
  private
    FAnalytics: JFirebaseAnalytics;
    // function GetNativeConsentStatus(const AConsentStatus: TConsentStatus): JFirebaseAnalytics_ConsentStatus;
    // function GetNativeConsentType(const AConsentType: TConsentType): JFirebaseAnalytics_ConsentType;
    function GetNativeEvent(const AEvent: TAnalyticsEvent): JString;
    function GetNativeEventParam(const AEventParam: TAnalyticsEventParam): JString;
    function GetNativeEventParams(const AEventParams: TEventParams): JBundle;
  protected
    procedure LogEvent(const AEventName: string; const AEventParams: TEventParams); overload; override;
    procedure LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams); overload; override;
    procedure ResetAnalyticsData; override;
    procedure SetAnalyticsCollectionEnabled(const AEnabled: Boolean); override;
    procedure SetConsent(const ASettings: TConsentSettings); override;
    procedure SetDefaultEventParameters(const AEventParams: TEventParams); override;
    procedure SetSessionTimeoutDuration(const ATimeout: Int64); override;
    procedure SetUserId(const AName: string); override;
    procedure SetUserProperty(const AName, AValue: string); override;
  public
    constructor Create(const AFirebaseAnalytics: TFirebaseAnalytics); override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers;

{ TPlatformFirebaseAnalytics }

constructor TPlatformFirebaseAnalytics.Create(const AFirebaseAnalytics: TFirebaseAnalytics);
begin
  inherited;
  FAnalytics := TJFirebaseAnalytics.JavaClass.getInstance(TAndroidHelper.Context);
end;

(*
function TPlatformFirebaseAnalytics.GetNativeConsentStatus(const AConsentStatus: TConsentStatus): JFirebaseAnalytics_ConsentStatus;
begin
  case AConsentStatus of
    TConsentStatus.Denied:
      Result := TJFirebaseAnalytics_ConsentStatus.JavaClass.DENIED;
    TConsentStatus.Granted:
      Result := TJFirebaseAnalytics_ConsentStatus.JavaClass.GRANTED;
  else
    Result := nil;
  end;
end;

function TPlatformFirebaseAnalytics.GetNativeConsentType(const AConsentType: TConsentType): JFirebaseAnalytics_ConsentType;
begin
  case AConsentType of
    TConsentType.AdStorage:
      Result := TJFirebaseAnalytics_ConsentType.JavaClass.AD_STORAGE;
    TConsentType.AnalyticsStorage:
      Result := TJFirebaseAnalytics_ConsentType.JavaClass.ANALYTICS_STORAGE;
  else
    Result := nil;
  end;
end;
*)

function TPlatformFirebaseAnalytics.GetNativeEvent(const AEvent: TAnalyticsEvent): JString;
begin
  case AEvent of
    TAnalyticsEvent.AddPaymentInfo:
      Result := TJFirebaseAnalytics_Event.JavaClass.ADD_PAYMENT_INFO;
    TAnalyticsEvent.AddShippingInfo:
      Result := TJFirebaseAnalytics_Event.JavaClass.ADD_SHIPPING_INFO;
    TAnalyticsEvent.AddToCart:
      Result := TJFirebaseAnalytics_Event.JavaClass.ADD_TO_CART;
    TAnalyticsEvent.AddToWishList:
      Result := TJFirebaseAnalytics_Event.JavaClass.ADD_TO_WISHLIST;
    TAnalyticsEvent.AdImpression:
      Result := TJFirebaseAnalytics_Event.JavaClass.AD_IMPRESSION;
    TAnalyticsEvent.AppOpen:
      Result := TJFirebaseAnalytics_Event.JavaClass.APP_OPEN;
    TAnalyticsEvent.BeginCheckout:
      Result := TJFirebaseAnalytics_Event.JavaClass.BEGIN_CHECKOUT;
    TAnalyticsEvent.CampaignDetails:
      Result := TJFirebaseAnalytics_Event.JavaClass.CAMPAIGN_DETAILS;
    TAnalyticsEvent.CheckoutProgress:
      Result := TJFirebaseAnalytics_Event.JavaClass.CHECKOUT_PROGRESS;
    TAnalyticsEvent.EarnVirtualCurrency:
      Result := TJFirebaseAnalytics_Event.JavaClass.EARN_VIRTUAL_CURRENCY;
    TAnalyticsEvent.EcommercePurchase:
      Result := TJFirebaseAnalytics_Event.JavaClass.ECOMMERCE_PURCHASE;
    TAnalyticsEvent.GenerateLead:
      Result := TJFirebaseAnalytics_Event.JavaClass.GENERATE_LEAD;
    TAnalyticsEvent.JoinGroup:
      Result := TJFirebaseAnalytics_Event.JavaClass.JOIN_GROUP;
    TAnalyticsEvent.LevelEnd:
      Result := TJFirebaseAnalytics_Event.JavaClass.LEVEL_END;
    TAnalyticsEvent.LevelStart:
      Result := TJFirebaseAnalytics_Event.JavaClass.LEVEL_START;
    TAnalyticsEvent.LevelUp:
      Result := TJFirebaseAnalytics_Event.JavaClass.LEVEL_UP;
    TAnalyticsEvent.Login:
      Result := TJFirebaseAnalytics_Event.JavaClass.LOGIN;
    TAnalyticsEvent.PostScore:
      Result := TJFirebaseAnalytics_Event.JavaClass.POST_SCORE;
    TAnalyticsEvent.PresentOffer:
      Result := TJFirebaseAnalytics_Event.JavaClass.PRESENT_OFFER;
    TAnalyticsEvent.Purchase:
      Result := TJFirebaseAnalytics_Event.JavaClass.PURCHASE;
    TAnalyticsEvent.PurchaseRefund:
      Result := TJFirebaseAnalytics_Event.JavaClass.PURCHASE_REFUND;
    TAnalyticsEvent.Refund:
      Result := TJFirebaseAnalytics_Event.JavaClass.REFUND;
    TAnalyticsEvent.RemoveFromCart:
      Result := TJFirebaseAnalytics_Event.JavaClass.REMOVE_FROM_CART;
    TAnalyticsEvent.ScreenView:
      Result := TJFirebaseAnalytics_Event.JavaClass.SCREEN_VIEW;
    TAnalyticsEvent.Search:
      Result := TJFirebaseAnalytics_Event.JavaClass.SEARCH;
    TAnalyticsEvent.SelectContent:
      Result := TJFirebaseAnalytics_Event.JavaClass.SELECT_CONTENT;
    TAnalyticsEvent.SelectItem:
      Result := TJFirebaseAnalytics_Event.JavaClass.SELECT_ITEM;
    TAnalyticsEvent.SelectPromotion:
      Result := TJFirebaseAnalytics_Event.JavaClass.SELECT_PROMOTION;
    TAnalyticsEvent.SetCheckoutOption:
      Result := TJFirebaseAnalytics_Event.JavaClass.SET_CHECKOUT_OPTION;
    TAnalyticsEvent.Share:
      Result := TJFirebaseAnalytics_Event.JavaClass.SHARE;
    TAnalyticsEvent.SignUp:
      Result := TJFirebaseAnalytics_Event.JavaClass.SIGN_UP;
    TAnalyticsEvent.SpendVirtualCurrency:
      Result := TJFirebaseAnalytics_Event.JavaClass.SPEND_VIRTUAL_CURRENCY;
    TAnalyticsEvent.TutorialBegin:
      Result := TJFirebaseAnalytics_Event.JavaClass.TUTORIAL_BEGIN;
    TAnalyticsEvent.TutorialComplete:
      Result := TJFirebaseAnalytics_Event.JavaClass.TUTORIAL_COMPLETE;
    TAnalyticsEvent.UnlockAchievement:
      Result := TJFirebaseAnalytics_Event.JavaClass.UNLOCK_ACHIEVEMENT;
    TAnalyticsEvent.ViewCart:
      Result := TJFirebaseAnalytics_Event.JavaClass.VIEW_CART;
    TAnalyticsEvent.ViewItem:
      Result := TJFirebaseAnalytics_Event.JavaClass.VIEW_ITEM;
    TAnalyticsEvent.ViewItemList:
      Result := TJFirebaseAnalytics_Event.JavaClass.VIEW_ITEM_LIST;
    TAnalyticsEvent.ViewPromotion:
      Result := TJFirebaseAnalytics_Event.JavaClass.VIEW_PROMOTION;
    TAnalyticsEvent.ViewSearchResults:
      Result := TJFirebaseAnalytics_Event.JavaClass.VIEW_SEARCH_RESULTS;
  else
    Result := nil;
  end;
end;

function TPlatformFirebaseAnalytics.GetNativeEventParam(const AEventParam: TAnalyticsEventParam): JString;
begin
  case AEventParam of
    TAnalyticsEventParam.AchievementId:
      Result := TJFirebaseAnalytics_Param.JavaClass.ACHIEVEMENT_ID;
    TAnalyticsEventParam.ACLId:
      Result := TJFirebaseAnalytics_Param.JavaClass.ACLID;
    TAnalyticsEventParam.AdFormat:
      Result := TJFirebaseAnalytics_Param.JavaClass.AD_FORMAT;
    TAnalyticsEventParam.AdPlatform:
      Result := TJFirebaseAnalytics_Param.JavaClass.AD_PLATFORM;
    TAnalyticsEventParam.AdSource:
      Result := TJFirebaseAnalytics_Param.JavaClass.AD_SOURCE;
    TAnalyticsEventParam.AdUnitName:
      Result := TJFirebaseAnalytics_Param.JavaClass.AD_UNIT_NAME;
    TAnalyticsEventParam.Affiliation:
      Result := TJFirebaseAnalytics_Param.JavaClass.AFFILIATION;
    TAnalyticsEventParam.Campaign:
      Result := TJFirebaseAnalytics_Param.JavaClass.CAMPAIGN;
    TAnalyticsEventParam.Character:
      Result := TJFirebaseAnalytics_Param.JavaClass.CHARACTER;
    TAnalyticsEventParam.CheckoutOption:
      Result := TJFirebaseAnalytics_Param.JavaClass.CHECKOUT_OPTION;
    TAnalyticsEventParam.CheckOutStep:
      Result := TJFirebaseAnalytics_Param.JavaClass.CHECKOUT_STEP;
    TAnalyticsEventParam.Content:
      Result := TJFirebaseAnalytics_Param.JavaClass.CONTENT;
    TAnalyticsEventParam.ContentType:
      Result := TJFirebaseAnalytics_Param.JavaClass.CONTENT_TYPE;
    TAnalyticsEventParam.Coupon:
      Result := TJFirebaseAnalytics_Param.JavaClass.COUPON;
    TAnalyticsEventParam.CP1:
      Result := TJFirebaseAnalytics_Param.JavaClass.CP1;
    TAnalyticsEventParam.CreativeName:
      Result := TJFirebaseAnalytics_Param.JavaClass.CREATIVE_NAME;
    TAnalyticsEventParam.CreativeSlot:
      Result := TJFirebaseAnalytics_Param.JavaClass.CREATIVE_SLOT;
    TAnalyticsEventParam.Currency:
      Result := TJFirebaseAnalytics_Param.JavaClass.CURRENCY;
    TAnalyticsEventParam.Destination:
      Result := TJFirebaseAnalytics_Param.JavaClass.DESTINATION;
    TAnalyticsEventParam.Discount:
      Result := TJFirebaseAnalytics_Param.JavaClass.DISCOUNT;
    TAnalyticsEventParam.EndDate:
      Result := TJFirebaseAnalytics_Param.JavaClass.END_DATE;
    TAnalyticsEventParam.ExtendSession:
      Result := TJFirebaseAnalytics_Param.JavaClass.EXTEND_SESSION;
    TAnalyticsEventParam.FlightNumber:
      Result := TJFirebaseAnalytics_Param.JavaClass.FLIGHT_NUMBER;
    TAnalyticsEventParam.GroupId:
      Result := TJFirebaseAnalytics_Param.JavaClass.GROUP_ID;
    TAnalyticsEventParam.Index:
      Result := TJFirebaseAnalytics_Param.JavaClass.INDEX;
    TAnalyticsEventParam.Items:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEMS;
    TAnalyticsEventParam.ItemBrand:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_BRAND;
    TAnalyticsEventParam.ItemCategory:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_CATEGORY;
    TAnalyticsEventParam.ItemCategory2:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_CATEGORY2;
    TAnalyticsEventParam.ItemCategory3:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_CATEGORY3;
    TAnalyticsEventParam.ItemCategory4:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_CATEGORY4;
    TAnalyticsEventParam.ItemCategory5:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_CATEGORY5;
    TAnalyticsEventParam.ItemId:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_ID;
    TAnalyticsEventParam.ItemList:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_LIST;
    TAnalyticsEventParam.ItemListId:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_LIST_ID;
    TAnalyticsEventParam.ItemListName:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_LIST_NAME;
    TAnalyticsEventParam.ItemLocationId:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_LOCATION_ID;
    TAnalyticsEventParam.ItemName:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_NAME;
    TAnalyticsEventParam.ItemVariant:
      Result := TJFirebaseAnalytics_Param.JavaClass.ITEM_VARIANT;
    TAnalyticsEventParam.Level:
      Result := TJFirebaseAnalytics_Param.JavaClass.LEVEL;
    TAnalyticsEventParam.LevelName:
      Result := TJFirebaseAnalytics_Param.JavaClass.LEVEL_NAME;
    TAnalyticsEventParam.Location:
      Result := TJFirebaseAnalytics_Param.JavaClass.LOCATION;
    TAnalyticsEventParam.LocationId:
      Result := TJFirebaseAnalytics_Param.JavaClass.LOCATION_ID;
    TAnalyticsEventParam.Medium:
      Result := TJFirebaseAnalytics_Param.JavaClass.MEDIUM;
    TAnalyticsEventParam.Method:
      Result := TJFirebaseAnalytics_Param.JavaClass.METHOD;
    TAnalyticsEventParam.NumberOfNights:
      Result := TJFirebaseAnalytics_Param.JavaClass.NUMBER_OF_NIGHTS;
    TAnalyticsEventParam.NumberOfPassengers:
      Result := TJFirebaseAnalytics_Param.JavaClass.NUMBER_OF_PASSENGERS;
    TAnalyticsEventParam.NumberOfRooms:
      Result := TJFirebaseAnalytics_Param.JavaClass.NUMBER_OF_ROOMS;
    TAnalyticsEventParam.Origin:
      Result := TJFirebaseAnalytics_Param.JavaClass.ORIGIN;
    TAnalyticsEventParam.PaymentType:
      Result := TJFirebaseAnalytics_Param.JavaClass.PAYMENT_TYPE;
    TAnalyticsEventParam.Price:
      Result := TJFirebaseAnalytics_Param.JavaClass.PRICE;
    TAnalyticsEventParam.PromotionId:
      Result := TJFirebaseAnalytics_Param.JavaClass.PROMOTION_ID;
    TAnalyticsEventParam.PromotionName:
      Result := TJFirebaseAnalytics_Param.JavaClass.PROMOTION_NAME;
    TAnalyticsEventParam.Quantity:
      Result := TJFirebaseAnalytics_Param.JavaClass.QUANTITY;
    TAnalyticsEventParam.Score:
      Result := TJFirebaseAnalytics_Param.JavaClass.SCORE;
    TAnalyticsEventParam.ScreenClass:
      Result := TJFirebaseAnalytics_Param.JavaClass.SCREEN_CLASS;
    TAnalyticsEventParam.ScreenName:
      Result := TJFirebaseAnalytics_Param.JavaClass.SCREEN_NAME;
    TAnalyticsEventParam.SearchTerm:
      Result := TJFirebaseAnalytics_Param.JavaClass.SEARCH_TERM;
    TAnalyticsEventParam.Shipping:
      Result := TJFirebaseAnalytics_Param.JavaClass.SHIPPING;
    TAnalyticsEventParam.ShippingTier:
      Result := TJFirebaseAnalytics_Param.JavaClass.SHIPPING_TIER;
    TAnalyticsEventParam.SignUpMethod:
      Result := TJFirebaseAnalytics_Param.JavaClass.SIGN_UP_METHOD;
    TAnalyticsEventParam.Source:
      Result := TJFirebaseAnalytics_Param.JavaClass.SOURCE;
    TAnalyticsEventParam.StartDate:
      Result := TJFirebaseAnalytics_Param.JavaClass.START_DATE;
    TAnalyticsEventParam.Success:
      Result := TJFirebaseAnalytics_Param.JavaClass.SUCCESS;
    TAnalyticsEventParam.Tax:
      Result := TJFirebaseAnalytics_Param.JavaClass.TAX;
    TAnalyticsEventParam.Term:
      Result := TJFirebaseAnalytics_Param.JavaClass.TERM;
    TAnalyticsEventParam.TransactionId:
      Result := TJFirebaseAnalytics_Param.JavaClass.TRANSACTION_ID;
    TAnalyticsEventParam.TravelClass:
      Result := TJFirebaseAnalytics_Param.JavaClass.TRAVEL_CLASS;
    TAnalyticsEventParam.Value:
      Result := TJFirebaseAnalytics_Param.JavaClass.VALUE;
    TAnalyticsEventParam.VirtualCurrencyName:
      Result := TJFirebaseAnalytics_Param.JavaClass.VIRTUAL_CURRENCY_NAME;
  else
    Result := nil;
  end;
end;

function TPlatformFirebaseAnalytics.GetNativeEventParams(const AEventParams: TEventParams): JBundle;
var
  LEventParam: TEventParam;
begin
  Result := TJBundle.Create;
  for LEventParam in AEventParams do
  begin
    if not LEventParam.Name.IsEmpty then
      Result.putString(StringToJString(LEventParam.Name), StringToJString(LEventParam.Value))
    else
      Result.putString(GetNativeEventParam(LEventParam.Param), StringToJString(LEventParam.Value));
  end;
end;

procedure TPlatformFirebaseAnalytics.LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams);
begin
  FAnalytics.logEvent(GetNativeEvent(AEvent), GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.LogEvent(const AEventName: string; const AEventParams: TEventParams);
begin
  FAnalytics.logEvent(StringToJString(AEventName), GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.ResetAnalyticsData;
begin
  FAnalytics.resetAnalyticsData;
end;

procedure TPlatformFirebaseAnalytics.SetAnalyticsCollectionEnabled(const AEnabled: Boolean);
begin
  FAnalytics.setAnalyticsCollectionEnabled(AEnabled);
end;

procedure TPlatformFirebaseAnalytics.SetConsent(const ASettings: TConsentSettings);
begin
  // Future enhancement - see code below
end;

{
var
  LSetting: TConsentSetting;
  LMap: JMap;
begin
  LMap := TJDWUtility.JavaClass.createObjectMap;
  for LSetting in ASettings do
    LMap.put(GetNativeConsentType(LSetting.ConsentType), GetNativeConsentStatus(LSetting.ConsentStatus));
  FAnalytics.setConsent(LMap);
end;
}

procedure TPlatformFirebaseAnalytics.SetDefaultEventParameters(const AEventParams: TEventParams);
begin
  FAnalytics.setDefaultEventParameters(GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.SetSessionTimeoutDuration(const ATimeout: Int64);
begin
  FAnalytics.setSessionTimeoutDuration(ATimeout);
end;

procedure TPlatformFirebaseAnalytics.SetUserId(const AName: string);
begin
  if AName.IsEmpty then
    FAnalytics.setUserId(nil)
  else
    FAnalytics.setUserId(StringToJString(AName));
end;

procedure TPlatformFirebaseAnalytics.SetUserProperty(const AName, AValue: string);
begin
  if AValue.IsEmpty then
    FAnalytics.setUserProperty(StringToJString(AName), nil)
  else
    FAnalytics.setUserProperty(StringToJString(AName), StringToJString(AValue));
end;

end.
