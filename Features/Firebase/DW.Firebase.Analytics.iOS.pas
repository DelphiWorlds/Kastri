unit DW.Firebase.Analytics.iOS;

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
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Firebase.Analytics;

type
  TPlatformFirebaseAnalytics = class(TCustomPlatformFirebaseAnalytics)
  private
    function GetNativeConsent(const ASettings: TConsentSettings): NSDictionary;
    function GetNativeEvent(const AEvent: TAnalyticsEvent): NSString;
    function GetNativeEventParam(const AEventParam: TAnalyticsEventParam): NSString;
    function GetNativeEventParams(const AEventParams: TEventParams): NSDictionary;
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
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers,
  // DW
  DW.iOSapi.FirebaseAnalytics, DW.iOSapi.FirebaseCore, DW.Firebase.Common.iOS;

// https://firebase.google.com/docs/reference/ios/firebaseanalytics/api/reference/Type-Definitions
const
  cConsentStatusValues: array[TConsentStatus] of string = ('ConsentStatus.denied', 'ConsentStatus.granted');
  cConsentTypeValues: array[TConsentType] of string = ('ConsentStatus.adStorage', 'ConsentStatus.analyticsStorage');

{ TPlatformFirebaseAnalytics }

function TPlatformFirebaseAnalytics.GetNativeConsent(const ASettings: TConsentSettings): NSDictionary;
var
  LNativeSettings: NSMutableDictionary;
  LSetting: TConsentSetting;
begin
  LNativeSettings := TNSMutableDictionary.Create;
  for LSetting in ASettings do
    LNativeSettings.setValueForKey(StringToID(cConsentStatusValues[LSetting.ConsentStatus]), StrToNSStr(cConsentTypeValues[LSetting.ConsentType]));
  Result := LNativeSettings;
end;

function TPlatformFirebaseAnalytics.GetNativeEvent(const AEvent: TAnalyticsEvent): NSString;
begin
  case AEvent of
    TAnalyticsEvent.AddPaymentInfo:
      Result := kFIREventAddPaymentInfo;
    TAnalyticsEvent.AddShippingInfo:
      Result := kFIREventAddShippingInfo;
    TAnalyticsEvent.AddToCart:
      Result := kFIREventAddToCart;
    TAnalyticsEvent.AddToWishList:
      Result := kFIREventAddToWishlist;
    TAnalyticsEvent.AdImpression:
      Result := kFIREventAdImpression;
    TAnalyticsEvent.AppOpen:
      Result := kFIREventAppOpen;
    TAnalyticsEvent.BeginCheckout:
      Result := kFIREventBeginCheckout;
    TAnalyticsEvent.CampaignDetails:
      Result := kFIREventCampaignDetails;
    TAnalyticsEvent.CheckoutProgress:
      Result := kFIREventCheckoutProgress;
    TAnalyticsEvent.EarnVirtualCurrency:
      Result := kFIREventEarnVirtualCurrency;
    TAnalyticsEvent.EcommercePurchase:
      Result := kFIREventEcommercePurchase;
    TAnalyticsEvent.GenerateLead:
      Result := kFIREventGenerateLead;
    TAnalyticsEvent.JoinGroup:
      Result := kFIREventJoinGroup;
    TAnalyticsEvent.LevelEnd:
      Result := kFIREventLevelEnd;
    TAnalyticsEvent.LevelStart:
      Result := kFIREventLevelStart;
    TAnalyticsEvent.LevelUp:
      Result := kFIREventLevelUp;
    TAnalyticsEvent.Login:
      Result := kFIREventLogin;
    TAnalyticsEvent.PostScore:
      Result := kFIREventPostScore;
    TAnalyticsEvent.PresentOffer:
      Result := kFIREventPresentOffer;
    TAnalyticsEvent.Purchase:
      Result := kFIREventPurchase;
    TAnalyticsEvent.PurchaseRefund:
      Result := kFIREventPurchaseRefund;
    TAnalyticsEvent.Refund:
      Result := kFIREventRefund;
    TAnalyticsEvent.RemoveFromCart:
      Result := kFIREventRemoveFromCart;
    TAnalyticsEvent.ScreenView:
      Result := kFIREventScreenView;
    TAnalyticsEvent.Search:
      Result := kFIREventSearch;
    TAnalyticsEvent.SelectContent:
      Result := kFIREventSelectContent;
    TAnalyticsEvent.SelectItem:
      Result := kFIREventSelectItem;
    TAnalyticsEvent.SelectPromotion:
      Result := kFIREventSelectPromotion;
    TAnalyticsEvent.SetCheckoutOption:
      Result := kFIREventSetCheckoutOption;
    TAnalyticsEvent.Share:
      Result := kFIREventShare;
    TAnalyticsEvent.SignUp:
      Result := kFIREventSignUp;
    TAnalyticsEvent.SpendVirtualCurrency:
      Result := kFIREventSpendVirtualCurrency;
    TAnalyticsEvent.TutorialBegin:
      Result := kFIREventTutorialBegin;
    TAnalyticsEvent.TutorialComplete:
      Result := kFIREventTutorialComplete;
    TAnalyticsEvent.UnlockAchievement:
      Result := kFIREventUnlockAchievement;
    TAnalyticsEvent.ViewCart:
      Result := kFIREventViewCart;
    TAnalyticsEvent.ViewItem:
      Result := kFIREventViewItem;
    TAnalyticsEvent.ViewItemList:
      Result := kFIREventViewItemList;
    TAnalyticsEvent.ViewPromotion:
      Result := kFIREventViewPromotion;
    TAnalyticsEvent.ViewSearchResults:
      Result := kFIREventViewSearchResults;
  else
    Result := nil;
  end;
end;

function TPlatformFirebaseAnalytics.GetNativeEventParam(const AEventParam: TAnalyticsEventParam): NSString;
begin
  case AEventParam of
    TAnalyticsEventParam.AchievementId:
      Result := kFIRParameterAchievementID;
    TAnalyticsEventParam.ACLId:
      Result := kFIRParameterAdNetworkClickID;
    TAnalyticsEventParam.AdFormat:
      Result := kFIRParameterAdFormat;
    TAnalyticsEventParam.AdPlatform:
      Result := kFIRParameterAdPlatform;
    TAnalyticsEventParam.AdSource:
      Result := kFIRParameterAdSource;
    TAnalyticsEventParam.AdUnitName:
      Result := kFIRParameterAdUnitName;
    TAnalyticsEventParam.Affiliation:
      Result := kFIRParameterAffiliation;
    TAnalyticsEventParam.Campaign:
      Result := kFIRParameterCampaign;
    TAnalyticsEventParam.Character:
      Result := kFIRParameterCharacter;
    TAnalyticsEventParam.CheckoutOption:
      Result := kFIRParameterCheckoutOption;
    TAnalyticsEventParam.CheckOutStep:
      Result := kFIRParameterCheckoutStep;
    TAnalyticsEventParam.Content:
      Result := kFIRParameterContent;
    TAnalyticsEventParam.ContentType:
      Result := kFIRParameterContentType;
    TAnalyticsEventParam.Coupon:
      Result := kFIRParameterCoupon;
    TAnalyticsEventParam.CP1:
      Result := kFIRParameterCP1;
    TAnalyticsEventParam.CreativeName:
      Result := kFIRParameterCreativeName;
    TAnalyticsEventParam.CreativeSlot:
      Result := kFIRParameterCreativeSlot;
    TAnalyticsEventParam.Currency:
      Result := kFIRParameterCurrency;
    TAnalyticsEventParam.Destination:
      Result := kFIRParameterDestination;
    TAnalyticsEventParam.Discount:
      Result := kFIRParameterDiscount;
    TAnalyticsEventParam.EndDate:
      Result := kFIRParameterEndDate;
    TAnalyticsEventParam.ExtendSession:
      Result := kFIRParameterExtendSession;
    TAnalyticsEventParam.FlightNumber:
      Result := kFIRParameterFlightNumber;
    TAnalyticsEventParam.GroupId:
      Result := kFIRParameterGroupID;
    TAnalyticsEventParam.Index:
      Result := kFIRParameterIndex;
    TAnalyticsEventParam.Items:
      Result := kFIRParameterItems;
    TAnalyticsEventParam.ItemBrand:
      Result := kFIRParameterItemBrand;
    TAnalyticsEventParam.ItemCategory:
      Result := kFIRParameterItemCategory;
    TAnalyticsEventParam.ItemCategory2:
      Result := kFIRParameterItemCategory2;
    TAnalyticsEventParam.ItemCategory3:
      Result := kFIRParameterItemCategory3;
    TAnalyticsEventParam.ItemCategory4:
      Result := kFIRParameterItemCategory4;
    TAnalyticsEventParam.ItemCategory5:
      Result := kFIRParameterItemCategory5;
    TAnalyticsEventParam.ItemId:
      Result := kFIRParameterItemId;
    TAnalyticsEventParam.ItemList:
      Result := kFIRParameterItemList;
    TAnalyticsEventParam.ItemListId:
      Result := kFIRParameterItemListId;
    TAnalyticsEventParam.ItemListName:
      Result := kFIRParameterItemListName;
    TAnalyticsEventParam.ItemLocationId:
      Result := kFIRParameterItemLocationId;
    TAnalyticsEventParam.ItemName:
      Result := kFIRParameterItemName;
    TAnalyticsEventParam.ItemVariant:
      Result := kFIRParameterItemVariant;
    TAnalyticsEventParam.Level:
      Result := kFIRParameterLevel;
    TAnalyticsEventParam.LevelName:
      Result := kFIRParameterLevelName;
    TAnalyticsEventParam.Location:
      Result := kFIRParameterLocation;
    TAnalyticsEventParam.LocationId:
      Result := kFIRParameterLocationId;
    TAnalyticsEventParam.Medium:
      Result := kFIRParameterMedium;
    TAnalyticsEventParam.Method:
      Result := kFIRParameterMethod;
    TAnalyticsEventParam.NumberOfNights:
      Result := kFIRParameterNumberOfNights;
    TAnalyticsEventParam.NumberOfPassengers:
      Result := kFIRParameterNumberOfPassengers;
    TAnalyticsEventParam.NumberOfRooms:
      Result := kFIRParameterNumberOfRooms;
    TAnalyticsEventParam.Origin:
      Result := kFIRParameterOrigin;
    TAnalyticsEventParam.PaymentType:
      Result := kFIRParameterPaymentType;
    TAnalyticsEventParam.Price:
      Result := kFIRParameterPrice;
    TAnalyticsEventParam.PromotionId:
      Result := kFIRParameterPromotionID;
    TAnalyticsEventParam.PromotionName:
      Result := kFIRParameterPromotionName;
    TAnalyticsEventParam.Quantity:
      Result := kFIRParameterQuantity;
    TAnalyticsEventParam.Score:
      Result := kFIRParameterScore;
    TAnalyticsEventParam.ScreenClass:
      Result := kFIRParameterScreenClass;
    TAnalyticsEventParam.ScreenName:
      Result := kFIRParameterScreenName;
    TAnalyticsEventParam.SearchTerm:
      Result := kFIRParameterSearchTerm;
    TAnalyticsEventParam.Shipping:
      Result := kFIRParameterShipping;
    TAnalyticsEventParam.ShippingTier:
      Result := kFIRParameterShippingTier;
    TAnalyticsEventParam.SignUpMethod:
      Result := kFIRParameterSignUpMethod;
    TAnalyticsEventParam.Source:
      Result := kFIRParameterSource;
    TAnalyticsEventParam.StartDate:
      Result := kFIRParameterStartDate;
    TAnalyticsEventParam.Success:
      Result := kFIRParameterSuccess;
    TAnalyticsEventParam.Tax:
      Result := kFIRParameterTax;
    TAnalyticsEventParam.Term:
      Result := kFIRParameterTerm;
    TAnalyticsEventParam.TransactionId:
      Result := kFIRParameterTransactionID;
    TAnalyticsEventParam.TravelClass:
      Result := kFIRParameterTravelClass;
    TAnalyticsEventParam.Value:
      Result := kFIRParameterValue;
    TAnalyticsEventParam.VirtualCurrencyName:
      Result := kFIRParameterVirtualCurrencyName;
  else
    Result := nil;
  end;
end;

function TPlatformFirebaseAnalytics.GetNativeEventParams(const AEventParams: TEventParams): NSDictionary;
var
  LParams: NSMutableDictionary;
  LEventParam: TEventParam;
begin
  Result := nil;
  if Length(AEventParams) > 0 then
  begin
    LParams := TNSMutableDictionary.Create;
    for LEventParam in AEventParams do
    begin
      if not LEventParam.Name.IsEmpty then
        LParams.setValueForKey(StringToID(LEventParam.Value), StrToNSStr(LEventParam.Name))
      else
        LParams.setValueForKey(StringToID(LEventParam.Value), GetNativeEventParam(LEventParam.Param));
    end;
    Result := LParams;
  end;
end;

procedure TPlatformFirebaseAnalytics.LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams);
begin
  TFIRAnalytics.OCClass.logEventWithName(GetNativeEvent(AEvent), GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.LogEvent(const AEventName: string; const AEventParams: TEventParams);
begin
  TFIRAnalytics.OCClass.logEventWithName(StrToNSStr(AEventName), GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.ResetAnalyticsData;
begin
  TFIRAnalytics.OCClass.resetAnalyticsData;
end;

procedure TPlatformFirebaseAnalytics.SetAnalyticsCollectionEnabled(const AEnabled: Boolean);
begin
  TFIRAnalytics.OCClass.setAnalyticsCollectionEnabled(AEnabled);
end;

procedure TPlatformFirebaseAnalytics.SetConsent(const ASettings: TConsentSettings);
begin
  TFIRAnalytics.OCClass.setConsent(GetNativeConsent(ASettings));
end;

procedure TPlatformFirebaseAnalytics.SetDefaultEventParameters(const AEventParams: TEventParams);
begin
  TFIRAnalytics.OCClass.setDefaultEventParameters(GetNativeEventParams(AEventParams));
end;

procedure TPlatformFirebaseAnalytics.SetSessionTimeoutDuration(const ATimeout: Int64);
begin
  TFIRAnalytics.OCClass.setSessionTimeoutInterval(ATimeout / 1000);
end;

procedure TPlatformFirebaseAnalytics.SetUserId(const AName: string);
begin
  if AName.IsEmpty then
    TFIRAnalytics.OCClass.setUserId(nil)
  else
    TFIRAnalytics.OCClass.setUserId(StrToNSStr(AName));
end;

procedure TPlatformFirebaseAnalytics.SetUserProperty(const AName, AValue: string);
begin
  if AValue.IsEmpty then
    TFIRAnalytics.OCClass.setUserPropertyString(StrToNSStr(AName), nil)
  else
    TFIRAnalytics.OCClass.setUserPropertyString(StrToNSStr(AName), StrToNSStr(AValue));
end;

end.
