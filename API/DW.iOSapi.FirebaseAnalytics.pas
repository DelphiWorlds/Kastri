unit DW.iOSapi.FirebaseAnalytics;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  FIRAnalytics = interface;

  FIRConsentType = NSString;
  FIRConsentStatus = NSString;
  TFIRAnalyticsBlockMethod1 = procedure of object;

  FIRAnalyticsClass = interface(NSObjectClass)
    ['{5633684A-6C9D-4C13-8499-D074FF0F0F71}']
    {class} function appInstanceID: NSString; cdecl;
    {class} procedure handleEventsForBackgroundURLSession(identifier: NSString; completionHandler: TFIRAnalyticsBlockMethod1); cdecl;
    {class} procedure handleOpenURL(url: NSURL); cdecl;
    {class} procedure handleUserActivity(userActivity: Pointer); cdecl;
    {class} procedure logEventWithName(name: NSString; parameters: NSDictionary); cdecl;
    {class} procedure resetAnalyticsData; cdecl;
    {class} procedure setAnalyticsCollectionEnabled(analyticsCollectionEnabled: Boolean); cdecl;
    {class} procedure setConsent(consentSettings: NSDictionary); cdecl;
    {class} procedure setDefaultEventParameters(parameters: NSDictionary); cdecl;
    {class} procedure setScreenName(screenName: NSString; screenClass: NSString); cdecl;
    {class} procedure setSessionTimeoutInterval(sessionTimeoutInterval: NSTimeInterval); cdecl;
    {class} procedure setUserID(userID: NSString); cdecl;
    {class} procedure setUserPropertyString(value: NSString; forName: NSString); cdecl;
  end;

  FIRAnalytics = interface(NSObject)
    ['{5BF49B9F-6E65-4FBF-A8CB-25B64E07982F}']
  end;
  TFIRAnalytics = class(TOCGenericImport<FIRAnalyticsClass, FIRAnalytics>) end;

function kFIREventAddPaymentInfo: NSString;
function kFIREventAddToCart: NSString;
function kFIREventAddToWishlist: NSString;
function kFIREventAdImpression: NSString;
function kFIREventAppOpen: NSString;
function kFIREventBeginCheckout: NSString;
function kFIREventCampaignDetails: NSString;
function kFIREventCheckoutProgress: NSString;
function kFIREventEarnVirtualCurrency: NSString;
function kFIREventEcommercePurchase: NSString;
function kFIREventGenerateLead: NSString;
function kFIREventJoinGroup: NSString;
function kFIREventLevelEnd: NSString;
function kFIREventLevelStart: NSString;
function kFIREventLevelUp: NSString;
function kFIREventLogin: NSString;
function kFIREventPostScore: NSString;
function kFIREventPresentOffer: NSString;
function kFIREventPurchaseRefund: NSString;
function kFIREventRemoveFromCart: NSString;
function kFIREventScreenView: NSString;
function kFIREventSearch: NSString;
function kFIREventSelectContent: NSString;
function kFIREventSetCheckoutOption: NSString;
function kFIREventShare: NSString;
function kFIREventSignUp: NSString;
function kFIREventSpendVirtualCurrency: NSString;
function kFIREventTutorialBegin: NSString;
function kFIREventTutorialComplete: NSString;
function kFIREventUnlockAchievement: NSString;
function kFIREventViewItem: NSString;
function kFIREventViewItemList: NSString;
function kFIREventViewSearchResults: NSString;
function kFIREventAddShippingInfo: NSString;
function kFIREventPurchase: NSString;
function kFIREventRefund: NSString;
function kFIREventSelectItem: NSString;
function kFIREventSelectPromotion: NSString;
function kFIREventViewCart: NSString;
function kFIREventViewPromotion: NSString;
function kFIRParameterAchievementID: NSString;
function kFIRParameterAdFormat: NSString;
function kFIRParameterAdNetworkClickID: NSString;
function kFIRParameterAdPlatform: NSString;
function kFIRParameterAdSource: NSString;
function kFIRParameterAdUnitName: NSString;
function kFIRParameterAffiliation: NSString;
function kFIRParameterCampaign: NSString;
function kFIRParameterCharacter: NSString;
function kFIRParameterCheckoutStep: NSString;
function kFIRParameterCheckoutOption: NSString;
function kFIRParameterContent: NSString;
function kFIRParameterContentType: NSString;
function kFIRParameterCoupon: NSString;
function kFIRParameterCP1: NSString;
function kFIRParameterCreativeName: NSString;
function kFIRParameterCreativeSlot: NSString;
function kFIRParameterCurrency: NSString;
function kFIRParameterDestination: NSString;
function kFIRParameterEndDate: NSString;
function kFIRParameterFlightNumber: NSString;
function kFIRParameterGroupID: NSString;
function kFIRParameterIndex: NSString;
function kFIRParameterItemBrand: NSString;
function kFIRParameterItemCategory: NSString;
function kFIRParameterItemID: NSString;
function kFIRParameterItemLocationID: NSString;
function kFIRParameterItemName: NSString;
function kFIRParameterItemList: NSString;
function kFIRParameterItemVariant: NSString;
function kFIRParameterLevel: NSString;
function kFIRParameterLocation: NSString;
function kFIRParameterMedium: NSString;
function kFIRParameterNumberOfNights: NSString;
function kFIRParameterNumberOfPassengers: NSString;
function kFIRParameterNumberOfRooms: NSString;
function kFIRParameterOrigin: NSString;
function kFIRParameterPrice: NSString;
function kFIRParameterQuantity: NSString;
function kFIRParameterScore: NSString;
function kFIRParameterScreenClass: NSString;
function kFIRParameterScreenName: NSString;
function kFIRParameterSearchTerm: NSString;
function kFIRParameterShipping: NSString;
function kFIRParameterSignUpMethod: NSString;
function kFIRParameterMethod: NSString;
function kFIRParameterSource: NSString;
function kFIRParameterStartDate: NSString;
function kFIRParameterTax: NSString;
function kFIRParameterTerm: NSString;
function kFIRParameterTransactionID: NSString;
function kFIRParameterTravelClass: NSString;
function kFIRParameterValue: NSString;
function kFIRParameterVirtualCurrencyName: NSString;
function kFIRParameterLevelName: NSString;
function kFIRParameterSuccess: NSString;
function kFIRParameterExtendSession: NSString;
function kFIRParameterDiscount: NSString;
function kFIRParameterItemCategory2: NSString;
function kFIRParameterItemCategory3: NSString;
function kFIRParameterItemCategory4: NSString;
function kFIRParameterItemCategory5: NSString;
function kFIRParameterItemListID: NSString;
function kFIRParameterItemListName: NSString;
function kFIRParameterItems: NSString;
function kFIRParameterLocationID: NSString;
function kFIRParameterPaymentType: NSString;
function kFIRParameterPromotionID: NSString;
function kFIRParameterPromotionName: NSString;
function kFIRParameterShippingTier: NSString;
function kFIRUserPropertySignUpMethod: NSString;
function kFIRUserPropertyAllowAdPersonalizationSignals: NSString;

implementation

uses
  // macOS
  Macapi.Helpers,
  // DW
  DW.iOSapi.FirebaseCore;

function kFIREventAddPaymentInfo: NSString;
begin
  Result := StrToNSStr('add_payment_info');
end;

function kFIREventAddToCart: NSString;
begin
  Result := StrToNSStr('add_to_cart');
end;

function kFIREventAddToWishlist: NSString;
begin
  Result := StrToNSStr('add_to_wishlist');
end;

function kFIREventAdImpression: NSString;
begin
  Result := StrToNSStr('ad_impression');
end;

function kFIREventAppOpen: NSString;
begin
  Result := StrToNSStr('app_open');
end;

function kFIREventBeginCheckout: NSString;
begin
  Result := StrToNSStr('begin_checkout');
end;

function kFIREventCampaignDetails: NSString;
begin
  Result := StrToNSStr('campaign_details');
end;

function kFIREventCheckoutProgress: NSString;
begin
  Result := StrToNSStr('checkout_progress');
end;

function kFIREventEarnVirtualCurrency: NSString;
begin
  Result := StrToNSStr('earn_virtual_currency');
end;

function kFIREventEcommercePurchase: NSString;
begin
  Result := StrToNSStr('ecommerce_purchase');
end;

function kFIREventGenerateLead: NSString;
begin
  Result := StrToNSStr('generate_lead');
end;

function kFIREventJoinGroup: NSString;
begin
  Result := StrToNSStr('join_group');
end;

function kFIREventLevelEnd: NSString;
begin
  Result := StrToNSStr('level_end');
end;

function kFIREventLevelStart: NSString;
begin
  Result := StrToNSStr('level_start');
end;

function kFIREventLevelUp: NSString;
begin
  Result := StrToNSStr('level_up');
end;

function kFIREventLogin: NSString;
begin
  Result := StrToNSStr('login');
end;

function kFIREventPostScore: NSString;
begin
  Result := StrToNSStr('post_score');
end;

function kFIREventPresentOffer: NSString;
begin
  Result := StrToNSStr('present_offer');
end;

function kFIREventPurchaseRefund: NSString;
begin
  Result := StrToNSStr('purchase_refund');
end;

function kFIREventRemoveFromCart: NSString;
begin
  Result := StrToNSStr('remove_from_cart');
end;

function kFIREventScreenView: NSString;
begin
  Result := StrToNSStr('screen_view');
end;

function kFIREventSearch: NSString;
begin
  Result := StrToNSStr('search');
end;

function kFIREventSelectContent: NSString;
begin
  Result := StrToNSStr('select_content');
end;

function kFIREventSetCheckoutOption: NSString;
begin
  Result := StrToNSStr('set_checkout_option');
end;

function kFIREventShare: NSString;
begin
  Result := StrToNSStr('share');
end;

function kFIREventSignUp: NSString;
begin
  Result := StrToNSStr('sign_up');
end;

function kFIREventSpendVirtualCurrency: NSString;
begin
  Result := StrToNSStr('spend_virtual_currency');
end;

function kFIREventTutorialBegin: NSString;
begin
  Result := StrToNSStr('tutorial_begin');
end;

function kFIREventTutorialComplete: NSString;
begin
  Result := StrToNSStr('tutorial_complete');
end;

function kFIREventUnlockAchievement: NSString;
begin
  Result := StrToNSStr('unlock_achievement');
end;

function kFIREventViewItem: NSString;
begin
  Result := StrToNSStr('view_item');
end;

function kFIREventViewItemList: NSString;
begin
  Result := StrToNSStr('view_item_list');
end;

function kFIREventViewSearchResults: NSString;
begin
  Result := StrToNSStr('view_search_results');
end;

function kFIREventAddShippingInfo: NSString;
begin
  Result := StrToNSStr('add_shipping_info');
end;

function kFIREventPurchase: NSString;
begin
  Result := StrToNSStr('purchase');
end;

function kFIREventRefund: NSString;
begin
  Result := StrToNSStr('refund');
end;

function kFIREventSelectItem: NSString;
begin
  Result := StrToNSStr('select_item');
end;

function kFIREventSelectPromotion: NSString;
begin
  Result := StrToNSStr('select_promotion');
end;

function kFIREventViewCart: NSString;
begin
  Result := StrToNSStr('view_cart');
end;

function kFIREventViewPromotion: NSString;
begin
  Result := StrToNSStr('view_promotion');
end;

function kFIRParameterAchievementID: NSString;
begin
  Result := StrToNSStr('achievement_id');
end;

function kFIRParameterAdFormat: NSString;
begin
  Result := StrToNSStr('ad_format');
end;

function kFIRParameterAdNetworkClickID: NSString;
begin
  Result := StrToNSStr('aclid');
end;

function kFIRParameterAdPlatform: NSString;
begin
  Result := StrToNSStr('ad_platform');
end;

function kFIRParameterAdSource: NSString;
begin
  Result := StrToNSStr('ad_source');
end;

function kFIRParameterAdUnitName: NSString;
begin
  Result := StrToNSStr('ad_unit_name');
end;

function kFIRParameterAffiliation: NSString;
begin
  Result := StrToNSStr('affiliation');
end;

function kFIRParameterCampaign: NSString;
begin
  Result := StrToNSStr('campaign');
end;

function kFIRParameterCharacter: NSString;
begin
  Result := StrToNSStr('character');
end;

function kFIRParameterCheckoutStep: NSString;
begin
  Result := StrToNSStr('checkout_step');
end;

function kFIRParameterCheckoutOption: NSString;
begin
  Result := StrToNSStr('checkout_option');
end;

function kFIRParameterContent: NSString;
begin
  Result := StrToNSStr('content');
end;

function kFIRParameterContentType: NSString;
begin
  Result := StrToNSStr('content_type');
end;

function kFIRParameterCoupon: NSString;
begin
  Result := StrToNSStr('coupon');
end;

function kFIRParameterCP1: NSString;
begin
  Result := StrToNSStr('cp1');
end;

function kFIRParameterCreativeName: NSString;
begin
  Result := StrToNSStr('creative_name');
end;

function kFIRParameterCreativeSlot: NSString;
begin
  Result := StrToNSStr('creative_slot');
end;

function kFIRParameterCurrency: NSString;
begin
  Result := StrToNSStr('currency');
end;

function kFIRParameterDestination: NSString;
begin
  Result := StrToNSStr('destination');
end;

function kFIRParameterEndDate: NSString;
begin
  Result := StrToNSStr('end_date');
end;

function kFIRParameterFlightNumber: NSString;
begin
  Result := StrToNSStr('flight_number');
end;

function kFIRParameterGroupID: NSString;
begin
  Result := StrToNSStr('group_id');
end;

function kFIRParameterIndex: NSString;
begin
  Result := StrToNSStr('index');
end;

function kFIRParameterItemBrand: NSString;
begin
  Result := StrToNSStr('item_brand');
end;

function kFIRParameterItemCategory: NSString;
begin
  Result := StrToNSStr('item_category');
end;

function kFIRParameterItemID: NSString;
begin
  Result := StrToNSStr('item_id');
end;

function kFIRParameterItemLocationID: NSString;
begin
  Result := StrToNSStr('item_location_id');
end;

function kFIRParameterItemName: NSString;
begin
  Result := StrToNSStr('item_name');
end;

function kFIRParameterItemList: NSString;
begin
  Result := StrToNSStr('item_list');
end;

function kFIRParameterItemVariant: NSString;
begin
  Result := StrToNSStr('item_variant');
end;

function kFIRParameterLevel: NSString;
begin
  Result := StrToNSStr('level');
end;

function kFIRParameterLocation: NSString;
begin
  Result := StrToNSStr('location');
end;

function kFIRParameterMedium: NSString;
begin
  Result := StrToNSStr('medium');
end;

function kFIRParameterNumberOfNights: NSString;
begin
  Result := StrToNSStr('number_of_nights');
end;

function kFIRParameterNumberOfPassengers: NSString;
begin
  Result := StrToNSStr('number_of_passengers');
end;

function kFIRParameterNumberOfRooms: NSString;
begin
  Result := StrToNSStr('number_of_rooms');
end;

function kFIRParameterOrigin: NSString;
begin
  Result := StrToNSStr('origin');
end;

function kFIRParameterPrice: NSString;
begin
  Result := StrToNSStr('price');
end;

function kFIRParameterQuantity: NSString;
begin
  Result := StrToNSStr('quantity');
end;

function kFIRParameterScore: NSString;
begin
  Result := StrToNSStr('score');
end;

function kFIRParameterScreenClass: NSString;
begin
  Result := StrToNSStr('screen_class');
end;

function kFIRParameterScreenName: NSString;
begin
  Result := StrToNSStr('screen_name');
end;

function kFIRParameterSearchTerm: NSString;
begin
  Result := StrToNSStr('search_term');
end;

function kFIRParameterShipping: NSString;
begin
  Result := StrToNSStr('shipping');
end;

function kFIRParameterSignUpMethod: NSString;
begin
  Result := StrToNSStr('sign_up_method');
end;

function kFIRParameterMethod: NSString;
begin
  Result := StrToNSStr('method');
end;

function kFIRParameterSource: NSString;
begin
  Result := StrToNSStr('source');
end;

function kFIRParameterStartDate: NSString;
begin
  Result := StrToNSStr('start_date');
end;

function kFIRParameterTax: NSString;
begin
  Result := StrToNSStr('tax');
end;

function kFIRParameterTerm: NSString;
begin
  Result := StrToNSStr('term');
end;

function kFIRParameterTransactionID: NSString;
begin
  Result := StrToNSStr('transaction_id');
end;

function kFIRParameterTravelClass: NSString;
begin
  Result := StrToNSStr('travel_class');
end;

function kFIRParameterValue: NSString;
begin
  Result := StrToNSStr('value');
end;

function kFIRParameterVirtualCurrencyName: NSString;
begin
  Result := StrToNSStr('virtual_currency_name');
end;

function kFIRParameterLevelName: NSString;
begin
  Result := StrToNSStr('level_name');
end;

function kFIRParameterSuccess: NSString;
begin
  Result := StrToNSStr('success');
end;

function kFIRParameterExtendSession: NSString;
begin
  Result := StrToNSStr('extend_session');
end;

function kFIRParameterDiscount: NSString;
begin
  Result := StrToNSStr('discount');
end;

function kFIRParameterItemCategory2: NSString;
begin
  Result := StrToNSStr('item_category2');
end;

function kFIRParameterItemCategory3: NSString;
begin
  Result := StrToNSStr('item_category3');
end;

function kFIRParameterItemCategory4: NSString;
begin
  Result := StrToNSStr('item_category4');
end;

function kFIRParameterItemCategory5: NSString;
begin
  Result := StrToNSStr('item_category5');
end;

function kFIRParameterItemListID: NSString;
begin
  Result := StrToNSStr('item_list_id');
end;

function kFIRParameterItemListName: NSString;
begin
  Result := StrToNSStr('item_list_name');
end;

function kFIRParameterItems: NSString;
begin
  Result := StrToNSStr('items');
end;

function kFIRParameterLocationID: NSString;
begin
  Result := StrToNSStr('location_id');
end;

function kFIRParameterPaymentType: NSString;
begin
  Result := StrToNSStr('payment_type');
end;

function kFIRParameterPromotionID: NSString;
begin
  Result := StrToNSStr('promotion_id');
end;

function kFIRParameterPromotionName: NSString;
begin
  Result := StrToNSStr('promotion_name');
end;

function kFIRParameterShippingTier: NSString;
begin
  Result := StrToNSStr('shipping_tier');
end;

function kFIRUserPropertySignUpMethod: NSString;
begin
  Result := StrToNSStr('sign_up_method');
end;

function kFIRUserPropertyAllowAdPersonalizationSignals: NSString;
begin
  Result := StrToNSStr('allow_personalized_ads');
end;

end.