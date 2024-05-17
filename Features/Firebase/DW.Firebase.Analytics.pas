unit DW.Firebase.Analytics;

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

{$SCOPEDENUMS ON}

type
  TAnalyticsEvent = (AddPaymentInfo, AddShippingInfo, AddToCart, AddToWishList, AdImpression, AppOpen, BeginCheckout, CampaignDetails,
    CheckoutProgress, EarnVirtualCurrency, EcommercePurchase, GenerateLead, JoinGroup, LevelEnd, LevelStart, LevelUp, Login, PostScore, PresentOffer,
    Purchase, PurchaseRefund, Refund, RemoveFromCart, ScreenView, Search, SelectContent, SelectItem, SelectPromotion, SetCheckoutOption, Share,
    SignUp, SpendVirtualCurrency, TutorialBegin, TutorialComplete, UnlockAchievement, ViewCart, ViewItem, ViewItemList, ViewPromotion,
    ViewSearchResults);

  TAnalyticsEventParam = (AchievementId, ACLId, AdFormat, AdPlatform, AdSource, AdUnitName, Affiliation, Campaign, Character, CheckoutOption,
    CheckOutStep, Content, ContentType, Coupon, CP1, CreativeName, CreativeSlot, Currency, Destination, Discount, EndDate, ExtendSession,
    FlightNumber, GroupId, Index, Items, ItemBrand, ItemCategory, ItemCategory2, ItemCategory3, ItemCategory4, ItemCategory5, ItemId, ItemList,
    ItemListId, ItemListName, ItemLocationId, ItemName, ItemVariant, Level, LevelName, Location, LocationId, Medium, Method, NumberOfNights,
    NumberOfPassengers, NumberOfRooms, Origin, PaymentType, Price, PromotionId, PromotionName, Quantity, Score, ScreenClass, ScreenName,
    SearchTerm, Shipping, ShippingTier, SignUpMethod, Source, StartDate, Success, Tax, Term, TransactionId, TravelClass, Value,
    VirtualCurrencyName);

  TEventParam = record
    Name: string;
    Param: TAnalyticsEventParam;
    Value: string;
  end;

  TEventParams = TArray<TEventParam>;

  TConsentType = (AdStorage, AnalyticsStorage);

  TConsentStatus = (Denied, Granted);

  TConsentSetting = record
    ConsentType: TConsentType;
    ConsentStatus: TConsentStatus;
  end;

  TConsentSettings = TArray<TConsentSetting>;

  TFirebaseAnalytics = class;

  TCustomPlatformFirebaseAnalytics = class(TObject)
  private
    FFirebaseAnalytics: TFirebaseAnalytics;
  protected
    procedure LogEvent(const AEventName: string; const AEventParams: TEventParams); overload; virtual; abstract;
    procedure LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams); overload; virtual; abstract;
    procedure ResetAnalyticsData; virtual; abstract;
    procedure SetAnalyticsCollectionEnabled(const AEnabled: Boolean); virtual; abstract;
    procedure SetConsent(const ASettings: TConsentSettings); virtual; abstract;
    procedure SetDefaultEventParameters(const AEventParams: TEventParams); virtual; abstract;
    procedure SetSessionTimeoutDuration(const ATimeout: Int64); virtual; abstract;
    procedure SetUserId(const AName: string); virtual; abstract;
    procedure SetUserProperty(const AName, AValue: string); virtual; abstract;
    property FirebaseAnalytics: TFirebaseAnalytics read FFirebaseAnalytics;
  public
    constructor Create(const AFirebaseAnalytics: TFirebaseAnalytics); virtual;
  end;

  TFirebaseAnalytics = class(TObject)
  private
    FPlatformFirebaseAnalytics: TCustomPlatformFirebaseAnalytics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LogEvent(const AEventName: string; const AEventParams: TEventParams); overload;
    procedure LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams); overload;
    procedure ResetAnalyticsData;
    procedure SetAnalyticsCollectionEnabled(const AEnabled: Boolean);
    procedure SetConsent(const ASettings: TConsentSettings);
    procedure SetDefaultEventParameters(const AEventParams: TEventParams);
    procedure SetSessionTimeoutDuration(const ATimeout: Int64);
    procedure SetUserId(const AName: string);
    procedure SetUserProperty(const AName, AValue: string);
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Firebase.Analytics.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Firebase.Analytics.Android;
  {$ENDIF}

{ TCustomPlatformFirebaseAnalytics }

constructor TCustomPlatformFirebaseAnalytics.Create(const AFirebaseAnalytics: TFirebaseAnalytics);
begin
  inherited Create;
  FFirebaseAnalytics := AFirebaseAnalytics;
end;

{ TFirebaseAnalytics }

constructor TFirebaseAnalytics.Create;
begin
  inherited;
  FPlatformFirebaseAnalytics := TPlatformFirebaseAnalytics.Create(Self);
end;

destructor TFirebaseAnalytics.Destroy;
begin
  FPlatformFirebaseAnalytics.Free;
  inherited;
end;

procedure TFirebaseAnalytics.LogEvent(const AEventName: string; const AEventParams: TEventParams);
begin
  FPlatformFirebaseAnalytics.LogEvent(AEventName, AEventParams);
end;

procedure TFirebaseAnalytics.LogEvent(const AEvent: TAnalyticsEvent; const AEventParams: TEventParams);
begin
  FPlatformFirebaseAnalytics.LogEvent(AEvent, AEventParams);
end;

procedure TFirebaseAnalytics.ResetAnalyticsData;
begin
  FPlatformFirebaseAnalytics.ResetAnalyticsData;
end;

procedure TFirebaseAnalytics.SetAnalyticsCollectionEnabled(const AEnabled: Boolean);
begin
  FPlatformFirebaseAnalytics.SetAnalyticsCollectionEnabled(AEnabled);
end;

procedure TFirebaseAnalytics.SetConsent(const ASettings: TConsentSettings);
begin
  FPlatformFirebaseAnalytics.SetConsent(ASettings);
end;

procedure TFirebaseAnalytics.SetDefaultEventParameters(const AEventParams: TEventParams);
begin
  FPlatformFirebaseAnalytics.SetDefaultEventParameters(AEventParams);
end;

procedure TFirebaseAnalytics.SetSessionTimeoutDuration(const ATimeout: Int64);
begin
  FPlatformFirebaseAnalytics.SetSessionTimeoutDuration(ATimeout);
end;

procedure TFirebaseAnalytics.SetUserId(const AName: string);
begin
  FPlatformFirebaseAnalytics.SetUserId(AName);
end;

procedure TFirebaseAnalytics.SetUserProperty(const AName, AValue: string);
begin
  FPlatformFirebaseAnalytics.SetUserProperty(AName, AValue);
end;

end.
