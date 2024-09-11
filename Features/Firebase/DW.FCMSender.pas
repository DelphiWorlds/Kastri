unit DW.FCMSender;

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


// ********* NOTE *************
// This unit is dependent on code/binaries from Grijjy, which can be found here:
//   https://github.com/grijjy/DelphiOpenSsl
// This unit requires the OpenSSL.Api_11 unit from DelphiOpenSsl (see the implementation section of this unit)
// For Windows, it is also dependent on the Open SSL DLLs, located here:
//   https://github.com/grijjy/DelphiOpenSsl/tree/master/Bin
// The DLLs for the relevant platform (i.e. Win32 or Win64) will need to be in the same folder as your executable, or in the system path

interface

{$SCOPEDENUMS ON}

uses
  // RTL
  System.JSON, System.Classes;

const
  cFCMHTTPv1SendURL = 'https://fcm.googleapis.com/v1';
  cFCMHTTPv1SendResourceProjectID = 'projectid';
  cFCMHTTPv1SendResource = '/projects/{projectid}/messages:send';
  cFCMGrantType = 'urn:ietf:params:oauth:grant-type:jwt-bearer';

type
  TServiceAccount = record
    AuthURI: string;
    ClientEmail: string;
    ClientID: string;
    IsValid: Boolean;
    PrivateKey: string;
    ProjectID: string;
    TokenURI: string;
    function Parse(const AJSON: string): Boolean;
  end;

  TBearerToken = record
    access_token: string;
    expires_in: Integer;
    ext_expires_in: Integer;
    id_token: string;
    refresh_token: string;
    scope: string;
    token_type: string;
    TokenDateTime: TDateTime;
    function IsExpired: Boolean;
    procedure LoadFromFile(const AFileName: string);
    procedure Parse(const AJSON: string);
    procedure Reset;
    procedure SaveToFile(const AFileName: string);
    function ToJSON: string;
  end;

  TFCMMessageOption = (BigImage, BigText, ContentAvailable);
  TFCMMessageOptions = set of TFCMMessageOption;
  TFCMMessagePriority = (None, Normal, High);

  TFCMMessage = class(TObject)
  private
    FBadgeCount: Integer;
    FBody: string;
    FChannelID: string;
    FClickAction: string;
    FData: string;
    FImageURL: string;
    FIsCritical: Boolean;
    FIsDataOnly: Boolean;
    FIsSilent: Boolean;
    FOptions: TFCMMessageOptions;
    FPriority: TFCMMessagePriority;
    FSoundName: string;
    FSoundVolume: Single;
    FTitle: string;
    function GetAndroidJSONValue: TJSONValue;
    function GetAndroidNotificationJSONValue: TJSONValue;
    function GetAPNSJSONValue: TJSONValue;
    function GetDataJSONValue: TJSONValue;
    procedure SetSoundVolume(const Value: Single);
  public
    constructor Create;
    function GetPayload(const ATo: string; const AIsTopic: Boolean): string;
    function GetTokenPayload(const AToken: string): string;
    function GetTopicPayload(const ATopic: string): string;
    procedure ResetAll;
    procedure ResetContent;
    property BadgeCount: Integer read FBadgeCount write FBadgeCount;
    property Body: string read FBody write FBody;
    property ChannelID: string read FChannelID write FChannelID;
    property ClickAction: string read FClickAction write FClickAction;
    /// <summary>
    ///   Additional information to include in the data member of the payload. Needs to be valid JSON, with string properties *only*
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    ///   Used by the Kastri client-side implementation of FCM to display an optional image with the notification
    /// </summary>
    property ImageURL: string read FImageURL write FImageURL;
    /// <summary>
    ///   Indicates that the message is critical - currently applies to iOS only
    /// </summary>
    property IsCritical: Boolean read FIsCritical write FIsCritical;
    /// <summary>
    ///   Indicates whether or not a notification member should be included in the Android part of the payload
    /// </summary>
    /// <remarks>
    ///   This should be set to True if the Kastri client-side implementation of FCM for Android is being used, so that messages can be handled
    ///   in both the *background* and foreground. Please see this link for further explanation:
    ///     https://firebase.google.com/docs/cloud-messaging/android/receive
    /// </remarks>
    property IsDataOnly: Boolean read FIsDataOnly write FIsDataOnly;
    /// <summary>
    ///   Used by the Kastri client-side implementation of FCM to indicate that a notification should not be shown
    /// </summary>
    property IsSilent: Boolean read FIsSilent write FIsSilent;
    property Options: TFCMMessageOptions read FOptions write FOptions;
    property Priority: TFCMMessagePriority read FPriority write FPriority;
    property SoundName: string read FSoundName write FSoundName;
    property SoundVolume: Single read FSoundVolume write SetSoundVolume;
    property Title: string read FTitle write FTitle;
  end;

  TFCMSenderErrorKind = (AccessTokenFailure, PostFailure);

  TFCMSenderErrorKindHelper = record helper for TFCMSenderErrorKind
    function ToString: string;
  end;

  TFCMSenderError = record
    Content: string;
    ErrorMessage: string;
    Kind: TFCMSenderErrorKind;
    constructor Create(const AKind: TFCMSenderErrorKind; const AContent, AErrorMessage: string);
  end;

  TFCMSenderErrorEvent = procedure(Sender: TObject; const Error: TFCMSenderError) of object;

  TFCMSenderResponse = record
    Request: string;
    Response: string;
    constructor Create(const ARequest, AResponse: string);
  end;

  TFCMSenderResponseEvent = procedure(Sender: TObject; const Response: TFCMSenderResponse) of object;

  /// <summary>
  ///   Base class for sending FCM messages
  /// </summary>
  /// <remarks>
  ///   Descend from this class and provide the comms (eg plain HTTP, or REST components), overriding DoPost and DoGetAccessToken
  ///   It is envisaged that this class will normally be used in a thread, service, webserver, or other server environment
  /// </remarks>
  TCustomFCMSender = class(TObject)
  private
    FBearerToken: TBearerToken;
    FServiceAccount: TServiceAccount;
    FOnError: TFCMSenderErrorEvent;
    FOnNewAccessToken: TNotifyEvent;
    FOnResponse: TFCMSenderResponseEvent;
  protected
    procedure DoError(const AError: TFCMSenderError); virtual;
    function DoPost(const AJSON: string): Boolean; virtual; abstract;
    function DoGetAccessToken(const AJWT: string): Boolean; virtual; abstract;
    procedure DoNewAccessToken; virtual;
    procedure DoResponse(const AResponse: TFCMSenderResponse); virtual;
    function GenerateJWT: string;
    function GetAccessToken: Boolean;
  public
    function LoadServiceAccount(const AFileName: string): Boolean;
    function Post(const AJSON: string): Boolean;
    property BearerToken: TBearerToken read FBearerToken;
    property ServiceAccount: TServiceAccount read FServiceAccount write FServiceAccount;
    property OnError: TFCMSenderErrorEvent read FOnError write FOnError;
    property OnNewAccessToken: TNotifyEvent read FOnNewAccessToken write FOnNewAccessToken;
    property OnResponse: TFCMSenderResponseEvent read FOnResponse write FOnResponse;
  end;

  /// <summary>
  ///   TCustomFCMSender descendant that implements comms using plain HTTP
  /// </summary>
  TFCMSender = class(TCustomFCMSender)
  protected
    function DoGetAccessToken(const AJWT: string): Boolean; override;
    function DoPost(const AJSON: string): Boolean; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils, System.Net.HttpClient, System.Net.URLClient, System.NetEncoding,  System.NetConsts,
  System.Hash, System.IOUtils, System.TypInfo, System.Generics.Collections,
  // Grijjy - from: https://github.com/grijjy/DelphiOpenSsl/blob/master/OpenSSL.Api_11.pas
  OpenSSL.Api_11;

const
  CONTENTTYPE_APPLICATION_JSON = 'application/json'; // do not localize
  CONTENTTYPE_APPLICATION_X_WWW_FORM_URLENCODED = 'application/x-www-form-urlencoded'; // do not localize
  cFCMHTTPv1SendURLTemplate = cFCMHTTPv1SendURL + '/projects/%s/messages:send'; // eg https://fcm.googleapis.com/v1/projects/delphi-worlds-test/messages:send
  cFCMAccessTokenRequestTemplate = 'grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=%s';
  cFCMJWTScopes = 'https://www.googleapis.com/auth/firebase.messaging';
  cFCMJWTAudience = 'https://oauth2.googleapis.com/token';

  cFCMMessagePriorityValues: array[TFCMMessagePriority] of string = ('', 'normal', 'high');

type
  TGoogleJWTHeader = record
    Algorithm: string;
    TokenType: string;
    function Generate(const AEncoding: TNetEncoding): string;
  end;

  TGoogleJWTClaim = record
    Iss: string;
    Scope: string;
    Aud: string;
    function Generate(const AEncoding: TNetEncoding): string;
  end;

  TGoogleJWT = record
    Header: TGoogleJWTHeader;
    Claim: TGoogleJWTClaim;
    function GenerateToken(const APrivateKey: string): string;
    function SignSHA256WithRSA(const APrivateKey: TBytes; const AData: TBytes): TBytes;
  end;

{ TGoogleJWTHeader }

function TGoogleJWTHeader.Generate(const AEncoding: TNetEncoding): string;
begin
  Result := AEncoding.Encode(Format('{"alg":"%s", "typ":"%s"}', [Algorithm, TokenType]));
end;

{ TGoogleJWTClaim }

function TGoogleJWTClaim.Generate(const AEncoding: TNetEncoding): string;
var
  LIAT, LExp: Int64;
  LJSON: string;
begin
  LIAT := DateTimeToUnix(Now, False);
  LExp := DateTimeToUnix(IncHour(Now, 1), False);
  LJSON := Format('{"iss":"%s", "scope":"%s", "aud":"%s", "exp":%d, "iat":%d}', [Iss, Scope, Aud, LExp, LIAT]);
  Result := AEncoding.Encode(LJSON);
end;

{ TGoogleJWT }

function TGoogleJWT.GenerateToken(const APrivateKey: string): string;
var
  LBase64: TBase64Encoding;
  LSignable: string;
  LSignature: TBytes;
begin
  Header.Algorithm := 'RS256';
  Header.TokenType := 'JWT';
  Claim.Aud := cFCMJWTAudience;
  Claim.Scope := cFCMJWTScopes;
  LBase64 := TBase64Encoding.Create(0, '');
  try
    LSignable := Header.Generate(LBase64) + '.' + Claim.Generate(LBase64);
    LSignature := SignSHA256WithRSA(TEncoding.UTF8.GetBytes(APrivateKey), TEncoding.UTF8.GetBytes(LSignable));
    Result := LSignable + '.' + LBase64.EncodeBytesToString(LSignature);
  finally
    LBase64.Free;
  end;
end;

function TGoogleJWT.SignSHA256WithRSA(const APrivateKey: TBytes; const AData: TBytes): TBytes;
var
  LPrivateKeyRef: PBIO;
  LPrivateKey: PEVP_PKEY;
  LContext: PEVP_MD_CTX;
  SHA256: PEVP_MD;
  LSize: NativeUInt;
begin
	LPrivateKeyRef := BIO_new_mem_buf(@APrivateKey[0], Length(APrivateKey));
  try
    LPrivateKey := PEM_read_bio_PrivateKey(LPrivateKeyRef, nil, nil, nil);
    try
      LContext := EVP_MD_CTX_create;
      try
        SHA256 := EVP_sha256;
        if (EVP_DigestSignInit(LContext, nil, SHA256, nil, LPrivateKey) > 0) and
          (EVP_DigestUpdate(LContext, @AData[0], Length(AData)) > 0) and
          (EVP_DigestSignFinal(LContext, nil, LSize) > 0) then
        begin
          SetLength(Result, LSize);
          if EVP_DigestSignFinal(LContext, @Result[0], LSize) = 0 then
            SetLength(Result, 0);
        end;
      finally
        EVP_MD_CTX_destroy(LContext);
      end;
    finally
      EVP_PKEY_free(LPrivateKey);
    end;
  finally
	  BIO_free(LPrivateKeyRef);
  end;
end;

{ TServiceAccount }

function TServiceAccount.Parse(const AJSON: string): Boolean;
var
  LJSONValue: TJSONValue;
begin
  Result := False;
  IsValid := False;
  ClientID := '';
  ClientEmail := '';
  ProjectID := '';
  PrivateKey := '';
  AuthURI := '';
  TokenURI := '';
  if not AJSON.IsEmpty then
  begin
    LJSONValue := TJsonObject.ParseJSONValue(AJSON);
    if LJSONValue <> nil then
    try
      IsValid := LJSONValue.TryGetValue('client_id', ClientID)
        and LJSONValue.TryGetValue('client_email', ClientEmail)
        and LJSONValue.TryGetValue('private_key', PrivateKey)
        and LJSONValue.TryGetValue('project_id', ProjectID)
        and LJSONValue.TryGetValue('auth_uri', AuthURI)
        and LJSONValue.TryGetValue('token_uri', TokenURI);
      Result := IsValid;
    finally
      LJSONValue.Free;
    end;
  end;
end;

{ TBearerToken }

procedure TBearerToken.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    Parse(TFile.ReadAllText(AFileName));
end;

procedure TBearerToken.Parse(const AJSON: string);
var
  LDateTime: string;
  LValue: TJSONValue;
begin
  Reset;
  LValue := TJSONObject.ParseJSONValue(AJSON);
  if LValue <> nil then
  try
    LValue.TryGetValue('access_token', access_token);
    LValue.TryGetValue('expires_in', expires_in);
    LValue.TryGetValue('ext_expires_in', ext_expires_in);
    LValue.TryGetValue('id_token', id_token);
    LValue.TryGetValue('refresh_token', refresh_token);
    LValue.TryGetValue('scope', scope);
    LValue.TryGetValue('token_type', token_type);
    // Note: This is *NOT* returned by the server - it is used for determining whether or not the token has expired
    if LValue.TryGetValue('token_datetime', LDateTime) then
      TokenDateTime := ISO8601ToDate(LDateTime, False)
    else
      TokenDateTime := Now;
  finally
    LValue.Free;
  end;
end;

function TBearerToken.IsExpired: Boolean;
begin
  Result := access_token.IsEmpty or (SecondsBetween(Now, TokenDateTime) > expires_in);
end;

procedure TBearerToken.Reset;
begin
  access_token := '';
  expires_in := 0;
  ext_expires_in := 0;
  id_token := '';
  refresh_token := '';
  scope := '';
  token_type := '';
  TokenDateTime := 0;
end;

procedure TBearerToken.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, ToJSON);
end;

function TBearerToken.ToJSON: string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('access_token', access_token);
    LJSON.AddPair('expires_in', expires_in.ToString);
    LJSON.AddPair('ext_expires_in', ext_expires_in.ToString);
    LJSON.AddPair('id_token', id_token);
    LJSON.AddPair('refresh_token', refresh_token);
    LJSON.AddPair('scope', scope);
    LJSON.AddPair('token_type', token_type);
    LJSON.AddPair('token_datetime', DateToISO8601(TokenDateTime, False));
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

{ TFCMMessage }

constructor TFCMMessage.Create;
begin
  inherited;
  ResetAll;
end;

procedure TFCMMessage.ResetAll;
begin
  FChannelID := '';
  ResetContent;
end;

procedure TFCMMessage.ResetContent;
begin
  FBadgeCount := -1;
  FBody := '';
  FClickAction := '';
  FData := '';
  FTitle := '';
  FImageURL := '';
  FOptions := [];
  FSoundName := '';
  FIsCritical := False;
  FSoundVolume := -1;
end;

procedure TFCMMessage.SetSoundVolume(const Value: Single);
begin
  FSoundVolume := Value;
end;

// https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification?language=objc#2943363
//   See Table 1
function TFCMMessage.GetAPNSJSONValue: TJSONValue;
var
  LAPNS, LPayload, LAPS, LAlert, LDataValue, LSoundValue: TJSONObject;
  LNeedsAPNS: Boolean;
  I: Integer;
  LPair: TJSONPair;
begin
  Result := nil;
  LNeedsAPNS := False;
  LAPNS := TJSONObject.Create;
  try
    LPayload := TJSONObject.Create;
    LAPNS.AddPair('payload', LPayload);
    LAPS := TJSONObject.Create;
    LPayload.AddPair('aps', LAPS);
    if not FSoundName.IsEmpty or FIsCritical then
    begin
      LSoundValue := TJSONObject.Create;
      LSoundValue.AddPair('critical', Ord(FIsCritical));
      if FSoundName.IsEmpty then
        LSoundValue.AddPair('name', 'default')
      else
        LSoundValue.AddPair('name', FSoundName);
      if FSoundVolume = -1 then
        LSoundValue.AddPair('volume', 1)
      else
        LSoundValue.AddPair('volume', FSoundVolume);
      LAPS.AddPair('sound', LSoundValue);
    end;
    if FBadgeCount >= 0 then
      LAPS.AddPair('badge', TJSONNumber.Create(FBadgeCount));
    // "Data only" notifications need an APS alert member
    if FIsDataOnly then
    begin
      LAlert := TJSONObject.Create;
      LAlert.AddPair('title', TJSONString.Create(FTitle));
      LAlert.AddPair('body', TJSONString.Create(FBody));
      LAPS.AddPair('alert', LAlert);
    end;
    if TFCMMessageOption.ContentAvailable in FOptions then
      LAPS.AddPair('content-available', TJSONNumber.Create(1));
    if not FClickAction.IsEmpty then
      LAPS.AddPair('category', FClickAction);
    // Add any data JSON values to the APS member
    if not FData.IsEmpty then
    begin
      LDataValue := TJSONObject(TJSONObject.ParseJSONValue(FData));
      if LDataValue <> nil then
      try
        for I := 0 to LDataValue.Count - 1 do
        begin
          LPair := LDataValue.Pairs[I];
          LPayload.AddPair(LPair.JsonString.Value, TJSONValue(LPair.JsonValue.Clone));
        end;
      finally
        LDataValue.Free;
      end;
    end;
    // Add this LAST
    if LAPS.Count > 0 then
      LAPS.AddPair('mutable-content', TJSONNumber.Create(1));
    // imageUrl is used in FCM handling code in Kastri
    if not FImageURL.IsEmpty then
      LPayload.AddPair('imageUrl', FImageURL);
    LNeedsAPNS := (LPayload.Count > 0) or (LAPS.Count > 0);
  finally
    if LNeedsAPNS then
      Result := LAPNS
    else
      LAPNS.Free;
  end;
end;

function TFCMMessage.GetDataJSONValue: TJSONValue;
var
  LData: TJSONObject;
  LValue: TJSONValue;
  LHasData: Boolean;
begin
  Result := nil;
  LHasData := False;
  LValue := TJSONObject.ParseJSONValue(FData);
  if LValue is TJSONObject then
    LData := TJSONObject(LValue)
  else
    LData := TJSONObject.Create;
  try
    // The big_text and big_image flags are specific to FCM handling code in Kastri
    if TFCMMessageOption.BigText in FOptions then
      LData.AddPair('big_text', '1');
    if not FImageURL.IsEmpty and (TFCMMessageOption.BigImage in FOptions) then
      LData.AddPair('big_image', '1');
    if IsSilent then
      LData.AddPair('isSilent', '1');
    if FIsDataOnly then
    begin
      LData.AddPair('title', TJSONString.Create(FTitle));
      LData.AddPair('body', TJSONString.Create(FBody));
      if not FChannelID.IsEmpty then
        LData.AddPair('channel_id', FChannelID);
      // imageUrl is used in FCM handling code in Kastri
      if not FImageURL.IsEmpty then
        LData.AddPair('imageUrl', TJSONString.Create(FImageURL));
    end;
    LHasData := LData.Count > 0;
  finally
    if LHasData then
      Result := LData
    else
      LData.Free;
  end;
end;

function TFCMMessage.GetAndroidNotificationJSONValue: TJSONValue;
var
  LNotification: TJSONObject;
  LHasProps: Boolean;
begin
  Result := nil;
  LHasProps := False;
  LNotification := TJSONObject.Create;
  try
    if not FClickAction.IsEmpty then
      LNotification.AddPair('click_action', FClickAction);
    if not FChannelID.IsEmpty then
      LNotification.AddPair('channel_id', FChannelID);
    if not FIsDataOnly then
    begin
      if not FImageURL.IsEmpty then
        LNotification.AddPair('image', FImageURL);
      if not FSoundName.IsEmpty then
        LNotification.AddPair('sound', FSoundName);
    end;
    LHasProps := LNotification.Count > 0;
  finally
    if LHasProps then
      Result := LNotification
    else
      LNotification.Free;
  end;
end;

function TFCMMessage.GetAndroidJSONValue: TJSONValue;
var
  LAndroid: TJSONObject;
  LNotification: TJSONValue;
  LHasProps: Boolean;
begin
  Result := nil;
  LHasProps := False;
  LAndroid := TJSONObject.Create;
  try
    if Ord(FPriority) > 0 then
     LAndroid.AddPair('priority', cFCMMessagePriorityValues[FPriority]);
    if not IsDataOnly then
    begin
      LNotification := GetAndroidNotificationJSONValue;
      if LNotification <> nil then
        LAndroid.AddPair('notification', LNotification);
    end;
    LHasProps := LAndroid.Count > 0;
  finally
    if LHasProps then
      Result := LAndroid
    else
      LAndroid.Free;
  end;
end;

// https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
function TFCMMessage.GetPayload(const ATo: string; const AIsTopic: Boolean): string;
var
  LJSON, LMessage, LNotification: TJSONObject;
  LAndroid, LAPNS, LData: TJSONValue;
begin
  LJSON := TJSONObject.Create;
  try
    LMessage := TJSONObject.Create;
    LJSON.AddPair('message', LMessage);
    if AIsTopic then
      LMessage.AddPair('topic', TJSONString.Create(ATo))
    else
      LMessage.AddPair('token', TJSONString.Create(ATo));
    LData := GetDataJSONValue;
    if LData <> nil then
      LMessage.AddPair('data', LData);
    if not FIsDataOnly then
    begin
      LNotification := TJSONObject.Create;
      LNotification.AddPair('title', FTitle.Substring(0, 100));
      if not FBody.IsEmpty then
        LNotification.AddPair('body', TJSONString.Create(FBody));
      if not FImageURL.IsEmpty then
        LNotification.AddPair('image', FImageURL);
      LMessage.AddPair('notification', LNotification);
    end;
    LAndroid := GetAndroidJSONValue;
    if LAndroid <> nil then
      LMessage.AddPair('android', LAndroid);
    LAPNS := GetAPNSJSONValue;
    if LAPNS <> nil then
      LMessage.AddPair('apns', LAPNS);
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

function TFCMMessage.GetTokenPayload(const AToken: string): string;
begin
  Result := GetPayload(AToken, False);
end;

function TFCMMessage.GetTopicPayload(const ATopic: string): string;
begin
  Result := GetPayload(ATopic, True);
end;

{ TFCMSenderError }

constructor TFCMSenderError.Create(const AKind: TFCMSenderErrorKind; const AContent, AErrorMessage: string);
begin
  Kind := AKind;
  Content := AContent;
  ErrorMessage := AErrorMessage;
end;

{ TFCMSenderResponse }

constructor TFCMSenderResponse.Create(const ARequest, AResponse: string);
begin
  Request := ARequest;
  Response := AResponse;
end;

{ TTFCMSenderErrorKindHelper }

function TFCMSenderErrorKindHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TFCMSenderErrorKind), Ord(Self));
end;

{ TCustomFCMSender }

function TCustomFCMSender.LoadServiceAccount(const AFileName: string): Boolean;
var
  LJSON: string;
begin
  LJSON := '';
  if TFile.Exists(AFileName) then
    LJSON := TFile.ReadAllText(AFileName);
  Result := FServiceAccount.Parse(LJSON);
end;

function TCustomFCMSender.Post(const AJSON: string): Boolean;
begin
  Result := False;
  if not BearerToken.IsExpired or GetAccessToken then
  try
    Result := DoPost(AJSON);
  except
    on E: Exception do
      DoError(TFCMSenderError.Create(TFCMSenderErrorKind.PostFailure, AJSON, Format('%s: %s', [E.ClassName, E.Message])));
  end;
end;

procedure TCustomFCMSender.DoError(const AError: TFCMSenderError);
begin
  if Assigned(FOnError) then
    FOnError(Self, AError);
end;

procedure TCustomFCMSender.DoNewAccessToken;
begin
  if Assigned(FOnNewAccessToken) then
    FOnNewAccessToken(Self);
end;

procedure TCustomFCMSender.DoResponse(const AResponse: TFCMSenderResponse);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse);
end;

function TCustomFCMSender.GenerateJWT: string;
var
  LJWT: TGoogleJWT;
begin
  Result := '';
  if FServiceAccount.IsValid then
  begin
    LJWT.Claim.Iss := FServiceAccount.ClientEmail;
    Result := LJWT.GenerateToken(FServiceAccount.PrivateKey);
  end;
end;

function TCustomFCMSender.GetAccessToken: Boolean;
var
  LJWT: string;
begin
  Result := False;
  LJWT := GenerateJWT;
  if not LJWT.IsEmpty then
  try
    Result := DoGetAccessToken(LJWT) and not FBearerToken.IsExpired;
    if Result then
      DoNewAccessToken;
  except
    on E: Exception do
      DoError(TFCMSenderError.Create(TFCMSenderErrorKind.AccessTokenFailure, LJWT, Format('%s: %s', [E.ClassName, E.Message])));
  end;
end;

{ TFCMSender }

function TFCMSender.DoGetAccessToken(const AJWT: string): Boolean;
var
  LContent, LResponseString: string;
  LRequest: TStream;
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  Result := False;
  LContent := Format(cFCMAccessTokenRequestTemplate, [TNetEncoding.URL.EncodeForm(AJWT)]);
  LHTTP := THTTPClient.Create;
  try
    LHTTP.ContentType := CONTENTTYPE_APPLICATION_X_WWW_FORM_URLENCODED;
    LRequest := TStringStream.Create(LContent);
    try
      LResponse := LHTTP.Post(ServiceAccount.TokenURI, LRequest);
    finally
      LRequest.Free;
    end;
  finally
    LHTTP.Free;
  end;
  LResponseString := LResponse.ContentAsString;
  if LResponse.StatusCode = 200 then
  begin
    BearerToken.Parse(LResponseString);
    Result := True;
  end;
end;

function TFCMSender.DoPost(const AJSON: string): Boolean;
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
  LRequest: TStream;
  LErrorMessage: string;
begin
  LHTTP := THTTPClient.Create;
  try
    LHTTP.Accept := CONTENTTYPE_APPLICATION_JSON;
    LHTTP.ContentType := CONTENTTYPE_APPLICATION_JSON;
    LHTTP.CustomHeaders['Authorization'] := 'Bearer ' + BearerToken.access_token;
    LRequest := TStringStream.Create(AJSON, TEncoding.UTF8);
    try
      LResponse := LHTTP.Post(Format(cFCMHTTPv1SendURLTemplate, [ServiceAccount.ProjectID]), LRequest);
    finally
      LRequest.Free;
    end;
    Result := LResponse.StatusCode = 200;
    if not Result then
    begin
      LErrorMessage := Format('%d: %s - %s', [LResponse.StatusCode, LResponse.StatusText, LResponse.ContentAsString]);
      DoError(TFCMSenderError.Create(TFCMSenderErrorKind.PostFailure, AJSON, LErrorMessage));
    end
    else
      DoResponse(TFCMSenderResponse.Create(AJSON, LResponse.ContentAsString));
  finally
    LHTTP.Free;
  end;
end;

end.
