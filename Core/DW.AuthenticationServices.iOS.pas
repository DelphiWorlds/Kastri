unit DW.AuthenticationServices.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.iOSapi.AuthenticationServices, DW.AuthenticationServices.Types;

type
  TAuthenticationServices = class;

  TAuthorizationControllerDelegate = class(TOCLocal, ASAuthorizationControllerDelegate)
  private
    FServices: TAuthenticationServices;
    procedure SendResponseMessage(const AResponse: TAppleIDAuthorizationResponse);
  public
    { ASAuthorizationControllerDelegate }
    [MethodName('authorizationController:didCompleteWithAuthorization:')]
    procedure authorizationController(controller: ASAuthorizationController; authorization: ASAuthorization); overload; cdecl;
    [MethodName('authorizationController:didCompleteWithError:')]
    procedure authorizationController(controller: ASAuthorizationController; error: NSError); overload; cdecl;
  public
    constructor Create(const AServices: TAuthenticationServices);
  end;

  TAuthorizationControllerPresentationContextProvider = class(TOCLocal, ASAuthorizationControllerPresentationContextProviding)
  private
    FServices: TAuthenticationServices;
  public
    { ASAuthorizationControllerPresentationContextProviding }
    function presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor; cdecl;
  public
    constructor Create(const AServices: TAuthenticationServices);
  end;

  TAuthenticationServices = class(TObject)
  private
    class var FCurrent: TAuthenticationServices;
    class destructor DestroyClass;
    class function GetCurrent: TAuthenticationServices; static;
  private
    FAppleIDProvider: ASAuthorizationAppleIDProvider;
    FController: ASAuthorizationController;
    FControllerDelegate: TAuthorizationControllerDelegate;
    FControllerPresentationContextProvider: TAuthorizationControllerPresentationContextProvider;
    FPasswordProvider: ASAuthorizationPasswordProvider;
    function GetToken: string;
    procedure SetupController(const ARequests: TArray<NSObject>);
    procedure SetToken(const Value: string);
  protected
    property Token: string read GetToken write SetToken;
  public
    class property Current: TAuthenticationServices read GetCurrent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PerformExistingAccountSetup;
    procedure RequestAuthorization;
  end;

implementation

uses
  // RTL
  System.Messaging, System.SysUtils, System.Hash, System.DateUtils,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Helpers,
  // DW
  DW.OSLog,
  DW.UserDefaults.iOS;

function NSObjectArrayToNSArray(const AValues: TArray<NSObject>): NSArray;
var
  LArray: TArray<Pointer>;
  I: Integer;
begin
  SetLength(LArray, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    LArray[I] := NSObjectToID(AValues[I]);
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArray[0], Length(LArray)));
end;

{ TAuthorizationControllerDelegate }

constructor TAuthorizationControllerDelegate.Create(const AServices: TAuthenticationServices);
begin
  inherited Create;
  FServices := AServices;
end;

procedure TAuthorizationControllerDelegate.SendResponseMessage(const AResponse: TAppleIDAuthorizationResponse);
begin
  // Might need to synch this?
  TMessageManager.DefaultManager.SendMessage(nil, TAppleIDAuthorizationResponseMessage.Create(AResponse), True);
end;

procedure TAuthorizationControllerDelegate.authorizationController(controller: ASAuthorizationController; authorization: ASAuthorization);
var
  LCredential: NSObject;
  LAppleIDCredential: ASAuthorizationAppleIDCredential;
  LPasswordCredential: ASPasswordCredential;
  LCredentials: TAppleIDCredentials;
  LResponse: TAppleIDAuthorizationResponse;
  LToken, LAuthCode: NSString;
  LTokenStr: string;
begin
  LCredential := TNSObject.Wrap(authorization.credential);
  if LCredential.isKindOfClass(objc_getClass('ASAuthorizationAppleIDCredential')) then
  begin
    LResponse.FailReason := TAppleIDAuthorizationFailReason.None;
    LAppleIDCredential := TASAuthorizationAppleIDCredential.Wrap(authorization.credential);
    LTokenStr := '';
    if LAppleIDCredential.identityToken <> nil then
    begin
      LToken := TNSString.Wrap(TNSString.Alloc.initWithData(LAppleIDCredential.identityToken, NSUTF8StringEncoding));
      if LToken <> nil then
        LTokenStr := NSStrToStr(LToken);
    end;
    TOSLog.d('Tokens: (stored) %s vs %s (returned)', [FServices.Token, LTokenStr]);
    case LResponse.FailReason of
      TAppleIDAuthorizationFailReason.None:
      begin
        LCredentials.Credential := TAppleIDAuthorizationCredential.AppleID;
        LCredentials.User := NSStrToStr(LAppleIDCredential.user);
        if LAppleIDCredential.fullName <> nil then
        begin
          LCredentials.GivenName := NSStrToStr(LAppleIDCredential.fullName.givenName);
          LCredentials.FamilyName := NSStrToStr(LAppleIDCredential.fullName.familyName);
        end;
        if LAppleIDCredential.email <> nil then
          LCredentials.EMail := NSStrToStr(LAppleIDCredential.email);
        if LAppleIDCredential.authorizationCode <> nil then
        begin
          LAuthCode := TNSString.Wrap(TNSString.Alloc.initWithData(LAppleIDCredential.authorizationCode, NSUTF8StringEncoding));
          if LAuthCode <> nil then
            LCredentials.AuthorizationCode := NSStrToStr(LAuthCode);
        end;
        TOSLog.d('Saving token from response: ' + LTokenStr);
        FServices.Token := LTokenStr;
        LResponse.Credentials := LCredentials;
        SendResponseMessage(LResponse);
      end;
      TAppleIDAuthorizationFailReason.Failed:
      begin
        TOSLog.d('TAppleIDAuthorizationFailReason.Failed');
        FServices.Token := '';
      end;
    end;
  end
  else if LCredential.isKindOfClass(objc_getClass('ASPasswordCredential')) then
  begin
    LPasswordCredential := TASPasswordCredential.Wrap(authorization.credential);
    LCredentials.Credential := TAppleIDAuthorizationCredential.Password;
    LCredentials.User := NSStrToStr(LPasswordCredential.user);
    LCredentials.Password := NSStrToStr(LPasswordCredential.password);
    LResponse.FailReason := TAppleIDAuthorizationFailReason.None;
    LResponse.Credentials := LCredentials;
    SendResponseMessage(LResponse);
  end;
end;

procedure TAuthorizationControllerDelegate.authorizationController(controller: ASAuthorizationController; error: NSError);
var
  LResponse: TAppleIDAuthorizationResponse;
begin
  LResponse.FailReason := TAppleIDAuthorizationFailReason.Unknown;
  case error.code of
    ASAuthorizationErrorCanceled:
      LResponse.FailReason := TAppleIDAuthorizationFailReason.Canceled;
    ASAuthorizationErrorFailed:
      LResponse.FailReason := TAppleIDAuthorizationFailReason.Failed;
    ASAuthorizationErrorInvalidResponse:
      LResponse.FailReason := TAppleIDAuthorizationFailReason.InvalidResponse;
    ASAuthorizationErrorNotHandled:
      LResponse.FailReason := TAppleIDAuthorizationFailReason.NotHandled;
  end;
  LResponse.Credentials.Credential := TAppleIDAuthorizationCredential.Invalid;
  SendResponseMessage(LResponse);
end;

{ TAuthorizationControllerPresentationContextProvider }

constructor TAuthorizationControllerPresentationContextProvider.Create(const AServices: TAuthenticationServices);
begin
  inherited Create;
  FServices := AServices;
end;

function TAuthorizationControllerPresentationContextProvider.presentationAnchorForAuthorizationController(
  controller: ASAuthorizationController): ASPresentationAnchor;
begin
  Result := TiOSHelper.SharedApplication.keyWindow;
end;

{ TAuthenticationServices }

constructor TAuthenticationServices.Create;
begin
  inherited;
  FAppleIDProvider := TASAuthorizationAppleIDProvider.Create;
  FPasswordProvider := TASAuthorizationPasswordProvider.Create;
  FControllerDelegate := TAuthorizationControllerDelegate.Create(Self);
  FControllerPresentationContextProvider := TAuthorizationControllerPresentationContextProvider.Create(Self);
end;

destructor TAuthenticationServices.Destroy;
begin
  FAppleIDProvider := nil;
  FPasswordProvider := nil;
  FController := nil;
  FControllerDelegate.Free;
  FControllerPresentationContextProvider.Free;
  inherited;
end;

class destructor TAuthenticationServices.DestroyClass;
begin
  FCurrent.Free;
end;

class function TAuthenticationServices.GetCurrent: TAuthenticationServices;
begin
  if FCurrent = nil then
    FCurrent := TAuthenticationServices.Create;
  Result := FCurrent;
end;

function TAuthenticationServices.GetToken: string;
begin
  Result := TUserDefaults.GetValue('AppleIDToken', '');
end;

procedure TAuthenticationServices.SetToken(const Value: string);
begin
  TUserDefaults.SetValue('AppleIDToken', Value);
end;

procedure TAuthenticationServices.PerformExistingAccountSetup;
begin
  SetupController([FAppleIDProvider.createRequest, FPasswordProvider.createRequest]);
  FController.performRequests;
end;

procedure TAuthenticationServices.SetupController(const ARequests: TArray<NSObject>);
begin
  FController := nil;
  FController := TASAuthorizationController.Wrap(TASAuthorizationController.OCClass.alloc);
  FController := TASAuthorizationController.Wrap(FController.initWithAuthorizationRequests(NSObjectArrayToNSArray(ARequests)));
  FController.setDelegate(FControllerDelegate.GetObjectID);
  FController.setPresentationContextProvider(FControllerPresentationContextProvider.GetObjectID);
end;

procedure TAuthenticationServices.RequestAuthorization;
var
  LRequest: ASAuthorizationAppleIDRequest;
begin
  LRequest := FAppleIDProvider.createRequest;
  LRequest.setRequestedScopes(NSObjectArrayToNSArray([ASAuthorizationScopeFullName, ASAuthorizationScopeEmail]));
  SetupController([LRequest]);
  FController.performRequests;
end;

end.
