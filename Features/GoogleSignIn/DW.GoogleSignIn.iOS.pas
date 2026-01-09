unit DW.GoogleSignIn.iOS;

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

// Docs for Google SignIn on iOS:
//   https://firebase.google.com/docs/auth/ios/google-signin#objective-c_3

interface

uses
  System.TypInfo,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.UIKit,
  iOSapi.Foundation,
  // DW
  DW.GoogleSignIn, DW.iOSapi.GoogleSignIn;

type
  ASPresentationAnchor = UIWindow;

  ASAuthorizationControllerClass = interface(NSObjectClass)
    ['{7DF2E170-3728-4358-81F6-6CDEABEAE00D}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationController = interface(NSObject)
    ['{24C55963-DDD2-438A-B9B1-DFE6A42B989D}']
  end;
  TASAuthorizationController = class(TOCGenericImport<ASAuthorizationControllerClass, ASAuthorizationController>) end;

  SignInController = interface(UIViewController)
    ['{326056D7-C8B1-43B9-819A-60A7AF412D6E}']
    { ASAuthorizationControllerPresentationContextProviding }
    function presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor; cdecl;
  end;

  TSignInController = class(TOCLocal)
  private
    function GetViewController: UIViewController;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { SignInController }
    function presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor; cdecl;
  public
    property ViewController: UIViewController read GetViewController;
  end;

  TPlatformGoogleSignIn = class(TCustomPlatformGoogleSignIn)
  private
    FIsSignedIn: Boolean;
    FScopes: TArray<string>;
    FSignin: GIDSignIn;
    FSignInController: TSignInController;
    procedure AddScopesCompletion(signInResult: GIDSignInResult; error: NSError);
    procedure DisconnectCompletion(error: NSError);
    procedure DoHandleSignIn(const ASignInResult: GIDSignInResult);
    function GetSignInDetails(const AUser: GIDGoogleUser): TSignInDetails;
    procedure HandleSignIn(const ASignInResult: GIDSignInResult);
    procedure HandleSignInError(const AError: NSError);
    procedure RestorePreviousSignInCompletion(user: GIDGoogleUser; error: NSError);
    procedure SignInCompletion(signInResult: GIDSignInResult; error: NSError);
  public
    constructor Create(const AGoogleSignIn: TGoogleSignIn); override;
    destructor Destroy; override;
    function IsSignedIn: Boolean; override;
    procedure RestorePreviousSignIn; override;
    procedure RevokeAccess; override;
    procedure SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean); override;
    procedure SignOut; override;
  end;

implementation

uses
  DW.OSLog,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers,
  FMX.Forms, FMX.Platform.iOS;

{ TSignInController }

function TSignInController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(SignInController);
end;

function TSignInController.presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor;
begin
  TOSLog.d('TSignInController.presentationAnchorForAuthorizationController');
  Result := ViewController.view.window;
end;

function TSignInController.GetViewController: UIViewController;
begin
  Result := UIViewController(Super);
end;

{ TPlatformGoogleSignIn }

constructor TPlatformGoogleSignIn.Create(const AGoogleSignIn: TGoogleSignIn);
begin
  inherited;
  FSignin := TGIDSignIn.OCClass.sharedInstance;
  FSignInController := TSignInController.Create;
end;

destructor TPlatformGoogleSignIn.Destroy;
begin
  FSignInController.Free;
  inherited;
end;

function TPlatformGoogleSignIn.IsSignedIn: Boolean;
begin
  Result := FIsSignedIn;
end;

procedure TPlatformGoogleSignIn.RestorePreviousSignIn;
begin
  FScopes := [];
  FIsSignedIn := False;
  FSignin.restorePreviousSignInWithCompletion(RestorePreviousSignInCompletion)
end;

function TPlatformGoogleSignIn.GetSignInDetails(const AUser: GIDGoogleUser): TSignInDetails;
begin
  Result.DisplayName := NSStrToStr(AUser.profile.name);
  Result.Email := NSStrToStr(AUser.profile.email);
  Result.Id := NSStrToStr(AUser.userID);
  Result.IdToken := NSStrToStr(AUser.idToken.tokenString);
  Result.PhotoUrl := NSStrToStr(AUser.profile.imageURLWithDimension(96).absoluteString);
end;

procedure TPlatformGoogleSignIn.RestorePreviousSignInCompletion(user: GIDGoogleUser; error: NSError);
var
  LDetails: TSignInDetails;
begin
  if (error = nil) and (user <> nil) then
  begin
    FIsSignedIn := True;
    LDetails := GetSignInDetails(user);
    DoSignInResult(TSignInResult.SignedIn, LDetails);
  end
  else
    HandleSignInError(error);
end;

procedure TPlatformGoogleSignIn.DoHandleSignIn(const ASignInResult: GIDSignInResult);
var
  LDetails: TSignInDetails;
begin
  FIsSignedIn := True;
  LDetails := GetSignInDetails(ASignInResult.user);
  LDetails.AuthCode := NSStrToStr(ASignInResult.serverAuthCode);
  DoSignInResult(TSignInResult.SignedIn, LDetails);
end;

procedure TPlatformGoogleSignIn.HandleSignIn(const ASignInResult: GIDSignInResult);
var
  LScopes: NSMutableArray;
  LScope: string;
begin
  if Length(FScopes) > 0 then
  begin
    LScopes := TNSMutableArray.Create;
    for LScope in FScopes do
      LScopes.addObject(StringToID(LScope));
    FScopes := [];
    FSignIn.currentUser.addScopes(LScopes, TiOSHelper.SharedApplication.keyWindow.rootViewController, AddScopesCompletion);
  end
  else
    DoHandleSignIn(ASignInResult);
end;

procedure TPlatformGoogleSignIn.AddScopesCompletion(signInResult: GIDSignInResult; error: NSError);
begin
  // Handle differently from SignInCompletion? YES
  if error <> nil then
    HandleSignInError(error)
  else
    DoHandleSignIn(signInResult);
end;

procedure TPlatformGoogleSignIn.HandleSignInError(const AError: NSError);
begin
  if AError <> nil then
    DoSignInError(AError.code, NSStrToStr(AError.localizedDescription))
  else
    DoSignInError(-MaxInt, 'Unknown error');
end;

procedure TPlatformGoogleSignIn.RevokeAccess;
begin
  FSignin.disconnectWithCompletion(DisconnectCompletion);
end;

procedure TPlatformGoogleSignIn.DisconnectCompletion(error: NSError);
var
  LDetails: TSignInDetails;
begin
  DoSignInResult(TSignInResult.AccessRevoked, LDetails);
end;

procedure TPlatformGoogleSignIn.SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean);
begin
  TOSLog.d('TPlatformGoogleSignIn.SignIn');
  FScopes := AScopes;
  FSignInController.ViewController.setView(WindowHandleToPlatform(Screen.ActiveForm.Handle).View);
  // FSignin.signInWithPresentingViewController(FSignInController.ViewController.presentingViewController, SignInCompletion);
  FSignin.signInWithPresentingViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController, SignInCompletion);
  TOSLog.d('> Should be showing now?');
end;

procedure TPlatformGoogleSignIn.SignInCompletion(signInResult: GIDSignInResult; error: NSError);
begin
  TOSLog.d('TPlatformGoogleSignIn.SignInCompletion');
  if error <> nil then
    HandleSignInError(error)
  else
    HandleSignIn(signInResult);
end;

procedure TPlatformGoogleSignIn.SignOut;
begin
  FIsSignedIn := False;
  FSignin.signOut;
end;

end.
