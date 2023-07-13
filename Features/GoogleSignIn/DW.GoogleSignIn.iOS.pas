unit DW.GoogleSignIn.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.GoogleSignIn, DW.iOSapi.GoogleSignIn;

type
  TPlatformGoogleSignIn = class(TCustomPlatformGoogleSignIn)
  private
    FScopes: TArray<string>;
    FSignin: GIDSignIn;
    procedure DisconnectCallback(error: NSError);
    procedure DoHandleSignIn(const AUser: GIDGoogleUser);
    procedure HandleSignIn(const AUser: GIDGoogleUser);
    procedure HandleSignInError(const AError: NSError);
    procedure SignInCallback(user: GIDGoogleUser; error: NSError);
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
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers;

{ TPlatformGoogleSignIn }

constructor TPlatformGoogleSignIn.Create(const AGoogleSignIn: TGoogleSignIn);
begin
  inherited;
  FSignin := TGIDSignIn.OCClass.sharedInstance;
end;

destructor TPlatformGoogleSignIn.Destroy;
begin
  //
  inherited;
end;

function TPlatformGoogleSignIn.IsSignedIn: Boolean;
begin
  Result := FSignin.currentUser.authentication <> nil;
end;

procedure TPlatformGoogleSignIn.RestorePreviousSignIn;
begin
  FScopes := [];
  FSignin.restorePreviousSignInWithCallback(SignInCallback);
end;

procedure TPlatformGoogleSignIn.SignInCallback(user: GIDGoogleUser; error: NSError);
begin
  if (error <> nil) or (user = nil) then
    HandleSignInError(error)
  else
    HandleSignIn(user);
end;

procedure TPlatformGoogleSignIn.DoHandleSignIn(const AUser: GIDGoogleUser);
var
  LDetails: TSignInDetails;
begin
  LDetails.AuthCode := NSStrToStr(AUser.serverAuthCode);
  LDetails.DisplayName := NSStrToStr(AUser.profile.name);
  LDetails.Email := NSStrToStr(AUser.profile.email);
  LDetails.Id := NSStrToStr(AUser.userID);
  LDetails.IdToken := NSStrToStr(AUser.authentication.idToken);
  LDetails.PhotoUrl := NSStrToStr(AUser.profile.imageURLWithDimension(96).absoluteString);
  DoSignInResult(TSignInResult.SignedIn, LDetails);
end;

procedure TPlatformGoogleSignIn.HandleSignIn(const AUser: GIDGoogleUser);
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
    FSignin.addScopes(LScopes, TiOSHelper.SharedApplication.keyWindow.rootViewController, SignInCallback);
  end
  else
    DoHandleSignIn(AUser);
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
  FSignin.disconnectWithCallback(DisconnectCallback);
end;

procedure TPlatformGoogleSignIn.DisconnectCallback(error: NSError);
var
  LDetails: TSignInDetails;
begin
  DoSignInResult(TSignInResult.AccessRevoked, LDetails);
end;

procedure TPlatformGoogleSignIn.SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean);
var
  LConfiguration: GIDConfiguration;
begin
  FScopes := AScopes;
  LConfiguration := TGIDConfiguration.Wrap(TGIDConfiguration.Alloc.initWithClientID(StrToNSStr(AClientID)));
  FSignin.signInWithConfiguration(LConfiguration, TiOSHelper.SharedApplication.keyWindow.rootViewController, SignInCallback);
end;

procedure TPlatformGoogleSignIn.SignOut;
begin
  FSignin.signOut;
end;

end.
