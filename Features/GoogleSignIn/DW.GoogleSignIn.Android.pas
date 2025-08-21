unit DW.GoogleSignIn.Android;

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
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.GoogleSignIn, DW.Androidapi.JNI.DWGoogleSignIn, DW.GoogleSignIn;

type
  TPlatformGoogleSignIn = class;

  TDWGoogleSignInDelegate = class(TJavaLocal, JDWGoogleSignInDelegate)
  private
    FPlatformGoogleSignin: TPlatformGoogleSignIn;
  public
    { JDWGoogleSignInDelegate }
    procedure revokeAccessComplete; cdecl;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl;
    procedure signInComplete(account: JGoogleSignInAccount; statusCode: Integer; statusMessage: JString); cdecl;
    procedure signOutComplete; cdecl;
  public
    constructor Create(const AGoogleSignin: TPlatformGoogleSignIn);
  end;

  TPlatformGoogleSignIn = class(TCustomPlatformGoogleSignIn)
  private
    FDelegate: JDWGoogleSignInDelegate;
    FSignIn: JDWGoogleSignIn;
    procedure MessageResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure HandleSignInResult(const ASignInResult: TSignInResult); overload;
    procedure HandleSignInResult(const AAccount: JGoogleSignInAccount; const AStatusCode: Integer; const AStatusMessage: string); overload;
    procedure RestorePreviousSignIn; override;
    procedure RevokeAccess; override;
    procedure SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean); override;
    procedure SignOut; override;
    procedure SilentSignIn; override;
  public
    constructor Create(const AGoogleSignIn: TGoogleSignIn); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  //Android
  Androidapi.Helpers, Androidapi.JNI.App;

{ TDWGoogleSignInDelegate }

constructor TDWGoogleSignInDelegate.Create(const AGoogleSignin: TPlatformGoogleSignIn);
begin
  inherited Create;
  FPlatformGoogleSignin := AGoogleSignin;
end;

procedure TDWGoogleSignInDelegate.revokeAccessComplete;
begin
  FPlatformGoogleSignin.HandleSignInResult(TSignInResult.AccessRevoked);
end;

procedure TDWGoogleSignInDelegate.signInComplete(account: JGoogleSignInAccount; statusCode: Integer; statusMessage: JString);
begin
  FPlatformGoogleSignin.HandleSignInResult(account, statusCode, JStringToString(statusMessage));
end;

procedure TDWGoogleSignInDelegate.signOutComplete;
begin
  FPlatformGoogleSignin.HandleSignInResult(TSignInResult.SignedOut);
end;

procedure TDWGoogleSignInDelegate.startActivityForResult(intent: JIntent; requestCode: Integer);
begin
  TAndroidHelper.Activity.startActivityForResult(intent, requestCode);
end;

{ TPlatformGoogleSignIn }

constructor TPlatformGoogleSignIn.Create(const AGoogleSignIn: TGoogleSignIn);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationMessageHandler);
  FDelegate := TDWGoogleSignInDelegate.Create(Self);
  FSignIn := TJDWGoogleSignIn.JavaClass.init(TAndroidHelper.Context, FDelegate);
end;

destructor TPlatformGoogleSignIn.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationMessageHandler);
  inherited;
end;

procedure TPlatformGoogleSignIn.HandleSignInResult(const ASignInResult: TSignInResult);
var
  LDetails: TSignInDetails;
begin
  DoSignInResult(ASignInResult, LDetails);
end;

procedure TPlatformGoogleSignIn.HandleSignInResult(const AAccount: JGoogleSignInAccount; const AStatusCode: Integer; const AStatusMessage: string);
var
  LDetails: TSignInDetails;
begin
  if AStatusCode = 0 then
  begin
    LDetails.AuthCode := JStringToString(AAccount.getServerAuthCode);
    LDetails.Email := JStringToString(AAccount.getEmail);
    LDetails.Id := JStringToString(AAccount.getId);
    LDetails.IdToken := JStringToString(AAccount.getIdToken);
    LDetails.DisplayName := JStringToString(AAccount.getDisplayName);
    LDetails.PhotoUrl := JStringToString(AAccount.getPhotoUrl.toString);
    DoSignInResult(TSignInResult.SignedIn, LDetails);
  end
  else
    DoSignInError(AStatusCode, AStatusMessage);
end;

procedure TPlatformGoogleSignIn.MessageResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
var
  LMessage: TMessageResultNotification;
begin
  LMessage := TMessageResultNotification(M);
  if LMessage.RequestCode = TJDWGoogleSignIn.JavaClass.RC_SIGN_IN then
    FSignIn.handleSignInResult(LMessage.Value);
end;

procedure TPlatformGoogleSignIn.RestorePreviousSignIn;
var
  LAccount: JGoogleSignInAccount;
begin
  LAccount := TJGoogleSignIn.JavaClass.getLastSignedInAccount(TAndroidHelper.Context);
  if LAccount <> nil then
    HandleSignInResult(LAccount, 0, '')
  else
    HandleSignInResult(LAccount, -1, ''); // Indicates there is no previous sign-in
end;

procedure TPlatformGoogleSignIn.RevokeAccess;
begin
  FSignIn.revokeAccess;
end;

procedure TPlatformGoogleSignIn.SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean);
var
  LScopes: TJavaObjectArray<JString>;
  I: Integer;
begin
  LScopes := TJavaObjectArray<JString>.Create(Length(AScopes));
  try
    for I := 0 to Length(AScopes) - 1 do
      LScopes[I] := StringToJString(AScopes[I]);
    FSignIn.setupClient(StringToJString(AClientID), LScopes, ANeedsAuthCode);
  finally
    LScopes.Free;
  end;
  FSignIn.signIn;
end;

procedure TPlatformGoogleSignIn.SignOut;
begin
  FSignIn.signOut;
end;

procedure TPlatformGoogleSignIn.SilentSignIn;
begin
  FSignIn.silentSignIn;
end;

end.
