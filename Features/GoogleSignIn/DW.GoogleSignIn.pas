unit DW.GoogleSignIn;

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
  TGoogleSignIn = class;

  TSignInResult = (SignedIn, SignedOut, AccessRevoked);

  TSignInDetails = record
    AuthCode: string;
    DisplayName: string;
    Email: string;
    Id: string;
    IdToken: string;
    PhotoUrl: string;
  end;

  TCustomPlatformGoogleSignIn = class(TObject)
  private
    FGoogleSignIn: TGoogleSignIn;
  protected
    procedure DoSignInError(const AStatusCode: Integer; const AStatusMessage: string);
    procedure DoSignInResult(const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
    function IsSignedIn: Boolean; virtual;
    procedure RestorePreviousSignIn; virtual;
    procedure RevokeAccess; virtual;
    procedure SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean); virtual;
    procedure SignOut; virtual;
    procedure SilentSignIn; virtual;
    property GoogleSignIn: TGoogleSignIn read FGoogleSignIn;
  public
    constructor Create(const AGoogleSignIn: TGoogleSignIn); virtual;
    destructor Destroy; override;
  end;

  TSignInErrorEvent = procedure(Sender: TObject; const StatusCode: Integer; const StatusMessage: string) of object;
  TSignInResultEvent = procedure(Sender: TObject; const SignInResult: TSignInResult; const Credentials: TSignInDetails) of object;

  TGoogleSignIn = class(TObject)
  private
    FPlatformGoogleSignIn: TCustomPlatformGoogleSignIn;
    FOnSignInError: TSignInErrorEvent;
    FOnSignInResult: TSignInResultEvent;
  protected
    procedure DoSignInError(const AStatusCode: Integer; const AStatusMessage: string);
    procedure DoSignInResult(const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RestorePreviousSignIn;
    procedure RevokeAccess;
    procedure SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean = True);
    procedure SignOut;
    procedure SilentSignIn;
    property OnSignInError: TSignInErrorEvent read FOnSignInError write FOnSignInError;
    property OnSignInResult: TSignInResultEvent read FOnSignInResult write FOnSignInResult;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.GoogleSignIn.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.GoogleSignIn.Android;
  {$ENDIF}

{ TCustomPlatformGoogleSignIn }

constructor TCustomPlatformGoogleSignIn.Create(const AGoogleSignIn: TGoogleSignIn);
begin
  inherited Create;
  FGoogleSignIn := AGoogleSignIn;
end;

destructor TCustomPlatformGoogleSignIn.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformGoogleSignIn.DoSignInError(const AStatusCode: Integer; const AStatusMessage: string);
begin
  FGoogleSignIn.DoSignInError(AStatusCode, AStatusMessage);
end;

procedure TCustomPlatformGoogleSignIn.DoSignInResult(const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
begin
  FGoogleSignIn.DoSignInResult(ASignInResult, ADetails);
end;

function TCustomPlatformGoogleSignIn.IsSignedIn: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformGoogleSignIn.RestorePreviousSignIn;
begin
  //
end;

procedure TCustomPlatformGoogleSignIn.RevokeAccess;
begin
  //
end;

procedure TCustomPlatformGoogleSignIn.SignIn;
begin
  //
end;

procedure TCustomPlatformGoogleSignIn.SignOut;
begin
  //
end;

procedure TCustomPlatformGoogleSignIn.SilentSignIn;
begin
  //
end;

{ TGoogleSignIn }

constructor TGoogleSignIn.Create;
begin
  inherited;
  FPlatformGoogleSignIn := TPlatformGoogleSignIn.Create(Self);
end;

destructor TGoogleSignIn.Destroy;
begin
  FPlatformGoogleSignIn.Free;
  inherited;
end;

procedure TGoogleSignIn.DoSignInError(const AStatusCode: Integer; const AStatusMessage: string);
begin
  if Assigned(FOnSignInError) then
    FOnSignInError(Self, AStatusCode, AStatusMessage);
end;

procedure TGoogleSignIn.DoSignInResult(const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
begin
  if Assigned(FOnSignInResult) then
    FOnSignInResult(Self, ASignInResult, ADetails);
end;

procedure TGoogleSignIn.RestorePreviousSignIn;
begin
  FPlatformGoogleSignIn.RestorePreviousSignIn;
end;

procedure TGoogleSignIn.RevokeAccess;
begin
  FPlatformGoogleSignIn.RevokeAccess;
end;

procedure TGoogleSignIn.SignIn(const AClientID: string; const AScopes: TArray<string>; const ANeedsAuthCode: Boolean = True);
begin
  FPlatformGoogleSignIn.SignIn(AClientID, AScopes, ANeedsAuthCode);
end;

procedure TGoogleSignIn.SignOut;
begin
  FPlatformGoogleSignIn.SignOut;
end;

procedure TGoogleSignIn.SilentSignIn;
begin
  FPlatformGoogleSignIn.SilentSignIn;
end;

end.
