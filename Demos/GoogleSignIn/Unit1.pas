unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.ImageList,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ImgList, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects, FMX.Layouts,
  DW.GoogleSignIn;

type
  TForm1 = class(TForm)
    ImageList: TImageList;
    SignInButton: TButton;
    Memo: TMemo;
    Layout1: TLayout;
    PhotoImage: TImage;
    RevokeButton: TButton;
    procedure SignInButtonClick(Sender: TObject);
    procedure RevokeButtonClick(Sender: TObject);
  private
    FGoogleSignIn: TGoogleSignIn;
    procedure GoogleSignInResultHandler(Sender: TObject; const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
    procedure GoogleSignInErrorHandler(Sender: TObject; const AStatusCode: Integer; const AStatusMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Net.HttpClient;

const
  {$IF Defined(ANDROID)}
  cClientID = 'YOUR_ANDROID_CLIENT_ID.apps.googleusercontent.com';
  {$ENDIF}
  {$IF Defined(IOS)}
  cClientID = 'YOUR_IOS_CLIENT_ID.apps.googleusercontent.com';
  {$ENDIF}

type
  TURLImageHelper = class helper for TImage
  private
    procedure DoLoadFromURL(const AURL: string);
  public
    procedure Clear;
    procedure LoadFromURL(const AURL: string);
  end;

{ TPhotoImageHelper }

procedure TURLImageHelper.Clear;
begin
  Bitmap.Assign(nil);
end;

procedure TURLImageHelper.DoLoadFromURL(const AURL: string);
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LHTTP := THTTPClient.Create;
  try
    LResponse := LHTTP.Get(AURL);
    if LResponse.StatusCode = 200 then
      TThread.Synchronize(nil, procedure begin Bitmap.LoadFromStream(LResponse.ContentStream) end);
  finally
    LHTTP.Free;
  end;
end;

procedure TURLImageHelper.LoadFromURL(const AURL: string);
begin
  if not AURL.IsEmpty then
    TThread.CreateAnonymousThread(procedure begin DoLoadFromURL(AURL) end).Start;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FGoogleSignIn := TGoogleSignIn.Create;
  FGoogleSignIn.OnSignInResult := GoogleSignInResultHandler;
  FGoogleSignIn.OnSignInError := GoogleSignInErrorHandler;
  // Restores previous sign-in when the app starts.
  // If the user does not have an active signin for this app, the GoogleSignInErrorHandler will be called with a value of -1 for AStatusCode
  FGoogleSignIn.RestorePreviousSignIn;
end;

destructor TForm1.Destroy;
begin
  FGoogleSignIn.Free;
  inherited;
end;

procedure TForm1.GoogleSignInErrorHandler(Sender: TObject; const AStatusCode: Integer; const AStatusMessage: string);
begin
  // A value of -1 for AStatusCode means that RestorePreviousSignIn did not complete because the user does not have an active signin
  if AStatusCode <> -1 then
    Memo.Lines.Add(Format('SignIn Error - %d: %s', [AStatusCode, AStatusMessage]));
end;

procedure TForm1.GoogleSignInResultHandler(Sender: TObject; const ASignInResult: TSignInResult; const ADetails: TSignInDetails);
begin
  case ASignInResult of
    TSignInResult.SignedIn:
      PhotoImage.LoadFromURL(ADetails.PhotoUrl);
    TSignInResult.AccessRevoked:
    begin
      PhotoImage.Clear;
      Memo.Lines.Add('Access revoked');
    end;
  end;
end;

procedure TForm1.RevokeButtonClick(Sender: TObject);
begin
  FGoogleSignIn.RevokeAccess;
end;

procedure TForm1.SignInButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
  // Apparently the email scope is implied??
  // FGoogleSignIn.SignIn(cClientID, ['email']);
  FGoogleSignIn.SignIn(cClientID, []);
end;

end.
