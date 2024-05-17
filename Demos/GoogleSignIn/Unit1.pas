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
  {$IF Defined(ANDROID)}
  System.Hash, System.NetEncoding,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI, Androidapi.JNI.JavaTypes,
  {$ENDIF}
  DW.OSLog,
  System.Net.HttpClient;

const
  {$IF Defined(ANDROID)}
  cClientID = 'YOUR_ANDROID_CLIENT_ID.apps.googleusercontent.com';
  {$ENDIF}
  {$IF Defined(IOS)}
  cClientID = 'YOUR_IOS_CLIENT_ID.apps.googleusercontent.com';
  {$ENDIF}

type
  {$IF Defined(ANDROID)}
  TAppSignatureHelper = record
  private
    class function GetSignatureHash(const APackageName: string; ASignature: string): string; static;
  public
    class function GetAppSignatures: TArray<TBytes>; static;
    class function GetAppSignatureHashes: TArray<string>; static;
    class function GetSHA1Signature: string; static;
  end;
  {$ENDIF}

  TURLImageHelper = class helper for TImage
  private
    procedure DoLoadFromURL(const AURL: string);
  public
    procedure Clear;
    procedure LoadFromURL(const AURL: string);
  end;


function BytesToHex(const ABytes: TBytes; const ASeparator: string = ''): string;
var
  LByte: Byte;
begin
  Result := '';
  for LByte in ABytes do
  begin
    if not ASeparator.IsEmpty and not Result.IsEmpty then
      Result := Result + ASeparator;
    Result := Result + IntToHex(LByte, 2);
  end;
end;

{$IF Defined(ANDROID)}
class function TAppSignatureHelper.GetAppSignatures: TArray<TBytes>;
var
  LPackageManager: JPackageManager;
  LSignatures: TJavaObjectArray<JSignature>;
  LBytes: TJavaArray<Byte>;
  I: Integer;
begin
  LPackageManager := TAndroidHelper.Context.getPackageManager;
  LSignatures := LPackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName, TJPackageManager.JavaClass.GET_SIGNATURES).signatures;
  if LSignatures <> nil then
  try
    for I := 0 to LSignatures.Length - 1 do
    begin
      LBytes := LSignatures[I].toByteArray;
      try
        Result := Result + [TJavaArrayToTBytes(LBytes)];
      finally
        LBytes.Free;
      end;
    end;
  finally
    LSignatures.Free;
  end;
end;

class function TAppSignatureHelper.GetAppSignatureHashes: TArray<string>;
var
  LPackageManager: JPackageManager;
  LSignatures: TJavaObjectArray<JSignature>;
  I: Integer;
  LHash: string;
begin
  LPackageManager := TAndroidHelper.Context.getPackageManager;
  LSignatures := LPackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName, TJPackageManager.JavaClass.GET_SIGNATURES).signatures;
  try
    for I := 0 to LSignatures.Length - 1 do
    begin
      LHash := GetSignatureHash(JStringToString(TAndroidHelper.Context.getPackageName), JStringToString(LSignatures[I].toCharsString));
      if not LHash.IsEmpty then
        Result := Result + [LHash];
    end;
  finally
    LSignatures.Free;
  end;
end;

class function TAppSignatureHelper.GetSignatureHash(const APackageName: string; ASignature: string): string;
const
  cNumHashedBytes = 10;
  cNumBase64Char = 11;
var
  LBytes: TBytes;
begin
  LBytes := THashSHA2.GetHashBytes(APackageName + ' ' + ASignature);
  Result := TNetEncoding.Base64.EncodeBytesToString(Copy(LBytes, 0, cNumHashedBytes)).Substring(0, cNumBase64Char);
end;

class function TAppSignatureHelper.GetSHA1Signature: string;
var
  LSignatures: TArray<TBytes>;
  LSHA1: THashSHA1;
begin
  Result := '';
  LSignatures := GetAppSignatures;
  if Length(LSignatures) > 0 then
  begin
    LSHA1 := THashSHA1.Create;
    LSHA1.Update(LSignatures[0]);
    Result := BytesToHex(LSHA1.HashAsBytes, ':').ToLower;
  end;
end;

{$ENDIF}

{ TURLImageHelper }

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
  TOSLog.d('SHA1: %s', [TAppSignatureHelper.GetSHA1Signature]);
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
