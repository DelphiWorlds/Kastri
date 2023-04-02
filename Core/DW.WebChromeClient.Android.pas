unit DW.WebChromeClient.Android;

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
  // RTL
  System.Classes, System.Messaging,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.WebKit, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net,
  // FMX
  FMX.WebBrowser,
  // DW
  DW.Androidapi.JNI.DWWebChromeClient, DW.Androidapi.JNI.Os, DW.WebChromeClient;

type
  TWebChromeClientManager = class;

  TWebChromeClientDelegate = class(TJavaLocal, JDWWebChromeClientDelegate)
  private
    FManager: TWebChromeClientManager;
  public
    { JDWWebChromeClientDelegate }
    function onFileChooserIntent(intent: JIntent): Boolean; cdecl;
    function onShouldOverrideUrlLoading(url: JString): Boolean; cdecl;
  public
    constructor Create(const AManager: TWebChromeClientManager);
  end;

  TWebChromeClientManager = class(TCustomPlatformWebChromeClientManager)
  private
    FDelegate: JDWWebChromeClientDelegate;
    FImageUri: Jnet_Uri;
    FWebChromeClient: JDWWebChromeClient;
    function GetPhotosPath: JFile;
    procedure MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure FlushCookies; override;
    function HandleFileChooserIntent(const AIntent: JIntent): Boolean;
    function ShouldOverrideUrlLoading(const AUrl: JString): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPlatformWebChromeClientManager = class(TWebChromeClientManager);

implementation

uses
  DW.OSLog,
  // RTL
  System.SysUtils, System.Permissions,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.Provider, Androidapi.JNI.Os,
  // DW
  DW.Android.Helpers, DW.Consts.Android, DW.Permissions.Helpers;

const
  cFileChooserRequestCodeDefault = 9000;
  cFileChooserRequestCodeCamera = 9001;

{ TWebChromeClientDelegate }

constructor TWebChromeClientDelegate.Create(const AManager: TWebChromeClientManager);
begin
  inherited Create;
  FManager := AManager;
end;

function TWebChromeClientDelegate.onFileChooserIntent(intent: JIntent): Boolean;
begin
  Result := FManager.HandleFileChooserIntent(intent);
end;

function TWebChromeClientDelegate.onShouldOverrideUrlLoading(url: JString): Boolean;
begin
  Result := FManager.ShouldOverrideUrlLoading(url);
end;

{ TWebChromeClientManager }

constructor TWebChromeClientManager.Create(AOwner: TComponent);
var
  LWebView: JWebView;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationHandler);
  if Supports(AOwner, JWebView, LWebView) then
  begin
    FDelegate := TWebChromeClientDelegate.Create(Self);
    FWebChromeClient := TJDWWebChromeClient.JavaClass.init(FDelegate, TAndroidHelper.Activity);
    LWebView.setWebChromeClient(FWebChromeClient);
  end;
end;

destructor TWebChromeClientManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationHandler);
  inherited;
end;

function TWebChromeClientManager.GetPhotosPath: JFile;
var
  LDCIMPath, LStoragePath: JFile;
begin
	Result := nil;
  if FileCachePath.IsEmpty then
  begin
    if TJEnvironment.JavaClass.getExternalStorageState.equals(TJEnvironment.JavaClass.MEDIA_MOUNTED) then
    begin
      LDCIMPath := TJEnvironment.JavaClass.getExternalStoragePublicDirectory(TJEnvironment.JavaClass.DIRECTORY_DCIM);
      LStoragePath := TJFile.JavaClass.init(LDCIMPath, StringToJString('Camera'));
      if (LStoragePath <> nil) and not LStoragePath.mkdirs and not LStoragePath.exists then
        Result := nil
      else
        Result := LStoragePath;
    end;
  end
  else
  begin
    ForceDirectories(FileCachePath);
    Result := TJFile.JavaClass.init(StringToJString(FileCachePath));
  end;
end;

function TWebChromeClientManager.HandleFileChooserIntent(const AIntent: JIntent): Boolean;
const
  cImageFileNameTemplate = 'IMG_%s.jpg';
var
  LType, LFileName: string;
  LMimeTypes: TArray<string>;
  LFileChooser: TFileChooserKind;
  LIntent: JIntent;
  LPhotosPath: JFile;
begin
  LType := '';
  if AIntent.getType <> nil then
    LType := JStringToString(AIntent.getType);
  LMimeTypes := LType.Split([' ']);
  LFileChooser := TFileChooserKind.Default;
  DoFileChooser(LMimeTypes, LFileChooser);
  case LFileChooser of
    TFileChooserKind.Default:
      TAndroidHelper.Activity.startActivityForResult(AIntent, cFileChooserRequestCodeDefault);
    TFileChooserKind.VisualMedia:
    begin
      LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_PICK);
      LIntent.setData(TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI);
      TAndroidHelper.Activity.startActivityForResult(LIntent, cFileChooserRequestCodeDefault);
    end;
    TFileChooserKind.Camera:
    begin
      PermissionsService.RequestPermissions([cPermissionCamera],
        procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
        begin
          if AGrantResults.AreAllGranted then
          begin
            LPhotosPath := GetPhotosPath;
            if LPhotosPath <> nil then
            begin
              LFileName := Format(cImageFileNameTemplate, [FormatDateTime('yyyymmdd_hhnnss', Now)]);
              FImageUri := TAndroidHelperEx.UriFromFile(TJFile.JavaClass.init(LPhotosPath, StringToJString(LFileName)));
              LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_IMAGE_CAPTURE);
              LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap(FImageUri));
              LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_WRITE_URI_PERMISSION);
              TAndroidHelper.Activity.startActivityForResult(LIntent, cFileChooserRequestCodeCamera);
            end;
          end;
        end
      );
    end;
  end;
  Result := True;
end;

procedure TWebChromeClientManager.MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
var
  LResult: TMessageResultNotification;
begin
  if M is TMessageResultNotification then
  begin
    LResult := TMessageResultNotification(M);
    if (LResult.RequestCode = cFileChooserRequestCodeDefault) or (LResult.RequestCode = cFileChooserRequestCodeCamera) then
    begin
      if LResult.RequestCode = cFileChooserRequestCodeCamera then
      begin
        LResult.Value.setData(FImageUri);
        LResult.Value.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
      end;
      FWebChromeClient.handleFileChooserResult(LResult.Value, LResult.ResultCode);
    end;
  end;
end;

function TWebChromeClientManager.ShouldOverrideUrlLoading(const AUrl: JString): Boolean;
begin
  Result := False;
  DoShouldOverrideUrl(JStringToString(AUrl), Result);
end;

procedure TWebChromeClientManager.FlushCookies;
begin
  TJCookieManager.JavaClass.getInstance.Flush;
end;

end.
