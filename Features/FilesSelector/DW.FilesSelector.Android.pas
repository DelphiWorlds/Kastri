unit DW.FilesSelector.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Messaging, System.Classes, System.SysUtils,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net,
  // FMX
  FMX.Graphics,
  // DW
  DW.FilesSelector;

type
  TPlatformFilesSelector = class(TCustomPlatformFilesSelector)
  private
    const cSelectorCode = 1001;
  private
    FIntent: JIntent;
    FMimeTypes: TArray<string>;
    procedure AddFile(const AURI: Jnet_Uri);
    procedure CreateIntent;
    function GetMimeTypes(const AFileKind: TFileKind): string;
    procedure HandleSelectorOK(const AData: JIntent);
    procedure MessageResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    procedure UpdateIntentMimeTypes;
    procedure UpdateIntentActivities;
  protected
    procedure DoSelect(const AMode: TSelectionMode); override;
    procedure FileKindsChanged; override;
    procedure FileTypesChanged; override;
  public
    constructor Create(const ASelector: TFilesSelector); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.IOUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.JNI.Provider,
  // FMX
  FMX.Platform.Android,
  // DW
  DW.Androidapi.JNI.Content;

{ TPlatformFilesSelector }

constructor TPlatformFilesSelector.Create(const ASelector: TFilesSelector);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationMessageHandler);
  CreateIntent;
end;

destructor TPlatformFilesSelector.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationMessageHandler);
  inherited;
end;

function TPlatformFilesSelector.GetMimeTypes(const AFileKind: TFileKind): string;
begin
  case AFileKind of
    TFileKind.Image:
      Result := 'image/*';
    TFileKind.Audio:
      Result := 'audio/*';
    TFileKind.Movie:
      Result := 'video/*';
    TFileKind.Text, TFileKind.Content, TFileKind.SourceCode:
      Result := 'text/*';
    TFileKind.PDF:
      Result := 'application/pdf';
    TFileKind.X509Certificate:
      begin
        // From: https://android.googlesource.com/platform/packages/apps/CertInstaller/+/88ded90/src/com/android/certinstaller/CertInstallerMain.java
        Result := 'application/x-pkcs12|application/x-x509-ca-cert|application/x-x509-user-cert|application/x-x509-server-cert|' +
          'application/x-pem-file|application/pkix-cert';
      end;
    TFileKind.Key:
      Result := 'application/pgp-keys';
  else
    Result := '';
  end;
end;

procedure TPlatformFilesSelector.CreateIntent;
begin
  FIntent := TJIntent.JavaClass.init;
  FIntent.putExtra(TJIntent.JavaClass.EXTRA_ALLOW_MULTIPLE, True);
  FIntent.setType(StringToJString('*/*'));
  UpdateIntentActivities;
end;

procedure TPlatformFilesSelector.AddFile(const AURI: Jnet_Uri);
var
  LSelectedFile: TSelectedFile;
  LProjection: TJavaObjectArray<JString>;
  LCursor: JCursor;
begin
  if AURI <> nil then
  begin
    LSelectedFile.RawPath := JStringToString(AURI.toString);
    LSelectedFile.DecodedPath := JStringToString(TJnet_Uri.JavaClass.decode(AURI.toString));
    LProjection := TJavaObjectArray<JString>.Create(1);
    try
      LProjection[0] := TJMediaStore_MediaColumns.JavaClass.DISPLAY_NAME;
      LCursor := TAndroidHelper.Context.getContentResolver.query(AURI, LProjection, nil, nil, nil);
    finally
      LProjection.Free;
    end;
    if LCursor <> nil then
    try
      if LCursor.moveToFirst then
        LSelectedFile.DisplayName := JStringToString(LCursor.getString(0));
    finally
      LCursor.close;
    end;
    AddSelectedFile(LSelectedFile);
  end;
end;

procedure TPlatformFilesSelector.HandleSelectorOK(const AData: JIntent);
var
  I: Integer;
begin
  if AData.getClipData <> nil then
  begin
    for I := 0 to AData.getClipData.getItemCount - 1 do
      AddFile(AData.getClipData.getItemAt(I).getUri);
  end
  else
    AddFile(AData.getData);
  DoComplete(True);
end;

procedure TPlatformFilesSelector.MessageResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
var
  LMessage: TMessageResultNotification;
begin
  LMessage := TMessageResultNotification(M);
  if LMessage.RequestCode = cSelectorCode then
  begin
    if LMessage.ResultCode = TJActivity.JavaClass.RESULT_OK then
      HandleSelectorOK(LMessage.Value)
    else
      DoComplete(False);
  end;
end;

procedure TPlatformFilesSelector.DoSelect(const AMode: TSelectionMode);
begin
  case AMode of
    TSelectionMode.Documents:
      FIntent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
    TSelectionMode.Content:
      FIntent.setAction(TJIntent.JavaClass.ACTION_GET_CONTENT);
  end;
  TAndroidHelper.Activity.startActivityForResult(FIntent, cSelectorCode);
end;

procedure TPlatformFilesSelector.FileKindsChanged;
var
  LFileKind: TFileKind;
  LMimeType: string;
begin
  FMimeTypes := [];
  for LFileKind := Low(TFileKind) to High(TFileKind) do
  begin
    if (LFileKind in FileKinds) or (FileKinds = []) then
    begin
      for LMimeType in GetMimeTypes(LFileKind).Split(['|']) do
        FMimeTypes := FMimeTypes + [LMimeType];
    end;
  end;
  UpdateIntentMimeTypes;
end;

procedure TPlatformFilesSelector.FileTypesChanged;
begin
  FMimeTypes := FFileTypes.ToStringArray;
  UpdateIntentMimeTypes;
end;

procedure TPlatformFilesSelector.UpdateIntentMimeTypes;
var
  LMimeTypes: TJavaObjectArray<JString>;
  I: Integer;
begin
  if Length(FMimeTypes) > 0 then
  begin
    LMimeTypes := TJavaObjectArray<JString>.Create(Length(FMimeTypes));
    try
      for I := 0 to Length(FMimeTypes) - 1 do
      begin
        LMimeTypes.Items[I] := StringToJString(FMimeTypes[I]);
      end;
      FIntent.putExtra(TJIntent.JavaClass.EXTRA_MIME_TYPES, LMimeTypes);
    finally
      LMimeTypes.Free;
    end;
  end
  else
  begin
    LMimeTypes := nil;
    FIntent.putExtra(TJIntent.JavaClass.EXTRA_MIME_TYPES, LMimeTypes);
  end;
  UpdateIntentActivities;
end;

procedure TPlatformFilesSelector.UpdateIntentActivities;
var
  LList: JList;
  LResolveInfo: JResolveInfo;
  LActivityInfo: JActivityInfo;
  LApplicationInfo: JApplicationInfo;
  I: Integer;
  LActivityClassName: string;
  LFlags: JPackageManager_ResolveInfoFlags;
begin
  Activities.Clear;
  if TOSVersion.Check(13) then
  begin
    LFlags := TJPackageManager_ResolveInfoFlags.JavaClass.&of(0);
    LList := TJPackageManagerEx.Wrap(TAndroidHelper.Context.getPackageManager).queryIntentActivities(FIntent, LFlags)
  end
  else
    LList := TAndroidHelper.Context.getPackageManager.queryIntentActivities(FIntent, 0);
  for I := 0 to LList.size - 1 do
  begin
    LResolveInfo := TJResolveInfo.Wrap(LList.get(i));
    LActivityInfo := TJActivityInfo.Wrap(LResolveInfo.activityInfo);
    LApplicationInfo := TJApplicationInfo.Wrap(LActivityInfo.applicationInfo);
    LActivityClassName := JStringToString(LApplicationInfo.className);
    if not LActivityClassName.IsEmpty and (Activities.IndexOf(LActivityClassName) = -1) then
      Activities.Add(LActivityClassName);
  end;
end;

end.
