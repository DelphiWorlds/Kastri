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
    FIntentActivities: TStrings;
    procedure AddFile(const AURI: Jnet_Uri);
    procedure CreateIntent;
    procedure HandleSelectorOK(const AData: JIntent);
    procedure MessageResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    procedure UpdateIntentActivities;
  protected
    procedure DoSelect; override;
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
  FMX.Platform.Android;

{ TPlatformFilesSelector }

constructor TPlatformFilesSelector.Create(const ASelector: TFilesSelector);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationMessageHandler);
  FIntentActivities := TStringList.Create;
  CreateIntent;
end;

destructor TPlatformFilesSelector.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationMessageHandler);
  FIntentActivities.Free;
  inherited;
end;

procedure TPlatformFilesSelector.CreateIntent;
begin
  FIntent := TJIntent.JavaClass.init;
  FIntent.setType(StringToJString('*/*')); // EXTRA_MIME_TYPES  = String[] eg image/*
  FIntent.setAction(TJIntent.JavaClass.ACTION_OPEN_DOCUMENT);
  FIntent.putExtra(TJIntent.JavaClass.EXTRA_ALLOW_MULTIPLE, True);
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
    if (LCursor <> nil) and LCursor.moveToFirst then
      LSelectedFile.DisplayName := JStringToString(LCursor.getString(0));
    AddSelectedFile(LSelectedFile);
    Files.Add(LSelectedFile.RawPath);
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
  if M is TMessageResultNotification then
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
end;

procedure TPlatformFilesSelector.DoSelect;
begin
  TAndroidHelper.Activity.startActivityForResult(FIntent, cSelectorCode);
end;

procedure TPlatformFilesSelector.UpdateIntentActivities;
var
  LList: JList;
  LResolveInfo: JResolveInfo;
  LActivityInfo: JActivityInfo;
  LApplicationInfo: JApplicationInfo;
  I: Integer;
begin
  FIntentActivities.Clear;
  LList := TAndroidHelper.Context.getPackageManager.queryIntentActivities(FIntent, 0);
  for I := 0 to LList.size - 1 do
  begin
    LResolveInfo := TJResolveInfo.Wrap((LList.get(i) as ILocalObject).GetObjectID);
    LActivityInfo := TJActivityInfo.Wrap((LResolveInfo.activityInfo as ILocalObject).GetObjectID);
    LApplicationInfo := TJApplicationInfo.Wrap((LActivityInfo.applicationInfo as ILocalObject).GetObjectID);
    FIntentActivities.Add(JStringToString(LApplicationInfo.className));
  end;
end;

end.
