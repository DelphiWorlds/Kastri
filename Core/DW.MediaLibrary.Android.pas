unit DW.MediaLibrary.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  // DW
  DW.MediaLibrary;

type
  TPlatformMediaLibrary = class(TCustomPlatformMediaLibrary)
  private
    FImageFile: JFile;
    FIsImageReceived: Boolean;
    function GetPhotosPath: JFile;
    procedure HandleReceivedImage;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure TakePhoto; override;
  public
    constructor Create(const AMediaLibrary: TMediaLibrary); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.Provider, Androidapi.JNI.App, Androidapi.JNI.Media, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNIBridge,
  // FMX
  FMX.Graphics, FMX.Platform,
  // DW
  DW.Androidapi.JNI.Os, DW.Androidapi.JNI.SupportV4, DW.Android.Helpers;

const
  cImageFileNameTemplate = 'IMG_%s.jpg';
  ACTION_TAKE_IMAGE_FROM_CAMERA = 991; // Avoiding a clash with existing FMX code
  ACTION_TAKE_IMAGE_FROM_LIBRARY = 992; // Avoiding a clash with existing FMX code

{ TPlatformMediaLibrary }

constructor TPlatformMediaLibrary.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationHandler);
end;

destructor TPlatformMediaLibrary.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationHandler);
  inherited;
end;

procedure TPlatformMediaLibrary.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  if FIsImageReceived and (M is TApplicationEventMessage) then
  begin
    case TApplicationEventMessage(M).Value.Event of
      TApplicationEvent.BecameActive:
      begin
        FIsImageReceived := False;
        TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
        HandleReceivedImage;
      end;
    end;
  end;
end;

function TPlatformMediaLibrary.GetPhotosPath: JFile;
var
  LDCIMPath, LStoragePath: JFile;
begin
	Result := nil;
  if TJEnvironment.JavaClass.getExternalStorageState.equals(TJEnvironment.JavaClass.MEDIA_MOUNTED) then
  begin
    LDCIMPath := TJEnvironment.JavaClass.getExternalStoragePublicDirectory(TJEnvironment.JavaClass.DIRECTORY_DCIM);
    LStoragePath := TJFile.JavaClass.init(LDCIMPath, StringToJString('Camera'));
    if (LStoragePath <> nil) and not LStoragePath.mkdirs and not LStoragePath.exists then
      Result := nil
    else
      Result := LStoragePath;
  end;
end;

procedure TPlatformMediaLibrary.HandleReceivedImage;
var
  LBitmap: TBitmap;
  LImagePath: string;
  LEXIF: JExifInterface;
begin
  LImagePath := JStringToString(FImageFile.getAbsolutePath);
  LBitmap := TBitmap.CreateFromFile(LImagePath);
  try
    // Check EXIF data for orientation and rotate bitmap if necessary
    LEXIF := TJExifInterface.JavaClass.init(StringToJString(LImagePath));
    if LEXIF.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, -1) = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then
      LBitmap.Rotate(90)
    else if LEXIF.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, -1) = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then
      LBitmap.Rotate(180)
    else if LEXIF.getAttributeInt(TJExifInterface.JavaClass.TAG_ORIENTATION, -1) = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then
      LBitmap.Rotate(270);
    DoReceivedImage(LImagePath, LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TPlatformMediaLibrary.MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
var
  LResult: TMessageResultNotification;
begin
  if M is TMessageResultNotification then
  begin
    LResult := TMessageResultNotification(M);
    if (LResult.ResultCode = TJActivity.JavaClass.RESULT_OK) and (LResult.RequestCode = ACTION_TAKE_IMAGE_FROM_CAMERA) then
    begin
      {$IF CompilerVersion > 31}
      HandleReceivedImage;
      {$ELSE} // Workaround for: https://quality.embarcadero.com/browse/RSP-14217
      FIsImageReceived := True;
      TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
      {$ENDIF}
    end
    else
      DoCanceled;
  end;
end;

procedure TPlatformMediaLibrary.TakePhoto;
var
  LIntent: JIntent;
  LUri: Jnet_Uri;
  LFileName: string;
  LPhotosPath: JFile;
begin
  FImageFile := nil;
  LPhotosPath := GetPhotosPath;
  if LPhotosPath <> nil then
  begin
    LFileName := Format(cImageFileNameTemplate, [FormatDateTime('yyyymmdd_hhnnss', Now)]);
    FImageFile := TJFile.JavaClass.init(LPhotosPath, StringToJString(LFileName));
    LUri := TAndroidHelperEx.UriFromFile(FImageFile);
    LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_IMAGE_CAPTURE);
    LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap((LUri as ILocalObject).GetObjectID));
    TAndroidHelper.Activity.startActivityForResult(LIntent, ACTION_TAKE_IMAGE_FROM_CAMERA);
  end;
  // TODO: Handle case where LPhotosPath is nil
end;

end.
