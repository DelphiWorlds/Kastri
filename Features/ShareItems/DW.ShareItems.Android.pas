unit DW.ShareItems.Android;

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

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net,
  // FMX
  FMX.Controls, FMX.Graphics,
  // DW
  DW.ShareItems;

type
  TPlatformShareItems = class(TCustomPlatformShareItems)
  private
    function GetShareIntent(const AExcludedActivities: TShareActivities): JIntent;
    procedure MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
    function SaveFile(const AFileName: string): Jnet_Uri;
    function SaveImage(const ABitmap: TBitmap): Jnet_Uri;
    procedure StartActivity(const AIntent: JIntent);
  protected
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); overload; override;
    procedure Share(const AControl: TControl; const ATargetPackage: string); overload; override;
  public
    constructor Create(const AShareItems: TShareItems); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Classes, System.IOUtils, System.Net.Mime, System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.Support;

const
  cMIMETypeAll = '*/*';
  cRequestCodeShare = 9876;

{ TPlatformShareItems }

constructor TPlatformShareItems.Create(const AShareItems: TShareItems);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationHandler);
end;

destructor TPlatformShareItems.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationHandler);
  inherited;
end;

function TPlatformShareItems.SaveFile(const AFileName: string): Jnet_Uri;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(JStringToString(TAndroidHelper.Context.getExternalCacheDir.getAbsolutePath), TPath.GetFileName(AFileName));
  if TFile.Exists(LFileName) then
    TFile.Delete(LFileName);
  TFile.Copy(AFileName, LFileName);
  Result := TAndroidHelper.JFileToJURI(TJFile.JavaClass.init(StringToJString(LFileName)));
  // The following seems to not help at all
  TAndroidHelper.Context.grantUriPermission(TAndroidHelper.Context.getPackageName, Result, TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
end;

function TPlatformShareItems.SaveImage(const ABitmap: TBitmap): Jnet_Uri;
var
  LFile: JFile;
begin
  LFile := TJFile.JavaClass.createTempFile(StringToJString(TPath.GetGUIDFileName), StringToJString('.png'), TAndroidHelper.Context.getExternalCacheDir);
  Result := TAndroidHelper.JFileToJURI(LFile);
  ABitmap.SaveToFile(JStringToString(LFile.getAbsolutePath));
end;

function TPlatformShareItems.GetShareIntent(const AExcludedActivities: TShareActivities): JIntent;
var
  LIntent: JIntent;
  LItem: TSharingItem;
  LMIMETypes: TStrings;
  LURIs, LTexts: JArrayList;
  LFileName, LType, LText: string;
  LKind: TMimeTypes.TKind;
  LClipData: JClipData;
  I: Integer;
begin
  LURIs := TJArrayList.Create;
  LTexts := TJArrayList.Create;
  LIntent := TJIntent.Create;
  LMIMETypes := TStringList.Create(TDuplicates.dupIgnore, False, False);
  try
    LMIMETypes.Delimiter := ';';
    for LItem in Items do
    begin
      if LItem is TSharingItemText then
      begin
        LText := TSharingItemFile(LItem).Text;
        LTexts.add(StringToJString(LText));
        if LText.StartsWith('http://', True) or LText.StartsWith('https://', True) then
          LMIMETypes.Add('text/url')
        else
          LMIMETypes.Add('text/plain');
      end
      else if LItem is TSharingItemFile then
      begin
        LFileName := TSharingItemFile(LItem).Text;
        LURIs.add(SaveFile(LFileName));
        if TMimeTypes.Default.GetFileInfo(LFileName, LType, LKind) then
          LMIMETypes.Add(LType)
        else
          LMIMETypes.Add(cMIMETypeAll);
      end
      else if LItem is TSharingItemImage then
      begin
        LURIs.add(SaveImage(TSharingItemImage(LItem).Image));
        LMIMETypes.Add('image/png');
      end;
    end;
    if LMIMETypes.IndexOf(cMIMETypeAll) > -1 then
      LIntent.setType(StringToJString(cMIMETypeAll))
    else
      LIntent.setType(StringToJString(LMIMETypes.DelimitedText));
  finally
    LMIMETypes.Free;
  end;
  if (LURIs.size > 1) or (LTexts.size > 1) then
  begin
    LIntent.setAction(TJIntent.JavaClass.ACTION_SEND_MULTIPLE);
    LIntent.putParcelableArrayListExtra(TJIntent.JavaClass.EXTRA_TEXT, LTexts);
    LIntent.putParcelableArrayListExtra(TJIntent.JavaClass.EXTRA_STREAM, LURIs);
  end
  else
  begin
    LIntent.setAction(TJIntent.JavaClass.ACTION_SEND);
    if LTexts.size > 0 then
      LIntent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, TJString.Wrap(LTexts.get(0)));
    if LURIs.size > 0 then
      LIntent.putExtra(TJIntent.JavaClass.EXTRA_STREAM, TJParcelable.Wrap(LURIs.get(0)));
  end;
  // From https://stackoverflow.com/a/69393343/3164070
  if LURIs.size > 0 then
  begin
    LClipData := TJClipData.JavaClass.newRawUri(StrToJCharSequence(''), TJnet_Uri.Wrap(LURIs.get(0)));
    for I := 1 to LURIs.size - 1 do
      LClipData.addItem(TJClipData_Item.JavaClass.init(TJnet_Uri.Wrap(LURIs.get(I))));
    LIntent.setClipData(LClipData);
  end;
  LIntent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
  LIntent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  Result := LIntent;
end;

procedure TPlatformShareItems.Share(const AControl: TControl; const ATargetPackage: string);
var
  LIntent: JIntent;
begin
  // For now, assume that the package name is everything before the activity classname
  LIntent := GetShareIntent([]);
  LIntent.setPackage(StringToJString(ATargetPackage));
  StartActivity(LIntent);
end;

procedure TPlatformShareItems.StartActivity(const AIntent: JIntent);
begin
  TAndroidHelper.Activity.startActivityForResult(TJIntent.JavaClass.createChooser(AIntent, StrToJCharSequence('Share using:')), cRequestCodeShare);
end;

procedure TPlatformShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
begin
  StartActivity(GetShareIntent(AExcludedActivities));
end;

procedure TPlatformShareItems.MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
var
  LResult: TMessageResultNotification;
begin
  if M is TMessageResultNotification then
  begin
    LResult := TMessageResultNotification(M);
    if LResult.RequestCode = cRequestCodeShare then
    begin
      // Unfortunately on Android, there is no way to determine whether or not the app did anything with the content
      DoShareCompleted(TShareActivity.Unknown, '');
      Items.Clear;
    end;
  end;
end;

end.
