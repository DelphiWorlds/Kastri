unit DW.ShareItems.Android;

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
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net,
  // FMX
  FMX.Controls, FMX.Graphics,
  // DW
  DW.ShareItems;

type
  TPlatformShareItems = class(TCustomPlatformShareItems)
  private
    procedure MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
    function SaveFile(const AFileName: string): Jnet_Uri;
    function SaveImage(const ABitmap: TBitmap): Jnet_Uri;
  protected
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); override;
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

procedure TPlatformShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
var
  LIntent: JIntent;
  LItem: TSharingItem;
  LMIMETypes: TStrings;
  LURIs, LTexts: JArrayList;
  LFileName, LType: string;
  LKind: TMimeTypes.TKind;
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
        LTexts.add(StringToJString(TSharingItemFile(LItem).Text));
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
  if (LURIs.size + LTexts.size) > 1 then
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
  LIntent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
  LIntent.addFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivityForResult(TJIntent.JavaClass.createChooser(LIntent, StrToJCharSequence('Share using:')), cRequestCodeShare);
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
