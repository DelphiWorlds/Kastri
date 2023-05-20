unit DW.Toast.Android;

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
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  TToast = class(TJavaLocal, JRunnable)
  private
    class var FToast: TToast;
    class destructor DestroyClass;
  private
    FHandler: JHandler;
    FIsCustom: Boolean;
    FIsShort: Boolean;
    FMsg: string;
    procedure DoRun(const AToastLength: Integer);
    procedure DoRunEx(const AToastLength: Integer);
  public
    { JRunnable }
    procedure run; cdecl;
  public
    /// <summary>
    ///   Convenience equivalent of MakeToast
    /// </summary>
    class procedure Make(const AMsg: string; const AIsShort: Boolean = True);
    /// <summary>
    ///   Convenience equivalent of MakeToastEx
    /// </summary>
    class procedure MakeEx(const AMsg: string; const AIsShort: Boolean = True);
  public
    constructor Create;
    /// <summary>
    ///   Shows a toast with the message provided, for the length specified
    /// </summary>
    procedure MakeToast(const AMsg: string; const AIsShort: Boolean = True);
    /// <summary>
    ///   Shows a custom toast with the message provided, for the length specified
    /// </summary>
    /// <remarks>
    ///   This method creates its own text view, so that more lines of text can be shown
    ///   Note: Presently, the application icon is not shown in the toast
    /// </remarks>
    procedure MakeToastEx(const AMsg: string; const AIsShort: Boolean = True);
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.Widget, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Support,
  // DW
  DW.Androidapi.JNI.Widget.Toast, DW.UIHelper;

{ TToast }

constructor TToast.Create;
begin
  inherited;
  FHandler := TJHandler.JavaClass.init(TJLooper.JavaClass.getMainLooper);
end;

class destructor TToast.DestroyClass;
begin
  FToast.Free;
end;

class procedure TToast.Make(const AMsg: string; const AIsShort: Boolean = True);
begin
  if FToast = nil then
    FToast := TToast.Create;
  FToast.MakeToast(AMsg, AIsShort);
end;

class procedure TToast.MakeEx(const AMsg: string; const AIsShort: Boolean);
begin
  if FToast = nil then
    FToast := TToast.Create;
  FToast.MakeToastEx(AMsg, AIsShort);
end;

procedure TToast.MakeToast(const AMsg: string; const AIsShort: Boolean = True);
begin
  FMsg := AMsg;
  FIsShort := AIsShort;
  FIsCustom := False;
  FHandler.post(Self);
end;

procedure TToast.MakeToastEx(const AMsg: string; const AIsShort: Boolean);
begin
  FMsg := AMsg;
  FIsShort := AIsShort;
  FIsCustom := True;
  FHandler.post(Self);
end;

procedure TToast.DoRun(const AToastLength: Integer);
begin
  TJToast.JavaClass.makeText(TAndroidHelper.Context.getApplicationContext, StrToJCharSequence(FMsg), AToastLength).show;
end;

procedure TToast.DoRunEx(const AToastLength: Integer);
const
  cResNameSuffix: array[Boolean] of string = ('light', 'dark');
var
  LToast: JToast;
  LView: JTextView;
  LBackground: JGradientDrawable;
  LResources: JResources;
  LIsDark: Boolean;
  LBackgroundColorID, LTextColorID: Integer;
begin
  LIsDark := TUIHelper.GetUserInterfaceStyle = TUserInterfaceStyle.Dark;
  LResources := TAndroidHelper.Context.getResources;
  LBackgroundColorID := LResources.getIdentifier(StringToJString('android:color/background_' + cResNameSuffix[LIsDark]), nil, nil);
  LTextColorID := LResources.getIdentifier(StringToJString('android:color/primary_text_' + cResNameSuffix[LIsDark]), nil, nil);
  LView := TJTextView.JavaClass.init(TAndroidHelper.Context.getApplicationContext);
  LView.setBackgroundColor(LResources.getColor(LBackgroundColorID));
  LView.setTextColor(LResources.getColor(LTextColorID));
  LView.setText(StrToJCharSequence(FMsg));
  LView.setPadding(16, 16, 16, 16);
  LView.setGravity(TJGravity.JavaClass.CENTER);
  LBackground := TJGradientDrawable.JavaClass.init;
  LBackground.setCornerRadius(48);
  LBackground.setColor(LResources.getColor(LBackgroundColorID));
  LView.setBackground(LBackground);
  LToast := TJToast.JavaClass.init(TAndroidHelper.Context.getApplicationContext);
  LToast.setView(LView);
  LToast.setDuration(AToastLength);
  LToast.show;
end;

procedure TToast.run;
var
  LToastLength: Integer;
begin
  if FIsShort then
    LToastLength := TJToast.JavaClass.LENGTH_SHORT
  else
    LToastLength := TJToast.JavaClass.LENGTH_LONG;
  if FIsCustom then
    DoRunEx(LToastLength)
  else
    DoRun(LToastLength);
end;

end.
