unit DW.MultiReceiver.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero;

type
  JDWMultiBroadcastReceiver = interface;

  [JavaSignature('com/delphiworlds/kastri/DWMultiBroadcastReceiverDelegate')]
  JDWMultiBroadcastReceiverDelegate = interface(IJavaInstance)
    ['{1AD78992-D81F-48A4-B341-F82B43094B67}']
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;

  JDWMultiBroadcastReceiverClass = interface(JBroadcastReceiverClass)
    ['{92B62B20-F752-4D10-B336-8B602E506BD2}']
    {class} function init(delegate: JDWMultiBroadcastReceiverDelegate): JDWMultiBroadcastReceiver; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWMultiBroadcastReceiver')]
  JDWMultiBroadcastReceiver = interface(JBroadcastReceiver)
    ['{B2D7FDB7-FC47-40B5-8A48-1D6A723C0494}']
  end;
  TJDWMultiBroadcastReceiver = class(TJavaGenericImport<JDWMultiBroadcastReceiverClass, JDWMultiBroadcastReceiver>) end;

  TCustomMultiReceiver = class;

  TMultiBroadcastReceiverDelegate = class(TJavaLocal, JDWMultiBroadcastReceiverDelegate)
  private
    FMultiReceiver: TCustomMultiReceiver;
  public
    { JDWMultiBroadcastReceiverDelegate }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AMultiReceiver: TCustomMultiReceiver);
  end;

  TCustomMultiReceiver = class(TObject)
  private
    FReceiver: JBroadcastReceiver;
    FReceiverDelegate: JDWMultiBroadcastReceiverDelegate;
  protected
    procedure Receive(context: JContext; intent: JIntent); virtual; abstract;
  public
    constructor Create;
    property Receiver: JBroadcastReceiver read FReceiver;
  end;

  TMultiReceiver = class(TCustomMultiReceiver)
  private
    FIntentFilter: JIntentFilter;
    FLocal: Boolean;
    function HasNonSystemActions: Boolean;
    procedure RegisterReceiver;
  protected
    procedure ConfigureActions; virtual; abstract;
    function GetResultCode: Integer;
    property IntentFilter: JIntentFilter read FIntentFilter;
  public
    constructor Create(const ALocal: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Os,
  // DW
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.Content, DW.Androidapi.JNI.AndroidX.LocalBroadcastManager,
  {$ENDIF}
  DW.Android.Helpers;

{ TMultiBroadcastReceiverDelegate }

constructor TMultiBroadcastReceiverDelegate.Create(const AMultiReceiver: TCustomMultiReceiver);
begin
  inherited Create;
  FMultiReceiver := AMultiReceiver;
end;

procedure TMultiBroadcastReceiverDelegate.onReceive(context: JContext; intent: JIntent);
begin
  FMultiReceiver.Receive(context, intent);
end;

{ TCustomMultiReceiver }

constructor TCustomMultiReceiver.Create;
begin
  inherited;
  FReceiverDelegate := TMultiBroadcastReceiverDelegate.Create(Self);
  FReceiver := TJDWMultiBroadcastReceiver.JavaClass.init(FReceiverDelegate);
end;

{ TMultiReceiver }

constructor TMultiReceiver.Create(const ALocal: Boolean = False);
begin
  inherited Create;
  FLocal := ALocal;
  FIntentFilter := TJIntentFilter.JavaClass.init;
  ConfigureActions;
  RegisterReceiver;
end;

destructor TMultiReceiver.Destroy;
begin
  if not FLocal then
    TAndroidHelper.Context.unregisterReceiver(FReceiver)
  else
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).unregisterReceiver(FReceiver);
end;

function TMultiReceiver.GetResultCode: Integer;
begin
  Result := FReceiver.getResultCode;
end;

function TMultiReceiver.HasNonSystemActions: Boolean;
var
  I: Integer;
  LAction: string;
begin
  Result := False;
  for I := 0 to FIntentFilter.countActions - 1 do
  begin
    LAction := JStringToString(FIntentFilter.getAction(I));
    // Crude way of determining whether or not this is a system broadcast
    if not (LAction.StartsWith('android.') or LAction.StartsWith('com.android.')) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TMultiReceiver.RegisterReceiver;
{$IF CompilerVersion > 35}
var
  LFlags: Integer;
{$ENDIF}
begin
  if not FLocal then
  begin
    {$IF CompilerVersion > 35}
    // https://developer.android.com/about/versions/14/behavior-changes-14#runtime-receivers-exported
    LFlags := 0;
    if HasNonSystemActions then
      LFlags := TJContextCompat.JavaClass.RECEIVER_EXPORTED;
    TJContextCompat.JavaClass.registerReceiver(TAndroidHelper.Context, FReceiver, FIntentFilter, LFlags);
    {$ELSE}
    TAndroidHelper.Context.registerReceiver(FReceiver, FIntentFilter);
    {$ENDIF}
  end
  else
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FReceiver, FIntentFilter);
end;

end.
