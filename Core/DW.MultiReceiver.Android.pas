unit DW.MultiReceiver.Android;

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

  TMultiReceiver = class;

  TMultiBroadcastReceiverDelegate = class(TJavaLocal, JDWMultiBroadcastReceiverDelegate)
  private
    FMultiReceiver: TMultiReceiver;
  public
    { JDWMultiBroadcastReceiverDelegate }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AMultiReceiver: TMultiReceiver);
  end;

  TMultiReceiver = class(TObject)
  private
    FIntentFilter: JIntentFilter;
    FLocal: Boolean;
    FReceiver: JBroadcastReceiver;
    FReceiverDelegate: JDWMultiBroadcastReceiverDelegate;
  protected
    procedure Receive(context: JContext; intent: JIntent); virtual; abstract;
    procedure ConfigureActions; virtual; abstract;
    property IntentFilter: JIntentFilter read FIntentFilter;
  public
    constructor Create(const ALocal: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.SupportV4, DW.Android.Helpers;

{ TMultiBroadcastReceiverDelegate }

constructor TMultiBroadcastReceiverDelegate.Create(const AMultiReceiver: TMultiReceiver);
begin
  inherited Create;
  FMultiReceiver := AMultiReceiver;
end;

procedure TMultiBroadcastReceiverDelegate.onReceive(context: JContext; intent: JIntent);
begin
  FMultiReceiver.Receive(context, intent);
end;

{ TMultiReceiver }

constructor TMultiReceiver.Create(const ALocal: Boolean = False);
begin
  inherited Create;
  FLocal := ALocal;
  FReceiverDelegate := TMultiBroadcastReceiverDelegate.Create(Self);
  FReceiver := TJDWMultiBroadcastReceiver.JavaClass.init(FReceiverDelegate);
  FIntentFilter := TJIntentFilter.JavaClass.init;
  ConfigureActions;
  if not FLocal then
  begin
    if System.DelphiActivity = nil then
      TOSLog.d('TAndroidHelper.Context.registerReceiver(FReceiver)');
    TAndroidHelper.Context.registerReceiver(FReceiver, FIntentFilter);
  end
  else
  begin
    if System.DelphiActivity = nil then
      TOSLog.d('TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FReceiver)');
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FReceiver, FIntentFilter);
  end;
end;

destructor TMultiReceiver.Destroy;
begin
  if not FLocal then
  begin
    if System.DelphiActivity = nil then
      TOSLog.d('TAndroidHelper.Context.unregisterReceiver(FReceiver)');
    TAndroidHelper.Context.unregisterReceiver(FReceiver);
  end
  else
  begin
    if System.DelphiActivity = nil then
      TOSLog.d('TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).unregisterReceiver(FReceiver)');
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).unregisterReceiver(FReceiver);
  end;
end;

end.
