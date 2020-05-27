unit DW.MultiReceiver.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero;

type
  TMultiReceiver = class;

  TMultiReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  private
    FMultiReceiver: TMultiReceiver;
  public
    { JFMXBroadcastReceiverListener }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AMultiReceiver: TMultiReceiver);
  end;

  TMultiReceiver = class(TObject)
  private
    FBroadcastReceiver: JFMXBroadcastReceiver;
    FIntentFilter: JIntentFilter;
    FLocal: Boolean;
    FReceiverListener: TMultiReceiverListener;
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
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.SupportV4;

{ TMultiReceiverListener }

constructor TMultiReceiverListener.Create(const AMultiReceiver: TMultiReceiver);
begin
  inherited Create;
  FMultiReceiver := AMultiReceiver;
end;

procedure TMultiReceiverListener.onReceive(context: JContext; intent: JIntent);
begin
  FMultiReceiver.Receive(context, intent);
end;

{ TMultiReceiver }

constructor TMultiReceiver.Create(const ALocal: Boolean = False);
begin
  inherited Create;
  FLocal := ALocal;
  FReceiverListener := TMultiReceiverListener.Create(Self);
  FBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FReceiverListener);
  FIntentFilter := TJIntentFilter.JavaClass.init;
  ConfigureActions;
  if not FLocal then
    TAndroidHelper.Context.registerReceiver(FBroadcastReceiver, FIntentFilter)
  else
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FBroadcastReceiver, FIntentFilter);
end;

destructor TMultiReceiver.Destroy;
begin
  if not FLocal then
    TAndroidHelper.Context.unregisterReceiver(FBroadcastReceiver)
  else
    TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).unregisterReceiver(FBroadcastReceiver);
  FBroadcastReceiver := nil;
end;

end.
