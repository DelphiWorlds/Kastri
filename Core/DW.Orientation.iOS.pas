unit DW.Orientation.iOS;

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
  System.TypInfo,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation;

type
  IOrientationNotification = interface(NSObject)
    ['{D8EE8419-FAE7-41D3-8DE9-91F1F5D4E007}']
    procedure onOrientationDidChange(notification: Pointer); cdecl;
    procedure onOrientationWillChange(notification: Pointer); cdecl;
  end;

  TOrientationNotificationListener = class(TOCLocal)
  private
    procedure AddOrientationObservers;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    procedure onOrientationDidChange(notification: Pointer); cdecl;
    procedure onOrientationWillChange(notification: Pointer); cdecl;
  end;

implementation

uses
  // RTL
  System.Messaging,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers,
  // iOS
  iOSapi.Helpers,
  // DW
  DW.Macapi.Helpers, DW.Messaging;

var
  OrientationListener: TOrientationNotificationListener;

{ TOrientationNotificationListener }

constructor TOrientationNotificationListener.Create;
begin
  inherited;
  AddOrientationObservers;
end;

procedure TOrientationNotificationListener.AddOrientationObservers;
begin
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('onOrientationDidChange:'),
    StringToID('UIDeviceOrientationDidChangeNotification'), nil);
  //  !!!! Undocumented!!
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('onOrientationWillChange:'),
    StringToID('UIDeviceOrientationWillChangeNotification'), nil);
end;

function TOrientationNotificationListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IOrientationNotification);
end;

procedure TOrientationNotificationListener.onOrientationDidChange(notification: Pointer);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TOrientationDidChangeMessage.Create);
end;

procedure TOrientationNotificationListener.onOrientationWillChange(notification: Pointer);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TOrientationWillChangeMessage.Create);
end;

initialization
  OrientationListener := TOrientationNotificationListener.Create;

finalization
  OrientationListener.Free;

end.
