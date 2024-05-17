# Manual patching instructions for FMX.PushNotification.FCM.iOS.pas

## Copy the source file

Copy `FMX.PushNotification.FCM.iOS.pas` from the the Delphi `source\fmx` folder to the `Features\Firebase` folder in your copy of Kastri (i.e. the same one as this readme file).

**NOTE**: The patched file needs to be in in the *search path* (like the `Features\Firebase` folder in your copy of Kastri, as described above) **OR** the *project folder*. **Do not patch it "in place" in the Delphi source**, because those folders are not normally in the search path.

## Modify the copied source file

In the `interface` section:

Modify the uses clause to:

```delphi
uses
  System.SysUtils, System.Classes, System.JSON, System.PushNotification, System.Messaging, System.Notification, System.RTLConsts,
  Macapi.ObjectiveC, Macapi.Helpers, Macapi.ObjCRuntime,
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.UserNotifications, iOSapi.Helpers,
  DW.iOSapi.FirebaseCore, DW.iOSapi.FirebaseMessaging,
  FMX.Forms, FMX.Platform;
```

Modify the `TFIRMessagingDelegate` declaration to:

```delphi
  TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
  private
    FPushService: TFcmPushService;
  public
    { FIRMessagingDelegate }
    procedure messaging(messaging: FIRMessaging; didReceiveRegistrationToken: NSString); cdecl;
  public
    constructor Create(const APushService: TFcmPushService);
  end;
```

In the `implementation` section:

Remove these methods:

```delphi
procedure TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
procedure TFIRMessagingDelegate.didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage);
procedure TFIRMessagingDelegate.didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
procedure TFIRMessagingDelegate.didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
procedure TFIRMessagingDelegate.ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
```

Add this method:

```delphi
procedure TFIRMessagingDelegate.messaging(messaging: FIRMessaging; didReceiveRegistrationToken: NSString);
begin
  FPushService.SetDeviceToken(NSStrToStr(didReceiveRegistrationToken));
end;
```

In the procedure TFcmPushService.Register method, **REMOVE** this line:

```delphi
  TFIRApp.OCClass.configure;
```

Modify the method to:

```delphi
procedure TFcmPushService.StartService;
begin
  FStatus := TPushService.TStatus.Starting;
  DoChange([TChange.Status]);
  RequestAuthorization;
  Register;
end;
```

i.e. the calls to `RequestAuthorization` and `Register` are **swapped**.
