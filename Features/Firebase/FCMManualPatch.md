# Manual patching instructions for FMX.PushNotification.FCM.iOS.pas

## Copy the source file

Copy `FMX.PushNotification.FCM.iOS.pas` from the the Delphi `source\fmx` folder to the `Features\Firebase` folder in your copy of Kastri (i.e. the same one as this readme file).

**NOTE**: The patched file needs to be in in the *search path* (like the `Features\Firebase` folder in your copy of Kastri, as described above) **OR** the *project folder*. **Do not patch it "in place" in the Delphi source**, because those folders are not normally in the search path.

## Modify the copied source file

Modify the uses clause to:

```delphi
uses
  System.SysUtils, System.Classes, System.JSON, System.PushNotification, System.Messaging, System.Notification, System.RTLConsts,
  Macapi.ObjectiveC, Macapi.Helpers, Macapi.ObjCRuntime,
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.UserNotifications, iOSapi.Helpers,
  DW.iOSapi.FirebaseCore, DW.iOSapi.FirebaseMessaging, DW.FCMManager,
  FMX.Forms, FMX.Platform;
```
Modify the `TFcmPushService` **declaration** to add this method just before the `Unregister` method:

```delphi
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
```

Add this method:

```delphi
procedure TFcmPushService.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  if not TiOSHelper.SharedApplication.isRegisteredForRemoteNotifications then
    TiOSHelper.SharedApplication.registerForRemoteNotifications;
end;
```

Modify the `TFcmPushService.StartService` method to:

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

In the procedure `TFcmPushService.Register` method, **REMOVE** this line:

```delphi
  TFIRApp.OCClass.configure;
```

..as it is called elsewhere by the Kastri code

### Additional changes for Delphi 13.1 - See below for additional changes for Delphi 13.0 or earlier

Modify the `TFcmPushService.RequestAuthorization` method to:

```delphi
procedure TFcmPushService.RequestAuthorization;
begin
  UserNotificationCenter.requestAuthorizationWithOptions(FCM.GetNativeAuthOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;
```

This enables the support for the full authorization options available via the `AuthOptions` property of `IFCMManager`

### Additional changes for Delphi 13.0 or earlier

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

Modify the `TFcmPushService.RegisterRemoteNotificationsIOS10OrLater` method to look like this:

```delphi
procedure TFcmPushService.RegisterRemoteNotificationsIOS10OrLater;
begin
  UserNotificationCenter.requestAuthorizationWithOptions(FCM.GetNativeAuthOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;
```

This enables the support for the full authorization options available via the `AuthOptions` property of `IFCMManager`
