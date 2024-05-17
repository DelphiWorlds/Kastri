## Android App Focus Demo

### Description

Demonstrates the use of the `DW.StartupHook.Android` unit in the `Core` folder

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

### Technical details

Application events can be handled by using `TMessageManager` to `Subscribe` to events by using `TApplicationEventMessage`, however there are at least two events that are missing. 

In the `Androidapi.AppGlue` unit in the Delphi source, there are "hooks" into native methods such as for the `OnPause` and `OnResume` methods of the activity. The `TPlatformAndroid` class in `FMX.Platform.Android` connects to these events, and sends a `TApplicationEventMessage` that your app can listen for, such as in this demo. Unfortunately, the `GainedFocus` and `LostFocus` events handled by the AppGlue are **not** handled by `TPlatformAndroid` - this has been [reported here](https://quality.embarcadero.com/browse/RSP-35891), but will need to wait until at least the next _major_ version of Delphi, since it would be interface-breaking. 

What the `DW.StartUpHook.Android` unit does is re-routes the events in the AppGlue so that the missing events **_can_** be handled, and sends a TWindowFocusChanged method that the app can listen for.

### When an app "loses focus"

Examples of when an app might "lose focus" but not ectually enter the background are:

* The user swipes down from the top of the screen to display the settings icons, Notifications area etc
* The user taps the microphone icon on the virtual keyboard to invoke Google Voice Typing

There may be a number of other scenarios, however the above scenarios can be common. 



