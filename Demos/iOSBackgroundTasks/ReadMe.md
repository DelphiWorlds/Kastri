# Background Tasks support for iOS

## Description

This project demonstrates the use of an implementation of background tasks on iOS.

**NOTE: This code will work ONLY on iOS 13 and above**

Note also that background tasks on iOS are **short lived**. App Refresh tasks typically run < 30 seconds, and Processing tasks for up to a few minutes.

They **do NOT run continuously**, like services can (to some extent) on Android.

## TCustomBackgroundTask

This class is provided for developers who wish to have finer grained control over the background task. Create a descendant of this class and override at least the abstract methods:

* Execute - Performs the work of the task being executed
* GetIdentifier - Provides a unique identifier for the task. See below regarding modification of `info.plist.TemplateiOS.xml`
* GetTaskKind - Determines what kind of task, i.e. either `TBackgroundTask.AppRefresh` or `TBackgroundTask.Processing`

Optionally override any or all of the following methods if the task kind is `TBackgroundTask.Processing`:

* Cancel - Override this method if there is anything to clean up if a task is cancelled for any reason
* GetRequiresConnectivity - If not overridden, defaults to True. Determines whether or not the task should run only when connectivity is available
* GetRequiresExternalPower - If not overridden, defaults to True. Determines whether or not the task should run only when external power is available
* ScheduleError - Override this method to gain access to the error message if a scheduling error occurs

Note that a background task is more likely to be executed if it requires external power (and actually is connected, of course)

## BackgroundTaskManager

BackgroundTaskManager is a convenient means of creating background tasks without needing to define a descendant of `TCustomBackgroundTask`. The `ScheduleTask` method creates a background task instance that provides the requirements for the task to execute. Example:

```delphi
  FMyTask := BackgroundTaskManager.ScheduleTask('com.delphiworlds.bgdemo.processing', TBackgroundTaskKind.Processing, 5 * 60,
    function: Boolean
    var
      LHTTP: THTTPClient;
      LResponse: IHTTPResponse;
      LStream: TMemoryStream;
      LFileName: string;
    begin
      LHTTP := THTTPClient.Create;
      try
        LResponse := LHTTP.Get('https://picsum.photos/300/200');
        if LResponse.StatusCode = 200 then
        begin
          LFileName := TPath.Combine(TPath.GetDocumentsPath, 'Random.png');
          LStream := TMemoryStream.Create;
          try
            LStream.CopyFrom(LResponse.ContentStream);
            LStream.SaveToFile(LFileName);
          finally
            LStream.Free;
          end;
        end;
      finally
        LHTTP.Free;
      end;
      Result := True;
    end
  );
```

Alternatively, pass a reference to a function that returns a `Boolean` (instead of the anonymous function in the example above), e.g.:

```delphi
  FMyTask := BackgroundTaskManager.ScheduleTask('com.delphiworlds.bgdemo.processing', TBackgroundTaskKind.Processing, 5 * 60, MyTaskHandler);
```

Where `MyTaskHandler` is your function that performs the work. 

In these examples, `FMyTask` is a **global** reference of the type `IBackgroundTask`. Since it is an interface reference, it will be free'd when it falls out of scope.

In each example above, the task is scheduled to run every 5 minutes (see [below](#actual-execution-of-tasks) regarding **actual** execution times), and passes the function to be executed when the task is actually run.

BackgroundTaskManager also has an event handler for when the user restricts or allows background refresh for the application (via `Settings > General > Background App Refresh`) called `OnBackgroundRefreshStatusChange`. Alternatively the `BackgroundRefreshStatus` property could be examined, for example when the application becomes active, to inform the user that certain application tasks will not be available and may therefore diminish the user experience. 

## Scheduling of tasks

Tasks are **registered** when the background task instance is created, however as the name implies, background tasks are **executed** when the application is in the **background**. To this end, tasks are not actually scheduled until such time as the application moves to the background. 

## Actual execution of tasks

Although an interval can be specified for task execution, this cannot be relied upon - iOS will execute tasks only when conditions are optimal. I highly recommend reading [this post on the Apple developer forums](https://forums.developer.apple.com/forums/thread/685525), specifically the section that mentions background execution time. The link to the video "Background Execution Demystified" is actually broken, however you can view it via [this link](https://devstreaming-cdn.apple.com/videos/wwdc/2020/10063/3/2E1C3BA0-2643-4330-A5B2-3A9878453987/master.m3u8) **if you use the Safari browser**.

## Modifications to info.plist.TemplateiOS.xml

As Delphi is yet to support registration of background tasks "out of the box", you will need to modify `info.plist.TemplateiOS.xml`. The first entry is `BGTaskSchedulerPermittedIdentifiers`:

```xml
  <key>BGTaskSchedulerPermittedIdentifiers</key>
  <array>
    <string>com.delphiworlds.bgdemo.apprefresh</string>
    <string>com.delphiworlds.bgdemo.processing</string>
  </array>
```

As can be seen, this entry is an array of strings, including an identifier for each of the background tasks. The format chosen for the identifiers here is just to ensure they're unique.

The other entry is for `UIBackgroundModes`:

```xml
  <key>UIBackgroundModes</key>
  <array>
    <string>fetch</string>
    <string>processing</string>
  </array>
```

Delphi has support for setting these modes in the Version Info of Project Options, however it is **presently missing the `processing` option**. Unfortunately, it's not possible to set the `fetch` option in the Project Options, **and** customise the array, so `UIBackgroundModes` in the Version Info **must be left blank**, in order for the entry to work.

## Supported Delphi versions

Delphi 12.x, Delphi 11.x. It may also work in earlier versions.
