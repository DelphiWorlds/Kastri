unit DW.iOSapi.BackgroundTasks;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

const
  BGTaskSchedulerErrorCodeUnavailable = 1;
  BGTaskSchedulerErrorCodeTooManyPendingTaskRequests = 2;
  BGTaskSchedulerErrorCodeNotPermitted = 3;

type
  BGTaskScheduler = interface;
  BGTaskRequest = interface;
  BGAppRefreshTaskRequest = interface;
  BGProcessingTaskRequest = interface;
  BGHealthResearchTaskRequest = interface;
  BGTask = interface;
  BGProcessingTask = interface;
  BGHealthResearchTask = interface;
  BGAppRefreshTask = interface;

  BGTaskSchedulerErrorCode = NSInteger;
  TBGTaskSchedulerBlockMethod1 = procedure(task: BGTask) of object;
  TBGTaskSchedulerBlockMethod2 = procedure(taskRequests: NSArray) of object;
  TBGTaskBlockMethod1 = procedure of object;

  BGTaskSchedulerClass = interface(NSObjectClass)
    ['{6DC56E3D-22FF-4684-A12C-F0ABB540D60C}']
    {class} function sharedScheduler: BGTaskScheduler; cdecl;
  end;

  BGTaskScheduler = interface(NSObject)
    ['{2458823E-238A-44E7-83F3-E08F7694197B}']
    procedure cancelAllTaskRequests; cdecl;
    procedure cancelTaskRequestWithIdentifier(identifier: NSString); cdecl;
    procedure getPendingTaskRequestsWithCompletionHandler(completionHandler: TBGTaskSchedulerBlockMethod2); cdecl;
    function registerForTaskWithIdentifier(identifier: NSString; usingQueue: dispatch_queue_t;
      launchHandler: TBGTaskSchedulerBlockMethod1): Boolean; cdecl;
    function submitTaskRequest(taskRequest: BGTaskRequest; error: PPointer): Boolean; cdecl;
  end;
  TBGTaskScheduler = class(TOCGenericImport<BGTaskSchedulerClass, BGTaskScheduler>) end;

  BGTaskRequestClass = interface(NSObjectClass)
    ['{536730D7-EA86-44FD-A17B-4DB74A678095}']
    {class} function new: Pointer; cdecl;
  end;

  BGTaskRequest = interface(NSObject)
    ['{BC75CF9C-54FA-46B3-8DBB-7B9ACFF028B9}']
    function earliestBeginDate: NSDate; cdecl;
    function identifier: NSString; cdecl;
    procedure setEarliestBeginDate(earliestBeginDate: NSDate); cdecl;
  end;
  TBGTaskRequest = class(TOCGenericImport<BGTaskRequestClass, BGTaskRequest>) end;

  BGAppRefreshTaskRequestClass = interface(BGTaskRequestClass)
    ['{5597940A-19A1-4329-BF30-BDFF8E646766}']
  end;

  BGAppRefreshTaskRequest = interface(BGTaskRequest)
    ['{FCBAA338-8FC1-4DA6-A572-A3AB8B1582B3}']
    function initWithIdentifier(identifier: NSString): Pointer; cdecl;
  end;
  TBGAppRefreshTaskRequest = class(TOCGenericImport<BGAppRefreshTaskRequestClass, BGAppRefreshTaskRequest>) end;

  BGProcessingTaskRequestClass = interface(BGTaskRequestClass)
    ['{26A61A53-F8E6-4BA8-A0E9-67F4242DFC47}']
  end;

  BGProcessingTaskRequest = interface(BGTaskRequest)
    ['{C25D675F-067C-4D61-A360-5506D802EB3B}']
    function initWithIdentifier(identifier: NSString): Pointer; cdecl;
    function requiresExternalPower: Boolean; cdecl;
    function requiresNetworkConnectivity: Boolean; cdecl;
    procedure setRequiresExternalPower(requiresExternalPower: Boolean); cdecl;
    procedure setRequiresNetworkConnectivity(requiresNetworkConnectivity: Boolean); cdecl;
  end;
  TBGProcessingTaskRequest = class(TOCGenericImport<BGProcessingTaskRequestClass, BGProcessingTaskRequest>) end;

  BGHealthResearchTaskRequestClass = interface(BGProcessingTaskRequestClass)
    ['{50722FB4-23B9-47FD-9B53-0FD2CEABED99}']
  end;

  BGHealthResearchTaskRequest = interface(BGProcessingTaskRequest)
    ['{D6EC8D1B-B547-4F41-AF42-701AC2D1C404}']
    function protectionTypeOfRequiredData: NSFileProtectionType; cdecl;
    procedure setProtectionTypeOfRequiredData(protectionTypeOfRequiredData: NSFileProtectionType); cdecl;
  end;
  TBGHealthResearchTaskRequest = class(TOCGenericImport<BGHealthResearchTaskRequestClass, BGHealthResearchTaskRequest>) end;

  BGTaskClass = interface(NSObjectClass)
    ['{AFFFBDB5-FDA1-405B-8D70-E0DC0F4135FF}']
  end;

  BGTask = interface(NSObject)
    ['{E1678BA5-B2EC-41C7-8283-BE7B9C5F6200}']
    function expirationHandler: TBGTaskBlockMethod1; cdecl;
    function identifier: NSString; cdecl;
    procedure setExpirationHandler(expirationHandler: TBGTaskBlockMethod1); cdecl;
    procedure setTaskCompletedWithSuccess(success: Boolean); cdecl;
  end;
  TBGTask = class(TOCGenericImport<BGTaskClass, BGTask>) end;

  BGProcessingTaskClass = interface(BGTaskClass)
    ['{A20FE6A2-585B-490E-9C83-F6C59438A0E7}']
  end;

  BGProcessingTask = interface(BGTask)
    ['{82AAAAF7-0C95-4719-9FF3-D9122A3FB6F6}']
  end;
  TBGProcessingTask = class(TOCGenericImport<BGProcessingTaskClass, BGProcessingTask>) end;

  BGHealthResearchTaskClass = interface(BGProcessingTaskClass)
    ['{62B45518-8BC9-4867-8398-E43DDBBDEE2A}']
  end;

  BGHealthResearchTask = interface(BGProcessingTask)
    ['{4D186B97-A01C-4AEB-A91E-98C2A5D4B8B5}']
  end;
  TBGHealthResearchTask = class(TOCGenericImport<BGHealthResearchTaskClass, BGHealthResearchTask>) end;

  BGAppRefreshTaskClass = interface(BGTaskClass)
    ['{F7502BBC-7985-4676-915A-B6C8A87D7EE1}']
  end;

  BGAppRefreshTask = interface(BGTask)
    ['{17D697E0-46C3-4245-864A-0F83C879FFDE}']
  end;
  TBGAppRefreshTask = class(TOCGenericImport<BGAppRefreshTaskClass, BGAppRefreshTask>) end;

function BGTaskSchedulerErrorDomain: NSErrorDomain;

const
  libBackgroundTasks = '/System/Library/Frameworks/BackgroundTasks.framework/BackgroundTasks';

implementation

uses
  // Posix
  Posix.Dlfcn;

var
  BackgroundTasksModule: THandle;

function BGTaskSchedulerErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libBackgroundTasks, 'BGTaskSchedulerErrorDomain');
end;

initialization
  BackgroundTasksModule := dlopen(MarshaledAString(libBackgroundTasks), RTLD_LAZY);

finalization
  dlclose(BackgroundTasksModule);

end.