unit DW.Macapi.Dispatch;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}


// This is an extension of the dispatch functions
// See also: http://ridingdelphi.blogspot.com.au/2014/01/the-quest-to-migrate-ios-squarecam-app_3169.html

interface

uses
  // macOS
  Macapi.CoreServices, Macapi.Dispatch;

const
{$IF Defined(IOS)}
  libDispatch = '/usr/lib/libSystem.dylib';
{$ELSE}
  libDispatch = '/usr/lib/system/libdispatch.dylib';
{$ENDIF}
  DISPATCH_QUEUE_PRIORITY_DEFAULT = 0;
  DISPATCH_TIME_NOW = 0;
  NSEC_PER_MSEC:UInt64 = 1000000;

type
  dispatch_work_t = reference to procedure;
  dispatch_queue_t = dispatch_object_t;
  dispatch_group_t = dispatch_object_t;
  dispatch_source_t = dispatch_object_t;
  dispatch_source_type_t = dispatch_object_t;
  dispatch_block_t = Pointer;
  dispatch_data_t = Pointer;

  TGrandCentral = record
  private
    class var FMainQueue: dispatch_queue_t;
  public
    class function GetMainQueue: dispatch_queue_t; static;
    class procedure DispatchAsync(const AProc: dispatch_work_t; const AQueue: dispatch_queue_t = 0); static;
  end;

function dispatch_get_global_queue(priority: LongInt; flags: LongInt): dispatch_queue_t; cdecl;
  external libDispatch name _PU + 'dispatch_get_global_queue';
procedure dispatch_resume(source: dispatch_source_t); cdecl;
  external libDispatch name _PU + 'dispatch_resume';
function dispatch_source_create(&type: dispatch_source_type_t; handle: NativeUInt; mask: NativeUInt; queue: dispatch_queue_t): dispatch_source_t; cdecl;
  external libDispatch name _PU + 'dispatch_source_create';
procedure dispatch_source_set_event_handler(source: dispatch_source_t; handler: dispatch_block_t); cdecl;
  external libDispatch name _PU + 'dispatch_source_set_event_handler';
procedure dispatch_source_set_timer(source: dispatch_source_t; start: dispatch_time_t; interval: LongInt; leeway: LongInt); cdecl;
  external libDispatch name _PU + 'dispatch_source_set_timer';
function dispatch_time(when: dispatch_time_t; delta: Int64): dispatch_time_t; cdecl;
  external libDispatch name _PU + 'dispatch_time';

implementation

uses
  // RTL
  System.SysUtils;

procedure dispatch_sync_f(queue: dispatch_queue_t; context: Pointer; work: dispatch_function_t); cdecl;
  external libDispatch name _PU + 'dispatch_sync_f';

{ Grand Central Dispatch implementation }

function dispatch_get_main_queue: dispatch_queue_t;
var
  LModule: HMODULE;
begin
  Result := 0;
  LModule := LoadLibrary(PWideChar(libDispatch));
  if LModule <> 0 then
  try
    Result := dispatch_queue_t(GetProcAddress(LModule, PWideChar('_dispatch_main_q')));
  finally
    FreeLibrary(LModule);
  end;
end;

procedure DispatchCallback(context: Pointer); cdecl;
var
  LCallbackProc: dispatch_work_t absolute context;
begin
  try
    LCallbackProc;
  finally
    IInterface(context)._Release;
  end;
end;

procedure dispatch_async(queue: dispatch_queue_t; work: dispatch_work_t);
var
  LCallback: Pointer absolute work;
begin
  IInterface(LCallback)._AddRef;
  dispatch_async_f(queue, LCallback, DispatchCallback);
end;

procedure dispatch_sync(queue: dispatch_queue_t; work: dispatch_work_t);
var
  LCallback: Pointer absolute work;
begin
  IInterface(LCallback)._AddRef;
  dispatch_sync_f(queue, LCallback, DispatchCallback);
end;

{ TGrandCentral }

class procedure TGrandCentral.DispatchAsync(const AProc: dispatch_work_t; const AQueue: dispatch_queue_t = 0);

begin
  if AQueue = 0 then
    dispatch_async(GetMainQueue, AProc)
  else
    dispatch_async(AQueue, AProc)
end;

class function TGrandCentral.GetMainQueue: dispatch_queue_t;
begin
  if FMainQueue = 0 then
    FMainQueue := dispatch_get_main_queue;
  Result := FMainQueue;
end;

end.
