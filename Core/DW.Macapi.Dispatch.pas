unit DW.Macapi.Dispatch;

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

// This is an extension of the dispatch functions
// See also: http://ridingdelphi.blogspot.com.au/2014/01/the-quest-to-migrate-ios-squarecam-app_3169.html

interface

uses
  // macOS
  Macapi.Dispatch;

type
  dispatch_work_t = reference to procedure;
  dispatch_block_t = Pointer; // dispatch_work_t;
  dispatch_source_t = Pointer;
  dispatch_function_t = procedure(context: Pointer); cdecl;

  TGrandCentral = record
  private
    class var FMainQueue: dispatch_queue_t;
  public
    class function GetMainQueue: dispatch_queue_t; static;
    class procedure DispatchAsync(const AProc: dispatch_work_t; const AQueue: dispatch_queue_t = 0); static;
  end;

implementation

uses
  // RTL
  System.SysUtils;

const
{$IF Defined(IOS)}
  libdispatch = '/usr/lib/libSystem.dylib';
{$ELSE}
  libdispatch = '/usr/lib/system/libdispatch.dylib';
{$ENDIF}

procedure dispatch_sync_f(queue: dispatch_queue_t; context: Pointer; work: dispatch_function_t); cdecl;
  external libdispatch name _PU + 'dispatch_sync_f';

{ Grand Central Dispatch implementation }

function dispatch_get_main_queue: dispatch_queue_t;
var
  LModule: HMODULE;
begin
  Result := 0;
  LModule := LoadLibrary(PWideChar(libdispatch));
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
