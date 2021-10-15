unit DW.Classes.Helpers;

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
  // RTL
  System.Classes;

type
  /// <summary>
  ///   Thread-based convenience methods.
  /// </summary>
  /// <remarks>
  ///   "TDo" as in "Do" something - shortest form of a word associated with running a task that I could think of
  /// </remarks>
  TDo = record
  public
    /// <summary>
    ///   Determines whether the current execution is in the main thread
    /// </summary>
    class function IsMainThread: Boolean; static;
    /// <summary>
    ///   Queues a method for execution after an optional delay
    /// </summary>
    class procedure Queue(const AProc: TThreadProcedure; const ADelay: Integer = 0); static;
    /// <summary>
    ///   Queues a method in the main thread, if necessary
    /// </summary>
    class procedure QueueMain(const ARunProc: TThreadProcedure); static;
    /// <summary>
    ///   Synchronizes a method for execution after an optional delay
    /// </summary>
    class procedure Sync(const AProc: TThreadProcedure; const ADelay: Integer = 0); static;
    /// <summary>
    ///   Syncs a method in the main thread, if necessary
    /// </summary>
    class procedure SyncMain(const ARunProc: TThreadProcedure); static;
    /// <summary>
    ///   Runs a method in a thread
    /// </summary>
    class procedure Run(const ARunProc: TThreadProcedure); static;
    /// <summary>
    ///   Runs a method in a thread and queues/syncs a callback if supplied
    /// </summary>
    class procedure RunQueue(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil); static;
    /// <summary>
    ///   Runs a method in a thread and queues/syncs a callback if supplied
    /// </summary>
    class procedure RunSync(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil); static;
  end;

  TStreamHelper = record
  public
    class function AsString(const AStream: TStream): string; static;
  end;

  TResourceHelper = record
  public
    class function LoadString(const AResourceName: string): string; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Types;

{ TDo }

class function TDo.IsMainThread: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID = MainThreadID;
end;

class procedure TDo.Queue(const AProc: TThreadProcedure; const ADelay: Integer = 0);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(ADelay);
      TThread.Queue(nil, AProc);
    end
  ).Start;
end;

class procedure TDo.QueueMain(const ARunProc: TThreadProcedure);
begin
  if not IsMainThread then
    TThread.Queue(nil, ARunProc)
  else
    ARunProc;
end;

class procedure TDo.Sync(const AProc: TThreadProcedure; const ADelay: Integer = 0);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(ADelay);
      TThread.Synchronize(nil, AProc);
    end
  ).Start;
end;

class procedure TDo.SyncMain(const ARunProc: TThreadProcedure);
begin
  if not IsMainThread then
    TThread.Synchronize(nil, ARunProc)
  else
    ARunProc;
end;

class procedure TDo.Run(const ARunProc: TThreadProcedure);
begin
  RunQueue(ARunProc);
end;

class procedure TDo.RunQueue(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ARunProc;
      if Assigned(ACallbackProc) then
        TThread.Queue(nil, ACallbackProc);
    end
  ).Start;
end;

class procedure TDo.RunSync(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ARunProc;
      if Assigned(ACallbackProc) then
        TThread.Synchronize(nil, ACallbackProc);
    end
  ).Start;
end;

{ TStreamHelper }

class function TStreamHelper.AsString(const AStream: TStream): string;
var
  LStream: TStringStream;
begin
  AStream.Position := 0;
  LStream := TStringStream.Create;
  try
    LStream.CopyFrom(AStream, AStream.Size);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

{ TResourceHelper }

class function TResourceHelper.LoadString(const AResourceName: string): string;
var
  LResourceStream: TStream;
  LStringStream: TStringStream;
begin
  if FindResource(HInstance, PChar(AResourceName), RT_RCDATA) > 0 then
  begin
    LResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      LStringStream := TStringStream.Create;
      try
        LStringStream.CopyFrom(LResourceStream, LResourceStream.Size);
        Result := LStringStream.DataString;
      finally
        LStringStream.Free;
      end;
    finally
      LResourceStream.Free;
    end;
  end;
end;

end.
