unit DW.FMX.Helpers;

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
  System.SysUtils, System.Messaging;

type
  /// <summary>
  ///   Utility class for deferring execution of methods in FMX apps
  /// </summary>
  TIdleCaller = class(TObject)
  public
    /// <summary>
    ///   Calls the method AProc the next time an idle message is received
    /// </summary>
    /// <remarks>
    ///   Can be useful for delaying code that might prevent the debugger from being able to hook the app process if the method causes
    ///   a crash for example inside the OnCreate event handler of the main form.
    ///   Example: Move the suspicious code out of the OnCreate event handler into a method called DoSomethingThatMightCrash, and replace with:
    ///     TIdleCaller.CallOnNextIdle(DoSomethingThatMightCrash);
    /// </remarks>
    class procedure CallOnNextIdle(const AProc: TProc);
  end;

implementation

uses
  // FMX
  FMX.Types;

{ TIdleCaller }

class procedure TIdleCaller.CallOnNextIdle(const AProc: TProc);
var
  LListener: TMessageListener;
begin
  LListener := procedure(const Sender: TObject; const M: TMessage)
  begin
    TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, LListener);
    try
      AProc;
    finally
      LListener := nil;
    end;
  end;
  TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, LListener);
end;

end.
