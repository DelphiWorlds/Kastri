unit DW.WebBroker.Logger;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  ILogListener = interface(IInterface)
    ['{B022E763-8464-4D9A-9349-6B63CB853714}']
    procedure Log(const AMsg: string);
  end;

  TLogListeners = TArray<ILogListener>;

  /// <summary>
  ///   Utility used for logging messages
  /// </summary>
  TLogger = record
  private
    class var FListeners: TLogListeners;
  public
    class var IsEnabled: Boolean;
    class procedure AddListener(const AListener: ILogListener); static;
    class procedure Log(const AMsg: string); static;
    class procedure RemoveListener(const AListener: ILogListener); static;
  end;

implementation

{ TLogger }

class procedure TLogger.AddListener(const AListener: ILogListener);
begin
  FListeners := FListeners + [AListener];
end;

class procedure TLogger.RemoveListener(const AListener: ILogListener);
var
  I: Integer;
begin
  for I := 0 to Length(FListeners) - 1 do
  begin
    if FListeners[I] = AListener then
    begin
      Delete(FListeners, I, 1);
      Break;
    end;
  end;
end;

class procedure TLogger.Log(const AMsg: string);
var
  AListener: ILogListener;
begin
  if IsEnabled then
  begin
    for AListener in FListeners do
      AListener.Log(AMsg);
  end;
end;

end.
