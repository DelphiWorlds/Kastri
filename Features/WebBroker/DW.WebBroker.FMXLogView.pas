unit DW.WebBroker.FMXLogView;

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

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  DW.WebBroker.Logger, FMX.StdCtrls, FMX.Layouts;

type
  /// <summary>
  ///    Basic GUI log message listener
  /// </summary>
  /// <remarks>
  ///    Use this in standalone apps ONLY - it will not work for example in an ISAPI DLL
  /// </remarks>
  TLogView = class(TForm, ILogListener)
    Memo: TMemo;
    BottomLayout: TLayout;
    ClearButton: TButton;
    procedure ClearButtonClick(Sender: TObject);
  private
    procedure InternalLog(const AMsg: string);
  public
    { ILogListener }
    procedure Log(const AMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
  end;

var
  LogView: TLogView;

implementation

{$R *.fmx}

{ TFMXLogView }

constructor TLogView.Create(AOwner: TComponent);
begin
  inherited;
  TLogger.AddListener(Self);
end;

destructor TLogView.Destroy;
begin
  TLogger.RemoveListener(Self);
  inherited;
end;

procedure TLogView.InternalLog(const AMsg: string);
begin
  Show;
  Memo.Lines.Add(FormatDateTime('mm-dd hh:nn:ss.zzz', Now) + ' ' + AMsg);
end;

procedure TLogView.Clear;
begin
  Memo.Lines.Clear;
end;

procedure TLogView.Log(const AMsg: string);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Queue(nil, procedure begin InternalLog(AMsg) end)
  else
    InternalLog(AMsg);
end;

procedure TLogView.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.
