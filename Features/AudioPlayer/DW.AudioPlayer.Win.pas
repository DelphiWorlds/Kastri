unit DW.AudioPlayer.Win;

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
  // Win
  Winapi.Windows, Winapi.Messages, Winapi.DirectShow9,
  // DW
  DW.AudioPlayer;

const
  WM_GRAPH_EVENT = WM_APP + 1;

type
  TPlatformAudioPlayer = class;

  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer)
  private
    FDelay: Integer;
    FGraphBuilder: IGraphBuilder;
    FFileName: string;
    FHandle: THandle;
    FMediaControl: IMediaControl;
    FMediaEventEx: IMediaEventEx;
    FMediaSeeking: IMediaSeeking;
    FPlayStartTime: TDateTime;
    procedure DoSetup;
    function RenderFile: Boolean;
    procedure Setup;
    function SetupGraph: Boolean;
    procedure Stopped;
    procedure TearDownGraph;
    procedure WndProc(var Message: TMessage);
    procedure WMGraphEvent(var Msg: TMessage); message WM_GRAPH_EVENT;
  protected
    function GetDelay: Integer; override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure Pause; override;
    procedure Play; override;
    procedure Stop; override;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Classes, System.SysUtils, System.DateUtils, System.IOUtils,
  // Win
  Winapi.ActiveX;

{ TPlatformAudioPlayer }

constructor TPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
end;

destructor TPlatformAudioPlayer.Destroy;
begin
  DeallocateHWnd(FHandle);
  TearDownGraph;
  inherited;
end;

function TPlatformAudioPlayer.GetDelay: Integer;
begin
  Result := FDelay;
end;

function TPlatformAudioPlayer.RenderFile: Boolean;
begin
  Result := False;
  if TFile.Exists(FFileName) and Succeeded(FGraphBuilder.RenderFile(PChar(FFileName), nil)) then
  begin
    SetIsReady(True);
    Result := True;
  end;
end;

procedure TPlatformAudioPlayer.DoSetup;
begin
  FDelay := 0;
  // Ensure Setup is called on the main thread
  if TThread.Current.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, Setup)
  else
    Setup;
end;

procedure TPlatformAudioPlayer.Setup;
begin
  if SetupGraph then
    RenderFile;
end;

function TPlatformAudioPlayer.SetupGraph: Boolean;
begin
  TearDownGraph;
  Result := Succeeded(CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraphBuilder)) and
    Succeeded(FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl)) and
    Succeeded(FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx)) and
    Succeeded(FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking)) and
    Succeeded(FMediaEventEx.SetNotifyWindow(FHandle, WM_GRAPH_EVENT, 0));
end;

procedure TPlatformAudioPlayer.Stop;
begin
  if FMediaControl <> nil then
  begin
    if Succeeded(FMediaControl.StopWhenReady) then
      Stopped;
  end;
end;

procedure TPlatformAudioPlayer.Stopped;
begin
  TearDownGraph;
  DoAudioStateChange(TAudioState.Stopped);
end;

procedure TPlatformAudioPlayer.TearDownGraph;
begin
  if FMediaEventEx <> nil then
    FMediaEventEx.SetNotifyWindow(0, 0, 0);
  FGraphBuilder := nil;
  FMediaControl := nil;
  FMediaEventEx := nil;
end;

procedure TPlatformAudioPlayer.LoadFromFile(const AFileName: string);
begin
  FFileName := AFileName;
  DoSetup;
end;

procedure TPlatformAudioPlayer.Pause;
begin
  if (FMediaControl <> nil) and (AudioState = TAudioState.Playing) then
    FMediaControl.Pause;
end;

procedure TPlatformAudioPlayer.Play;
begin
  if FMediaControl = nil then
    DoSetup;
  if FMediaControl <> nil then
  begin
    FPlayStartTime := Now;
    FMediaControl.Run;
    if AudioState = TAudioState.Paused  then
      DoAudioStateChange(TAudioState.Playing);
  end;
end;

procedure TPlatformAudioPlayer.WMGraphEvent(var Msg: TMessage);
var
  LCode: Integer;
  LParam1, LParam2: LONG_PTR;
begin
  if FMediaEventEx <> nil then
  begin
    while Succeeded(FMediaEventEx.GetEvent(LCode, LParam1, LParam2, 0)) do
    try
      case LCode of
        EC_COMPLETE, EC_USERABORT, EC_ERRORABORT:
          Stopped;
        EC_PAUSED:
        begin
          if AudioState <> TAudioState.Playing then
          begin
            if FDelay = 0 then
              FDelay := MilliSecondsBetween(Now, FPlayStartTime);
            DoAudioStateChange(TAudioState.Playing);
          end
          else
            DoAudioStateChange(TAudioState.Paused);
        end;
      end;
    finally
      FMediaEventEx.FreeEventParams(LCode, LParam1, LParam2);
    end;
  end;
end;

procedure TPlatformAudioPlayer.WndProc(var Message: TMessage);
begin
  Dispatch(Message);
end;

end.
