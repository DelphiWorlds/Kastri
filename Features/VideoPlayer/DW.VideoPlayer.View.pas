unit DW.VideoPlayer.View;

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

// {$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Types, System.Messaging,
  // FMX
  FMX.Controls.Presentation;

type
  TVideoPlayerView = class(TPresentedControl)
  private
    FOnOrientationChange: TNotifyEvent;
    FOnSizeChange: TNotifyEvent;
    FOnVisibleChanged: TNotifyEvent;
    procedure OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SizeChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnOrientationChange: TNotifyEvent read FOnOrientationChange write FOnOrientationChange;
    property OnSizeChange: TNotifyEvent read FOnSizeChange write FOnSizeChange;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
  end;

implementation

uses
  // DW
  {$IF Defined(ANDROID)}
  DW.VideoPlayer.View.Android,
  {$ENDIF}
  {$IF Defined(IOS)}
  // DW.VideoPlayer.View.iOS,
  {$ENDIF}
  // RTL
  System.SysUtils, System.UITypes,
  // FMX
  FMX.Types, FMX.Forms;

{ TVideoPlayerView }

constructor TVideoPlayerView.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, SizeChangedMessageHandler);
  Name := 'VideoPlayerView';
  ControlType := TControlType.Platform;
end;

destructor TVideoPlayerView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, SizeChangedMessageHandler);
  inherited;
end;

procedure TVideoPlayerView.OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if Assigned(FOnOrientationChange) then
    FOnOrientationChange(Self);
end;

procedure TVideoPlayerView.SizeChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if (Root <> nil) and (Root.GetObject = Sender) and Assigned(FOnSizeChange) then
    FOnSizeChange(Self);
end;

procedure TVideoPlayerView.VisibleChanged;
begin
  inherited;
  if Assigned(FOnVisibleChanged) then
    FOnVisibleChanged(Self);
end;

function TVideoPlayerView.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

end.
