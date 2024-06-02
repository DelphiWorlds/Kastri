unit DW.VideoPlayer.View.Android;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Presentation.Android, FMX.Controls.Presentation, FMX.Presentation.Messages,
  // DW
  DW.Androidapi.JNI.AndroidX.Media3.ExoPlayer, DW.Androidapi.JNI.AndroidX.Media3.UI;

type
  TPlayerViewTouchEvent = procedure(view: JView; event: JMotionEvent) of object;

  TAndroidExoPlayerView = class(TAndroidNativeView)
  private
    FView: JPlayerView;
    FOnTouch: TPlayerViewTouchEvent;
    procedure ViewVisibleChangedHandler(Sender: TObject);
  protected
    function CreateView: JView; override;
    function ProcessTouch(view: JView; event: JMotionEvent): Boolean; override;
  public
    constructor Create; override;
    property OnTouch: TPlayerViewTouchEvent read FOnTouch write FOnTouch;
    property View: JPlayerView read FView;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Presentation.Factory, FMX.Controls,
  // DW
  DW.VideoPlayer.View;

{ TAndroidExoPlayerView }

constructor TAndroidExoPlayerView.Create;
begin
  inherited;
  TVideoPlayerView(Control).OnVisibleChanged := ViewVisibleChangedHandler;
end;

function TAndroidExoPlayerView.CreateView: JView;
begin
  FView := TJPlayerView.JavaClass.init(TAndroidHelper.Context);
  Result := FView;
end;

function TAndroidExoPlayerView.ProcessTouch(view: JView; event: JMotionEvent): Boolean;
begin
  if Assigned(FOnTouch) then
    FOnTouch(view, event);
  Result := inherited;
end;

procedure TAndroidExoPlayerView.ViewVisibleChangedHandler(Sender: TObject);
begin
  if Control.Visible then
    FView.setVisibility(TJView.JavaClass.VISIBLE)
  else
    FView.setVisibility(TJView.JavaClass.INVISIBLE);
end;

initialization
  TPresentationProxyFactory.Current.Register(TVideoPlayerView, TControlType.Platform, TAndroidPresentationProxy<TAndroidExoPlayerView>);

finalization
  TPresentationProxyFactory.Current.Unregister(TVideoPlayerView, TControlType.Platform, TAndroidPresentationProxy<TAndroidExoPlayerView>);

end.
