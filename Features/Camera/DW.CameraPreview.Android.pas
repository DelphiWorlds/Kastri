unit DW.CameraPreview.Android;

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

interface

uses
  // RTL
  System.Types,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Presentation.Android, FMX.Controls.Presentation, FMX.Presentation.Messages,
  // DW
  DW.Androidapi.JNI.DWCameraHelpers;

type
  TAndroidCameraPreview = class;

  TDWCameraViewStateDelegate = class(TJavaLocal, JDWCameraView_StateDelegate)
  private
    FPreview: TAndroidCameraPreview;
  public
    { JDWCameraView_StateDelegate }
    procedure onDestroyed(view: JDWCameraView); cdecl;
    procedure onReady(view: JDWCameraView); cdecl;
  public
    constructor Create(const APreview: TAndroidCameraPreview);
  end;

  TAndroidCameraPreview = class(TAndroidNativeView)
  private
    FStateDelegate: JDWCameraView_StateDelegate;
    FView: JDWCameraView;
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    property View: JDWCameraView read FView;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.Util, Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Presentation.Factory, FMX.Controls, FMX.Forms, FMX.Platform.Android, FMX.Platform.UI.Android,
  // DW
  DW.CameraPreview;

{ TDWCameraViewStateDelegate }

constructor TDWCameraViewStateDelegate.Create(const APreview: TAndroidCameraPreview);
begin
  inherited Create;
  FPreview := APreview;
end;

procedure TDWCameraViewStateDelegate.onDestroyed(view: JDWCameraView);
begin
  //
end;

procedure TDWCameraViewStateDelegate.onReady(view: JDWCameraView);
begin
  //
end;

{ TAndroidCameraPreview }

constructor TAndroidCameraPreview.Create;
begin
  inherited;
  FStateDelegate := TDWCameraViewStateDelegate.Create(Self);
end;

function TAndroidCameraPreview.CreateView: JView;
begin
  FView := TJDWCameraView.JavaClass.init(TAndroidHelper.Activity);
  FView.setStateDelegate(FStateDelegate);
  Result := FView;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

end.
