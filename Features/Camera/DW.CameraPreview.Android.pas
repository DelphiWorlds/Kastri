unit DW.CameraPreview.Android;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Presentation.Android, FMX.Controls.Presentation, FMX.Presentation.Messages,
  // DW
  DW.Androidapi.JNI.DWCameraHelpers;

type
  TAndroidCameraPreview = class(TAndroidNativeView)
  private
    FView: JDWCameraView;
    // FView: JDWGLCameraView;
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    property View: JDWCameraView read FView;
    // property View: JDWGLCameraView read FView;
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Presentation.Factory, FMX.Controls,
  // RTL
  System.SysUtils,
  // DW
  DW.CameraPreview;

{ TAndroidCameraPreview }

constructor TAndroidCameraPreview.Create;
begin
  inherited;
  //
end;

function TAndroidCameraPreview.CreateView: JView;
begin
  FView := TJDWCameraView.JavaClass.init(TAndroidHelper.Activity);
  // FView := TJDWGLCameraView.JavaClass.init(TAndroidHelper.Activity);
  Result := FView;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

end.
