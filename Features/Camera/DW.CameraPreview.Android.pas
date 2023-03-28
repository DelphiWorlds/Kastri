unit DW.CameraPreview.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
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
    {$IF Defined(USEGL)}
    FView: JDWGLCameraView;
    {$ELSE}
    FView: JDWCameraView;
    {$ENDIF}
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    {$IF Defined(USEGL)}
    property View: JDWGLCameraView read FView;
    {$ELSE}
    property View: JDWCameraView read FView;
    {$ENDIF}
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
  DW.CameraPreview;

{ TAndroidCameraPreview }

constructor TAndroidCameraPreview.Create;
begin
  inherited;
  //
end;

function TAndroidCameraPreview.CreateView: JView;
begin
  {$IF Defined(USEGL)}
  FView := TJDWGLCameraView.JavaClass.init(TAndroidHelper.Activity);
  {$ELSE}
  FView := TJDWCameraView.JavaClass.init(TAndroidHelper.Activity);
  {$ENDIF}
  Result := FView;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TAndroidPresentationProxy<TAndroidCameraPreview>);

end.
