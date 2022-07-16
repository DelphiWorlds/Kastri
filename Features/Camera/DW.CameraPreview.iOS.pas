unit DW.CameraPreview.iOS;

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
  // RTL
  System.Types,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.QuartzCore, iOSapi.AVFoundation,
  // FMX
  FMX.Presentation.iOS, FMX.Presentation.Messages, FMX.Controls.Presentation;

type
  AVCaptureVideoPreviewLayer = interface(iOSapi.AVFoundation.AVCaptureVideoPreviewLayer)
    ['{9DFFD2AB-ED0D-4A68-8417-76E33AAAD1C3}']
    function connection: AVCaptureConnection; cdecl;
  end;
  TAVCaptureVideoPreviewLayer = class(TOCGenericImport<AVCaptureVideoPreviewLayerClass, AVCaptureVideoPreviewLayer>)
  end;

  TiOSCameraPreview = class(TiOSNativeView)
  private
    FPreviewLayer: AVCaptureVideoPreviewLayer;
  public
    destructor Destroy; override;
    procedure StartPreview(const ASession: AVCaptureSession);
    procedure StopPreview;
    procedure UpdatePreview;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // iOS
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.Helpers,
  // FMX
  FMX.Presentation.Factory, FMX.Controls, FMX.Platform.iOS,
  // DW
  DW.CameraPreview;

function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill');
end;

{ TiOSCameraPreview }

destructor TiOSCameraPreview.Destroy;
begin
  if FPreviewLayer <> nil then
  begin
    FPreviewLayer.removeFromSuperlayer;
    FPreviewLayer := nil;
  end;
  inherited;
end;

procedure TiOSCameraPreview.StartPreview(const ASession: AVCaptureSession);
begin
  if FPreviewLayer = nil then
  begin
    FPreviewLayer := TAVCaptureVideoPreviewLayer.Wrap(TAVCaptureVideoPreviewLayer.OCClass.layerWithSession(ASession));
    FPreviewLayer.setBackgroundColor(TUIColor.Wrap(TUIColor.OCClass.blackColor).CGColor);
    FPreviewLayer.setVideoGravity(AVLayerVideoGravityResizeAspectFill);
    View.layer.addSublayer(FPreviewLayer);
  end
  else
    FPreviewLayer.setSession(ASession);
  FPreviewLayer.setHidden(False);
  UpdatePreview;
end;

procedure TiOSCameraPreview.StopPreview;
begin
  if FPreviewLayer <> nil then
    FPreviewLayer.setHidden(True);
end;

procedure TiOSCameraPreview.UpdatePreview;
var
  LPt: TPointF;
  LRect: CGRect;
begin
  if FPreviewLayer <> nil then
  begin
    LRect := View.layer.bounds;
    if Control <> nil then
    begin
      LPt := Control.LocalToAbsolute(TPointF.Zero);
      LRect.origin.y := LPt.Y;
      LRect.origin.x := LPt.X;
      LRect.size.height := Control.Height;
      LRect.size.width := Control.Width;
    end;
    FPreviewLayer.setFrame(LRect);
    if FPreviewLayer.connection <> nil then
      FPreviewLayer.connection.setVideoOrientation(TiOSHelper.SharedApplication.statusBarOrientation);
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TCameraPreview, TControlType.Platform, TiOSPresentationProxy<TiOSCameraPreview>);

finalization
  TPresentationProxyFactory.Current.Unregister(TCameraPreview, TControlType.Platform, TiOSPresentationProxy<TiOSCameraPreview>);

end.
