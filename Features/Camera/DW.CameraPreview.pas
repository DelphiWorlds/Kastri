unit DW.CameraPreview;

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
  System.Classes, System.Types, System.Messaging,
  // FMX
  FMX.Controls.Presentation;

type
  TCameraPreview = class(TPresentedControl)
  private
    FOnOrientationChange: TNotifyEvent;
    procedure OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnOrientationChange: TNotifyEvent read FOnOrientationChange write FOnOrientationChange;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.UITypes,
  // FMX
  FMX.Types, FMX.Forms,
  // DW
  {$IF Defined(ANDROID)}
  DW.CameraPreview.Android;
  {$ENDIF}
  {$IF Defined(IOS)}
  DW.CameraPreview.iOS;
  {$ENDIF}

{ TCameraPreview }

constructor TCameraPreview.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedMessageHandler);
  Name := 'CameraPreview';
  ControlType := TControlType.Platform;
end;

destructor TCameraPreview.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, OrientationChangedMessageHandler);
  inherited;
end;

procedure TCameraPreview.OrientationChangedMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  if Assigned(FOnOrientationChange) then
    FOnOrientationChange(Self);
end;

function TCameraPreview.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

end.
