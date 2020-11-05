unit DW.NativeImage;

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
  System.Classes, System.Types,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model;

const
  MM_NATIVEIMAGE_LOADFROMFILE = MM_USER + 1;
  MM_NATIVEIMAGE_LOADFROMRESOURCE = MM_USER + 2;
  MM_NATIVEIMAGE_LOADFROMSTREAM = MM_USER + 3;
  MM_NATIVEIMAGE_LOADFROMNATIVE = MM_USER + 4;

type
  TCustomNativeImageModel = class(TDataModel)
  protected
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(const AResourceName: string);
    procedure LoadFromStream(const AStream: TStream);
  end;

  TCustomNativeImage = class(TPresentedControl)
  private
    function GetModel: TCustomNativeImageModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(const AResourceName: string);
    procedure LoadFromStream(const AStream: TStream);
    property Model: TCustomNativeImageModel read GetModel;
  end;

  [ComponentPlatformsAttribute(pfidiOS or pidAndroid or pidWin32 or pidWin64)]
  TNativeImage = class(TCustomNativeImage)
  published
    property Align;
    property Anchors;
    property Height;
    property Margins;
    property Position;
    property Visible default True;
    property Width;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // DW
  {$IF Defined(MSWINDOWS)}
  DW.NativeImage.Win;
  {$ENDIF}
  {$IF Defined(IOS)}
  DW.NativeImage.iOS;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.NativeImage.Android;
  {$ENDIF}

{ TCustomNativeImageModel }

procedure TCustomNativeImageModel.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    SendMessage<string>(MM_NATIVEIMAGE_LOADFROMFILE, AFileName);
end;

procedure TCustomNativeImageModel.LoadFromResource(const AResourceName: string);
var
  LStream: TStream;
begin
  if FindResource(HInstance, PChar(AResourceName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

procedure TCustomNativeImageModel.LoadFromStream(const AStream: TStream);
begin
  SendMessage<TStream>(MM_NATIVEIMAGE_LOADFROMSTREAM, AStream);
end;

{ TCustomNativeImage }

constructor TCustomNativeImage.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomNativeImage.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeImageModel;
end;

function TCustomNativeImage.GetModel: TCustomNativeImageModel;
begin
  Result := inherited GetModel<TCustomNativeImageModel>;
end;

procedure TCustomNativeImage.LoadFromFile(const AFileName: string);
begin
  Model.LoadFromFile(AFileName);
end;

procedure TCustomNativeImage.LoadFromResource(const AResourceName: string);
begin
  Model.LoadFromResource(AResourceName);
end;

procedure TCustomNativeImage.LoadFromStream(const AStream: TStream);
begin
  Model.LoadFromStream(AStream);
end;

function TCustomNativeImage.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

end.
