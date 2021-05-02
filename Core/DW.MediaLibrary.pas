unit DW.MediaLibrary;

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
  // RTL
  System.Classes,
  // FMX
  FMX.Graphics;

type
  TMediaLibrary = class;

  TCustomPlatformMediaLibrary = class(TObject)
  private
    FMediaLibrary: TMediaLibrary;
  protected
    procedure DoCanceled;
    procedure DoReceivedImage(const AImagePath: string; const ABitmap: TBitmap);
    procedure TakePhoto; virtual;
  public
    constructor Create(const AMediaLibrary: TMediaLibrary); virtual;
  end;

  TReceivedImageEvent = procedure(Sender: TObject; const ImagePath: string; const Image: TBitmap) of object;

  TMediaLibrary = class(TObject)
  private
    FPlatformMediaLibrary: TCustomPlatformMediaLibrary;
    FOnCanceled: TNotifyEvent;
    FOnReceivedImage: TReceivedImageEvent;
  protected
    procedure DoCanceled;
    procedure DoReceivedImage(const AImagePath: string; const ABitmap: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure TakePhoto;
    property OnCanceled: TNotifyEvent read FOnCanceled write FOnCanceled;
    property OnReceivedImage: TReceivedImageEvent read FOnReceivedImage write FOnReceivedImage;
  end;

implementation

{$IF Defined(ANDROID)}
uses
  // DW
  DW.MediaLibrary.Android;
  // TODO: Support other platforms
{$ENDIF}

{$IF not Defined(ANDROID)}
type
  TPlatformMediaLibrary = class(TCustomPlatformMediaLibrary);
{$ENDIF}

{ TCustomPlatformMediaLibrary }

constructor TCustomPlatformMediaLibrary.Create(const AMediaLibrary: TMediaLibrary);
begin
  inherited Create;
  FMediaLibrary := AMediaLibrary;
end;

procedure TCustomPlatformMediaLibrary.DoCanceled;
begin
  FMediaLibrary.DoCanceled;
end;

procedure TCustomPlatformMediaLibrary.DoReceivedImage(const AImagePath: string; const ABitmap: TBitmap);
begin
  FMediaLibrary.DoReceivedImage(AImagePath, ABitmap);
end;

procedure TCustomPlatformMediaLibrary.TakePhoto;
begin
  //
end;

{ TMediaLibrary }

constructor TMediaLibrary.Create;
begin
  inherited;
  FPlatformMediaLibrary := TPlatformMediaLibrary.Create(Self);
end;

destructor TMediaLibrary.Destroy;
begin
  FPlatformMediaLibrary.Free;
  inherited;
end;

procedure TMediaLibrary.DoCanceled;
begin
  if Assigned(FOnCanceled) then
    FOnCanceled(Self);
end;

procedure TMediaLibrary.DoReceivedImage(const AImagePath: string; const ABitmap: TBitmap);
begin
  if Assigned(FOnReceivedImage) then
    FOnReceivedImage(Self, AImagePath, ABitmap);
end;

procedure TMediaLibrary.TakePhoto;
begin
  FPlatformMediaLibrary.TakePhoto;
end;

end.
