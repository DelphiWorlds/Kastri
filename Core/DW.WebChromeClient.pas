unit DW.WebChromeClient;

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
  System.Classes;

type
  TCustomPlatformWebChromeClientManager = class(TComponent)
  protected
    procedure FlushCookies; virtual;
  end;

  TWebChromeClientManager = class(TComponent)
  private
    FPlatformWebChromeClientManager: TCustomPlatformWebChromeClientManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlushCookies;
  end;

implementation

{$IF Defined(ANDROID)}
uses
  DW.WebChromeClient.Android;
{$ELSE}

type
  TPlatformWebChromeClientManager = class(TCustomPlatformWebChromeClientManager);

{$ENDIF}

{ TCustomPlatformWebChromeClientManager }

procedure TCustomPlatformWebChromeClientManager.FlushCookies;
begin
  //
end;

{ TWebChromeClientManager }

constructor TWebChromeClientManager.Create(AOwner: TComponent);
begin
  inherited;
  FPlatformWebChromeClientManager := TPlatformWebChromeClientManager.Create(AOwner);
end;

destructor TWebChromeClientManager.Destroy;
begin
  //
  inherited;
end;

procedure TWebChromeClientManager.FlushCookies;
begin
  FPlatformWebChromeClientManager.FlushCookies;
end;

end.
