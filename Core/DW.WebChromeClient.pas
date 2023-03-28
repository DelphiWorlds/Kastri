unit DW.WebChromeClient;

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
  System.Classes;

type
  TWebChromeClientManager = class;

  TCustomPlatformWebChromeClientManager = class(TComponent)
  private
    FWebChromeClientManager: TWebChromeClientManager;
  protected
    procedure DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
    procedure FlushCookies; virtual;
    property WebChromeClientManager: TWebChromeClientManager read FWebChromeClientManager write FWebChromeClientManager;
  end;

  TShouldOverrideUrlEvent = procedure(Sender: TObject; const URL: string; var ShouldOverride: Boolean) of object;

  TWebChromeClientManager = class(TComponent)
  private
    FPlatformWebChromeClientManager: TCustomPlatformWebChromeClientManager;
    FOnShouldOverrideUrl: TShouldOverrideUrlEvent;
  protected
    procedure DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlushCookies;
    property OnShouldOverrideUrl: TShouldOverrideUrlEvent read FOnShouldOverrideUrl write FOnShouldOverrideUrl;
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

procedure TCustomPlatformWebChromeClientManager.DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
begin
  FWebChromeClientManager.DoShouldOverrideUrl(AURL, AShouldOverride);
end;

procedure TCustomPlatformWebChromeClientManager.FlushCookies;
begin
  //
end;

{ TWebChromeClientManager }

constructor TWebChromeClientManager.Create(AOwner: TComponent);
begin
  inherited;
  FPlatformWebChromeClientManager := TPlatformWebChromeClientManager.Create(AOwner);
  FPlatformWebChromeClientManager.WebChromeClientManager := Self;
end;

destructor TWebChromeClientManager.Destroy;
begin
  //
  inherited;
end;

procedure TWebChromeClientManager.DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
begin
  if Assigned(FOnShouldOverrideUrl) then
    FOnShouldOverrideUrl(Self, AURL, AShouldOverride);
end;

procedure TWebChromeClientManager.FlushCookies;
begin
  FPlatformWebChromeClientManager.FlushCookies;
end;

end.
