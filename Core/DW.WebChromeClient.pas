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

  TFileChooserKind = (Default, VisualMedia, Camera);

  TCustomPlatformWebChromeClientManager = class(TComponent)
  private
    FFileCacheFath: string;
    FWebChromeClientManager: TWebChromeClientManager;
  protected
    procedure DoFileChooser(const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
    procedure DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
    procedure FlushCookies; virtual;
    property FileCachePath: string read FFileCacheFath write FFileCacheFath;
    property WebChromeClientManager: TWebChromeClientManager read FWebChromeClientManager write FWebChromeClientManager;
  end;

  TShouldOverrideUrlEvent = procedure(Sender: TObject; const URL: string; var ShouldOverride: Boolean) of object;
  TFileChooserEvent = procedure(Sender: TObject; const MimeTypes: TArray<string>; var FileChooserKind: TFileChooserKind) of object;

  TWebChromeClientManager = class(TComponent)
  private
    FPlatformWebChromeClientManager: TCustomPlatformWebChromeClientManager;
    FOnFileChooser: TFileChooserEvent;
    FOnShouldOverrideUrl: TShouldOverrideUrlEvent;
    function GetFileCacheFath: string;
    procedure SetFileCacheFath(const Value: string);
  protected
    procedure DoFileChooser(const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
    procedure DoShouldOverrideUrl(const AURL: string; var AShouldOverride: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlushCookies;
    property FileCachePath: string read GetFileCacheFath write SetFileCacheFath;
    property OnFileChooser: TFileChooserEvent read FOnFileChooser write FOnFileChooser;
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

procedure TCustomPlatformWebChromeClientManager.DoFileChooser(const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
begin
  FWebChromeClientManager.DoFileChooser(AMimeTypes, AFileChooserKind);
end;

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

procedure TWebChromeClientManager.DoFileChooser(const AMimeTypes: TArray<string>; var AFileChooserKind: TFileChooserKind);
begin
  if Assigned(FOnFileChooser) then
    FOnFileChooser(Self, AMimeTypes, AFileChooserKind);
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

function TWebChromeClientManager.GetFileCacheFath: string;
begin
  Result := FPlatformWebChromeClientManager.FileCachePath;
end;

procedure TWebChromeClientManager.SetFileCacheFath(const Value: string);
begin
  FPlatformWebChromeClientManager.FileCachePath := Value;
end;

end.
