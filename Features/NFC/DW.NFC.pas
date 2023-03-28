unit DW.NFC;

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

type
  TNFCPayloadTypeNameFormat = (Empty, NFCWellKnown, Media, AbsoluteURI, NFCExternal, Unknown, Unchanged);

  TNFCPayload = record
    Identifier: string;
    Payload: string;
    PayloadType: string;
    TypeNameFormat: TNFCPayloadTypeNameFormat;
  end;

  TNFCPayloads = TArray<TNFCPayload>;

  TNFCMessage = record
    Payloads: TNFCPayloads
  end;

  TNFCMessages = TArray<TNFCMessage>;

  TNFCTechnologyKind = (NFCA, NFCB, NFCF, NFCV, NFCNDef, NFCIsoDep);

  TNFCTechnology = record
    Kind: TNFCTechnologyKind;
    // Might expand this in the future
  end;

  TNFCTechnologies = TArray<TNFCTechnology>;

  TTagInfo = record
    ID: string;
    Technologies: TNFCTechnologies;
  end;

  TNFCResult = record
    TagInfo: TTagInfo;
    Messages: TNFCMessages;
  end;

  TNFCReader = class;

  TCustomPlatformNFCReader = class(TObject)
  private
    FIsActive: Boolean;
    FNFCReader: TNFCReader;
  protected
    procedure BeginSession; virtual; abstract;
    procedure DoResult(const ANFCResult: TNFCResult);
    procedure DoError(const AError: string);
    procedure EndSession; virtual; abstract;
    property IsActive: Boolean read FIsActive write FIsActive;
    property NFCReader: TNFCReader read FNFCReader;
  public
    class function IsSupported: Boolean; virtual;
  public
    constructor Create(const ANFCReader: TNFCReader); virtual;
    destructor Destroy; override;
  end;

  TNFCResultEvent = procedure(Sender: TObject; const NFCResult: TNFCResult) of object;
  TNFCErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TNFCReader = class(TObject)
  private
    FAlertMessage: string;
    FPlatformNFCReader: TCustomPlatformNFCReader;
    FOnResult: TNFCResultEvent;
    FOnError: TNFCErrorEvent;
    function GetIsActive: Boolean;
  protected
    procedure DoResult(const ANFCResult: TNFCResult);
    procedure DoError(const AError: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginSession;
    procedure EndSession;
    property AlertMessage: string read FAlertMessage write FAlertMessage;
    property IsActive: Boolean read GetIsActive;
    property OnResult: TNFCResultEvent read FOnResult write FOnResult;
    property OnError: TNFCErrorEvent read FOnError write FOnError;
  end;

implementation

uses
{$IF Defined(IOS)}
  DW.NFC.iOS,
{$ELSEIF Defined(ANDROID)}
  DW.NFC.Android,
{$ENDIF}
  DW.OSLog;

{$IF not Defined(IOS) and not Defined(ANDROID)}
type
  TPlatformNFCReader = class(TCustomPlatformNFCReader)
  protected
    procedure BeginSession; override;
    procedure EndSession; override;
  end;

{ TPlatformNFCReader }

procedure TPlatformNFCReader.BeginSession;
begin
  //
end;

procedure TPlatformNFCReader.EndSession;
begin
  //
end;
{$ENDIF}

{ TCustomPlatformNFCReader }

constructor TCustomPlatformNFCReader.Create(const ANFCReader: TNFCReader);
begin
  inherited Create;
  FNFCReader := ANFCReader;
end;

destructor TCustomPlatformNFCReader.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformNFCReader.DoResult(const ANFCResult: TNFCResult);
begin
  FNFCReader.DoResult(ANFCResult);
end;

procedure TCustomPlatformNFCReader.DoError(const AError: string);
begin
  FNFCReader.DoError(AError);
end;

class function TCustomPlatformNFCReader.IsSupported: Boolean;
begin
  Result := False;
end;

{ TNFCReader }

constructor TNFCReader.Create;
begin
  inherited;
  if TPlatformNFCReader.IsSupported then
    FPlatformNFCReader := TPlatformNFCReader.Create(Self)
  else
    TOSLog.d('NFC Not Supported');
end;

destructor TNFCReader.Destroy;
begin
  FPlatformNFCReader.Free;
  inherited;
end;

procedure TNFCReader.DoResult(const ANFCResult: TNFCResult);
begin
  if Assigned(FOnResult) then
    FOnResult(Self, ANFCResult);
end;

procedure TNFCReader.DoError(const AError: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, AError);
end;

procedure TNFCReader.BeginSession;
begin
  if FPlatformNFCReader <> nil then
    FPlatformNFCReader.BeginSession;
end;

procedure TNFCReader.EndSession;
begin
  if FPlatformNFCReader <> nil then
    FPlatformNFCReader.EndSession;
end;

function TNFCReader.GetIsActive: Boolean;
begin
  if FPlatformNFCReader <> nil then
    Result := FPlatformNFCReader.IsActive
  else
    Result := False;
end;

end.
