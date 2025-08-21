unit DW.NFC;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  System.Classes;

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

  TNDEFStatus = (NotSupported, ReadOnly, ReadWrite, Unknown);

  TConnectTagCompletionProc = reference to procedure(const Error: string);
  TQueryStatusCompletionProc = reference to procedure(const Status: TNDEFStatus; const Capacity: LongWord; const Error: string);
  TReadCompletionProc = reference to procedure(const Msg: TNFCMessage; const Error: string);
  TWriteCompletionProc = reference to procedure(const Error: string);

  INDEFTag = interface(IInterface)
    ['{90FAB641-D280-40DE-A3F9-BC7D3516D0DD}']
    procedure Connect(const AHandler: TConnectTagCompletionProc);
    function IsAvailable: Boolean;
    procedure QueryStatus(const AHandler: TQueryStatusCompletionProc);
    procedure Read(const AHandler: TReadCompletionProc);
    procedure Write(const AMessage: TNFCMessage; const AHandler: TWriteCompletionProc);
  end;

  TNDEFTags = TArray<INDEFTag>;

  TNFCReader = class;

  TCustomPlatformNFCReader = class(TObject)
  private
    FIsActive: Boolean;
    FNFCReader: TNFCReader;
  protected
    procedure BeginSession; virtual; abstract;
    procedure DoDetectedTags(const ATags: TNDEFTags);
    procedure DoError(const AError: string);
    procedure DoResult(const ANFCResult: TNFCResult);
    procedure DoSessionBecameActive;
    procedure EndSession; virtual; abstract;
    property IsActive: Boolean read FIsActive write FIsActive;
    property NFCReader: TNFCReader read FNFCReader;
  public
    class function IsSupported: Boolean; virtual;
  public
    constructor Create(const ANFCReader: TNFCReader); virtual;
    destructor Destroy; override;
  end;

  TNFCErrorEvent = procedure(Sender: TObject; const Error: string) of object;
  TNFCResultEvent = procedure(Sender: TObject; const NFCResult: TNFCResult) of object;
  TNFCDetectedTagsEvent = procedure(Sender: TObject; const Tags: TNDEFTags) of object;

  TNFCReader = class(TObject)
  private
    FAlertMessage: string;
    FPlatformNFCReader: TCustomPlatformNFCReader;
    FOnBecameActive: TNotifyEvent;
    FOnDetectedTags: TNFCDetectedTagsEvent;
    FOnError: TNFCErrorEvent;
    FOnResult: TNFCResultEvent;
    function GetIsActive: Boolean;
  protected
    procedure DoError(const AError: string);
    procedure DoDetectedTags(const ATags: TNDEFTags);
    procedure DoResult(const ANFCResult: TNFCResult);
    procedure DoSessionBecameActive;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginSession;
    procedure EndSession;
    property AlertMessage: string read FAlertMessage write FAlertMessage;
    property IsActive: Boolean read GetIsActive;
    property OnBecameActive: TNotifyEvent read FOnBecameActive write FOnBecameActive;
    property OnDetectedTags: TNFCDetectedTagsEvent read FOnDetectedTags write FOnDetectedTags;
    property OnError: TNFCErrorEvent read FOnError write FOnError;
    property OnResult: TNFCResultEvent read FOnResult write FOnResult;
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

procedure TCustomPlatformNFCReader.DoSessionBecameActive;
begin
  FNFCReader.DoSessionBecameActive;
end;

procedure TCustomPlatformNFCReader.DoDetectedTags(const ATags: TNDEFTags);
begin
  FNFCReader.DoDetectedTags(ATags);
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

procedure TNFCReader.DoSessionBecameActive;
begin
  if Assigned(FOnBecameActive) then
    FOnBecameActive(Self);
end;

procedure TNFCReader.DoDetectedTags(const ATags: TNDEFTags);
begin
  if Assigned(FOnDetectedTags) then
    FOnDetectedTags(Self, ATags);
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
