unit DW.NFC.iOS;

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


// ****** NOTE: This is a work in progress, so don't expect miracles :-) *****
// Tag ID reading might be added for iOS later - adding this URL as a possible reference
//   https://github.com/hansemannn/iOS-NFC-Example/issues/16

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes,
  // DW
  DW.iOSapi.CoreNFC, DW.NFC;

type
  TPlatformNFCReader = class;

  TNFCNDEFReaderSessionDelegate = class(TOCLocal, NFCNDEFReaderSessionDelegate)
  private
    FPlatformNFCReader: TPlatformNFCReader;
  public
    procedure readerSession(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray); overload; cdecl;
    procedure readerSession(session: NFCNDEFReaderSession; didInvalidateWithError: NSError); overload; cdecl;
    procedure readerSessionDidBecomeActive(session: NFCNDEFReaderSession); cdecl;
    [MethodName('readerSession:didDetectTags:')]
    procedure readerSessionDidDetectTags(session: NFCNDEFReaderSession; didDetectTags: NSArray); cdecl;
  public
    constructor Create(const APlatformNFCReader: TPlatformNFCReader);
  end;

  TPlatformNDEFTag = class(TInterfacedObject, INDEFTag)
  private
    FConnectTagCompletionHandler: TConnectTagCompletionProc;
    FQueryStatusCompletionHandler: TQueryStatusCompletionProc;
    FReadCompletionHandler: TReadCompletionProc;
    FReader: TPlatformNFCReader;
    FTag: NFCNDEFTag;
    FWriteCompletionHandler: TWriteCompletionProc;
    procedure ConnectToTagCompletion(error: NSError);
    procedure QueryStatusCompletion(status: NFCNDEFStatus; capacity: NSUInteger; error: NSError);
    procedure ReadCompletion(msg: NFCNDEFMessage; error: NSError);
    procedure WriteCompletion(error: NSError);
  public
    { INDEFTag }
    procedure Connect(const AHandler: TConnectTagCompletionProc);
    function IsAvailable: Boolean;
    procedure QueryStatus(const AHandler: TQueryStatusCompletionProc);
    procedure Read(const AHandler: TReadCompletionProc);
    procedure Write(const AMessage: TNFCMessage; const AHandler: TWriteCompletionProc);
  public
    constructor Create(const ATag: NFCNDEFTag; const AReader: TPlatformNFCReader);
  end;

  TPlatformNFCReader = class(TCustomPlatformNFCReader)
  private
    FDelegate: TNFCNDEFReaderSessionDelegate;
    FReaderSession: NFCNDEFReaderSession;
    FTags: TNDEFTags;
  protected
    procedure BeginSession; override;
    procedure EndSession; override;
    procedure readerSessionDidBecomeActive(session: NFCNDEFReaderSession);
    procedure readerSessionDidInvalidateWithError(session: NFCNDEFReaderSession; didInvalidateWithError: NSError);
    procedure readerSessionDidDetectNDEFs(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray);
    procedure readerSessionDidDetectTags(session: NFCNDEFReaderSession; didDetectTags: NSArray); cdecl;
    property Session: NFCNDEFReaderSession read FReaderSession;
  public
    class function IsSupported: Boolean; override;
  public
    constructor Create(const ANFCReader: TNFCReader); override;
  end;

implementation

uses
  // RTL
  System.Classes, System.SysUtils,
  // macOS
  Macapi.Helpers,
  // DW
  DW.iOSapi.Foundation;

type
  TNDEFStatusHelper = record helper for TNDEFStatus
    class function FromNative(const AStatus: NFCNDEFStatus): TNDEFStatus; static;
  end;

  TNFCPayloadHelper = record helper for TNFCPayload
    procedure FromNative(const APayload: NFCNDEFPayload);
    function ToNative: NFCNDEFPayload;
    function ToNativePointer: Pointer;
  end;

  TNFCMessageHelper = record helper for TNFCMessage
    procedure FromNative(const AMessage: NFCNDEFMessage);
    function ToNative: NFCNDEFMessage;
  end;

function NSDataToString(const AData: NSData): string;
begin
  Result := NSStrToStr(TNSString.Wrap(TNSString.Alloc.initWithData(AData, NSUTF8StringEncoding)));
end;

function ProtocolToObjectID(const AProtocol: IObjectiveC): Pointer;
var
  LLocal: ILocalObject;
begin
  Result := nil;
  if AProtocol <> nil then
  begin
    LLocal := AProtocol as ILocalObject;
    if LLocal <> nil then
      Result := LLocal.GetObjectID;
  end
end;

function GetErrorMessage(const AError: NSError): string;
begin
  if AError <> nil then
    Result := NSStrToStr(AError.localizedDescription)
  else
    Result := '';
end;

{ TNDEFStatusHelper }

class function TNDEFStatusHelper.FromNative(const AStatus: NFCNDEFStatus): TNDEFStatus;
begin
  Result := TNDEFStatus.Unknown;
  case AStatus of
    NFCNDEFStatusNotSupported:
      Result := TNDEFStatus.NotSupported;
    NFCNDEFStatusReadOnly:
      Result := TNDEFStatus.ReadOnly;
    NFCNDEFStatusReadWrite:
      Result := TNDEFStatus.ReadWrite;
  end;
end;

{ TNFCPayloadHelper }

procedure TNFCPayloadHelper.FromNative(const APayload: NFCNDEFPayload);
begin
  Identifier := NSDataToString(APayload.identifier);
  Payload := NSDataToString(APayload.payload);
  PayloadType := NSDataToString(APayload.&type);
  TypeNameFormat := TNFCPayloadTypeNameFormat(APayload.typeNameFormat);
end;

function TNFCPayloadHelper.ToNative: NFCNDEFPayload;
var
  LPointer: Pointer;
begin
  LPointer := ToNativePointer;
  if LPointer <> nil then
    Result := TNFCNDEFPayload.Wrap(LPointer)
  else
    Result := nil;
end;

function TNFCPayloadHelper.ToNativePointer: Pointer;
var
  LLocale: NSLocale;
begin
  Result := nil;
  if PayloadType.ToLower.Equals('uri') then
    Result := TNFCNDEFPayload.OCClass.wellKnownTypeURIPayloadWithURL(StrToNSUrl(Payload))
  else if PayloadType.ToLower.Equals('text') then
  begin
    LLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
    Result := TNFCNDEFPayload.OCClass.wellKnownTypeTextPayloadWithString(StrToNSStr(Payload), LLocale);
  end;
end;

{ TNFCMessageHelper }

procedure TNFCMessageHelper.FromNative(const AMessage: NFCNDEFMessage);
var
  I: Integer;
begin
  SetLength(Payloads, AMessage.records.count);
  for I := 0 to AMessage.records.count - 1 do
    Payloads[I].FromNative(TNFCNDEFPayload.Wrap(AMessage.records.objectAtIndex(I)));
end;

function TNFCMessageHelper.ToNative: NFCNDEFMessage;
var
  LRecords: NSMutableArray;
  LPayload: TNFCPayload;
begin
  LRecords := TNSMutableArray.Create;
  for LPayload in Payloads do
    LRecords.addObject(LPayload.ToNativePointer);
  Result := TNFCNDEFMessage.Wrap(TNFCNDEFMessage.Alloc.initWithNDEFRecords(LRecords));
end;

{ TPlatformNDEFTag }

constructor TPlatformNDEFTag.Create(const ATag: NFCNDEFTag; const AReader: TPlatformNFCReader);
begin
  inherited Create;
  FTag := ATag;
  FReader := AReader;
end;

procedure TPlatformNDEFTag.Connect(const AHandler: TConnectTagCompletionProc);
begin
  if not Assigned(FConnectTagCompletionHandler) then
  begin
    FConnectTagCompletionHandler := AHandler;
    FReader.Session.connectToTag(ProtocolToObjectID(FTag), ConnectToTagCompletion);
  end;
end;

procedure TPlatformNDEFTag.ConnectToTagCompletion(error: NSError);
begin
  try
    if Assigned(FConnectTagCompletionHandler) then
      FConnectTagCompletionHandler(GetErrorMessage(error));
  finally
    FConnectTagCompletionHandler := nil;
  end;
end;

function TPlatformNDEFTag.IsAvailable: Boolean;
begin
  Result := FTag.isAvailable;
end;

procedure TPlatformNDEFTag.QueryStatus(const AHandler: TQueryStatusCompletionProc);
begin
  if not Assigned(FQueryStatusCompletionHandler) then
  begin
    FQueryStatusCompletionHandler := AHandler;
    FTag.queryNDEFStatusWithCompletionHandler(QueryStatusCompletion);
  end;
end;

procedure TPlatformNDEFTag.QueryStatusCompletion(status: NFCNDEFStatus; capacity: NSUInteger; error: NSError);
begin
  try
    if Assigned(FQueryStatusCompletionHandler) then
      FQueryStatusCompletionHandler(TNDEFStatus.FromNative(status), capacity, GetErrorMessage(error));
  finally
    FQueryStatusCompletionHandler := nil;
  end;
end;

procedure TPlatformNDEFTag.Read(const AHandler: TReadCompletionProc);
begin
  if not Assigned(FReadCompletionHandler) then
  begin
    FReadCompletionHandler := AHandler;
    FTag.readNDEFWithCompletionHandler(ReadCompletion);
  end;
end;

procedure TPlatformNDEFTag.ReadCompletion(msg: NFCNDEFMessage; error: NSError);
var
  LMessage: TNFCMessage;
begin
  try
    if Assigned(FReadCompletionHandler) then
    begin
      LMessage.FromNative(msg);
      FReadCompletionHandler(LMessage, GetErrorMessage(error));
    end;
  finally
    FReadCompletionHandler := nil;
  end;
end;

procedure TPlatformNDEFTag.Write(const AMessage: TNFCMessage; const AHandler: TWriteCompletionProc);
begin
  if not Assigned(FWriteCompletionHandler) then
  begin
    FWriteCompletionHandler := AHandler;
    FTag.writeNDEF(AMessage.ToNative, WriteCompletion);
  end;
end;

procedure TPlatformNDEFTag.WriteCompletion(error: NSError);
begin
  try
    if Assigned(FWriteCompletionHandler) then
      FWriteCompletionHandler(GetErrorMessage(error));
  finally
    FWriteCompletionHandler := nil;
    FReader.EndSession;
  end;
end;

{ TNFCNDEFReaderSessionDelegate }

constructor TNFCNDEFReaderSessionDelegate.Create(const APlatformNFCReader: TPlatformNFCReader);
begin
  inherited Create;
  FPlatformNFCReader := APlatformNFCReader;
end;

procedure TNFCNDEFReaderSessionDelegate.readerSession(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray);
begin
  FPlatformNFCReader.readerSessionDidDetectNDEFs(session, didDetectNDEFs);
end;

procedure TNFCNDEFReaderSessionDelegate.readerSession(session: NFCNDEFReaderSession; didInvalidateWithError: NSError);
begin
  FPlatformNFCReader.readerSessionDidInvalidateWithError(session, didInvalidateWithError);
end;

procedure TNFCNDEFReaderSessionDelegate.readerSessionDidBecomeActive(session: NFCNDEFReaderSession);
begin
  FPlatformNFCReader.readerSessionDidBecomeActive(session);
end;

procedure TNFCNDEFReaderSessionDelegate.readerSessionDidDetectTags(session: NFCNDEFReaderSession; didDetectTags: NSArray);
begin
  FPlatformNFCReader.readerSessionDidDetectTags(session, didDetectTags);
end;

{ TPlatformNFCReader }

constructor TPlatformNFCReader.Create(const ANFCReader: TNFCReader);
begin
  inherited;
  FDelegate := TNFCNDEFReaderSessionDelegate.Create(Self);
end;

procedure TPlatformNFCReader.BeginSession;
begin
  FReaderSession := TNFCNDEFReaderSession.Wrap(TNFCNDEFReaderSession.Alloc.initWithDelegate(FDelegate.GetObjectID, 0, False));
  FReaderSession.setAlertMessage(StrToNSStr(NFCReader.AlertMessage));
  FReaderSession.beginSession;
  IsActive := True;
end;

procedure TPlatformNFCReader.EndSession;
begin
  FReaderSession.invalidateSession;
  FReaderSession := nil;
  IsActive := False;
end;

class function TPlatformNFCReader.IsSupported: Boolean;
begin
  Result := TOSVersion.Check(11) and TNFCNDEFReaderSession.OCClass.readingAvailable;
end;

procedure TPlatformNFCReader.readerSessionDidBecomeActive(session: NFCNDEFReaderSession);
begin
  DoSessionBecameActive;
end;

procedure TPlatformNFCReader.readerSessionDidDetectNDEFs(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray);
var
  LMessage: NFCNDEFMessage;
  LNFCMessages: TNFCMessages;
  I: Integer;
  LNFCResult: TNFCResult;
begin
  SetLength(LNFCMessages, didDetectNDEFs.count);
  for I := 0 to didDetectNDEFs.count - 1 do
  begin
    LMessage := TNFCNDEFMessage.Wrap(didDetectNDEFs.objectAtIndex(I));
    LNFCMessages[I].FromNative(LMessage);
  end;
  LNFCResult.Messages := LNFCMessages;
  TThread.Synchronize(nil,
    procedure
    begin
      DoResult(LNFCResult);
    end
  );
end;

procedure TPlatformNFCReader.readerSessionDidDetectTags(session: NFCNDEFReaderSession; didDetectTags: NSArray);
var
  I: Integer;
  LTag: NFCNDEFTag;
begin
  FTags := [];
  SetLength(FTags, didDetectTags.count);
  for I := 0 to High(FTags) do
  begin
    WrapInterface(didDetectTags.objectAtIndex(I), TypeInfo(NFCNDEFTag), LTag);
    FTags[I] := TPlatformNDEFTag.Create(LTag, Self);
  end;
  DoDetectedTags(FTags);
end;

procedure TPlatformNFCReader.readerSessionDidInvalidateWithError(session: NFCNDEFReaderSession; didInvalidateWithError: NSError);
begin
  IsActive := False;
  TThread.Synchronize(nil,
    procedure
    begin
      DoError(NSStrToStr(didInvalidateWithError.localizedDescription));
    end
  );
  EndSession;
end;

end.
