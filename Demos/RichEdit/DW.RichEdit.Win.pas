unit DW.RichEdit.Win;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Classes, System.SysUtils,
  // Windows
  Winapi.Messages, Winapi.Windows,
  // FMX
  FMX.Presentation.Win, FMX.Controls.Win, FMX.Controls.Presentation, FMX.Controls.Model, FMX.Memo.Win,
  // DW
  DW.RichEdit;

type
  TConversionBuffer = TBytes;

  TConversion = class(TObject)
  public
    constructor Create; virtual;
    function ConvertReadStream(Stream: TStream; Buffer: TConversionBuffer; BufSize: Integer): Integer; virtual;
    function ConvertWriteStream(Stream: TStream; Buffer: TConversionBuffer; BufSize: Integer): Integer; virtual;
  end;

  TWinNativeRichEdit = class(TWinNativeMemo)
  private
    FConverter: TConversion;
    FModel: TCustomRichEditModel;
    procedure ColorChanged;
    function GetRichEdit: TCustomRichEdit;
    procedure LoadFromStream(const AStream: TStream; AEncoding: TEncoding);
    procedure LoadFromStreamHandler(Sender: TObject; const AStream: TStream; const AEncoding: TEncoding);
    function PlainText: Boolean;
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function DefineModelClass: TDataModelClass; override;
    procedure PMRichEditColorChanged(var Message: TDispatchMessage); message PM_RICHEDIT_COLOR_CHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultHandler(var Message); override;
    property RichEdit: TCustomRichEdit read GetRichEdit;
    property Model: TCustomRichEditModel read FModel;
  end;

implementation

uses
  // RTL
  System.UITypes,
  // Windows
  Winapi.RichEdit,
  // FMX
  FMX.Controls, FMX.Types, FMX.ScrollBox, FMX.Presentation.Factory;

resourcestring
  sRichEditLoadFail = 'Failed to Load Stream';
  sRichEditSaveFail = 'Failed to Save Stream';

const
  RichEditModuleName = 'RICHED20.DLL';
  ReadError = $0001;
  WriteError = $0002;
  NoError = $0000;
  cRichEditMessages: array[0..3] of Integer = (
    EM_SETBKGNDCOLOR, EM_STREAMIN, EM_STREAMOUT, EM_EXLIMITTEXT
  );

var
  FRichEditModule: HMODULE;

type
  PRichEditStreamInfo = ^TRichEditStreamInfo;
  TRichEditStreamInfo = record
    Converter: TConversion;
    Stream: TStream;
    PlainText: Boolean;
    Encoding: TEncoding;
  end;

function AdjustLineBreaks(Dest: PByte; Source: TBytes; Start, Len: Integer): Integer;
var
  P: PByte;
  I: Integer;
begin
  I := Start;
  P := Dest;
  while I < (Len - 1) do
  begin
    if (Source[I] = 10) and (Source[I + 1] = 0) then
    begin
      P^ := 13;
      Inc(P);
      P^ := 0;
      Inc(P);
      P^ := 10;
      Inc(P);
      P^ := 0;
      Inc(P);
    end
    else
    begin
      P^ := Source[I];
      Inc(P);
      P^ := Source[I + 1];
      Inc(P);
      if (Source[I] = 13) and (Source[I + 1] = 0) then
      begin
        P^ := 10;
        Inc(P);
        P^ := 0;
        Inc(P);
        if (Source[I + 2] = 10) and (Source[I + 3] = 0) then
          Inc(I, 2);
      end;
    end;
    Inc(I, 2);
  end;
  if I = Len - 1 then
  begin
    P^ := Source[I];
    Inc(P);
  end;
  Result := P - Dest;
end;

function StreamSave(dwCookie: DWORD_PTR; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall;
var
  StreamInfo: TRichEditStreamInfo;
  Buffer: TBytes;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(dwCookie)^;
  try
    pcb := 0;
    if StreamInfo.Converter <> nil then
    begin
      Move(pbBuff^, Buffer[0], cb);
      if StreamInfo.PlainText then
      begin
        if StreamInfo.Encoding = nil then
          Buffer := TEncoding.Convert(TEncoding.Unicode, TEncoding.Default, Buffer)
        else
        begin
          if not TEncoding.Unicode.Equals(StreamInfo.Encoding) then
            Buffer := TEncoding.Convert(TEncoding.Unicode, StreamInfo.Encoding, Buffer);
        end;
      end;
      pcb := StreamInfo.Converter.ConvertWriteStream(StreamInfo.Stream, Buffer, Length(Buffer));
      if (pcb <> cb) and (pcb = Length(Buffer)) then
        pcb := cb;
    end;
  except
    Result := WriteError;
  end;
end;

function StreamLoad(dwCookie: DWORD_PTR; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall;
var
  Buffer, Preamble: TBytes;
  StreamInfo: TRichEditStreamInfo;
  StartIndex: Integer;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(dwCookie)^;
  SetLength(Buffer, cb + 1);
  cb := cb div 2;
  if (cb mod 2) > 0 then
    cb := cb -1 ;
  StartIndex := 0;
  pcb := 0;
  try
    if StreamInfo.Converter <> nil then
      pcb := StreamInfo.Converter.ConvertReadStream(StreamInfo.Stream, Buffer, cb);
    if pcb > 0 then
    begin
      Buffer[pcb] := 0;
      if Buffer[pcb - 1] = 13 then
      begin
        Buffer[pcb - 1] := 0;
        Dec(pcb);
      end;
      if StreamInfo.PlainText then
      begin
        if StreamInfo.Encoding = nil then
        begin
          Buffer := TEncoding.Convert(TEncoding.Default, TEncoding.Unicode, Buffer, 0, pcb);
          pcb := Length(Buffer);
        end
        else
        begin
          if not TEncoding.Unicode.Equals(StreamInfo.Encoding) then
          begin
            Buffer := TEncoding.Convert(StreamInfo.Encoding, TEncoding.Unicode, Buffer, 0, pcb);
            pcb := Length(Buffer);
          end;
          Preamble := TEncoding.Unicode.GetPreamble;
          if (pcb >= 2) and (Buffer[0] = Preamble[0]) and (Buffer[1] = Preamble[1]) then
            StartIndex := 2;
        end;
      end;
      pcb := AdjustLineBreaks(pbBuff, Buffer, StartIndex, pcb);
    end;
  except
    Result := ReadError;
  end;
end;

function ContainsPreamble(Stream: TStream; Signature: TBytes): Boolean;
var
  Buffer: TBytes;
  I, LBufLen, LSignatureLen, LPosition: Integer;
begin
  Result := True;
  LSignatureLen := Length(Signature);
  LPosition := Stream.Position;
  try
    SetLength(Buffer, LSignatureLen);
    LBufLen := Stream.Read(Buffer[0], LSignatureLen);
  finally
    Stream.Position := LPosition;
  end;
  if LBufLen = LSignatureLen then
  begin
    for I := 1 to LSignatureLen do
      if Buffer[I - 1] <> Signature [I - 1] then
      begin
        Result := False;
        Break;
      end;
  end
  else
    Result := False;
end;

{ TConversion }

function TConversion.ConvertReadStream(Stream: TStream; Buffer: TConversionBuffer; BufSize: Integer): Integer;
begin
  Result := Stream.Read(Buffer[0], BufSize);
end;

function TConversion.ConvertWriteStream(Stream: TStream; Buffer: TConversionBuffer; BufSize: Integer): Integer;
begin
  Result := Stream.Write(Buffer[0], BufSize);
end;

constructor TConversion.Create;
begin
  inherited;
end;

function IntInArray(const AValue: Integer; const AArray: array of Integer): Boolean;
var
  I: Integer;
begin
  for I := Low(AArray) to High(AArray) do
  begin
    if AValue = AArray[I] then
      Exit(True);
  end;
  Result := False;
end;

{ TWinNativeRichEdit }

constructor TWinNativeRichEdit.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TCustomRichEditModel(inherited Model);
  FModel.OnLoadFromStream := LoadFromStreamHandler;
end;

procedure TWinNativeRichEdit.DefaultHandler(var Message);
begin
  if (Handle <> 0) and ((TMessage(Message).Msg < PM_BASE) or IntInArray(TMessage(Message).Msg, cRichEditMessages)) then
    TMessage(Message).Result := CallWindowProc(DefWndProc, Handle, TMessage(Message).Msg, TMessage(Message).WParam, TMessage(Message).LParam)
  else
    inherited DefaultHandler(Message);
end;

procedure TWinNativeRichEdit.CreateHandle;
begin
  inherited;
  ColorChanged;
end;

procedure TWinNativeRichEdit.CreateParams(var Params: TCreateParams);
const
  RichEditClassName = 'RICHEDIT20W';
begin
  inherited;
  if FRichEditModule = 0 then
  begin
    FRichEditModule := LoadLibrary(RichEditModuleName);
    if FRichEditModule <= HINSTANCE_ERROR then
      FRichEditModule := 0;
  end;
  CreateSubClass(Params, RichEditClassName);
end;

function TWinNativeRichEdit.DefineModelClass: TDataModelClass;
begin
  Result := TCustomRichEditModel;
end;

function TWinNativeRichEdit.GetRichEdit: TCustomRichEdit;
begin
  Result := Control as TCustomRichEdit;
end;

procedure TWinNativeRichEdit.LoadFromStreamHandler(Sender: TObject; const AStream: TStream; const AEncoding: TEncoding);
begin
  LoadFromStream(AStream, AEncoding);
end;

function TWinNativeRichEdit.PlainText: Boolean;
begin
  Result := Model.PlainText;
end;

procedure TWinNativeRichEdit.PMRichEditColorChanged(var Message: TDispatchMessage);
begin
  ColorChanged;
end;

procedure TWinNativeRichEdit.ColorChanged;
begin
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, TAlphaColors.ColorToRGB(RichEdit.Color));
end;

procedure TWinNativeRichEdit.LoadFromStream(const AStream: TStream; AEncoding: TEncoding);
var
  LEditStream: TEditStream;
  LPosition: Longint;
  LTextType: Longint;
  LStreamInfo: TRichEditStreamInfo;
  LConverter: TConversion;
begin
  SendMessage(Handle, EM_EXLIMITTEXT, 0, $7FFFFFF0);
  if AEncoding = nil then
  begin
    // Find the appropriate encoding
    if ContainsPreamble(AStream, TEncoding.Unicode.GetPreamble) then
      AEncoding := TEncoding.Unicode
    else
      if ContainsPreamble(AStream, TEncoding.BigEndianUnicode.GetPreamble) then
        AEncoding := TEncoding.BigEndianUnicode
      else
        if ContainsPreamble(AStream, TEncoding.UTF8.GetPreamble) then
          AEncoding := TEncoding.UTF8
        else
          AEncoding := TEncoding.Default;
  end;

  LStreamInfo.Stream := AStream;
  if FConverter <> nil then
    LConverter := FConverter
  else
    LConverter := TConversion.Create;
  LStreamInfo.Converter := LConverter;
  LStreamInfo.PlainText := PlainText;
  LStreamInfo.Encoding := AEncoding;
  try
    with LEditStream do
    begin
      dwCookie := DWORD_PTR(@LStreamInfo);
      pfnCallBack := StreamLoad;
      dwError := 0;
    end;
    LPosition := AStream.Position;
    if PlainText then
      LTextType := SF_TEXT or SF_UNICODE
    else
      LTextType := SF_RTF;
    SendMessage(Handle, EM_STREAMIN, LTextType, LPARAM(@LEditStream));
    if (LTextType = SF_RTF) and (LEditStream.dwError <> 0) then
    begin
      AStream.Position := LPosition;
      if PlainText then
        LTextType := SF_RTF
      else
        LTextType := SF_TEXT or SF_UNICODE;
      LStreamInfo.PlainText := not PlainText;
      SendMessage(Handle, EM_STREAMIN, LTextType, LPARAM(@LEditStream));
      if LEditStream.dwError <> 0 then
        raise EOutOfResources.Create(sRichEditLoadFail);
    end;
    // SetEncoding(AEncoding); // Keep Encoding in case the stream is saved
  finally
    if FConverter = nil then
      LConverter.Free;
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TRichEdit, TControlType.Platform, TWinPresentationProxy<TWinNativeRichEdit>);

finalization
  TPresentationProxyFactory.Current.Unregister(TRichEdit, TControlType.Platform, TWinPresentationProxy<TWinNativeRichEdit>);
  if FRichEditModule <> 0 then
    FreeLibrary(FRichEditModule);

end.
