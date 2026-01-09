unit DW.Macapi.ObjCBlocks;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}


// This unit is based on this article:
//   https://sudonull.com/post/72033-Using-code-blocks-from-Objective-C-in-Delphi-on-macOS-how-we-built-bridges
// If the link ever disappears, I have it "backed up"

interface

uses
  // RTL
  System.SysUtils;

type
  TObjCBlock = record
  private
    class function InternalCreateBlock(const AProc: TProc; const AParamCount: Integer): Pointer; static;
  public
    class function CreateBlock(const AProc: TProc): Pointer; overload; static;
    class function CreateBlock(const AProc: TProc<Pointer>): Pointer; overload; static;
  end;

implementation

uses
  // RTL
  System.Rtti, System.TypInfo,
  // macOS
  Macapi.Helpers, Macapi.ObjectiveC,
  {$IF not Defined(IOS)}
  Macapi.Foundation;
  {$ELSE}
  // iOS
  iOSapi.Foundation;
  {$ENDIF}

type
  TBlockMethodParams = TArray<Pointer>;

  TBlockDescriptor = packed record
    Reserved: NativeUInt;
    Size: NativeUInt;
    CopyBlockProc: Pointer;
    DisposeBlockProc: Pointer;
  end;
  PBlockDescriptor = ^TBlockDescriptor;

  TBlockLiteral = packed record
    Isa: Pointer;
    Flags: integer;
    Reserved: integer;
    Invoke: Pointer;
    Descriptor: PBlockDescriptor;
  end;
  PBlockLiteral = ^TBlockLiteral;

  TBlockInfo = packed record
    Literal: TBlockLiteral;
    Proc: TProc;
    ParamCount: Integer;
  end;

  TBlockList = TArray<TBlockInfo>;

  TObjCBlocks = class(TObject)
  private
    FList: TBlockList;
    procedure ClearAllBlocks;
  protected
    function AddBlock(const AProc: TProc; const AParamCount: Integer): Pointer;
    procedure ClearBlock(const AIndex: Integer);
    function FindBlock(const ABlock: Pointer): Integer;
  public
    destructor Destroy; override;
    property BlockList: TBlockList read FList;
  end;

var
  Blocks: TObjCBlocks;

procedure InvokeProc(const AInfo: TBlockInfo; const AParams: TBlockMethodParams);
begin
  case AInfo.ParamCount of
    0:
      AInfo.Proc();
    1:
      TProc<Pointer>(AInfo.Proc)(AParams[0]);
  end;
end;

function Invoke(const ABlock: Pointer; const AParams: TBlockMethodParams): Pointer; cdecl;
var
  LIndex: Integer;
begin
  Result := nil;
  TMonitor.Enter(Blocks);
  try
    LIndex := Blocks.FindBlock(ABlock);
    if LIndex > -1 then
      InvokeProc(Blocks.BlockList[LIndex], AParams);
  finally
    TMonitor.Exit(Blocks);
  end;
end;

procedure DisposeBlock(ABlock: Pointer) cdecl;
var
  LIndex: Integer;
begin
  TMonitor.Enter(Blocks);
  try
    LIndex := Blocks.FindBlock(ABlock);
    if LIndex > -1 then
      Blocks.ClearBlock(LIndex);
  finally
    TMonitor.Exit(Blocks);
  end;
  TNSObject.Wrap(ABlock).release;
end;

procedure CopyBlock(ASource, ADest: Pointer) cdecl;
begin
  // Do nothing
end;

{ TObjCBlock }

class function TObjCBlock.InternalCreateBlock(const AProc: TProc; const AParamCount: Integer): Pointer;
begin
  TMonitor.Enter(Blocks);
  try
    Result:= Blocks.AddBlock(AProc, AParamCount);
  finally
    TMonitor.Exit(Blocks);
  end;
end;

class function TObjCBlock.CreateBlock(const AProc: TProc): Pointer;
begin
  Result := TObjCBlock.InternalCreateBlock(TProc(AProc), 0);
end;

class function TObjCBlock.CreateBlock(const AProc: TProc<Pointer>): Pointer;
begin
  Result := TObjCBlock.InternalCreateBlock(TProc(AProc), 1);
end;

{TObjCBlocks}

destructor TObjCBlocks.Destroy;
begin
  TMonitor.Enter(Self);
  try
    ClearAllBlocks;
  finally
    TMonitor.Exit(Self);
  end;
  inherited Destroy;
end;

procedure TObjCBlocks.ClearBlock(const AIndex: Integer);
begin
  Dispose(FList[AIndex].Literal.Descriptor);
  FList[AIndex].Literal.Isa := nil;
  Delete(FList, AIndex, 1);
end;

function TObjCBlocks.AddBlock(const AProc: TProc; const AParamCount: Integer): Pointer;
const
  BLOCK_HAS_COPY_DISPOSE = 1 shl 25;
var
  LDescriptor: PBlockDescriptor;
  LBlockInfo: TBlockInfo;
begin
  SetLength(FList, Length(FList) + 1);
  LBlockInfo := Default(TBlockInfo);
  LBlockInfo.ParamCount := AParamCount;
  LBlockInfo.Literal.Isa := NSClassFromString(NSObjectToID(StrToNSStr('NSBlock')));
  LBlockInfo.Literal.Invoke := @Invoke;
  LBlockInfo.Literal.Flags := BLOCK_HAS_COPY_DISPOSE;
  LBlockInfo.Proc := AProc;
  New(LDescriptor);
  LDescriptor.Reserved := 0;
  LDescriptor.Size := SizeOf(TBlockLiteral);
  LDescriptor.CopyBlockProc := @CopyBlock;
  LDescriptor.DisposeBlockProc := @DisposeBlock;
  LBlockInfo.Literal.Descriptor := LDescriptor;
  FList[High(FList)] := LBlockInfo;
  Result:= @FList[High(FList)].Literal;
end;

procedure TObjCBlocks.ClearAllBlocks;
var
  I: Integer;
begin
  for I := High(FList) downto Low(FList) do
    ClearBlock(I);
end;

function TObjCBlocks.FindBlock(const ABlock: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FList) to High(FList) do
  begin
    if FList[I].Literal.Descriptor = PBlockLiteral(ABlock).Descriptor then
      Exit(I);
  end;
end;

initialization
  Blocks := TObjCBlocks.Create;

finalization
  FreeAndNil(Blocks);

end.
