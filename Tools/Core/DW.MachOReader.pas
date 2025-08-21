unit DW.MachOReader;

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

// HUGE thanks to Alex Denisov with his article:
//   https://lowlevelbits.org/parsing-mach-o-files/
// ..and corresponding project:
//   https://github.com/AlexDenisov/segment_dumper
// His work provided enormous leaps forward to make this possible

interface

uses
  // RTL
  System.Classes,
  // DW
  DW.MachO;

type
  TMagic = record
    Magic: UInt32;
    Is64: Boolean;
    IsFat: Boolean;
    IsMagic: Boolean;
    NeedsSwapBytes: Boolean;
    constructor Create(const AMagic: UInt32);
  end;

  TMachOItem = record
    CPUType: string;
    Dylibs: TArray<string>;
  end;

  TMachOItems = TArray<TMachOItem>;

  TMachOReader = class(TBinaryReader)
  private
    FItems: TMachOItems;
    procedure ExtractFatHeader(const AMagic: TMagic);
    procedure ExtractLoadCommands(const AMagic: TMagic; const ACPUType: string; const ACount: UInt32; const AOffset: Int64);
    procedure ExtractMachHeader(const AMagic: TMagic; const AOffset: Int64);
    function ReadMachOString(const AOffset: Int64): string;
    procedure ReadMachOStruct(AStruct: Pointer; const ASize: NativeUInt; const AOffset: Int64);
    function ReadMagic(const AOffset: Int64): TMagic;
    procedure Extract;
  public
    constructor Create(const AFileName: string);
    property Items: TMachOItems read FItems;
  end;

implementation

uses
  // RTL
  System.SysUtils;

type
  TMachOItemsHelper = record helper for TMachOItems
  private
    function IndexOfCPUType(const ACPUType: string): Integer;
  public
    procedure AddDylib(const ACPUType, ADylib: string);
  end;

function GetCPUType(const AType, ASubtype: Integer): string;
begin
  case AType and CPU_ARCH_MASK of
    CPU_TYPE_X86:
      Result := 'Intel x86';
    CPU_TYPE_X86_64:
      Result := 'Intel x86_64';
    CPU_TYPE_ARM:
    begin
      Result := 'ARM';
      case ASubtype of
        9:
          Result := Result + 'v7';
        11:
          Result := Result + 'v7s';
      end;
    end;
    CPU_TYPE_ARM64:
      Result := 'ARM64';
    CPU_TYPE_ARM64_32:
      Result := 'ARM64 32';
    CPU_TYPE_MC680x0:
      Result := 'Motorola 68000';
    CPU_TYPE_MC88000:
      Result := 'Motorola 88000';
    CPU_TYPE_MC98000:
      Result := 'Motorola 98000';
    CPU_TYPE_HPPA:
      Result := 'HP PA-RISC';
    CPU_TYPE_SPARC:
      Result := 'SPARC';
    CPU_TYPE_I860:
      Result := 'Intel i860';
  else
    Result := 'Unknown';
  end;
end;

{ TMachOItemsHelper }

procedure TMachOItemsHelper.AddDylib(const ACPUType, ADylib: string);
var
  LIndex: Integer;
begin
  LIndex := IndexOfCPUType(ACPUType);
  if LIndex = -1 then
  begin
    LIndex := Length(Self);
    SetLength(Self, LIndex + 1);
  end;
  Self[LIndex].CPUType := ACPUType;
  Self[LIndex].Dylibs := Self[LIndex].Dylibs + [ADylib];
end;

function TMachOItemsHelper.IndexOfCPUType(const ACPUType: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(Self) - 1 do
  begin
    if Self[I].CPUType.Equals(ACPUType) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TMagic }

constructor TMagic.Create(const AMagic: UInt32);
begin
  Magic := AMagic;
  Is64 := (Magic = MH_MAGIC_64) or (Magic = MH_CIGAM_64);
  IsFat := (Magic = FAT_MAGIC) or (Magic = FAT_CIGAM);
  IsMagic := Is64 or IsFat or (Magic = MH_MAGIC) or (Magic = MH_CIGAM);
  NeedsSwapBytes := (Magic = MH_CIGAM) or (Magic = MH_CIGAM_64) or (Magic = FAT_CIGAM);
end;

{ TMachOReader }

constructor TMachOReader.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  Extract;
end;

procedure TMachOReader.Extract;
var
  LMagic: TMagic;
begin
  LMagic := ReadMagic(0);
  if LMagic.IsMagic then
  begin
    if LMagic.IsFat then
      ExtractFatHeader(LMagic)
    else
      ExtractMachHeader(LMagic, 0);
  end;
  // else ERROR
end;

procedure TMachOReader.ExtractFatHeader(const AMagic: TMagic);
var
  LHeader: fat_header;
  LHeaderSize, LArchSize: NativeUInt;
  LArchOffset, LMachOffset: UInt64;
  LArch: fat_arch;
  I: UInt32;
  LMagic: TMagic;
begin
  FillChar(LHeader, SizeOf(LHeader), 0);
  LHeaderSize := SizeOf(LHeader);
  ReadMachOStruct(@LHeader, LHeaderSize, 0);
  if AMagic.NeedsSwapBytes then
    swap_fat_header(LHeader);
  LArchSize := SizeOf(LArch);
  LArchOffset := LHeaderSize;
  for I := 0 to LHeader.nfat_arch - 1 do
  begin
    ReadMachOStruct(@LArch, LArchSize, LArchOffset);
    if AMagic.NeedsSwapBytes then
      swap_fat_arch(LArch);
    LMachOffset := LArch.offset;
    LArchOffset := LArchOffset + LArchSize;
    LMagic := ReadMagic(LMachOffset);
    if LMagic.IsMagic then
      ExtractMachHeader(LMagic, LMachOffset);
    // else ERROR
  end;
end;

procedure TMachOReader.ExtractMachHeader(const AMagic: TMagic; const AOffset: Int64);
var
  LNCmds: UInt32;
  LCommandsOffset: UInt64;
  LHeader64: mach_header_64;
  LHeader: mach_header;
  LHeaderSize: NativeUInt;
  LCPUType: string;
begin
  LCommandsOffset := AOffset;
  if AMagic.Is64 then
  begin
    LHeaderSize := SizeOf(LHeader64);
    ReadMachOStruct(@LHeader64, LHeaderSize, AOffset);
    if AMagic.NeedsSwapBytes then
      swap_mach_header_64(LHeader64);
    LCPUType := GetCPUType(LHeader64.cputype, LHeader64.cpusubtype);
    LNCmds := LHeader64.ncmds;
    LCommandsOffset := LCommandsOffset + LHeaderSize;
  end
  else
  begin
    LHeaderSize := SizeOf(LHeader);
    ReadMachOStruct(@LHeader, LHeaderSize, AOffset);
    if AMagic.NeedsSwapBytes then
      swap_mach_header(LHeader);
    LCPUType := GetCPUType(LHeader.cputype, LHeader.cpusubtype);
    LNCmds := LHeader.ncmds;
    LCommandsOffset := LCommandsOffset + LHeaderSize;
  end;
  ExtractLoadCommands(AMagic, LCPUType, LNCmds, LCommandsOffset);
end;

procedure TMachOReader.ExtractLoadCommands(const AMagic: TMagic; const ACPUType: string; const ACount: UInt32; const AOffset: Int64);
var
  LCmdOffset: Int64;
  I: UInt32;
  LCmd: load_command;
  LDylibCmd: dylib_command;
  LDylibName: string;
begin
  LCmdOffset := AOffset;
  for I := 0 to ACount - 1 do
  begin
    ReadMachOStruct(@LCmd, SizeOf(LCmd), LCmdOffset);
    if AMagic.NeedsSwapBytes then
      swap_load_cmd(LCmd);
    if (LCmd.cmd = LC_LOAD_DYLIB) or (LCmd.cmd = LC_ID_DYLIB) then
    begin
      ReadMachOStruct(@LDylibCmd, SizeOf(LDylibCmd), LCmdOffset);
      if AMagic.NeedsSwapBytes then
        swap_dylib_command(LDylibCmd);
      LDylibName := ReadMachOString(LCmdOffset + LDylibCmd.dylib.name.offset);
      FItems.AddDylib(ACPUType, LDylibName);
    end;
    LCmdOffset := LCmdOffset + LCmd.cmdsize;
  end;
end;

function TMachOReader.ReadMagic(const AOffset: Int64): TMagic;
begin
  BaseStream.Position := AOffset;
  Result := TMagic.Create(ReadUInt32);
end;

function TMachOReader.ReadMachOString(const AOffset: Int64): string;
var
  LBytes: TBytes;
  LByte: Byte;
begin
  LBytes := [];
  BaseStream.Position := AOffset;
  repeat
    LByte := ReadByte;
    if LByte <> 0 then
      LBytes := LBytes + [LByte];
  until (LByte = 0) or (BaseStream.Position = BaseStream.Size);
  Result := TEncoding.ASCII.GetString(LBytes);
end;

procedure TMachOReader.ReadMachOStruct(AStruct: Pointer; const ASize: NativeUInt; const AOffset: Int64);
var
  LBytes: TBytes;
begin
  SetLength(LBytes, ASize);
  BaseStream.Position := AOffset;
  Read(LBytes, 0, ASize);
  Move(LBytes[0], AStruct^, ASize);
end;

end.
