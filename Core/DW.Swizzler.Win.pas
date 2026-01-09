unit DW.Swizzler.Win;

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

interface

type
  TRelocation = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: ^Pointer;
  end;

  /// <summary>
  ///   Class for redirecting ("swizzling") methods
  /// </summary>
  /// <remarks>
  ///   Useful for "overriding" non-virtual methods. NOTE: At present, works on Windows ONLY
  /// </remarks>
  TSwizzler = class(TObject)
  private
    FNewProc: Pointer;
    FOldProc: Pointer;
    FRelocation: TRelocation;
  public
    class function GetActualAddr(const AProc: Pointer): Pointer;
    class function GetAddressOf(const AMethodAddr: Pointer; const ASignature: array of Byte): Pointer;
  public
    constructor Create(const APackage, AProcName: string; const ANewProc: Pointer); overload;
    constructor Create(const AOldProc, ANewProc: Pointer); overload;
    procedure BeforeDestruction; override;
    /// <summary>
    ///   Disables swizzling
    /// </summary>
    /// <remarks>
    ///   Call this method inside of the "overriding" method before calling the swizzled method to prevent recursion.
    /// </remarks>
    procedure Disable;
    /// <summary>
    ///   Enables swizzling
    /// </summary>
    procedure Enable;
    /// <summary>
    ///   Reference to the original (swizzled) method
    /// </summary>
    property OldProc: Pointer read FOldProc;
  end;

implementation

uses
  // RTL
  System.Types, System.SysUtils,
  // Windows
  Winapi.Windows;

{ TSwizzler }

constructor TSwizzler.Create(const AOldProc, ANewProc: Pointer);
begin
  inherited Create;
  FOldProc := AOldProc;
  FNewProc := ANewProc;
  Enable;
end;

constructor TSwizzler.Create(const APackage, AProcName: string; const ANewProc: Pointer);
var
  LProc: Pointer;
  LHandle: Cardinal;
begin
  LHandle := GetModuleHandle(PChar(APackage));
  LProc := GetProcAddress(LHandle, PChar(AProcName));
  Create(LProc, ANewProc);
end;

procedure TSwizzler.BeforeDestruction;
begin
  inherited;
  Disable;
end;

procedure TSwizzler.Disable;
var
  LBytesWritten: NativeUInt;
begin
  if FRelocation.Jump <> 0 then
    WriteProcessMemory(GetCurrentProcess, GetActualAddr(FOldProc), @FRelocation, SizeOf(FRelocation), LBytesWritten);
end;

procedure TSwizzler.Enable;
var
  LOldProtect: Cardinal;
  LActualAddr: Pointer;
begin
  if Assigned(FOldProc) then
  begin
    LActualAddr := GetActualAddr(FOldProc);
    if VirtualProtect(LActualAddr, SizeOf(FRelocation), PAGE_EXECUTE_READWRITE, LOldProtect) then
    begin
      FRelocation := TRelocation(LActualAddr^);
      TRelocation(LActualAddr^).Jump := $E9;
      TRelocation(LActualAddr^).Offset := Integer(FNewProc) - (Integer(LActualAddr) + SizeOf(FRelocation));
      VirtualProtect(LActualAddr, SizeOf(FRelocation), LOldProtect, @LOldProtect);
      FlushInstructionCache(GetCurrentProcess, LActualAddr, SizeOf(FRelocation));
    end;
  end;
end;

class function TSwizzler.GetActualAddr(const AProc: Pointer): Pointer;
begin
  Result := nil;
  if AProc <> nil then
  begin
    if PAbsoluteIndirectJmp(AProc).OpCode = $25FF then
      Result := PAbsoluteIndirectJmp(AProc).Addr^
    else
      Result := AProc;
  end;
end;

class function TSwizzler.GetAddressOf(const AMethodAddr: Pointer; const ASignature: array of Byte): Pointer;
var
  LActualAddr: PByteArray;
begin
  LActualAddr := GetActualAddr(AMethodAddr);
  while not CompareMem(LActualAddr, @ASignature, Length(ASignature)) do
    Inc(PByte(LActualAddr));
  Result := Pointer(Integer(@LActualAddr[5]) + PInteger(@LActualAddr[1])^);
end;

end.

