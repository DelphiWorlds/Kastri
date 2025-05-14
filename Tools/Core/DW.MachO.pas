unit DW.MachO;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

// **** This is a subset of translations of the mach-o headers in /usr/include/mach-o in iOS/macOS SDKs *****

// As at 11-APR-2024, is sufficient to:
//   * Read the header, and commands (e.g. LC_LOAD_DYLIB)

interface

const
  MH_MAGIC = $feedface; // the 32-bit mach magic number
  MH_CIGAM = $cefaedfe;

  MH_MAGIC_64 = $feedfacf; // the 64-bit mach magic number
  MH_CIGAM_64 = $cffaedfe;

  FAT_MAGIC =	$cafebabe;
  FAT_CIGAM	= $bebafeca;

  LC_REQ_DYLD = $80000000;
  LC_SEGMENT = $1;              // segment of this file to be mapped
  LC_SYMTAB = $2;               // link-edit stab symbol table info
  LC_SYMSEG = $3;               // link-edit gdb symbol table info (obsolete)
  LC_THREAD = $4;               // thread
  LC_UNIXTHREAD = $5;           // unix thread (includes a stack)
  LC_LOADFVMLIB = $6;           // load a specified fixed VM shared library
  LC_IDFVMLIB = $7;             // fixed VM shared library identification
  LC_IDENT = $8;                // object identification info (obsolete)
  LC_FVMFILE = $9;              // fixed VM file inclusion (internal use)
  LC_PREPAGE = $a;              // prepage command (internal use)
  LC_DYSYMTAB = $b;             // dynamic link-edit symbol table info
  LC_LOAD_DYLIB = $c;           // load a dynamically linked shared library
  LC_ID_DYLIB = $d;             // dynamically linked shared lib ident
  LC_LOAD_DYLINKER = $e;        // load a dynamic linker
  LC_ID_DYLINKER = $f;          // dynamic linker identification
  LC_PREBOUND_DYLIB = $10;      // modules prebound for a dynamically linked shared library
  LC_ROUTINES = $11;            // image routines
  LC_SUB_FRAMEWORK = $12;       // sub framework
  LC_SUB_UMBRELLA = $13;        // sub umbrella
  LC_SUB_CLIENT = $14;          // sub client
  LC_SUB_LIBRARY = $15;         // sub library
  LC_TWOLEVEL_HINTS = $16;      // two-level namespace lookup hints
  LC_PREBIND_CKSUM = $17;       // prebind checksum

  CPU_ARCH_MASK = $FF000000;      // mask for architecture bits
  CPU_ARCH_ABI64 = $01000000;     // 64 bit ABI
  CPU_ARCH_ABI64_32 = $02000000;  // ABI for 64-bit hardware with 32-bit types; LP32
  CPU_TYPE_MC680x0 = 6;
  CPU_TYPE_X86 = 7;
  CPU_TYPE_I386 = CPU_TYPE_X86;   // compatibility
  CPU_TYPE_X86_64 = CPU_TYPE_X86 or CPU_ARCH_ABI64;
  CPU_TYPE_MC98000 = 10;
  CPU_TYPE_HPPA = 11;
  CPU_TYPE_ARM = 12;
  CPU_TYPE_ARM64 = CPU_TYPE_ARM or CPU_ARCH_ABI64;
  CPU_TYPE_ARM64_32 = CPU_TYPE_ARM or CPU_ARCH_ABI64_32;
  CPU_TYPE_MC88000 = 13;
  CPU_TYPE_SPARC = 14;
  CPU_TYPE_I860 = 15;

type
  size_t = NativeUInt;

  mach_header = record
    magic: UInt32;      // mach magic number identifier  (uint32_t)
    cputype: Integer;     // cpu specifier
    cpusubtype: Integer;  // machine specifier
    filetype: UInt32;   // type of file
    ncmds: UInt32;      // number of load commands
    sizeofcmds: UInt32; // the size of all the load commands
    flags: UInt32;      // flags
  end;

  mach_header_64 = record
    magic: UInt32;      // mach magic number identifier
    cputype: Integer;     // cpu specifier
    cpusubtype: UInt32;  // machine specifier
    filetype: UInt32;   // type of file
    ncmds: UInt32;      // number of load commands
    sizeofcmds: UInt32; // the size of all the load commands
    flags: UInt32;      // flags
    reserved: UInt32;   // reserved
  end;

  fat_header = record
    magic: UInt32;     // FAT_MAGIC or FAT_MAGIC_64
    nfat_arch: UInt32; // number of structs that follow
  end;

  fat_arch = record
    cputype: Integer;    // cpu specifier (int)
    cpusubtype: UInt32; // machine specifier (int)
    offset: UInt32;    // file offset to this object file
    size: UInt32;      // size of this object file
    align: UInt32;     // alignment as a power of 2
  end;

  fat_arch_64 = record
    cputype: Integer;     // cpu specifier (int)
    cpusubtype: UInt32;  // machine specifier (int)
    offset: UInt64;       // file offset to this object file
    size: UInt64;         // size of this object file
    align: UInt32;      // alignment as a power of 2
    reserved: UInt32;   // reserved
  end;

  lc_str = record
    offset: UInt32; // offset to the string
  end;

  load_command = record
    cmd: UInt32;
    cmdsize: UInt32;
  end;

  segment_command = record
    cmd: UInt32;             // LC_SEGMENT
    cmdsize: UInt32;         // includes sizeof section structs
    segname: array[0..15] of Char; // segment name
    vmaddr: UInt32;          // memory address of this segment
    vmsize: UInt32;          // memory size of this segment
    fileoff: UInt32;         // file offset of this segment
    filesize: UInt32;        // amount to map from the file
    maxprot: Integer;          // maximum VM protection
    initprot: Integer;         // initial VM protection
    nsects: UInt32;          // number of sections in segment
    flags: UInt32;           // flags
  end;

  segment_command_64 = record
    cmd: UInt32;             // LC_SEGMENT_64
    cmdsize: UInt32;         // includes sizeof section_64 structs
    segname: array[0..15] of Char; // segment name
    vmaddr: UInt64;            // memory address of this segment
    vmsize: UInt64;            // memory size of this segment
    fileoff: UInt64;           // file offset of this segment
    filesize: UInt64;          // amount to map from the file
    maxprot: Integer;          // maximum VM protection
    initprot: Integer;         // initial VM protection
    nsects: UInt32;          // number of sections in segment
    flags: UInt32;           // flags
  end;

  dylib = record
    name: lc_str;                   // library's path name
    timestamp: UInt32;            // library's build time stamp
    current_version: UInt32;      // library's current version number
    compatibility_version: UInt32; // library's compatibility version number
  end;

  dylib_command = record
    cmd: UInt32;    // LC_ID_DYLIB, LC_LOAD_{,WEAK_}DYLIB, LC_REEXPORT_DYLIB
    cmdsize: UInt32; // includes pathname string
    dylib: dylib;      // the library identification
  end;

  dylib_use_command = record
    cmd: UInt32;            // LC_LOAD_DYLIB or LC_LOAD_WEAK_DYLIB
    cmdsize: UInt32;        // overall size, including path
    nameoff: UInt32;        // == 28, dylibs's path offset
    marker: UInt32;         // == DYLIB_USE_MARKER
    current_version: UInt32; // dylib's current version number
    compat_version: UInt32; // dylib's compatibility version number
    flags: UInt32;          // DYLIB_USE_... flags
  end;

procedure swap_dylib(var value: dylib);
procedure swap_dylib_command(var cmd: dylib_command);
procedure swap_fat_arch(var arch: fat_arch);
procedure swap_fat_header(var header: fat_header);
procedure swap_load_cmd(var cmd: load_command);
procedure swap_mach_header(var header: mach_header);
procedure swap_mach_header_64(var header: mach_header_64);

implementation

function ReverseBytes(const AValue: Word): Word; overload;
begin
  Result := ((AValue and $FF00) shr 8) or ((AValue and $00FF) shl 8);
end;

function ReverseBytes(const AValue: LongWord): LongWord; overload;
begin
  Result := (((AValue and $FF000000) shr 24) or ((AValue and $00FF0000) shr 8) or ((AValue and $0000FF00) shl 8)
    or ((AValue and $000000FF) shl 24));
end;

procedure swap_dylib(var value: dylib);
begin
  value.name.offset := ReverseBytes(value.name.offset);
  value.timestamp := ReverseBytes(value.timestamp);
  value.current_version := ReverseBytes(value.current_version);
  value.compatibility_version := ReverseBytes(value.compatibility_version);
end;

procedure swap_dylib_command(var cmd: dylib_command);
begin
  cmd.cmd := ReverseBytes(cmd.cmd);
  cmd.cmdsize := ReverseBytes(cmd.cmdsize);
  swap_dylib(cmd.dylib);
end;

procedure swap_fat_arch(var arch: fat_arch);
begin
  arch.cputype := ReverseBytes(arch.cputype);
  arch.cpusubtype := ReverseBytes(arch.cpusubtype);
  arch.offset := ReverseBytes(arch.offset);
  arch.size := ReverseBytes(arch.size);
  arch.align := ReverseBytes(arch.align);
end;

procedure swap_fat_header(var header: fat_header);
begin
  header.magic := ReverseBytes(header.magic);
  header.nfat_arch := ReverseBytes(header.nfat_arch);
end;

procedure swap_load_cmd(var cmd: load_command);
begin
  cmd.cmd := ReverseBytes(cmd.cmd);
  cmd.cmdsize := ReverseBytes(cmd.cmdsize);
end;

procedure swap_mach_header(var header: mach_header);
begin
  header.magic := ReverseBytes(header.magic);
  header.cputype := ReverseBytes(header.cputype);
  header.cpusubtype := ReverseBytes(header.cpusubtype);
  header.filetype := ReverseBytes(header.filetype);
  header.ncmds := ReverseBytes(header.ncmds);
  header.sizeofcmds := ReverseBytes(header.sizeofcmds);
  header.flags := ReverseBytes(header.flags);
end;

procedure swap_mach_header_64(var header: mach_header_64);
begin
  header.magic := ReverseBytes(header.magic);
  header.cputype := ReverseBytes(header.cputype);
  header.cpusubtype := ReverseBytes(header.cpusubtype);
  header.filetype := ReverseBytes(header.filetype);
  header.ncmds := ReverseBytes(header.ncmds);
  header.sizeofcmds := ReverseBytes(header.sizeofcmds);
  header.flags := ReverseBytes(header.flags);
  header.reserved := ReverseBytes(header.reserved);
end;

end.
