unit DW.Base64.Helpers;

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
  // RTL
  System.Classes;

type
  TBase64Helper = record
  public
    /// <summary>
    ///   Takes a string, compresses it, then Base64 encodes it
    /// </summary>
    class function CompressEncode(const ASource: string): string; overload; static;
    /// <summary>
    ///   Takes a stream, compresses it, then Base64 encodes it
    /// </summary>
    class function CompressEncode(const AStream: TStream): string; overload; static;
    /// <summary>
    ///   Compresses and encodes a file to a Base64 string
    /// </summary>
    class function CompressEncodeFromFile(const AFileName: string): string; static;
    /// <summary>
    ///   Decodes a Base64 string to a stream
    /// </summary>
    class procedure Decode(const ASource: string; const AStream: TStream); static;
    /// <summary>
    ///   Takes a string, Base64 decodes it, then decompresses it
    /// </summary>
    class function DecodeDecompress(const ASource: string): string; overload; static;
    /// <summary>
    ///   Takes a string, Base64 decodes it, then decompresses it into a stream
    /// </summary>
    class procedure DecodeDecompress(const ASource: string; const AStream: TStream); overload; static;
    /// <summary>
    ///   Decodes a Base64 string, decompresses it, and saves it to a file
    /// </summary>
    class procedure DecodeDecompressToFile(const ASource, AFileName: string); static;
    /// <summary>
    ///   Decodes a Base64 string and saves it to a file
    /// </summary>
    class procedure DecodeToFile(const ASource, AFileName: string); static;
    /// <summary>
    ///   Encodes a stream to a Base64 string
    /// </summary>
    class function Encode(const AStream: TStream): string; overload; static;
    /// <summary>
    ///   Encodes a string to a Base64 string
    /// </summary>
    class function Encode(const ASource: string): string; overload; static;
    /// <summary>
    ///   Encodes a file to a Base64 string
    /// </summary>
    class function EncodeFromFile(const AFileName: string): string; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.ZLib, System.NetEncoding;

{ TBase64Helper }

class function TBase64Helper.CompressEncode(const ASource: string): string;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(ASource);
  try
    Result := CompressEncode(LStream);
  finally
    LStream.Free;
  end;
end;

class function TBase64Helper.CompressEncode(const AStream: TStream): string;
var
  LCompressedStream: TStream;
  LBase64Stream: TStringStream;
begin
  AStream.Position := 0;
  LCompressedStream := TMemoryStream.Create;
  try
    ZCompressStream(AStream, LCompressedStream);
    LCompressedStream.Position := 0;
    LBase64Stream := TStringStream.Create('', TEncoding.ASCII);
    try
      TNetEncoding.Base64.Encode(LCompressedStream, LBase64Stream);
      Result := LBase64Stream.DataString;
    finally
      LBase64Stream.Free;
    end;
  finally
    LCompressedStream.Free;
  end;
end;

class function TBase64Helper.DecodeDecompress(const ASource: string): string;
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create;
  try
    DecodeDecompress(ASource, LStringStream);
    Result := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
end;

class procedure TBase64Helper.Decode(const ASource: string; const AStream: TStream);
var
  LBase64Stream: TStream;
begin
  LBase64Stream := TStringStream.Create(ASource, TEncoding.ASCII);
  try
    TNetEncoding.Base64.Decode(LBase64Stream, AStream);
  finally
    LBase64Stream.Free;
  end;
end;

class procedure TBase64Helper.DecodeDecompress(const ASource: string; const AStream: TStream);
var
  LCompressedStream: TStream;
  LBase64Stream: TStream;
begin
  AStream.Position := 0;
  LCompressedStream := TMemoryStream.Create;
  try
    LBase64Stream := TStringStream.Create(ASource, TEncoding.ASCII);
    try
      TNetEncoding.Base64.Decode(LBase64Stream, LCompressedStream);
      LCompressedStream.Position := 0;
      ZDecompressStream(LCompressedStream, AStream);
    finally
      LBase64Stream.Free;
    end;
  finally
    LCompressedStream.Free;
  end;
end;

class procedure TBase64Helper.DecodeDecompressToFile(const ASource, AFileName: string);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    DecodeDecompress(ASource, LStream);
    LStream.SaveToFile(AFileName);
  finally
    LStream.Free;
  end;
end;

class procedure TBase64Helper.DecodeToFile(const ASource, AFileName: string);
var
  LStream: TMemoryStream;
  LBytes: TBytes;
begin
  LStream := TMemoryStream.Create;
  try
    LBytes := TNetEncoding.Base64.DecodeStringToBytes(ASource);
    LStream.Write(LBytes, Length(LBytes));
    LStream.SaveToFile(AFileName);
  finally
    LStream.Free;
  end;
end;

class function TBase64Helper.CompressEncodeFromFile(const AFileName: string): string;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFileName);
    Result := CompressEncode(LStream);
  finally
    LStream.Free;
  end;
end;

class function TBase64Helper.Encode(const AStream: TStream): string;
var
  LBase64Stream: TStringStream;
  LBase64: TBase64Encoding;
begin
  AStream.Position := 0;
  LBase64Stream := TStringStream.Create;
  try
    LBase64 := TBase64Encoding.Create(0, '');
    try
      LBase64.Encode(AStream, LBase64Stream);
    finally
      LBase64.Free;
    end;
    Result := LBase64Stream.DataString;
  finally
    LBase64Stream.Free;
  end;
end;

class function TBase64Helper.Encode(const ASource: string): string;
var
  LBase64: TBase64Encoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := LBase64.Encode(ASource);
  finally
    LBase64.Free;
  end;
end;

class function TBase64Helper.EncodeFromFile(const AFileName: string): string;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFileName);
    Result := Encode(LStream);
  finally
    LStream.Free;
  end;
end;

end.
