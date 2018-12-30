(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008-2012 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the Iztok Kacin nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ==================================================================================
 * Fast bust secure enough XTEA based encryption / decryption
 * ==================================================================================
 * 15/12/2009 (1.0.0)
 *   - Initial implementation.
 * 21/01/2010 (1.0.1)
 *   - Use AnsiString for non unicode and not RawByteString
 * 11/10/2010 (1.1.0)
 *   - Added overloaded procedures for stream and file encryption / decryption
 * ==================================================================================
*)
unit Cromis.XTEA;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows, SysUtils, Classes;
  {$ENDIF}
  {$IFDEF CLR}
     System.Text, System.IO;
  {$ENDIF}

  const
    Delta: Longword = $9e3779b9;

  const
    {$IFDEF MSWINDOWS}
      cFileModeCreate =  fmCreate;
      cFileModeRead =  fmOpenRead;
    {$ELSE}
      cFileModeCreate =  System.IO.FileMode.CreateNew;
      cFileModeRead =  System.IO.FileMode.Open;
    {$ENDIF}

  type
    TTeaUnicodeString = {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF};
    TTeaAnsiString = {$IFDEF UNICODE} AnsiString {$ELSE} string {$ENDIF};
    TCPFileStream = {$IFDEF MSWINDOWS} TFileStream {$ELSE} FileStream {$ENDIF};
    {$IFDEF CLR} TStream = Stream; {$ENDIF}
    TLong2 = array[0.. 1] of Longword;  // 64-bit
    TTeaKey = array[0..3] of Longword;  // 128-bit
    TByte16 = array[0..15] of Byte;     // 128-bit
    TByte4 = array[0..3] of Byte;       // 32-bit
    TTeaData = array of Longword;       // n*32-bit
    {$IF RTLVersion < 20}
      TBytes = array of Byte;
    {$IFEND}

  //***************************************************
  // tea stream encryption / decryption routines
  //***************************************************

  // XTEA encryption and decryption function
  function XTeaEncryptBytes(const Data, Key: TBytes): TBytes;
  function XTeaDecryptBytes(const Data, Key: TBytes): TBytes;

  // stream encryption / decryption
  procedure XTeaEncryptStream(const InStream, OutStream: TStream; const Key: TBytes); overload;
  procedure XTeaDecryptStream(const InStream, OutStream: TStream; const Key: TBytes); overload;
  procedure XTeaEncryptStream(const InStream: TStream; const OutFile: string; const Key: TBytes); overload;
  procedure XTeaDecryptStream(const InStream: TStream; const OutFile: string; const Key: TBytes); overload;

  // file encryption / decryption
  procedure XTeaEncryptFile(const InFile, OutFile: string; const Key: TBytes); overload;
  procedure XTeaDecryptFile(const InFile, OutFile: string; const Key: TBytes); overload;
  procedure XTeaEncryptFile(const InFile: string; const OutStream: TStream; const Key: TBytes); overload;
  procedure XTeaDecryptFile(const InFile: string; const OutStream: TStream; const Key: TBytes); overload;

  // support functions for string <-> bytes conversions
  function GetBytesFromUnicodeString(const Value: TTeaUnicodeString): TBytes;
  function GetBytesFromAnsiString(const Value: TTeaAnsiString): TBytes;

  function GetUnicodeString(const Value: TBytes): TTeaUnicodeString;
  function GetAnsiString(const Value: TBytes): TTeaAnsiString;

  // --------------------------------------------------
  // Interface end
  // --------------------------------------------------

implementation
 
//******************************************************************************************
// Pascal/Delphi implementation of TEA - Tiny Encryption Algorithm
// Algorithms: TEA, XTEA, BlockTEA, XXTEA
// Original Author: Nikolai Shokhirev
// Created: April 04, 2004
// Last modified: November 24, 2009 by Kacin Iztok
// Thanks to Pedro Gimeno Fortea <parigalo@formauri.es> }
//******************************************************************************************

const
  cTeaBlockSize = 4; // size of one tea block in bytes (4 bytes)
  cTeaHeaderSize = 2; // size of header in Longwords (64 bit)

const
  // DO NOT CHANGE THIS VALUES!!!
  cTeaEncryptBlockSize = 64000; // in bytes
  cTeaDecryptBlockSize = 64008; // in bytes

procedure SetSizeHeader(var Data: TTeaData; const DataLen: Int64);
{$IFNDEF MSWINDOWS}
var
  K: Integer;
  Buffer: array of Byte;
  TempBuff1: TByte4;
  TempBuff2: TByte4;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Move(DataLen, Data[0], SizeOf(Int64));
{$ELSE}
  Buffer := BitConverter.GetBytes(DataLen);

  for K := 0 to 3 do
  begin
    TempBuff1[K] :=  Buffer[ K];
    TempBuff2[K] :=  Buffer[cTeaBlockSize + K];
  end;

  Data[0] := BitConverter.ToUInt32(TempBuff1, 0);
  Data[1] := BitConverter.ToUInt32(TempBuff2, 0);
{$ENDIF}
end;

function GetSizeHeader(const Data: TTeaData): Int64;
{$IFNDEF MSWINDOWS}
var
  K: Integer;
  Buffer: array of Byte;
  TempBuff1: TByte4;
  TempBuff2: TByte4;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Move(Data[0], Result, SizeOf(Int64));
{$ELSE}
  SetLength(Buffer, cTeaBlockSize * 2);
  TempBuff1 := BitConverter.GetBytes(Data[0]);
  TempBuff2 := BitConverter.GetBytes(Data[1]);

  for K := 0 to 3 do
  begin
    Buffer[K] := TempBuff1[K];
    Buffer[cTeaBlockSize + K] := TempBuff1[K];
  end;

  Result := BitConverter.ToInt64(Buffer, 0);
{$ENDIF}
end;

{$OVERFLOWCHECKS OFF}
procedure DoXTeaEncrypt(var Data: TLong2; const Key: TTeaKey; N: Longword = 32);
var
  y, z, sum, limit: Longword;
begin
  y := Data[0];
  z := Data[1];
  sum := 0;
  limit := Delta * N;

  while sum <> limit do
  begin
{ c code:
      y += (z << 4 ^ z >> 5) + z ^ sum + key[sum&3];
      sum += delta;
      z += (y << 4 ^ y >> 5) + y ^ sum + key[sum>>11 & 3];
}
    Inc(y, (((z shl 4) xor (z shr 5)) + z) xor (sum + Key[sum and 3]));
    Inc(sum,Delta);
    Inc(z, (((y shl 4) xor (y shr 5)) + y) xor (sum + Key[(sum shr 11) and 3]));
  end;

  Data[0] := y;
  Data[1] := z
end;

procedure DoXTeaDecrypt(var Data: TLong2; const Key: TTeaKey; N: Longword = 32);
var
  y, z, sum: Longword;
begin
  y := Data[0];
  z := Data[1];
  sum := Delta * N;

  while sum <> 0 do
  begin
{ c code:
      z -= (y << 4 ^ y >> 5) + y ^ sum + key[sum>>11 & 3];
      sum -= delta;
      y -= (z << 4 ^ z >> 5) + z ^ sum + key[sum&3];
}
    Dec(z, (((y shl 4) xor (y shr 5)) + y) xor (sum + Key[(sum shr 11) and 3]));
    Dec(sum,Delta);
    Dec(y, (((z shl 4) xor (z shr 5)) + z) xor (sum + Key[sum and 3]));
  end;

  Data[0] := y;
  Data[1] := z;
end;

procedure DoBlockXTeaEncrypt(Data: TTeaData; const Key: TTeaKey; const HeaderLen: Integer);
var
  z, y, x, sum, e, p: Longword;
  q, n: Integer;

  function mx: longword;
  begin
    result := (((z shr 5) xor (y shl 2)) +
              ((y shr 3) xor (z shl 4))) xor
              ((sum xor y) + (Key[(p and 3) xor e] xor z) );
  end;

begin
  n := Length(Data);
  q := 6 + 52 div n;
  z := Data[n - 1];
  y := Data[HeaderLen];
  sum := 0;

  repeat
    Inc(sum, Delta);
    e := (sum shr 2) and 3;

    for p := HeaderLen to n - 2 do
    begin
      y := Data[p + 1];
      x := Data[p];
      inc(x, mx);
      data[p] := x;
      z := x;
    end;

    y := Data[HeaderLen];
    x := Data[n - 1];
    Inc(x, mx);
    Data[n - 1] := x;
    z := x;
    Dec(q);
  until q = 0;
end;

procedure DoBlockXTeaDecrypt(Data: TTeaData; const Key: TTeaKey; const HeaderLen: Integer);
var
  z, y, x, sum, e, p, q: Longword;
  n: Integer;

  function mx: longword;
  begin
    Result := (((z shr 5) xor (y shl 2)) +
              ((y shr 3) xor (z shl 4))) xor
              ((sum xor y) + (Key[(p and 3) xor e] xor z) );
  end;

begin
  n := Length(Data);
  q := 6 + 52 div n;
  z := Data[n - 1];
  y := Data[HeaderLen];
  sum := q * Delta;

  while sum <> 0 do
  begin
    e := (sum shr 2) and 3;

    for p := n - 1 downto 1 + HeaderLen do
    begin
      z := Data[p - 1];
      x := Data[p];
      Dec(x, mx);
      Data[p] := x;
      y := x;
    end;
    z := Data[n - 1];
    x := Data[HeaderLen];
    Dec(x, mx);
    Data[HeaderLen] := x;
    y := x;
    Dec(sum, Delta);
  end;
end;
{$OVERFLOWCHECKS ON}

function SameKey(const Key1, Key2: TTeaKey): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to 3 do
    if Key1[I] <> Key2[I] then
      Exit;

  Result := True;
end;

procedure BytesToKey(Data: TBytes; var Key: TTeaKey);
{$IFDEF CLR}
var
  TempBytes: TByte4;
  I, K: Integer;
{$ENDIF}
begin
  SetLength(Data, cTeaBlockSize * 4);

{$IFDEF MSWINDOWS}
  Move(Data[0], Key[0], Length(Data));
{$ELSE}
  for I := 0 to 3 do
  begin
    for K := 0 to 3 do
      TempBytes[K] := Data[I * cTeaBlockSize + K];

    Key[I] := BitConverter.ToUInt32(TempBytes, 0);
  end;
{$ENDIF}
end;

procedure BytesToData(S: TBytes; var Data: TTeaData; const HeaderLen: Integer);
var
  N, M: Integer;
{$IFDEF CLR}
  TempBytes: TByte4;
  I: Integer;
{$ENDIF}
begin
  N := Length(S) div cTeaBlockSize;
  M := Length(S) mod cTeaBlockSize;

  if M <> 0 then
  begin
    Inc(N);
    SetLength(S, Length(S) + cTeaBlockSize - M);
  end;

  if N < 2 then  // n = 1
  begin
    N := 2;
    SetLength(S, Length(S) + cTeaBlockSize);
  end;

  // set buffer length (plus header)
  SetLength(Data, N + HeaderLen);

{$IFDEF MSWINDOWS}
  Move(S[0], Data[HeaderLen], Length(S));
{$ELSE}
  for I := 0 to N - 1 do
  begin
    for M := 0 to 3 do
      TempBytes[M] := S[(I * cTeaBlockSize) + M];

    // put it back to longword
    Data[I + HeaderLen] := BitConverter.ToUInt32(TempBytes, 0);
  end;
{$ENDIF}
end;

procedure DataToBytes(var S: TBytes; const Data: TTeaData; const HeaderLen: Integer);
{$IFDEF CLR}
var
  TempBytes: TByte4;
  I, M: Integer;
{$ENDIF}
begin
  SetLength(S, (Length(Data) - HeaderLen) * cTeaBlockSize);

{$IFDEF MSWINDOWS}
  Move(Data[HeaderLen], S[0], Length(S));
{$ELSE}
  for I := HeaderLen to Length(Data) - 1 do
  begin
    TempBytes := BitConverter.GetBytes(Data[I]);

    for M := 0 to 3 do
      S[((I - HeaderLen) * cTeaBlockSize) + M] := TempBytes[M];
  end;
{$ENDIF}
end;

function GetBytesFromAnsiString(const Value: TTeaAnsiString): TBytes;
begin
{$IFDEF MSWINDOWS}
  SetLength(Result, Length(Value));
  Move(Value[1], Result[0], Length(Result));
{$ELSE}
  Result := Encoding.ASCII.GetBytes(Value);
{$ENDIF}
end;

function GetBytesFromUnicodeString(const Value: TTeaUnicodeString): TBytes;
begin
{$IFDEF MSWINDOWS}
  Result := GetBytesFromAnsiString(Utf8Encode(Value));
{$ELSE}
  Result := Encoding.UTF8.GetBytes(Value);
{$ENDIF}
end;

function GetUnicodeString(const Value: TBytes): TTeaUnicodeString;
begin
{$IFDEF MSWINDOWS}
  Result := {$IFDEF UNICODE}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(GetAnsiString(Value));
{$ELSE}
  Result := Encoding.UTF8.GetString(Value, 0, Length(Value));
{$ENDIF}
end;

function GetAnsiString(const Value: TBytes): TTeaAnsiString;
begin
{$IFDEF MSWINDOWS}
  SetString(Result, PAnsiChar(@Value[0]), Length(Value));
{$ELSE}
  Result := Encoding.ASCII.GetString(Value, 0, Length(Value));
{$ENDIF}
end;

function XTeaEncryptBytes(const Data, Key: TBytes): TBytes;
var
  KeyBuf: TTeaKey;
  DataBuf: TTeaData;
begin
  BytesToKey(Key, KeyBuf);

  BytesToData(Data, DataBuf, cTeaHeaderSize);
  DoBlockXTeaEncrypt(DataBuf, KeyBuf, cTeaHeaderSize);
  SetSizeHeader(DataBuf, Length(Data));
  DataToBytes(Result, DataBuf, 0);
end;

function XTeaDecryptBytes(const Data, Key: TBytes): TBytes;
var
  KeyBuf: TTeaKey;
  DataBuf: TTeaData;
  DataLen: Int64;
begin
  BytesToKey(Key, KeyBuf);

  BytesToData(Data, DataBuf, 0);
  DoBlockXTeaDecrypt(DataBuf, KeyBuf, cTeaHeaderSize);
  DataLen := GetSizeHeader(DataBuf);
  DataToBytes(Result, DataBuf, cTeaHeaderSize);

  // set data to original size
  SetLength(Result, DataLen);
end;

Type
  TCryptFunc = function(const Data, Key: TBytes): TBytes;

{$IFDEF MSWINDOWS}
procedure XCryptStream(const InStream, OutStream: TStream;
                       const CryptoFunction: TCryptFunc;
                       const CryptoBlock: Cardinal;
                       const Key: TBytes);
var
  BytesRead: Cardinal;
  ByteBuffer: TBytes;
begin
  repeat
    SetLength(ByteBuffer, CryptoBlock);
    BytesRead := InStream.Read(ByteBuffer[0], CryptoBlock);
    SetLength(ByteBuffer, BytesRead);

    if BytesRead > 0 then
    begin
      // call actual encrypt / decrypt function
      ByteBuffer := CryptoFunction(ByteBuffer, Key);
      OutStream.Write(ByteBuffer[0], Length(ByteBuffer));
    end;
  until BytesRead < CryptoBlock;
end;

{$ELSE}

procedure XCryptStream(const InStream, OutStream: Stream;
                       const CryptoFunction: TCryptFunc;
                       const CryptoBlock: Cardinal;
                       const Key: TBytes);
var
  BytesRead: Cardinal;
  ByteBuffer: TBytes;
begin
  InStream.Seek(0, SeekOrigin.&Begin);

  repeat
    SetLength(ByteBuffer, CryptoBlock);
    BytesRead := InStream.Read(ByteBuffer, 0, CryptoBlock);
    SetLength(ByteBuffer, BytesRead);

    if BytesRead > 0 then
    begin
      // call actual encrypt / decrypt function
      ByteBuffer := CryptoFunction(ByteBuffer, Key);
      OutStream.Write(ByteBuffer, 0, Length(ByteBuffer));
    end;
  until BytesRead < CryptoBlock;
end;
{$ENDIF}

procedure XTeaEncryptStream(const InStream, OutStream: TStream; const Key: TBytes);
begin
  XCryptStream(InStream, OutStream, XTeaEncryptBytes, cTeaEncryptBlockSize, Key);
end;

procedure XTeaDecryptStream(const InStream, OutStream: TStream; const Key: TBytes);
begin
  XCryptStream(InStream, OutStream, XTeaDecryptBytes, cTeaDecryptBlockSize, Key);
end;

procedure XTeaEncryptStream(const InStream: TStream; const OutFile: string; const Key: TBytes);
var
  OutStream: TCPFileStream;
begin
  OutStream := TCPFileStream.Create(OutFile, cFileModeCreate);
  try
    XTeaEncryptStream(InStream, OutStream, Key);
  finally
    OutStream.Free;
  end;
end;

procedure XTeaDecryptStream(const InStream: TStream; const OutFile: string; const Key: TBytes);
var
  OutStream: TCPFileStream;
begin
  OutStream := TCPFileStream.Create(OutFile, cFileModeCreate);
  try
    XTeaDecryptStream(InStream, OutStream, Key);
  finally
    OutStream.Free;
  end;
end;

procedure XTeaEncryptFile(const InFile, OutFile: string; const Key: TBytes); overload;
var
  InStream: TCPFileStream;
  OutStream: TCPFileStream;
begin
  InStream := TCPFileStream.Create(InFile, cFileModeRead);
  try
    OutStream := TCPFileStream.Create(OutFile, cFileModeCreate);
    try
      XTeaEncryptStream(InStream, OutStream, Key);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure XTeaDecryptFile(const InFile, OutFile: string; const Key: TBytes); overload;
var
  InStream: TCPFileStream;
  OutStream: TCPFileStream;
begin
  InStream := TCPFileStream.Create(InFile, cFileModeRead);
  try
    OutStream := TCPFileStream.Create(OutFile, cFileModeCreate);
    try
      XTeaDecryptStream(InStream, OutStream, Key);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure XTeaEncryptFile(const InFile: string; const OutStream: TStream; const Key: TBytes); overload;
var
  InStream: TCPFileStream;
begin
  InStream := TCPFileStream.Create(InFile, cFileModeRead);
  try
    XTeaEncryptStream(InStream, OutStream, Key);
  finally
    InStream.Free;
  end;
end;

procedure XTeaDecryptFile(const InFile: string; const OutStream: TStream; const Key: TBytes); overload;
var
  InStream: TCPFileStream;
begin
  InStream := TCPFileStream.Create(InFile, cFileModeRead);
  try
    XTeaDecryptStream(InStream, OutStream, Key);
  finally
    InStream.Free;
  end;
end;

end.

