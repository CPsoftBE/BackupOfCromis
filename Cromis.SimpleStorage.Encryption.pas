(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2009 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * A simple long interval scheduler. Good for scheduling tasks over long
 * periods of time. Low resolution but almost no resource usage.
 * ==================================================================================
 * 12/01/2010 (1.0.0)
 *   - Initial implementation.
 * 16/01/2010 (1.1.0)
 *   - Added direct Encrypt / Decrypt methods
 * ==================================================================================
*)
unit Cromis.SimpleStorage.Encryption;

interface

uses
  SysUtils, Classes,

  // OmniXML
  OmniXML_Types,

  // cromis units
  Cromis.SimpleStorage, Cromis.XTEA;

type
  IEncryptedSimpleStorage = interface(IInterface)
  ['{918930D7-14C0-4E24-A33A-A8AF0A6833DC}']
    function LoadFromFile(const FileName: string): ISimpleStorage;
    function LoadFromStream(const Stream: TStream): ISimpleStorage;
    procedure SaveToFile(const Storage: ISimpleStorage; const FileName: string);
    procedure SaveToStream(const Storage: ISimpleStorage; const Stream: TStream);
    procedure DirectEncrypt(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectDecrypt(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectEncrypt(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectDecrypt(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectEncrypt(const SourceStream, TargetStream: TStream); overload;
    procedure DirectDecrypt(const SourceStream, TargetStream: TStream); overload;
    procedure DirectEncrypt(const SourceFile, TargetFile: string); overload;
    procedure DirectDecrypt(const SourceFile, TargetFile: string); overload;
  end;

  function EncryptedStorage(const Key: TBytes): IEncryptedSimpleStorage; overload;
  function EncryptedStorage(const Key: XmlString): IEncryptedSimpleStorage; overload;
  function IsEncryptedStorage(const FileName: string): Boolean; overload;
  function IsEncryptedStorage(const Stream: TStream): Boolean; overload;

implementation

type
  TEncryptedSimpleStorage = class(TInterfacedObject, IEncryptedSimpleStorage)
  private
    FKey: TBytes;
  public
    constructor Create(const Key: TBytes);
    function LoadFromFile(const FileName: string): ISimpleStorage;
    function LoadFromStream(const Stream: TStream): ISimpleStorage;
    procedure SaveToFile(const Storage: ISimpleStorage; const FileName: string);
    procedure SaveToStream(const Storage: ISimpleStorage; const Stream: TStream);
    procedure DirectEncrypt(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectDecrypt(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectEncrypt(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectDecrypt(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectEncrypt(const SourceStream, TargetStream: TStream); overload;
    procedure DirectDecrypt(const SourceStream, TargetStream: TStream); overload;
    procedure DirectEncrypt(const SourceFile, TargetFile: string); overload;
    procedure DirectDecrypt(const SourceFile, TargetFile: string); overload;
  end;

type
  TMagicHeader = array [0..16] of Byte;

var
  MagicHeader: TMagicHeader;

procedure InitializeHeader;
begin
  MagicHeader[0] := Ord('<');
  MagicHeader[1] := Ord('E');
  MagicHeader[2] := Ord('X');
  MagicHeader[3] := Ord('M');
  MagicHeader[4] := Ord('L');
  MagicHeader[5] := Ord('/');
  MagicHeader[6] := Ord('>');
  MagicHeader[7] := 26;
  MagicHeader[8] := 1;
  MagicHeader[9] := 2;
  MagicHeader[10] := 3;
  MagicHeader[11] := 4;
  MagicHeader[12] := 5;
  MagicHeader[13] := 6;
  MagicHeader[14] := 7;
  MagicHeader[15] := 8;
  MagicHeader[16] := 9;
end;

function EncryptedStorage(const Key: TBytes): IEncryptedSimpleStorage;
begin
  Result := TEncryptedSimpleStorage.Create(Key);
end;

function EncryptedStorage(const Key: XmlString): IEncryptedSimpleStorage;
begin
  Result := TEncryptedSimpleStorage.Create(GetBytesFromUnicodeString(Key));
end;

function IsEncryptedStorage(const FileName: string): Boolean;
var
  InputStream: TFileStream;
begin
  InputStream :=  TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsEncryptedStorage(InputStream);
  finally
    InputStream.Free;
  end;
end;

function IsEncryptedStorage(const Stream: TStream): Boolean;
var
  Header: TMagicHeader;
begin
  Stream.Read(Header[0], Length(Header));
  Result := CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader));
end;

{ TEnyryptedStorage }

constructor TEncryptedSimpleStorage.Create(const Key: TBytes);
begin
  FKey := Key;
end;

procedure TEncryptedSimpleStorage.DirectDecrypt(const SourceFile, TargetFile: string);
var
  SourceStream: TFileStream;
  TargetStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
    try
      DirectDecrypt(SourceStream, TargetStream);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.DirectDecrypt(const SourceStream, TargetStream: TStream);
var
  Header: TMagicHeader;
begin
  SourceStream.Read(Header[0], Length(Header));

  if not CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader)) then
    raise Exception.Create('This is not a valid encrypted SimpleStorage');

  XTeaDecryptStream(SourceStream, TargetStream, FKey);
end;

procedure TEncryptedSimpleStorage.DirectDecrypt(const SourceFile: string; const TargetStream: TStream);
var
  SourceStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DirectDecrypt(SourceStream, TargetStream);
  finally
    SourceStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.DirectDecrypt(const SourceStream: TStream; const TargetFile: string);
var
  TargetStream: TFileStream;
begin
  TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
  try
    DirectDecrypt(SourceStream, TargetStream);
  finally
    TargetStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.DirectEncrypt(const SourceStream: TStream; const TargetFile: string);
var
  TargetStream: TFileStream;
begin
  TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
  try
    DirectEncrypt(SourceStream, TargetStream);
  finally
    TargetStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.DirectEncrypt(const SourceStream, TargetStream: TStream);
begin
  TargetStream.Write(MagicHeader[0], Length(MagicHeader));
  XTeaEncryptStream(SourceStream, TargetStream, FKey);
end;

procedure TEncryptedSimpleStorage.DirectEncrypt(const SourceFile: string; const TargetStream: TStream);
var
  SourceStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DirectEncrypt(SourceStream, TargetStream);
  finally
    SourceStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.DirectEncrypt(const SourceFile, TargetFile: string);
var
  SourceStream: TFileStream;
  TargetStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
    try
      DirectEncrypt(SourceStream, TargetStream);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

function TEncryptedSimpleStorage.LoadFromFile(const FileName: string): ISimpleStorage;
var
  InputStream: TFileStream;
begin
  InputStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(InputStream);
  finally
    InputStream.Free;
  end;
end;

function TEncryptedSimpleStorage.LoadFromStream(const Stream: TStream): ISimpleStorage;
var
  Header: TMagicHeader;
  OutputStream: TMemoryStream;
begin
  OutputStream := TMemoryStream.Create;
  try
    Stream.Read(Header[0], Length(Header));

    if not CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader)) then
      raise Exception.Create('This is not a valid encrypted SimpleStorage');

    XTeaDecryptStream(Stream, OutputStream, FKey);
    OutputStream.Seek(0, soFromBeginning);

    Result := StorageFromStream(OutputStream);
  finally
    OutputStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.SaveToFile(const Storage: ISimpleStorage; const FileName: string);
var
  OutputStream: TFileStream;
begin
  OutputStream :=  TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Storage, OutputStream);
  finally
    OutputStream.Free;
  end;
end;

procedure TEncryptedSimpleStorage.SaveToStream(const Storage: ISimpleStorage; const Stream: TStream);
var
  InputStream: TMemoryStream;
begin
  InputStream := TMemoryStream.Create;
  try
    Storage.SaveToStream(InputStream);
    InputStream.Seek(0, soFromBeginning);
    Stream.Write(MagicHeader[0], Length(MagicHeader));
    XTeaEncryptStream(InputStream, Stream, FKey);
  finally
    InputStream.Free;
  end;
end;

initialization
  InitializeHeader;

end.
