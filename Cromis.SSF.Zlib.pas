(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * ZLIB compression wrapper for the SimpleStorage XML document and values
 * ==================================================================================
 * 28/07/2010 (1.0.0)
 *   - Initial implementation.
 * 30/07/2010 (1.0.1)
 *   - Added GZIP contructor alias
 * 05/11/2010 (1.1.0)
 *   - Merged with document filter
 *   - Renamed GZIP contructor to ZLIB
 * ==================================================================================
*)
unit Cromis.SSF.Zlib;

interface

uses
  SysUtils, Classes, Zlib,

  // OmniXML
  OmniXML, OmniXML_Types,

  // cromis units
  Cromis.SimpleStorage;

  function ZLIB: ICustomFilterData;
  function CompressedStorage: IDocumentFilterData;
  function IsCompressedStorage(const FileName: string): Boolean; overload;
  function IsCompressedStorage(const Stream: TStream): Boolean; overload;

implementation

type
  TZLibFilterData = class(TInterfacedObject, ICustomFilterData)
  public
    function ValueFilterData(const Node: IXMLNode): IValueFilterData;
    function DocumentFilterData: IDocumentFilterData;
  end;

  // element zlib filter
  TFilterZLibValue = class(TValueFilterData)
  protected
    procedure DoLoadValueAsStream(const Value, Result: TStream); override;
    procedure DoSaveValueAsStream(const Value, Result: TStream); override;
  end;

  // document zlib filter
  TFilterZLibDocument = class(TDocumentFilterData)
  public
    procedure DirectFilterIn(const SourceStream, TargetStream: TStream); overload; override;
    procedure DirectFilterOut(const SourceStream, TargetStream: TStream); overload; override;
  end;

type
  TMagicHeader = array [0..3] of Byte;

var
  MagicHeader: TMagicHeader;

procedure InitializeHeader;
begin
  MagicHeader[0] := $50;
  MagicHeader[1] := $4B;
  MagicHeader[2] := $03;
  MagicHeader[3] := $04;
end;

function ZLIB: ICustomFilterData;
begin
  Result := TZLibFilterData.Create;
end;

function CompressedStorage: IDocumentFilterData;
begin
  Result := TFilterZLibDocument.Create
end;

function IsCompressedStorage(const FileName: string): Boolean;
var
  InputStream: TFileStream;
begin
  InputStream :=  TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsCompressedStorage(InputStream);
  finally
    InputStream.Free;
  end;
end;

function IsCompressedStorage(const Stream: TStream): Boolean;
var
  Header: TMagicHeader;
begin
  Stream.Read(Header[0], Length(Header));
  Result := CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader));
end;

procedure ZlibCompress(const SourceStream, TargetStream: TStream);
const
  BufferSize = 4096;
var
  Count: Integer;
  ZStream: TCompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  ZStream := TCompressionStream.Create(clMax, TargetStream);
  try
    while True do
    begin
      Count := SourceStream.Read(Buffer[0], BufferSize);

      if Count <> 0 then
        ZStream.Write(Buffer[0], Count)
      else
        Break;
    end;
  finally
    ZStream.Free;
  end;
end;

procedure ZlibDecompress(const SourceStream, TargetStream: TStream);
const
  BufferSize = 4096;
var
  Count: Integer;
  ZStream: TDecompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  ZStream := TDecompressionStream.Create(SourceStream);
  try
    while True do
    begin
      Count := ZStream.Read(Buffer[0], BufferSize);

      if Count <> 0 then
        TargetStream.WriteBuffer(Buffer[0], Count)
      else
        Break;
    end;
  finally
    ZStream.Free;
  end;
end;

{ TFilterZLibDocument }

procedure TFilterZLibDocument.DirectFilterIn(const SourceStream, TargetStream: TStream);
begin
  ZlibCompress(SourceStream, TargetStream);
end;

procedure TFilterZLibDocument.DirectFilterOut(const SourceStream, TargetStream: TStream);
begin
  ZlibDecompress(SourceStream, TargetStream);
end;

{ TFilterZLibValue }

procedure TFilterZLibValue.DoLoadValueAsStream(const Value, Result: TStream);
begin
  ZlibDecompress(Value, Result);
end;

procedure TFilterZLibValue.DoSaveValueAsStream(const Value, Result: TStream);
begin
  ZlibCompress(Value, Result);
end;

{ TZLibFilterData }

function TZLibFilterData.DocumentFilterData: IDocumentFilterData;
begin
  Result := TFilterZLibDocument.Create;
end;

function TZLibFilterData.ValueFilterData(const Node: IXMLNode): IValueFilterData;
begin
  Result := TFilterZLibValue.Create(Node);
end;

initialization
  RegisterFilter('zlib', TFilterZLibValue);

end.
