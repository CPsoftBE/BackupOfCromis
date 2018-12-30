(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * ==================================================================================
 * XTEA Encryption wrapper for the SimpleStorage XML document and values
 * ==================================================================================
 * 16/01/2010 (1.1.0)
 *   - Added direct Encrypt / Decrypt methods
 * 31/07/2010 (1.2.0)
 *   - Initial implementation of element filter
 * 05/11/2010 (1.3.0)
 *   - Merged with the document XTEA filter 
 * 05/11/2010 (1.3.1)
 *   - InitializeHeader must be called on unit initialization
 * ==================================================================================
*)
unit Cromis.SSF.XTEA;

interface

uses
  SysUtils, Classes,

  // omniXML library units
  {$IFNDEF USE_MSXML}OmniXML{$ELSE}MSXML, OmniXML_MSXML{$ENDIF}, OmniXML_Types, OmniXMLUtils,

  // cromis library units
  Cromis.SimpleStorage, Cromis.Streams, Cromis.XTEA;

  // filter data constructor
  function XTEA(const Key: TBytes): ICustomFilterData; overload;
  function XTEA(const Key: XmlString): ICustomFilterData; overload;
  function EncryptedStorage(const Key: TBytes): IDocumentFilterData; overload;
  function EncryptedStorage(const Key: XmlString): IDocumentFilterData; overload;
  function IsEncryptedStorage(const FileName: string): Boolean; overload;
  function IsEncryptedStorage(const Stream: TStream): Boolean; overload;

var
  DefaultKey: XmlString;

implementation

type
  TXTEAFilterData = class(TInterfacedObject, ICustomFilterData)
  private
    FKey: TBytes;
  public
    constructor Create(const Key: TBytes);
    function ValueFilterData(const Node: IXMLNode): IValueFilterData;
    function DocumentFilterData: IDocumentFilterData;
  end;

  // element xtea filter
  TFilterXTEAValue = class(TValueFilterData)
  private
    FKey: TBytes;
  protected
    procedure DoLoadValueAsStream(const Value, Result: TStream); override;
    procedure DoSaveValueAsStream(const Value, Result: TStream); override;
  public
    property Key: TBytes read FKey write FKey;
  end;

  // document xtea filter
  TFilterXTEADocument = class(TDocumentFilterData)
  private
    FKey: TBytes;
  protected
    procedure DirectFilterIn(const SourceStream, TargetStream: TStream); overload; override;
    procedure DirectFilterOut(const SourceStream, TargetStream: TStream); overload; override;
  public
    constructor Create(const Key: TBytes);
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

function XTEA(const Key: TBytes): ICustomFilterData;
begin
  Result := TXTEAFilterData.Create(Key);
end;

function XTEA(const Key: XmlString): ICustomFilterData;
begin
  Result := TXTEAFilterData.Create(GetBytesFromUnicodeString(Key));
end;

function EncryptedStorage(const Key: TBytes): IDocumentFilterData;
begin
  Result := TFilterXTEADocument.Create(Key);
end;

function EncryptedStorage(const Key: XmlString): IDocumentFilterData;
begin
  Result := TFilterXTEADocument.Create(GetBytesFromUnicodeString(Key));
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

{ TFilterXTEAValue }

procedure TFilterXTEAValue.DoLoadValueAsStream(const Value, Result: TStream);
var
  Header: TMagicHeader;
begin
  if Value.Size > 0 then
  begin
    // read the header first from stream
    Value.Read(Header[0], Length(Header));

    // check if this is really encrypted storage
    if not CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader)) then
      raise Exception.Create('This is not a valid encrypted content');

    XTeaDecryptStream(Value, Result, FKey);
    Result.Seek(0, soFromBeginning);
  end;
end;

procedure TFilterXTEAValue.DoSaveValueAsStream(const Value, Result: TStream);
begin
  if Value.Size > 0 then
  begin
    Result.Write(MagicHeader[0], Length(MagicHeader));
    XTeaEncryptStream(Value, Result, FKey);
    Result.Seek(0, soFromBeginning);
  end;
end;

{ TEnyryptedStorage }

constructor TFilterXTEADocument.Create(const Key: TBytes);
begin
  FKey := Key;
end;

procedure TFilterXTEADocument.DirectFilterOut(const SourceStream, TargetStream: TStream);
var
  Header: TMagicHeader;
begin
  // read the header first from stream
  SourceStream.Read(Header[0], Length(Header));

  // check if this is really encrypted storage
  if not CompareMem(@Header[0], @MagicHeader[0], Length(MagicHeader)) then
    raise Exception.Create('This is not a valid encrypted SimpleStorage');

  // decrypt the storage to target stream
  XTeaDecryptStream(SourceStream, TargetStream, FKey);
end;

procedure TFilterXTEADocument.DirectFilterIn(const SourceStream, TargetStream: TStream);
begin
  // write the magic header and encrypt the stream
  TargetStream.Write(MagicHeader[0], Length(MagicHeader));
  XTeaEncryptStream(SourceStream, TargetStream, FKey);
end;

{ TXTEAFilterData }

constructor TXTEAFilterData.Create(const Key: TBytes);
begin
  FKey := Key;
end;

function TXTEAFilterData.DocumentFilterData: IDocumentFilterData;
begin
  Result := TFilterXTEADocument.Create(FKey);
end;

function TXTEAFilterData.ValueFilterData(const Node: IXMLNode): IValueFilterData;
var
  FilterValue: TFilterXTEAValue;
begin
  FilterValue := TFilterXTEAValue.Create(Node);
  FilterValue.Key := FKey;
  Result := FilterValue;
end;

initialization
  DefaultKey := 'CromisXTEAFilter';
  InitializeHeader;
  RegisterFilter('xtea', TFilterXTEAValue);

end.
