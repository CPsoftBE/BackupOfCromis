(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * =============================================================================
 * Stream support classes and routines, to make working with streams easier
 * =============================================================================
 * 25/10/2009 (1.1.0)
 *   - Added TStreamStorage, a class that handles name / value pairs in stream
 * 25/10/2009 (1.1.1)
 *   - D2010 compatible
 * 09/11/2009 (1.2.0)
 *   - Use hash table internally to fasten the data reading from stream
 *   - Do not allow two or more values with same name
 * 15/11/2009 (1.2.1)
 *   - Bugfixes
 * 28/11/2009 (1.2.2)
 *   - TStreamStorage WriteCardinal / ReadCardinal
 * 29/11/2009 (1.2.3)
 *   - Exists function added. Checks if field with a name exists
 *   - ReadStream changed from function to procedure for safety
 * 27/03/2010 (1.2.4)
 *   - Names enumerator for the TStreamStorage
 * 27/05/2011 (1.2.5)
 *   - IgnoreDuplicates support for TStreamStorage
 * 14/03/2012 (1.2.6)
 *   - Use BOM constants from Cromis.Unicode
 * 01/04/2012 (1.3.0)
 *   - IMessageData interface added
 * 22/05/2012 (1.3.1)
 *   - AcquireMessageData added
 * 16/01/2013 (1.3.2)
 *   - ReadStream overload that allows to read from stream without temp stream
 * =============================================================================
*)
unit Cromis.Streams;

interface

uses
  SysUtils, Classes, Math, IniFiles,

  // unicode types
  Cromis.Unicode, Cromis.Streams.Filters;

procedure WriteToStreamAsString(const Stream: TStream; const Content: string);
procedure WriteToStreamAsUnicode(const Stream: TStream; const Content: ustring);
procedure WriteToStreamAsUTF8(const Stream: TStream; const Content: ustring); overload;
procedure WriteToStreamAsUTF8(const Stream: TStream; const Content: astring); overload;

function ReadFromStreamAsUnicode(const Stream: TStream): ustring;
function ReadFromStreamAsString(const Stream: TStream): string;
function ReadFromStreamAsUTF8(const Stream: TStream): astring;

const
  cDefaultSearchSize = 50;

type
  TNamesEnumerator = class
  private
    FFieldName: ustring;
    FStorage: TMemoryStream;
    FOriginalPosition: Int64;
  public
    destructor Destroy; override;
    constructor Create(const Storage: TMemoryStream);
    function GetCurrent: ustring;
    function MoveNext: Boolean;
    property Current: ustring read GetCurrent;
  end;

  IStorageStream = Interface(IInterface)
  ['{49D53510-E7F6-452B-B12A-826AA7AC00B6}']
    function _GetData: TStream;
    property Data: TStream read _GetData;
  end;

  TStreamStorage = class
  strict private
    FLastPos: Int64;
    FStorage: TMemoryStream;
    FPositionsHash: TStringHash;
    FIgnoreDuplicates: Boolean;
    function GetStorage: TMemoryStream;
    procedure WriteHeaders(const Name: ustring; const DataLength: Int64);
    function FindNamedPosition(const Name: ustring; var ValueSize: Int64): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const StreamStorage: TStreamStorage);
    procedure ApplyInputFilters(const FilterList: TStreamFilters);
    function ApplyOutputFilters(const FilterList: TStreamFilters): IUsedFilters;
    // stream writing procedures (name and value pairs)
    procedure WriteUnicodeString(const Name: ustring; const Value: ustring);
    procedure WriteUTF8String(const Name: ustring; const Value: astring);
    procedure WriteDateTime(const Name: ustring; const Value: TDateTime);
    procedure WriteCardinal(const Name: ustring; const Value: Cardinal);
    procedure WriteInteger(const Name: ustring; const Value: Integer);
    procedure WriteBoolean(const Name: ustring; const Value: Boolean);
    procedure WriteStream(const Name: ustring; const Value: TStream);
    procedure WriteString(const Name: ustring; const Value: ustring);
    procedure WriteReal(const Name: ustring; const Value: Real);
    // stream reading functions (name and value pairs)
    procedure ReadStream(const Name: ustring; const Stream: TStream); overload;
    function ReadStream(const Name: ustring): IStorageStream; overload;
    function ReadUnicodeString(const Name: ustring): ustring;
    function ReadUTF8String(const Name: ustring): astring;
    function ReadDateTime(const Name: ustring): TDateTime;
    function ReadCardinal(const Name: ustring): Cardinal;
    function ReadInteger(const Name: ustring): Integer;
    function ReadBoolean(const Name: ustring): Boolean;
    function ReadString(const Name: ustring): ustring;
    function ReadReal(const Name: ustring): Real;
    // get the names enumerator to go through names
    function GetEnumerator: TNamesEnumerator;
    // misc procedures  and properties
    property IgnoreDuplicates: Boolean read FIgnoreDuplicates write FIgnoreDuplicates;
    property Storage: TMemoryStream read GetStorage;
    function Exists(const Name: ustring): Boolean;
    function IsFiltered: Boolean;
    procedure Clear;
  end;

implementation

type
  TStorageStream = class(TInterfacedObject, IStorageStream)
  private
    FMemoryStream: TMemoryStream;
    function _GetData: TStream;
  public
    constructor Create(const Source: TStream; const Count: Int64);
    destructor Destroy; override;
    property Data: TStream read _GetData;
  end;

function GetHashName(const Name: ustring):{$IFNDEF UNICODE}UTF8String{$ELSE}ustring{$ENDIF};
begin
{$IFNDEF UNICODE}
  Result := UTF8Encode(Name);
{$ELSE}
  Result := Name;
{$ENDIF}
end;

procedure WriteToStreamAsString(const Stream: TStream; const Content: string);
begin
  if Length(Content) > 0 then
    Stream.Write(Content[1], Length(Content) * SizeOf(Char));
end;

procedure WriteToStreamAsUnicode(const Stream: TStream; const Content: ustring);
begin
  if Length(Content) > 0 then
    Stream.Write(Content[1], Length(Content) * SizeOf(uchar));
end;

procedure WriteToStreamAsUTF8(const Stream: TStream; const Content: ustring);
var
  UTF8String: astring;
begin
  if Length(Content) > 0 then
  begin
    UTF8String := UTF8Encode(Content);
    Stream.Write(UTF8String[1], Length(UTF8String) * SizeOf(achar));
  end;
end;

procedure WriteToStreamAsUTF8(const Stream: TStream; const Content: astring); overload;
begin
  if Length(Content) > 0 then
    Stream.Write(Content[1], Length(Content) * SizeOf(achar));
end;

function ReadFromStreamAsUnicode(const Stream: TStream): ustring;
var
  BOM: array [0..2] of Byte;
  ReadSize: Int64;
begin
  Result := '';

  if Stream.Size > 0 then
  begin
    Stream.Seek(0, soFromBeginning);
    ReadSize := Stream.Size;

    if Stream.Size >= 2  then
    begin
      Stream.Read(BOM[0], Length(Utf16BEBOM));

      if not CompareMem(@BOM, @Utf16BEBOM, Length(Utf16BEBOM)) and
         not CompareMem(@BOM, @Utf16LEBOM, Length(Utf16LEBOM)) then
        Stream.Seek(0, soFromBeginning)
      else
        ReadSize := ReadSize - 4;
    end;

    SetLength(Result, ReadSize div SizeOf(uchar));
    Stream.Read(Result[1], ReadSize);
  end;
end;

function ReadFromStreamAsString(const Stream: TStream): string;
begin
  Result := '';

  if Stream.Size > 0 then
  begin
    Stream.Seek(0, soFromBeginning);
    SetLength(Result, Stream.Size div SizeOf(Char));
    Stream.Read(Result[1], Stream.Size);
  end;
end;

function ReadFromStreamAsUTF8(const Stream: TStream): astring;
var
  BOM: array [0..2] of Byte;
  ReadSize: Int64;
begin
  Result := '';

  if Stream.Size > 0 then
  begin
    Stream.Seek(0, soFromBeginning);
    ReadSize := Stream.Size;

    if Stream.Size >=3  then
    begin
      Stream.Read(BOM[0], Length(Utf8BOM));

      if not CompareMem(@BOM, @Utf8BOM, Length(Utf8BOM)) then
        Stream.Seek(0, soFromBeginning)
      else
        ReadSize := ReadSize - 3;
    end;

    SetLength(Result, ReadSize div SizeOf(achar));
    Stream.Read(Result[1], ReadSize);
  end;
end;

{ TStreamStorage }

constructor TStreamStorage.Create;
begin
  FStorage := TMemoryStream.Create;
  FPositionsHash := TStringHash.Create(cDefaultSearchSize);
end;

destructor TStreamStorage.Destroy;
begin
  FreeAndNil(FPositionsHash);
  FreeAndNil(FStorage);

  inherited;
end;

function TStreamStorage.Exists(const Name: ustring): Boolean;
var
  ValueSize: Int64;
begin
  Result := FindNamedPosition(Name, ValueSize);
end;

procedure TStreamStorage.ApplyInputFilters(const FilterList: TStreamFilters);
begin
  FilterList.ApplyInput(FStorage, FStorage);
end;

function TStreamStorage.ApplyOutputFilters(const FilterList: TStreamFilters): IUsedFilters;
begin
  Result := FilterList.ApplyOutput(FStorage, FStorage);
end;

procedure TStreamStorage.Assign(const StreamStorage: TStreamStorage);
begin
  FPositionsHash.Clear;
  FStorage.Clear;
  FLastPos := 0;

  // copy the whole content over
  StreamStorage.Storage.Position := 0;
  FStorage.CopyFrom(StreamStorage.Storage, 0);
end;

procedure TStreamStorage.Clear;
begin
  FPositionsHash.Clear;
  FStorage.Clear;
  FLastPos := 0;
end;

function TStreamStorage.FindNamedPosition(const Name: ustring; var ValueSize: Int64): Boolean;
var
  NameSize: Integer;
  FieldPos: Integer;
  FieldName: ustring;
begin
  FieldPos := FPositionsHash.ValueOf(GetHashName(Name));

  if FieldPos > -1 then
  begin
    FStorage.Position := FieldPos;
    FStorage.Read(ValueSize, SizeOf(Int64));
    Result := True;
    Exit;
  end
  else
  begin
    FStorage.Position := FLastPos;
    Result := False;

    while FStorage.Position < FStorage.Size do
    begin
      FStorage.Read(NameSize, SizeOf(Integer));
      SetLength(FieldName, NameSize div SizeOf(uchar));
      FStorage.Read(FieldName[1], NameSize);

      // always add name to the hash and set the last read position
      FPositionsHash.Add(GetHashName(FieldName), FStorage.Position);

      // check for match
      if Name = FieldName then
      begin
        FStorage.Read(ValueSize, SizeOf(Int64));
        Result := True;
        Exit;
      end
      else
      begin
        FStorage.Read(ValueSize, SizeOf(Int64));
        FStorage.Position := FStorage.Position + ValueSize;
      end;
    end;
  end;
end;

function TStreamStorage.GetEnumerator: TNamesEnumerator;
begin
  Result := TNamesEnumerator.Create(FStorage);
end;

function TStreamStorage.GetStorage: TMemoryStream;
begin
  Result := FStorage;
end;

function TStreamStorage.IsFiltered: Boolean;
begin
  Result := Cromis.Streams.Filters.IsFiltered(FStorage);
end;

procedure TStreamStorage.WriteHeaders(const Name: ustring; const DataLength: Int64);
var
  NameSize: Integer;
begin
  if (FPositionsHash.ValueOf(GetHashName(Name)) > -1) and not FIgnoreDuplicates then
    raise Exception.CreateFmt('Value with name "%s" already exists', [GetHashName(Name)]);

  NameSize := Length(Name) * SizeOf(uchar);

  FStorage.Position := FStorage.Size;
  FStorage.Write(NameSize, SizeOf(Integer));
  WriteToStreamAsUnicode(FStorage, Name);

  // write the position to hash and write actual value to stream
  FPositionsHash.Add(GetHashName(Name), FStorage.Position);
  FStorage.Write(DataLength, SizeOf(Int64));
end;

function TStreamStorage.ReadBoolean(const Name: ustring): Boolean;
var
  ValueSize: Int64;
begin
  case FindNamedPosition(Name, ValueSize) of
    True: FStorage.Read(Result, ValueSize);
    False: Result := False;
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadCardinal(const Name: ustring): Cardinal;
var
  ValueSize: Int64;
begin
  case FindNamedPosition(Name, ValueSize) of
    True: FStorage.Read(Result, ValueSize);
    False: Result := 0;
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadDateTime(const Name: ustring): TDateTime;
var
  ValueSize: Int64;
begin
  case FindNamedPosition(Name, ValueSize) of
    True: FStorage.Read(Result, ValueSize);
    False: Result := 0;
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadInteger(const Name: ustring): Integer;
var
  ValueSize: Int64;
begin
  case FindNamedPosition(Name, ValueSize) of
    True: FStorage.Read(Result, ValueSize);
    False: Result := 0;
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadReal(const Name: ustring): Real;
var
  ValueSize: Int64;
begin
  case FindNamedPosition(Name, ValueSize) of
    True: FStorage.Read(Result, ValueSize);
    False: Result := 0;
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

procedure TStreamStorage.ReadStream(const Name: ustring; const Stream: TStream);
var
  ValueSize: Int64;
begin
  if FindNamedPosition(Name, ValueSize) then
    Stream.CopyFrom(FStorage, ValueSize);

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadStream(const Name: ustring): IStorageStream;
var
  ValueSize: Int64;
begin
  if FindNamedPosition(Name, ValueSize) then
    Result := TStorageStream.Create(FStorage, ValueSize)
  else
    Result := TStorageStream.Create(nil, 0);

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadString(const Name: ustring): ustring;
begin
  Result := ReadUnicodeString(Name);
end;

function TStreamStorage.ReadUnicodeString(const Name: ustring): ustring;
var
  ValueSize: Int64;
begin
  Result := '';

  if FindNamedPosition(Name, ValueSize) then
  begin
    SetLength(Result, ValueSize div SizeOf(uchar));
    FStorage.Read(Result[1], ValueSize);
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

function TStreamStorage.ReadUTF8String(const Name: ustring): astring;
var
  ValueSize: Int64;
begin
  Result := '';

  if FindNamedPosition(Name, ValueSize) then
  begin
    SetLength(Result, ValueSize div SizeOf(achar));
    FStorage.Read(Result[1], ValueSize);
  end;

  // update last position if we advanced
  FLastPos := Max(FStorage.Position, FLastPos);
end;

procedure TStreamStorage.WriteBoolean(const Name: ustring; const Value: Boolean);
begin
  if Name <> '' then
  begin
    WriteHeaders(Name, SizeOf(Boolean));
    FStorage.Write(Value, SizeOf(Boolean));
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteCardinal(const Name: ustring; const Value: Cardinal);
begin
  if Name <> '' then
  begin
    WriteHeaders(Name, SizeOf(Cardinal));
    FStorage.Write(Value, SizeOf(Cardinal));
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteDateTime(const Name: ustring; const Value: TDateTime);
begin
  if Name <> '' then
  begin
    WriteHeaders(Name, SizeOf(TDateTime));
    FStorage.Write(Value, SizeOf(TDateTime));
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteInteger(const Name: ustring; const Value: Integer);
begin
  if Name <> '' then
  begin
    WriteHeaders(Name, SizeOf(Integer));
    FStorage.Write(Value, SizeOf(Integer));
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteReal(const Name: ustring; const Value: Real);
begin
  if Name <> '' then
  begin
    WriteHeaders(Name, SizeOf(Real));
    FStorage.Write(Value, SizeOf(Real));
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteStream(const Name: ustring; const Value: TStream);
begin
  if (Name <> '') and (Value.Size > 0) then
  begin
    WriteHeaders(Name, Value.Size);
    FStorage.CopyFrom(Value, 0);
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteString(const Name: ustring; const Value: ustring);
begin
  WriteUnicodeString(Name, Value);
end;

procedure TStreamStorage.WriteUnicodeString(const Name: ustring; const Value: ustring);
var
  ValueSize: Int64;
begin
  if (Value <> '') and (Name <> '') then
  begin
    ValueSize := Length(Value) * SizeOf(uchar);
    WriteHeaders(Name, ValueSize);
    FStorage.Write(Value[1], ValueSize);
    FLastPos := FStorage.Position;
  end;
end;

procedure TStreamStorage.WriteUTF8String(const Name: ustring; const Value: astring);
var
  ValueSize: Int64;
begin
  if (Value <> '') and (Name <> '') then
  begin
    ValueSize := Length(Value) * SizeOf(achar);
    WriteHeaders(Name, ValueSize);
    FStorage.Write(Value[1], ValueSize);
    FLastPos := FStorage.Position;
  end;
end;

{ TNamesEnumerator }

constructor TNamesEnumerator.Create(const Storage: TMemoryStream);
begin
  FOriginalPosition := Storage.Position;
  Storage.Position := 0;
  FStorage := Storage;
end;

destructor TNamesEnumerator.Destroy;
begin
  FStorage.Position := FOriginalPosition;

  inherited;
end;

function TNamesEnumerator.GetCurrent: ustring;
begin
  Result := FFieldName;
end;

function TNamesEnumerator.MoveNext: Boolean;
var
  Moved: Boolean;
  NameSize: Integer;
  ValueSize: Int64;
begin
  Moved := False;
  try
    if FStorage.Position < FStorage.Size then
    begin
      FStorage.Read(NameSize, SizeOf(Integer));
      SetLength(FFieldName, NameSize div SizeOf(uchar));
      FStorage.Read(FFieldName[1], NameSize);

      FStorage.Read(ValueSize, SizeOf(Int64));
      FStorage.Position := FStorage.Position + ValueSize;

      // success
      Moved := True;
    end;
  finally
    Result := Moved;
  end;
end;

{ TStorageStream }

constructor TStorageStream.Create(const Source: TStream; const Count: Int64);
begin
  FMemoryStream := TMemoryStream.Create;

  if Source <> nil then
    FMemoryStream.CopyFrom(Source, Count);
end;

destructor TStorageStream.Destroy;
begin
  FreeAndNil(FMemoryStream);

  inherited;
end;

function TStorageStream._GetData: TStream;
begin
  FMemoryStream.Seek(0, soFromBeginning);
  // return the stream
  Result := FMemoryStream;
end;

end.
