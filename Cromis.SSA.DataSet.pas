(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * A simple data storage based on OmniXML.
 * ==================================================================================
 * 25/01/2011 (1.0.1)
 *   - When serializing / deserializing use data type to correctly handle the values
 * 12/06/2012 (1.0.2)
 *   - Timestamp support
 * ==================================================================================
*)
unit Cromis.SSA.DataSet;

interface

uses
  SysUtils, Classes, DB, TypInfo,

  // cromis library units
  Cromis.SimpleStorage;

  // adapter data constructor
  function DataSet: IAdapterData;

implementation

type
  TAdapterDataSet = class(TAdapterData)
  protected
    procedure DoLoadAdapterData(const DataObject: TObject); override;
    procedure DoSaveAdapterData(const DataObject: TObject); override;
  end;

function DataSet: IAdapterData;
begin
  Result := TAdapterDataSet.Create;
end;

{ TAdapterDataSet }

procedure TAdapterDataSet.DoLoadAdapterData(const DataObject: TObject);
var
  I: Integer;
  Value: IElement;
  DataSet: TDataSet;
  TypeAsStr: string;
  NewRecord: IElement;
begin
  DataSet := TDataSet(DataObject);
  DataSet.First;

  while not DataSet.Eof do
  begin
    NewRecord := Element.Append('Record');

    for I := 0 to DataSet.Fields.Count - 1 do
    begin
      if not DataSet.Fields[I].IsNull then
      begin
        Value := NewRecord.Append('Field');

        TypeAsStr := GetEnumName(TypeInfo(TFieldType), Integer(DataSet.Fields[I].DataType));
        Value.EnsureAttr('Name').AsString := DataSet.FieldDefs[I].Name;
        Value.EnsureAttr('Type').AsString := TypeAsStr;

        case DataSet.Fields[I].DataType of
          ftString,
          ftWideString: Value.AsString := DataSet.Fields[I].AsString;
          {$IF CompilerVersion >= 20}
            ftLargeint: Value.AsInt64 := DataSet.Fields[I].AsLargeInt;
          {$ELSE}
            ftLargeint: Value.AsInt64 := DataSet.Fields[I].AsInteger;
          {$IFEND}
          ftWord,
          ftSmallint,
          ftInteger: Value.AsInteger := DataSet.Fields[I].AsInteger;
          ftCurrency,
          ftFloat: Value.AsFloat := DataSet.Fields[I].AsFloat;
          ftBoolean: Value.AsBoolean := DataSet.Fields[I].AsBoolean;
          ftTimeStamp,
          ftDateTime: Value.AsDateTime := DataSet.Fields[I].AsDateTime;
          ftTime: Value.AsTime := DataSet.Fields[I].AsDateTime;
          ftDate: Value.AsDate := DataSet.Fields[I].AsDateTime;
          ftBCD: Value.AsFloat := DataSet.Fields[I].AsFloat;
          else
            raise Exception.CreateFmt('Data type "%s" is not supported!', [TypeAsStr]);
        end;
      end;
    end;

    DataSet.Next;
  end;
end;

procedure TAdapterDataSet.DoSaveAdapterData(const DataObject: TObject);
var
  Name: string;
  Node: IElement;
  Field: IElement;
  DataSet: TDataSet;
  TypeAsStr: string;
begin
  DataSet := TDataSet(DataObject);

  for Node in Element.Nodes do
  begin
    DataSet.Append;

    for Field in Node.Values do
    begin
      Name := Field.GetAttr('Name').AsStringDef;
      TypeAsStr := Field.GetAttr('Type').AsString;

      case TFieldType(GetEnumValue(TypeInfo(TFieldType), TypeAsStr)) of
        ftString,
        ftWideString: DataSet.FieldByName(Name).AsWideString := Field.AsString;
        {$IF CompilerVersion >= 20}
          ftLargeint: DataSet.FieldByName(Name).AsLargeInt := Field.AsInt64;
        {$ELSE}
          ftLargeint: DataSet.FieldByName(Name).AsInteger := Field.AsInt64;
        {$IFEND}
        ftWord,
        ftSmallint,
        ftInteger: DataSet.FieldByName(Name).AsInteger := Field.AsInteger;
        ftCurrency,
        ftFloat: DataSet.FieldByName(Name).AsFloat := Field.AsFloat;
        ftBoolean: DataSet.FieldByName(Name).AsBoolean := Field.AsBoolean;
        ftTimeStamp,
        ftDateTime: DataSet.FieldByName(Name).AsDateTime := Field.AsDateTime;
        ftTime: DataSet.FieldByName(Name).AsDateTime := Field.AsTime;
        ftDate: DataSet.FieldByName(Name).AsDateTime := Field.AsDate;
        ftBCD: DataSet.FieldByName(Name).AsFloat := Field.AsFloat;
        else
          raise Exception.CreateFmt('Data type "%s" is not supported!', [TypeAsStr]);
      end;
    end;

    DataSet.Post;
  end;
end;

initialization
  RegisterAdapter('DataSet', TAdapterDataSet);

end.
