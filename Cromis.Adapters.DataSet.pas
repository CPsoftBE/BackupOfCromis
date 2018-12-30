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
*)
unit Cromis.Adapters.DataSet;

interface

uses
  SysUtils, Classes, DB,

  // cromis library units
  Cromis.SimpleStorage;

implementation

procedure LoadAdapterDataAsDataset(const Element: IElement; const DataObject: TObject);
var
  I: Integer;
  RecName: string;
  DataSet: TDataSet;
  NewRecord: IElement;
begin
  DataSet := TDataSet(DataObject);
  DataSet.First;

  while not DataSet.Eof do
  begin
    NewRecord := Element.Append('Record');

    for I := 0 to DataSet.Fields.Count - 1 do
    begin
      RecName := Format('Field[@Name="%s"]', [DataSet.FieldDefs[I].Name]);
      NewRecord.Append(RecName).AsString := DataSet.Fields[I].Value;
    end;

    DataSet.Next;
  end;
end;

procedure SaveAdapterDataAsDataset(const Element: IElement; const DataObject: TObject);
var
  Node: IElement;
  Field: IElement;
  DataSet: TDataSet;
begin
  DataSet := TDataSet(DataObject);

  for Node in Element.Nodes do
  begin
    DataSet.Append;

    for Field in Node.Values do
      DataSet.FieldByName(Field.Attributes.Get('Name').AsString).AsVariant := Field.AsString;

    DataSet.Post;
  end;
end;

function GetDataSetAdapterData(const Element: IElement): TAdapterData;
begin
  Result.LoadAdapterData := @LoadAdapterDataAsDataset;
  Result.SaveAdapterData := @SaveAdapterDataAsDataset;
  Result.Element := Element;
end;

initialization
  RegisterAdapter('DataSet', @GetDataSetAdapterData);

end.
