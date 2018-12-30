(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2014 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * ValueList container with variant like support
 * ==================================================================================
 * 22/02/2014 (1.0.0)
 *   - Initial release
 * ==================================================================================
 *)
unit Cromis.ValueList;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Variants;

type
  IAnyValue = Interface(IInterface)
  ['{9D866D8B-6FEC-4633-B968-AF8677AF6B40}']
    function GetName: string;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsDouble: Double;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function ValueType: Integer;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsException: Exception;
    function GetAsInterface: IInterface;
  {$IFDEF UNICODE}
    function GetAsAnsiString: AnsiString;
  {$ENDIF}
    function GetAsWideString: WideString;
    procedure SetName(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsException(const Value: Exception);
    procedure SetAsInterface(const Value: IInterface);
  {$IFDEF UNICODE}
    procedure SetAsAnsiString(const Value: AnsiString);
  {$ENDIF}
    procedure SetAsWideString(const Value: WideString);
    property Name: string read GetName write SetName;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsException: Exception read GetAsException write SetAsException;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
  {$IFDEF UNICODE}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  IValueList = Interface(IInterface)
  ['{54B01683-17B0-4719-B620-48FDF31BC574}']
    function GetCount: Integer;
    function GetItems(const Index: Integer): IAnyValue;
    function Get(const Name: string): IAnyValue;
    function Remove(const Name: string): Boolean;
    function Exists(const Name: string): Boolean;
    function Ensure(const Name: string): IAnyValue;
    property Items[const Index: Integer]: IAnyValue read GetItems;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

  // the acquire function for value list
  function AcquireValueList: IValueList;
  function AcquireAnyValue: IAnyValue;

implementation

type
  TAnyValueObject = class(TInterfacedObject, IAnyValue)
  private
    FName: string;
    FValue: Variant;
    function GetName: string;
    function GetAsInt64: Int64;
    function GetAsFloat: Extended;
    function GetAsDouble: Double;
    function GetAsString: string;
    function GetAsObject: TObject;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsPointer: Pointer;
    function GetAsCardinal: Cardinal;
    function GetAsDateTime: TDateTime;
    function GetAsException: Exception;
    function GetAsInterface: IInterface;
  {$IFDEF UNICODE}
    function GetAsAnsiString: AnsiString;
  {$ENDIF}
    function GetAsWideString: WideString;
    procedure SetName(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsException(const Value: Exception);
    procedure SetAsInterface(const Value: IInterface);
  {$IFDEF UNICODE}
    procedure SetAsAnsiString(const Value: AnsiString);
  {$ENDIF}
    procedure SetAsWideString(const Value: WideString);
  public
    constructor Create;
    function ValueType: Integer;
    property Name: string read GetName write SetName;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsException: Exception read GetAsException write SetAsException;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
  {$IFDEF UNICODE}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  TValueList = class(TInterfacedObject, IValueList)
  private
    FValuesList: TInterfaceList;
    function GetItems(const Index: Integer): IAnyValue;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const Name: string): IAnyValue;
    function Remove(const Name: string): Boolean;
    function Exists(const Name: string): Boolean;
    function Ensure(const Name: string): IAnyValue;
    property Items[const Index: Integer]: IAnyValue read GetItems;
    property Count: Integer read GetCount;
    procedure Clear;
  end;

function AcquireValueList: IValueList;
begin
  Result := TValueList.Create;
end;

function AcquireAnyValue: IAnyValue;
begin
  Result := TAnyValueObject.Create;
end;

{ TAnyValueObject }

constructor TAnyValueObject.Create;
begin
  FValue := Null;
end;

{$IFDEF UNICODE}
function TAnyValueObject.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(FValue);
end;
{$ENDIF}

function TAnyValueObject.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsCardinal: Cardinal;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsDouble: Double;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsException: Exception;
begin
  Result := Exception(Pointer(Integer(FValue)));
end;

function TAnyValueObject.GetAsFloat: Extended;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsInterface: IInterface;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsObject: TObject;
begin
  Result := Pointer(Integer(FValue));
end;

function TAnyValueObject.GetAsPointer: Pointer;
begin
  Result := Pointer(Integer(FValue));
end;

function TAnyValueObject.GetAsString: string;
begin
  Result := FValue;
end;

function TAnyValueObject.GetAsWideString: WideString;
begin
  Result := FValue;
end;

function TAnyValueObject.GetName: string;
begin
  Result := FName;
end;

{$IFDEF UNICODE}
procedure TAnyValueObject.SetAsAnsiString(const Value: AnsiString);
begin
  FValue := Value;
end;
{$ENDIF}

procedure TAnyValueObject.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsCardinal(const Value: Cardinal);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsDouble(const Value: Double);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsException(const Value: Exception);
begin
  FValue := Integer(Pointer(Value));
end;

procedure TAnyValueObject.SetAsFloat(const Value: Extended);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsInt64(const Value: Int64);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsInterface(const Value: IInterface);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsObject(const Value: TObject);
begin
  FValue := Integer(Value);
end;

procedure TAnyValueObject.SetAsPointer(const Value: Pointer);
begin
  FValue := Integer(Value);
end;

procedure TAnyValueObject.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetAsWideString(const Value: WideString);
begin
  FValue := Value;
end;

procedure TAnyValueObject.SetName(const Value: string);
begin
  FName := Value;
end;

function TAnyValueObject.ValueType: Integer;
begin
  Result := VarType(FValue) and VarTypeMask;
end;

{ TValueList }

procedure TValueList.Clear;
begin
  FValuesList.Clear;
end;

constructor TValueList.Create;
begin
  FValuesList := TInterfaceList.Create;
end;

destructor TValueList.Destroy;
begin
  FreeAndNil(FValuesList);

  inherited;
end;

function TValueList.Get(const Name: string): IAnyValue;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FValuesList.Count - 1 do
  begin
    if SameText(Name, IAnyValue(FValuesList[I]).Name) then
    begin
      Result := IAnyValue(FValuesList[I]);
      Exit;
    end;
  end;
end;

function TValueList.GetCount: Integer;
begin
  Result := FValuesList.Count;
end;

function TValueList.GetItems(const Index: Integer): IAnyValue;
begin
  Result := IAnyValue(FValuesList.Items[Index]);
end;

function TValueList.Remove(const Name: string): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FValuesList.Count - 1 do
  begin
    if SameText(Name, IAnyValue(FValuesList[I]).Name) then
    begin
      FValuesList.Delete(I);
      Result := True;
      Exit;
    end;
  end;
end;

function TValueList.Ensure(const Name: string): IAnyValue;
begin
  Result := Get(Name);

  if Result = nil then
  begin
    Result := TAnyValueObject.Create;
    FValuesList.Add(Result);
    Result.Name := Name;
  end;
end;

function TValueList.Exists(const Name: string): Boolean;
begin
  Result := Get(Name) <> nil;
end;

end.
