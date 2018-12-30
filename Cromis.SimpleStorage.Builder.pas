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
 * Storage builder. Helper interface to easy build and XML based SimpleStorage
 * ==================================================================================
 * 08/03/2010 (1.0.0)
 *   - Initial release
 * 20/06/2010 (1.1.0)
 *   - Use TAnyValue instead variant types
 *   - Reduced number of overloads to achieve cleaner interface
 *   - BDS2006 and up compatible (only 2009/2010 before)
 * 17/06/2011 (1.1.1)
 *   - Updated to use the new TAnyValue
 * 21/01/2014 (1.2.0)
 *   - Fixed attribute value setter. Now uses common value setter with element
 * ==================================================================================
*)
unit Cromis.SimpleStorage.Builder;

interface

uses
  SysUtils, Classes,

  // omniXML lib
  OmniXML_Types,

  // cromis units
  Cromis.SimpleStorage, Cromis.AnyValue;

type
  TNodeType = (ntElement, ntAttribute, ntCallbackProc, ntCallbackAnon, ntValue);
  TCallbackProc = procedure(const Element: IElement) of Object;
  TStringArray = array of XmlString;

  {$IF CompilerVersion >= 20}
    TCallbackAnon = reference to procedure(const Element: IElement);
  {$IFEND}

  ICustomNode = Interface(IInterface)
  ['{4276BC1F-9593-4F54-BA28-235CDDE4F8D5}']
    function GetName: XmlString;
    function GetNodeType: TNodeType;
    property Name: XmlString read GetName;
    property NodeType: TNodeType read GetNodeType;
  end;

  IValueNode = Interface(ICustomNode)
  ['{10268F45-35E8-433F-8EB5-74E1C4C6926E}']
    function GetValue: TAnyValue;
    property Value: TAnyValue read GetValue;
  end;

  IElementNode = Interface(IValueNode)
  ['{A23EE428-B7EC-4FDB-B2DD-65E00633C478}']
    function GetChildren: TInterfaceList;
    property Children: TInterfaceList read GetChildren;
  end;

  IAttributeNode = Interface(IValueNode)
  ['{F59C93CC-EC06-40E0-A6A5-C827CF1C380B}']
  end;

  ICallbackProcNode = Interface(ICustomNode)
  ['{7A9A14C4-1FAD-4DE3-B964-B930F4656AFC}']
    function GetCallback: TCallbackProc;
    property Callback: TCallbackProc read GetCallback;
  end;

  {$IF CompilerVersion >= 20}
    ICallbackAnonNode = Interface(ICustomNode)
    ['{7A9A14C4-1FAD-4DE3-B964-B930F4656AFC}']
      function GetCallback: TCallbackAnon;
      property Callback: TCallbackAnon read GetCallback;
    end;
  {$IFEND}

  ISimpleBuilder = Interface(IInterface)
  ['{892A6870-92EB-4F98-AAE8-2AD398F3E811}']
    function GetStorage: ISimpleStorage;
    procedure Construct(const Children: array of ICustomNode);
    property Storage: ISimpleStorage read GetStorage;
  end;

  // main function that create the builder object and returns interface
  function CreateBuilder(const Element: IElement): ISimpleBuilder; overload;

  // various versions of overloaded AddElement function to have a flexible way of adding new elements to XML
  function AddElement(const Name: array of XmlString; const Children: array of ICustomNode): IElementNode; overload;
  function AddElement(const Name: array of XmlString; const Value: TAnyValue): IElementNode; overload;
  function AddElement(const Name: XmlString; const Children: array of ICustomNode): IElementNode; overload;
  function AddElement(const Name: XmlString; const Value: TAnyValue): IElementNode; overload;

  function AddValue(const Value: TAnyValue): IValueNode;
  function AddAttr(const Name: XmlString; const Value: TAnyValue): IAttributeNode;
  function Add(const Callback: TCallbackProc): ICallbackProcNode; overload;
  {$IF CompilerVersion >= 20}
    function Add(const Callback: TCallbackAnon): ICallbackAnonNode; overload;
  {$IFEND}

implementation

type
  TCustomNode = class(TInterfacedObject, ICustomNode)
  private
    function GetName: XmlString;
    function GetNodeType: TNodeType;
  protected
    FName: XmlString;
    FNodeType: TNodeType;
  public
    property NodeType: TNodeType read GetNodeType;
    property Name: XmlString read GetName;
  end;

  TValueNode = class(TCustomNode, IValueNode)
  private
    function GetValue: TAnyValue;
  protected
    FValue: TAnyValue;
  public
    constructor Create(const Name: XmlString; const Value: TAnyValue); virtual;
    property Value: TAnyValue read GetValue;
  end;

  TElementNode = class(TValueNode, IElementNode)
  private
    FChildren: TInterfaceList;
    function GetChildren: TInterfaceList;
  protected
  public
    destructor Destroy; override;
    constructor Create(const Name: XmlString; const Value: TAnyValue); override;
    property Children: TInterfaceList read GetChildren;
  end;

  TAttributeNode = class(TValueNode, IAttributeNode)
  public
    constructor Create(const Name: XmlString; const Value: TAnyValue); override;
  end;

  TCallbackProcNode = class(TCustomNode, ICallbackProcNode)
  private
    FCallback: TCallbackProc;
    function GetCallback: TCallbackProc;
  public
    constructor Create(const Callback: TCallbackProc);
    property InsertProc: TCallbackProc read GetCallback;
  end;

  {$IF CompilerVersion >= 20}
    TCallbackAnonNode = class(TCustomNode, ICallbackAnonNode)
    private
      FCallback: TCallbackAnon;
      function GetCallback: TCallbackAnon;
    public
      constructor Create(const Callback: TCallbackAnon);
      property InsertProc: TCallbackAnon read GetCallback;
    end;
  {$IFEND}

  TSimpleBuilder = class(TInterfacedObject, ISimpleBuilder)
  private
    FElement: IElement;
    function GetStorage: ISimpleStorage;
    procedure DoSetValue(const TargetValue: IValue; const SourceValue: TAnyValue);
    procedure DoAddElement(const TargetNode: IElement; const ElementNode: IElementNode);
    procedure DoAddCallbackProc(const TargetNode: IElement; const CallbackProcNode: ICallbackProcNode);
    {$IF CompilerVersion >= 20}
      procedure DoAddCallbackAnon(const TargetNode: IElement; const CallbackAnonNode: ICallbackAnonNode);
    {$IFEND}
  public
    constructor Create(const Element: IElement);
    procedure Construct(const Children: array of ICustomNode);
    property Storage: ISimpleStorage read GetStorage;
  end;

function CreateBuilder(const Element: IElement): ISimpleBuilder;
begin
  Result := TSimpleBuilder.Create(Element);
end;

function CopyStringArray(const Input: array of XmlString; const From, Length: Integer): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length);

  for I := 0 to Length - 1 do
    Result[I] := Input[From + I];
end;

function AddElement(const Name: array of XmlString; const Children: array of ICustomNode): IElementNode;
begin
  if Length(Name) = 0 then
    raise Exception.Create('At least one valid name is required in AddElement');

  if Length(Name) = 1 then
  begin
    Result := AddElement(Name[0], Children);
    Exit;
  end
  else
  begin
    Result := TElementNode.Create(Name[0], TAnyValue.Null);
    Result.Children.Add(AddElement(CopyStringArray(Name, 1, Length(Name) - 1), Children));
  end;
end;

function AddElement(const Name: array of XmlString; const Value: TAnyValue): IElementNode;
begin
  if Length(Name) = 0 then
    raise Exception.Create('At least one valid name is required in AddElement');

  if Length(Name) = 1 then
  begin
    Result := TElementNode.Create(Name[0], Value); 
    Exit;
  end
  else
  begin
    Result := TElementNode.Create(Name[0], Value);
    Result.Children.Add(AddElement(CopyStringArray(Name, 1, Length(Name) - 1), Value));
  end;
end;

function AddElement(const Name: XmlString; const Children: array of ICustomNode): IElementNode;
var
  I: Integer;
begin
  Result := TElementNode.Create(Name, TAnyValue.Null);

  for I := 0 to Length(Children) - 1 do
    Result.Children.Add(Children[I]);
end;

function AddElement(const Name: XmlString; const Value: TAnyValue): IElementNode;
begin
  Result := TElementNode.Create(Name, Value);
end;

function AddAttr(const Name: XmlString; const Value: TAnyValue): IAttributeNode;
begin
  Result := TAttributeNode.Create(Name, Value);
end;

function Add(const Callback: TCallbackProc): ICallbackProcNode;
begin
  Result := TCallbackProcNode.Create(Callback);
end;

{$IF CompilerVersion >= 20}
function Add(const Callback: TCallbackAnon): ICallbackAnonNode;
begin
  Result := TCallbackAnonNode.Create(Callback);
end;
{$IFEND}

function AddValue(const Value: TAnyValue): IValueNode;
begin
   Result := TValueNode.Create('', Value);
end;

{ TElementNode }

constructor TElementNode.Create(const Name: XmlString; const Value: TAnyValue);
begin
  inherited;

  FChildren := TInterfaceList.Create;
  FNodeType := ntElement;
end;

destructor TElementNode.Destroy;
begin
  FreeAndNil(FChildren);

  inherited;
end;

function TElementNode.GetChildren: TInterfaceList;
begin
  Result := FChildren;
end;

{ TSimpleBuilder }

procedure TSimpleBuilder.Construct(const Children: array of ICustomNode);
var
  I: Integer;
begin
  for I := 0 to Length(Children) - 1 do
  begin
    case Children[I].NodeType of
      ntElement: DoAddElement(FElement.Append(Children[I].Name), IElementNode(Children[I]));
      ntCallbackProc: DoAddCallbackProc(FElement, ICallbackProcNode(Children[I]));
      {$IF CompilerVersion >= 20}
        ntCallbackAnon: DoAddCallbackAnon(FElement, ICallbackAnonNode(Children[I]));
      {$IFEND}
      ntValue: DoSetValue(FElement, IValueNode(Children[I]).Value);
      ntAttribute: DoSetValue(FElement.EnsureAttr(IAttributeNode(Children[I]).Name), IValueNode(Children[I]).Value);
    end;
  end;
end;

constructor TSimpleBuilder.Create(const Element: IElement);
begin
  FElement := Element;
end;

procedure TSimpleBuilder.DoAddElement(const TargetNode: IElement; const ElementNode: IElementNode);
var
  I: Integer;
  Child: IElementNode;
begin
  if not ElementNode.Value.IsEmpty then
    DoSetValue(TargetNode, ElementNode.Value);

  for I := 0 to ElementNode.Children.Count - 1 do
  begin
    Child := IElementNode(ElementNode.Children[I]);

    case Child.NodeType of
      ntElement: DoAddElement(TargetNode.Append(Child.Name), IElementNode(Child));
      ntCallbackProc: DoAddCallbackProc(TargetNode, ICallbackProcNode(Child));
      {$IF CompilerVersion >= 20}
        ntCallbackAnon: DoAddCallbackAnon(TargetNode, ICallbackAnonNode(Child));
      {$IFEND}
      ntValue: DoSetValue(TargetNode, IValueNode(Child).Value);
      ntAttribute: DoSetValue(TargetNode.EnsureAttr(IAttributeNode(Child).Name), IValueNode(Child).Value);
    end;
  end;
end;

procedure TSimpleBuilder.DoSetValue(const TargetValue: IValue; const SourceValue: TAnyValue);
begin
  case SourceValue.GetValueType of
    avtNone: raise Exception.Create('avtNone type is not supported');
    avtBoolean: TargetValue.AsBoolean := SourceValue.AsBoolean;
    avtInteger: TargetValue.AsInteger := SourceValue.AsInteger;
    avtException: TargetValue.AsInt64 := SourceValue.AsInt64;
    avtInt64: TargetValue.AsInt64 := SourceValue.AsInt64;
    avtCardinal: TargetValue.AsInt64 := SourceValue.AsInt64;
    avtDouble: TargetValue.AsFloat := SourceValue.AsDouble;
    avtFloat: TargetValue.AsFloat := SourceValue.AsFloat;
    avtString: TargetValue.AsString := SourceValue.AsString;
    avtArray: raise Exception.Create('avtArray type is not supported');
    avtVariant: raise Exception.Create('avtVariant type is not supported');
    avtObject: raise Exception.Create('avtObject type is not supported');
    avtPointer: raise Exception.Create('avtPointer type is not supported');
    avtInterface: raise Exception.Create('avtInterface type is not supported');
    avtNamedValue: raise Exception.Create('avtNamedValue type is not supported');
  {$IFDEF UNICODE}
    avtAnsiString: TargetValue.AsString := string(SourceValue.AsAnsiString);
  {$ENDIF}
    avtWideString: TargetValue.AsString := SourceValue.AsWideString;
    avtDateTime: TargetValue.AsDateTime := SourceValue.AsDateTime;
  end;
end;

procedure TSimpleBuilder.DoAddCallbackProc(const TargetNode: IElement; const CallbackProcNode: ICallbackProcNode);
begin
  CallbackProcNode.Callback(TargetNode);
end;

{$IF CompilerVersion >= 20}
procedure TSimpleBuilder.DoAddCallbackAnon(const TargetNode: IElement; const CallbackAnonNode: ICallbackAnonNode);
begin
  CallbackAnonNode.Callback(TargetNode);
end;
{$IFEND}

function TSimpleBuilder.GetStorage: ISimpleStorage;
begin
  Result := FElement.Storage;
end;

{ TCustomNode }

function TCustomNode.GetName: XmlString;
begin
  Result := FName;
end;

function TCustomNode.GetNodeType: TNodeType;
begin
  Result := FNodeType;
end;

{ TValueNode }

constructor TValueNode.Create(const Name: XmlString; const Value: TAnyValue);
begin
  FNodeType := ntValue;
  FValue := Value;
  FName := Name;
end;

function TValueNode.GetValue: TAnyValue;
begin
  Result := FValue;
end;

{ TCallbackProcNode }

constructor TCallbackProcNode.Create(const Callback: TCallbackProc);
begin
  FCallback := Callback;
  FNodeType := ntCallbackProc;
end;

function TCallbackProcNode.GetCallback: TCallbackProc;
begin
  Result := FCallback;
end;

{$IF CompilerVersion >= 20}
{ TCallbackAnonNode }

constructor TCallbackAnonNode.Create(const Callback: TCallbackAnon);
begin
  FCallback := Callback;
  FNodeType := ntCallbackAnon;
end;

function TCallbackAnonNode.GetCallback: TCallbackAnon;
begin
  Result := FCallback;
end;
{$IFEND}

{ TAttributeNode }

constructor TAttributeNode.Create(const Name: XmlString; const Value: TAnyValue);
begin
  inherited;

  FNodeType := ntAttribute;
end;

end.
