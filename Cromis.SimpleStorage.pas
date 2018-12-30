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
 * A simple data storage based on OmniXML.
 * ==================================================================================
 * 12/01/2010 (1.5.1)
 *   - Support for encryption based on XTEA algorithm
 * ==================================================================================
 * 06/02/2010 (1.6.1)
 *   - Added IDocumentsCollection interface for easy XML document searches
 * ==================================================================================
 * 07/02/2010 (1.6.2)
 *   - Added GetAttr / EnsureAttr function to simplify the attribute manipulation
 * ==================================================================================
 * 08/02/2010 (1.6.3)
 *   - Changes in SimpleStorage.Collection related to single document handling
 * ==================================================================================
 * 08/03/2010 (1.6.5)
 *   - Overloaded all functions that accept XPath. Now it is possible to use path
 *     in the form of an array of strings. It is easier to use constants this way
 *   - Included StorageBuilder as part of SimpleStorage
 *   - Was forced to expose XMLNode as Element property
 *   - Fixed Element.Remove inconsistencies
 *   - Remove can compact empty parent nodes
 *   - SaveChanges added to IDocument in Collection
 * ==================================================================================
 * 28/03/2010 (1.7.0)
 *   - Added IDocumentFilter as a base for document wide filtering
 *   - Added IDocumentFilterChain as a means to chain the multiple filters together
 *   - Added IStorageFilter to allow doument filtering through ISimpleStorage interface
 *   - IEncryptedSimpleStorage now inherits from IDocumentFilter
 *   - Added ICompressedStorage document filter
 *   - IDocuments now uses FilterChain to possibly filter the XML documents
 * ==================================================================================
 * 05/11/2010 (1.7.1)
 *   - ICustomFilterData added to allow one filter syntax for both elements and documents
 *   - renamed some of the internally used interfaces to have a clearer structure
 * ==================================================================================
 * 25/11/2010 (1.7.2)
 *   - Filter chaining code is now consistent
 *   - Do not directly work on filter input streams
 * ==================================================================================
 * 25/01/2011 (1.7.3)
 *   - TElement.Assign and TElement.Merge now correctly handle CData
 * ==================================================================================
 * 30/05/2011 (1.7.4)
 *   - TAttributes.Exists does not raise exception if no XML node is present (returns false)
 * ==================================================================================
 * 30/05/2011 (1.7.5)
 *   - AppendAllElements now assigns child nodes before the recursion to keep node order
 * ==================================================================================
 * 29/01/2012 (1.7.6)
 *   - Fixed Merge procedure
 * ==================================================================================
 * 12/06/2012 (1.7.7)
 *   - StorageFromXMLdocument and TSimpleStorage.LoadFromXMLDocument added
 *   - XMLDocument for ISimpleStorage allows acces to underlying document object
 *   - Added LastLoadStatus to allow the handling of XML parsing errors
 * ==================================================================================
*)

unit Cromis.SimpleStorage;

interface

uses
  SysUtils, Classes, Controls, Contnrs, Graphics, GraphUtil,

  // omniXML library units
  {$IFNDEF USE_MSXML}OmniXML{$ELSE}MSXML, OmniXML_MSXML{$ENDIF},OmniXML_Types, OmniXMLUtils,

  // internal cromis units
  Cromis.StringUtils, Cromis.Streams;

type
  TElementType = (etValue, etNode);
  ICustomFilterData = interface;
  ISimpleStorage = Interface;
  IElement = Interface;
  IValue = Interface;


  IValueData = Interface(IInterface)
  ['{ABB5B59C-4AEB-46E8-BBD3-9D4CFB449E77}']
    function _GetIsValid: Boolean;
    property IsValid: Boolean read _GetIsValid;
  end;

  ICData = Interface(IValueData)
  ['{6D1018C6-99EE-4175-8356-823E4D43E620}']
    function _GetData: IValue;
    // getters and setters
    // CData functions and procedures
    procedure SaveToStream(const Stream: TStream; const FailIfInvalid: Boolean = False);
    procedure LoadFromStream(const Stream: TStream);
    property Data: IValue read _GetData;
  end;

  IBinary = Interface(IValueData)
  ['{6CD7653A-55F8-477A-9F64-E49131982026}']
    // getters and setters
    function _GetStream: TStream;
    // binary functions and procedures
    procedure SaveToFile(const FileName: string; const FailIfInvalid: Boolean = False);
    procedure LoadFromFile(const FileName: string; const Mode: Word = fmShareDenyNone);
    procedure SaveToStream(const Stream: TStream; const FailIfInvalid: Boolean = False);
    procedure LoadFromBuffer(const Buffer: Pointer; const Size: Cardinal);
    procedure LoadFromElement(const Element: IElement);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToBuffer(var Buffer: Pointer);
    procedure LoadFromXML(const XML: XmlString);
    property Stream: TStream read _GetStream;
  end;

  IStorageBase = Interface(IInterface)
  ['{C41F71F0-1EEC-40E3-A7D7-B83EBBB45C3B}']
    function GetElementNode: IXMLNode;
    procedure SetElementNode(const Value: IXMLNode);
    function LoadValueAsString: XmlString;
    procedure LoadValueAsStream(const Value: TStream);
    procedure SaveValueAsStream(const Value: TStream);
    procedure SaveValueAsString(const Value: XmlString);
    property ElementNode: IXMLNode read GetElementNode write SetElementNode;
  end;

  IStorageData = Interface(IStorageBase)
  ['{284493C8-994E-4853-BF32-348B801A73F4}']
  end;

  IValueFilterData =  Interface(IStorageData)
  ['{78BC0D4B-EE30-4E7C-B778-3A3751A9758B}']
    function GetNextFilter: IValueFilterData;
    procedure AddFilter(const Data: IValueFilterData);
    procedure LoadChainValue(const Value, Result: TStream);
    procedure SaveChainValue(const Value, Result: TStream);
    property NextFilter: IValueFilterData read GetNextFilter;
  end;

  IAdapterData = Interface(IInterface)
  ['{284493C8-994E-4853-BF32-348B801A73F4}']
    // property getters and setters
    function GetElement: IElement;
    procedure SetElement(const Value: IElement);
    // storage load / save procedures / functions
    procedure LoadAdapterData(const DataObject: TObject);
    procedure SaveAdapterData(const DataObject: TObject);
    // IAdapterData properties
    property Element: IElement read GetElement write SetElement;
  end;

  IAdapter = Interface(IInterface)
  ['{11548A74-E56D-46BE-9D23-F294431B0139}']
    procedure Load(const SourceObject: TObject);
    procedure Save(const TargetObject: TObject);
  end;

  IValue = Interface(IValueData)
  ['{3D25DFB7-CC19-4598-9010-8FA55322F126}']
    // getters and setters
    function _GetName: XmlString;
    function _GetAsTime: TTime;
    function _GetAsDate: TDate;
    function _GetAsFloat: Real;
    function _GetAsInt64: Int64;
    function _GetAsCData: ICData;
    function _GetAsColor: TColor;
    function _GetAsBinary: IBinary;
    function _GetAsString: XmlString;
    function _GetAsInteger: Integer;
    function _GetAsBoolean: Boolean;
    function _GetAsDateTime: TDateTime;
    procedure _SetAsTime(Value: TTime);
    procedure _SetAsDate(Value: TDate);
    procedure _SetAsFloat(Value: Real);
    procedure _SetAsInt64(Value: Int64);
    procedure _SetAsColor(Value: TColor);
    procedure _SetAsString(Value: XmlString);
    procedure _SetAsInteger(Value: Integer);
    procedure _SetAsBoolean(Value: Boolean);
    procedure _SetAsDateTime(Value: TDateTime);
    // value properties that get/set value in different formats
    function AsFloatDef(const DefValue: Real = 0): Real;
    function AsInt64Def(const DefValue: Int64 = 0): Int64;
    function AsColorDef(const DefValue: TColor = clNone): TColor;
    function AsStringDef(const DefValue: XmlString = ''): XmlString;
    function AsIntegerDef(const DefValue: Integer = 0): Integer;
    function AsBooleanDef(const DefValue: Boolean = False): Boolean;
    function AsDateTimeDef(const DefValue: TDateTime = 0): TDateTime;
    property AsDateTime: TDateTime read _GetAsDateTime write _SetAsDateTime;
    property AsBoolean: Boolean read _GetAsBoolean write _SetAsBoolean;
    property AsInteger: Integer read _GetAsInteger write _SetAsInteger;
    property AsString: XmlString read _GetAsString write _SetAsString;
    property AsColor: TColor read _GetAsColor write _SetAsColor;
    property AsInt64: Int64 read _GetAsInt64 write _SetAsInt64;
    property AsFloat: Real read _GetAsFloat write _SetAsFloat;
    property AsTime: TTime read _GetAsTime write _SetAsTime;
    property AsDate: TDate read _GetAsDate write _SetAsDate;
    property AsBinary: IBinary read _GetAsBinary;
    property AsCData: ICData read _GetAsCData;
    property Name: XmlString read _GetName;
  end;

  IAttributesEnumerator = Interface(IInterface)
  ['{D0F64552-2B5A-4874-951C-C4064C482B6D}']
    // getters and setters
    function _GetCurrent: IValue;
    // iterator function and procedures
    function MoveNext: Boolean;
    property Current: IValue read _GetCurrent;
  end;

  IAttributes = Interface(IInterface)
  ['{36A4A35F-260F-4EC9-A8CD-84DF4DA12BCF}']
    procedure Assign(const Attributes: IAttributes);
    procedure Update(const Attributes: IAttributes);
    function GetEnumerator: IAttributesEnumerator;
    function Ensure(const Name: XmlString): IValue;
    function Exists(const Name: XmlString): Boolean;
    function Get(const Name: XmlString): IValue;
    procedure Remove(const Name: XmlString);
    function Count: Integer;
  end;

  IBaseEnumerator = Interface(IInterface)
  ['{A9525AC0-2831-451B-A112-B333B69D9168}']
    // getters and setters
    function _GetCurrent: IElement;
    // iterator function and procedures
    function MoveNext: Boolean;
    property Current: IElement read _GetCurrent;
  end;

  IElementsEnumerator = Interface(IBaseEnumerator)
  ['{E523BEAD-9B50-4749-B864-3F1E38CD35E5}']
  end;

  IElementsList = Interface(IInterface)
  ['{87AEE05F-1DED-49E1-96E1-55F6C8728F79}']
    function Count: Integer;
    function _GetLast: IElement;
    function _GetFirst: IElement;
    function GetEnumerator: IElementsEnumerator;
    function _GetItem(const Index: Integer): IElement;
    property Item[const Index: Integer]: IElement read _GetItem;
    property First: IElement read _GetFirst;
    property Last: IElement read _GetLast;
  end;

  INodesEnumerator = Interface(IBaseEnumerator)
  ['{825AB39D-F783-42B1-AE1A-239286D3C2B6}']
  end;

  INodesList = Interface(IInterface)
  ['{87AEE05F-1DED-49E1-96E1-55F6C8728F79}']
    function GetEnumerator: INodesEnumerator;
    function Count: Integer;
  end;

  IValuesEnumerator = Interface(IBaseEnumerator)
  ['{E523BEAD-9B50-4749-B864-3F1E38CD35E5}']
  end;

  IValuesList = Interface(IInterface)
  ['{87AEE05F-1DED-49E1-96E1-55F6C8728F79}']
    function GetEnumerator: IValuesEnumerator;
    function Count: Integer;
  end;

  IValueFilter = Interface(IValue)
  ['{E9AA30DA-4581-4922-A58A-4476E9D49AC2}']
    function Filter(const Data: ICustomFilterData): IValueFilter;
  end;

  IDocumentFilterData = interface(IInterface)
  ['{5230BB12-6B92-450D-B8EA-C7D475C612E8}']
    function LoadFromFile(const FileName: string): ISimpleStorage;
    function LoadFromStream(const Stream: TStream): ISimpleStorage;
    procedure SaveToFile(const Storage: ISimpleStorage; const FileName: string);
    procedure SaveToStream(const Storage: ISimpleStorage; const Stream: TStream);
    procedure DirectFilterIn(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectFilterIn(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectFilterIn(const SourceStream, TargetStream: TStream); overload;
    procedure DirectFilterIn(const SourceFile, TargetFile: string); overload;
    procedure DirectFilterOut(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectFilterOut(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectFilterOut(const SourceStream, TargetStream: TStream); overload;
    procedure DirectFilterOut(const SourceFile, TargetFile: string); overload;
  end;

  IDocumentFilterChain = interface(IDocumentFilterData)
  ['{FF9AA033-E77A-4149-83B9-55E5219CEBB1}']
    procedure AddFilter(const Filter: IDocumentFilterData);
    function Count: Integer;
    procedure Clear;
  end;

  IDocumentFilter = Interface(IInterface)
  ['{76DCCC1D-8D1B-46B9-A12A-05D6BAD8DB29}']
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);
    function Filter(const Filter: ICustomFilterData): IDocumentFilter;
  end;

  ICustomFilterData = Interface(IInterface)
  ['{E6AEC66D-733C-4126-8ACD-4F9CD841911C}']
    function ValueFilterData(const Node: IXMLNode): IValueFilterData;
    function DocumentFilterData: IDocumentFilterData;
  end;

  IElement = Interface(IValue)
  ['{D42DB0F3-766B-4392-A670-DA009D4BCB20}']
    // getters and setters
    function _GetParent: IElement;
    function _GetXMLNode: IXMLNode;
    function _GetHasNodes: Boolean;
    function _GetHasValues: Boolean;
    function _GetNodeIndex: Integer;
    function _GetHasElements: Boolean;
    function _GetRootElement: IElement;
    function _GetElementType: TElementType;
    // element functions and procedures
    function Attributes: IAttributes;
    procedure RemoveAllElements;
    procedure Merge(const Element: IElement);
    procedure Remove(const Path: XmlString; const Compact: Boolean = False); overload;
    procedure Remove(const Element: IElement; const Compact: Boolean = False); overload;
    procedure Remove(const Path: array of XmlString; const Compact: Boolean = False); overload;
    function Storage(const Path: array of XmlString): ISimpleStorage; overload;
    function Storage(const Path: XmlString = ''): ISimpleStorage; overload;
    function Elements(const Params: XmlString = '*'): IElementsList;
    function Values(const Params: XmlString = '*'): IValuesList;
    function Nodes(const Params: XmlString = '*'): INodesList;
    procedure Assign(const Element: IElement; const InDepth: Boolean = False); overload;
    procedure Assign(const Nodes: INodesList; const InDepth: Boolean = False); overload;
    procedure Assign(const Nodes: IElementsList; const InDepth: Boolean = False); overload;
    function Append(const Element: IElement; const InDepth: Boolean = True): IElement; overload;
    procedure Append(const Elements: IElementsList; const InDepth: Boolean = False); overload;
    procedure Append(const Nodes: INodesList; const InDepth: Boolean = False); overload;
    function Append(const Path: array of XmlString): IElement; overload;
    function Append(const Path: XmlString): IElement; overload;
    procedure Append(const Values: IValuesList); overload;
    function EnsureAttr(const Path: array of XmlString; const Name: XmlString): IValue; overload;
    function GetAttr(const Path: array of XmlString; const Name: XmlString): IValue; overload;
    function EnsureAttr(const Path, Name: XmlString): IValue; overload;
    function GetAttr(const Path, Name: XmlString): IValue; overload;
    function EnsureAttr(const Name: XmlString): IValue; overload;
    function GetAttr(const Name: XmlString): IValue; overload;
    function Ensure(const Path: array of XmlString): IElement; overload;
    function Exists(const Path: array of XmlString): Boolean; overload;
    function Get(const Path: array of XmlString): IElement; overload;
    function Ensure(const Path: XmlString): IElement; overload;
    function Exists(const Path: XmlString): Boolean; overload;
    function Get(const Path: XmlString): IElement; overload;
    function FirstChild: IElement;
    // custom plugin funtions. returns the interface
    function Filter(const Data: ICustomFilterData): IValueFilter; overload;
    function Filter(const Name: string): IValueFilter; overload; deprecated;
    function Adapter(const Data: IAdapterData): IAdapter; overload;
    function Adapter(const Name: string): IAdapter; overload; deprecated;
    // properties of the elements
    property ElementType: TElementType read _GetElementType;
    property RootElement: IElement read _GetRootElement;
    property HasElements: Boolean read _GetHasElements;
    property HasValues: Boolean read _GetHasValues;
    property NodeIndex: Integer read _GetNodeIndex;
    property HasNodes: Boolean read _GetHasNodes;
    property XMLNode: IXMLNode read _GetXMLNode;
    property Parent: IElement read _GetParent;
  end;

  TLoadError = record
    ErrorCode: Integer;
    SrcText: XmlString;
    FilePos: Integer;
    LinePos: Integer;
    Reason: string;
    Line: Integer;
  end;

  TLoadStatus = record
    Success: Boolean;
    Error: TLoadError;
  end;

  ISimpleStorage = Interface(IElement)
  ['{16EEA551-3D02-49C3-B097-0A1A6FA0B1CD}']
    // storage function and procedures
    procedure Clear;
    procedure SaveToFile(const FileName: XmlString);
    procedure SaveToStream(const OutStream: TStream);
    procedure LoadFromXML(const XML: XmlString);
    procedure LoadFromFile(const FileName: XmlString);
    procedure LoadFromStream(const InStream: TStream);
    procedure LoadFromXMLDocument(const Document: IXMLDocument);
    function Content(const Structured: Boolean = False): XmlString;
    function Filter(const Data: ICustomFilterData): IDocumentFilter; overload;
    function LastLoadStatus: TLoadStatus;
    function XMLDocument: IXMLDocument;
  end;

  // ***************************************************************************************
  //  Storage, filter and adapter data classes.
  //  They have to be overriden with actual implementation.
  //  WARNING: Constructor must not be redeclared or overriden !!!
  // ***************************************************************************************

  TStorageBase = class(TInterfacedObject, IStorageBase)
  private
    FElementNode: IXMLNode;
    function GetElementNode: IXMLNode;
    procedure SetElementNode(const Value: IXMLNode);
  public
    constructor Create(const ElementNode: IXMLNode);
    function LoadValueAsString: XmlString; virtual; abstract;
    procedure LoadValueAsStream(const Value: TStream); virtual; abstract;
    procedure SaveValueAsStream(const Value: TStream); virtual; abstract;
    procedure SaveValueAsString(const Value: XmlString); virtual; abstract;
    property ElementNode: IXMLNode read GetElementNode write SetElementNode;
  end;

  TStorageData = class(TStorageBase, IStorageData)
  protected
    function DoLoadValueAsString: XmlString; virtual; abstract;
    procedure DoLoadValueAsStream(const Value: TStream); virtual; abstract;
    procedure DoSaveValueAsStream(const Value: TStream); virtual; abstract;
    procedure DoSaveValueAsString(const Value: XmlString); virtual; abstract;
  public
    function LoadValueAsString: XmlString; override;
    procedure LoadValueAsStream(const Value: TStream); override;
    procedure SaveValueAsStream(const Value: TStream); override;
    procedure SaveValueAsString(const Value: XmlString); override;
  end;

  // ***************************************************************************************
  //  Value filter base class. It has to be overriden with actual implementation
  //  WARNING: Constructor must not be redeclared or overriden !!!
  // ***************************************************************************************

  TValueFilterData = class(TStorageBase, IValueFilterData)
  private
    FNextFilter: IValueFilterData;
    function GetNextFilter: IValueFilterData;
    procedure InternalLoadValueAsStream(const Value: TStream);
    procedure InternalSaveValueAsStream(const Value: TStream);
  protected
    procedure DoLoadValueAsStream(const Value, Result: TStream); virtual; abstract;
    procedure DoSaveValueAsStream(const Value, Result: TStream); virtual; abstract;
  public
    procedure AddFilter(const Filter: IValueFilterData);
    function LoadValueAsString: XmlString; override;
    procedure LoadChainValue(const Value, Result: TStream);
    procedure SaveChainValue(const Value, Result: TStream);
    procedure LoadValueAsStream(const Value: TStream); override;
    procedure SaveValueAsStream(const Value: TStream); override;
    procedure SaveValueAsString(const Value: XmlString); override;
    property NextFilter: IValueFilterData read GetNextFilter;
  end;

  TAdapterData = class(TInterfacedObject, IAdapterData)
  private
    FElement: IElement;
    function GetElement: IElement;
    procedure SetElement(const Value: IElement);
  protected
    procedure DoLoadAdapterData(const DataObject: TObject); virtual; abstract;
    procedure DoSaveAdapterData(const DataObject: TObject); virtual; abstract;
  public
    procedure LoadAdapterData(const DataObject: TObject);
    procedure SaveAdapterData(const DataObject: TObject);
    property Element: IElement read GetElement write SetElement;
  end;

  // generic class for storage and adapter data
  TStorageDataClass = class of TValueFilterData;
  TAdapterDataClass = class of TAdapterData;

  // ***************************************************************************************
  //  Document filter base class. It has to be overriden with actual implementation
  //  WARNING: Constructor must not be redeclared or overriden !!!
  // ***************************************************************************************

  TDocumentFilterData = class(TInterfacedObject, IDocumentFilterData)
  private
    procedure DirectFilterIn(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectFilterIn(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectFilterIn(const SourceFile, TargetFile: string); overload;
    procedure DirectFilterOut(const SourceStream: TStream; const TargetFile: string); overload;
    procedure DirectFilterOut(const SourceFile: string; const TargetStream: TStream); overload;
    procedure DirectFilterOut(const SourceFile, TargetFile: string); overload;
  protected
    procedure DirectFilterIn(const SourceStream, TargetStream: TStream); overload; virtual; abstract;
    procedure DirectFilterOut(const SourceStream, TargetStream: TStream); overload; virtual; abstract;
  public
    function LoadFromFile(const FileName: string): ISimpleStorage; virtual;
    function LoadFromStream(const Stream: TStream): ISimpleStorage; virtual;
    procedure SaveToFile(const Storage: ISimpleStorage; const FileName: string); virtual;
    procedure SaveToStream(const Storage: ISimpleStorage; const Stream: TStream); virtual;
  end;

  // function that create SimpleStorage from different input data
  function CreateStorage(const RootNode: XmlString = ''): ISimpleStorage;
  function StorageFromXML(const XML: XmlString): ISimpleStorage;
  function StorageFromFile(const FileName: XmlString): ISimpleStorage;
  function StorageFromStream(const Stream: TStream): ISimpleStorage;
  function StorageFromElement(const Element: IElement): ISimpleStorage;
  function StorageFromXMLDocument(const Document: IXMLDocument): ISimpleStorage;

  // function that takes a single XML node and makes an IElement
  function ElementFromXMLNode(const Node: IXMLNode): IElement;

  // functions that registers the global plugins for all simple storages
  procedure RegisterFilter(const Name: string; const DataClass: TStorageDataClass);
  procedure RegisterAdapter(const Name: string; const DataClass: TAdapterDataClass);

  // constructors for storage data
  function AttrNormalProxy(const ElementNode: IXMLNode): IStorageData;
  function NodeNormalProxy(const ElementNode: IXMLNode): IStorageData;
  function CDataProxy(const ElementNode: IXMLNode): IStorageData;

  // function that creates a new document filter chain
  function CreateDocumentFilterChain: IDocumentFilterChain;

implementation

const
  DATA_ROOT = 'Data';

// *****************************************************************************************
//  internal classes declarations hidden from the user
// *****************************************************************************************

type
  TValueData = class(TInterfacedObject, IValueData)
  private
    function _GetIsValid: Boolean;
  protected
    FStorageData: IStorageData;
  public
    constructor Create(const StorageData: IStorageData); virtual;
    property IsValid: Boolean read _GetIsValid;
  end;

  TCData = class(TValueData, ICData)
  private
    function _GetData: IValue;
  public
    procedure SaveToStream(const Stream: TStream; const FailIfNoData: Boolean = False);
    procedure LoadFromStream(const Stream: TStream);
    property Data: IValue read _GetData;
  end;

  TBinaryStream = class(TMemoryStream)
  public
    function CopyFrom(Source: TStream; Count: Int64): Int64; reintroduce;
  end;

  TBinary = class(TValueData, IBinary)
  private
    FMemoryStream: TBinaryStream;
    function _GetStream: TStream;
  public
    constructor Create(const StorageData: IStorageData); override;
    destructor Destroy; override;
    // binary functions and procedures
    procedure SaveToFile(const FileName: string; const FailIfNoData: Boolean = False);
    procedure LoadFromFile(const FileName: string; const Mode: Word = fmShareDenyNone);
    procedure SaveToStream(const Stream: TStream; const FailIfNoData: Boolean = False);
    procedure LoadFromBuffer(const Buffer: Pointer; const Size: Cardinal);
    procedure LoadFromElement(const Element: IElement);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToBuffer(var Buffer: Pointer);
    procedure LoadFromXML(const XML: XmlString);
    property Stream: TStream read _GetStream;
  end;

  TValue = class(TValueData, IValue)
  private
    function _GetName: XmlString;
    function _GetAsTime: TTime;
    function _GetAsDate: TDate;
    function _GetAsFloat: Real;
    function _GetAsInt64: Int64;
    function _GetAsCData: ICData;
    function _GetAsColor: TColor;
    function _GetAsBinary: IBinary;
    function _GetAsString: XmlString;
    function _GetAsInteger: Integer;
    function _GetAsBoolean: Boolean;
    function _GetAsDateTime: TDateTime;
    procedure _SetAsTime(Value: TTime);
    procedure _SetAsDate(Value: TDate);
    procedure _SetAsFloat(Value: Real);
    procedure _SetAsInt64(Value: Int64);
    procedure _SetAsColor(Value: TColor);
    procedure _SetAsString(Value: XmlString);
    procedure _SetAsInteger(Value: Integer);
    procedure _SetAsBoolean(Value: Boolean);
    procedure _SetAsDateTime(Value: TDateTime);
  public
    function AsFloatDef(const DefValue: Real = 0): Real;
    function AsInt64Def(const DefValue: Int64 = 0): Int64;
    function AsColorDef(const DefValue: TColor = clNone): TColor;
    function AsStringDef(const DefValue: XmlString = ''): XmlString;
    function AsIntegerDef(const DefValue: Integer = 0): Integer;
    function AsBooleanDef(const DefValue: Boolean = False): Boolean;
    function AsDateTimeDef(const DefValue: TDateTime = 0): TDateTime;
    property AsDateTime: TDateTime read _GetAsDateTime write _SetAsDateTime;
    property AsBoolean: Boolean read _GetAsBoolean write _SetAsBoolean;
    property AsInteger: Integer read _GetAsInteger write _SetAsInteger;
    property AsString: XmlString read _GetAsString write _SetAsString;
    property AsColor: TColor read _GetAsColor write _SetAsColor;
    property AsInt64: Int64 read _GetAsInt64 write _SetAsInt64;
    property AsFloat: Real read _GetAsFloat write _SetAsFloat;
    property AsTime: TTime read _GetAsTime write _SetAsTime;
    property AsDate: TDate read _GetAsDate write _SetAsDate;
    property AsBinary: IBinary read _GetAsBinary;
    property AsCData: ICData read _GetAsCData;
    property Name: XmlString read _GetName;
  end;

  TAttributes = class(TInterfacedObject, IAttributes)
  private
    FElementNode: IXMLNode;
  public
    constructor Create(const ElementNode: IXMLNode);
    procedure Assign(const Attributes: IAttributes);
    procedure Update(const Attributes: IAttributes);
    function GetEnumerator: IAttributesEnumerator;
    function Ensure(const Name: XmlString): IValue;
    function Exists(const Name: XmlString): Boolean;
    function Get(const Name: XmlString): IValue;
    procedure Remove(const Name: XmlString);
    function Count: Integer;
  end;

  TAttributesEnumerator = class(TInterfacedObject, IAttributesEnumerator)
  private
    FIndex: Integer;
    FElementNode: IXMLNode;
    FCurrentNode: IXMLNode;
    function _GetCurrent: IValue;
  public
    constructor Create(const RootNode: IXMLNode);
    function MoveNext: Boolean;
    property Current: IValue read _GetCurrent;
  end;

  TElementAdapter = class(TInterfacedObject, IAdapter)
  private
    FData: IAdapterData;
  public
    constructor Create(const Data: IAdapterData);
    procedure Load(const SourceObject: TObject);
    procedure Save(const TargetObject: TObject);
  end;

  TValueFilter = class(TValue, IValueFilter)
  public
    function Filter(const Data: ICustomFilterData): IValueFilter;
  end;

  TElement = class(TValue, IElement)
  private
    function _GetParent: IElement;
    function _GetXMLNode: IXMLNode;
    function _GetHasNodes: Boolean;
    function _GetNodeIndex: Integer;
    function _GetHasValues: Boolean;
    function _GetHasElements: Boolean;
    function _GetRootElement: IElement;
    function _GetElementType: TElementType;
    function DoConstructPath(const Path: array of XmlString): XmlString;
    function AssignUniqueValueNode(const Template, Target: IElement; const IndexList: TList): IElement;
    function InternalAppend(const Path: XmlString; const Append: Boolean): IXMLNode;
    procedure AppendAllElements(const Element, Node: IElement; const Recurse: Boolean);
    procedure DeleteAllChildNodes(const ParentNode: IXMLNode);
    procedure MergeAllElements(const Element, Node: IElement);
  public
    function Attributes: IAttributes;
    procedure RemoveAllElements;
    procedure Merge(const Element: IElement);
    procedure Remove(const Path: XmlString; const Compact: Boolean = False); overload;
    procedure Remove(const Element: IElement; const Compact: Boolean = False); overload;
    procedure Remove(const Path: array of XmlString; const Compact: Boolean = False); overload;
    function Storage(const Path: array of XmlString): ISimpleStorage; overload;
    function Storage(const Path: XmlString = ''): ISimpleStorage; overload;
    function Elements(const Params: XmlString = '*'): IElementsList;
    function Values(const Params: XmlString = '*'): IValuesList;
    function Nodes(const Params: XmlString = '*'): INodesList;
    procedure Assign(const Element: IElement; const InDepth: Boolean = False); overload;
    procedure Assign(const Nodes: INodesList; const InDepth: Boolean = False); overload;
    procedure Assign(const Nodes: IElementsList; const InDepth: Boolean = False); overload;
    function Append(const Element: IElement; const InDepth: Boolean = True): IElement; overload;
    procedure Append(const Elements: IElementsList; const InDepth: Boolean = False); overload;
    procedure Append(const Nodes: INodesList; const InDepth: Boolean = False); overload;
    function Append(const Path: array of XmlString): IElement; overload;
    function Append(const Path: XmlString): IElement; overload;
    procedure Append(const Values: IValuesList); overload;
    function EnsureAttr(const Path: array of XmlString; const Name: XmlString): IValue; overload;
    function GetAttr(const Path: array of XmlString; const Name: XmlString): IValue; overload;
    function EnsureAttr(const Path, Name: XmlString): IValue; overload;
    function GetAttr(const Path, Name: XmlString): IValue; overload;
    function EnsureAttr(const Name: XmlString): IValue; overload;
    function GetAttr(const Name: XmlString): IValue; overload;
    function Ensure(const Path: array of XmlString): IElement; overload;
    function Exists(const Path: array of XmlString): Boolean; overload;
    function Get(const Path: array of XmlString): IElement; overload;
    function Ensure(const Path: XmlString): IElement; overload;
    function Exists(const Path: XmlString): Boolean; overload;
    function Get(const Path: XmlString): IElement; overload;
    function FirstChild: IElement;
    // custom plugin funtions. returns the interface
    function Filter(const Data: ICustomFilterData): IValueFilter; overload;
    function Filter(const Name: string): IValueFilter; overload;
    function Adapter(const Data: IAdapterData): IAdapter; overload;
    function Adapter(const Name: string): IAdapter; overload;
    // properties of the elements
    property ElementType: TElementType read _GetElementType;
    property RootElement: IElement read _GetRootElement;
    property HasElements: Boolean read _GetHasElements;
    property HasValues: Boolean read _GetHasValues;
    property NodeIndex: Integer read _GetNodeIndex;
    property HasNodes: Boolean read _GetHasNodes;
    property XMLNode: IXMLNode read _GetXMLNode;
    property Parent: IElement read _GetParent;
  end;

  TBaseEnumerator = class(TInterfacedObject, IElementsEnumerator)
  private
    function _GetCurrent: IElement;
  protected
    FElements: IXMLNodeList;
    FCurrentNode: IXMLNode;
  public
    constructor Create(const Elements: IXMLNodeList);
    function MoveNext: Boolean; virtual; abstract;
    property Current: IElement read _GetCurrent;
  end;

  TElementsEnumerator = class(TBaseEnumerator, IElementsEnumerator)
  public
    function MoveNext: Boolean; override;
  end;

  TValuesEnumerator = class(TElementsEnumerator, IValuesEnumerator)
  public
    function MoveNext: Boolean; override;
  end;

  TNodesEnumerator = class(TElementsEnumerator, INodesEnumerator)
  public
    function MoveNext: Boolean; override;
  end;

  TElementsList = class(TInterfacedObject, IElementsList)
  private
    FElements: IXMLNodeList;
    function _GetLast: IElement;
    function _GetFirst: IElement;
    function _GetItem(const Index: Integer): IElement;
  public
    constructor Create(const Elements: IXMLNodeList);
    function Count: Integer;
    function GetEnumerator: IElementsEnumerator;
    property Item[const Index: Integer]: IElement read _GetItem;
    property First: IElement read _GetFirst;
    property Last: IElement read _GetLast;
  end;

  TNodesList = class(TInterfacedObject, INodesList)
  private
    FElements: IXMLNodeList;
  public
    constructor Create(const Elements: IXMLNodeList);
    function GetEnumerator: INodesEnumerator;
    function Count: Integer;
  end;

  TValuesList = class(TInterfacedObject, IValuesList)
  private
    FElements: IXMLNodeList;
  public
    constructor Create(const Elements: IXMLNodeList);
    function GetEnumerator: IValuesEnumerator;
    function Count: Integer;
  end;

  TDocumentFilterChain = class(TDocumentFilterData, IDocumentFilterChain)
  private
    FFilterList: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DirectFilterIn(const SourceStream, TargetStream: TStream); overload; override;
    procedure DirectFilterOut(const SourceStream, TargetStream: TStream); overload; override;
    procedure AddFilter(const Filter: IDocumentFilterData);
    function Count: Integer;
    procedure Clear;
  end;

  TDocumentFilter = class(TInterfacedObject, IDocumentFilter)
  private
    FFilterChain: IDocumentFilterChain;
    FStorage: ISimpleStorage;
  public
    constructor Create(const Storage: ISimpleStorage; const FilterChain: IDocumentFilterChain);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);
    function Filter(const Filter: ICustomFilterData): IDocumentFilter;
  end;

  TSimpleStorage = class(TElement, ISimpleStorage)
  private
    FDataXML: IXMLDocument;
    FLoadStatus: TLoadStatus;
    procedure FillLoadError(const Success: Boolean);
    procedure InternalCreate(const RootNode: XmlString);
    procedure InitializeStorage(const RootNode: XmlString = '');
  public
    constructor Create(const RootNode: XmlString); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(const FileName: XmlString);
    procedure SaveToStream(const OutStream: TStream);
    procedure LoadFromXML(const XML: XmlString);
    procedure LoadFromFile(const FileName: XmlString);
    procedure LoadFromStream(const InStream: TStream);
    procedure LoadFromXMLDocument(const Document: IXMLDocument);
    function Content(const Structured: Boolean = False): XmlString;
    function Filter(const Data: ICustomFilterData): IDocumentFilter; overload;
    function LastLoadStatus: TLoadStatus;
    function XMLDocument: IXMLDocument;
  end;

  // normal attribute data proxy
  TAttrNormalProxy = class(TStorageData)
  protected
    function DoLoadValueAsString: XmlString; override;
    procedure DoLoadValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsString(const Value: XmlString); override;
  end;

  // normal element (node) data proxy
  TNodeNormalProxy = class(TStorageData)
  protected
    function DoLoadValueAsString: XmlString; override;
    procedure DoLoadValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsString(const Value: XmlString); override;
  end;

  // normal element (node) data proxy
  TCDataProxy = class(TStorageData)
  protected
    function DoLoadValueAsString: XmlString; override;
    procedure DoLoadValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsStream(const Value: TStream); override;
    procedure DoSaveValueAsString(const Value: XmlString); override;
  end;

  TCustomPlugin = class
  private
    FName: string;
  public
    property Name: string read FName;
  end;

  TFilterPlugin = class(TCustomPlugin)
  private
    FDataClass: TStorageDataClass;
  public
    constructor Create(const Name: string; const DataClass: TStorageDataClass);
    property DataClass: TStorageDataClass read FDataClass;
  end;

  TAdapterPlugin = class(TCustomPlugin)
  private
    FDataClass: TAdapterDataClass;
  public
    constructor Create(const Name: string; const DataClass: TAdapterDataClass);
    property DataClass: TAdapterDataClass read FDataClass;
  end;

  TPluginList = class
  private
    FFilterList: TList;
    FAdapterList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFilter(const Name: string; const Node: IXMLNode): IValueFilterData;
    function GetAdapter(const Name: string; const Element: IElement): IAdapterData;
    procedure RegisterFilter(const Name: string; const DataClass: TStorageDataClass);
    procedure RegisterAdapter(const Name: string; const DataClass: TAdapterDataClass);
  end;

var
  Plugins: TPluginList;

// function that registers the global filter for all simple storages
procedure RegisterFilter(const Name: string; const DataClass: TStorageDataClass);
begin
  if not Assigned(Plugins) then
    Plugins := TPluginList.Create;

  // register the filter with name
  Plugins.RegisterFilter(Name, DataClass);
end;

// function that registers the global filter for all simple storages
procedure RegisterAdapter(const Name: string; const DataClass: TAdapterDataClass);
begin
  if not Assigned(Plugins) then
    Plugins := TPluginList.Create;

  // register the adapter with name
  Plugins.RegisterAdapter(Name, DataClass);
end;

// *****************************************************************************************
//  begin of the constructor functions section
// *****************************************************************************************

function CreateStorage(const RootNode: XmlString): ISimpleStorage;
begin
  if RootNode <> '' then
    Result := TSimpleStorage.Create(RootNode)
  else
    Result := TSimpleStorage.Create(DATA_ROOT);
end;

function StorageFromXML(const XML: XmlString): ISimpleStorage;
begin
  Result := TSimpleStorage.Create(DATA_ROOT);
  Result.LoadFromXML(XML);
end;

function StorageFromFile(const FileName: XmlString): ISimpleStorage;
begin
  Result := TSimpleStorage.Create(DATA_ROOT);
  Result.LoadFromFile(FileName);
end;

function StorageFromStream(const Stream: TStream): ISimpleStorage;
begin
  Result := TSimpleStorage.Create(DATA_ROOT);
  Result.LoadFromStream(Stream);
end;

function StorageFromElement(const Element: IElement): ISimpleStorage;
var
  SubElement: IElement;
begin
  Result := TSimpleStorage.Create(Element.Name);
  Result.Assign(Element);

  for SubElement in Element.Nodes do
    Result.Append(SubElement, True);

  for SubElement in Element.Values do
    Result.Append(SubElement, False);
end;

function StorageFromXMLDocument(const Document: IXMLDocument): ISimpleStorage;
begin
  Result := TSimpleStorage.Create(DATA_ROOT);
  Result.LoadFromXMLDocument(Document);
end;

function ElementFromXMLNode(const Node: IXMLNode): IElement;
begin
  Result := TElement.Create(NodeNormalProxy(Node));
end;

function CreateDocumentFilterChain: IDocumentFilterChain;
begin
  Result := TDocumentFilterChain.Create;
end;

// *****************************************************************************************
//  end of the constructor functions section
// *****************************************************************************************

// *****************************************************************************************
//  begin of the proxy data constructor functions
// *****************************************************************************************

function AttrNormalProxy(const ElementNode: IXMLNode): IStorageData;
begin
  Result := TAttrNormalProxy.Create(ElementNode);
end;

function NodeNormalProxy(const ElementNode: IXMLNode): IStorageData;
begin
  Result := TNodeNormalProxy.Create(ElementNode);
end;

function CDataProxy(const ElementNode: IXMLNode): IStorageData;
begin
  Result := TCDataProxy.Create(ElementNode);
end;

// *****************************************************************************************
//  end of the proxy data constructor functions
// *****************************************************************************************

{ TTagData }

procedure TSimpleStorage.Clear;
begin
  InternalCreate(Name);
end;

function TSimpleStorage.Content(const Structured: Boolean): XmlString;
{$IFNDEF USE_MSXML}
var
  Buffer: AnsiString;
  MemoryStream: TMemoryStream;
{$ENDIF}
begin
{$IFNDEF USE_MSXML}
  if Structured then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      FDataXML.SaveToStream(MemoryStream, ofIndent);
      MemoryStream.Seek(0, soFromBeginning);
      SetLength(Buffer, MemoryStream.Size);

      // write the content from the stream to result buffer
      MemoryStream.Read(Buffer[1], MemoryStream.Size);
      {$IF Defined(UNICODE)}
        Result := UTF8ToString(Buffer);
      {$ELSE}
        Result := UTF8Decode(Buffer);
      {$IFEND}
    finally
      MemoryStream.Free;
    end;
  end
  else
    Result := FDataXML.XML;
{$ELSE}
  Result := FDataXML.XML;
{$ENDIF}
end;

constructor TSimpleStorage.Create(const RootNode: XmlString);
begin
  InternalCreate(RootNode);
end;

destructor TSimpleStorage.Destroy;
begin
  FDataXML := nil;

  inherited;
end;

procedure TSimpleStorage.FillLoadError(const Success: Boolean);
begin
  if not Success then
  begin
    FLoadStatus.Error.ErrorCode := FDataXML.ParseError.ErrorCode;
    FLoadStatus.Error.SrcText := FDataXML.ParseError.SrcText;
    FLoadStatus.Error.FilePos := FDataXML.ParseError.FilePos;
    FLoadStatus.Error.LinePos := FDataXML.ParseError.LinePos;
    FLoadStatus.Error.Reason := FDataXML.ParseError.Reason;
    FLoadStatus.Error.Line := FDataXML.ParseError.Line;
  end
  else
  begin
    FLoadStatus.Error.ErrorCode := -1;
    FLoadStatus.Error.SrcText := '';
    FLoadStatus.Error.FilePos := -1;
    FLoadStatus.Error.LinePos := -1;
    FLoadStatus.Error.Reason := '';
    FLoadStatus.Error.Line := -1;
  end;
end;

function TSimpleStorage.Filter(const Data: ICustomFilterData): IDocumentFilter;
var
  FilterChain: IDocumentFilterChain;
begin
  FilterChain := CreateDocumentFilterChain;
  FilterChain.AddFilter(Data.DocumentFilterData);

  // create the storage filter and add the chain
  Result := TDocumentFilter.Create(Self, FilterChain);
end;

procedure TSimpleStorage.InitializeStorage(const RootNode: XmlString);
begin
  if FDataXML.DocumentElement = nil then
    FStorageData := NodeNormalProxy(FDataXML.AppendChild(FDataXML.CreateElement(RootNode)))
  else
    FStorageData := NodeNormalProxy(FDataXML.DocumentElement);
end;

procedure TSimpleStorage.InternalCreate(const RootNode: XmlString);
begin
  FDataXML := nil;
  FDataXML := CreateXMLDoc;
  FDataXML.PreserveWhiteSpace := False;

  // reset the load status
  FLoadStatus.Success := True;
  FillLoadError(FLoadStatus.Success);

  // always initialize data
  InitializeStorage(RootNode);
end;

function TSimpleStorage.LastLoadStatus: TLoadStatus;
begin
  Result := FLoadStatus;
end;

procedure TSimpleStorage.LoadFromFile(const FileName: XmlString);
begin
  // clear the storage
  InternalCreate(Name);
  // load xml from file
  FLoadStatus.Success := FDataXML.Load(FileName);
  FillLoadError(FLoadStatus.Success);
  // always initialize data
  InitializeStorage;
end;

procedure TSimpleStorage.LoadFromStream(const InStream: TStream);
{$IFDEF USE_MSXML}
var
  Buffer: XmlString;
{$ENDIF}
begin
  // clear the storage
  InternalCreate(Name);

  // load if not empty
  if InStream.Size > 0 then
  begin
    {$IFNDEF USE_MSXML}
      FLoadStatus.Success := FDataXML.LoadFromStream(InStream);
    {$ELSE}
      SetLength(Buffer, InStream.Size - InStream.Position);
      InStream.Read(Buffer[1], InStream.Size - InStream.Position);
      FLoadStatus.Success := FDataXML.loadXML(Buffer);
    {$ENDIF}
  end;

  FillLoadError(FLoadStatus.Success);
  // always initialize data
  InitializeStorage;
end;

procedure TSimpleStorage.LoadFromXML(const XML: XmlString);
begin
  // clear the storage
  InternalCreate(Name);

  // load if not empty
  if XML <> '' then
    FLoadStatus.Success := FDataXML.LoadXML(XML);

  FillLoadError(FLoadStatus.Success);
  // always initialize data
  InitializeStorage;
end;

procedure TSimpleStorage.LoadFromXMLDocument(const Document: IXMLDocument);
begin
  // clear the storage
  InternalCreate(Name);
  // assign the document
  FDataXML := Document;
  InitializeStorage;
end;

procedure TSimpleStorage.SaveToFile(const FileName: XmlString);
begin
  FDataXML.Save(FileName{$IFNDEF USE_MSXML}, ofIndent{$ENDIF});
end;

procedure TSimpleStorage.SaveToStream(const OutStream: TStream);
{$IFDEF USE_MSXML}
var
  XMLAsUTF8: AnsiString;
{$ENDIF}
begin
{$IFNDEF USE_MSXML}
  FDataXML.SaveToStream(OutStream, ofIndent);
{$ELSE}
  XMLAsUTF8 := UTF8Encode(FDataXML.XML);
  OutStream.Write(XMLAsUTF8[1], Length(XMLAsUTF8));
{$ENDIF}
end;

function TSimpleStorage.XMLDocument: IXMLDocument;
begin
  Result := FDataXML;
end;

{ TElementsList }

function TElementsList.Count: Integer;
begin
  if FElements <> nil then
    Result := FElements.Length
  else
    Result := 0;
end;

constructor TElementsList.Create(const Elements: IXMLNodeList);
begin
  FElements := Elements;
end;

function TElementsList.GetEnumerator: IElementsEnumerator;
begin
  Result := TElementsEnumerator.Create(FElements);
end;

function TElementsList._GetFirst: IElement;
begin
  if FElements.Length > 0 then
    Result := TElement.Create(NodeNormalProxy(FElements.Item[0]))
  else
    Result := TElement.Create(NodeNormalProxy(nil));
end;

function TElementsList._GetItem(const Index: Integer): IElement;
begin
  Result := TElement.Create(NodeNormalProxy(FElements.Item[Index]));
end;

function TElementsList._GetLast: IElement;
begin
  if FElements.Length > 0 then
    Result := TElement.Create(NodeNormalProxy(FElements.Item[FElements.Length - 1]))
  else
    Result := TElement.Create(NodeNormalProxy(nil));
end;

{ TValuesList }

function TValuesList.Count: Integer;
var
  Element: IElement;
begin
  Result := 0;

  for Element in Self do
    Inc(Result);
end;

constructor TValuesList.Create(const Elements: IXMLNodeList);
begin
  FElements := Elements;
end;

function TValuesList.GetEnumerator: IValuesEnumerator;
begin
  Result := TValuesEnumerator.Create(FElements);
end;

{ TNodesList }

function TNodesList.Count: Integer;
var
  Element: IElement;
begin
  Result := 0;

  for Element in Self do
    Inc(Result);
end;

constructor TNodesList.Create(const Elements: IXMLNodeList);
begin
  FElements := Elements;
end;

function TNodesList.GetEnumerator: INodesEnumerator;
begin
  Result := TNodesEnumerator.Create(FElements);
end;

{ TValuesEnumerator }

function TValuesEnumerator.MoveNext: Boolean;
begin
  if FElements <> nil then
  begin
    repeat
      FCurrentNode := FElements.NextNode;
      if (FCurrentNode = nil) then
        Break;
    until FCurrentNode.SelectNodes('*').Length = 0;
  end;

  Result := FCurrentNode <> nil;
end;

{ TElement }

function TElement.Append(const Path: array of XmlString): IElement;
begin
  Result := Append(DoConstructPath(Path));
end;

procedure TElement.Append(const Nodes: INodesList; const InDepth: Boolean);
var
  Element: IElement;
begin
  for Element in Nodes do
    Append(Element, InDepth);
end;

procedure TElement.Append(const Values: IValuesList);
var
  Element: IElement;
begin
  for Element in Values do
    Append(Element, False);
end;

procedure TElement.AppendAllElements(const Element, Node: IElement; const Recurse: Boolean);
var
  TempNode: IElement;
begin
  if Recurse then // do we go in depth?
  begin
    for TempNode in Element.Elements do
    begin
      case TempNode.HasElements of
        True: AppendAllElements(TempNode, Node.Append(TempNode, False), Recurse);
        False: Node.Append(TempNode.Name).Assign(TempNode);
      end;
    end;
  end;
end;

function TElement.EnsureAttr(const Path, Name: XmlString): IValue;
begin
  Result := Ensure(Path).Attributes.Ensure(Name);
end;

function TElement.Ensure(const Path: array of XmlString): IElement;
begin
  Result := Ensure(DoConstructPath(Path));
end;

function TElement.EnsureAttr(const Name: XmlString): IValue;
begin
  Result := Attributes.Ensure(Name);
end;

function TElement.Exists(const Path: array of XmlString): Boolean;
begin
  Result := Exists(DoConstructPath(Path));
end;

function TElement.GetAttr(const Path, Name: XmlString): IValue;
begin
  Result := Get(Path).Attributes.Get(Name);
end;

function TElement.Get(const Path: array of XmlString): IElement;
begin
  Result := Get(DoConstructPath(Path));
end;

function TElement.GetAttr(const Name: XmlString): IValue;
begin
  Result := Attributes.Get(Name);
end;

function TElement.Attributes: IAttributes;
begin
  Result := TAttributes.Create(FStorageData.ElementNode);
end;

function TElement.Filter(const Name: string): IValueFilter;
var
  Data: IStorageData;
begin
  Data := Plugins.GetFilter(Name, FStorageData.ElementNode);

  case Data <> nil of
    True: Result := TValueFilter.Create(Data);
    False: raise Exception.CreateFmt('No filter named "%s" is registered', [Name]);
  end;
end;

function TElement.Filter(const Data: ICustomFilterData): IValueFilter;
begin
  Result := TValueFilter.Create(Data.ValueFilterData(FStorageData.ElementNode));
end;

function TElement.FirstChild: IElement;
var
  ChildElements: IElementsList;
begin
  ChildElements := Elements;

  case ChildElements.Count > 0 of
    True: Result := ChildElements.Item[0];
    False: Result := TElement.Create(NodeNormalProxy(nil));
  end;
end;

procedure TElement.DeleteAllChildNodes(const ParentNode: IXMLNode);
var
  I: Integer;
begin
  for I := 0 to ParentNode.ChildNodes.Length - 1 do
    DeleteAllChildNodes(ParentNode.ChildNodes.Item[I]);

  for I := ParentNode.ChildNodes.Length - 1 downto 0 do
    ParentNode.RemoveChild(ParentNode.ChildNodes.Item[I]);
end;

function TElement.DoConstructPath(const Path: array of XmlString): XmlString;
var
  PathList: TStringList;
  PathString: XmlString;
begin
  PathList := TStringList.Create;
  try
    PathList.StrictDelimiter := True;
    PathList.Delimiter := '/';

    for PathString in Path do
      if Trim(PathString) <> '' then
        PathList.Add(PathString);

    Result := PathList.DelimitedText;
  finally
    PathList.Free;
  end;
end;

function TElement.Values(const Params: XmlString): IValuesList;
begin
  if FStorageData.ElementNode <> nil then
    Result := TValuesList.Create(FStorageData.ElementNode.SelectNodes(Params))
  else
    Result := TValuesList.Create(nil);
end;

function TElement.Nodes(const Params: XmlString): INodesList;
begin
  if FStorageData.ElementNode <> nil then
    Result := TNodesList.Create(FStorageData.ElementNode.SelectNodes(Params))
  else
    Result := TNodesList.Create(nil);
end;

procedure TElement.Remove(const Path: XmlString; const Compact: Boolean);
var
  Element: IElement;
begin
  Element := Get(Path);

  if Element.IsValid then
    if Element.Parent.IsValid then
      Element.Parent.Remove(Element, Compact);
end;

procedure TElement.Remove(const Element: IElement; const Compact: Boolean);
begin
  if Element.IsValid then
  begin
    if IsValid then
    begin
      if FStorageData.ElementNode.ChildNodes.IndexOf(Element.XMLNode) > -1 then
      begin
        // delete all node downwards
        DeleteAllChildNodes(Element.XMLNode);
        FStorageData.ElementNode.RemoveChild(Element.XMLNode);

        // check if we need to prune the upper node tree
        if Compact and (Self.Elements.Count = 0) and Parent.IsValid then
          Parent.Remove(Self, Compact);
      end;
    end;
  end;
end;

procedure TElement.RemoveAllElements;
var
  Element: IElement;
begin
  for Element in Elements do
    Remove(Element);
end;

function TElement.Storage(const Path: array of XmlString): ISimpleStorage;
begin
  Result := Storage(DoConstructPath(Path));
end;

function TElement.Adapter(const Name: string): IAdapter;
var
  Data: IAdapterData;
begin
  Data := Plugins.GetAdapter(Name, Self);

  case Data <> nil of
    True: Result := TElementAdapter.Create(Data);
    False: raise Exception.CreateFmt('No adapter named "%s" is registered', [Name]);
  end;
end;

function TElement.Adapter(const Data: IAdapterData): IAdapter;
begin
  Data.Element := Self;
  Result := TElementAdapter.Create(Data);
end;

function TElement.Append(const Path: XmlString): IElement;
var
  ListRoot: IXMLNode;
begin
  // append the child internally
  ListRoot := InternalAppend(Path, True);
  // always return the resulting node
  Result := TElement.Create(NodeNormalProxy(ListRoot));
end;

function TElement.Append(const Element: IElement; const InDepth: Boolean): IElement;
begin
  if Element.IsValid then
  begin
    Result := Append(Element.Name);
    Result.Assign(Element, InDepth);
  end;
end;

procedure TElement.Append(const Elements: IElementsList; const InDepth: Boolean);
var
  Element: IElement;
begin
  for Element in Elements do
    Append(Element, InDepth);
end;

procedure TElement.Assign(const Element: IElement; const InDepth: Boolean);
begin
  // remove all
  RemoveAllElements;

  // only if value element
  if not Element.HasElements then
  begin
    case GetCDataChild(Element.XMLNode) <> nil of
      True: SetCDataChild(FStorageData.ElementNode, Element.AsString);
      False: FStorageData.ElementNode.Text := Element.AsString;
    end;
  end;

  // assign all of the attributes
  Attributes.Assign(Element.Attributes);
  // assign the elements if specified
  AppendAllElements(Element, Self, InDepth);
end;

procedure TElement.Assign(const Nodes: INodesList; const InDepth: Boolean);
var
  Element: IElement;
begin
  for Element in Nodes do
    Assign(Element, InDepth);
end;

procedure TElement.Assign(const Nodes: IElementsList; const InDepth: Boolean);
var
  Element: IElement;
begin
  for Element in Elements do
    Assign(Element, InDepth);
end;

function TElement.AssignUniqueValueNode(const Template, Target: IElement;
  const IndexList: TList): IElement;
var
  Match: Boolean;
  Element: IElement;

  function CheckAttributes(const Source, Target: IElement): Boolean;
  var
    Attribute: IValue;
    TargetAttr: IValue;
  begin
    Result := True;
    
    for Attribute in Source.Attributes do
    begin
      TargetAttr := Target.Attributes.Get(Attribute.Name);

      if not TargetAttr.IsValid then
      begin
        Result := False;
        Break;
      end;

      if TargetAttr.AsString <> Attribute.AsString then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

begin
  for Element in Target.Elements do
  begin
    if Element.Name = Template.Name then
    begin
      // check all of template attributes
      Match := CheckAttributes(Template, Element);

      if Match then
        // if stil a match check element attributes
        Match := CheckAttributes(Element, Template);

      // found it?
      if Match and (IndexList.IndexOf(Pointer(Element.NodeIndex)) = - 1) then
      begin
        // add new index to the list for search
        IndexList.Add(Pointer(Element. NodeIndex));
        Result := Element;
        Exit;
      end;
    end;
  end;

  // no node was found, create new one
  Result := Target.Append(Template.Name);
  // add new index to the list for search
  IndexList.Add(Pointer(Result. NodeIndex));
end;

function TElement.Elements(const Params: XmlString): IElementsList;
begin
  if FStorageData.ElementNode <> nil then
    Result := TElementsList.Create(FStorageData.ElementNode.SelectNodes(Params))
  else
    Result := TElementsList.Create(nil);
end;

function TElement.Ensure(const Path: XmlString): IElement;
var
  ListRoot: IXMLNode;
begin
  ListRoot := FStorageData.ElementNode.SelectSingleNode(Path);

  if ListRoot = nil then
    ListRoot := InternalAppend(Path, False);

  // always return the resulting node
  Result := TElement.Create(NodeNormalProxy(ListRoot));
end;

function TElement.EnsureAttr(const Path: array of XmlString; const Name: XmlString): IValue;
begin
  Result := EnsureAttr(DoConstructPath(Path), Name);
end;

function TElement.Exists(const Path: XmlString): Boolean;
begin
  if FStorageData.ElementNode <> nil then
    Result := FStorageData.ElementNode.SelectSingleNode(Path) <> nil
  else
    Result := False;
end;

function TElement.Storage(const Path: XmlString): ISimpleStorage;
begin
  if Path <> '' then
    Result := StorageFromElement(Get(Path))
  else
    Result := StorageFromElement(Self);
end;

function TElement._GetElementType: TElementType;
begin
  if not HasElements then
    Result := etValue
  else
    Result := etNode;
end;

function TElement._GetHasElements: Boolean;
begin
  if FStorageData.ElementNode <> nil then
    Result := FStorageData.ElementNode.SelectNodes('*').Length > 0
  else
    Result := False;
end;

function TElement._GetHasNodes: Boolean;
var
  Element: IElement;
begin
  Result := False;

  for Element in Nodes do
  begin
    Result := True;
    Exit;
  end;
end;

function TElement._GetHasValues: Boolean;
var
  Element: IElement;
begin
  Result := False;

  for Element in Values do
  begin
    Result := True;
    Exit;
  end;
end;

function TElement._GetNodeIndex: Integer;
begin
  Result := -1;

  if FStorageData.ElementNode.ParentNode <> nil then
    if FStorageData.ElementNode.ParentNode.HasChildNodes then
      Result := FStorageData.ElementNode.ParentNode.ChildNodes.IndexOf(FStorageData.ElementNode);
end;

function TElement._GetParent: IElement;
begin
  if FStorageData.ElementNode <> nil then
    Result := TElement.Create(NodeNormalProxy(FStorageData.ElementNode.ParentNode))
  else
    Result := TElement.Create(NodeNormalProxy(nil));
end;

function TElement._GetRootElement: IElement;
var
  RootNode: IXMLNode;
begin
  RootNode := FStorageData.ElementNode.OwnerDocument.DocumentElement;
  Result := TElement.Create(NodeNormalProxy(RootNode));
end;

function TElement._GetXMLNode: IXMLNode;
begin
  Result := FStorageData.ElementNode;
end;

function TElement.Get(const Path: XmlString): IElement;
begin
  case FStorageData.ElementNode <> nil of
    True: Result := TElement.Create(NodeNormalProxy(FStorageData.ElementNode.SelectSingleNode(Path)));
    False: Result := TElement.Create(NodeNormalProxy(nil));
  end;
end;

function TElement.GetAttr(const Path: array of XmlString; const Name: XmlString): IValue;
begin
  Result := GetAttr(DoConstructPath(Path), Name);
end;

function TElement.InternalAppend(const Path: XmlString; const Append: Boolean): IXMLNode;
var
  I: Integer;
  Attr: IXMLNode;
  ListNode: IXMLNode;
  PathList: TStringList;
  NodePath: XmlString;
  AttrName: XmlString;
  Attribute: XmlString;
  AttrValue: XmlString;
begin
  Result := FStorageData.ElementNode;

  PathList := TStringList.Create;
  try
    PathList.StrictDelimiter := True;
    PathList.Delimiter := '/';

    // delimit the path to the list
    PathList.DelimitedText := Path;

    // ensure the whole path
    for I := 0 to PathList.Count - 1 do
    begin
      case Append of
        False: ListNode := Result.SelectSingleNode(PathList[I]);
        True: ListNode := nil;
      end;

      if ListNode = nil then
      begin
        Attribute := StrBetween('[', ']', PathList[I]);
        NodePath := PathList[I];

        case Attribute <> '' of
          True: NodePath := StrBefore('[', PathList[I]);
          False: NodePath := PathList[I];
        end;

        // we always construct the one with the path as the node name
        Result := Result.AppendChild(Result.OwnerDocument.CreateElement(NodePath));

        // recreate attribute
        if Attribute <> '' then
        begin
          if Pos('=', Attribute) = 0 then
            raise Exception.Create('Ambiguous attribute value. Cannot recreate!')
          else
          begin
            AttrValue := Trim(StrAfter('=', Attribute));
            AttrValue := Copy(AttrValue, 2, Length(AttrValue) - 2);
            AttrName := Trim(StrBetween('@', '=', Attribute));

            Attr := Result.OwnerDocument.CreateAttribute(AttrName);
            Attr.NodeValue := AttrValue;
            Result.Attributes.Add(Attr);
          end;
        end;
      end
      else
        Result := ListNode;
    end;
  finally
    PathList.Free;
  end;
end;

procedure TElement.Merge(const Element: IElement);
begin
  // only if value element
  if not Element.HasElements then
  begin
    case GetCDataChild(Element.XMLNode) <> nil of
      True: SetCDataChild(FStorageData.ElementNode, Element.AsString);
      False: FStorageData.ElementNode.Text := Element.AsString;
    end;
  end;

  // assign all of the attributes
  Attributes.Update(Element.Attributes);
  // assign the elements if specified
  MergeAllElements(Element, Self);
end;

procedure TElement.MergeAllElements(const Element, Node: IElement);
var
  TempNode: IElement;
  IndexList: TList;
  UniqueNode: IElement;
begin
  IndexList := TList.Create;
  try
    for TempNode in Element.Nodes do // enumerate the whole tree
    begin
      UniqueNode := AssignUniqueValueNode(TempNode, Node, IndexList);
      UniqueNode.Attributes.Update(TempNode.Attributes);
      MergeAllElements(TempNode, UniqueNode);
    end;

    // enumerate all and assign
    for TempNode in Element.Values do
    begin
      UniqueNode := AssignUniqueValueNode(TempNode, Node, IndexList);
      UniqueNode.Merge(TempNode);
    end;
  finally
    IndexList.Free;
  end;
end;

{ TNodesEnumerator }

function TNodesEnumerator.MoveNext: Boolean;
begin
  if FElements <> nil then
  begin
    repeat
      FCurrentNode := FElements.NextNode;
      if (FCurrentNode = nil) then
        Break;
    until FCurrentNode.SelectNodes('*').Length > 0;
  end;

  Result := FCurrentNode <> nil;
end;

{ TBaseEnumerator }

constructor TBaseEnumerator.Create(const Elements: IXMLNodeList);
begin
  if Elements <> nil then
  begin
    FElements := Elements;
    FElements.Reset;
  end;

  FCurrentNode := nil;
end;

function TBaseEnumerator._GetCurrent: IElement;
begin
  Result := TElement.Create(NodeNormalProxy(FCurrentNode));
end;

{ TElementsEnumerator }

function TElementsEnumerator.MoveNext: Boolean;
begin
  if FElements <> nil then
    FCurrentNode := FElements.NextNode;

  Result := FCurrentNode <> nil;
end;

{ TValue }

function TValue.AsColorDef(const DefValue: TColor): TColor;
begin
  if FStorageData.ElementNode <> nil then
    Result := WebColorStrToColor(FStorageData.LoadValueAsString)
  else
    Result := DefValue;
end;

function TValue.AsDateTimeDef(const DefValue: TDateTime): TDateTime;
begin
  if FStorageData.ElementNode <> nil then
    Result := XMLStrToDateTimeDef(FStorageData.LoadValueAsString, DefValue)
  else
    Result := DefValue;
end;

function TValue.AsBooleanDef(const DefValue: Boolean): Boolean;
begin
  if FStorageData.ElementNode <> nil then
    Result := XMLStrToBoolDef(FStorageData.LoadValueAsString, DefValue)
  else
    Result := DefValue;
end;

function TValue.AsFloatDef(const DefValue: Real): Real;
begin
  if FStorageData.ElementNode <> nil then
    Result := XMLStrToRealDef(FStorageData.LoadValueAsString, DefValue)
  else
    Result := DefValue;
end;

function TValue.AsInt64Def(const DefValue: Int64): Int64;
begin
  if FStorageData.ElementNode <> nil then
    Result := XMLStrToInt64Def(FStorageData.LoadValueAsString, DefValue)
  else
    Result := DefValue;
end;

function TValue.AsIntegerDef(const DefValue: Integer): Integer;
begin
  if FStorageData.ElementNode <> nil then
    Result := XMLStrToIntDef(FStorageData.LoadValueAsString, DefValue)
  else
    Result := DefValue;
end;

function TValue.AsStringDef(const DefValue: XmlString): XmlString;
begin
  if FStorageData.ElementNode <> nil then
    Result := FStorageData.LoadValueAsString
  else
    Result := DefValue;
end;

function TValue._GetAsBinary: IBinary;
begin
  Result := TBinary.Create(FStorageData);
end;

function TValue._GetAsBoolean: Boolean;
begin
  Result := XMLStrToBool(FStorageData.LoadValueAsString);
end;

function TValue._GetAsCData: ICData;
begin
  Result := TCData.Create(CDataProxy(FStorageData.ElementNode));
end;

function TValue._GetAsColor: TColor;
begin
  Result := WebColorStrToColor(FStorageData.LoadValueAsString);
end;

function TValue._GetAsDate: TDate;
begin
  Result := XMLStrToDate(FStorageData.LoadValueAsString);
end;

function TValue._GetAsDateTime: TDateTime;
begin
  Result := XMLStrToDateTime(FStorageData.LoadValueAsString);
end;

function TValue._GetAsFloat: Real;
begin
  Result := XMLStrToReal(FStorageData.LoadValueAsString);
end;

function TValue._GetAsInt64: Int64;
begin
  Result := XMLStrToInt64(FStorageData.LoadValueAsString);
end;

function TValue._GetAsInteger: Integer;
begin
  Result := XMLStrToInt(FStorageData.LoadValueAsString);
end;

function TValue._GetAsString: XmlString;
begin
  Result := FStorageData.LoadValueAsString;
end;

function TValue._GetAsTime: TTime;
begin
  Result := XMLStrToTime(FStorageData.LoadValueAsString);
end;

function TValue._GetName: XmlString;
begin
  Result := FStorageData.ElementNode.NodeName;
end;

procedure TValue._SetAsBoolean(Value: Boolean);
begin
  FStorageData.SaveValueAsString(XMLBoolToStr(Value));
end;

procedure TValue._SetAsColor(Value: TColor);
begin
  FStorageData.SaveValueAsString(ColorToWebColorStr(Value));
end;

procedure TValue._SetAsDate(Value: TDate);
begin
  FStorageData.SaveValueAsString(XMLDateToStr(Value));
end;

procedure TValue._SetAsDateTime(Value: TDateTime);
begin
  FStorageData.SaveValueAsString(XMLDateTimeToStr(Value));
end;

procedure TValue._SetAsFloat(Value: Real);
begin
  FStorageData.SaveValueAsString(XMLRealToStr(Value));
end;

procedure TValue._SetAsInt64(Value: Int64);
begin
  FStorageData.SaveValueAsString(XMLInt64ToStr(Value));
end;

procedure TValue._SetAsInteger(Value: Integer);
begin
  FStorageData.SaveValueAsString(XMLIntToStr(Value));
end;

procedure TValue._SetAsString(Value: XmlString);
begin
  FStorageData.SaveValueAsString(Value);
end;

procedure TValue._SetAsTime(Value: TTime);
begin
  FStorageData.SaveValueAsString(XMLTimeToStr(Value));
end;

{ TAttributesEnumerator }

constructor TAttributesEnumerator.Create(const RootNode: IXMLNode);
begin
  FElementNode := RootNode;
  FCurrentNode := nil;
  FIndex := 0;
end;

function TAttributesEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if FIndex < FElementNode.Attributes.Length then
  begin
    FCurrentNode := FElementNode.Attributes.Item[FIndex];
    Result := True;
    Inc(FIndex);
  end;
end;

function TAttributesEnumerator._GetCurrent: IValue;
begin
  Result := TValue.Create(AttrNormalProxy(FCurrentNode));
end;

{ TAttributes }

procedure TAttributes.Assign(const Attributes: IAttributes);
var
  Attr: IValue;
begin
  for Attr in Self do
    Remove(Attr.Name);

  for Attr in Attributes do
    Ensure(Attr.Name).AsString := Attr.AsString;
end;

function TAttributes.Count: Integer;
begin
  Result := FElementNode.Attributes.Length;
end;

constructor TAttributes.Create(const ElementNode: IXMLNode);
begin
  FElementNode := ElementNode;
end;

function TAttributes.Ensure(const Name: XmlString): IValue;
var
  AttrNode: IXMLNode;
begin
  AttrNode := FElementNode.Attributes.GetNamedItem(Name);

  if AttrNode = nil then
  begin
    AttrNode := FElementNode.OwnerDocument.CreateAttribute(Name);
    FElementNode.Attributes.SetNamedItem(AttrNode);
  end;

  // always create the attribute
  Result := TValue.Create(AttrNormalProxy(AttrNode));
end;

function TAttributes.Exists(const Name: XmlString): Boolean;
begin
  if FElementNode <> nil then
    Result := FElementNode.Attributes.GetNamedItem(Name) <> nil
  else
    Result := False;
end;

function TAttributes.Get(const Name: XmlString): IValue;
begin
  case FElementNode <> nil of
    True: Result := TValue.Create(AttrNormalProxy(FElementNode.Attributes.GetNamedItem(Name)));
    False: Result := TValue.Create(AttrNormalProxy(nil));
  end;
end;

function TAttributes.GetEnumerator: IAttributesEnumerator;
begin
  Result := TAttributesEnumerator.Create(FElementNode);
end;

procedure TAttributes.Update(const Attributes: IAttributes);
var
  Attr: IValue;
begin
  for Attr in Attributes do
    Ensure(Attr.Name).AsString := Attr.AsString;
end;

procedure TAttributes.Remove(const Name: XmlString);
begin
  FElementNode.Attributes.RemoveNamedItem(Name);
end;

{ TBinary }

constructor TBinary.Create(const StorageData: IStorageData);
begin
  inherited;

  FMemoryStream := TBinaryStream.Create;
end;

destructor TBinary.Destroy;
begin
  FreeAndNil(FMemoryStream);

  inherited;
end;

procedure TBinary.LoadFromFile(const FileName: string; const Mode: Word);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or Mode);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TBinary.LoadFromBuffer(const Buffer: Pointer; const Size: Cardinal);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write(Buffer^, Size);
    MS.Seek(0, soFromBeginning);
    FStorageData.SaveValueAsStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TBinary.LoadFromElement(const Element: IElement);
var
  MS: TMemoryStream;
  SS: ISimpleStorage;
begin
  MS := TMemoryStream.Create;
  try
    SS := StorageFromElement(Element);
    SS.SaveToStream(MS);

    MS.Seek(0, soFromBeginning);
    FStorageData.SaveValueAsStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TBinary.LoadFromStream(const Stream: TStream);
begin
  FStorageData.SaveValueAsStream(Stream);
end;

procedure TBinary.LoadFromXML(const XML: XmlString);
begin
  LoadFromElement(StorageFromXML(XML));
end;

procedure TBinary.SaveToBuffer(var Buffer: Pointer);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    FStorageData.LoadValueAsStream(MS);
    MS.Seek(0, soFromBeginning);
    MS.Read(Buffer^, MS.Size);
  finally
    MS.Free;
  end;
end;

procedure TBinary.SaveToFile(const FileName: string; const FailIfNoData: Boolean);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS, FailIfNoData);
  finally
    FS.Free;
  end;
end;

procedure TBinary.SaveToStream(const Stream: TStream; const FailIfNoData: Boolean);
begin
  if (FStorageData.ElementNode <> nil) or FailIfNoData then
    FStorageData.LoadValueAsStream(Stream);
end;

function TBinary._GetStream: TStream;
begin
  // clea the stream
  FMemoryStream.Clear;

  // only return data if node is valid
  if FStorageData.ElementNode <> nil then
  begin
    FStorageData.LoadValueAsStream(FMemoryStream);
    FMemoryStream.Seek(0, soFromBeginning);
  end;

  // return the stream
  Result := FMemoryStream;
end;

{ TBinaryStream }

function TBinaryStream.CopyFrom(Source: TStream; Count: Int64): Int64;
begin
  raise Exception.Create('Do not use "CopyFrom", use "LoadFromStream" instead!');
end;

{ TProxyValue }

constructor TValueData.Create(const StorageData: IStorageData);
begin
  FStorageData := StorageData;
end;

function TValueData._GetIsValid: Boolean;
begin
  Result := FStorageData.ElementNode <> nil;
end;

{ TFilterList }

constructor TPluginList.Create;
begin
  FFilterList := TList.Create;
  FAdapterList := TList.Create;
end;

destructor TPluginList.Destroy;
var
  I: Integer;
begin
  // first free all the registered adapters
  for I := 0 to FAdapterList.Count - 1 do
    TAdapterPlugin(FAdapterList[I]).Free;

  // then free all the registered filters
  for I := 0 to FFilterList.Count - 1 do
    TFilterPlugin(FFilterList[I]).Free;

  // free the lists itself
  FreeAndNil(FAdapterList);
  FreeAndNil(FFilterList);

  inherited;
end;

function TPluginList.GetAdapter(const Name: string; const Element: IElement): IAdapterData;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FAdapterList.Count - 1 do
  begin
    if SameText(TAdapterPlugin(FAdapterList[I]).Name, Name) then
    begin
      Result := TAdapterPlugin(FAdapterList[I]).DataClass.Create;
      Result.Element := Element;
      Exit;
    end;
  end;
end;

function TPluginList.GetFilter(const Name: string; const Node: IXMLNode): IValueFilterData;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FFilterList.Count - 1 do
  begin
    if SameText(TFilterPlugin(FFilterList[I]).Name, Name) then
    begin
      Result := TFilterPlugin(FFilterList[I]).DataClass.Create(nil);
      Result.ElementNode := Node;
      Exit;
    end;
  end;
end;

procedure TPluginList.RegisterAdapter(const Name: string; const DataClass: TAdapterDataClass);
begin
  FAdapterList.Add(TAdapterPlugin.Create(Name, DataClass));
end;

procedure TPluginList.RegisterFilter(const Name: string; const DataClass: TStorageDataClass);
begin
  FFilterList.Add(TFilterPlugin.Create(Name, DataClass));
end;

{ TFilter }

constructor TFilterPlugin.Create(const Name: string; const DataClass: TStorageDataClass);
begin
  FName := Name;
  FDataClass := DataClass;
end;

{ TAdapter }

constructor TAdapterPlugin.Create(const Name: string; const DataClass: TAdapterDataClass);
begin
  FName := Name;
  FDataClass := DataClass;
end;

{ TElementAdapter }

constructor TElementAdapter.Create(const Data: IAdapterData);
begin
  FData := Data;
end;

procedure TElementAdapter.Load(const SourceObject: TObject);
begin
  FData.LoadAdapterData(SourceObject);
end;

procedure TElementAdapter.Save(const TargetObject: TObject);
begin
  FData.SaveAdapterData(TargetObject);
end;

{ TCData }

procedure TCData.LoadFromStream(const Stream: TStream);
begin
  FStorageData.SaveValueAsStream(Stream);
end;

procedure TCData.SaveToStream(const Stream: TStream; const FailIfNoData: Boolean);
begin
  if (FStorageData.ElementNode <> nil) or FailIfNoData then
    FStorageData.LoadValueAsStream(Stream);
end;

function TCData._GetData: IValue;
begin
  Result := TValue.Create(FStorageData);
end;

procedure TElement.Remove(const Path: array of XmlString; const Compact: Boolean);
begin
  Remove(DoConstructPath(Path), Compact);
end;

{ TCustomDocumentFilter }

procedure TDocumentFilterData.DirectFilterOut(const SourceFile, TargetFile: string);
var
  SourceStream: TFileStream;
  TargetStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
    try
      DirectFilterOut(SourceStream, TargetStream);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure TDocumentFilterData.DirectFilterOut(const SourceFile: string; const TargetStream: TStream);
var
  SourceStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DirectFilterOut(SourceStream, TargetStream);
  finally
    SourceStream.Free;
  end;
end;

procedure TDocumentFilterData.DirectFilterOut(const SourceStream: TStream; const TargetFile: string);
var
  TargetStream: TFileStream;
begin
  TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
  try
    DirectFilterOut(SourceStream, TargetStream);
  finally
    TargetStream.Free;
  end;
end;

procedure TDocumentFilterData.DirectFilterIn(const SourceStream: TStream; const TargetFile: string);
var
  TargetStream: TFileStream;
begin
  TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
  try
    DirectFilterIn(SourceStream, TargetStream);
  finally
    TargetStream.Free;
  end;
end;

procedure TDocumentFilterData.DirectFilterIn(const SourceFile: string; const TargetStream: TStream);
var
  SourceStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DirectFilterIn(SourceStream, TargetStream);
  finally
    SourceStream.Free;
  end;
end;

procedure TDocumentFilterData.DirectFilterIn(const SourceFile, TargetFile: string);
var
  SourceStream: TFileStream;
  TargetStream: TFileStream;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    TargetStream := TFileStream.Create(TargetFile, fmCreate or fmShareExclusive);
    try
      DirectFilterIn(SourceStream, TargetStream);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

function TDocumentFilterData.LoadFromFile(const FileName: string): ISimpleStorage;
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

function TDocumentFilterData.LoadFromStream(const Stream: TStream): ISimpleStorage;
var
  TargetStream: TMemoryStream;
begin
  TargetStream := TMemoryStream.Create;
  try
    Stream.Seek(0, soFromBeginning);
    DirectFilterOut(Stream, TargetStream);
    Result := StorageFromStream(TargetStream);
  finally
    TargetStream.Free;
  end;
end;

procedure TDocumentFilterData.SaveToFile(const Storage: ISimpleStorage; const FileName: string);
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

procedure TDocumentFilterData.SaveToStream(const Storage: ISimpleStorage; const Stream: TStream);
var
  SourceStream: TMemoryStream;
begin
  SourceStream := TMemoryStream.Create;
  try
    Storage.SaveToStream(SourceStream);
    SourceStream.Seek(0, soFromBeginning);
    DirectFilterIn(SourceStream, Stream);
  finally
    SourceStream.Free;
  end;
end;

{ TDocumentFilterChain }

constructor TDocumentFilterChain.Create;
begin
  FFilterList := TInterfaceList.Create;
end;

destructor TDocumentFilterChain.Destroy;
begin
  FreeAndNil(FFilterList);

  inherited;
end;

procedure TDocumentFilterChain.AddFilter(const Filter: IDocumentFilterData);
begin
  FFilterList.Add(Filter);
end;

procedure TDocumentFilterChain.Clear;
begin
  FFilterList.Clear;
end;

function TDocumentFilterChain.Count: Integer;
begin
  Result := FFilterList.Count;
end;

procedure TDocumentFilterChain.DirectFilterOut(const SourceStream, TargetStream: TStream);
var
  I: Integer;
  TempStream: TMemoryStream;
begin
  inherited;

  if FFilterList.Count = 0 then
  begin
    TargetStream.CopyFrom(SourceStream, 0);
    Exit;
  end;

  TempStream := TMemoryStream.Create;
  try
    TempStream.CopyFrom(SourceStream, 0);
    TempStream.Seek(0, soFromBeginning);

    for I := FFilterList.Count - 1 downto 0 do
    begin
      IDocumentFilterData(FFilterList.Items[I]).DirectFilterOut(TempStream, TargetStream);

      if I > 0 then
      begin
        TempStream.Clear;
        TempStream.CopyFrom(TargetStream, 0);
        TempStream.Seek(0, soFromBeginning);
        TargetStream.Size := 0;
      end;
    end;
  finally
    TempStream.Free;
  end;
end;

procedure TDocumentFilterChain.DirectFilterIn(const SourceStream, TargetStream: TStream);
var
  I: Integer;
  TempStream: TMemoryStream;
begin
  inherited;

  if FFilterList.Count = 0 then
  begin
    TargetStream.CopyFrom(SourceStream, 0);
    Exit;
  end;

  TempStream := TMemoryStream.Create;
  try
    TempStream.CopyFrom(SourceStream, 0);
    TempStream.Seek(0, soFromBeginning);

    for I := 0 to FFilterList.Count - 1 do
    begin
      IDocumentFilterData(FFilterList.Items[I]).DirectFilterIn(TempStream, TargetStream);

      if I < (FFilterList.Count - 1) then
      begin
        TempStream.Clear;
        TempStream.CopyFrom(TargetStream, 0);
        TempStream.Seek(0, soFromBeginning);
        TargetStream.Size := 0;
      end;
    end;
  finally
    TempStream.Free;
  end;
end;

{ TStorageData }

procedure TStorageData.LoadValueAsStream(const Value: TStream);
begin
  DoLoadValueAsStream(Value);
end;

function TStorageData.LoadValueAsString: XmlString;
begin
  Result := DoLoadValueAsString;
end;

procedure TStorageData.SaveValueAsStream(const Value: TStream);
begin
  DoSaveValueAsStream(Value);
end;

procedure TStorageData.SaveValueAsString(const Value: XmlString);
begin
  DoSaveValueAsString(Value);
end;

{ TAttrNormalProxy }

function TAttrNormalProxy.DoLoadValueAsString: XmlString;
begin
  Result := ElementNode.NodeValue;
end;

procedure TAttrNormalProxy.DoLoadValueAsStream(const Value: TStream);
begin
  Base64Decode(ElementNode.NodeValue, Value);
end;

procedure TAttrNormalProxy.DoSaveValueAsStream(const Value: TStream);
begin
  ElementNode.NodeValue := Base64Encode(Value);
end;

procedure TAttrNormalProxy.DoSaveValueAsString(const Value: XmlString);
begin
  ElementNode.NodeValue := Value;
end;

{ TNodeNormalProxy }

function TNodeNormalProxy.DoLoadValueAsString: XmlString;
begin
  Result := ElementNode.Text;
end;

procedure TNodeNormalProxy.DoLoadValueAsStream(const Value: TStream);
begin
  Base64Decode(ElementNode.Text, Value);
end;

procedure TNodeNormalProxy.DoSaveValueAsStream(const Value: TStream);
begin
  ElementNode.Text := Base64Encode(Value);
end;

procedure TNodeNormalProxy.DoSaveValueAsString(const Value: XmlString);
begin
  ElementNode.Text := Value;
end;

{ TCDataProxy }

function TCDataProxy.DoLoadValueAsString: XmlString;
begin
  Result := GetNodeCData(ElementNode);
end;

procedure TCDataProxy.DoLoadValueAsStream(const Value: TStream);
begin
  Base64Decode(GetNodeCData(ElementNode), Value);
end;

procedure TCDataProxy.DoSaveValueAsStream(const Value: TStream);
begin
   SetCDataChild(ElementNode, Base64Encode(Value));
end;

procedure TCDataProxy.DoSaveValueAsString(const Value: XmlString);
begin
  SetCDataChild(ElementNode, Value);
end;

{ TAdapterData }

function TAdapterData.GetElement: IElement;
begin
  Result := FElement;
end;

procedure TAdapterData.SetElement(const Value: IElement);
begin
  FElement := Value;
end;

procedure TAdapterData.LoadAdapterData(const DataObject: TObject);
begin
  DoLoadAdapterData(DataObject);
end;

procedure TAdapterData.SaveAdapterData(const DataObject: TObject);
begin
  DoSaveAdapterData(DataObject);
end;

{ TDocumentFilter }

constructor TDocumentFilter.Create(const Storage: ISimpleStorage; const FilterChain: IDocumentFilterChain);
begin
  FFilterChain := FilterChain;
  FStorage := Storage;
end;

function TDocumentFilter.Filter(const Filter: ICustomFilterData): IDocumentFilter;
begin
  FFilterChain.AddFilter(Filter.DocumentFilterData);
  Result := Self;
end;

procedure TDocumentFilter.LoadFromFile(const FileName: string);
begin
  FStorage.LoadFromXML(FFilterChain.LoadFromFile(FileName).Content);
end;

procedure TDocumentFilter.LoadFromStream(const Stream: TStream);
begin
  FStorage.LoadFromXML(FFilterChain.LoadFromStream(Stream).Content);
end;

procedure TDocumentFilter.SaveToFile(const FileName: string);
begin
  FFilterChain.SaveToFile(FStorage, FileName);
end;

procedure TDocumentFilter.SaveToStream(const Stream: TStream);
begin
  FFilterChain.SaveToStream(FStorage, Stream);
end;

{ TValueFilter }

function TValueFilter.Filter(const Data: ICustomFilterData): IValueFilter;
begin
  IValueFilterData(FStorageData).AddFilter(Data.ValueFilterData(FStorageData.ElementNode));
  Result := Self;
end;

{ TFilterData }

procedure TValueFilterData.AddFilter(const Filter: IValueFilterData);
var
  Temp: IValueFilterData;
begin
  if FNextFilter = nil then
    FNextFilter := Filter
  else
  begin
    Temp := FNextFilter;

    while Temp.NextFilter <> nil do
      Temp := FNextFilter.NextFilter;

    // assign filter data
    Temp.AddFilter(Filter);
  end;
end;

procedure TValueFilterData.LoadChainValue(const Value, Result: TStream);
begin
  DoLoadValueAsStream(Value, Result);
end;

procedure TValueFilterData.SaveChainValue(const Value, Result: TStream);
begin
  DoSaveValueAsStream(Value, Result);
end;

function TValueFilterData.GetNextFilter: IValueFilterData;
begin
  Result := FNextFilter;
end;

procedure TValueFilterData.InternalLoadValueAsStream(const Value: TStream);
var
  I: Integer;
  InStream: TMemoryStream;
  OutStream: TMemoryStream;
  FilterData: IValueFilterData;
  FilterList: TInterfaceList;
begin
  InStream := TMemoryStream.Create;
  try
    Base64Decode(FElementNode.Text, InStream);
    InStream.Seek(0, soFromBeginning);
    FilterData := Self;

    OutStream :=  TMemoryStream.Create;
    try
      FilterList := TInterfaceList.Create;
      try
        repeat
          FilterList.Add(FilterData);
          FilterData := FilterData.NextFilter;
        until FilterData = nil;

        for I := FilterList.Count - 1 downto 0 do
        begin
          OutStream.Clear;
          // get the next filter data in chain
          FilterData := IValueFilterData(FilterList[I]);
          FilterData.LoadChainValue(InStream, OutStream);

          if FilterData <> nil then
          begin
            InStream.Clear;
            InStream.CopyFrom(OutStream, 0);
            InStream.Seek(0, soFromBeginning);
          end;
        end;

        // copy from out stream
        Value.CopyFrom(OutStream, 0);
      finally
        FilterList.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure TValueFilterData.InternalSaveValueAsStream(const Value: TStream);
var
  InStream: TStream;
  OutStream: TMemoryStream;
  TempStream: TMemoryStream;
  FilterData: IValueFilterData;
  FirstChain: Boolean;
begin
  if Value.Size > 0 then
  begin
    OutStream := TMemoryStream.Create;
    try
      FirstChain := True;
      FilterData := Self;
      InStream := Value;

      TempStream := TMemoryStream.Create;
      try
        repeat
          FilterData.SaveChainValue(InStream, OutStream);
          // get the next filter data in chain
          FilterData := FilterData.NextFilter;

          if FilterData <> nil then
          begin
            if FirstChain then
            begin
              InStream := TempStream;
              FirstChain := False;
            end;

            TMemoryStream(InStream).Clear;
            InStream.CopyFrom(OutStream, 0);
            InStream.Seek(0, soFromBeginning);
            OutStream.Clear;
          end;
        until FilterData = nil;

        // finally write the result to node
        OutStream.Seek(0, soFromBeginning);
        ElementNode.Text := Base64Encode(OutStream);
      finally
        TempStream.Free;
      end;
    finally
      OutStream.Free;
    end;
  end;
end;

function TValueFilterData.LoadValueAsString: XmlString;
var
  OutStream: TMemoryStream;
begin
  Result := '';

  if FElementNode.Text <> '' then
  begin
    OutStream := TMemoryStream.Create;
    try
      InternalLoadValueAsStream(OutStream);
      // read the final result from output stream
      Result := ReadFromStreamAsUnicode(OutStream);
    finally
      OutStream.Free;
    end;
  end;
end;

procedure TValueFilterData.LoadValueAsStream(const Value: TStream);
begin
  if FElementNode.Text <> '' then
  begin
    InternalLoadValueAsStream(Value);
    Value.Seek(0, soFromBeginning);
  end;
end;

procedure TValueFilterData.SaveValueAsStream(const Value: TStream);
begin
  InternalSaveValueAsStream(Value);
end;

procedure TValueFilterData.SaveValueAsString(const Value: XmlString);
var
  InStream: TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  try
    WriteToStreamAsUnicode(InStream, Value);
    InStream.Seek(0, soFromBeginning);

    InternalSaveValueAsStream(InStream);
  finally
    InStream.Free;
  end;
end;

{ TStorageBase }

constructor TStorageBase.Create(const ElementNode: IXMLNode);
begin
  FElementNode := ElementNode;
end;

function TStorageBase.GetElementNode: IXMLNode;
begin
  Result := FElementNode;
end;

procedure TStorageBase.SetElementNode(const Value: IXMLNode);
begin
  FElementNode := Value;
end;

initialization
  // no initialization
  
finalization
  if Assigned(Plugins) then
    FreeAndNil(Plugins);

end.
