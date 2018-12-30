{ $Cromis: lib/Cromis.SimpleStorage.pas,v 1.3 2008/04/13 17:09:35 ikacin Exp $ }
(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009-2010 Iztok Kacin, Cromis.
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
 * A simple interface that allows searches on document collections
 * ==================================================================================
 * 06/02/2010 (1.0.0)
 *   - Initial import
 * ==================================================================================
 * 06/02/2010 (1.1.0)
 *   - Moved IDocumentsCollection to IDocuments interface
 * ==================================================================================
 * 07/02/2010 (1.2.0)
 *   - Added support for anonymous functions (D2009 and up)
 *   - Added document subset selection with callback option
 * ==================================================================================
 * 08/02/2010 (1.3.0)
 *   - Added IDocument interface. Pass IDocument interface in callback functions
 *   - Can add new document from ISimpleStorage. Saves it on filesystem
 * ==================================================================================
 * 08/03/2010 (1.3.1)
 *   - SaveChanges added to IDocument. Can save the changes made back to file system
 * ==================================================================================
 * 28/07/2010 (1.4.0)
 *   - EnableEncryption / DissableEncryption removed
 *   - Added FilterChain that controls how documents are loaded and saved
 * ==================================================================================
 * 30/07/2010 (1.4.0)
 *   - Added AddDocument overload that takes in a stream as input
 * ==================================================================================
*)
unit Cromis.SimpleStorage.Collection;

interface

uses
  Windows, SysUtils, Classes, Math,

  // omniXML lib
  OmniXML_Types,

  // cromis units
  Cromis.SimpleStorage, Cromis.StringUtils;

type
  IDocument = Interface(IInterface)
  ['{0B2F03BD-9929-43A1-A7E8-62B372D2D96C}']
    function GetName: string;
    function GetPath: string;
    function GetData: ISimpleStorage;
    property Data: ISimpleStorage read GetData;
    property Path: string read GetPath;
    property Name: string read GetName;
    procedure SaveChanges;
  end;

  TOnDocumentCallback = procedure(const Document: IDocument;
                                  var Include: Boolean;
                                  const Data: Pointer
                                  ) of Object;
  TOnElementCallback = procedure(const Document: IDocument;
                                 const Element: IElement;
                                 const Data: Pointer
                                 ) of Object;

  {$IF CompilerVersion >= 20}
    TOnDocumentAnonProc = reference to procedure(const Document: IDocument;
                                                 var Include: Boolean;
                                                 const Data: Pointer);
    TOnElementAnonProc = reference to procedure(const Document: IDocument;
                                                const Element: IElement;
                                                const Data: Pointer);
  {$IFEND}

  TDoOnDocument = {$IF CompilerVersion >= 20}TOnDocumentAnonProc{$ELSE}TOnDocumentCallback{$IFEND};
  TDoOnElement = {$IF CompilerVersion >= 20}TOnElementAnonProc{$ELSE}TOnElementCallback{$IFEND};

  IDocumentsEnumerator = Interface(IInterface)
  ['{448A91DE-9E99-44E3-99EB-C56FA8644676}']
    // getters and setters
    function GetCurrent: IDocument;
    // iterator function and procedures
    function MoveNext: Boolean;
    property Current: IDocument read GetCurrent;
  end;

  IDocuments =  Interface(IInterface)
  ['{847A6DAB-45CD-46C2-82F1-32A4F79A2D2F}']
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: IDocumentsEnumerator;
    function GetFilterChain: IDocumentFilterChain;
    function GetItem(const Index: Integer): IDocument;
    procedure RemoveDocument(const FileName: string); overload;
    procedure RemoveDocumentByName(const Name: string); overload;
    procedure AddDocument(const FileName: string; const Name: string = ''); overload;
    procedure AddDocument(const Stream: TStream; const FileName: string;
      const Name: string = ''); overload;
    procedure AddDocument(const Storage: ISimpleStorage; const FileName: string;
      const Name: string = ''); overload;
    procedure AddDocuments(const Directory: string; const InDepth: Boolean;
      const Mask: string = '*.xml');
    {$IF CompilerVersion >= 20}
      procedure Get(const XPath: XmlString; const OnElement: TOnElementAnonProc;
        const Data: Pointer = nil); overload;
      function Get(const XPath: XmlString; const OnElement: TOnDocumentAnonProc;
        const Data: Pointer = nil): IDocuments; overload;
    {$IFEND}
    procedure Get(const XPath: XmlString; const OnElement: TOnElementCallback;
      const Data: Pointer = nil); overload;
    function Get(const XPath: XmlString; const OnElement: TOnDocumentCallback;
      const Data: Pointer = nil): IDocuments; overload;
    function Get(const XPath: XmlString): IDocuments; overload;
    function GetDocumentByName(const Name: string): IDocument;
    procedure AssignDocuments(const Documents: IDocuments);
    property Item[const Index: Integer]: IDocument read GetItem;
    property FilterChain: IDocumentFilterChain read GetFilterChain;
  end;

  // create the main documents collection interface
  function CreateCollection(const Directory: string = '';
                            const InDepth: Boolean = False;
                            const Mask: string = '*.xml';
                            const ExtInNames: Boolean = False
                            ): IDocuments;

implementation

type
  TDocument = class(TInterfacedObject, IDocument)
  private
    FPath: string;
    FName: string;
    FData: ISimpleStorage;
    FFilterChain: IDocumentFilterChain;
    function GetData: ISimpleStorage;
    function GetName: string;
    function GetPath: string;
  public
    constructor Create(const FilterChain: IDocumentFilterChain;
                       const Data: ISimpleStorage;
                       const Path: string;
                       const Name: string);
    property Data: ISimpleStorage read GetData;
    property Path: string read GetPath;
    property Name: string read GetName;
    procedure SaveChanges;
  end;

  TDocumentsEnumerator = class(TInterfacedObject, IDocumentsEnumerator)
  private
    FIndex: Integer;
    FDocuments: TStringList;
    FFilterChain: IDocumentFilterChain;
    function GetCurrent: IDocument;
  public
    constructor Create(const FilterChain: IDocumentFilterChain;
                       const Documents: TStringList);
    function MoveNext: Boolean;
    property Current: IDocument read GetCurrent;
  end;

  TDocuments = class(TInterfacedObject, IDocuments)
  private
    FExtInNames: Boolean;
    FFilterChain: IDocumentFilterChain;
    function GetFilterChain: IDocumentFilterChain;
    function GetItem(const Index: Integer): IDocument;
    function DoGet(const XPath: XmlString; const OnDocument: TDoOnDocument;
      const Data: Pointer): IDocuments; overload;
    procedure DoGet(const XPath: XmlString; const OnElement: TDoOnElement;
      const Data: Pointer); overload;
    procedure DoAddDocumentByName(const FileName, Name: string);
    procedure SortDocuments;
  protected
    FDocuments: TStringList;
  public
    constructor Create(const FilterChain: IDocumentFilterChain;
                       const Documents: TStringList;
                       const ExtInNames: Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: IDocumentsEnumerator;
    procedure RemoveDocument(const FileName: string); overload;
    procedure RemoveDocumentByName(const Name: string); overload;
    procedure AddDocument(const FileName: string; const Name: string = ''); overload;
    procedure AddDocument(const Stream: TStream; const FileName: string;
      const Name: string = ''); overload;
    procedure AddDocument(const Storage: ISimpleStorage; const FileName: string;
      const Name: string = ''); overload;
    procedure AddDocuments(const Directory: string; const InDepth: Boolean;
      const Mask: string = '*.*');
    procedure Get(const XPath: XmlString; const OnElement: TOnElementCallback;
      const Data: Pointer = nil); overload;
    {$IF CompilerVersion >= 20}
      procedure Get(const XPath: XmlString; const OnElement: TOnElementAnonProc;
        const Data: Pointer = nil); overload;
      function Get(const XPath: XmlString; const OnDocument: TOnDocumentAnonProc;
        const Data: Pointer = nil): IDocuments; overload;
    {$IFEND}
    function Get(const XPath: XmlString; const OnDocument: TOnDocumentCallback;
      const Data: Pointer = nil): IDocuments; overload;
    function Get(const XPath: XmlString): IDocuments; overload;
    procedure AssignDocuments(const Documents: IDocuments);
    function GetDocumentByName(const Name: string): IDocument;
    property Item[const Index: Integer]: IDocument read GetItem;
    property FilterChain: IDocumentFilterChain read GetFilterChain;
  end;

function CreateCollection(const Directory: string = '';
                          const InDepth: Boolean = False;
                          const Mask: string = '*.xml';
                          const ExtInNames: Boolean = False
                          ): IDocuments;
begin
  Result := TDocuments.Create(CreateDocumentFilterChain, nil, ExtInNames);

  if Directory <> '' then
    Result.AddDocuments(Directory, InDepth, Mask);
end;

function DoLoadDocument(const FilterChain: IDocumentFilterChain;
  const FileName: string): ISimpleStorage;
begin
  case FilterChain.Count > 0  of
    True: Result := FilterChain.LoadFromFile(FileName);
    False: Result := StorageFromFile(FileName);
  end;
end;

procedure DoSaveDocument(const FilterChain: IDocumentFilterChain;
                         const Storage: ISimpleStorage;
                         const FileName: string);
begin
  case FilterChain.Count > 0  of
    True: FilterChain.SaveToFile(Storage, FileName);
    False: Storage.SaveToFile(FileName);
  end;
end;

{ TDocumentsEnumerator }

constructor TDocumentsEnumerator.Create(const FilterChain: IDocumentFilterChain;
  const Documents: TStringList);
begin
  FFilterChain := FilterChain;
  FDocuments := Documents;
  FIndex := -1;
end;

function TDocumentsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FDocuments.Count - 1);

  if Result then
    Inc(FIndex);
end;

function TDocumentsEnumerator.GetCurrent: IDocument;
var
  Data: ISimpleStorage;
  DocName: string;
  DocPath: string;
begin
  DocName := FDocuments.Names[FIndex];
  DocPath := FDocuments.ValueFromIndex[FIndex];
  Data := DoLoadDocument(FFilterChain, FDocuments.ValueFromIndex[FIndex]);
  Result := TDocument.Create(FFilterChain, Data, DocPath, DocName);
end;

{ TDocuments }

procedure TDocuments.AddDocument(const FileName, Name: string);
begin
  DoAddDocumentByName(FileName, Name);
  SortDocuments;
end;

procedure TDocuments.AddDocument(const Stream: TStream; const FileName, Name: string);
begin
  DoSaveDocument(FFilterChain, StorageFromStream(Stream), FileName);
  DoAddDocumentByName(FileName, Name);
  SortDocuments;
end;

procedure TDocuments.AddDocument(const Storage: ISimpleStorage; const FileName, Name: string);
begin
  DoSaveDocument(FFilterChain, Storage, FileName);
  DoAddDocumentByName(FileName, Name);
  SortDocuments;
end;

procedure TDocuments.AddDocuments(const Directory: string; const InDepth: Boolean; const Mask: string);
var
  SR: TSearchRec;
  FileName: string;
begin
  if InDepth then
  begin
    // search for all subdirectories recursive
    if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*.*', faDirectory, SR) = 0 then
    begin
      try
        repeat
          if (SR.Attr and faDirectory) > 0 then
            if (SR.Name <> '') and (SR.Name[1] <> '.') and (SR.Name[1] <> '..') then
              AddDocuments(IncludeTrailingPathDelimiter(Directory) + SR.Name, InDepth, Mask);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

  // search for all files with the specified mask
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + Mask, faAnyFile, SR) = 0 then
  begin
    try
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          FileName := IncludeTrailingPathDelimiter(Directory) + SR.Name;
          DoAddDocumentByName(FileName, '');
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;

  // sort for faster searches
  SortDocuments;
end;

procedure TDocuments.AssignDocuments(const Documents: IDocuments);
var
  Document: IDocument;
begin
  for Document in Documents do
    Self.AddDocument(Document.Data, Document.Path);
end;

procedure TDocuments.Clear;
begin
  FDocuments.Clear;
end;

function TDocuments.Count: Integer;
begin
  Result := FDocuments.Count;
end;

constructor TDocuments.Create(const FilterChain: IDocumentFilterChain;
                              const Documents: TStringList;
                              const ExtInNames: Boolean);
begin
  FDocuments := TStringList.Create;
  FFilterChain := FilterChain;
  FExtInNames := ExtInNames;

  if Documents <> nil then
    FDocuments.Assign(Documents);
end;

destructor TDocuments.Destroy;
begin
  FreeAndNil(FDocuments);

  inherited;
end;

function TDocuments.DoGet(const XPath: XmlString; const OnDocument: TDoOnDocument;
  const Data: Pointer): IDocuments;
var
  I: Integer;
  DocName: string;
  DocPath: string;
  Include: Boolean;
  Storage: ISimpleStorage;
  Document: IDocument;
  Documents: TStringList;
begin
  Documents := TStringList.Create;
  try
    for I := 0 to FDocuments.Count - 1 do
    begin
      Storage := DoLoadDocument(FFilterChain, FDocuments.ValueFromIndex[I]);

      if Storage.Elements(XPath).Count > 0 then
      begin
        Include := True;
        DocName := FDocuments.Names[I];
        DocPath := FDocuments.ValueFromIndex[I];
        Document := TDocument.Create(FFilterChain, Storage, DocPath, DocName);
        OnDocument(Document, Include, Data);

        if Include then
          Documents.Add(FDocuments[I]);
      end;
    end;

    // return the filtered documents
    Result := TDocuments.Create(FFilterChain, Documents, FExtInNames);
  finally
    Documents.Free;
  end;
end;

procedure TDocuments.DoAddDocumentByName(const FileName, Name: string);
var
  DotPos: Integer;
  DocName: string;
begin
  if Name <> '' then
    FDocuments.Values[Name] := FileName
  else
  begin
    DocName := ExtractFileName(FileName);

    if not FExtInNames then
    begin
      DotPos := PosBack('.', DocName);
      DocName := Copy(DocName, 1, IfThen(DotPos = 0, Length(DocName), DotPos - 1));
    end;
    
    // add the document with the name
    FDocuments.Add(DocName + '=' + FileName);
  end;
end;

procedure TDocuments.DoGet(const XPath: XmlString; const OnElement: TDoOnElement; const Data: Pointer);
var
  I: Integer;
  DocName: string;
  DocPath: string;
  Element: IElement;
  Storage: ISimpleStorage;
  Document: IDocument;
  ElementList: IElementsList;
begin
  for I := 0 to FDocuments.Count - 1 do
  begin
    Storage := DoLoadDocument(FFilterChain, FDocuments.ValueFromIndex[I]);
    ElementList := Storage.Elements(XPath);

    for Element in ElementList do
    begin
      DocName := FDocuments.Names[I];
      DocPath := FDocuments.ValueFromIndex[I];
      Document := TDocument.Create(FFilterChain, Storage, DocPath, DocName);
      OnElement(Document, Element, Data);
    end;
  end;
end;

function TDocuments.Get(const XPath: XmlString): IDocuments;
var
  I: Integer;
  Document: ISimpleStorage;
  Documents: TStringList;
begin
  Documents := TStringList.Create;
  try
    for I := 0 to FDocuments.Count - 1 do
    begin
      Document := DoLoadDocument(FFilterChain, FDocuments.ValueFromIndex[I]);

      if Document.Elements(XPath).Count > 0 then
        Documents.Add(FDocuments.ValueFromIndex[I]);
    end;

    // return the filtered documents
    Result := TDocuments.Create(FFilterChain, Documents, FExtInNames);
  finally
    Documents.Free;
  end;
end;

function TDocuments.GetDocumentByName(const Name: string): IDocument;
var
  Data: ISimpleStorage;
  FileName: string;
begin
  FileName := FDocuments.Values[Name];
  Result := nil;

  if FileExists(FileName) then
  begin
    Data := DoLoadDocument(FFilterChain, FileName);
    Result := TDocument.Create(FFilterChain, Data, FileName, Name);
  end;
end;

function TDocuments.Get(const XPath: XmlString; const OnDocument: TOnDocumentCallback;
  const Data: Pointer): IDocuments;
begin
  Result := DoGet(XPath, OnDocument, Data);
end;

procedure TDocuments.Get(const XPath: XmlString; const OnElement: TOnElementCallback;
  const Data: Pointer);
begin
  DoGet(XPath, OnElement, Data);
end;

{$IF CompilerVersion >= 20}
function TDocuments.Get(const XPath: XmlString; const OnDocument: TOnDocumentAnonProc;
  const Data: Pointer): IDocuments;
begin
  Result := DoGet(XPath, OnDocument, Data);
end;

procedure TDocuments.Get(const XPath: XmlString; const OnElement: TOnElementAnonProc;
  const Data: Pointer);
begin
  DoGet(XPath, OnElement, Data);
end;
{$IFEND}

function TDocuments.GetEnumerator: IDocumentsEnumerator;
begin
  Result := TDocumentsEnumerator.Create(FFilterChain, FDocuments);
end;

function TDocuments.GetFilterChain: IDocumentFilterChain;
begin
  Result := FFilterChain;
end;

function TDocuments.GetItem(const Index: Integer): IDocument;
var
  Data: ISimpleStorage;
  DocName: string;
  DocPath: string;
begin
  DocName := FDocuments.Names[Index];
  DocPath := FDocuments.ValueFromIndex[Index];
  Data := DoLoadDocument(FFilterChain, FDocuments.ValueFromIndex[Index]);
  Result := TDocument.Create(FFilterChain, Data, DocPath, DocName);
end;

procedure TDocuments.RemoveDocument(const FileName: string);
var
  I: Integer;
begin
  for I := 0 to FDocuments.Count - 1 do
  begin
    if SameText(FDocuments.ValueFromIndex[I], FileName) then
    begin
      FDocuments.Delete(I);
      Exit;
    end;
  end;
end;

procedure TDocuments.RemoveDocumentByName(const Name: string);
begin
  FDocuments.Delete(FDocuments.IndexOfName(Name));
end;

procedure TDocuments.SortDocuments;
begin
  FDocuments.BeginUpdate;
  try
    FDocuments.Sorted := True;
    FDocuments.Sort;
  finally
    FDocuments.EndUpdate;
  end;
end;

{ TDocument }

constructor TDocument.Create(const FilterChain: IDocumentFilterChain;
                             const Data: ISimpleStorage;
                             const Path: string;
                             const Name: string);
begin
  FFilterChain := FilterChain;
  FData := Data;
  FPath := Path;
  FName := Name;
end;

function TDocument.GetData: ISimpleStorage;
begin
  Result := FData;
end;

function TDocument.GetName: string;
begin
  Result := FName;
end;

function TDocument.GetPath: string;
begin
  Result := FPath;
end;

procedure TDocument.SaveChanges;
begin
  DoSaveDocument(FFilterChain, FData, FPath);
end;

end.
