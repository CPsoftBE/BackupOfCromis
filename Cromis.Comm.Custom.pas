(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2013 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Base communications unit. Other high level units use it such as IMC and IPC
 * =============================================================================
 * 18/07/2013 (1.0.0)
 *   - Initial implementation
 * 20/08/2013 (1.1.0)
 *   - Reorganized context and client classes
 * =============================================================================
*)
unit Cromis.Comm.Custom;

interface

uses
  SysUtils, Classes, SyncObjs,

  // cromis units
  Cromis.Streams, Cromis.Hashing, Cromis.AnyValue, Cromis.Unicode, Cromis.Cryptography;

type
  {
    Generic message data packet
  }
  IMessageData = Interface(IInterface)
  ['{478D31BF-75D2-4EFF-8BCA-C72F41A6192E}']
    function GetID: ustring;
    function GetData: TStreamStorage;
    procedure SetID(const Value: ustring);
    property Data: TStreamStorage read GetData;
    property ID: ustring read GetID write SetID;
  end;

  TMessageData = class(TInterfacedObject, IMessageData)
  private
    FID: ustring;
    FData: TStreamStorage;
    function GetID: ustring;
    function GetData: TStreamStorage;
    procedure SetID(const Value: ustring);
  public
    constructor Create;
    destructor Destroy; override;
    property Data: TStreamStorage read GetData;
    property ID: ustring read GetID write SetID;
  end;

  {
    Base client context class.
    Derive from this if needed.
  }
  TCommClient = class
  private
    FID: string;
  public
    constructor Create; virtual;
    property ID: string read FID;
  end;

  // will be used for custom client context
  TCommClientClass = class of TCommClient;

  {
    Comm Context. Holds request specific data
  }
  ICommContext = Interface(IInterface)
  ['{3FF18B2F-BBA4-493C-A4DF-CDBA76D522E3}']
    function GetID: ustring;
    function GetClient: TCommClient;
    property ID: ustring read GetID;
    property Client: TCommClient read GetClient;
    procedure Acquire;
    procedure Release;
  end;

  {
    Server error
  }
  TServerError = record
    Code: Cardinal;
    Desc: string;
  end;

  // Event handler declarations
  TOnExecuteRequest = procedure(const Context: ICommContext; const Request, Response: IMessageData) of Object;
  TOnServerError = procedure(const Context: ICommContext; const Error: TServerError) of Object;
  TOnClientEvent = procedure(const Context: ICommContext) of Object;

  IContextList = Interface(IInterface)
  ['{A57319E2-69F1-4E8D-920C-BD8647D97443}']
    procedure Clear;
    procedure Add(const Context: ICommContext);
    function GetItem(const Index : Integer): ICommContext;
    procedure SetItem(const Index: Integer; const Value: ICommContext);
    property Items[const Index: Integer]: ICommContext read GetItem write SetItem; default;
  end;

  {
    list of all communication contexts
  }
  TCommContextList = class
  private
    FLock: TCriticalSection;
    FContextHash: TStringHashTable;
    procedure DoEnumerateContexts(const Key, Value: PAnyValue; const Data: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContext(const Context: ICommContext);
    procedure RemoveContext(const Context: ICommContext);
    function GetByID(const ID: string): ICommContext;
    function GetContexts: IContextList;
    procedure Clear;
  end;

  // acquire message data interface
  function AcquireMessageData: IMessageData;
  function AcquireCommContext(const ClientClass: TCommClientClass): ICommContext;

implementation

type
  TCommContext = class(TInterfacedObject, ICommContext)
  private
    FID: ustring;
    FLock: TCriticalSection;
    FClient: TCommClient;
    function GetID: ustring;
    function GetClient: TCommClient;
  public
    constructor Create(const ClientClass: TCommClientClass);
    destructor Destroy; override;
    procedure Acquire;
    procedure Release;
    property Client: TCommClient read GetClient;
    property ID: ustring read GetID;
  end;

  TContextList = class(TInterfacedObject, IContextList)
  private
    FList: TInterfaceList;
    function GetItem(const Index : Integer): ICommContext;
    procedure SetItem(const Index: Integer; const Value: ICommContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Context: ICommContext);
    property Items[const Index: Integer]: ICommContext read GetItem write SetItem; default;
  end;

function AcquireMessageData: IMessageData;
begin
  Result := TMessageData.Create;
end;

function AcquireCommContext(const ClientClass: TCommClientClass): ICommContext;
begin
  Result := TCommContext.Create(ClientClass);
end;

{ TCommClient }

constructor TCommClient.Create;
begin
  FID := GetNewFormatedGUID;
end;

{ TContextList }

procedure TContextList.Add(const Context: ICommContext);
begin
  FList.Add(Context);
end;

procedure TContextList.Clear;
begin
  FList.Clear;
end;

constructor TContextList.Create;
begin
  FList := TInterfaceList.Create;
end;

destructor TContextList.Destroy;
begin
  FreeAndNil(FList);

  inherited;
end;

function TContextList.GetItem(const Index: Integer): ICommContext;
begin
  Result := ICommContext(FList[Index]);
end;

procedure TContextList.SetItem(const Index: Integer; const Value: ICommContext);
begin
  FList[Index] := Value;
end;

{ TCommContextList }

procedure TCommContextList.Clear;
begin
  FLock.Enter;
  try
    FContextHash.Clear;
  finally
    FLock.Leave;
  end;
end;

constructor TCommContextList.Create;
begin
  FLock := TCriticalSection.Create;
  FContextHash := TStringHashTable.Create;
end;

destructor TCommContextList.Destroy;
begin
  FreeAndNil(FContextHash);
  FreeAndNil(FLock);

  inherited;
end;

procedure TCommContextList.DoEnumerateContexts(const Key, Value: PAnyValue; const Data: Pointer);
begin
  IContextList(Data).Add(ICommContext(Value.AsInterface));
end;

function TCommContextList.GetContexts: IContextList;
begin
  Result := TContextList.Create;

  FLock.Enter;
  try
    FContextHash.EnumerateKeys(DoEnumerateContexts, Pointer(Result));
  finally
    FLock.Free;
  end;
end;

function TCommContextList.GetByID(const ID: string): ICommContext;
var
  Value: TAnyValue;
begin
  Value := FContextHash.Item[ID];
  Result := ICommContext(Value.AsInterface);
end;

procedure TCommContextList.AddContext(const Context: ICommContext);
begin
  FLock.Enter;
  try
    FContextHash.Add(Context.ID, Context);
  finally
    FLock.Leave;
  end;
end;

procedure TCommContextList.RemoveContext(const Context: ICommContext);
begin
  FLock.Enter;
  try
    FContextHash.Remove(Context.ID);
  finally
    FLock.Leave;
  end;
end;

{ TMessageData }

constructor TMessageData.Create;
begin
  FData := TStreamStorage.Create;
end;

destructor TMessageData.Destroy;
begin
  FreeAndNil(FData);

  inherited;
end;

function TMessageData.GetData: TStreamStorage;
begin
  Result := FData;
end;

function TMessageData.GetID: ustring;
begin
  Result := FID;
end;

procedure TMessageData.SetID(const Value: ustring);
begin
  FID := Value;
end;

{ TCommContext }

procedure TCommContext.Acquire;
begin
  FLock.Enter;
end;

constructor TCommContext.Create(const ClientClass: TCommClientClass);
begin
  FID := GetNewFormatedGUID;
  FClient := ClientClass.Create;
  FLock := TCriticalSection.Create;
end;

destructor TCommContext.Destroy;
begin
  FreeAndNil(FClient);
  FreeAndNil(FLock);

  inherited;
end;

function TCommContext.GetClient: TCommClient;
begin
  Result := FClient;
end;

function TCommContext.GetID: ustring;
begin
  Result := FID;
end;

procedure TCommContext.Release;
begin
  FLock.Leave;
end;

end.
