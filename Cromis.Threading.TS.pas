(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009-2014 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Thread safe classes based on TAnyValue to support older Delphi versions
 * Refer to Cromis.Threading for changelog
 * =============================================================================
*)
unit Cromis.Threading.TS;

interface

uses
  Windows, SysUtils, Classes,

  // cromis units
  Cromis.Threading.TS.Common, Cromis.Threading.Sync, Cromis.AnyValue;

{$IFNDEF Threading_NoLockFreeStack}
type
  PStackItem = ^TStackItem;
  TStackItem = record
    Next: PStackItem;
    Data: TAnyVAlue;
  end;

type
  TLockFreeStack = class
  private
    FDeleted: Pointer;
    FListHead: Pointer;
    FUseDeleted: Boolean;
    function GetCount: Integer;
    procedure DoFinalizeList(const ListHead: Pointer);
    procedure InitializeDeleteSize(const InitialSize: Integer);
  public
    constructor Create(const InitialSize: Integer = cDefaultSize; const UseDeleted: Boolean = True);
    destructor Destroy; override;
    function Flush: PStackItem;
    function Pop(var Value: TAnyValue): Boolean;
    function Push(const Value: TAnyValue): PStackItem;
    property Count: Integer read GetCount;
    procedure Clear;
  end;
{$ENDIF}

type
  PQueueItem = ^TQueueItem;
  TQueueItem = record
    Next: PQueueItem;
    Data: TAnyValue;
  end;

  TQueueControl = class
    Head: PQueueItem;
    Tail: PQueueItem;
    Lock: TSRWLock;
    Count: Int64;
  end;

  TThreadSafeQueue = class
  private
    FQueue: TQueueControl;
    FDeleted: TQueueControl;
    FUseDeleted: Boolean;
    function GetCount: Int64;
    function AcquireNewItem: PQueueItem;
    procedure ClearData(const List: TQueueControl);
    procedure InitializeDeleteSize(const InitialSize: Integer);
  protected
    procedure DoEnqueue(const List: TQueueControl; const Item: PQueueItem);
    function DoDequeue(const List: TQueueControl; var Item: PQueueItem): Boolean;
  public
    constructor Create(const InitialSize: Integer = cDefaultSize; const UseDeleted: Boolean = True);
    destructor Destroy; override;
    procedure Enqueue(const Value: TAnyValue);
    function Dequeue(var Value: TAnyValue): Boolean;
    property Count: Int64 read GetCount;
  end;

implementation

{$IFNDEF Threading_NoLockFreeStack}

{ TLockFreeStack }

procedure TLockFreeStack.Clear;
var
  Value: TAnyValue;
begin
  while Count > 0 do
  begin
    Pop(Value);
    Value.Clear;
  end;
end;

constructor TLockFreeStack.Create(const InitialSize: Integer; const UseDeleted: Boolean);
begin
  FUseDeleted := UseDeleted;
  GetMem(FListHead, 2 * SizeOf(NativeInt));

  if FUseDeleted then
  begin
    GetMem(FDeleted, 2 * SizeOf(NativeInt));
    CallInitializeSListHead(FDeleted);
    InitializeDeleteSize(InitialSize);
  end;

  CallInitializeSListHead(FListHead);
end;

destructor TLockFreeStack.Destroy;
begin
  DoFinalizeList(FListHead);
  if FUseDeleted then
    DoFinalizeList(FDeleted);

  inherited;
end;

procedure TLockFreeStack.DoFinalizeList(const ListHead: Pointer);
var
  TempEntry: PStackItem;
  ListEntry: PStackItem;
  FirstEntry: PStackItem;
begin
  ListEntry := nil;
  try
    ListEntry := CallInterlockedFlushSList(ListHead);
    FirstEntry := CallInterlockedPopEntrySList(ListHead);
    FreeMem(ListHead);

    if FirstEntry <> nil then
      raise Exception.Create('Error: List was not emptied');
  finally
    // free all leftover items
    while ListEntry <> nil do
    begin
      TempEntry := ListEntry;
      ListEntry := ListEntry.Next;
      TempEntry.Data.Clear;
      FreeMem(TempEntry);
    end;
  end;
end;

function TLockFreeStack.Flush: PStackItem;
begin
  Result := CallInterlockedFlushSList(FListHead);
end;

function TLockFreeStack.GetCount: Integer;
begin
  Result := CallQueryDepthSList(FListHead);
end;

procedure TLockFreeStack.InitializeDeleteSize(const InitialSize: Integer);
var
  I: Integer;
  Item : PStackItem;
begin
  for I := 1 to InitialSize do
  begin
    New(Item);
    Item.Next := nil;
    CallInterlockedPushEntrySList(FDeleted, Item);
  end;
end;

function TLockFreeStack.Pop(var Value: TAnyValue): Boolean;
var
  Item : PStackItem;
begin
  Item := CallInterlockedPopEntrySList(FListHead);

  if Item = nil then
  begin
    Value := TAnyValue.Null;
    Result := False;
  end
  else
  begin
    CopyAnyValue(@Value, @Item.Data);
    Result := True;

    if FUseDeleted then
    begin
      Item.Data.Clear;
      CallInterlockedPushEntrySList(FDeleted, Item);
    end
    else
    begin
      Item.Data.Clear;
      Dispose(Item);
    end;
  end;
end;

function TLockFreeStack.Push(const Value: TAnyValue): PStackItem;
var
  Item : PStackItem;
begin
  if FUseDeleted then
    Item := CallInterlockedPopEntrySList(FDeleted)
  else
    Item := nil;

  if Item = nil then
  begin
    New(Item);
    Item.Next := nil;
  end;

  CopyAnyValue(@Item.Data, @Value);
  Result := CallInterlockedPushEntrySList(FListHead, Item);
end;

{$ENDIF}

{ TThreadSafeQueue }

function TThreadSafeQueue.AcquireNewItem: PQueueItem;
begin
  if not FUseDeleted then
    New(Result)
  else if not DoDequeue(FDeleted, Result) then
    New(Result);

  // next is always nil
  Result.Next := nil;
  Result.Data.Clear;
end;

procedure TThreadSafeQueue.ClearData(const List: TQueueControl);
var
  Item: PQueueItem;
begin
  while DoDequeue(List, Item) do
  begin
    Item.Data.Clear;
    Dispose(Item);
  end;
end;

constructor TThreadSafeQueue.Create(const InitialSize: Integer; const UseDeleted: Boolean);
begin
  FUseDeleted := UseDeleted;

  FQueue := TQueueControl.Create;
  FQueue.Lock.Initialize;
  // create nodes
  New(FQueue.Head);

  if FUseDeleted then
  begin
    FDeleted := TQueueControl.Create;
    FDeleted.Lock.Initialize;
    New(FDeleted.Head);
  end;

  // set queue pointers
  FQueue.Tail := FQueue.Head;
  FQueue.Head.Next := nil;
  FQueue.Count := 0;

  if FUseDeleted then
  begin
    // set deleted pointers
    FDeleted.Tail := FDeleted.Head;
    FDeleted.Head.Next := nil;
    FDeleted.Count := 0;

    InitializeDeleteSize(InitialSize);
  end;
end;

destructor TThreadSafeQueue.Destroy;
begin
  if FUseDeleted then
  begin
    ClearData(FDeleted);
    Dispose(FDeleted.Head);
    FreeAndNil(FDeleted);
  end;

  ClearData(FQueue);
  Dispose(FQueue.Head);
  FreeAndNil(FQueue);

  inherited;
end;

function TThreadSafeQueue.DoDequeue(const List: TQueueControl; var Item: PQueueItem): Boolean;
begin
  Result := False;

  List.Lock.AcquireExclusive;
  try
    if List.Count > 0 then
    begin
      Item := List.Head.Next;

      // set head to the next element
      List.Head.Next := Item.Next;
      Dec(List.Count);

      if List.Count = 0 then
        List.Tail := List.Head;

      // success
      Result := True;
    end;
  finally
    List.Lock.ReleaseExclusive;
  end;
end;

procedure TThreadSafeQueue.DoEnqueue(const List: TQueueControl; const Item: PQueueItem);
begin
  List.Lock.AcquireExclusive;
  try
    List.Tail.Next := Item;
    List.Tail := Item;
    Inc(List.Count);
  finally
    List.Lock.ReleaseExclusive;
  end;
end;

function TThreadSafeQueue.Dequeue(var Value: TAnyValue): Boolean;
var
  OldItem: PQueueItem;
begin
  Result := DoDequeue(FQueue, OldItem);
  Value.Clear;

  if Result then
  begin
    Value := OldItem.Data;

    if FUseDeleted then
    begin
      // delete the item
      OldItem.Data.Clear;
      DoEnqueue(FDeleted, OldItem);
    end
    else
    begin
      OldItem.Data.Clear;
      Dispose(OldItem);
    end;
  end;
end;

procedure TThreadSafeQueue.Enqueue(const Value: TAnyValue);
var
  NewItem: PQueueItem;
begin
  // acquire new item
  NewItem := AcquireNewItem;
  NewItem.Data := Value;

  DoEnqueue(FQueue, NewItem);
end;

function TThreadSafeQueue.GetCount: Int64;
begin
  FQueue.Lock.AcquireShared;
  try
    Result := FQueue.Count;
  finally
    FQueue.Lock.ReleaseShared;
  end;
end;

procedure TThreadSafeQueue.InitializeDeleteSize(const InitialSize: Integer);
var
  I: Integer;
  Item: PQueueItem;
begin
  for I := 1 to InitialSize do
  begin
    New(Item);
    Item.Data.Clear;
    Item.Next := nil;
    DoEnqueue(FDeleted, Item);
  end;
end;

end.
