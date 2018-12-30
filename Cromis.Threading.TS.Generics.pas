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
 * Thread safe classes based on generics for newer Delphi versions
 * Refer to Cromis.Threading for changelog
 * =============================================================================
*)
unit Cromis.Threading.TS.Generics;

interface

uses
  Windows, SysUtils, Classes,

  // common units
  Cromis.Threading.TS.Common, Cromis.Threading.Sync;

{$IFNDEF Threading_NoLockFreeStack}
type
  TStackObject<TValue> = class
    Data: TValue;
  end;

  PStackItem = ^TStackItem;
  TStackItem = record
    Next: PStackItem;
    Data: Pointer;
  end;

type
  TLockFreeStack<TValue> = class
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
    function Pop(var Value: TValue): Boolean;
    function Push(const Value: TValue): PStackItem;
    property Count: Integer read GetCount;
    procedure Clear;
  end;
{$ENDIF}

type
  TQueueItem<TValue> = class
    Next: TQueueItem<TValue>;
    Data: TValue;
  end;

  TQueueControl<TValue> = class
    Head: TQueueItem<TValue>;
    Tail: TQueueItem<TValue>;
    Lock: TSRWLock;
    Count: Int64;
  end;

  TThreadSafeQueue<TValue> = class
  private
    FQueue: TQueueControl<TValue>;
    FDeleted: TQueueControl<TValue>;
    FUseDeleted: Boolean;
    function GetCount: Int64;
    function AcquireNewItem: TQueueItem<TValue>;
    procedure ClearData(const List: TQueueControl<TValue>);
    procedure InitializeDeleteSize(const InitialSize: Integer);
  protected
    procedure DoEnqueue(const List: TQueueControl<TValue>; const Item: TQueueItem<TValue>);
    function DoDequeue(const List: TQueueControl<TValue>; var Item: TQueueItem<TValue>): Boolean;
  public
    constructor Create(const InitialSize: Integer = cDefaultSize; const UseDeleted: Boolean = True);
    destructor Destroy; override;
    procedure Enqueue(const Value: TValue);
    function Dequeue(var Value: TValue): Boolean;
    property Count: Int64 read GetCount;
  end;

implementation

{$IFNDEF Threading_NoLockFreeStack}

{ TLockFreeStack }

procedure TLockFreeStack<TValue>.Clear;
var
  Value: TValue;
begin
  while Count > 0 do
    Pop(Value);
end;

constructor TLockFreeStack<TValue>.Create(const InitialSize: Integer; const UseDeleted: Boolean);
begin
  FUseDeleted := UseDeleted;
  GetMem(FListHead, 2 * SizeOf(NativeInt));
  CallInitializeSListHead(FListHead);

  if FUseDeleted then
  begin
    GetMem(FDeleted, 2 * SizeOf(NativeInt));
    CallInitializeSListHead(FDeleted);
    InitializeDeleteSize(InitialSize);
  end;
end;

destructor TLockFreeStack<TValue>.Destroy;
begin
  DoFinalizeList(FListHead);
  if FUseDeleted then
    DoFinalizeList(FDeleted);

  inherited;
end;

procedure TLockFreeStack<TValue>.DoFinalizeList(const ListHead: Pointer);
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
      TObject(TempEntry.Data).Free;
      Dispose(TempEntry);
    end;
  end;
end;

function TLockFreeStack<TValue>.Flush: PStackItem;
begin
  Result := CallInterlockedFlushSList(FListHead);
end;

function TLockFreeStack<TValue>.GetCount: Integer;
begin
  Result := CallQueryDepthSList(FListHead);
end;

procedure TLockFreeStack<TValue>.InitializeDeleteSize(const InitialSize: Integer);
var
  I: Integer;
  Item: PStackItem;
begin
  for I := 1 to InitialSize do
  begin
    New(Item);
    Item.Next := nil;
    Item.Data := TStackObject<TValue>.Create;
    CallInterlockedPushEntrySList(FDeleted, Item);
  end;
end;

function TLockFreeStack<TValue>.Pop(var Value: TValue): Boolean;
var
  Item: PStackItem;
begin
  Item := CallInterlockedPopEntrySList(FListHead);

  if Item = nil then
  begin
    Value := Default(TValue);
    Result := False;
  end
  else
  begin
    Value := TStackObject<TValue>(Item.Data).Data;
    Result := True;

    if FUseDeleted then
    begin
      TStackObject<TValue>(Item.Data).Data := Default(TValue);
      CallInterlockedPushEntrySList(FDeleted, Item);
    end
    else
    begin
      TObject(Item.Data).Free;
      Dispose(Item);
    end;
  end;
end;

function TLockFreeStack<TValue>.Push(const Value: TValue): PStackItem;
var
  Item: PStackItem;
begin
  if FUseDeleted then
    Item := CallInterlockedPopEntrySList(FDeleted)
  else
    Item := nil;

  if Item = nil then
  begin
    New(Item);
    Item.Next := nil;
    Item.Data := TStackObject<TValue>.Create;
  end;

  TStackObject<TValue>(Item.Data).Data := Value;
  Result := CallInterlockedPushEntrySList(FListHead, Item);
end;

{$ENDIF}

{ TThreadSafeQueue }

function TThreadSafeQueue<TValue>.AcquireNewItem: TQueueItem<TValue>;
begin
  if not FUseDeleted then
    Result :=  TQueueItem<TValue>.Create
  else if not DoDequeue(FDeleted, Result) then
    Result :=  TQueueItem<TValue>.Create;

  // next is always nil
  Result.Next := nil;
  Result.Data := Default(TValue);
end;

procedure TThreadSafeQueue<TValue>.ClearData(const List: TQueueControl<TValue>);
var
  Item: TQueueItem<TValue>;
begin
  while DoDequeue(List, Item) do
  begin
    Item.Data := Default(TValue);
    Item.Free;
  end;
end;

constructor TThreadSafeQueue<TValue>.Create(const InitialSize: Integer; const UseDeleted: Boolean);
begin
  FUseDeleted := UseDeleted;

  FQueue := TQueueControl<TValue>.Create;
  FQueue.Head := TQueueItem<TValue>.Create;
  FQueue.Lock.Initialize;

  if FUseDeleted then
  begin
    FDeleted := TQueueControl<TValue>.Create;
    FDeleted.Head := TQueueItem<TValue>.Create;
    FDeleted.Lock.Initialize;
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

destructor TThreadSafeQueue<TValue>.Destroy;
begin
  if FUseDeleted then
  begin
    ClearData(FDeleted);
    FDeleted.Head.Free;
    FreeAndNil(FDeleted);
  end;

  ClearData(FQueue);
  FQueue.Head.Free;
  FreeAndNil(FQueue);

  inherited;
end;

function TThreadSafeQueue<TValue>.DoDequeue(const List: TQueueControl<TValue>; var Item: TQueueItem<TValue>): Boolean;
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

procedure TThreadSafeQueue<TValue>.DoEnqueue(const List: TQueueControl<TValue>; const Item: TQueueItem<TValue>);
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

function TThreadSafeQueue<TValue>.Dequeue(var Value: TValue): Boolean;
var
  OldItem: TQueueItem<TValue>;
begin
  Result := DoDequeue(FQueue, OldItem);
  Value := Default(TValue);

  if Result then
  begin
    Value := OldItem.Data;

    if FUseDeleted then
    begin
      // delete the item
      OldItem.Data := Default(TValue);
      DoEnqueue(FDeleted, OldItem);
    end
    else
    begin
      OldItem.Data := Default(TValue);
      OldItem.Free;
    end;
  end;
end;

procedure TThreadSafeQueue<TValue>.Enqueue(const Value: TValue);
var
  NewItem: TQueueItem<TValue>;
begin
  // acquire new item
  NewItem := AcquireNewItem;
  NewItem.Data := Value;

  DoEnqueue(FQueue, NewItem);
end;

function TThreadSafeQueue<TValue>.GetCount: Int64;
begin
  FQueue.Lock.AcquireShared;
  try
    Result := FQueue.Count;
  finally
    FQueue.Lock.ReleaseShared;
  end;
end;

procedure TThreadSafeQueue<TValue>.InitializeDeleteSize(const InitialSize: Integer);
var
  I: Integer;
  Item: TQueueItem<TValue>;
begin
  for I := 1 to InitialSize do
  begin
    Item := TQueueItem<TValue>.Create;
    Item.Data := Default(TValue);
    Item.Next := nil;

    DoEnqueue(FDeleted, Item);
  end;
end;

end.
