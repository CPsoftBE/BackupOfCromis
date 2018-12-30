(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2010-2014 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Hash related classes with generics support (Tables, hashing functions etc...)
 * For change log reffer to Cromis.Hashing
 * =============================================================================
 *)
unit Cromis.Hashing.Generics;

interface

uses
  SysUtils, Classes, Math, Rtti, TypInfo,
  Generics.Defaults,

  // cromis units
  Cromis.Hashing.Common, Cromis.Threading.Sync, Cromis.Threading.TS.Generics;

type
  THashedItemGeneric<TKey,TValue> = class
    Key: TKey;
    Value: TValue;
    NextItem: THashedItemGeneric<TKey,TValue>;
  end;

  TBuckets = array of Pointer;
  PPHashItem = ^PHashItem;
  PHashItem = Pointer;

  TEnumerateKeyAnonProc<TKey,TValue> = reference to procedure(const Key: TKey; const Value: TValue);
  THashFunctionImplementation = function(Data: Pointer; const Length: Cardinal): Cardinal;
  TKeyComparer<TKey> = function(const Key1, Key2: TKey): Boolean of object;

  ICustomHashComparer<TKey> = interface(IInterface)
  ['{88B05BBA-AE5E-4B50-8F24-6615A021082C}']
    function GetHashOf(const Key: TKey; const Size: Cardinal): Cardinal;
    function KeyEqual(const Key1, Key2: TKey): Boolean;
  end;

  TCustomHashComparer<TKey> = class(TSingletonImplementation)
  private
    FHashFunction: THashFunctionImplementation;
  public
    constructor Create(const HashFunction: THashFunctionImplementation); virtual;
  end;

  THashComparer_String_Relaxed = class(TCustomHashComparer<string>, ICustomHashComparer<string>)
  public
    function KeyEqual(const Key1, Key2: string): Boolean; inline;
    function GetHashOf(const Key: string; const Size: Cardinal): Cardinal; inline;
  end;

  THashComparer_Cardinal_Relaxed = class(TCustomHashComparer<Cardinal>, ICustomHashComparer<Cardinal>)
  public
    function KeyEqual(const Key1, Key2: Cardinal): Boolean; inline;
    function GetHashOf(const Key: Cardinal; const Size: Cardinal): Cardinal; inline;
  end;

  IHashElement<TKey,TValue> = Interface(IInterface)
  ['{E1AFB1AC-DFA8-476E-96B5-6A8DA210AF16}']
    function GetKey: TKey;
    function GetValue: TValue;
    property Key: TKey read GetKey;
    property Value: TValue read GetValue;
  end;

  IHashEnumerator<TKey,TValue> = Interface(IInterface)
  ['{A3A57D3D-65A1-48B6-AF8A-60BA7FAA0413}']
    // getters and setters
    function _GetCurrent: IHashElement<TKey,TValue>;
    // iterator function and procedures
    function MoveNext: Boolean;
    property Current: IHashElement<TKey,TValue> read _GetCurrent;
  end;

  THashTableBase<TKey,TValue> = class
  strict protected
    FSize: Cardinal;
    FCount: Integer;
    FOwnsObjects: Boolean;
    FMaxCountBeforeResize: Integer;
    FHashComparer: ICustomHashComparer<TKey>;
    procedure OnEnumKeys(const Key: TKey; const Value: TValue; const Data: Pointer);
    procedure OnEnumValues(const Key: TKey; const Value: TValue; const Data: Pointer);
  public
    constructor Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal = cDefaultHashSize); virtual;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc<TKey,TValue>); overload; virtual; abstract;
    function GetEnumerator: IHashEnumerator<TKey,TValue>; virtual;
    function ContainsKey(const Key: TKey): Boolean; virtual; abstract;
    function GetItem(const Key: TKey; var Value: TValue): Boolean; virtual; abstract;
    procedure Add(const Key: TKey; const Value: TValue); virtual; abstract;
    procedure SetItem(const Key: TKey; const Value: TValue); virtual; abstract;
    procedure Remove(const Key: TKey); virtual; abstract;
    procedure Clear; virtual; abstract;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Count: Integer read FCount;
  end;

  THashTableBase_Chaining<TKey,TValue> = class(THashTableBase<TKey,TValue>)
  strict private
    function CheckHashTableSize: Boolean;
    procedure Resize(const NewSize: Cardinal);
  strict protected
    FBuckets: TBuckets;
    procedure InternalClear(const Buckets: TBuckets); virtual;
    procedure InternalRemove(const Key: TKey; const KeyHash: Cardinal); virtual;
    function Find(const Key: TKey; const KeyHash: Cardinal): PPHashItem; inline;
    function AcquireNewBucket: THashedItemGeneric<TKey,TValue>; virtual; abstract;
    procedure InternalAdd(const Key: TKey; const Value: TValue; KeyHash: Cardinal); inline;
    procedure ReleaseOldBucket(const Bucket: THashedItemGeneric<TKey,TValue>); virtual; abstract;
  public
    constructor Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal = cDefaultHashSize); override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc<TKey,TValue>); overload; override;
  end;

  TCustomHashTable_Chaining<TKey,TValue> = class(THashTableBase_Chaining<TKey,TValue>)
  private
    FDeleted: THashedItemGeneric<TKey,TValue>;
    procedure ClearDeletedBuckets; inline;
    procedure InitializeDeletedItems(const Size: Cardinal);
  protected
    function AcquireNewBucket: THashedItemGeneric<TKey,TValue>; override;
    procedure ReleaseOldBucket(const Bucket: THashedItemGeneric<TKey,TValue>); override;
  public
    constructor Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal = cDefaultHashSize); override;
    destructor Destroy; override;
    function ContainsKey(const Key: TKey): Boolean; override;
    function GetItem(const Key: TKey; var Value: TValue): Boolean; override;
    procedure SetItem(const Key: TKey; const Value: TValue); override;
    procedure Add(const Key: TKey; const Value: TValue); override;
    procedure Remove(const Key: TKey); override;
    procedure Clear; override;
  end;

  TCustomThreadSafeHashTable_Chaining<TKey,TValue> = class(THashTableBase_Chaining<TKey,TValue>)
  private
    FSRWLock: TSRWLock;
  {$IFDEF Threading_NoLockFreeStack}
    FDeleted: TThreadSafeQueue<THashedItemGeneric<TKey,TValue>>;
  {$ELSE}
    FDeleted: TLockFreeStack<THashedItemGeneric<TKey,TValue>>;
  {$ENDIF}
    procedure ClearDeletedBuckets;
  protected
    function AcquireNewBucket: THashedItemGeneric<TKey,TValue>; override;
    procedure ReleaseOldBucket(const Bucket: THashedItemGeneric<TKey,TValue>); override;
  public
    constructor Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal = cDefaultHashSize); override;
    destructor Destroy; override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc<TKey,TValue>); override;
    function ContainsKey(const Key: TKey): Boolean; override;
    function GetItem(const Key: TKey; var Value: TValue): Boolean; override;
    procedure SetItem(const Key: TKey; const Value: TValue); override;
    procedure Add(const Key: TKey; const Value: TValue); override;
    procedure Remove(const Key: TKey); override;
    procedure Clear; override;
  end;

  THashElement<TKey,TValue> = class(TInterfacedObject, IHashElement<TKey,TValue>)
  private
    FKey: TKey;
    FValue: TValue;
    function GetKey: TKey;
    function GetValue: TValue;
  public
    constructor Create(const Key: TKey; const Value: TValue);
    property Value: TValue read GetValue;
    property Key: TKey read GetKey;
  end;

  THashEnumerator<TKey,TValue> = class(TInterfacedObject, IHashEnumerator<TKey,TValue>)
  private
    FIndex: Integer;
    FActualCount: Integer;
    FElementList: array of IHashElement<TKey,TValue>;
    FCurrentElement: IHashElement<TKey,TValue>;
    function _GetCurrent: IHashElement<TKey,TValue>;
    procedure InitializeEnum(const Key: TKey; const Value: TValue);
  public
    constructor Create(const HashTable: THashTableBase<TKey,TValue>);
    property Current: IHashElement<TKey,TValue> read _GetCurrent;
    function MoveNext: Boolean;
  end;

  // hash table that uses integers as keys
  TCardinalHashTable<TValue> = class(TCustomHashTable_Chaining<Cardinal, TValue>)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // hash table that uses strings as keys
  TStringHashTable<TValue> = class(TCustomHashTable_Chaining<string, TValue>)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // hash table that uses integers as keys
  TCardinalThreadSafeHashTable<TValue> = class(TCustomThreadSafeHashTable_Chaining<Cardinal, TValue>)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // hash table that uses strings as keys
  TStringThreadSafeHashTable<TValue> = class(TCustomThreadSafeHashTable_Chaining<string, TValue>)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

implementation

{ THashTableBase<TKey,TValue> }

constructor THashTableBase<TKey,TValue>.Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal);
begin
  FSize := GetGoodHashSize(Size);
  FHashComparer := Comparer;
end;

function THashTableBase<TKey,TValue>.GetEnumerator: IHashEnumerator<TKey,TValue>;
begin
  Result := THashEnumerator<TKey,TValue>.Create(Self);
end;

procedure THashTableBase<TKey,TValue>.OnEnumKeys(const Key: TKey;
                                                  const Value: TValue;
                                                  const Data: Pointer);
begin

end;

procedure THashTableBase<TKey,TValue>.OnEnumValues(const Key: TKey;
                                                    const Value: TValue;
                                                    const Data: Pointer);
begin

end;

{ THashTableBase_Chaining<TKey,TValue> }

function THashTableBase_Chaining<TKey,TValue>.CheckHashTableSize: Boolean;
begin
  Result := False;

  if FCount = FMaxCountBeforeResize then
  begin
    Resize(2 * FSize);
    Result := True;
  end;
end;

constructor THashTableBase_Chaining<TKey,TValue>.Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal);
begin
  inherited;

  SetLength(FBuckets, FSize);
  // calculate max items before resize
  FMaxCountBeforeResize := CalculateMaxItemsBeforeResize(FSize);
end;

procedure THashTableBase_Chaining<TKey,TValue>.EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc<TKey,TValue>);
var
  I: Integer;
  Item: THashedItemGeneric<TKey,TValue>;
begin
  for I := Low(FBuckets) to High(FBuckets) do
  begin
    Item := FBuckets[I];

    while Item <> nil do
    begin
      EnumProc(Item.Key, Item.Value);
      Item := Item.NextItem;
    end;
  end;
end;

function THashTableBase_Chaining<TKey,TValue>.Find(const Key: TKey;const KeyHash: Cardinal): PPHashItem;
begin
  Result := @FBuckets[KeyHash];

  while Result^ <> nil do
  begin
    if FHashComparer.KeyEqual(THashedItemGeneric<TKey,TValue>(Result^).Key, Key) then
      Exit
    else
      Result := @THashedItemGeneric<TKey,TValue>(Result^).NextItem;
  end;
end;

procedure THashTableBase_Chaining<TKey,TValue>.InternalAdd(const Key: TKey; const Value: TValue; KeyHash: Cardinal);
var
  Bucket: THashedItemGeneric<TKey,TValue>;
begin
  if CheckHashTableSize then
    KeyHash := FHashComparer.GetHashOf(Key, FSize);

  Bucket := AcquireNewBucket;
  Bucket.Key := Key;
  Bucket.Value := Value;
  Bucket.NextItem := FBuckets[KeyHash];
  FBuckets[KeyHash] := Bucket;
  Inc(FCount);
end;

procedure THashTableBase_Chaining<TKey,TValue>.InternalClear(const Buckets: TBuckets);
var
  I: Integer;
  P, N: THashedItemGeneric<TKey,TValue>;
begin
  for I := Low(Buckets) to High(Buckets) do
  begin
    P := Buckets[I];

    while P <> nil do
    begin
      N := P.NextItem;
      ReleaseOldBucket(P);
      P := N;
    end;

    Buckets[I] := nil;
  end;
end;

procedure THashTableBase_Chaining<TKey,TValue>.InternalRemove(const Key: TKey; const KeyHash: Cardinal);
var
  P: THashedItemGeneric<TKey,TValue>;
  Prev: PPHashItem;
begin
  Prev := Find(Key, KeyHash);
  P := Prev^;

  if P <> nil then
  begin
    Prev^ := P.NextItem;
    Dec(FCount);

    // return P to deleted
    ReleaseOldBucket(P);
  end;
end;

procedure THashTableBase_Chaining<TKey,TValue>.Resize(const NewSize: Cardinal);
var
  OldBuckets: TBuckets;
  BucketIndex: Integer;
  HashItem: THashedItemGeneric<TKey,TValue>;
begin
  FSize := GetGoodHashSize(NewSize);
  FMaxCountBeforeResize := CalculateMaxItemsBeforeResize(FSize);

  OldBuckets := FBuckets;
  FBuckets := nil;
  SetLength(FBuckets, FSize);
  FCount := 0;

  for BucketIndex := Low(OldBuckets) to High(OldBuckets) do
  begin
    HashItem := OldBuckets[BucketIndex];

    while HashItem <> nil do
    begin
      InternalAdd(HashItem.Key, HashItem.Value, FHashComparer.GetHashOf(HashItem.Key, FSize));
      HashItem := HashItem.NextItem;
    end;
  end;

  InternalClear(OldBuckets);
end;

{ TCustomHashTable_Chaining<TKey,TValue> }

function TCustomHashTable_Chaining<TKey,TValue>.AcquireNewBucket: THashedItemGeneric<TKey,TValue>;
begin
  if FDeleted <> nil then
  begin
    Result := FDeleted;
    FDeleted := FDeleted.NextItem;

    Result.NextItem := nil;
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end
  else
  begin
    Result := THashedItemGeneric<TKey,TValue>.Create;
    Result.NextItem := nil;
  end;
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  if Find(Key, KeyHash)^ <> nil then
    raise Exception.Create('Key already exists in hash table!');

  InternalAdd(Key, Value, KeyHash);
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.Clear;
begin
  InternalClear(FBuckets);
  FCount := 0;
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.ClearDeletedBuckets;
var
  P, N: THashedItemGeneric<TKey,TValue>;
begin
  P := FDeleted;

  while P <> nil do
  begin
    N := P.NextItem;
    P.Free;
    P := N;
  end;
end;

function TCustomHashTable_Chaining<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := Find(Key, FHashComparer.GetHashOf(Key, FSize))^ <> nil;
end;

constructor TCustomHashTable_Chaining<TKey,TValue>.Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal);
begin
  inherited;

  InitializeDeletedItems(FSize div 10);
end;

destructor TCustomHashTable_Chaining<TKey,TValue>.Destroy;
begin
  InternalClear(FBuckets);
  ClearDeletedBuckets;

  inherited;
end;

function TCustomHashTable_Chaining<TKey,TValue>.GetItem(const Key: TKey; var Value: TValue): Boolean;
var
  HashItem: THashedItemGeneric<TKey,TValue>;
begin
  HashItem := Find(Key, FHashComparer.GetHashOf(Key, FSize))^;

  if HashItem <> nil then
  begin
    Value := HashItem.Value;
    Result := True;
  end
  else
  begin
    Value := Default(TValue);
    Result := False;
  end;
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.InitializeDeletedItems(const Size: Cardinal);
var
  I: Integer;
  Bucket: THashedItemGeneric<TKey,TValue>;
begin
  for I := 1 to Size do
  begin
    Bucket := THashedItemGeneric<TKey,TValue>.Create;
    Bucket.NextItem := FDeleted;
    FDeleted := Bucket;
  end;
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.ReleaseOldBucket(const Bucket: THashedItemGeneric<TKey,TValue>);
var
  KeyType: Rtti.TValue;
  ValueType: Rtti.TValue;
begin
  Bucket.NextItem := FDeleted;
  FDeleted := Bucket;

  if FOwnsObjects then
  begin
    KeyType := Rtti.TValue.From<TKey>(Bucket.Key);
    ValueType := Rtti.TValue.From<TValue>(Bucket.Value);

    if KeyType.TypeInfo.Kind = tkClass then
      KeyType.AsObject.Free;
    if ValueType.TypeInfo.Kind = tkClass then
      ValueType.AsObject.Free;
  end;
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.Remove(const Key: TKey);
begin
  InternalRemove(Key, FHashComparer.GetHashOf(Key, FSize));
end;

procedure TCustomHashTable_Chaining<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
var
  KeyHash: Cardinal;
  HashItem: THashedItemGeneric<TKey,TValue>;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);
  HashItem := Find(Key, KeyHash)^;

  if HashItem <> nil then
    HashItem.Value := Value
  else
    InternalAdd(Key, Value, KeyHash);
end;

{ THashComparer_String_Relaxed<TKey> }

function THashComparer_String_Relaxed.KeyEqual(const Key1, Key2: string): Boolean;
begin
  Result := Key1 = Key2;
end;

function THashComparer_String_Relaxed.GetHashOf(const Key: string; const Size: Cardinal): Cardinal;
var
  DataLen: Integer;
begin
  DataLen := Length(Key) * SizeOf(Char);

  if DataLen > 0 then
    Result := Hash_SuperFastHash(PChar(@Key[1]), DataLen) mod Size
  else
    Result := 0;
end;

{ THashComparer_Cardinal_Relaxed<TKey> }

function THashComparer_Cardinal_Relaxed.KeyEqual(const Key1, Key2: Cardinal): Boolean;
begin
  Result := Key1 = Key2;
end;

function THashComparer_Cardinal_Relaxed.GetHashOf(const Key: Cardinal; const Size: Cardinal): Cardinal;
begin
  Result := Hash_SuperFastHash(@Key, SizeOf(Cardinal)) mod Size;
end;

{ THashElement<TKey,TValue> }

constructor THashElement<TKey,TValue>.Create(const Key: TKey; const Value: TValue);
begin
  FKey := Key;
  FValue := Value;
end;

function THashElement<TKey,TValue>.GetKey: TKey;
begin
  Result := FKey;
end;

function THashElement<TKey,TValue>.GetValue: TValue;
begin
  Result := FValue;
end;

{ THashEnumerator<TKey,TValue> }

constructor THashEnumerator<TKey,TValue>.Create(const HashTable: THashTableBase<TKey,TValue>);
begin
  FIndex := 0;
  try
    FActualCount := 0;
    SetLength(FElementList, HashTable.Count);
    HashTable.EnumerateKeys(InitializeEnum);
  finally
    FCurrentElement := nil;
    FIndex := 0;
  end;
end;

procedure THashEnumerator<TKey,TValue>.InitializeEnum(const Key: TKey; const Value: TValue);
var
  NewLength: Integer;
begin
  if FActualCount >= Length(FElementList) then
  begin
    NewLength := Max(Round(Length(FElementList) * 1.2), 100);
    SetLength(FElementList, NewLength);
  end;

  FElementList[FIndex] := THashElement<TKey,TValue>.Create(Key, Value);
  Inc(FActualCount);
  Inc(FIndex);
end;

function THashEnumerator<TKey,TValue>.MoveNext: Boolean;
begin
  Result := False;

  if FIndex < FActualCount then
  begin
    FCurrentElement := FElementList[FIndex];
    Result := True;
    Inc(FIndex);
  end;
end;

function THashEnumerator<TKey,TValue>._GetCurrent: IHashElement<TKey,TValue>;
begin
  Result := FCurrentElement;
end;

{ TCustomThreadSafeHashTable_Chaining<TKey, TValue> }

constructor TCustomThreadSafeHashTable_Chaining<TKey, TValue>.Create(const Comparer: ICustomHashComparer<TKey>; const Size: Cardinal);
begin
  inherited;

  FSRWLock.Initialize;
  {$IFDEF Threading_NoLockFreeStack}
    FDeleted := TThreadSafeQueue<THashedItemGeneric<TKey,TValue>>.Create(0, False);
  {$ELSE}
    FDeleted := TLockFreeStack<THashedItemGeneric<TKey,TValue>>.Create(0, False);
  {$ENDIF}
end;

destructor TCustomThreadSafeHashTable_Chaining<TKey, TValue>.Destroy;
begin
  InternalClear(FBuckets);
  ClearDeletedBuckets;
  FreeAndNil(FDeleted);

  inherited;
end;

function TCustomThreadSafeHashTable_Chaining<TKey, TValue>.AcquireNewBucket: THashedItemGeneric<TKey,TValue>;
var
  FoundItem: Boolean;
begin
{$IFDEF Threading_NoLockFreeStack}
  FoundItem := FDeleted.Dequeue(Result);
{$ELSE}
  FoundItem := FDeleted.Pop(Result);
{$ENDIF}

  if not FoundItem then
  begin
    Result := THashedItemGeneric<TKey,TValue>.Create;
    Result.NextItem := nil;
  end
  else
  begin
    Result.Value := Default(TValue);
    Result.Key := Default(TKey);
    Result.NextItem := nil;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    if Find(Key, KeyHash)^ <> nil then
      raise Exception.Create('Key already exists in hash table!');

    InternalAdd(Key, Value, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.Clear;
begin
  FSRWLock.AcquireExclusive;
  try
    InternalClear(FBuckets);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.ClearDeletedBuckets;
var
  P: THashedItemGeneric<TKey,TValue>;
  FoundItem: Boolean;
begin
  repeat
    {$IFDEF Threading_NoLockFreeStack}
      FoundItem := FDeleted.Dequeue(P);
    {$ELSE}
      FoundItem := FDeleted.Pop(P);
    {$ENDIF}

    if FoundItem then
      P.Free;
  until FoundItem = False;
end;

function TCustomThreadSafeHashTable_Chaining<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  FSRWLock.AcquireShared;
  try
    Result := Find(Key, KeyHash)^ <> nil;
  finally
    FSRWLock.ReleaseShared;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc<TKey, TValue>);
begin
  FSRWLock.AcquireShared;
  try
    inherited EnumerateKeys(EnumProc);
  finally
    FSRWLock.ReleaseShared;
  end;
end;

function TCustomThreadSafeHashTable_Chaining<TKey, TValue>.GetItem(const Key: TKey; var Value: TValue): Boolean;
var
  KeyHash: Cardinal;
  HashItem: THashedItemGeneric<TKey,TValue>;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  FSRWLock.AcquireShared;
  try
    HashItem := Find(Key, KeyHash)^;

    if HashItem <> nil then
    begin
      Value := HashItem.Value;
      Result := True;
    end
    else
    begin
      Value := Default(TValue);
      Result := False;
    end;
  finally
    FSRWLock.ReleaseShared;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.ReleaseOldBucket(const Bucket: THashedItemGeneric<TKey,TValue>);
var
  KeyType: Rtti.TValue;
  ValueType: Rtti.TValue;
begin
{$IFDEF Threading_NoLockFreeStack}
  FDeleted.Enqueue(Bucket);
{$ELSE}
  FDeleted.Push(Bucket);
{$ENDIF}

  if FOwnsObjects then
  begin
    KeyType := Rtti.TValue.From<TKey>(Bucket.Key);
    ValueType := Rtti.TValue.From<TValue>(Bucket.Value);

    if KeyType.TypeInfo.Kind = tkClass then
      KeyType.AsObject.Free;
    if ValueType.TypeInfo.Kind = tkClass then
      ValueType.AsObject.Free;
  end;

  Bucket.Key := Default(TKey);
  Bucket.Value := Default(TValue);
  Bucket.NextItem := nil;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.Remove(const Key: TKey);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    InternalRemove(Key, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining<TKey, TValue>.SetItem(const Key: TKey; const Value: TValue);
var
  KeyHash: Cardinal;
  HashItem: THashedItemGeneric<TKey,TValue>;
begin
  KeyHash := FHashComparer.GetHashOf(Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    HashItem := Find(Key, KeyHash)^;

    if HashItem <> nil then
      HashItem.Value := Value
    else
      InternalAdd(Key, Value, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

{ TCustomHashComparer<TKey> }

constructor TCustomHashComparer<TKey>.Create(const HashFunction: THashFunctionImplementation);
begin
  FHashFunction := HashFunction;
end;

{ TCardinalHashTable<TKey,TValue> }

constructor TCardinalHashTable<TValue>.Create(const Size: Cardinal);
begin
  inherited Create(THashComparer_Cardinal_Relaxed.Create(Hash_SuperFastHash), Size);
end;

{ TStringHashTable<TKey,TValue> }

constructor TStringHashTable<TValue>.Create(const Size: Cardinal);
begin
  inherited Create(THashComparer_String_Relaxed.Create(Hash_SuperFastHash), Size);
end;

{ TCardinalThreadSafeHashTable<TKey,TValue> }

constructor TCardinalThreadSafeHashTable<TValue>.Create(const Size: Cardinal);
begin
  inherited Create(THashComparer_Cardinal_Relaxed.Create(Hash_SuperFastHash), Size);
end;

{ TStringThreadSafeHashTable<TKey,TValue> }

constructor TStringThreadSafeHashTable<TValue>.Create(const Size: Cardinal);
begin
  inherited Create(THashComparer_String_Relaxed.Create(Hash_SuperFastHash), Size);
end;

end.
