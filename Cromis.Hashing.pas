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
 * Hash related classes (Tables, hashing functions etc...)
 * =============================================================================
 * 24/08/2010 (1.0.0)
 *  - Initial implementation
 * 19/06/2011 (2.0.0)
 *  - Complete rewrite that uses TAnyValue and very fast hashing functions
 * 06/07/2012 (2.0.1)
 *  - Fixed hash calculation of actual implememented hash tables
 *  - Custom hash is abstract and never implemented
 * 17/07/2012 (2.1.0)
 *  - Added EnumerateKeys overload that uses anonymous function as parameter
 *  - Added EnumerateKeys overload that takes TEnumerateKeyExCallback
 *  - Use Hash_SuperFastHash as string hash function
 * 28/02/2013 (2.2.0)
 *  - Added support for node manager where deleted nodes are not disposed
 *  - Use PAnyValue where appropriate internally to avoid record copies
 * 06/10/2013 (2.2.1)
 *  - Enumeration support for easier enumeration of hash members
 * 06/10/2013 (2.2.2)
 *  - GetRaw added to allow direct access to hash members
 * 30/12/2013 (2.3.0)
 *  - ThreadSafe hash table implementation added
 *  - MurmurHash2 function added to hash functions
 * 03/01/2014 (2.4.0)
 *  - Open Addressing table implementation added
 *  - MurmurHash3 function added to hash functions
 *  - FNV1a function added to hash functions
 * 14/01/2014 (2.5.0)
 *  - User can provide its own key comparer function
 * 14/01/2014 (2.6.0)
 *  - Redesigned threading units by content and added generics support
 * ============================================================================
*)
unit Cromis.Hashing;

interface

uses
  Windows, SysUtils, IniFiles, Classes, Math,

  // Cromis units
  Cromis.Hashing.Common, Cromis.AnyValue, Cromis.Threading.Sync, Cromis.Threading.TS;

type
  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Value: TAnyValue;
    Next: PHashItem;
    Key: TAnyValue;
  end;
  TBuckets = array of PHashItem;

  PArrayItem = ^TArrayItem;
  TArrayItem = record
    Value: TAnyValue;
    Key: TAnyValue;
    Taken: Boolean;
  end;
  TOAArray = array of TArrayItem;

  // function and procedure definitons
  TEnumerateKeyExCallback = procedure(const Key, Value: PAnyValue; const Data: Pointer) of object;
  THashFunctionInstance = function(const Key: PAnyValue; const Size: Cardinal): Cardinal;
  TEnumerateKeyCallback = procedure(const Key, Value: PAnyValue) of object;
  THashFunction = function(Data: Pointer; const Length: Cardinal): Cardinal;
  TKeyComparer = function(const Key1, Key2: PAnyValue): Boolean;

  {$IF CompilerVersion >= 20}
    TEnumerateKeyAnonProc = reference to procedure(const Key: TAnyValue; const Value: TAnyValue);
  {$IFEND}

  IHashElement = Interface(IInterface)
  ['{94A8F32F-5972-4A9F-BD88-F8677AF1E18F}']
    function GetKey: PAnyValue;
    function GetValue: PAnyValue;
    property Key: PAnyValue read GetKey;
    property Value: PAnyValue read GetValue;
  end;

  IHashEnumerator = Interface(IInterface)
  ['{4D7E00D3-BFA9-4E3B-82F9-E590DAE74DE7}']
    // getters and setters
    function _GetCurrent: IHashElement;
    // iterator function and procedures
    function MoveNext: Boolean;
    property Current: IHashElement read _GetCurrent;
  end;

  THashTableBase = class
  strict protected
    FSize: Cardinal;
    FCount: Integer;
    FOwnsObjects: Boolean;
    FMaxCountBeforeResize: Integer;
    FKeyComparerInstance: TKeyComparer;
    FHashFunctionInstance: THashFunctionInstance;
    function GetItem(const Key: TAnyValue): TAnyValue; virtual; abstract;
    procedure OnEnumKeys(const Key, Value: PAnyValue; const Data: Pointer);
    procedure OnEnumValues(const Key, Value: PAnyValue; const Data: Pointer);
    procedure SetItem(const Key: TAnyValue; const Value: TAnyValue); virtual; abstract;
  public
    constructor Create(const HashFunction: THashFunctionInstance; const KeyComparer: TKeyComparer; const Size: Cardinal = cDefaultHashSize); virtual;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer); overload; virtual; abstract;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyCallback); overload; virtual; abstract;
  {$IF CompilerVersion >= 20}
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc); overload; virtual; abstract;
  {$IFEND}
    function Keys: IAnyArray;
    function Values: IAnyArray;
    function GetEnumerator: IHashEnumerator; virtual;
    function GetRaw(const Key: TAnyValue): PAnyValue; virtual;
    function ContainsKey(const Key: TAnyValue): Boolean; virtual; abstract;
    procedure Add(const Key: TAnyValue; const Value: TAnyValue); virtual; abstract;
    procedure Remove(const Key: TAnyValue); virtual; abstract;
    procedure Clear; virtual; abstract;
    property Item[const Key: TAnyValue]: TAnyValue read GetItem write SetItem;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Count: Integer read FCount;
  end;

  THashTableBase_Chaining = class(THashTableBase)
  strict private
    function CheckHashTableSize: Boolean;
    procedure Resize(const NewSize: Cardinal);
  strict protected
    FBuckets: TBuckets;
    function AcquireNewBucket: PHashItem; virtual; abstract;
    procedure InternalClear(const Buckets: TBuckets); virtual;
    procedure ReleaseOldBucket(const Bucket: PHashItem); virtual; abstract;
    procedure InternalRemove(const Key: PAnyValue; const KeyHash: Cardinal); virtual;
    function Find(const Key: PAnyValue; const KeyHash: Cardinal): PPHashItem; inline;
    procedure InternalAdd(const Key: PAnyValue; const Value: PAnyValue; KeyHash: Cardinal); inline;
  public
    constructor Create(const HashFunction: THashFunctionInstance; const KeyComparer: TKeyComparer; const Size: Cardinal = cDefaultHashSize); override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer); overload; override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyCallback); overload; override;
  {$IF CompilerVersion >= 20}
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc); overload; override;
  {$IFEND}
  end;

  THashTableBase_Open = class(THashTableBase)
  strict private
    FCollision: Integer;
    procedure Resize(const NewSize: Cardinal);
  strict protected
    FItemArray: TOAArray;
    procedure InternalClear(const ItemArray: TOAArray); virtual;
    procedure InternalRemove(const Key: PAnyValue; const KeyHash: Cardinal); virtual;
    procedure InternalAdd(const Key, Value: PAnyValue; ArrayItem: PArrayItem); virtual;
    function Find(const Key: PAnyValue; const KeyHash: Cardinal): PArrayItem; inline;
  public
    constructor Create(const HashFunction: THashFunctionInstance; const KeyComparer: TKeyComparer; const Size: Cardinal = cDefaultHashSize); override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer); overload; override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyCallback); overload; override;
  {$IF CompilerVersion >= 20}
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc); overload; override;
  {$IFEND}
    property Collision: Integer read FCollision;
  end;

  TCustomHashTable_Chaining = class(THashTableBase_Chaining)
  private
    FDeleted: PHashItem;
    procedure ClearDeletedBuckets; inline;
    procedure InitializeDeletedItems(const Size: Cardinal);
  protected
    function AcquireNewBucket: PHashItem; override;
    function GetItem(const Key: TAnyValue): TAnyValue; override;
    procedure ReleaseOldBucket(const Bucket: PHashItem); override;
    procedure SetItem(const Key: TAnyValue; const Value: TAnyValue); override;
  public
    constructor Create(const HashFunction: THashFunctionInstance; const KeyComparer: TKeyComparer; const Size: Cardinal = cDefaultHashSize); override;
    destructor Destroy; override;
    function ContainsKey(const Key: TAnyValue): Boolean; override;
    procedure Add(const Key: TAnyValue; const Value: TAnyValue); override;
    function GetRaw(const Key: TAnyValue): PAnyValue; override;
    procedure Remove(const Key: TAnyValue); override;
    procedure Clear; override;
  end;

  TCustomThreadSafeHashTable_Chaining = class(THashTableBase_Chaining)
  private
    FSRWLock: TSRWLock;
  {$IFDEF Threading_NoLockFreeStack}
    FDeleted: TThreadSafeQueue;
  {$ELSE}
    FDeleted: TLockFreeStack;
  {$ENDIF}
    procedure ClearDeletedBuckets; 
  protected
    function AcquireNewBucket: PHashItem; override;
    function GetItem(const Key: TAnyValue): TAnyValue; override;
    procedure ReleaseOldBucket(const Bucket: PHashItem); override;
    procedure SetItem(const Key: TAnyValue; const Value: TAnyValue); override;
  public
    constructor Create(const HashFunction: THashFunctionInstance; const KeyComparer: TKeyComparer; const Size: Cardinal = cDefaultHashSize); override;
    destructor Destroy; override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer); override;
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyCallback); override;
  {$IF CompilerVersion >= 20}
    procedure EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc); override;
  {$IFEND}
    function ContainsKey(const Key: TAnyValue): Boolean; override;
    procedure Add(const Key: TAnyValue; const Value: TAnyValue); override;
    procedure Remove(const Key: TAnyValue); override;
    procedure Clear; override;
  end;

  TCustomHashTable_Open = class(THashTableBase_Open)
  protected
    function GetItem(const Key: TAnyValue): TAnyValue; override;
    procedure SetItem(const Key: TAnyValue; const Value: TAnyValue); override;
  public
    function ContainsKey(const Key: TAnyValue): Boolean; override;
    procedure Add(const Key: TAnyValue; const Value: TAnyValue); override;
    function GetRaw(const Key: TAnyValue): PAnyValue; override;
    procedure Remove(const Key: TAnyValue); override;
    procedure Clear; override;
  end;

  // hash table that uses integers as keys
  TCardinalHashTable = class(TCustomHashTable_Chaining)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // hash table that uses strings as keys
  TStringHashTable = class(TCustomHashTable_Chaining)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // thread safe hash table that uses integers as keys
  TCardinalThreadSafeHashTable = class(TCustomThreadSafeHashTable_Chaining)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // thread safe hash table that uses strings as keys
  TStringThreadSafeHashTable = class(TCustomThreadSafeHashTable_Chaining)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // thread safe hash table that uses integers as keys
  TCardinalHashTableOpen = class(TCustomHashTable_Open)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

  // hash table that uses strings as keys
  TStringHashTableOpen = class(TCustomHashTable_Open)
  public
    constructor Create(const Size: Cardinal = cDefaultHashSize); reintroduce;
  end;

implementation

type
  TCardinalArray = array[0..(MaxInt div SizeOf(Cardinal)) - 1] of Cardinal;
  PCardinalArray = ^TCardinalArray;

(* =============================================================================*)
(* Enumeration support. It enumarates as Interface elements.                    *)
(* This allows for sipmle element auto-memory release after usage               *)
(* =============================================================================*)

type
  THashElement = class(TInterfacedObject, IHashElement)
  private
    FKey: PAnyValue;
    FValue: PAnyValue;
    function GetKey: PAnyValue;
    function GetValue: PAnyValue;
  public
    constructor Create(const Key, Value: PAnyValue);
    property Key: PAnyValue read GetKey;
    property Value: PAnyValue read GetValue;
  end;

  THashEnumerator = class(TInterfacedObject, IHashEnumerator)
  private
    FIndex: Integer;
    FActualCount: Integer;
    FElementList: array of IHashElement;
    FCurrentElement: IHashElement;
    function _GetCurrent: IHashElement;
    procedure InitializeEnum(const Key, Value: PAnyValue);
  public
    constructor Create(const HashTable: THashTableBase);
    property Current: IHashElement read _GetCurrent;
    function MoveNext: Boolean;
  end;

(* =============================================================================*)
(* Hashing implementation function stub                                         *)
(* Implement this to pass to constructor and thus have your own hash function   *)
(* =============================================================================*)

function CalculateHash_String_SuperFastHash(const Key: PAnyValue; const Size: Cardinal): Cardinal;
begin
  Result := Hash_SuperFastHash(PChar(Key.AsPointer), Key.ValueSize) mod Size;
end;

function CalculateHash_Cardinal_SuperFastHash(const Key: PAnyValue; const Size: Cardinal): Cardinal;
var
  KeyData: Cardinal;
begin
  KeyData := Key.AsCardinal;
  Result := Hash_SuperFastHash(@KeyData, SizeOf(Cardinal)) mod Size;
end;

(* =============================================================================*)
(* Key comparer functions                                                       *)
(* =============================================================================*)

function KeyComparer_String_Relaxed(const Key1, Key2: PAnyValue): Boolean; inline;
begin
  Result := Key1.AsString = Key2.AsString;
end;

function KeyComparer_Cardinal_Relaxed(const Key1, Key2: PAnyValue): Boolean; inline;
begin
  Result := Key1.AsCardinal = Key2.AsCardinal;
end;

function KeyComparer_General_Strict(const Key1, Key2: PAnyValue): Boolean; inline;
begin
  Result := Key1.Equal(Key2);
end;

{ TCustomHashTable_Chaining }

constructor TCustomHashTable_Chaining.Create(const HashFunction: THashFunctionInstance;
                                             const KeyComparer: TKeyComparer;
                                             const Size: Cardinal);
begin
  inherited;

  InitializeDeletedItems(FSize div 10);
end;

destructor TCustomHashTable_Chaining.Destroy;
begin
  InternalClear(FBuckets);
  ClearDeletedBuckets;

  inherited;
end;

procedure TCustomHashTable_Chaining.SetItem(const Key: TAnyValue; const Value: TAnyValue);
var
  KeyHash: Cardinal;
  HashItem: PHashItem;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);
  HashItem := Find(@Key, KeyHash)^;

  if HashItem <> nil then
    CopyAnyValue(@HashItem.Value, @Value)
  else
    InternalAdd(@Key, @Value, KeyHash);
end;

function TCustomHashTable_Chaining.GetItem(const Key: TAnyValue): TAnyValue;
var
  HashItem: PHashItem;
begin
  HashItem := Find(@Key, FHashFunctionInstance(@Key, FSize))^;

  if HashItem <> nil then
    CopyAnyValue(@Result, @HashItem.Value)
  else
    Result.Clear;
end;

function TCustomHashTable_Chaining.GetRaw(const Key: TAnyValue): PAnyValue;
var
  HashItem: PHashItem;
begin
  HashItem := Find(@Key, FHashFunctionInstance(@Key, FSize))^;

  if HashItem <> nil then
    Result := @HashItem.Value
  else
    Result := nil;
end;

function TCustomHashTable_Chaining.ContainsKey(const Key: TAnyValue): Boolean;
begin
  Result := Find(@Key, FHashFunctionInstance(@Key, FSize))^ <> nil;
end;

procedure TCustomHashTable_Chaining.InitializeDeletedItems(const Size: Cardinal);
var
  I: Integer;
  Bucket: PHashItem;
begin
  for I := 1 to Size do
  begin
    New(Bucket);
    Bucket.Next := FDeleted;
    FDeleted := Bucket;
  end;
end;

procedure TCustomHashTable_Chaining.Clear;
begin
  InternalClear(FBuckets);
  FCount := 0;
end;

procedure TCustomHashTable_Chaining.ClearDeletedBuckets;
var
  P, N: PHashItem;
begin
  P := FDeleted;

  while P <> nil do
  begin
    N := P^.Next;
    Dispose(P);
    P := N;
  end;
end;

function TCustomHashTable_Chaining.AcquireNewBucket: PHashItem;
begin
  if FDeleted <> nil then
  begin
    Result := FDeleted;
    FDeleted := FDeleted.Next;
    Result.Next := nil;
  end
  else
  begin
    New(Result);
    Result^.Next := nil;
  end;
end;

procedure TCustomHashTable_Chaining.Add(const Key: TAnyValue; const Value: TAnyValue);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  if Find(@Key, KeyHash)^ <> nil then
    raise Exception.CreateFmt('Key "%s" already exists in hash table.', [Key.AsString]);

  InternalAdd(@Key, @Value, KeyHash);
end;

procedure TCustomHashTable_Chaining.ReleaseOldBucket(const Bucket: PHashItem);
begin
  Bucket.Next := FDeleted;
  FDeleted := Bucket;

  if FOwnsObjects then
  begin
    if Bucket.Key.GetValueType = avtObject then
      Bucket.Key.AsObject.Free;
    if Bucket.Value.GetValueType = avtObject then
      Bucket.Value.AsObject.Free;
  end;

  // clear value and key
  Bucket.Value.Clear;
  Bucket.Key.Clear;
end;

procedure TCustomHashTable_Chaining.Remove(const Key: TAnyValue);
begin
  InternalRemove(@Key, FHashFunctionInstance(@Key, FSize));
end;

{ TCardinalHashTable }

constructor TCardinalHashTable.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_Cardinal_SuperFastHash, KeyComparer_Cardinal_Relaxed, Size);
end;

{ TStringHashTable }

constructor TStringHashTable.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_String_SuperFastHash, KeyComparer_String_Relaxed, Size);
end;

{ THashElement }

constructor THashElement.Create(const Key, Value: PAnyValue);
begin
  FKey := Key;
  FValue := Value;
end;

function THashElement.GetKey: PAnyValue;
begin
  Result := FKey;
end;

function THashElement.GetValue: PAnyValue;
begin
  Result := FValue;
end;

{ THashEnumerator }

constructor THashEnumerator.Create(const HashTable: THashTableBase);
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

procedure THashEnumerator.InitializeEnum(const Key, Value: PAnyValue);
var
  NewLength: Integer;
begin
  if FActualCount >= Length(FElementList) then
  begin
    NewLength := Max(Round(Length(FElementList) * 1.2), 100);
    SetLength(FElementList, NewLength);
  end;

  FElementList[FIndex] := IHashElement(THashElement.Create(Key, Value));
  Inc(FActualCount);
  Inc(FIndex);
end;

function THashEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if FIndex < FActualCount then
  begin
    FCurrentElement := IHashElement(FElementList[FIndex]);
    Result := True;
    Inc(FIndex);
  end;
end;

function THashEnumerator._GetCurrent: IHashElement;
begin
  Result := FCurrentElement;
end;

{ TCustomThreadSafeHashTable_Chaining }

function TCustomThreadSafeHashTable_Chaining.AcquireNewBucket: PHashItem;
var
  Item: TAnyValue;
begin
{$IFDEF Threading_NoLockFreeStack}
  FDeleted.Dequeue(Item);
{$ELSE}
  FDeleted.Pop(Item);
{$ENDIF}

  if Item.IsEmpty then
  begin
    New(Result);
    Result^.Next := nil;
  end
  else
  begin
    Result := PHashItem(Item.AsPointer);
    Result^.Next := nil;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.Add(const Key, Value: TAnyValue);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    if Find(@Key, KeyHash)^ <> nil then
      raise Exception.CreateFmt('Key "%s" already exists in hash table.', [Key.AsString]);

    InternalAdd(@Key, @Value, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.Clear;
begin
  FSRWLock.AcquireExclusive;
  try
    InternalClear(FBuckets);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.ClearDeletedBuckets;
var
  P: TAnyValue;
  Empty: Boolean;
begin
  repeat
    Empty := True;

    {$IFDEF Threading_NoLockFreeStack}
      FDeleted.Dequeue(P);
    {$ELSE}
      FDeleted.Pop(P);
    {$ENDIF}

    if not P.IsEmpty then
    begin
      Empty := False;
      Dispose(PHashItem(P.AsPointer));
    end;
  until Empty = True;
end;

function TCustomThreadSafeHashTable_Chaining.ContainsKey(const Key: TAnyValue): Boolean;
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  FSRWLock.AcquireShared;
  try
    Result := Find(@Key, KeyHash)^ <> nil;
  finally
    FSRWLock.ReleaseShared;
  end;
end;

constructor TCustomThreadSafeHashTable_Chaining.Create(const HashFunction: THashFunctionInstance;
                                                       const KeyComparer: TKeyComparer;
                                                       const Size: Cardinal);
begin
  inherited;

  FSRWLock.Initialize;
  {$IFDEF Threading_NoLockFreeStack}
    FDeleted := TThreadSafeQueue.Create(0, False);
  {$ELSE}
    FDeleted := TLockFreeStack.Create(0, False);
  {$ENDIF}
end;

destructor TCustomThreadSafeHashTable_Chaining.Destroy;
begin
  InternalClear(FBuckets);
  ClearDeletedBuckets;
  FreeAndNil(FDeleted);

  inherited;
end;

procedure TCustomThreadSafeHashTable_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyCallback);
begin
  FSRWLock.AcquireShared;
  try
    inherited EnumerateKeys(EnumProc);
  finally
    FSRWLock.ReleaseShared;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer);
begin
  FSRWLock.AcquireShared;
  try
    inherited EnumerateKeys(EnumProc, Data);
  finally
    FSRWLock.ReleaseShared;
  end;
end;

{$IF CompilerVersion >= 20}
procedure TCustomThreadSafeHashTable_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc);
begin
  FSRWLock.AcquireShared;
  try
    inherited EnumerateKeys(EnumProc);
  finally
    FSRWLock.ReleaseShared;
  end;
end;
{$IFEND}

function TCustomThreadSafeHashTable_Chaining.GetItem(const Key: TAnyValue): TAnyValue;
var
  KeyHash: Cardinal;
  HashItem: PHashItem;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  FSRWLock.AcquireShared;
  try
    HashItem := Find(@Key, KeyHash)^;

    if HashItem <> nil then
      CopyAnyValue(@Result, @HashItem^.Value)
    else
      Result.Clear;
  finally
    FSRWLock.ReleaseShared;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.ReleaseOldBucket(const Bucket: PHashItem);
begin
{$IFDEF Threading_NoLockFreeStack}
  FDeleted.Enqueue(Bucket);
{$ELSE}
  FDeleted.Push(Bucket);
{$ENDIF}

  if FOwnsObjects then
  begin
    if Bucket^.Key.GetValueType = avtObject then
      Bucket^.Key.AsObject.Free;
    if Bucket^.Value.GetValueType = avtObject then
      Bucket^.Value.AsObject.Free;
  end;

  // clear value and key
  Bucket^.Value.Clear;
  Bucket^.Key.Clear;
end;

procedure TCustomThreadSafeHashTable_Chaining.Remove(const Key: TAnyValue);
var
  KeyHash: Cardinal;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    InternalRemove(@Key, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

procedure TCustomThreadSafeHashTable_Chaining.SetItem(const Key, Value: TAnyValue);
var
  KeyHash: Cardinal;
  HashItem: PHashItem;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);

  FSRWLock.AcquireExclusive;
  try
    HashItem := Find(@Key, KeyHash)^;

    if HashItem <> nil then
      CopyAnyValue(@HashItem^.Value, @Value)
    else
      InternalAdd(@Key, @Value, KeyHash);
  finally
    FSRWLock.ReleaseExclusive;
  end;
end;

{ THashTableBase_Chaining }

function THashTableBase_Chaining.CheckHashTableSize: Boolean;
begin
  Result := False;

  if FCount = FMaxCountBeforeResize then
  begin
    Resize(2 * FSize);
    Result := True;
  end;
end;

constructor THashTableBase_Chaining.Create(const HashFunction: THashFunctionInstance;
                                           const KeyComparer: TKeyComparer;
                                           const Size: Cardinal);
begin
  inherited;

  SetLength(FBuckets, FSize);
  // calculate max items before resize
  FMaxCountBeforeResize := CalculateMaxItemsBeforeResize(FSize);
end;

procedure THashTableBase_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer);
var
  I: Integer;
  Item: PHashItem;
begin
  for I := Low(FBuckets) to High(FBuckets) do
  begin
    Item := FBuckets[I];

    while Item <> nil do
    begin
      EnumProc(@Item.Key, @Item.Value, Data);
      Item := Item.Next;
    end;
  end;
end;

procedure THashTableBase_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyCallback);
var
  I: Integer;
  Item: PHashItem;
begin
  for I := Low(FBuckets) to High(FBuckets) do
  begin
    Item := FBuckets[I];

    while Item <> nil do
    begin
      EnumProc(@Item.Key, @Item.Value);
      Item := Item.Next;
    end;
  end;
end;

{$IF CompilerVersion >= 20}
procedure THashTableBase_Chaining.EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc);
var
  I: Integer;
  Item: PHashItem;
begin
  for I := Low(FBuckets) to High(FBuckets) do
  begin
    Item := FBuckets[I];

    while Item <> nil do
    begin
      EnumProc(@Item.Key, @Item.Value);
      Item := Item.Next;
    end;
  end;
end;
{$IFEND}

function THashTableBase_Chaining.Find(const Key: PAnyValue; const KeyHash: Cardinal): PPHashItem;
begin
  Result := @FBuckets[KeyHash];

  while Result^ <> nil do
  begin
    if FKeyComparerInstance(@Result^.Key, Key) then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

procedure THashTableBase_Chaining.InternalAdd(const Key, Value: PAnyValue; KeyHash: Cardinal);
var
  Bucket: PHashItem;
begin
  if CheckHashTableSize then
    KeyHash := FHashFunctionInstance(Key, FSize);

  Bucket := AcquireNewBucket;
  CopyAnyValue(@Bucket.Key, Key);
  CopyAnyValue(@Bucket.Value, Value);
  Bucket.Next := FBuckets[KeyHash];
  FBuckets[KeyHash] := Bucket;
  Inc(FCount);
end;

procedure THashTableBase_Chaining.InternalClear(const Buckets: TBuckets);
var
  I: Integer;
  P, N: PHashItem;
begin
  for I := Low(Buckets) to High(Buckets) do
  begin
    P := Buckets[I];

    while P <> nil do
    begin
      N := P^.Next;
      ReleaseOldBucket(P);
      P := N;
    end;

    Buckets[I] := nil;
  end;
end;

procedure THashTableBase_Chaining.InternalRemove(const Key: PAnyValue; const KeyHash: Cardinal);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key, KeyHash);
  P := Prev^;

  if P <> nil then
  begin
    Prev^ := P.Next;
    Dec(FCount);

    // return P to deleted
    ReleaseOldBucket(P);
  end;
end;

procedure THashTableBase_Chaining.Resize(const NewSize: Cardinal);
var
  OldBuckets: TBuckets;
  BucketIndex: Integer;
  HashItem: PHashItem;
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
      InternalAdd(@HashItem.Key, @HashItem.Value, FHashFunctionInstance(@HashItem.Key, FSize));
      HashItem := HashItem^.Next;
    end;
  end;

  InternalClear(OldBuckets);
end;

{ TCardinalThreadSafeHashTable }

constructor TCardinalThreadSafeHashTable.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_Cardinal_SuperFastHash, KeyComparer_Cardinal_Relaxed, Size);
end;

{ TStringThreadSafeHashTable }

constructor TStringThreadSafeHashTable.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_String_SuperFastHash, KeyComparer_String_Relaxed, Size);
end;

{ THashTableBase }

constructor THashTableBase.Create(const HashFunction: THashFunctionInstance;
                                  const KeyComparer: TKeyComparer;
                                  const Size: Cardinal);
begin
  FSize := GetGoodHashSize(Size);
  FKeyComparerInstance := KeyComparer;
  FHashFunctionInstance := HashFunction;
end;

function THashTableBase.GetEnumerator: IHashEnumerator;
begin
  Result := THashEnumerator.Create(Self);
end;

function THashTableBase.GetRaw(const Key: TAnyValue): PAnyValue;
begin
  raise Exception.Create('GetRaw is not supported!');
end;

function THashTableBase.Keys: IAnyArray;
begin
  Result := CreateAnyArray;
  EnumerateKeys(OnEnumKeys, Pointer(Result));
end;

procedure THashTableBase.OnEnumKeys(const Key, Value: PAnyValue; const Data: Pointer);
begin
  IAnyArray(Data).Push(Key);
end;

procedure THashTableBase.OnEnumValues(const Key, Value: PAnyValue; const Data: Pointer);
begin
  IAnyArray(Data).Push(Value);
end;

function THashTableBase.Values: IAnyArray;
begin
  Result := CreateAnyArray;
  EnumerateKeys(OnEnumValues, Pointer(Result));
end;

{ THashTableBase_Open }

constructor THashTableBase_Open.Create(const HashFunction: THashFunctionInstance;
                                       const KeyComparer: TKeyComparer;
                                       const Size: Cardinal);
begin
  inherited;

  SetLength(FItemArray, FSize);
  // calculate max items before resize
  FMaxCountBeforeResize := CalculateMaxItemsBeforeResize(FSize);
end;

procedure THashTableBase_Open.EnumerateKeys(const EnumProc: TEnumerateKeyExCallback; const Data: Pointer);
var
  Index: Integer;
begin
  for Index := Low(FItemArray) to High(FItemArray) do
    EnumProc(@FItemArray[Index].Key, @FItemArray[Index].Value, Data);
end;

procedure THashTableBase_Open.EnumerateKeys(const EnumProc: TEnumerateKeyCallback);
var
  Index: Integer;
begin
  for Index := Low(FItemArray) to High(FItemArray) do
    EnumProc(@FItemArray[Index].Key, @FItemArray[Index].Value);
end;

{$IF CompilerVersion >= 20}
procedure THashTableBase_Open.EnumerateKeys(const EnumProc: TEnumerateKeyAnonProc);
var
  Index: Integer;
begin
  for Index := Low(FItemArray) to High(FItemArray) do
    EnumProc(@FItemArray[Index].Key, @FItemArray[Index].Value);
end;
{$IFEND}

function THashTableBase_Open.Find(const Key: PAnyValue; const KeyHash: Cardinal): PArrayItem;
var
  RealHash: Cardinal;
begin
  Result := @FItemArray[KeyHash];

  if not Result.Taken or FKeyComparerInstance(@Result.Key, Key) then
    Exit;

  // check for collisioned items
  RealHash := KeyHash;

  repeat
    RealHash := (RealHash + 1) mod FSize;
    Result := @FItemArray[RealHash];
    Inc(FCollision);
  until (Result.Taken or FKeyComparerInstance(@Result.Key, Key));
end;

procedure THashTableBase_Open.InternalAdd(const Key, Value: PAnyValue; ArrayItem: PArrayItem);
var
  KeyHash: Cardinal;
begin
  if FCount = FMaxCountBeforeResize then
  begin
    Resize(2 * FCount);
    KeyHash := FHashFunctionInstance(Key, FSize);
    ArrayItem := Find(Key, KeyHash);
  end;

  if (ArrayItem <> nil) and (not ArrayItem.Taken) then
  begin
    CopyAnyValue(@ArrayItem.Value, Value);
    CopyAnyValue(@ArrayItem.Key, Key);
    ArrayItem.Taken := True;
    Inc(FCount);
  end;
end;

procedure THashTableBase_Open.InternalClear(const ItemArray: TOAArray);
var
  I: Integer;
begin
  for I := Low(ItemArray) to High(ItemArray) do
  begin
    ItemArray[I].Taken := False;
    ItemArray[I].Value.Clear;
    ItemArray[I].Key.Clear;
  end;

  FCount := 0;
end;

procedure THashTableBase_Open.InternalRemove(const Key: PAnyValue; const KeyHash: Cardinal);
var
  IsEmpty: Boolean;
  RealHash: Cardinal;
  ArrayItem: PArrayItem;
begin
  ArrayItem := @FItemArray[KeyHash];

  if ArrayItem.Taken then
  begin
    ArrayItem.Taken := False;
    ArrayItem.Value.Clear;
    ArrayItem.Key.Clear;
  end;

  // check for collisioned items
  RealHash := KeyHash;

  repeat
    RealHash := (RealHash + 1) mod FSize;
    ArrayItem := @FItemArray[RealHash];
    IsEmpty := not ArrayItem.Taken;

    if ArrayItem.Taken then
    begin
      ArrayItem.Taken := False;
      ArrayItem.Value.Clear;
      ArrayItem.Key.Clear;
    end;
  until IsEmpty;
end;

procedure THashTableBase_Open.Resize(const NewSize: Cardinal);
var
  ArrayItem: PArrayItem;
  OldArray: TOAArray;
  Index: Integer;
begin
  FSize := GetGoodHashSize(NewSize);
  FMaxCountBeforeResize := CalculateMaxItemsBeforeResize(FSize);

  OldArray := FItemArray;
  FItemArray := nil;
  SetLength(FItemArray, FSize);

  for Index := Low(OldArray) to High(OldArray) do
  begin
    ArrayItem := @OldArray[Index];
    InternalAdd(@ArrayItem.Key, @ArrayItem.Value, Find(@ArrayItem.Key, FHashFunctionInstance(@ArrayItem.Key, FSize)));
  end;

  InternalClear(OldArray);
end;

{ TCustomHashTable_OpenDH }

procedure TCustomHashTable_Open.Add(const Key, Value: TAnyValue);
var
  KeyHash: Cardinal;
  ArrayItem: PArrayItem;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);
  ArrayItem := Find(@Key, KeyHash);

  if (ArrayItem <> nil) and (ArrayItem.Taken) then
    raise Exception.CreateFmt('Key "%s" already exists in hash table.', [Key.AsString]);

  InternalAdd(@Key, @Value, ArrayItem);
end;

procedure TCustomHashTable_Open.Clear;
begin
  InternalClear(FItemArray);
  FCount := 0;
end;

function TCustomHashTable_Open.ContainsKey(const Key: TAnyValue): Boolean;
begin
  Result := Find(@Key, FHashFunctionInstance(@Key, FSize)).Taken;
end;

function TCustomHashTable_Open.GetItem(const Key: TAnyValue): TAnyValue;
var
  ArrayItem: PArrayItem;
begin
  ArrayItem := Find(@Key, FHashFunctionInstance(@Key, FSize));

  if (ArrayItem <> nil) and (ArrayItem.Taken) then
    CopyAnyValue(@Result, @ArrayItem.Value)
  else
    Result.Clear;
end;

function TCustomHashTable_Open.GetRaw(const Key: TAnyValue): PAnyValue;
var
  ArrayItem: PArrayItem;
begin
  ArrayItem := Find(@Key, FHashFunctionInstance(@Key, FSize));

  if (ArrayItem <> nil) and (ArrayItem.Taken) then
    Result := @ArrayItem.Value
  else
    Result := nil;
end;

procedure TCustomHashTable_Open.Remove(const Key: TAnyValue);
begin
  InternalRemove(@Key, FHashFunctionInstance(@Key, FSize));
end;

procedure TCustomHashTable_Open.SetItem(const Key, Value: TAnyValue);
var
  KeyHash: Cardinal;
  ArrayItem: PArrayItem;
begin
  KeyHash := FHashFunctionInstance(@Key, FSize);
  ArrayItem := Find(@Key, KeyHash);

  if (ArrayItem <> nil) and (ArrayItem.Taken) then
    CopyAnyValue(@ArrayItem.Value, @Value)
  else
    InternalAdd(@Key, @Value, ArrayItem);
end;

{ TStringHashTableOpen }

constructor TStringHashTableOpen.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_String_SuperFastHash, KeyComparer_String_Relaxed, Size);
end;

{ TCardinalHashTableOpen }

constructor TCardinalHashTableOpen.Create(const Size: Cardinal);
begin
  inherited Create(CalculateHash_Cardinal_SuperFastHash, KeyComparer_Cardinal_Relaxed, Size);
end;

end.

