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
 * Custom thread safe object pool with dynamic sizing and min pool size
 * =============================================================================
 * 08/03/2014 (1.0.0)
 *   - Initial implementation of a custom pool.
 * =============================================================================
*)
unit Cromis.Threading.CustomPool;

interface

uses
  SysUtils, Classes, Contnrs, Types,

  // threading units
  Cromis.Threading.Sync,
{$IF CompilerVersion >= 20}
  Cromis.Threading.TS.Generics
{$ELSE}
  Cromis.Threading.TS,
  Cromis.AnyValue
{$IFEND};

const
  cDefaultMinPoolSize = 5;

type
  TCustomObject = class
  private
    FLastRequest: TDateTime;
    FRequestCount: Int64;
  public
    constructor Create; virtual;
    procedure SetLastRequest;
    procedure IncreaseRequestCount;
    property RequestCount: Int64 read FRequestCount;
    property LastRequest: TDateTime read FLastRequest;
  end;

  TObjectClass = class of TCustomObject;
  TObjectArray = array of TCustomObject;

  TFreeObjectList = class
  private
  {$IFNDEF Threading_NoLockFreeStack}
    {$IF CompilerVersion >= 20}
      FInternalList: TLockFreeStack<TCustomObject>;
    {$ELSE}
      FInternalList: TLockFreeStack;
    {$IFEND}
  {$ELSE}
    FInternalList: TInterfaceList;
    FListLock: TSRWLock;
  {$ENDIF}
    function GetCount: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireObject: TCustomObject;
    procedure Clear;
    procedure AddObject(const AObject: TCustomObject);
    property Count: Int64 read GetCount;
  end;

  TAllObjectList = class
  private
    FListLock: TSRWLock;
    FInternalList: TObjectList;
    function GetCount: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddObject(const AObject: TCustomObject);
    procedure RemoveObject(const AObject: TCustomObject);
    property Count: Int64 read GetCount;
  end;

  TTSObjectsPool = class
  private
    FMinPoolSize: Integer;
    FDynamicPool: Boolean;
    FObjectClass: TObjectClass;
    FAllObjectsList: TAllObjectList;
    FFreeObjectsList: TFreeObjectList;
    function GetAllObjects: Integer;
    function GetFreeObjects: Integer;
  public
    constructor Create(const ObjectClass: TObjectClass); virtual;
    destructor Destroy; override;
    procedure InitializePool;
    function AcquireObject: TCustomObject; virtual;
    procedure ReleaseObject(const AObject: TCustomObject); virtual;
    property AllObjects: Integer read GetAllObjects;
    property FreeObjects: Integer read GetFreeObjects;
    property MinPollSize: Integer read FMinPoolSize write FMinPoolSize;
    property DynamicPool: Boolean read FDynamicPool write FDynamicPool;
  end;

implementation

{ TTSObjectsPool }

constructor TTSObjectsPool.Create(const ObjectClass: TObjectClass);
begin
  FMinPoolSize := cDefaultMinPoolSize;
  FDynamicPool := False;
  
  FObjectClass := ObjectClass;
  FAllObjectsList := TAllObjectList.Create;
  FFreeObjectsList := TFreeObjectList.Create;
end;

destructor TTSObjectsPool.Destroy;
begin
  FreeAndNil(FFreeObjectsList);
  FreeAndNil(FAllObjectsList);

  inherited;
end;

function TTSObjectsPool.AcquireObject: TCustomObject;
begin
  Result := FFreeObjectsList.AcquireObject;

  if Result = nil then
  begin
    Result := FObjectClass.Create;
    FAllObjectsList.AddObject(Result);
    Result.IncreaseRequestCount;
    Result.SetLastRequest;
  end;
end;

function TTSObjectsPool.GetAllObjects: Integer;
begin
  Result := FAllObjectsList.Count;
end;

function TTSObjectsPool.GetFreeObjects: Integer;
begin
  Result := FFreeObjectsList.Count;
end;

procedure TTSObjectsPool.InitializePool;
var
  I: Integer;
  AObject: TCustomObject;
begin
  for I := FAllObjectsList.Count to FMinPoolSize - 1 do
  begin
    AObject := FObjectClass.Create;
    FAllObjectsList.AddObject(AObject);
    FFreeObjectsList.AddObject(AObject);
  end;
end;

procedure TTSObjectsPool.ReleaseObject(const AObject: TCustomObject);
begin
  if AObject <> nil then
  begin
    if FDynamicPool then
    begin
      if FAllObjectsList.Count > FMinPoolSize then
      begin
        FAllObjectsList.RemoveObject(AObject);
        Exit;
      end;
    end;

    AObject.SetLastRequest;
    FFreeObjectsList.AddObject(AObject);
  end;
end;

{ TFreeObjectList }

constructor TFreeObjectList.Create;
const
  cInitialSize = 500;
begin
{$IFNDEF Threading_NoLockFreeStack}
  {$IF CompilerVersion >= 20}
    FInternalList := TLockFreeStack<TCustomObject>.Create(cInitialSize);
  {$ELSE}
    FInternalList := TLockFreeStack.Create(cInitialSize);
  {$IFEND};
{$ELSE}
  FInternalList := TList.Create;
  FListLock.Initialize;
{$ENDIF}
end;

destructor TFreeObjectList.Destroy;
begin
  FreeAndNil(FInternalList);

  inherited;
end;

function TFreeObjectList.AcquireObject: TCustomObject;
{$IFNDEF Threading_NoLockFreeStack}
{$IF CompilerVersion < 20}
var
  Value: TAnyValue;
{$IFEND}
{$ENDIF}
begin
{$IFNDEF Threading_NoLockFreeStack}
  {$IF CompilerVersion >= 20}
    FInternalList.Pop(Result);
  {$ELSE}
    FInternalList.Pop(Value);
    Result := TCustomObject(Value.AsObject);
  {$IFEND};
{$ELSE}
  FListLock.AcquireExclusive;
  try
    Result := nil;

    if FInternalList.Count > 0 then
    begin
      Result := TCustomObject(FInternalList.First);
      FInternalList.Remove(Result);
    end;
  finally
    FListLock.ReleaseExclusive;
  end;
{$ENDIF}
end;

procedure TFreeObjectList.AddObject(const AObject: TCustomObject);
begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Push(AObject);
{$ELSE}
  FListLock.AcquireExclusive;
  try
    FInternalList.Add(AObject);
  finally
    FListLock.ReleaseExclusive;
  end;
{$ENDIF}
end;

procedure TFreeObjectList.Clear;
begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Clear;
{$ELSE}
  FListLock.AcquireExclusive;
  try
    FInternalList.Clear;
  finally
    FListLock.ReleaseExclusive;
  end;
{$ENDIF}
end;

function TFreeObjectList.GetCount: Int64;
begin
{$IFNDEF Threading_NoLockFreeStack}
  Result := FInternalList.Count;
{$ELSE}
  FListLock.AcquireShared;
  try
    Result := FInternalList.Count;
  finally
    FListLock.ReleaseShared;
  end;
{$ENDIF}
end;

{ TAllObjectList }

constructor TAllObjectList.Create;
begin
  FInternalList := TObjectList.Create;
  FListLock.Initialize;
end;

destructor TAllObjectList.Destroy;
begin
  FreeAndNil(FInternalList);

  inherited;
end;

procedure TAllObjectList.AddObject(const AObject: TCustomObject);
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Add(AObject);
  finally
    FListLock.ReleaseExclusive;
  end;
end;

procedure TAllObjectList.Clear;
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Clear;
  finally
    FListLock.ReleaseExclusive;
  end;
end;

function TAllObjectList.GetCount: Int64;
begin
  FListLock.AcquireShared;
  try
    Result := FInternalList.Count;
  finally
    FListLock.ReleaseShared;
  end;
end;

procedure TAllObjectList.RemoveObject(const AObject: TCustomObject);
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Remove(AObject);
  finally
    FListLock.ReleaseExclusive;
  end;
end;

{ TCustomObject }

constructor TCustomObject.Create;
begin
  FRequestCount := 0;
  FLastRequest := 0;
end;

procedure TCustomObject.IncreaseRequestCount;
begin
  Inc(FRequestCount);
end;

procedure TCustomObject.SetLastRequest;
begin
  FLastRequest := Now;
end;

end.
