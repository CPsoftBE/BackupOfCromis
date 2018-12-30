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
 * Threading synchronization primitives and helpers
 * Refer to Cromis.Threading for changelog
 * =============================================================================
*)
unit Cromis.Threading.Sync;

interface

uses
  Windows, SysUtils;

type
  // synchronization primitives
  TSRWLock = record
    FHandle: Pointer;
    procedure Initialize;
    procedure Finalize;
    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
    function TryAcquireShared: Boolean;
    function TryAcquireExclusive: Boolean;
  end;

  TSpinLock = record
    FLock: Integer;
    function Acquire: Boolean; inline;
    function Release: Boolean; inline;
  end;

  TSpinLockArray = record
    FLockArray: array of TSpinLock;
    function Acquire(const Index: Cardinal): Boolean; inline;
    function Release(const Index: Cardinal): Boolean; inline;
    procedure Initialize(const Size: Cardinal);
    procedure Finalize;
  end;

  TNamedLock = record
    function Acquire(const Name: string): THandle; inline;
    procedure Release(const Handle: THandle); inline;
  end;

  function CASV32(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer; inline;
  function CASP32(var Destination: Pointer; Exchange: Pointer; Comparand: pointer): Pointer; inline;

implementation

var
  KernelHandle: HModule;
  UseSRWFallback: Boolean = False;
  CallInitializeSRWLock: function (out P: Pointer): NativeUInt; stdcall;
  CallAcquireSRWLockShared: function (var P: Pointer): NativeUInt; stdcall;
  CallReleaseSRWLockShared: function (var P: Pointer): NativeUInt; stdcall;
  CallAcquireSRWLockExclusive: function (var P: Pointer): NativeUInt; stdcall;
  CallReleaseSRWLockExclusive: function (var P: Pointer): NativeUInt; stdcall;
  CallTryAcquireSRWLockExclusive: function (var P: Pointer): BOOL; stdcall;
  CallTryAcquireSRWLockShared: function (var P: Pointer): BOOL; stdcall;

function CASV32(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer; inline;
begin
  Result := InterlockedCompareExchange(Destination, Exchange, Comparand);
end;

function CASP32(var Destination: Pointer; Exchange: Pointer; Comparand: pointer): Pointer; inline;
{$IFDEF CPUX64}
var
  TempDest: Integer;
{$ENDIF}
begin
{$IFDEF CPUX64}
  Result := Pointer(InterlockedCompareExchange(TempDest, Integer(Exchange), Integer(Comparand)));
  Destination := Pointer(TempDest);
{$ELSE}
  Result := Pointer(InterlockedCompareExchange(Integer(Destination), Integer(Exchange), Integer(Comparand)));
{$ENDIF}
end;

{ TSRWLock }

procedure TSRWLock.Initialize;
begin
  if UseSRWFallback then
  begin
    FHandle := New(PRTLCriticalSection);
    InitializeCriticalSection(PRTLCriticalSection(FHandle)^);
  end
  else
    CallInitializeSRWLock(FHandle);
end;

procedure TSRWLock.Finalize;
begin
  if UseSRWFallback then
  begin
    DeleteCriticalSection(PRTLCriticalSection(FHandle)^);
    FreeMem(FHandle);
  end;
end;

procedure TSRWLock.AcquireExclusive;
begin
  if UseSRWFallback then
    EnterCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    CallAcquireSRWLockExclusive(FHandle);
end;

procedure TSRWLock.AcquireShared;
begin
  if UseSRWFallback then
    EnterCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    CallAcquireSRWLockShared(FHandle);
end;

procedure TSRWLock.ReleaseExclusive;
begin
  if UseSRWFallback then
    LeaveCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    CallReleaseSRWLockExclusive(FHandle);
end;

procedure TSRWLock.ReleaseShared;
begin
  if UseSRWFallback then
    LeaveCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    CallReleaseSRWLockShared(FHandle);
end;

function TSRWLock.TryAcquireExclusive: Boolean;
begin
  if UseSRWFallback then
    Result := TryEnterCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    Result := CallTryAcquireSRWLockExclusive(FHandle);
end;

function TSRWLock.TryAcquireShared: Boolean;
begin
  if UseSRWFallback then
    Result := TryEnterCriticalSection(PRTLCriticalSection(FHandle)^)
  else
    Result := CallTryAcquireSRWLockShared(FHandle);
end;

{ TSpinLockArray }

function TSpinLockArray.Acquire(const Index: Cardinal): Boolean;
begin
  Result := FLockArray[Index].Acquire;
end;

procedure TSpinLockArray.Finalize;
begin
  SetLength(FLockArray, 0);
end;

procedure TSpinLockArray.Initialize(const Size: Cardinal);
begin
  SetLength(FLockArray, Size);
end;

function TSpinLockArray.Release(const Index: Cardinal): Boolean;
begin
  Result := FLockArray[Index].Release;
end;

{ TSpinLock }

function TSpinLock.Acquire: Boolean;
begin
  repeat
    Result := CASV32(FLock, 1, 0) = 0;
  until Result;
end;

function TSpinLock.Release: Boolean;
begin
  Result := CASV32(FLock, 0, 1) = 1;
end;

{ TNamedLock }

function TNamedLock.Acquire(const Name: string): THandle;
var
  LastError: Cardinal;
  WaitResult: Cardinal;
begin
  repeat
    Result := CreateEvent(nil, False, False, PChar(Name));
    LastError := GetLastError;

    if (LastError = ERROR_ALREADY_EXISTS) then
    begin
      WaitResult := WaitForSingleObject(Result, INFINITE);

      if WaitResult <> WAIT_OBJECT_0 then
      begin
        CloseHandle(Result);
        raise Exception.CreateFmt('Failed to wait for event "%s". Error code %d', [Name, WaitResult]);
      end;
    end;
  until (Result <> INVALID_HANDLE_VALUE) and (LastError <> ERROR_ALREADY_EXISTS);
end;

procedure TNamedLock.Release(const Handle: THandle);
begin
  CloseHandle(Handle);
end;

initialization
  KernelHandle := GetModuleHandle(kernel32);

  @CallInitializeSRWLock := GetProcAddress(KernelHandle, 'InitializeSRWLock');
  @CallAcquireSRWLockShared := GetProcAddress(KernelHandle, 'AcquireSRWLockShared');
  @CallReleaseSRWLockShared := GetProcAddress(KernelHandle, 'ReleaseSRWLockShared');
  @CallAcquireSRWLockExclusive := GetProcAddress(KernelHandle, 'AcquireSRWLockExclusive');
  @CallReleaseSRWLockExclusive := GetProcAddress(KernelHandle, 'ReleaseSRWLockExclusive');
  @CallTryAcquireSRWLockExclusive := GetProcAddress(KernelHandle, 'TryAcquireSRWLockExclusive');
  @CallTryAcquireSRWLockShared := GetProcAddress(KernelHandle, 'TryAcquireSRWLockShared');

  UseSRWFallback := (not Assigned(CallInitializeSRWLock)) or
                    (not Assigned(CallAcquireSRWLockShared)) or
                    (not Assigned(CallReleaseSRWLockShared)) or
                    (not Assigned(CallAcquireSRWLockExclusive)) or
                    (not Assigned(CallReleaseSRWLockExclusive)) or
                    (not Assigned(CallTryAcquireSRWLockExclusive)) or
                    (not Assigned(CallTryAcquireSRWLockShared));

finalization
  FreeLibrary(KernelHandle);

end.
