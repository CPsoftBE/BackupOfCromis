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
 *
 * =============================================================================
 * The unit uses a hashing function originaly written by
 * Paul Hsieh (http://www.azillionmonkeys.com/qed/hash.html)
 * The code was translated by Davy Landman.
 *
 * "Hash_SuperFastHash" is used under MPL 1.1
 * =============================================================================
 *
 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is SuperFastHash Delphi and BASM translation.
 *
 * The Initial Developer of the Original Code is
 * Davy Landman.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****
 *
 * =============================================================================
 * Hashing function and helper functions
 * For changelog reffer to Cromis.Hashing
 * =============================================================================
*)
unit Cromis.Hashing.Common;

interface

const
  cDefaultHashSize = 6151;

const
  // customized version from http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
  CGoodHashTablePrimes: array [3..30] of Cardinal =
    (
     17, 31, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613,
     393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319,
     201326611, 402653189, 805306457, 1610612741
    );

function GetGoodHashSize(const Size: Cardinal): Cardinal; inline;
function CalculateMaxItemsBeforeResize(const CurrentSize: Cardinal): Cardinal; inline;

// hash functions
function Hash_FNV1aHash(Data: Pointer; const Length: Cardinal): Cardinal; inline;
function Hash_SimpleHash(Data: Pointer; const Length: Cardinal): Cardinal; inline;
function Hash_MurmurHash2(Data: Pointer; const Length: Cardinal): Cardinal; inline;
function Hash_MurmurHash3(Data: Pointer; const Length: Cardinal): Cardinal; inline;
function Hash_SuperFastHash(Data: Pointer; const Length: Cardinal): Cardinal; inline;
function Hash_SimpleCardinalMod(Data: Pointer; const Length: Cardinal): Cardinal; inline;

implementation

(* =============================================================================*)
(* Get good hash size from predefined table of sizes                            *)
(* Calculate maximum limit of elements before a resize happens                  *)
(* =============================================================================*)

function GetGoodHashSize(const Size: Cardinal): Cardinal; inline;
var
  UpToSize: Cardinal;
  TableIndex: Integer;
begin
  TableIndex := Low(CGoodHashTablePrimes);
  UpToSize := 1 shl TableIndex;

  while Size > UpToSize do
  begin
    Inc(TableIndex);
    UpToSize := UpToSize shl 1;
  end;

  // set the good hash size
  Result := CGoodHashTablePrimes[TableIndex];
end;

function CalculateMaxItemsBeforeResize(const CurrentSize: Cardinal): Cardinal; inline;
begin
  Result := Trunc((CurrentSize / 3) * 2);
end;

(* =============================================================================*)
(* Various hashing functions implementation. All have standard parameters       *)
(*                                                                              *)
(* =============================================================================*)

function Hash_SimpleCardinalMod(Data: Pointer; const Length: Cardinal): Cardinal;
begin
  Result := Cardinal(Data^) mod Length;
end;

function Hash_SimpleHash(Data: Pointer; const Length: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := 0;

  for I := 1 to Length do
  begin
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor PByte(Data)^;
    Inc(PByte(Data));
  end;
end;

function Hash_FNV1aHash(Data: Pointer; const Length: Cardinal): Cardinal; inline;
var
  I: Integer;
const
  FNV_offset_basis = 2166136261;
  FNV_prime = 16777619;
begin
  //FNV-1a hash
  Result := FNV_offset_basis;

  for I := 1 to Length do
  begin
    Result := (Result xor PByte(Data)^) * FNV_prime;
    Inc(PByte(Data));
  end;
end;

function Hash_MurmurHash2(Data: Pointer; const Length: Cardinal): Cardinal; inline;
var
  k: Cardinal;
  len: Cardinal;
  hash: Cardinal;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
begin
  len := Length;

  // The default seed, $9747b28c, is from the original C library
  // Initialize the hash to a 'random' value
  hash := $9747b28c xor len;

  while(len >= 4) do
  begin
    k := PLongWord(Data)^;

    k := k * m;
    k := k xor (k shr r);
    k := k * m;

    hash := hash * m;
    hash := hash xor k;

    Inc(PLongWord(Data));
    len := len - 4;
  end;

  {   Handle the last few bytes of the input array
          S: ... $69 $18 $2f
  }
  Assert(len <= 3);
  if len = 3 then
  begin
    Inc(PByte(Data), 2);
    hash := hash xor (PLongWord(Data)^ shl 16);
  end;
  if len >= 2 then
  begin
    Inc(PByte(Data));
    hash := hash xor (PLongWord(Data)^ shl 8);
  end;
  if len >= 1 then
  begin
    hash := hash xor (PLongWord(Data)^);
    hash := hash * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  hash := hash xor (hash shr 13);
  hash := hash * m;
  hash := hash xor (hash shr 15);

  Result := hash;
end;

function Hash_MurmurHash3(Data: Pointer; const Length: Cardinal): Cardinal;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
var
  I: Integer;
  h1: Cardinal;
  k1: Cardinal;
begin
  h1 := $9747b28c;
  i := Length div SizeOf(Cardinal);

  while I <> 0 do
  begin
    k1 := PCardinal(Data)^ * c1;
    k1 := ((k1 shl 15) or (k1 shr 17)) * c2;
    h1 := h1 xor k1;
    h1 := (h1 shl 13) or (h1 shr 19);
    h1 := ((h1 shl 2) + h1) + $E6546B64;
    Inc(PByte(Data), SizeOf(Cardinal));
    Dec(I);
  end;

  // tail
  if (Length and 1) <> 0 then
  begin
    if (Length and 2) <> 0 then
      k1 := (((Cardinal(PByte(PByte(Data)^ + 2)^) shl 16) xor PWord(Data)^) * c1) // 3 bytes
    else
      k1 := Cardinal(PByte(Data)^) * c1; // 1 bytes
    h1 := h1 xor (((k1 shl 16) or (k1 shr 16)) * c2);
  end
  else if (Length and 2) <> 0 then
  begin
    k1 := Cardinal(PWord(Data)^) * c1; // 2 bytes
    h1 := h1 xor (((k1 shl 16) or (k1 shr 16)) * c2);
  end;

  // finalization mix - force all bits of hash block to avalanche within 0.25% bias
  h1 := h1 xor Length;
  h1 := (h1 xor (h1 shr 16)) * $85EBCA6B;
  h1 := (h1 xor (h1 shr 13)) * $C2B2AE35;
  Result := h1 xor (h1 shr 16);
end;

{$OVERFLOWCHECKS OFF}
function Hash_SuperFastHash(Data: Pointer; const Length: Cardinal): Cardinal;
var
  TempPart: Cardinal;
  RemainingBytes: Integer;
  RemainingDWords: Integer;
begin
  if not Assigned(Data) or (Length <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Result := Length;
  RemainingBytes := Length and 3; // mod 4
  RemainingDWords := Length shr 2; // div 4

  // main loop
  while RemainingDWords > 0 do
  begin
    Result := Result + PWord(Data)^;
    // splitting the pointer math keeps the amount of registers pushed at 2
    Data := Pointer(Cardinal(Data) + SizeOf(Word));
    TempPart := (PWord(Data)^ shl 11) xor Result;
    Result := (Result shl 16) xor TempPart;
    Data  := Pointer(Cardinal(Data) + SizeOf(Word));
    Result := Result + (Result shr 11);
    Dec(RemainingDWords);
  end;
  // Handle end cases
  if RemainingBytes = 3 then
  begin
    Result := Result + PWord(Data)^;
    Result := Result xor (Result shl 16);
    Data  := Pointer(Cardinal(Data) + SizeOf(Word)); // skip to the last byte
    Result := Result xor ((PByte(Data)^ shl 18));
    Result := Result + (Result shr 11);
  end
  else if RemainingBytes = 2 then
  begin
    Result := Result +  PWord(Data)^;
    Result := Result xor (Result shl 11);
    Result := Result + (Result shr 17);
  end
  else if RemainingBytes = 1 then
  begin
    Result := Result + PByte(Data)^;
    Result := Result xor (Result shl 10);
    Result := Result + (Result shr 1);
  end;
  // Force "avalanching" of final 127 bits
  Result := Result xor (Result shl 3);
  Result := Result +   (Result shr 5);
  Result := Result xor (Result shl 4);
  Result := Result +   (Result shr 17);
  Result := Result xor (Result shl 25);
  Result := Result +   (Result shr 6);
end;
{$OVERFLOWCHECKS ON}

end.
