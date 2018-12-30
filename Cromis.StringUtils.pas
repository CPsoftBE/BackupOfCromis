(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2006-2009 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * String utilities that are not found in delphi already. Support for
 * both ansi and widestring.
 * =============================================================================
 * 15/08/2006:
 *   - Initial implementation.
 * 10/01/2008
 *   - optimization (Copy - > Move)
 *   - changed string to AnsiString for future compatibility with Tiburion
 * 03/10/2008
 *   - D2009 compatible
 * 25/10/2009
 *   - D2010 compatible
 * 09/12/2010
 *   - Added TTextTokenizer
 * 30/1/2012
 *   - XE2 compatible  (hs)
 * 16/2/2012
 *   - TTextTokenizer made unicode compatible
 * 21/1/2013
 *   - Compiler defines adjusted to support XE3 and newer
 * =============================================================================
*)

unit Cromis.StringUtils;

interface

uses
  SysUtils, StrUtils, Classes,

  // unicode support
  Cromis.Unicode;

const
  NumChars = '1234567890+-';

// string searching functions (ansi and wide)
function PosBack(const SubStr, S: astring): Integer; overload;
function PosBack(const SubStr, S: ustring): Integer; overload;
function PosAfter(const SubStr, S: ustring; Offset: Integer = 1): Integer; overload;
function PosAfter(const SubStr, S: astring; Offset: Integer = 1): Integer; overload;

// string token counting funcions (ansi and wide)
function CountSubStrings(const SubStr, S: astring): Integer; overload;
function CountSubStrings(const SubStr, S: ustring): Integer; overload;

// string extracting functions (ansi and wide)
function StrAfter(const BegPattern, S: astring): astring; overload;
function StrAfter(const BegPattern, S: ustring): ustring; overload;
function StrBefore(const EndPattern, S: astring): astring; overload;
function StrBefore(const EndPattern, S: ustring): ustring; overload;
function StrBetween(const BegPattern, EndPattern, S: astring): astring; overload;
function StrBetween(const BegPattern, EndPattern, S: ustring): ustring; overload;

// string replace function (ansi and wide)
function AnsiReplaceChars(Chars: array of achar; NewChar: achar; S: astring): astring;
function WideReplaceChars(Chars: array of uchar; NewChar: uchar; S: ustring): ustring;

// string manipulation functions (ansi and wide)
function PadLeft(const S: astring; const MaxLen: Integer; const PadChar: achar = #32): astring; overload;
function PadLeft(const S: ustring; const MaxLen: Integer; const PadChar: uchar = #32): ustring; overload;
function PadRight(const S: astring; const MaxLen: Integer; const PadChar: achar = #32): astring; overload;
function PadRight(const S: ustring; const MaxLen: Integer; const PadChar: uchar = #32): ustring; overload;

// string test functions (ansi and wide)
function StrConsistsOfNumberChars(S: astring): Boolean; overload;
function StrConsistsOfNumberChars(S: ustring): Boolean; overload;
function StrIsNumber(S: astring): Boolean; overload;
function StrIsNumber(S: ustring): Boolean; overload;

type
  TTokens = array of ustring;

  TTextTokenizer = class
  private
    FTokens: TTokens;
    FDelimiters: array of ustring;
  public
    constructor Create;
    procedure Tokenize(const Text: ustring);
    procedure AddDelimiters(const Delimiters: array of ustring);
    property Tokens: TTokens read FTokens;
  end;

implementation


// -----------------------------------------------------------------------------------------
// string searching functions (ansi and wide)
// -----------------------------------------------------------------------------------------

function PosBack(const SubStr, S: astring): Integer;
var
  LastStr: astring;
  LastPos: Integer;
begin
  Result := Pos(SubStr, S);
  LastPos := Result;
  LastStr := S;

  while LastPos > 0 do
  begin
    LastStr := Copy(LastStr, LastPos + 1, Length(LastStr) - LastPos);
    LastPos := Pos(SubStr, LastStr);

    // set the result
    if LastPos > 0 then
      Result := Result + LastPos;
  end;
end;

function PosBack(const SubStr, S: ustring): Integer;
var
  LastStr: ustring;
  LastPos: Integer;
begin
  Result := Pos(SubStr, S);
  LastPos := Result;
  LastStr := S;

  while LastPos > 0 do
  begin
    LastStr := Copy(LastStr, LastPos + 1, Length(LastStr) - LastPos);
    LastPos := Pos(SubStr, LastStr);

    // set the result
    if LastPos > 0 then
      Result := Result + LastPos;
  end;
end;

function PosAfter(const SubStr, S: ustring; Offset: Integer): Integer;
var
  BegPos: Integer;
begin
  Result := 0;

  if Offset <= Length(S) then
  begin
    BegPos := Pos(SubStr, Copy(S, Offset, Length(S) - Offset + 1));

    if BegPos > 0 then
      Result := BegPos + Offset - 1;
  end;
end;


function PosAfter(const SubStr, S: astring; Offset: Integer): Integer;
var
  BegPos: Integer;
begin
  Result := 0;

  if Offset <= Length(S) then
  begin
    BegPos := Pos(SubStr, Copy(S, Offset, Length(S) - Offset + 1));

    if BegPos > 0 then
      Result := BegPos + Offset - 1;
  end;
end;

// -----------------------------------------------------------------------------------------
// string token counting funcions (ansi and wide)
// -----------------------------------------------------------------------------------------

function CountSubStrings(const SubStr, S: astring): Integer;
var
  PrevPos: Integer;
begin
  Result := 0;
  PrevPos := 0;
  repeat
    PrevPos := PosAfter(SubStr, S, PrevPos + 1);
    if PrevPos > 0 then
      Inc(Result);
  until (PrevPos = 0);
end;

function CountSubStrings(const SubStr, S: ustring): Integer;
var
  PrevPos: Integer;
begin
  Result := 0;
  PrevPos := 0;
  repeat
    PrevPos := PosAfter(SubStr, S, PrevPos + 1);
    if PrevPos > 0 then
      Inc(Result);
  until (PrevPos = 0);
end;

// -----------------------------------------------------------------------------------------
// string extracting functions (ansi and wide)
// -----------------------------------------------------------------------------------------

function StrBefore(const EndPattern, S: astring): astring; overload;
var
  EndPos: Integer;
begin
  Result := '';
  EndPos := Pos(EndPattern, S);

  if EndPos > 1 then
  begin
    SetLength(Result, EndPos - 1);
    Move(S[1], Result[1],  SizeOf(achar) * Length(Result));
  end;
end;

function StrBefore(const EndPattern, S: ustring): ustring; overload;
var
  EndPos: Integer;
begin
  Result := '';
  EndPos := Pos(EndPattern, S);

  if EndPos > 1 then
  begin
    SetLength(Result, EndPos - 1);
    Move(S[1], Result[1],  SizeOf(uchar) * Length(Result));
  end;
end;

function StrAfter(const BegPattern, S: astring): astring; overload;
var
  BegPos: Integer;
begin
  Result := '';
  BegPos := Pos(BegPattern, S) + Length(BegPattern);

  if (BegPos <= Length(S)) and (BegPos > Length(BegPattern)) then
  begin
    SetLength(Result, Length(S) - BegPos + 1);
    Move(S[BegPos], Result[1], SizeOf(achar) * Length(Result));
  end;
end;

function StrAfter(const BegPattern, S: ustring): ustring; overload;
var
  BegPos: Integer;
begin
  Result := '';
  BegPos := Pos(BegPattern, S) + Length(BegPattern);

  if (BegPos <= Length(S)) and (BegPos > Length(BegPattern)) then
  begin
    SetLength(Result, Length(S) - BegPos + 1);
    Move(S[BegPos], Result[1], SizeOf(uchar) * Length(Result));
  end;
end;

function StrBetween(const BegPattern, EndPattern, S: astring): astring; overload;
var
  BegPos: Integer;
  EndPos: Integer;
begin
  BegPos := PosAfter(BegPattern, S, 1) + Length(BegPattern);
  EndPos := PosAfter(EndPattern, S, BegPos + 1);
  Result := '';

  if (EndPos > BegPos) and (BegPos > Length(BegPattern)) then
  begin
    // move the string to the result
    SetLength(Result, EndPos - BegPos);
    Move(S[BegPos], Result[1], SizeOf(achar) * Length(Result));
  end;
end;

function StrBetween(const BegPattern, EndPattern, S: ustring): ustring; overload;
var
  BegPos: Integer;
  EndPos: Integer;
begin
  BegPos := PosAfter(BegPattern, S, 1) + Length(BegPattern);
  EndPos := PosAfter(EndPattern, S, BegPos + 1);
  Result := '';

  if (EndPos > BegPos) and (BegPos > Length(BegPattern)) then
  begin
    // move the string to the result
    SetLength(Result, EndPos - BegPos);
    Move(S[BegPos], Result[1], SizeOf(uchar) * Length(Result));
  end;
end;

// -----------------------------------------------------------------------------------------
// string test functions (ansi and wide)
// -----------------------------------------------------------------------------------------

function StrConsistsOfNumberChars(S: astring): Boolean; overload;
var
  I: Integer;
begin
  Result := True;

  for I := 1 to Length(S) do
  begin
    if (Pos(S[I], AnsiString(NumChars)) = 0) and
      not (S[I] = achar({$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrConsistsOfNumberChars(S: ustring): Boolean; overload;
var
  I: Integer;
begin
  Result := True;

  for I := 1 to Length(S) do
  begin
    if (Pos(S[I], NumChars) = 0) and
      not (S[I] = uchar({$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsNumber(S: astring): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 1 to Length(S) do
  begin
    if I = 1 then
    begin
      if not (Ord(S[I]) in [48..57]) and not (Ord(S[I]) in [43, 45]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
    begin
      if not (Ord(S[I]) in [48..57]) and
         not (S[I] = achar({$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function StrIsNumber(S: ustring): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 1 to Length(S) do
  begin
    if I = 1 then
    begin
      if not (Ord(S[I]) in [48..57]) and not (Ord(S[I]) in [43, 45]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else
    begin
      if not (Ord(S[I]) in [48..57]) and
         not (S[I] = uchar({$IF CompilerVersion >= 22}Formatsettings.{$IFEND}DecimalSeparator)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------------------
// string replace function (ansi and wide)
// -----------------------------------------------------------------------------------------

function AnsiReplaceChars(Chars: array of achar; NewChar: achar; S: astring): astring;
var
  I: Integer;

  function CharInArray(TestChar: achar): Boolean;
  var
    K: Integer;
  begin
    Result := False;

    for K := 0 to Length(Chars) - 1 do
    begin
      if TestChar = Chars[I] then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  for I := 1 to Length(S) do
    if CharInArray(S[I]) then
      S[I] := NewChar;
  Result := S;
end;

function WideReplaceChars(Chars: array of uchar; NewChar: uchar; S: ustring): ustring;
var
  I: Integer;

  function CharInArray(TestChar: uchar): Boolean;
  var
    K: Integer;
  begin
    Result := False;

    for K := 0 to Length(Chars) - 1 do
    begin
      if TestChar = Chars[I] then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  for I := 1 to Length(S) do
    if CharInArray(S[I]) then
      S[I] := NewChar;
  Result := S;
end;

// -----------------------------------------------------------------------------------------
// string manipulation functions (ansi and wide)
// -----------------------------------------------------------------------------------------

function PadLeft(const S: astring; const MaxLen: Integer; const PadChar: achar): astring;
begin
  Result := StringOfChar(PadChar, MaxLen - Length(S)) + S;
end;

function PadLeft(const S: ustring; const MaxLen: Integer; const PadChar: uchar): ustring;
begin
  Result := StringOfChar(PadChar, MaxLen - Length(S)) + S;
end;

function PadRight(const S: astring; const MaxLen: Integer; const PadChar: achar): astring;
begin
  Result := S + StringOfChar(PadChar, MaxLen - Length(S));
end;

function PadRight(const S: ustring; const MaxLen: Integer; const PadChar: uchar): ustring;
begin
  Result := S + StringOfChar(PadChar, MaxLen - Length(S));
end;

{ TTextTokenizer }

procedure TTextTokenizer.AddDelimiters(const Delimiters: array of ustring);
var
  I: Integer;
begin
  if Length(Delimiters) > 0 then
  begin
    SetLength(FDelimiters, Length(Delimiters));

    for I := 0 to Length(Delimiters) - 1 do
      FDelimiters[I] := Delimiters[I];
  end;
end;

constructor TTextTokenizer.Create;
begin
  SetLength(FTokens, 0);
  SetLength(FDelimiters, 0);
end;

procedure TTextTokenizer.Tokenize(const Text: ustring);
var
  I, K: Integer;
  Counter: Integer;
  NewToken: ustring;
  Position: Integer;
  CurrToken: ustring;
begin
  SetLength(FTokens, 100);
  CurrToken := '';
  Counter := 0;

  for I := 1 to Length(Text) do
  begin
    CurrToken := CurrToken + Text[I];

    for K := 0 to Length(FDelimiters) - 1 do
    begin
      Position := Pos(FDelimiters[K], CurrToken);

      if Position > 0 then
      begin
        NewToken := Copy(CurrToken, 1, Position - 1);

        if NewToken <> '' then
        begin
          if Counter > Length(FTokens) then
            SetLength(FTokens, Length(FTokens) * 2);

          FTokens[Counter] := Trim(NewToken);
          Inc(Counter)
        end;

        CurrToken := '';
      end;
    end;
  end;

  if CurrToken <> '' then
  begin
    if Counter > Length(FTokens) then
      SetLength(FTokens, Length(FTokens) * 2);

    FTokens[Counter] := Trim(CurrToken);
    Inc(Counter)
  end;

  SetLength(FTokens, Counter);
end;

end.
