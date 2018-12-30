{ $Cromis: lib/Cromis.Streams.pas,v 1.3 2008/04/13 17:09:35 ikacin Exp $ }
(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Application wide exception handling that can catch all thrown exceptions
 * =============================================================================
 * 25/10/2009 (1.1.0)
 *   - rewritten the code to allow to only catch unhandled exceptions
 * 06/01/2011 (1.2.0)
 *   - added OnException event to allow the application to react on unhandled exceptions
 * =============================================================================
*)
unit Cromis.Exceptions;

interface

uses
  Windows, SysUtils, Classes,

  JclDebug, JclHookExcept, AppEvnts, Forms,

  // cromis units
  Cromis.SimpleLog;

type
  TExceptionHandler = class
  private
    FSimpleLogID: string;
    FAppEvents: TApplicationEvents;
    FOnException: TExceptionEvent;
    FUnhandledExceptionsOnly: Boolean;
    procedure SetUnhandledExceptionsOnly(const Value: Boolean);
    procedure InternalLogException(Sender: TObject; E: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    property SimpleLogID: string read FSimpleLogID write FSimpleLogID;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property UnhandledExceptionsOnly: Boolean read FUnhandledExceptionsOnly write SetUnhandledExceptionsOnly;
  end;

  // function that takes exception message and logs it with stack trace
  procedure LogExceptionWithStackTrace(const ExceptionMsg: string);

var
  ExceptionHandler: TExceptionHandler;

implementation

procedure LogExceptionWithStackTrace(const ExceptionMsg: string);
var
  StrList: TStringList;
begin
  if ExceptionHandler.SimpleLogID <> '' then
  begin
    StrList := TStringList.Create;
    try
      StrList.Add(Format('{ Original Exception - %s }', [ExceptionMsg]));
      JclLastExceptStackListToStrings(StrList, False, True, True, False);
      StrList.Add('{ _______End of the exception stack trace block_______ }');
      StrList.Add(' ');

      // save the error with stack log to file
      SimpleLog.LogEvent(ExceptionHandler.SimpleLogID, StrList.Text);
    finally
      StrList.Free;
    end;
  end;
end;

procedure HookGlobalException(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  LogExceptionWithStackTrace(Exception(ExceptObj).Message);
end;

{ TExceptionHandler }

constructor TExceptionHandler.Create;
begin
  FAppEvents := TApplicationEvents.Create(nil);
  UnhandledExceptionsOnly := False;
end;

destructor TExceptionHandler.Destroy;
begin
  FreeAndNil(FAppEvents);

  inherited;
end;

procedure TExceptionHandler.InternalLogException(Sender: TObject; E: Exception);
begin
  LogExceptionWithStackTrace(E.Message);

  if Assigned(FOnException) then
    FOnException(Sender, E);
end;

procedure TExceptionHandler.SetUnhandledExceptionsOnly(const Value: Boolean);
begin
  FUnhandledExceptionsOnly := Value;

  if FUnhandledExceptionsOnly then
  begin
    JclRemoveExceptNotifier(HookGlobalException);
    FAppEvents.OnException := InternalLogException;
  end
  else
  begin
    JclAddExceptNotifier(HookGlobalException);
    FAppEvents.OnException := nil;
  end;
end;

initialization
  // set debug tracking options
  Include(JclStackTrackingOptions, stTraceAllExceptions);
  Include(JclStackTrackingOptions, stRawMode);

  // create the exception handler here
  ExceptionHandler := TExceptionHandler.Create;

  // Initialize Exception tracking
  JclStartExceptionTracking;
  JclHookExceptions;

finalization
  JclUnhookExceptions;
  JclStopExceptionTracking;
  FreeAndNil(ExceptionHandler);

end.
