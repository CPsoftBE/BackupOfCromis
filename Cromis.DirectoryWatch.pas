(*
  * This software is distributed under BSD license.
  *
  * Copyright (c) 2009 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
  * NOTICE OF CODE ORIGIN
  *
  * This code was derived from the original code of author "Gleb Yourchenko"
  * The original code "FnugryDirWatch" can still be found at Torry Components
  * The URL is: http://www.torry.net/pages.php?id=252
  *
  * The code was taken as a starting point and then mainly written from scratch
  * keeping some of the healthy code parts. So I am not in any way an author of
  * the original idea. But I am the author of all the changes and new code parts.
  *
  * ============================================================================
  * 12/10/2009 (1.0.0)
  *  - Initial code rewrite from "FnugryDirWatch"
  * 16/01/2010 (1.0.1)
  *  - Refactored the main watch loop
  * 18/03/2011 (1.1.0)
  *  - 64 bit compiler compatible
  *  - Fixed thread termination bug
  * 12/06/2012 (1.2.0)
  *  - WaitForFileReady added. Waits until file is free to be used
  * 11/07/2012 (1.2.1)
  *  - Close the FChangeEvent handle
  * 21/07/2012 (1.3.0)
  *  - Added buffer size property, so that the user can control the size of buffer
  *  - Added OnError event handler, so user get notified of all errors
  *  - Check the result of ReadDirectoryChangesW with GetOverlappedResult.
  *    This way we can check if buffer overflow happens and trigger an error.
  * 15/03/2014 (1.4.0)
  *  - Added property SignalType
  *  - Implemented additional signaling method that run in the context of a worker
  *    thread. This way we can use the DirectoryWatch inside a secondary thread
  *    with no message loop.
  *  - Use unicode strings for all notifications
  * 15/05/2014 (1.4.1)
  *  - WaitForFileReady changed to function to indicate success or failure
  * 10/03/2023
  *  - fixed Delphi 11.x compatibility according to
  *    https://stackoverflow.com/questions/76693497/tested-code-in-seattle-is-giving-an-access-violation-in-alexandria and
  *    https://www.delphipraxis.net/60995-readdirectorychangesw-wird-mehrfach-ausgeloest-warum-2.html
  * ============================================================================
*)

Unit Cromis.DirectoryWatch;

Interface

Uses
  Windows, SysUtils, Classes, Messages, SyncObjs, DateUtils,
  // DSiWin32
  DSiWin32,
  // Cromis units
  Cromis.Unicode, Cromis.Threading.TS.Generics;

Const
  FILE_NOTIFY_CHANGE_FILE_NAME = $00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME = $00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES = $00000004;
  FILE_NOTIFY_CHANGE_SIZE = $00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE = $00000010;
  FILE_NOTIFY_CHANGE_LAST_ACCESS = $00000020;
  FILE_NOTIFY_CHANGE_CREATION = $00000040;
  FILE_NOTIFY_CHANGE_SECURITY = $00000100;

Const
  cShutdownTimeout = 3000;
  cFileWaitTimeout = 0;
  cErrorTypeNone = 0;
  cErrorTypeStop = 1;

Type
  // the filters that control when the watch is triggered
  TWatchOption = (woFileName, woDirName, woAttributes, woSize, woLastWrite, woLastAccess, woCreation, woSecurity);
  TWatchOptions = Set Of TWatchOption;

  // the actions that are the result of the watch being triggered
  TWatchAction = (waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew);
  TWatchActions = Set Of TWatchAction;

  TFileChangeNotifyEvent = Procedure(Const Sender: TObject; Const Action: TWatchAction; Const FileName: ustring) Of Object;
  TOnError = Procedure(Const Sender: TObject; Const ErrorCode: Integer; Const ErrorMessage: ustring) Of Object;

  TDirectoryWatch = Class
  Private
    FWatchOptions: TWatchOptions;
    FWatchActions: TWatchActions;
    FWatchSubTree: Boolean;
    FWatchThread: TThread;
    FBufferSize: Integer;
    FWndHandle: HWND;
    FDirectory: String;
    FOnError: TOnError;
    FOnChange: TNotifyEvent;
    FOnNotify: TFileChangeNotifyEvent;
    Procedure WatchWndProc(Var Msg: TMessage);
    Procedure SetDirectory(Const Value: String);
    Procedure SetWatchOptions(Const Value: TWatchOptions);
    Procedure SetWatchActions(Const Value: TWatchActions);
    Procedure SetWatchSubTree(Const Value: Boolean);
    Function MakeFilter: Integer;
  Protected
    Procedure Change; Virtual;
    Procedure AllocWatchThread;
    Procedure ReleaseWatchThread;
    Procedure RestartWatchThread;
    Procedure Notify(Const Action: Integer; Const FileName: ustring); Virtual;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Start;
    Procedure Stop;
    Function Running: Boolean;
    Property WatchSubTree: Boolean Read FWatchSubTree Write SetWatchSubTree;
    Property WatchOptions: TWatchOptions Read FWatchOptions Write SetWatchOptions;
    Property WatchActions: TWatchActions Read FWatchActions Write SetWatchActions;
    Property BufferSize: Integer Read FBufferSize Write FBufferSize;
    Property Directory: String Read FDirectory Write SetDirectory;
    // notification properties. Notify about internal and exernal changes
    Property OnNotify: TFileChangeNotifyEvent Read FOnNotify Write FOnNotify;
    Property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
    Property OnError: TOnError Read FOnError Write FOnError;
  End;

  // waits for the file to be ready (it is not in use anymore) or timeout occurs
Function WaitForFileReady(Const FileName: String; Const Timeout: Cardinal = cFileWaitTimeout): Boolean;

Implementation

Type
  PFILE_NOTIFY_INFORMATION = ^TFILE_NOTIFY_INFORMATION;

  TFILE_NOTIFY_INFORMATION = Record
    NextEntryOffset: Cardinal;
    Action: Cardinal;
    FileNameLength: Cardinal;
    FileName: Array [0 .. MAX_PATH - 1] Of WideChar;
  End;

Const
  WM_DIRWATCH_ERROR = WM_USER + 137;
  WM_DIRWATCH_NOTIFY = WM_USER + 138;

  FILE_LIST_DIRECTORY = $0001;

Const
  // error messages
  cErrorInWatchThread = 'Error "%s" in watch thread. Error code: %d';
  cErrorCreateWatchError = 'Error trying to create file handle for "%s". Error code: %d';

Type
  PNotifyRecord = ^TNotifyRecord;

  TNotifyRecord = Record
    Code: Integer;
    AMsg: puchar;
  End;

  TDirWatchThread = Class(TThread)
  Private
    FWatchSubTree: Boolean;
    FChangeEvent: THandle;
    FAbortEvent: THandle;
    FBufferSize: Integer;
    FWndHandle: HWND;
    FDirHandle: THandle;
    FDirectory: String;
    FIOResult: Pointer;
    FFilter: Integer;
    Procedure SignalError(Const ErrorMessage: ustring; ErrorCode: Cardinal = 0; ErrorType: LParam = 0);
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(Const Directory: String; Const WndHandle: HWND; Const BufferSize: Integer; Const TypeFilter: Cardinal; Const aWatchSubTree: Boolean);
    Destructor Destroy; Override;
    Procedure SignalAbort;
  End;

Function WaitForFileReady(Const FileName: String; Const Timeout: Cardinal): Boolean;
Var
  hFile: THandle;
  StartTime: TDateTime;
Begin
  StartTime := Now;
  Result := False;

  // wait to close
  While (MilliSecondsBetween(Now, StartTime) < Timeout) Or (Timeout = 0) Do
  Begin
    hFile := CreateFile(PChar(FileName), GENERIC_READ, 0, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    If hFile <> INVALID_HANDLE_VALUE Then
    Begin
      CloseHandle(hFile);
      Result := True;
      Break;
    End;

    // wait for file
    Sleep(50);
  End;
End;

Procedure TDirWatchThread.Execute;
Var
  NotifyData: PFILE_NOTIFY_INFORMATION;
  Events: Array [0 .. 1] Of THandle;
  NotifyRecord: PNotifyRecord;
  ErrorCode: Cardinal;
  ErrorMessage: String;
  WaitResult: DWORD; // DWORD is a 4-byte unsigned integer, type DWORD = Cardinal
  NextEntry: Cardinal;
  Overlap: TOverlapped;
  ResSize: Cardinal;
Begin
  FillChar(Overlap, SizeOf(TOverlapped), 0);
  Overlap.hEvent := FChangeEvent;

  // set the array of events
  Events[0] := FChangeEvent;
  Events[1] := FAbortEvent;

  While Not Terminated Do
    Try
      If ReadDirectoryChangesW(FDirHandle, FIOResult, FBufferSize, FWatchSubTree, FFilter, @ResSize, @Overlap, Nil) Then
      Begin
        WaitResult := WaitForMultipleObjects(Length(Events), @Events, False, INFINITE);

        // check if we have terminated the thread
        If WaitResult <> WAIT_OBJECT_0 Then
        Begin
          Terminate;
          Exit;
        End;

        If WaitResult = WAIT_OBJECT_0 Then
        Begin
          If GetOverlappedResult(FDirHandle, Overlap, ResSize, False) Then
          Begin
            NotifyData := FIOResult;

            // check overflow
            If ResSize = 0 Then
            Begin
              ErrorCode := ERROR_NOTIFY_ENUM_DIR;
              ErrorMessage := SysErrorMessage(ERROR_NOTIFY_ENUM_DIR);
              SignalError(ErrorMessage, ErrorCode, cErrorTypeNone);
            End;

            Repeat
              New(NotifyRecord);
              NotifyRecord.Code := NotifyData^.Action;
              // get memory for filename and fill it with data
              GetMem(NotifyRecord.AMsg, NotifyData^.FileNameLength + SizeOf(WideChar));
              Move(NotifyData^.FileName, Pointer(NotifyRecord.AMsg)^, NotifyData^.FileNameLength);
              PWord(NativeUint(NotifyRecord.AMsg) + NotifyData^.FileNameLength)^ := 0;
              // send the message about the filename information
              PostMessage(FWndHandle, WM_DIRWATCH_NOTIFY, WParam(NotifyRecord), 0);
              // advance to the next entry in the current buffer
              NextEntry := NotifyData^.NextEntryOffset;
              If NextEntry = 0 Then
                Break
              Else
                PByte(NotifyData) := PByte(Ulong_ptr(NotifyData) + NextEntry);
            Until Terminated;
          End
          Else
          Begin
            ErrorCode := GetLastError;
            ErrorMessage := SysErrorMessage(ErrorCode);
            SignalError('GetOverlappedResult: ' + ErrorMessage, ErrorCode, cErrorTypeNone);
          End;
        End;
      End
      Else
      Begin
        ErrorCode := GetLastError;
        ErrorMessage := SysErrorMessage(ErrorCode);
        SignalError('ReadDirectoryChanges: ' + ErrorMessage, ErrorCode, cErrorTypeStop);
      End;
    Except
      On E: Exception Do
      Begin
        ErrorCode := GetLastError;
        ErrorMessage := E.Message;
        SignalError('Execute Exception: ' + ErrorMessage, ErrorCode, cErrorTypeStop);
      End;
    End;
End;

(* Example Code form Madshi
  procedure Proc2;
  var pStrVar : TPString;
  begin
  // as you know already, "GetMem" does not initialize the memory
  GetMem(pStrVar, sizeOf(string));

  // so if you're doing it yourself, you're out of danger
  ZeroMemory(pStrVar, sizeOf(string));

  // "pStrVar^" is initialized this time
  // -> Delphi knows that "pStrVar^" holds no string currently
  // -> Delphi assigns the new value to "pStrVar^" successfully
  pStrVar^ := 'test';

  // FreeMem frees the allocated memory, but the "test" string does NOT get freed
  // this is at least a memory leak, sometimes it can even result in a crash
  FreeMem(pStrVar);
  end;
*)

Procedure TDirWatchThread.SignalAbort;
Begin
  SetEvent(FAbortEvent);
End;

Procedure TDirWatchThread.SignalError(Const ErrorMessage: ustring; ErrorCode: Cardinal; ErrorType: LParam);
Var
  MessageSize: Integer;
  NotifyRecord: PNotifyRecord;
Begin
  New(NotifyRecord);

  If ErrorCode = 0 Then
    ErrorCode := GetLastError;

  // calculate the size of the error message buffer
  MessageSize := Length(ErrorMessage) * SizeOf(Char) + SizeOf(WideChar);

  NotifyRecord.Code := ErrorCode;
  GetMem(NotifyRecord.AMsg, MessageSize);
  StrPCopy(NotifyRecord.AMsg, ErrorMessage);

  PostMessage(FWndHandle, WM_DIRWATCH_ERROR, WParam(NotifyRecord), LParam(ErrorType));
End;

Constructor TDirWatchThread.Create(Const Directory: String; Const WndHandle: HWND; Const BufferSize: Integer; Const TypeFilter: Cardinal;
  Const aWatchSubTree: Boolean);
Begin
  //
  // Retrieve proc pointer, open directory to
  // watch and allocate buffer for notification data.
  // (note, it is done before calling inherited
  // create (that calls BeginThread) so any exception
  // will be still raised in caller's thread)
  //
  FDirHandle := CreateFile(PChar(Directory), FILE_LIST_DIRECTORY, FILE_SHARE_READ OR FILE_SHARE_DELETE OR FILE_SHARE_WRITE, Nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS OR FILE_FLAG_OVERLAPPED, 0);

  If FDirHandle = INVALID_HANDLE_VALUE Then
    Raise Exception.CreateFmt(cErrorCreateWatchError, [Directory, GetLastError]);

  FChangeEvent := CreateEvent(Nil, False, False, Nil);
  FAbortEvent := CreateEvent(Nil, False, False, Nil);

  // allocate the buffer memory
  FBufferSize := BufferSize * 1024; // SizeOf(TFILE_NOTIFY_INFORMATION); // CPsoft 2021.3.6.0
  GetMem(FIOResult, FBufferSize);

  FWatchSubTree := aWatchSubTree;
  FWndHandle := WndHandle;
  FDirectory := Directory;
  FFilter := TypeFilter;

  Inherited Create(False);
End;

Destructor TDirWatchThread.Destroy;
Begin
  DSiCloseHandleAndNull(FChangeEvent);
  DSiCloseHandleAndNull(FAbortEvent);

  If FDirHandle <> INVALID_HANDLE_VALUE Then
    CloseHandle(FDirHandle);
  If Assigned(FIOResult) Then
    FreeMem(FIOResult);

  Inherited Destroy;
End;

{ TFnugryDirWatch }

Procedure TDirectoryWatch.AllocWatchThread;
Begin
  If FWatchThread = Nil Then
  Begin
    FWndHandle := DsiAllocateHWnd(WatchWndProc);

    FWatchThread := TDirWatchThread.Create(Directory, FWndHandle, FBufferSize, MakeFilter, WatchSubTree);
  End;
End;

Procedure TDirectoryWatch.ReleaseWatchThread;
Var
  AResult: Cardinal;
  ThreadHandle: THandle;
Begin
  If FWatchThread <> Nil Then
  Begin
    ThreadHandle := FWatchThread.Handle;

    TDirWatchThread(FWatchThread).SignalAbort;

    // wait and block until thread is finished
    AResult := WaitForSingleObject(ThreadHandle, cShutdownTimeout);

    // check if we timed out
    If AResult = WAIT_TIMEOUT Then
      TerminateThread(ThreadHandle, 0);

    // free the watch thread
    FreeAndNil(FWatchThread);

    DSiDeallocateHWnd(FWndHandle);
    FWndHandle := 0;
  End;
End;

Procedure TDirectoryWatch.RestartWatchThread;
Begin
  Stop;
  Start;
End;

Function TDirectoryWatch.Running: Boolean;
Begin
  Result := FWatchThread <> Nil;
End;

Destructor TDirectoryWatch.Destroy;
Begin
  Stop;

  Inherited Destroy;
End;

Constructor TDirectoryWatch.Create;
Begin
  FWatchSubTree := True;
  FBufferSize := 64; // 32 - CPsoft 2021.3.6.0

  // construct the default watch actions and options
  FWatchActions := [waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew];
  FWatchOptions := [woFileName, woDirName, woAttributes, woSize, woLastWrite, woLastAccess, woCreation, woSecurity];
End;

Procedure TDirectoryWatch.SetWatchActions(Const Value: TWatchActions);
Begin
  If FWatchActions <> Value Then
  Begin
    FWatchActions := Value;

    If Running Then
      RestartWatchThread;

    Change;
  End;
End;

Procedure TDirectoryWatch.SetWatchOptions(Const Value: TWatchOptions);
Begin
  If FWatchOptions <> Value Then
  Begin
    FWatchOptions := Value;

    If Running Then
      RestartWatchThread;

    Change;
  End;
End;

Procedure TDirectoryWatch.WatchWndProc(Var Msg: TMessage);
Var
  NotifyRecord: PNotifyRecord;
Begin
  Case Msg.Msg Of
    WM_DIRWATCH_NOTIFY:
      //
      // Retrieve notify data and forward
      // the event to TDirectoryWatch's notify
      // handler. Free filename string (allocated
      // in WatchThread's notify handler.)
      //
      Begin
        NotifyRecord := PNotifyRecord(Msg.WParam);
        Try
          Notify(NotifyRecord.Code, NotifyRecord.AMsg);
          // LeakFix CPsoft.be
          FreeMem(NotifyRecord.AMsg);
        Finally
          Dispose(NotifyRecord);
        End;
      End;

    WM_DIRWATCH_ERROR:
      //
      // Disable dir watch and re-raise
      // exception on error
      //
      Begin
        NotifyRecord := PNotifyRecord(Msg.WParam);
        Try
          If Assigned(FOnError) Then
            FOnError(Self, NotifyRecord.Code, NotifyRecord.AMsg);
          // LeakFix CPsoft.be
          FreeMem(NotifyRecord.AMsg);
        Finally
          Dispose(NotifyRecord);
        End;
        // Restart Watch Thread
        If Msg.LParam = cErrorTypeStop Then
          Stop;
      End;
    //
    // pass all other messages down the line
    //
  Else
    Begin
      Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
      Exit;
    End;
  End;
End;

Function TDirectoryWatch.MakeFilter: Integer;
Const
  FilterFlags: Array [TWatchOption] Of Integer = (FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME, FILE_NOTIFY_CHANGE_ATTRIBUTES,
    FILE_NOTIFY_CHANGE_SIZE, FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_LAST_ACCESS, FILE_NOTIFY_CHANGE_CREATION, FILE_NOTIFY_CHANGE_SECURITY);
Var
  Flag: TWatchOption;
Begin
  Result := 0;

  For Flag In FWatchOptions Do
    Result := Result Or FilterFlags[Flag];
End;

Procedure TDirectoryWatch.SetWatchSubTree(Const Value: Boolean);
Begin
  If Value <> FWatchSubTree Then
  Begin
    FWatchSubTree := Value;

    If Running Then
      RestartWatchThread;

    Change;
  End;
End;

Procedure TDirectoryWatch.Start;
Begin
  If FDirectory = '' Then
    Exit;

  If Not Running Then
  Begin
    AllocWatchThread;
    Change;
  End;
End;

Procedure TDirectoryWatch.Stop;
Begin
  If Running Then
  Begin
    ReleaseWatchThread;
    Change;
  End;
End;

Procedure TDirectoryWatch.SetDirectory(Const Value: String);
Begin
  If StrIComp(PChar(Trim(Value)), PChar(FDirectory)) <> 0 Then
  Begin
    FDirectory := Trim(Value);

    If Running Then
      RestartWatchThread;

    Change;
  End;
End;

Procedure TDirectoryWatch.Change;
Begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
End;

Procedure TDirectoryWatch.Notify(Const Action: Integer; Const FileName: ustring);
Begin
  If Assigned(FOnNotify) Then
    If TWatchAction(Action - 1) In FWatchActions Then
      FOnNotify(Self, TWatchAction(Action - 1), FileName);
End;

End.
