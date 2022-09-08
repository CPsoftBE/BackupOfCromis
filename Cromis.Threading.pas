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
  * Threading support. Thread Pool and threading support routines and classes
  * =============================================================================
  * 26/11/2010 (1.0.0)
  *   - Initial implementation of a Thread Pool.
  * 27/11/2010 (1.1.0)
  *   - Use direct notification calls from threads instead of messages
  *   - Changed names of some procedures
  * 28/11/2010 (1.2.0)
  *   - Change approach to task based. No need to define your own threads anymore
  *   - Use TStreamStorage as versatile data carrier between threads
  * 30/11/2010 (1.3.0)
  *   - Renamed ThreadPool to TaskPool
  *   - Added ITask as the main pool object
  *   - Implemented ITaskValues / ITaskValue as data carrier for more flexibility
  * 03/12/2010 (1.3.1)
  *   - Added DynamicSize property. Pool downsizes if larger than initial size
  *   - Only MinPoolSize is left (no MaxPoolSize). Can be set before initialize
  * 06/12/2010 (1.3.2)
  *   - Added Task Queue class
  * 07/12/2010 (1.3.3)
  *   - Simplified Task Queue and made it faster
  * 15/12/2010 (1.3.4)
  *   - Added "SendMessageAsync" as means of sending messages from tasks
  * 12/03/2010 (1.3.5)
  *   - WaitFor is only available in Delphi 2005 and up
  *   - Added AsBoolean and AsInterface types for ITaskValue
  *   - Added ITaskValues.Exists
  * 05/09/2010 (1.3.6)
  *   - Added WaitFor so tasks can be waited upon
  *   - Added Terminated flag for task
  * 18/10/2010 (1.3.7)
  *   - Added ShutdownTimeout (INFINITE by default)
  *   - Wait for all tasks to finish then shutting down the task pool
  * 28/12/2010 (1.4.0)
  *   - Added TLockFreeStack based on Windows SLISTS
  *   - Added TThreadSafeQueue based on linked lists
  * 27/05/2011 (1.4.1)
  *   - TThreadSafeQueue.AcquireNewItem does not need the lock
  * 09/06/2011 (1.4.2)
  *   - TTaskQueue is not only available as ITaskQueue interface
  * 01/07/2011 (1.4.3)
  *   - Added StopAllTasks for TTaskPool
  * 18/03/2012 (1.5.0)
  *   - 64 bit compiler compatible
  * 18/02/2013 (1.6.0)
  *   - Use new TAnyValue for internal data carrier
  *   - Rewriten TLockFreeStack
  *   - Task pool no uses TLockFreeStack for free tasks list
  * 11/12/2013 (1.7.0)
  *   - Added TSRWLock based on Windows APIs with fallback to critical section
  * 17/12/2013 (1.7.1)
  *   - Bind SLIST Windows APIs at runtime and raise exception if not available
  *   - TLockFreeStack and TThreadSafeQueue have new optional parameter UseDeleted
  * 06/01/2014 (1.7.2)
  *   - SpinLock and SpinLockArray added
  * 22/02/2014 (1.8.0)
  *   - Redesigned threading units by content and added generics implementations
  * 05/03/2014 (1.8.1)
  *   - Use thread safe AllocateHWnd / DeallocateHWnd
  * 02/06/2014 (1.9.0)
  *   - overloaded AcquireTask to allow for specific OnMessage handler per task
  * 04/06/2014 (1.9.1)
  *   - Added ExecInMainThread and ExecInMainThreadAndWait
  * 04/06/2014 (1.9.2)
  *   - Fixed a possible bug in TTaskPool Finalize
  * =============================================================================
*)
Unit Cromis.Threading;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Contnrs, Variants,

  // DSiWin32
  DSiWin32,
  // cromis units
  Cromis.Threading.Sync, Cromis.ValueList,
{$IF CompilerVersion >= 20}
  Cromis.Threading.TS.Generics
{$ELSE}
  Cromis.Threading.TS,
  Cromis.AnyValue
{$IFEND};

Const
  cDefaultTimeout = 5000;
  cDefaultSize = 5000;
  cCSSpinCount = 4000;

Type
  PTaskQueueItem = ^TTaskQueueItem;

  TTaskQueueItem = Record
    Next: PTaskQueueItem;
    Event: THandle;
{$IF CompilerVersion >= 17}
    Procedure WaitFor;
{$IFEND}
  End;

  PTaskHeadItem = ^TTaskHeadItem;

  TTaskHeadItem = Record
    Last: PTaskQueueItem;
    First: PTaskQueueItem;
    Count: Integer;
  End;

  ITaskQueue = Interface(IInterface)
    ['{D6DFBA09-968D-4634-BA1D-79AFD4A029BB}']
    Function EnqueueTask: PTaskQueueItem;
    Procedure DequeueTask;
    Function GetQueueSize: Integer;
  End;

  // task values interfaces
  ITaskValues = IValueList;
  ITaskValue = IAnyValue;
  ITask = Interface;

  ITaskMessage = Interface(IInterface)
    ['{7E356929-C4BF-4352-84FD-472F87D50C3C}']
    Function GetName: String;
    Function GetValues: ITaskValues;
    Property Name: String Read GetName;
    Property Values: ITaskValues Read GetValues;
  End;

  TOnTaskMessage = Procedure(Const Msg: ITaskMessage) Of Object;
  TOnTaskEvent = Procedure(Const ATask: ITask) Of Object;

  ITask = Interface(IInterface)
    ['{3215E86F-ABC0-428C-9E5D-520BE08B0B50}']
    Function GetName: String;
    Function GetValues: ITaskValues;
    Function GetMessage: ITaskValues;
    Function GetTerminated: Boolean;
    Function GetTaskMethod: TOnTaskEvent;
    Function GetOnTaskMessage: TOnTaskMessage;
    Procedure SetName(Const Value: String);
    Procedure SetTerminated(Const Value: Boolean);
    Procedure SetTaskMethod(Const Value: TOnTaskEvent);
    Procedure SetOnTaskMessage(Const Value: TOnTaskMessage);
    Property OnTaskMessage: TOnTaskMessage Read GetOnTaskMessage Write SetOnTaskMessage;
    Property TaskMethod: TOnTaskEvent Read GetTaskMethod Write SetTaskMethod;
    Property Terminated: Boolean Read GetTerminated Write SetTerminated;
    Property Name: String Read GetName Write SetName;
    Property Message: ITaskValues Read GetMessage;
    Property Values: ITaskValues Read GetValues;
    // procedures of the ITask interface
    Procedure SendMessageSync(Const Timeout: Integer = cDefaultTimeout);
{$IF CompilerVersion >= 20}
    Procedure ExecInMainThreadAndWait(Const AThreadProc: TThreadProcedure);
    Procedure ExecInMainThread(Const AThreadProc: TThreadProcedure);
{$IFEND}
    Procedure WaitFor(Const Timeout: Cardinal = INFINITE);
    Procedure SendMessageAsync;
    Procedure SignalWaitFor;
    Procedure Terminate;
    Procedure Release;
    Procedure Run;
  End;

  IBaseTaskList = Interface(IInterface)
    ['{942D599D-D651-452D-821A-37C1A11720E0}']
    Procedure Clear;
    Function GetCount: Int64;
    Function AcquireFirstTask: ITask;
    Procedure AddTask(Const ATask: ITask);
    Property Count: Int64 Read GetCount;
  End;

  IFreeTaskList = Interface(IBaseTaskList)
    ['{5A40BA9E-66F5-45CB-86A5-2C32C88053A7}']
  End;

  IAllTaskList = Interface(IBaseTaskList)
    ['{460E7CF3-9467-4F4B-85C6-4D5EDDDA268E}']
    Procedure RemoveTask(Const ATask: ITask);
  End;

  TTaskPool = Class
  Strict Private
    FRunning: Boolean;
    FWndHandle: HWND;
    FDynamicSize: Boolean;
    FMinPoolSize: Integer;
    FAllTaskList: IAllTaskList;
    FFreeTaskList: IFreeTaskList;
    FOnTaskMessage: TOnTaskMessage;
    FShutdownTimeout: Cardinal;
    Function GetPoolSize: Integer;
    Function GetFreeTasks: Integer;
    Function AcquireFreeTask: ITask;
    Procedure WatchWndProc(Var Msg: TMessage);
    Procedure OnTaskComplete(Const ATask: ITask);
  Public
    Destructor Destroy; Override;
    Constructor Create(Const MinPoolSize: Integer);
    Function AcquireTask(Const TaskMethod: TOnTaskEvent; Const Name: String; Const OnMessage: TOnTaskMessage): ITask; Overload;
    Function AcquireTask(Const TaskMethod: TOnTaskEvent; Const Name: String): ITask; Overload;
    Procedure Initialize;
    Procedure Finalize;
    // properties of the thread pool
    Property OnTaskMessage: TOnTaskMessage Read FOnTaskMessage Write FOnTaskMessage;
    Property ShutdownTimeout: Cardinal Read FShutdownTimeout Write FShutdownTimeout;
    Property DynamicSize: Boolean Read FDynamicSize Write FDynamicSize;
    Property MinPoolSize: Integer Read FMinPoolSize Write FMinPoolSize;
    Property FreeTasks: Integer Read GetFreeTasks;
    Property PoolSize: Integer Read GetPoolSize;
    Property Running: Boolean Read FRunning;
  End;

  // acuire the task valus function
Function AcquireTaskValues: ITaskValues;
Function AcquireTaskQueue: ITaskQueue;

Implementation

Const
  WM_TASK_MESSAGE = WM_USER + 100;

Type
  PMethod = ^TMethod;

  TTaskMessage = Class(TInterfacedObject, ITaskMessage)
  Private
    FName: String;
    FValues: ITaskValues;
    Function GetName: String;
    Function GetValues: ITaskValues;
  Public
    Constructor Create(Const Name: String);
    Property Name: String Read GetName;
    Property Values: ITaskValues Read GetValues;
  End;

  TWorkerThread = Class(TThread)
  Private
    FOwner: Pointer;
    FEvents: Array [0 .. 1] Of THandle;
    FOnTaskComplete: TOnTaskEvent;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(Const Owner: ITask; Const OnTaskComplete: TOnTaskEvent);
    Destructor Destroy; Override;
    Procedure SignalAbort;
    Procedure SignalRun;
  End;

  TMessageObj = Class
  Private
    FMessage: ITaskMessage;
  Public
    Constructor Create(Const Msg: ITaskMessage);
    Property Msg: ITaskMessage Read FMessage;
  End;

  TFreeTaskList = Class(TInterfacedObject, IFreeTaskList)
  Private
{$IFNDEF Threading_NoLockFreeStack}
{$IF CompilerVersion >= 20}
    FInternalList: TLockFreeStack<ITask>;
{$ELSE}
    FInternalList: TLockFreeStack;
{$IFEND}
{$ELSE}
    FInternalList: TInterfaceList;
    FCriticalSec: TRTLCriticalSection;
{$ENDIF}
    Function GetCount: Int64;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function AcquireFirstTask: ITask;
    Procedure Clear;
    Procedure AddTask(Const ATask: ITask);
    Property Count: Int64 Read GetCount;
  End;

  TAllTaskList = Class(TInterfacedObject, IAllTaskList)
  Private
    FListLock: TSRWLock;
    FInternalList: TInterfaceList;
    Function GetCount: Int64;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function AcquireFirstTask: ITask;
    Procedure Clear;
    Procedure AddTask(Const ATask: ITask);
    Procedure RemoveTask(Const ATask: ITask);
    Property Count: Int64 Read GetCount;
  End;

  TTask = Class(TInterfacedObject, ITask)
  Private
    FName: String;
    FHWND: HWND;
    FValues: ITaskValues;
    FTerminated: Boolean;
    FTaskMethod: TOnTaskEvent;
    FOnTaskMessage: TOnTaskMessage;
    FThreadHandle: THandle;
    FWaitForEvent: THandle;
    FWorkerThread: TWorkerThread;
    FCurrentMessage: TMessageObj;
    Function GetName: String;
    Function GetValues: ITaskValues;
    Function GetMessage: ITaskValues;
    Function GetTerminated: Boolean;
    Function GetTaskMethod: TOnTaskEvent;
    Function GetOnTaskMessage: TOnTaskMessage;
    Procedure SetName(Const Value: String);
    Procedure SetTerminated(Const Value: Boolean);
    Procedure SetTaskMethod(Const Value: TOnTaskEvent);
    Procedure SetOnTaskMessage(Const Value: TOnTaskMessage);
  Public
    Destructor Destroy; Override;
    Constructor Create(Const OnTaskComplete: TOnTaskEvent; Const WinHandle: HWND);
    Property OnTaskMessage: TOnTaskMessage Read GetOnTaskMessage Write SetOnTaskMessage;
    Property TaskMethod: TOnTaskEvent Read GetTaskMethod Write SetTaskMethod;
    Property Terminated: Boolean Read GetTerminated Write SetTerminated;
    Property Name: String Read GetName Write SetName;
    Property Message: ITaskValues Read GetMessage;
    Property Values: ITaskValues Read GetValues;
    // procedures of the ITask interface
    Procedure SendMessageSync(Const Timeout: Integer = cDefaultTimeout);
{$IF CompilerVersion >= 20}
    Procedure ExecInMainThreadAndWait(Const AThreadProc: TThreadProcedure);
    Procedure ExecInMainThread(Const AThreadProc: TThreadProcedure);
{$IFEND}
    Procedure WaitFor(Const Timeout: Cardinal = INFINITE);
    Procedure SendMessageAsync;
    Procedure SignalWaitFor;
    Procedure Terminate;
    Procedure Release;
    Procedure Run;
  End;

  TTaskQueue = Class(TInterfacedObject, ITaskQueue)
  Private
    FHead: PTaskHeadItem;
    FDeleted: PTaskHeadItem;
    FCriticalSec: TRTLCriticalSection;
    Function AcquireNewItem: PTaskQueueItem;
    Procedure ClearData(Const RootItem: PTaskHeadItem);
    Procedure DeleteUnusedItem(Const Item: PTaskQueueItem);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function EnqueueTask: PTaskQueueItem;
    Procedure DequeueTask;
    Function GetQueueSize: Integer;
  End;

Function AcquireTaskValues: ITaskValues;
Begin
  Result := AcquireValueList;
End;

Function AcquireTaskQueue: ITaskQueue;
Begin
  Result := TTaskQueue.Create;
End;

{ TTaskPool }

Constructor TTaskPool.Create(Const MinPoolSize: Integer);
Begin
  FWndHandle := DSiAllocateHWnd(WatchWndProc);
  FFreeTaskList := TFreeTaskList.Create;
  FAllTaskList := TAllTaskList.Create;

  // the shutdown timeout
  FShutdownTimeout := INFINITE;

  // minimum size of the pool
  FMinPoolSize := MinPoolSize;
End;

Destructor TTaskPool.Destroy;
Begin
  If FAllTaskList.Count > 0 Then
    Finalize;

  // dealocate window handle
  DSiDeallocateHWnd(FWndHandle);

  Inherited;
End;

Procedure TTaskPool.Finalize;
Var
  ATask: ITask;
Begin
  // not running
  FRunning := False;
  Repeat
    ATask := FAllTaskList.AcquireFirstTask;

    If ATask <> Nil Then
    Begin
      ATask.Release;
      ATask.WaitFor(INFINITE);
    End;

  Until ATask = Nil;

  FAllTaskList.Clear;
  FFreeTaskList.Clear;
End;

Function TTaskPool.GetFreeTasks: Integer;
Begin
  Result := FFreeTaskList.Count;
End;

Function TTaskPool.GetPoolSize: Integer;
Begin
  Result := FAllTaskList.Count;
End;

Procedure TTaskPool.Initialize;
Var
  I: Integer;
  Task: ITask;
Begin
  For I := 0 To FMinPoolSize - 1 Do
  Begin
    Task := TTask.Create(OnTaskComplete, FWndHandle);
    FFreeTaskList.AddTask(Task);
    FAllTaskList.AddTask(Task);
  End;

  // set the flag
  FRunning := True;
End;

Procedure TTaskPool.OnTaskComplete(Const ATask: ITask);
Begin
  If FDynamicSize And (FAllTaskList.Count > FMinPoolSize) Then
  Begin
    FAllTaskList.RemoveTask(ATask);
    Exit;
  End;

  If FRunning Then
  Begin
    FFreeTaskList.AddTask(ATask);
    ATask.SignalWaitFor;
  End;
End;

Procedure TTaskPool.WatchWndProc(Var Msg: TMessage);
Var
  MessageObj: TMessageObj;
  TaskMessageProc: TOnTaskMessage;
Begin
  If Msg.Msg = WM_TASK_MESSAGE Then
  Begin
    MessageObj := TMessageObj(Pointer(Msg.WParam));
    Try
      If Msg.LParam <> 0 Then
      Begin
        TaskMessageProc := TOnTaskMessage(PMethod(Msg.LParam)^);
        TaskMessageProc(MessageObj.Msg);
      End;
    Finally
      MessageObj.Free;
    End;
  End
  Else
    Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
End;

Function TTaskPool.AcquireTask(Const TaskMethod: TOnTaskEvent; Const Name: String; Const OnMessage: TOnTaskMessage): ITask;
Begin
  Result := AcquireTask(TaskMethod, Name);
  Result.OnTaskMessage := OnMessage;
End;

Function TTaskPool.AcquireTask(Const TaskMethod: TOnTaskEvent; Const Name: String): ITask;
Begin
  If Not FRunning Then
    Raise Exception.Create('Cannot AcquireTask, the pool is not running!');

  // set task data
  Result := AcquireFreeTask;
  Result.OnTaskMessage := FOnTaskMessage;
  Result.TaskMethod := TaskMethod;
  Result.Name := Name;
  Result.Values.Clear;
End;

Function TTaskPool.AcquireFreeTask: ITask;
Begin
  Result := FFreeTaskList.AcquireFirstTask;

  If Result = Nil Then
  Begin
    Result := TTask.Create(OnTaskComplete, FWndHandle);
    FAllTaskList.AddTask(Result);
  End;
End;

{ TWorkerThread }

Constructor TWorkerThread.Create(Const Owner: ITask; Const OnTaskComplete: TOnTaskEvent);
Begin
  FreeOnTerminate := True;

  // create the event to put thread to sleep
  FEvents[0] := CreateEvent(Nil, False, False, Nil);
  FEvents[1] := CreateEvent(Nil, False, False, Nil);
  // set the pool window handle
  FOnTaskComplete := OnTaskComplete;
  // set the owner task with weak ref.
  FOwner := Pointer(Owner);

  Inherited Create(False);
End;

Destructor TWorkerThread.Destroy;
Begin
  DSiCloseHandleAndNull(FEvents[1]);
  DSiCloseHandleAndNull(FEvents[0]);

  Inherited;
End;

Procedure TWorkerThread.Execute;
Var
  WaitResult: Cardinal;
Begin
  Inherited;

  // wait until the thread is actually called
  WaitResult := WaitForMultipleObjects(Length(FEvents), @FEvents, False, INFINITE);

  // main thread loop
  While Not(WaitResult = WAIT_OBJECT_0) And Not Terminated Do
  Begin
    ITask(FOwner).Terminated := False;
    // execute the task method
    ITask(FOwner).TaskMethod(ITask(FOwner));

    // signal we are done
    FOnTaskComplete(ITask(FOwner));
    // wait for the next client call
    WaitResult := WaitForMultipleObjects(Length(FEvents), @FEvents, False, INFINITE);
  End;
End;

Procedure TWorkerThread.SignalAbort;
Begin
  SetEvent(FEvents[0]);
End;

Procedure TWorkerThread.SignalRun;
Begin
  SetEvent(FEvents[1]);
End;

{ TAllTaskList }

Procedure TAllTaskList.Clear;
Begin
  FListLock.AcquireExclusive;
  Try
    FInternalList.Clear;
  Finally
    FListLock.ReleaseExclusive;
  End;
End;

Constructor TAllTaskList.Create;
Begin
  FInternalList := TInterfaceList.Create;
  FListLock.Initialize;
End;

Destructor TAllTaskList.Destroy;
Begin
  FreeAndNil(FInternalList);

  Inherited;
End;

Procedure TAllTaskList.AddTask(Const ATask: ITask);
Begin
  FListLock.AcquireExclusive;
  Try
    FInternalList.Add(ATask);
  Finally
    FListLock.ReleaseExclusive;
  End;
End;

Function TAllTaskList.GetCount: Int64;
Begin
  FListLock.AcquireShared;
  Try
    Result := FInternalList.Count;
  Finally
    FListLock.ReleaseShared;
  End;
End;

Procedure TAllTaskList.RemoveTask(Const ATask: ITask);
Begin
  FListLock.AcquireExclusive;
  Try
    FInternalList.Remove(ATask);
  Finally
    FListLock.ReleaseExclusive;
  End;
End;

Function TAllTaskList.AcquireFirstTask: ITask;
Begin
  Result := Nil;

  FListLock.AcquireExclusive;
  Try
    If FInternalList.Count > 0 Then
    Begin
      Result := ITask(FInternalList.First);
      FInternalList.Remove(FInternalList.First);
    End;
  Finally
    FListLock.ReleaseExclusive;
  End;
End;

{ TFreeTaskList }

Function TFreeTaskList.AcquireFirstTask: ITask;
{$IFNDEF Threading_NoLockFreeStack}
{$IF CompilerVersion < 20}
Var
  Value: TAnyValue;
{$IFEND}
{$ENDIF}
Begin
{$IFNDEF Threading_NoLockFreeStack}
{$IF CompilerVersion >= 20}
  FInternalList.Pop(Result);
{$ELSE}
  FInternalList.Pop(Value);
  Result := ITask(Value.AsInterface);
{$IFEND};
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  Try
    Result := Nil;

    If FInternalList.Count > 0 Then
    Begin
      Result := ITask(FInternalList.First);
      FInternalList.Remove(Result);
    End;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
{$ENDIF}
End;

Procedure TFreeTaskList.AddTask(Const ATask: ITask);
Begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Push(ATask);
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  Try
    FInternalList.Add(ATask);
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
{$ENDIF}
End;

Procedure TFreeTaskList.Clear;
Begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Clear;
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  Try
    FInternalList.Clear;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
{$ENDIF}
End;

Constructor TFreeTaskList.Create;
Const
  cInitialSize = 500;
Begin
{$IFNDEF Threading_NoLockFreeStack}
{$IF CompilerVersion >= 20}
  FInternalList := TLockFreeStack<ITask>.Create(cInitialSize);
{$ELSE}
  FInternalList := TLockFreeStack.Create(cInitialSize);
{$IFEND};
{$ELSE}
  InitializeCriticalSectionAndSpinCount(FCriticalSec, cCSSpinCount);
  FInternalList := TInterfaceList.Create;
{$ENDIF}
End;

Destructor TFreeTaskList.Destroy;
Begin
  FreeAndNil(FInternalList);
{$IFDEF Threading_NoLockFreeStack}
  DeleteCriticalSection(FCriticalSec);
{$ENDIF}
  Inherited;
End;

Function TFreeTaskList.GetCount: Int64;
Begin
{$IFNDEF Threading_NoLockFreeStack}
  Result := FInternalList.Count;
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  Try
    Result := FInternalList.Count;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
{$ENDIF}
End;

{ TTask }

Constructor TTask.Create(Const OnTaskComplete: TOnTaskEvent; Const WinHandle: HWND);
Begin
  FWorkerThread := TWorkerThread.Create(Self, OnTaskComplete);
  FWaitForEvent := CreateEvent(Nil, False, False, Nil);
  FThreadHandle := FWorkerThread.Handle;
  FValues := AcquireTaskValues;
  FHWND := WinHandle;
End;

Destructor TTask.Destroy;
Begin
  // BugFix CPsoft.be
  If Not FWorkerThread.Terminated Then
    FWorkerThread.Terminate;
  FWorkerThread.SignalAbort;
  // LeakFix CPsoft.be
  DSiCloseHandleAndNull(FWaitForEvent);

  Inherited;
End;

Function TTask.GetValues: ITaskValues;
Begin
  Result := FValues;
End;

Function TTask.GetMessage: ITaskValues;
Begin
  If FCurrentMessage = Nil Then
    FCurrentMessage := TMessageObj.Create(TTaskMessage.Create(FName));

  // return current message
  Result := FCurrentMessage.Msg.Values;
End;

Function TTask.GetName: String;
Begin
  Result := FName;
End;

Function TTask.GetOnTaskMessage: TOnTaskMessage;
Begin
  Result := FOnTaskMessage;
End;

Function TTask.GetTaskMethod: TOnTaskEvent;
Begin
  Result := FTaskMethod;
End;

Function TTask.GetTerminated: Boolean;
Begin
  Result := FTerminated;
End;

Procedure TTask.Run;
Begin
  FWorkerThread.SignalRun;
End;

Procedure TTask.SendMessageAsync;
Var
  AResult: Boolean;
  TryCount: Integer;
Begin
  AResult := False;
  TryCount := 0;

  While Not AResult And (TryCount < 3) Do
  Begin
    AResult := PostMessage(FHWND, WM_TASK_MESSAGE, WParam(Pointer(FCurrentMessage)), LParam(Addr(TMethod(FOnTaskMessage))));
    Inc(TryCount);
  End;

  // check result
  Case AResult Of
    False:
      FreeAndNil(FCurrentMessage);
    True:
      FCurrentMessage := Nil;
  End;
End;

Procedure TTask.SendMessageSync(Const Timeout: Integer);
Var
  AResult: LRESULT;
  TryCount: Integer;
  Response: {$IF CompilerVersion >= 23}PDWORD_PTR{$ELSE}Cardinal{$IFEND};
Begin
{$IF CompilerVersion >= 23}Response := Nil{$ELSE}Response := 0{$IFEND};
  TryCount := 0;
  AResult := 1;

  While (AResult <> 0) And (TryCount < 3) Do
  Begin
    AResult := SendMessageTimeout(FHWND, WM_TASK_MESSAGE, WParam(Pointer(FCurrentMessage)), LParam(Addr(TMethod(FOnTaskMessage))), SMTO_BLOCK, Timeout,
      Response);
    Inc(TryCount);
  End;

  Case (AResult <> 0) Of
    False:
      FreeAndNil(FCurrentMessage);
    True:
      FCurrentMessage := Nil;
  End;
End;

Procedure TTask.SetName(Const Value: String);
Begin
  FName := Value;
End;

Procedure TTask.SetOnTaskMessage(Const Value: TOnTaskMessage);
Begin
  FOnTaskMessage := Value;
End;

Procedure TTask.SetTaskMethod(Const Value: TOnTaskEvent);
Begin
  FTaskMethod := Value;
End;

Procedure TTask.SetTerminated(Const Value: Boolean);
Begin
  FTerminated := Value;
End;

Procedure TTask.Release;
Begin
  If Not FWorkerThread.Terminated Then
    FWorkerThread.Terminate;
  FWorkerThread.SignalAbort;
End;

Procedure TTask.SignalWaitFor;
Begin
  SetEvent(FWaitForEvent);
End;

Procedure TTask.Terminate;
Begin
  FTerminated := True;
End;

Procedure TTask.WaitFor(Const Timeout: Cardinal);
Begin
  WaitForSingleObject(FThreadHandle, Timeout);
End;

{$IF CompilerVersion >= 20}

Procedure TTask.ExecInMainThreadAndWait(Const AThreadProc: TThreadProcedure);
Begin
  FWorkerThread.Synchronize(AThreadProc);
End;

Procedure TTask.ExecInMainThread(Const AThreadProc: TThreadProcedure);
Begin
  FWorkerThread.Queue(AThreadProc);
End;
{$IFEND}
{ TTaskQueue }

Constructor TTaskQueue.Create;
Begin
  InitializeCriticalSectionAndSpinCount(FCriticalSec, cCSSpinCount);

  // create nodes
  New(FDeleted);
  New(FHead);

  // set pointer to last
  FDeleted.First := Nil;
  FDeleted.Last := Nil;
  FDeleted.Count := 0;

  // set pointer to last
  FHead.First := Nil;
  FHead.Last := Nil;
  FHead.Count := 0;
End;

Destructor TTaskQueue.Destroy;
Begin
  ClearData(FDeleted);
  ClearData(FHead);
  Dispose(FDeleted);
  Dispose(FHead);

  // delete the critical section last
  DeleteCriticalSection(FCriticalSec);

  Inherited;
End;

Procedure TTaskQueue.ClearData(Const RootItem: PTaskHeadItem);
Var
  Item: PTaskQueueItem;
  OldItem: PTaskQueueItem;
Begin
  EnterCriticalSection(FCriticalSec);
  Try
    Item := RootItem.Last;

    While Item <> Nil Do
    Begin
      OldItem := Item;
      Item := Item.Next;

      DSiCloseHandleAndNull(OldItem.Event);
      Dispose(OldItem);
    End;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
End;

Function TTaskQueue.AcquireNewItem: PTaskQueueItem;
Begin
  If FDeleted.Last <> Nil Then
  Begin
    Result := FDeleted.Last;
    FDeleted.Last := FDeleted.Last.Next;
    FDeleted.Count := FDeleted.Count - 1;

    If FDeleted.Last = Nil Then
      FDeleted.First := Nil;
  End
  Else
  Begin
    New(Result);
    Result.Event := CreateEvent(Nil, False, False, Nil);
  End;

  // next is always nil
  Result.Next := Nil;
End;

Procedure TTaskQueue.DeleteUnusedItem(Const Item: PTaskQueueItem);
Begin
  // rewire the first element
  Case FDeleted.First <> Nil Of
    True:
      FDeleted.First.Next := Item;
    False:
      FDeleted.Last := Item;
  End;

  // add the item at head
  FDeleted.Count := FDeleted.Count + 1;
  FDeleted.First := Item;
  Item.Next := Nil;
End;

Function TTaskQueue.EnqueueTask: PTaskQueueItem;
Begin
  // acquire new item
  Result := AcquireNewItem;

  EnterCriticalSection(FCriticalSec);
  Try
    // rewire the list
    FHead.Count := FHead.Count + 1;

    // rewire the first element
    If FHead.First <> Nil Then
      FHead.First.Next := Result;

    // Head.First points to new element
    FHead.First := Result;

    If FHead.Last = Nil Then
    Begin
      SetEvent(Result.Event);
      FHead.Last := Result;
    End;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
End;

Procedure TTaskQueue.DequeueTask;
Var
  CurrItem: PTaskQueueItem;
  NextItem: PTaskQueueItem;
Begin
  EnterCriticalSection(FCriticalSec);
  Try
    CurrItem := FHead.Last;
    Try
      FHead.Count := FHead.Count - 1;
      NextItem := CurrItem.Next;
      FHead.Last := NextItem;

      If NextItem <> Nil Then
        // resume the oldest thread
        SetEvent(NextItem.Event)
      Else
        FHead.First := Nil;
    Finally
      DeleteUnusedItem(CurrItem);
    End;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
End;

Function TTaskQueue.GetQueueSize: Integer;
Begin
  EnterCriticalSection(FCriticalSec);
  Try
    If FHead.First <> Nil Then
      Result := FHead.Count
    Else
      Result := 0;
  Finally
    LeaveCriticalSection(FCriticalSec);
  End;
End;

{ TQueueItem }

{$IF CompilerVersion >= 17}

Procedure TTaskQueueItem.WaitFor;
Begin
  WaitForSingleObject(Event, INFINITE);
End;
{$IFEND}
{ TMessageObj }

Constructor TMessageObj.Create(Const Msg: ITaskMessage);
Begin
  FMessage := Msg;
End;

{ TTaskMessage }

Constructor TTaskMessage.Create(Const Name: String);
Begin
  FName := Name;
  FValues := AcquireTaskValues;
End;

Function TTaskMessage.GetName: String;
Begin
  Result := FName;
End;

Function TTaskMessage.GetValues: ITaskValues;
Begin
  Result := FValues;
End;

End.
