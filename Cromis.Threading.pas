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
unit Cromis.Threading;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Variants,

  // cromis units
  Cromis.Threading.Sync, Cromis.ValueList, Cromis.Utils,
{$IF CompilerVersion >= 20}
  Cromis.Threading.TS.Generics
{$ELSE}
  Cromis.Threading.TS,
  Cromis.AnyValue
{$IFEND};

const
  cDefaultTimeout = 5000;
  cDefaultSize = 5000;
  cCSSpinCount = 4000;

type
  PTaskQueueItem = ^TTaskQueueItem;
  TTaskQueueItem = record
    Next: PTaskQueueItem;
    Event: THandle;
    {$IF CompilerVersion >= 17}
      procedure WaitFor;
    {$IFEND}
  end;

  PTaskHeadItem = ^TTaskHeadItem;
  TTaskHeadItem = record
    Last: PTaskQueueItem;
    First: PTaskQueueItem;
    Count: Integer;
  end;

  ITaskQueue = Interface(IInterface)
  ['{D6DFBA09-968D-4634-BA1D-79AFD4A029BB}']
    function EnqueueTask: PTaskQueueItem;
    procedure DequeueTask;
    function GetQueueSize: Integer;
  end;

  // task values interfaces
  ITaskValues = IValueList;
  ITaskValue = IAnyValue;
  ITask = Interface;

  ITaskMessage = Interface(IInterface)
  ['{7E356929-C4BF-4352-84FD-472F87D50C3C}']
    function GetName: string;
    function GetValues: ITaskValues;
    property Name: string read GetName;
    property Values: ITaskValues read GetValues;
  end;

  TOnTaskMessage = procedure(const Msg: ITaskMessage) of Object;
  TOnTaskEvent = procedure(const ATask: ITask) of Object;

  ITask = Interface(IInterface)
  ['{3215E86F-ABC0-428C-9E5D-520BE08B0B50}']
    function GetName: string;
    function GetValues: ITaskValues;
    function GetMessage: ITaskValues;
    function GetTerminated: Boolean;
    function GetTaskMethod: TOnTaskEvent;
    function GetOnTaskMessage: TOnTaskMessage;
    procedure SetName(const Value: string);
    procedure SetTerminated(const Value: Boolean);
    procedure SetTaskMethod(const Value: TOnTaskEvent);
    procedure SetOnTaskMessage(const Value: TOnTaskMessage);
    property OnTaskMessage: TOnTaskMessage read GetOnTaskMessage write SetOnTaskMessage;
    property TaskMethod: TOnTaskEvent read GetTaskMethod write SetTaskMethod;
    property Terminated: Boolean read GetTerminated write SetTerminated;
    property Name: string read GetName write SetName;
    property Message: ITaskValues read GetMessage;
    property Values: ITaskValues read GetValues;
    // procedures of the ITask interface
    procedure SendMessageSync(const Timeout: Integer = cDefaultTimeout);
  {$IF CompilerVersion >= 20}
    procedure ExecInMainThreadAndWait(const AThreadProc: TThreadProcedure);
    procedure ExecInMainThread(const AThreadProc: TThreadProcedure);
  {$IFEND}
    procedure WaitFor(const Timeout: Cardinal = INFINITE);
    procedure SendMessageAsync;
    procedure SignalWaitFor;
    procedure Terminate;
    procedure Release;
    procedure Run;
  end;

  IBaseTaskList = Interface(IInterface)
  ['{942D599D-D651-452D-821A-37C1A11720E0}']
    procedure Clear;
    function GetCount: Int64;
    function AcquireFirstTask: ITask;
    procedure AddTask(const ATask: ITask);
    property Count: Int64 read GetCount;
  end;

  IFreeTaskList = Interface(IBaseTaskList)
  ['{5A40BA9E-66F5-45CB-86A5-2C32C88053A7}']
  end;

  IAllTaskList = Interface(IBaseTaskList)
  ['{460E7CF3-9467-4F4B-85C6-4D5EDDDA268E}']
    procedure RemoveTask(const ATask: ITask);
  end;

  TTaskPool = class
  strict private
    FRunning: Boolean;
    FWndHandle: HWND;
    FDynamicSize: Boolean;
    FMinPoolSize: Integer;
    FAllTaskList: IAllTaskList;
    FFreeTaskList: IFreeTaskList;
    FOnTaskMessage: TOnTaskMessage;
    FShutdownTimeout: Cardinal;
    function GetPoolSize: Integer;
    function GetFreeTasks: Integer;
    function AcquireFreeTask: ITask;
    procedure WatchWndProc(var Msg: TMessage);
    procedure OnTaskComplete(const ATask: ITask);
  public
    destructor Destroy; override;
    constructor Create(const MinPoolSize: Integer);
    function AcquireTask(const TaskMethod: TOnTaskEvent; const Name: string; const OnMessage: TOnTaskMessage): ITask; overload;
    function AcquireTask(const TaskMethod: TOnTaskEvent; const Name: string): ITask; overload;
    procedure Initialize;
    procedure Finalize;
    // properties of the thread pool
    property OnTaskMessage: TOnTaskMessage read FOnTaskMessage write FOnTaskMessage;
    property ShutdownTimeout: Cardinal read FShutdownTimeout write FShutdownTimeout;
    property DynamicSize: Boolean read FDynamicSize write FDynamicSize;
    property MinPoolSize: Integer read FMinPoolSize write FMinPoolSize;
    property FreeTasks: Integer read GetFreeTasks;
    property PoolSize: Integer read GetPoolSize;
    property Running: Boolean read FRunning;
  end;

  // acuire the task valus function
  function AcquireTaskValues: ITaskValues;
  function AcquireTaskQueue: ITaskQueue;

implementation

const
  WM_TASK_MESSAGE = WM_USER + 100;

type
  PMethod = ^TMethod;

  TTaskMessage = class(TInterfacedObject, ITaskMessage)
  private
    FName: string;
    FValues: ITaskValues;
    function GetName: string;
    function GetValues: ITaskValues;
  public
    constructor Create(const Name: string);
    property Name: string read GetName;
    property Values: ITaskValues read GetValues;
  end;

  TWorkerThread = class(TThread)
  private
    FOwner: Pointer;
    FEvents: array [0..1] of THandle;
    FOnTaskComplete: TOnTaskEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const Owner: ITask; const OnTaskComplete: TOnTaskEvent);
    destructor Destroy; override;
    procedure SignalAbort;
    procedure SignalRun;
  end;

  TMessageObj = class
  private
    FMessage: ITaskMessage;
  public
    constructor Create(const Msg: ITaskMessage);
    property Msg: ITaskMessage read FMessage;
  end;

  TFreeTaskList = class(TInterfacedObject, IFreeTaskList)
  private
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
    function GetCount: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireFirstTask: ITask;
    procedure Clear;
    procedure AddTask(const ATask: ITask);
    property Count: Int64 read GetCount;
  end;

  TAllTaskList = class(TInterfacedObject, IAllTaskList)
  private
    FListLock: TSRWLock;
    FInternalList: TInterfaceList;
    function GetCount: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireFirstTask: ITask;
    procedure Clear;
    procedure AddTask(const ATask: ITask);
    procedure RemoveTask(const ATask: ITask);
    property Count: Int64 read GetCount;
  end;

  TTask = class(TInterfacedObject, ITask)
  private
    FName: string;
    FHWND: HWND;
    FValues: ITaskValues;
    FTerminated: Boolean;
    FTaskMethod: TOnTaskEvent;
    FOnTaskMessage: TOnTaskMessage;
    FThreadHandle: THandle;
    FWaitForEvent: THandle;
    FWorkerThread: TWorkerThread;
    FCurrentMessage: TMessageObj;
    function GetName: string;
    function GetValues: ITaskValues;
    function GetMessage: ITaskValues;
    function GetTerminated: Boolean;
    function GetTaskMethod: TOnTaskEvent;
    function GetOnTaskMessage: TOnTaskMessage;
    procedure SetName(const Value: string);
    procedure SetTerminated(const Value: Boolean);
    procedure SetTaskMethod(const Value: TOnTaskEvent);
    procedure SetOnTaskMessage(const Value: TOnTaskMessage);
  public
    destructor Destroy; override;
    constructor Create(const OnTaskComplete: TOnTaskEvent; const WinHandle: HWND);
    property OnTaskMessage: TOnTaskMessage read GetOnTaskMessage write SetOnTaskMessage;
    property TaskMethod: TOnTaskEvent read GetTaskMethod write SetTaskMethod;
    property Terminated: Boolean read GetTerminated write SetTerminated;
    property Name: string read GetName write SetName;
    property Message: ITaskValues read GetMessage;
    property Values: ITaskValues read GetValues;
    // procedures of the ITask interface
    procedure SendMessageSync(const Timeout: Integer = cDefaultTimeout);
  {$IF CompilerVersion >= 20}
    procedure ExecInMainThreadAndWait(const AThreadProc: TThreadProcedure);
    procedure ExecInMainThread(const AThreadProc: TThreadProcedure);
  {$IFEND}
    procedure WaitFor(const Timeout: Cardinal = INFINITE);
    procedure SendMessageAsync;
    procedure SignalWaitFor;
    procedure Terminate;
    procedure Release;
    procedure Run;
  end;

  TTaskQueue = class(TInterfacedObject, ITaskQueue)
  private
    FHead: PTaskHeadItem;
    FDeleted: PTaskHeadItem;
    FCriticalSec: TRTLCriticalSection;
    function AcquireNewItem: PTaskQueueItem;
    procedure ClearData(const RootItem: PTaskHeadItem);
    procedure DeleteUnusedItem(const Item: PTaskQueueItem);
  public
    constructor Create;
    destructor Destroy; override;
    function EnqueueTask: PTaskQueueItem;
    procedure DequeueTask;
    function GetQueueSize: Integer;
  end;

function AcquireTaskValues: ITaskValues;
begin
  Result := AcquireValueList;
end;

function AcquireTaskQueue: ITaskQueue;
begin
  Result := TTaskQueue.Create;
end;

{ TTaskPool }

constructor TTaskPool.Create(const MinPoolSize: Integer);
begin
  FWndHandle := TSAllocateHWnd(WatchWndProc);
  FFreeTaskList := TFreeTaskList.Create;
  FAllTaskList := TAllTaskList.Create;

  // the shutdown timeout
  FShutdownTimeout := INFINITE;

  // minimum size of the pool
  FMinPoolSize := MinPoolSize;
end;

destructor TTaskPool.Destroy;
begin
  if FAllTaskList.Count > 0 then
    Finalize;

  // dealocate window handle
  TSDeallocateHWnd(FWndHandle);

  inherited;
end;

procedure TTaskPool.Finalize;
var
  ATask: ITask;
begin
  // not running
  FRunning := False;

  repeat
    ATask := FAllTaskList.AcquireFirstTask;

    if ATask <> nil then
    begin
      ATask.Release;
      ATask.WaitFor(INFINITE);
    end;

  until ATask = nil;

  FAllTaskList.Clear;
  FFreeTaskList.Clear;
end;

function TTaskPool.GetFreeTasks: Integer;
begin
  Result := FFreeTaskList.Count;
end;

function TTaskPool.GetPoolSize: Integer;
begin
  Result := FAllTaskList.Count;
end;

procedure TTaskPool.Initialize;
var
  I: Integer;
  Task: ITask;
begin
  for I := 0 to FMinPoolSize - 1 do
  begin
    Task := TTask.Create(OnTaskComplete, FWndHandle);
    FFreeTaskList.AddTask(Task);
    FAllTaskList.AddTask(Task);
  end;

  // set the flag
  FRunning := True;
end;

procedure TTaskPool.OnTaskComplete(const ATask: ITask);
begin
  if FDynamicSize and (FAllTaskList.Count > FMinPoolSize) then
  begin
    FAllTaskList.RemoveTask(ATask);
    Exit;
  end;

  if FRunning then
  begin
    FFreeTaskList.AddTask(ATask);
    ATask.SignalWaitFor;
  end;
end;

procedure TTaskPool.WatchWndProc(var Msg: TMessage);
var
  MessageObj: TMessageObj;
  TaskMessageProc: TOnTaskMessage;
begin
  if Msg.msg = WM_TASK_MESSAGE then
  begin
    MessageObj := TMessageObj(Pointer(Msg.WParam));
    try
      if Msg.LParam <> 0 then
      begin
        TaskMessageProc := TOnTaskMessage(PMethod(Msg.LParam)^);
        TaskMessageProc(MessageObj.Msg);
      end;
    finally
      MessageObj.Free;
    end;
  end
  else
    Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

function TTaskPool.AcquireTask(const TaskMethod: TOnTaskEvent; const Name: string; const OnMessage: TOnTaskMessage): ITask;
begin
  Result := AcquireTask(TaskMethod, Name);
  Result.OnTaskMessage := OnMessage;
end;

function TTaskPool.AcquireTask(const TaskMethod: TOnTaskEvent; const Name: string): ITask;
begin
  if not FRunning then
    raise Exception.Create('Cannot AcquireTask, the pool is not running!');
    
  // set task data
  Result := AcquireFreeTask;
  Result.OnTaskMessage := FOnTaskMessage;
  Result.TaskMethod := TaskMethod;
  Result.Name := Name;
  Result.Values.Clear;
end;

function TTaskPool.AcquireFreeTask: ITask;
begin
  Result := FFreeTaskList.AcquireFirstTask;

  if Result = nil then
  begin
    Result := TTask.Create(OnTaskComplete, FWndHandle);
    FAllTaskList.AddTask(Result);
  end;
end;

{ TWorkerThread }

constructor TWorkerThread.Create(const Owner: ITask; const OnTaskComplete: TOnTaskEvent);
begin
  FreeOnTerminate := True;

  // create the event to put thread to sleep
  FEvents[0] := CreateEvent(nil, False, False, nil);
  FEvents[1] := CreateEvent(nil, False, False, nil);
  // set the pool window handle
  FOnTaskComplete := OnTaskComplete;
  // set the owner task with weak ref.
  FOwner := Pointer(Owner);

  inherited Create(False);
end;

destructor TWorkerThread.Destroy;
begin
  CloseHandle(FEvents[1]);
  CloseHandle(FEvents[0]);

  inherited;
end;

procedure TWorkerThread.Execute;
var
  WaitResult: Cardinal;
begin
  inherited;

  // wait until the thread is actually called
  WaitResult := WaitForMultipleObjects(Length(FEvents), @FEvents, False, INFINITE);

  // main thread loop
  while not (WaitResult = WAIT_OBJECT_0) and not Terminated do
  begin
    ITask(FOwner).Terminated := False;
    // execute the task method
    ITask(FOwner).TaskMethod(ITask(FOwner));

    // signal we are done
    FOnTaskComplete(ITask(FOwner));
    // wait for the next client call
    WaitResult := WaitForMultipleObjects(Length(FEvents), @FEvents, False, INFINITE);
  end;
end;

procedure TWorkerThread.SignalAbort;
begin
  SetEvent(FEvents[0]);
end;

procedure TWorkerThread.SignalRun;
begin
  SetEvent(FEvents[1]);
end;

{ TAllTaskList }

procedure TAllTaskList.Clear;
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Clear;
  finally
    FListLock.ReleaseExclusive;
  end;
end;

constructor TAllTaskList.Create;
begin
  FInternalList := TInterfaceList.Create;
  FListLock.Initialize;
end;

destructor TAllTaskList.Destroy;
begin
  FreeAndNil(FInternalList);

  inherited;
end;

procedure TAllTaskList.AddTask(const ATask: ITask);
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Add(ATask);
  finally
    FListLock.ReleaseExclusive;
  end;
end;

function TAllTaskList.GetCount: Int64;
begin
  FListLock.AcquireShared;
  try
    Result := FInternalList.Count;
  finally
    FListLock.ReleaseShared;
  end;
end;

procedure TAllTaskList.RemoveTask(const ATask: ITask);
begin
  FListLock.AcquireExclusive;
  try
    FInternalList.Remove(ATask);
  finally
    FListLock.ReleaseExclusive;
  end;
end;

function TAllTaskList.AcquireFirstTask: ITask;
begin
  Result := nil;

  FListLock.AcquireExclusive;
  try
    if FInternalList.Count > 0 then
    begin
      Result := ITask(FInternalList.First);
      FInternalList.Remove(FInternalList.First);
    end;
  finally
    FListLock.ReleaseExclusive;
  end;
end;

{ TFreeTaskList }

function TFreeTaskList.AcquireFirstTask: ITask;
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
    Result := ITask(Value.AsInterface);
  {$IFEND};
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  try
    Result := nil;

    if FInternalList.Count > 0 then
    begin
      Result := ITask(FInternalList.First);
      FInternalList.Remove(Result);
    end;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
{$ENDIF}
end;

procedure TFreeTaskList.AddTask(const ATask: ITask);
begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Push(ATask);
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  try
    FInternalList.Add(ATask);
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
{$ENDIF}
end;

procedure TFreeTaskList.Clear;
begin
{$IFNDEF Threading_NoLockFreeStack}
  FInternalList.Clear;
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  try
    FInternalList.Clear;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
{$ENDIF}
end;

constructor TFreeTaskList.Create;
const
  cInitialSize = 500;
begin
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
end;

destructor TFreeTaskList.Destroy;
begin
  FreeAndNil(FInternalList);
{$IFDEF Threading_NoLockFreeStack}
  DeleteCriticalSection(FCriticalSec);
{$ENDIF}

  inherited;
end;

function TFreeTaskList.GetCount: Int64;
begin
{$IFNDEF Threading_NoLockFreeStack}
  Result := FInternalList.Count;
{$ELSE}
  EnterCriticalSection(FCriticalSec);
  try
    Result := FInternalList.Count;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
{$ENDIF}
end;

{ TTask }

constructor TTask.Create(const OnTaskComplete: TOnTaskEvent; const WinHandle: HWND);
begin
  FWorkerThread := TWorkerThread.Create(Self, OnTaskComplete);
  FWaitForEvent := CreateEvent(nil, False, False, nil);
  FThreadHandle := FWorkerThread.Handle;
  FValues := AcquireTaskValues;
  FHWND := WinHandle;
end;

destructor TTask.Destroy;
begin
  FWorkerThread.Terminate;
  FWorkerThread.SignalAbort;

  inherited;
end;

function TTask.GetValues: ITaskValues;
begin
  Result := FValues;
end;

function TTask.GetMessage: ITaskValues;
begin
  if FCurrentMessage = nil then
    FCurrentMessage := TMessageObj.Create(TTaskMessage.Create(FName));

  // return current message
  Result := FCurrentMessage.Msg.Values;
end;

function TTask.GetName: string;
begin
  Result := FName;
end;

function TTask.GetOnTaskMessage: TOnTaskMessage;
begin
  Result := FOnTaskMessage;
end;

function TTask.GetTaskMethod: TOnTaskEvent;
begin
  Result := FTaskMethod;
end;

function TTask.GetTerminated: Boolean;
begin
  Result := FTerminated;
end;

procedure TTask.Run;
begin
  FWorkerThread.SignalRun;
end;

procedure TTask.SendMessageAsync;
var
  AResult: Boolean;
  TryCount: Integer;
begin
  AResult := False;
  TryCount := 0;

  while not AResult and (TryCount < 3) do
  begin
    AResult := PostMessage(FHWND, WM_TASK_MESSAGE, Integer(Pointer(FCurrentMessage)), Integer(Addr(TMethod(FOnTaskMessage))));
    Inc(TryCount);
  end;

  // check result
  case AResult of
    False: FreeAndNil(FCurrentMessage);
    True: FCurrentMessage := nil;
  end;
end;

procedure TTask.SendMessageSync(const Timeout: Integer);
var
  WParam: Integer;
  AResult: Integer;
  TryCount: Integer;
  Response: {$IF CompilerVersion >= 23}PDWORD_PTR{$ELSE}Cardinal{$IFEND};
begin
  {$IF CompilerVersion >= 23}Response := nil{$ELSE}Response := 0{$IFEND};
  WParam :=  Integer(Pointer(FCurrentMessage));
  TryCount := 0;
  AResult := 1;

  while (AResult <> 0) and (TryCount < 3) do
  begin
    AResult := SendMessageTimeout(FHWND, WM_TASK_MESSAGE, WParam, Integer(Addr(TMethod(FOnTaskMessage))), SMTO_BLOCK, Timeout, Response);
    Inc(TryCount);
  end;

  case (AResult <> 0) of
    False: FreeAndNil(FCurrentMessage);
    True: FCurrentMessage := nil;
  end;
end;

procedure TTask.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TTask.SetOnTaskMessage(const Value: TOnTaskMessage);
begin
  FOnTaskMessage := Value;
end;

procedure TTask.SetTaskMethod(const Value: TOnTaskEvent);
begin
  FTaskMethod := Value;
end;

procedure TTask.SetTerminated(const Value: Boolean);
begin
  FTerminated := Value;
end;

procedure TTask.Release;
begin
  FWorkerThread.Terminate;
  FWorkerThread.SignalAbort;
end;

procedure TTask.SignalWaitFor;
begin
  SetEvent(FWaitForEvent);
end;

procedure TTask.Terminate;
begin
  FTerminated := True;
end;

procedure TTask.WaitFor(const Timeout: Cardinal);
begin
  WaitForSingleObject(FThreadHandle, Timeout);
end;

{$IF CompilerVersion >= 20}
procedure TTask.ExecInMainThreadAndWait(const AThreadProc: TThreadProcedure);
begin
  FWorkerThread.Synchronize(AThreadProc);
end;

procedure TTask.ExecInMainThread(const AThreadProc: TThreadProcedure);
begin
  FWorkerThread.Queue(AThreadProc);
end;
{$IFEND}

{ TTaskQueue }

constructor TTaskQueue.Create;
begin
  InitializeCriticalSectionAndSpinCount(FCriticalSec, cCSSpinCount);

  // create nodes
  New(FDeleted);
  New(FHead);

  // set pointer to last
  FDeleted.First := nil;
  FDeleted.Last := nil;
  FDeleted.Count := 0;

  // set pointer to last
  FHead.First := nil;
  FHead.Last := nil;
  FHead.Count := 0;
end;

destructor TTaskQueue.Destroy;
begin
  ClearData(FDeleted);
  ClearData(FHead);
  Dispose(FDeleted);
  Dispose(FHead);

  // delete the critical section last
  DeleteCriticalSection(FCriticalSec);

  inherited;
end;

procedure TTaskQueue.ClearData(const RootItem: PTaskHeadItem);
var
  Item: PTaskQueueItem;
  OldItem: PTaskQueueItem;
begin
  EnterCriticalSection(FCriticalSec);
  try
    Item := RootItem.Last;

    while Item <> nil do
    begin
      OldItem := Item;
      Item := Item.Next;

      CloseHandle(OldItem.Event);
      Dispose(OldItem);
    end;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
end;

function TTaskQueue.AcquireNewItem: PTaskQueueItem;
begin
  if FDeleted.Last <> nil then
  begin
    Result := FDeleted.Last;
    FDeleted.Last := FDeleted.Last.Next;
    FDeleted.Count := FDeleted.Count - 1;

    if FDeleted.Last = nil then
      FDeleted.First := nil;
  end
  else
  begin
    New(Result);
    Result.Event := CreateEvent(nil, False, False, nil);
  end;

  // next is always nil
  Result.Next := nil;
end;

procedure TTaskQueue.DeleteUnusedItem(const Item: PTaskQueueItem);
begin
  // rewire the first element
  case FDeleted.First <> nil of
    True: FDeleted.First.Next := Item;
    False: FDeleted.Last := Item;
  end;

  // add the item at head
  FDeleted.Count := FDeleted.Count + 1;
  FDeleted.First := Item;
  Item.Next := nil;
end;

function TTaskQueue.EnqueueTask: PTaskQueueItem;
begin
  // acquire new item
  Result := AcquireNewItem;

  EnterCriticalSection(FCriticalSec);
  try
    // rewire the list
    FHead.Count := FHead.Count + 1;

    // rewire the first element
    if FHead.First <> nil then
      FHead.First.Next := Result;

    // Head.First points to new element
    FHead.First := Result;

    if FHead.Last = nil then
    begin
      SetEvent(Result.Event);
      FHead.Last := Result;
    end;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
end;

procedure TTaskQueue.DequeueTask;
var
  CurrItem: PTaskQueueItem;
  NextItem: PTaskQueueItem;
begin
  EnterCriticalSection(FCriticalSec);
  try
    CurrItem := FHead.Last;
    try
      FHead.Count := FHead.Count - 1;
      NextItem := CurrItem.Next;
      FHead.Last := NextItem;

      if NextItem <> nil then
        // resume the oldest thread
        SetEvent(NextItem.Event)
      else
        FHead.First := nil;
    finally
      DeleteUnusedItem(CurrItem);
    end;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
end;

function TTaskQueue.GetQueueSize: Integer;
begin
  EnterCriticalSection(FCriticalSec);
  try
    if FHead.First <> nil then
      Result := FHead.Count
    else
      Result := 0;
  finally
    LeaveCriticalSection(FCriticalSec);
  end;
end;

{ TQueueItem }

{$IF CompilerVersion >= 17}
procedure TTaskQueueItem.WaitFor;
begin
  WaitForSingleObject(Event, INFINITE);
end;
{$IFEND}

{ TMessageObj }

constructor TMessageObj.Create(const Msg: ITaskMessage);
begin
  FMessage := Msg;
end;

{ TTaskMessage }

constructor TTaskMessage.Create(const Name: string);
begin
  FName := Name;
  FValues := AcquireTaskValues;
end;

function TTaskMessage.GetName: string;
begin
  Result := FName;
end;

function TTaskMessage.GetValues: ITaskValues;
begin
  Result := FValues;
end;

end.
