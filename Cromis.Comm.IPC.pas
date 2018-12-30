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
 * Fast IPC client server communication based on named pipes. Very lightweight.
 * =============================================================================
 * 25/10/2009 (1.0.1)
 *   - Replaced SimpleStorage as data medium with TStreamStorage
 * 25/10/2009 (1.0.2)
 *   - D2010 compatible
 * 30/11/2009 (1.1.0)
 *   - IIPCData contains ID field so requests can be identified
 *   - Use Task Pool to avoid constant thread creation for each request
 * 03/12/2009 (1.1.1)
 *   - Server listening property is read only
 *   - Server has a MinPoolSize property to define the size of task pool
 * 13/12/2009 (1.2.0)
 *   - Improved how the pipe name is constructed and maintained
 *   - Added computer name property to client. Allows to do LAN communication
 *   - Call Break instead of Exit to terminate the listening thread
 * 19/01/2010 (1.2.1)
 *   - Send the data length first. Eliminates the need for PeekPipe usage
 *   - Write data to the pipe in chunks instead in one big swoop
 * 16/10/2011 (1.2.2)
 *   - Improved wait for ERROR_IO_PENDING
 *   - Usage of CommTimeouts
 * 18/03/2012 (1.3.0)
 *   - 64 bit compiler compatible
 * 22/03/2012 (1.3.1)
 *   - Added error description for the client
 * 01/04/2012 (1.4.0)
 *   - Support for connect and disconnect (breaking change in protocol)
 * 14/05/2012 (1.4.1)
 *   - IMessageData ID is now unicode on all delphi versions
 * 22/05/2012 (1.4.2)
 *   - Do not try to WaitForNamedPipe if pipe is not available, saves CPU
 *   - Signal WAIT_TIMEOUT in connect and not in execute request
 *   - Send packet over, even if it contains no data
 * 19/07/2013 (1.5.0)
 *   - OnClientConnect, OnClientDisconnect, OnServerError handlers for server
 *   - Internal list of all connected clients (optional)
 *   - Pass client context inside OnExecute handler
 *   - Improved error handling and notifications
 *   - Unit renamed to Cromis.Comm.IMC
 * 20/08/2013 (1.6.0)
 *   - Reorganized context and client classes
 * 21/10/2013 (1.6.1)
 *   - x64 compatibility fix
 * 22/02/2014 (1.6.2)
 *   - stability improvements
 * 10/03/2014 (1.7.0)
 *   - support for filters (encryption, compression...)
 * =============================================================================
*)
unit Cromis.Comm.IPC;

interface

uses
  Windows, SysUtils, Classes, DateUtils, Math,

  // cromis units
  Cromis.Unicode, Cromis.Streams, Cromis.Streams.Filters, Cromis.Threading,
  Cromis.Threading.CustomPool, Cromis.Comm.Custom;

const
  cBufferSize = 65536;

const
  cStartupTimeout = 3000;
  cDefaultTimeout = 5000;
  cShutdownTimeout = 3000;
  cDefaultMessageSize = 1024;

const
  cInitialPoolSize = 5;

const
  cParamPipeHandle = 'PipeHandle';

type
  IIPCData = IMessageData;

type
  TNotifyServerError = procedure(const Context: ICommContext;
                                 const Code: Cardinal;
                                 const Desc: string)
                                 of Object;

type
  TIPCServer = class
  private
    FFilters: TStreamFilters;
    FPipeName: string;
    FListening: Boolean;
    FServerName: string;
    FAbortEvent: THandle;
    FMinPoolSize: Integer;
    FNumOfErrors: Integer;
    FDynamicPool: Boolean;
    FContextList: TCommContextList;
    FStartupEvent: THandle;
    FFirstErrorTime: TDateTime;
    FListeningThread: TThread;
    FOnServerError: TOnServerError;
    FOnClientConnect: TOnClientEvent;
    FCommClientClass: TCommClientClass;
    FOnExecuteRequest: TOnExecuteRequest;
    FOnClientDisconnect: TOnClientEvent;
    procedure OnExecuteTask(const Task: ITask);
    procedure SetServerName(const Value: string);
    procedure OnThreadTerminated(Sender: TObject);
    procedure DoOnClientConnect(const Context: ICommContext);
    procedure DoOnClientDisconnect(const Context: ICommContext);
    procedure NotifyServerError(const Context: ICommContext;
                                const Code: Cardinal;
                                const Desc: string);
  public
    procedure Stop;
    procedure Start;
    procedure Restart;
    constructor Create;
    destructor Destroy; override;
    property Listening: Boolean read FListening;
    property Filters: TStreamFilters read FFilters;
    property ServerName: string read FServerName write SetServerName;
    property DynamicPool: Boolean read FDynamicPool write FDynamicPool;
    property MinPoolSize: Integer read FMinPoolSize write FMinPoolSize;
    property OnServerError: TOnServerError read FOnServerError write FOnServerError;
    property OnClientConnect: TOnClientEvent read FOnClientConnect write FOnClientConnect;
    property CommClientClass: TCommClientClass read FCommClientClass write FCommClientClass;
    property OnExecuteRequest: TOnExecuteRequest read FOnExecuteRequest write FOnExecuteRequest;
    property OnClientDisconnect: TOnClientEvent read FOnClientDisconnect write FOnClientDisconnect;
  end;

  TIPCClient = class
  private
    FFilters: TStreamFilters;
    FPipeName: string;
    FErrorDesc: string;
    FLastError: Cardinal;
    FServerName: string;
    FPipeHandle: THandle;
    FAnswerValid: Boolean;
    FIsConnected: Boolean;
    FComputerName: string;
    FExecuteTimeout: Cardinal;
    procedure SetServerName(const Value: string);
    procedure SetComputerName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectClient(const ConnectTimeout: Cardinal = cDefaultTimeout);
    procedure ReconnectClient(const ConnectTimeout: Cardinal = cDefaultTimeout);
    procedure DisconnectClient;
    function ExecuteRequest(const Request: IMessageData; const ConnectTimeout: Cardinal = cDefaultTimeout): IMessageData;
    function ExecuteConnectedRequest(const Request: IMessageData): IMessageData;
    property ExecuteTimeout: Cardinal read FExecuteTimeout write FExecuteTimeout;
    property ComputerName: string read FComputerName write SetComputerName;
    property ServerName: string read FServerName write SetServerName;
    property AnswerValid: Boolean read FAnswerValid;
    property IsConnected: Boolean read FIsConnected;
    property Filters: TStreamFilters read FFilters;
    property LastError: Cardinal read FLastError;
    property ErrorDesc: string read FErrorDesc;
  end;

  TIPCClientObject = class(TCustomObject)
  private
    FIPCClient: TIPCClient;
  public
    constructor Create; override;
    destructor Destroy; override;
    property IPCClient: TIPCClient read FIPCClient;
  end;

  TIPCClientPool = class(TTSObjectsPool)
  public
    constructor Create; reintroduce;
    function AcquireObject: TIPCClientObject; reintroduce;
  end;

  function AcquireIPCData: IMessageData;

implementation

type
  TListeningThread = class(TThread)
  private
    FPipeName: string;
    FTaskPool: TTaskPool;
    FAbortEvent: THandle;
    FPipeCreated: Boolean;
    FStartupEvent: THandle;
    FOverlappEvent: THandle;
    FOnExecuteTask: TOnTaskEvent;
    FNotifyServerError: TNotifyServerError;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
    constructor Create(const PipeName: string;
                       const AbortEvent: THandle;
                       const MinPoolSize: Integer;
                       const DynamicPool: Boolean;
                       const StartupEvent: THandle;
                       const OnExecuteTask: TOnTaskEvent;
                       const NotifyServerError: TNotifyServerError);
  end;

function AcquireIPCData: IMessageData;
begin
  Result := TMessageData.Create;
end;

procedure CloseNamedPipe(const PipeHandle: THandle);
begin
  DisconnectNamedPipe(PipeHandle);
  CloseHandle(PipeHandle);
end;

{ TIPCServer }

constructor TIPCServer.Create;
begin
  FContextList := TCommContextList.Create;
  FFilters := TStreamFilters.Create(True);
  FCommClientClass := TCommClient;
  FMinPoolSize := cInitialPoolSize;
  FDynamicPool := True;
  FListening := False;
end;

destructor TIPCServer.Destroy;
begin
  Stop;
  FreeAndNil(FFilters);
  FreeAndNil(FContextList);

  inherited;
end;

procedure TIPCServer.DoOnClientConnect(const Context: ICommContext);
begin
  if FContextList <> nil then
    FContextList.AddContext(Context);

  if Assigned(FOnClientConnect) then
    FOnClientConnect(Context);
end;

procedure TIPCServer.DoOnClientDisconnect(const Context: ICommContext);
begin
  if Assigned(FOnClientDisconnect) then
    FOnClientDisconnect(Context);

  if FContextList <> nil then
    FContextList.RemoveContext(Context);
end;

procedure TIPCServer.NotifyServerError(const Context: ICommContext;
                                       const Code: Cardinal;
                                       const Desc: string);
var
  Error: TServerError;
begin
  if Assigned(FOnServerError) then
  begin
    Error.Code := Code;
    Error.Desc := Desc;
    FOnServerError(Context, Error);
  end;
end;

procedure TIPCServer.OnExecuteTask(const Task: ITask);
var
  AResult: Boolean;
  AllRead: Int64;
  DataSize: Int64;
  IDLength: Int64;
  BytesRead: Cardinal;
  IDAsString: ustring;
  BytesWritten: Cardinal;
  ErrorMessage: string;

  // context variables
  Context: ICommContext;

  // input output data
  InStream: IMessageData;
  OutStream: IMessageData;

  // use filters list
  UsedFilters: IUsedFilters;

  // pipe handle
  PipeHandle: THandle;
  // id as array of byte
  IDAsBytes: array of Byte;
  // input buffer of cBufferSize bytes
  DataBuffer: array [0..cBufferSize - 1] of Byte;
begin
  inherited;

  Context := nil;
  try
    PipeHandle := Task.Values.Get(cParamPipeHandle).AsInt64;
    try
      // acquire new client context first
      Context := AcquireCommContext(FCommClientClass);
      // we are connected already here
      DoOnClientConnect(Context);
      try
        repeat
          // first read the ID size of the IPC
          AResult := ReadFile(PipeHandle, IDLength, SizeOf(Int64), BytesRead, nil);

          // did we recieve disconnect
          if IDLength = -1 then
            Exit;

          // read the request data first
          InStream := AcquireIPCData;

          // check if the result is a valid response
          if not AResult and (GetLastError <> ERROR_MORE_DATA) then
          begin
            NotifyServerError(Context, GetLastError, 'ReadFile failed while reading message ID Length');
            Exit;
          end;

          if IDLength > 0 then
          begin
            // set ID bytes length
            SetLength(IDAsBytes, IDLength);
            SetLength(IDAsString, IDLength div SizeOf(uchar));
            // then read the actual ID of the IPC data from the pipe
            AResult := ReadFile(PipeHandle, IDAsBytes[0], IDLength, BytesRead, nil);

            // check if the result is a valid response
            if not AResult and (GetLastError <> ERROR_MORE_DATA) then
            begin
              NotifyServerError(Context, GetLastError, 'ReadFile failed while reading message ID');
              Exit;
            end;

            // first read the ID of the request
            Move(IDAsBytes[0], IDAsString[1], IDLength);
            InStream.ID := IDAsString;
          end;

          // then read the actual size of the IPC data from the pipe
          AResult := ReadFile(PipeHandle, DataSize, SizeOf(Int64), BytesRead, nil);

          // check if the result is a valid response
          if not AResult and (GetLastError <> ERROR_MORE_DATA) then
          begin
            NotifyServerError(Context, GetLastError, 'ReadFile failed while reading message size');
            Exit;
          end;

          if DataSize > 0 then
          begin
            AllRead := 0;

            repeat
              AResult := ReadFile(PipeHandle, DataBuffer[0], cBufferSize, BytesRead, nil);
              InStream.Data.Storage.Write(DataBuffer[0], BytesRead);
              Inc(AllRead, BytesRead);

              // check if the result is a valid response
              if not AResult and (GetLastError <> ERROR_MORE_DATA) then
              begin
                NotifyServerError(Context, GetLastError, 'ReadFile failed while reading message data');
                Exit;
              end;
            until AllRead = DataSize;
          end;

          // create result and call event
          OutStream := AcquireIPCData;
          try
            if FFilters.Count > 0 then
              UsedFilters := InStream.Data.ApplyOutputFilters(FFilters)
            else
              UsedFilters := nil;

            // call the OnExecuteRequest callback handler
            FOnExecuteRequest(Context, InStream, OutStream);

            if (UsedFilters <> nil) and (UsedFilters.FilterList.Count > 0) then
              OutStream.Data.ApplyInputFilters(UsedFilters.FilterList);
          finally
            OutStream.Data.Storage.Position := 0;
          end;

          // write the length and the actual id to byte array
          IDLength := Length(OutStream.ID) * SizeOf(uchar);
          SetLength(IDAsBytes, IDLength + SizeOf(Int64));
          Move(IDLength, IDAsBytes[0], SizeOf(Int64));
          DataSize := OutStream.Data.Storage.Size;

          // only if there is data
          if IDLength > 0 then
            Move(OutStream.ID[1], IDAsBytes[SizeOf(Int64)], IDLength);

          // write the id as first data to the pipe
          if not WriteFile(PipeHandle, IDAsBytes[0], Length(IDAsBytes), BytesWritten, nil) then
          begin
            NotifyServerError(Context, GetLastError, 'WriteFile failed while writing message ID and data');
            Exit;
          end;

          // write the data size so that server knows how much it will get
          if not WriteFile(PipeHandle, DataSize, SizeOf(Int64), BytesWritten, nil) then
          begin
            NotifyServerError(Context, GetLastError, 'WriteFile failed while writing message size');
            Exit;
          end;

          // check if IPC data is empty
          if OutStream.Data.Storage.Size > 0 then
          begin
            BytesRead := OutStream.Data.Storage.Read(DataBuffer[0], cBufferSize);

            while BytesRead > 0 do
            begin
              if not WriteFile(PipeHandle, DataBuffer[0], BytesRead, BytesWritten, nil) then
              begin
                NotifyServerError(Context, GetLastError, 'WriteFile failed while writing message data');
                Exit;
              end;

              // read the next chunk from the input data stream
              BytesRead := OutStream.Data.Storage.Read(DataBuffer[0], cBufferSize);
            end;
          end;
        until IDLength = -1;
      finally
        FlushFileBuffers(PipeHandle);
        DoOnClientDisconnect(Context);
      end;
    finally
      CloseNamedPipe(PipeHandle);
    end;
  except
    on E: Exception do
    begin
      ErrorMessage := Format('Error in OnExecuteTask: %s', [E.Message]);
      NotifyServerError(Context, GetLastError, ErrorMessage);
    end;
  end;
end;

procedure TIPCServer.OnThreadTerminated(Sender: TObject);
const
  cErrorResetTime = 30;
  cMaxErrorCount = 5;
begin
  if FListening then
  begin
    NotifyServerError(nil, GetLastError, 'Listening thread was terminated abnormally!' );

    if SecondsBetween(Now, FFirstErrorTime) > cErrorResetTime then
    begin
      FFirstErrorTime := Now;
      FNumOfErrors := 0;
    end;

    if FNumOfErrors < cMaxErrorCount then
    begin
      FListening := False;
      // increase errors
      Inc(FNumOfErrors);

      // set and close event
      CloseHandle(FAbortEvent);
      // clear contexts
      FContextList.Clear;
      // start again
      Start;
    end
    else
    begin
      NotifyServerError(nil, GetLastError, 'Tried to restart listening thread to many times!');
      raise Exception.Create('Tried to restart listening thread to many times!');
    end;
  end;
end;

procedure TIPCServer.Restart;
begin
  Stop;
  Start;
end;

procedure TIPCServer.SetServerName(const Value: string);
begin
  FPipeName := Format('\\.\pipe\%s', [Value]);
  FServerName := Value;
end;

procedure TIPCServer.Start;
begin
  if not Assigned(FOnExecuteRequest) then
    raise Exception.Create('OnExecuteRequest not assigned');

  // is listening
  FListening := True;
  // create the thread abortion event
  FAbortEvent := CreateEvent(nil, False, False, nil);
  // create the named pipe thread startup event
  FStartupEvent :=  CreateEvent(nil, False, False, nil);
  // start listening for request in a separate thread
  FListeningThread := TListeningThread.Create(FPipeName,
                                              FAbortEvent,
                                              FMinPoolSize,
                                              FDynamicPool,
                                              FStartupEvent,
                                              OnExecuteTask,
                                              NotifyServerError);
  FListeningThread.OnTerminate := OnThreadTerminated;

  // wait for the server to start up
  WaitForSingleObject(FStartupEvent, cStartupTimeout);
end;

procedure TIPCServer.Stop;
var
  AResult: Cardinal;
begin
  if FListening then
  begin
    // set on terminate to nil and set listening flag
    FListeningThread.OnTerminate := nil;
    FListening := False;

    // set and close event
    SetEvent(FAbortEvent);

    // wait and block until thread is finished
    AResult := WaitForSingleObject(FListeningThread.Handle, cShutdownTimeout);

    // check if we timed out
    if AResult = WAIT_TIMEOUT then
      TerminateThread(FListeningThread.Handle, 0);

    // clear contexts and events
    CloseHandle(FAbortEvent);
    FContextList.Clear;
  end;
end;

{ TIPCClient }

constructor TIPCClient.Create;
begin
  FFilters := TStreamFilters.Create(True);
  FComputerName := '.';
end;

destructor TIPCClient.Destroy;
begin
  DisconnectClient;
  FreeAndNil(FFilters);

  inherited;
end;

procedure TIPCClient.ConnectClient(const ConnectTimeout: Cardinal);
var
  AResult: Boolean;
  TimeLeft: Cardinal;
  StartTime: TDateTime;
begin
  FPipeHandle := INVALID_HANDLE_VALUE;
  FIsConnected := False;
  FErrorDesc := '';
  FLastError := 0;
  StartTime := Now;

  while MilliSecondsBetween(Now, StartTime) < ConnectTimeout do
  begin
    FPipeHandle := CreateFile(PChar(FPipeName),
                              GENERIC_READ or GENERIC_WRITE,
                              0, nil, OPEN_EXISTING, 0, 0);

    // pipe handle acquired, so exit loop
    if FPipeHandle <> INVALID_HANDLE_VALUE then
    begin
      FIsConnected := True;
      Exit;
    end;

    if not GetLastError in [ERROR_FILE_NOT_FOUND, ERROR_PIPE_BUSY] then
    begin
      FErrorDesc := 'CreateFile failed while creating new pipe handle';
      FLastError := GetLastError;
      Exit;
    end;

    // wait for the pipe to become available (include timeout left)
    TimeLeft := Max(0, ConnectTimeout - MilliSecondsBetween(Now, StartTime));
    AResult := WaitNamedPipe(PChar(FPipeName), TimeLeft);

    if not AResult and not (GetLastError in [ERROR_SEM_TIMEOUT, ERROR_PIPE_BUSY, ERROR_FILE_NOT_FOUND]) then
    begin
      FErrorDesc := 'WaitNamedPipe failed with fatal error';
      FLastError := GetLastError;
      Exit;
    end;
  end;

  // we have timed out while trying to connect
  if FPipeHandle = INVALID_HANDLE_VALUE then
  begin
    FErrorDesc := 'IPC client failed with WAIT timeout';
    FLastError := WAIT_TIMEOUT;
    Exit;
  end;
end;

procedure TIPCClient.DisconnectClient;
var
  ABytes: Cardinal;
  DisconnectSignal: Int64;
begin
  if (FPipeHandle <> INVALID_HANDLE_VALUE) and (FPipeHandle <> 0) then
  begin
    // send signal
    DisconnectSignal := -1;
    WriteFile(FPipeHandle, DisconnectSignal, SizeOf(Int64), ABytes, nil);
  end;

  // always close handle
  CloseHandle(FPipeHandle);
  FIsConnected := False;
  FPipeHandle := 0;
end;

function TIPCClient.ExecuteRequest(const Request: IMessageData; const ConnectTimeout: Cardinal): IMessageData;
begin
  ConnectClient(ConnectTimeout);
  Result := nil;

  if FIsConnected then
  begin
    Result := ExecuteConnectedRequest(Request);
    DisconnectClient;
  end;
end;

procedure TIPCClient.ReconnectClient(const ConnectTimeout: Cardinal = cDefaultTimeout);
begin
  DisconnectClient;
  ConnectClient(ConnectTimeout);
end;

function TIPCClient.ExecuteConnectedRequest(const Request: IMessageData): IMessageData;
var
  ABytes: Cardinal;
  AResult: Boolean;
  AllRead: Int64;
  DataSize: Int64;
  IDLength: Int64;
  BytesRead: Cardinal;
  IDAsBytes: array of Byte;
  IDAsString: ustring;

  // communication timeout
  CommTimeouts: TCommTimeouts;
  // output buffer of cBufferSize bytes
  DataBuffer: array [0..cBufferSize - 1] of Byte;
begin
  FAnswerValid := False;
  FLastError := 0;
  Result := nil;

  // check for filters
  if FFilters.Count > 0 then
    Request.Data.ApplyInputFilters(FFilters);

  if FExecuteTimeout > 0 then
  begin
    GetCommTimeouts(FPipeHandle, CommTimeouts);

    CommTimeouts.WriteTotalTimeoutMultiplier := MAXDWORD;
    CommTimeouts.WriteTotalTimeoutConstant := FExecuteTimeout;

    CommTimeouts.ReadTotalTimeoutMultiplier := MAXDWORD;
    CommTimeouts.ReadTotalTimeoutConstant := FExecuteTimeout;
    CommTimeouts.ReadIntervalTimeout := MAXDWORD;
    SetCommTimeouts(FPipeHandle, CommTimeouts);
  end;

  // rewind the stream before write
  DataSize := Request.Data.Storage.Size;
  Request.Data.Storage.Position := 0;

  // write the length and the actual id to byte array
  IDLength := Length(Request.ID) * SizeOf(uchar);
  SetLength(IDAsBytes, IDLength + SizeOf(Int64));
  Move(IDLength, IDAsBytes[0], SizeOf(Int64));

  // only if there is data
  if IDLength > 0 then
    Move(Request.ID[1], IDAsBytes[SizeOf(Int64)], IDLength);

  // write the id as first data to the pipe
  if not WriteFile(FPipeHandle, IDAsBytes[0], Length(IDAsBytes), ABytes, nil) then
  begin
    FErrorDesc := 'WriteFile failed while writing message ID';
    FLastError := GetLastError;
    Exit;
  end;

  // write the data size so that server knows how much it will get
  if not WriteFile(FPipeHandle, DataSize, SizeOf(Int64), ABytes, nil) then
  begin
    FErrorDesc := 'WriteFile failed while writing message data size';
    FLastError := GetLastError;
    Exit;
  end;

  if Request.Data.Storage.Size > 0 then
  begin
    BytesRead := Request.Data.Storage.Read(DataBuffer[0], cBufferSize);

    while BytesRead > 0 do
    begin
      // send the request data to the pipe server and wait for response
      if not WriteFile(FPipeHandle, DataBuffer[0], BytesRead, ABytes, nil) then
      begin
        FErrorDesc := 'WriteFile failed while writing message chunk';
        FLastError := GetLastError;
        Exit;
      end;

      // read the next chunk from the input data stream
      BytesRead := Request.Data.Storage.Read(DataBuffer[0], cBufferSize);
    end;
  end;

  // create the response data
  Result := AcquireIPCData;
  // first read the ID size of the IPC
  AResult := ReadFile(FPipeHandle, IDLength, SizeOf(Int64), BytesRead, nil);

  // check if the result is a valid response
  if not AResult and (GetLastError <> ERROR_MORE_DATA) then
  begin
    FErrorDesc := 'ReadFile failed while reading message ID length';
    FLastError := GetLastError;
    Exit;
  end;

  if IDLength > 0 then
  begin
    // set ID bytes length
    SetLength(IDAsBytes, IDLength);
    SetLength(IDAsString, IDLength div SizeOf(uchar));
    // then read the actual ID of the IPC data from the pipe
    AResult := ReadFile(FPipeHandle, IDAsBytes[0], IDLength, BytesRead, nil);

    // check if the result is a valid response
    if not AResult and (GetLastError <> ERROR_MORE_DATA) then
    begin
      FErrorDesc := 'ReadFile failed while reading message ID';
      FLastError := GetLastError;
      Exit;
    end;

    // first read the ID of the request
    Move(IDAsBytes[0], IDAsString[1], IDLength);
    Result.ID := IDAsString;
  end;

  // read the size of the data from server
  AResult := ReadFile(FPipeHandle, DataSize, SizeOf(Int64), BytesRead, nil);

  // check if the result is a valid response
  if not AResult and (GetLastError <> ERROR_MORE_DATA) then
  begin
    FErrorDesc := 'ReadFile failed while reading message data size';
    FLastError := GetLastError;
    Exit;
  end;

  if DataSize > 0 then
  begin
    AllRead := 0;

    repeat
      AResult := ReadFile(FPipeHandle, DataBuffer[0], cBufferSize, BytesRead, nil);
      Result.Data.Storage.Write(DataBuffer[0], BytesRead);
      Inc(AllRead, BytesRead);

      // check if the result is a valid response
      if not AResult and (GetLastError <> ERROR_MORE_DATA) then
      begin
        FErrorDesc := 'ReadFile failed while reading message data';
        FLastError := GetLastError;
        Exit;
      end;
    until AllRead = DataSize;
  end;

  // check for filters
  if FFilters.Count > 0 then
    Result.Data.ApplyOutputFilters(FFilters);

  // return response to the caller
  Result.Data.Storage.Position := 0;
  // we were succesfull
  FAnswerValid := True;
end;

procedure TIPCClient.SetComputerName(const Value: string);
begin
  case Value <> '' of
    True: FComputerName := Value;
    False: FComputerName := '.';
  end;

  // set the full name of the pipe to connect to
  FPipeName := Format('\\%s\pipe\%s', [FComputerName, FServerName]);
end;

procedure TIPCClient.SetServerName(const Value: string);
begin
  FServerName := Value;
  FPipeName := Format('\\%s\pipe\%s', [FComputerName, FServerName]);
end;

{ TListeningThread }

constructor TListeningThread.Create(const PipeName: string;
                                    const AbortEvent: THandle;
                                    const MinPoolSize: Integer;
                                    const DynamicPool: Boolean;
                                    const StartupEvent: THandle;
                                    const OnExecuteTask: TOnTaskEvent;
                                    const NotifyServerError: TNotifyServerError);
begin
  FTaskPool := TTaskPool.Create(MinPoolSize);
  FTaskPool.DynamicSize := DynamicPool;
  FTaskPool.Initialize;

  FNotifyServerError := NotifyServerError;
  FOnExecuteTask := OnExecuteTask;
  FStartupEvent := StartupEvent;
  FAbortEvent := AbortEvent;
  FreeOnTerminate := True;
  FPipeCreated := False;
  FPipeName := PipeName;

  inherited Create(False);
end;

destructor TListeningThread.Destroy;
begin
  FTaskPool.Finalize;
  FreeAndNil(FTaskPool);

  inherited;
end;

procedure TListeningThread.Execute;
const
  cIOPendingTimeout = 100;
var
  AError: Cardinal;
  AResult: Cardinal;
  PipeHandle: THandle;
  Overlapped: TOverlapped;

  // security attributes
  PSA: TSecurityAttributes;
  PSD: TSecurityDescriptor;

  // task
  Task: ITask;
  // array of wait handles for wait func
  WaitArray: array [0..1] of THandle;
begin
  inherited;

  PipeHandle := INVALID_HANDLE_VALUE;
  try
    InitializeSecurityDescriptor(@PSD, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@PSD, True, PACL(0), False);
    PSA.lpSecurityDescriptor := @PSD;
    PSA.bInheritHandle := True;
    PSA.nLength := SizeOf(PSA);

    FOverlappEvent := CreateEvent(nil, False, False, nil);
    try
      FillChar(Overlapped, SizeOf(TOverlapped), 0);
      // create the overlapped event for the named pipe
      Overlapped.hEvent := FOverlappEvent;
      WaitArray[0] := Overlapped.hEvent;
      WaitArray[1] := FAbortEvent;

      while not Terminated do
      begin
        // create the pipe
        PipeHandle := CreateNamedPipe(PChar(FPipeName),
                                      PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,
                                      PIPE_TYPE_BYTE or PIPE_READMODE_BYTE,
                                      PIPE_UNLIMITED_INSTANCES,
                                      cDefaultMessageSize,
                                      cDefaultMessageSize,
                                      cDefaultTimeout,
                                      @PSA);

        // check if pipe was created
        if PipeHandle = INVALID_HANDLE_VALUE then
        begin
          FNotifyServerError(nil, GetLastError, 'Could not create new pipe instance.');
          raise Exception.Create('Could not create new pipe instance.');
        end;

        // signal success
        if not FPipeCreated then
        begin
          FPipeCreated := True;
          SetEvent(FStartupEvent);
          CloseHandle(FStartupEvent);
        end;

        // connect the pipe in overlapped mode
        ConnectNamedPipe(PipeHandle, @Overlapped);
        // get pipe connect state
        AError := GetLastError;

        if (AError = ERROR_IO_PENDING) then
        begin
          repeat
            AResult := WaitForMultipleObjects(Length(WaitArray), @WaitArray, False, cIOPendingTimeout);
            AError := GetLastError;

            if AResult <> WAIT_OBJECT_0 then
            begin
              if AResult = WAIT_TIMEOUT then
              begin
                if AError <> ERROR_IO_PENDING then
                begin
                  FNotifyServerError(nil, AError, 'Error waiting for Pipe to connect');
                  CloseNamedPipe(PipeHandle);
                end;
              end
              else if AResult = (WAIT_OBJECT_0 + 1) then
              begin
                CloseNamedPipe(PipeHandle);
                Exit;
              end;
            end;
          until (AResult = WAIT_OBJECT_0) or (AError <> ERROR_IO_PENDING);
        end;

        if (AError = ERROR_PIPE_CONNECTED) or (AError = ERROR_IO_PENDING) then
        begin
          Task := FTaskPool.AcquireTask(FOnExecuteTask, IntToStr(PipeHandle));
          Task.Values.Ensure(cParamPipeHandle).AsInt64 := PipeHandle;
          Task.Run;
        end
        else
        begin
          FNotifyServerError(nil, GetLastError, 'Error waiting for Pipe to connect');
          CloseNamedPipe(PipeHandle);
        end;
      end;
    finally
      // close the overlapp event
      CloseHandle(FOverlappEvent);
    end;
  except
    on E: Exception do
    begin
      FNotifyServerError(nil, GetLastError, E.Message);
      CloseNamedPipe(PipeHandle);
      Exit;
    end;
  end;
end;

{ TIPCClientObject }

constructor TIPCClientObject.Create;
begin
  inherited;

  FIPCClient := TIPCClient.Create;
end;

destructor TIPCClientObject.Destroy;
begin
  FreeAndNil(FIPCClient);

  inherited;
end;

{ TIPCClientPool }

constructor TIPCClientPool.Create;
begin
  inherited Create(TIPCClientObject);
end;

function TIPCClientPool.AcquireObject: TIPCClientObject;
begin
  Result := (inherited AcquireObject) as TIPCClientObject;
end;

end.
