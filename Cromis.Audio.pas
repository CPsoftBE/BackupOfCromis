(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008-2012 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * ==================================================================================
 * Collection of various Audio helpers and classes
 * ==================================================================================
 * 3/6/2012 (1.0.0)
 *   - Initial implementation.
 * ==================================================================================
*)unit Cromis.Audio;

interface

uses
  Windows, SysUtils, Classes, Math,

  // cromis units
  Cromis.Threading, Cromis.Threading.TS, Cromis.AnyValue;

const
  cDefaultInitializeTime = 3000;
  cDefaultShutdownTime = 3000;

type
  // RTP declarations
  TRTPHeader = packed record
    Control: Byte;
    PayloadType: Byte;
    SeqNo: WORD;
    TimeStamp: DWORD;
    SSRC: DWORD;
  end;

  TRTPHeader2 = packed record
    Control: Byte;
    PayloadType: Byte;
    SeqNo: WORD;
    TimeStamp: DWORD;
    SSRC: DWORD;
    SCRC: DWORD;
  end;

  TRTP = packed record
    H: TRTPHeader;
    Payload: array[0..159] of Byte;
  end;

  TBufferWrapper = class
  private
    FRTP: TRTP;
    FTimestamp: Int64;
  public
    constructor Create(const RTP: TRTP; const Timestamp: Int64);
    property Timestamp: Int64 read FTimestamp;
    property RTP: TRTP read FRTP;
  end;

  // on RTP packet process function
  TOnBufferEmptyCallback = procedure(const Sender: TObject; const EmptyCount: Integer) of Object;
  TOnRTPPacketProcess = procedure(const Sender: TObject; var RTP: TRTP) of Object;

  TAudioJitter = class
  private
    FFrequency: Int64;
    FSampleRate: Integer;
    FCurrentJitter: Real;
    FPreviousJitter: Real;
    FPrevSentTime: Cardinal;
    FPrevReceivedTime: Int64;
  public
    constructor Create(const SampleRate: Integer);
    procedure ResetJitter;
    procedure AddTimestamp(const Value: Cardinal);
    property CurrentJitter: Real read FCurrentJitter;
  end;

  TJitterBuffer = class(TThread)
  private
    FEnabled: Boolean;
    FFrequency: Int64;
    FProcessing: Boolean;
    FFirstPacket: Boolean;
    FFinishedFlag: THandle;
    FEmptyCounter: Integer;
    FRTPPacketRate: Integer;
    FStartingDelay: Cardinal;
    FStartTimestamp: Int64;
    FProcessingFlag: THandle;
    FInitializedFlag: THandle;
    FFirstPacketFlag: THandle;
    FThreadSafeQueue: TThreadSafeQueue;
    FOnRTPPacketProcess: TOnRTPPacketProcess;
    FOnBufferEmptyCallback: TOnBufferEmptyCallback;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTimestamp: Int64;
    function StopRTPProcessing: Boolean;
    function StartRTPProcessing: Boolean;
    procedure AddBufferPacket(const RTP: TRTP; const Timestamp: Int64);
    property RTPPacketRate: Integer read FRTPPacketRate write FRTPPacketRate;
    property StartingDelay: Cardinal read FStartingDelay write FStartingDelay;
    property OnRTPPacketProcess: TOnRTPPacketProcess read FOnRTPPacketProcess write FOnRTPPacketProcess;
    property OnBufferEmptyCallback: TOnBufferEmptyCallback read FOnBufferEmptyCallback write FOnBufferEmptyCallback;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

{ TAudioJitter }

procedure TAudioJitter.AddTimestamp(const Value: Cardinal);
var
  Diff: Real;
  ReceivedTime: Int64;
begin
  QueryPerformanceCounter(ReceivedTime);
  try
    if FPrevSentTime <> 0 then
    begin
      Diff := (((ReceivedTime - FPrevReceivedTime) / FFrequency) * 1000);
      Diff := Diff - (Value - FPrevSentTime) / (FSampleRate / 1000);
      
      FCurrentJitter := FPreviousJitter + (Abs(Diff) - FPreviousJitter);
      FCurrentJitter := FCurrentJitter / ((FSampleRate / 1000) * 2);
      FPreviousJitter := FCurrentJitter;
    end;
  finally
    // always set previous timestamp
    FPrevReceivedTime := ReceivedTime;
    FPrevSentTime := Value;
  end;
end;

constructor TAudioJitter.Create(const SampleRate: Integer);
begin
  QueryPerformanceFrequency(FFrequency);
  FSampleRate := SampleRate;
end;

procedure TAudioJitter.ResetJitter;
begin
  FCurrentJitter := 0;
end;

{ TJitterBuffer }

procedure TJitterBuffer.AddBufferPacket(const RTP: TRTP; const Timestamp: Int64);
begin
  FThreadSafeQueue.Enqueue(TBufferWrapper.Create(RTP, Timestamp));

  if FFirstPacket then
  begin
    QueryPerformanceCounter(FStartTimestamp);
    SetEvent(FFirstPacketFlag);
    FFirstPacket := False;
  end;
end;

constructor TJitterBuffer.Create;
begin
  FFirstPacketFlag := CreateEvent(nil, False, False, nil);
  FInitializedFlag := CreateEvent(nil, False, False, nil);
  FProcessingFlag := CreateEvent(nil, False, False, nil);
  FFinishedFlag := CreateEvent(nil, False, False, nil);
  FThreadSafeQueue := TThreadSafeQueue.Create;
  QueryPerformanceFrequency(FFrequency);
  FFirstPacket := True;
  FEnabled := True;

  inherited Create(False);
end;

destructor TJitterBuffer.Destroy;
begin
  FreeAndNil(FThreadSafeQueue);

  CloseHandle(FFinishedFlag);
  CloseHandle(FProcessingFlag);
  CloseHandle(FInitializedFlag);
  CloseHandle(FFirstPacketFlag);

  inherited;
end;

procedure TJitterBuffer.Execute;
const
  cAvgBufferCount = 20;
var
  RTP: TRTP;
  Timestamp: Int64;
  SleepDelta: Integer;
  DequeueValue: TAnyValue;
  BufferWrapper: TBufferWrapper;
begin
  inherited;

  // this thread should have a very high priority
  SetThreadPriority(Self.Handle, THREAD_PRIORITY_HIGHEST);

  while not Terminated do
  begin
    // thread is ready to go
    SetEvent(FInitializedFlag);
    // wait for processing flag to be set
    WaitForSingleObject(FProcessingFlag, INFINITE);

    // fill the buffer when signaled, it is empty
    WaitForSingleObject(FFirstPacketFlag, INFINITE);
    Sleep(FStartingDelay);

    while not Terminated and FProcessing do
    begin
      if FThreadSafeQueue.Dequeue(DequeueValue) then
      begin
        BufferWrapper := TBufferWrapper(DequeueValue.AsObject);
        try
          FEmptyCounter := 1;
          RTP := BufferWrapper.RTP;
          FOnRTPPacketProcess(Self, RTP);
          QueryPerformanceCounter(Timestamp);

          if FEnabled then
          begin
            // sleep for the specified sample interval
            SleepDelta := Round((((Timestamp - BufferWrapper.Timestamp) / FFrequency) * 1000) - FStartingDelay);

            if SleepDelta > 5 then
              SleepDelta :=  5
            else if SleepDelta < -5 then
              SleepDelta :=  -5;

            Sleep(Max(1, FRTPPacketRate - SleepDelta));
          end;
        finally
          BufferWrapper.Free;
        end;
      end
      else
      begin
        if FEnabled and Assigned(FOnBufferEmptyCallback) then
        begin
          FOnBufferEmptyCallback(Self, FEmptyCounter);
          Inc(FEmptyCounter);
        end;

        if FEnabled then
          Sleep(FStartingDelay)
        else
          Sleep(1);
      end;
    end;
  end;

  // clean the remaining voice RTP packages
  while FThreadSafeQueue.Dequeue(DequeueValue) do
  begin
    BufferWrapper := TBufferWrapper(DequeueValue.AsObject);
    BufferWrapper.Free;
  end;

  // signal that we are done
  SetEvent(FFinishedFlag);
end;

function TJitterBuffer.GetTimestamp: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function TJitterBuffer.StartRTPProcessing: Boolean;
begin
  if WaitForSingleObject(FInitializedFlag, cDefaultInitializeTime) = WAIT_TIMEOUT then
  begin
    Result := False;
    Exit;
  end;

  FProcessing := True;
  // signal thread to continue
  SetEvent(FProcessingFlag);
  // we have success
  Result := True;
end;

function TJitterBuffer.StopRTPProcessing: Boolean;
begin
  Terminate;
  FProcessing := False;
  SetEvent(FProcessingFlag);
  SetEvent(FFirstPacketFlag);
  Result := WaitForSingleObject(FFinishedFlag, cDefaultShutdownTime) <> WAIT_TIMEOUT;
end;

{ TBufferWrapper }

constructor TBufferWrapper.Create(const RTP: TRTP; const Timestamp: Int64);
begin
  FTimestamp := Timestamp;
  FRTP := RTP;
end;

end.
