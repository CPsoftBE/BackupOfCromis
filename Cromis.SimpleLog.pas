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
 * A Simple yet powerfull loging unit that can log over multiple processes
 * and can handle more that one log from a single location. Integrates with
 * Cromis.Exceptions
 * =============================================================================
 * 25/10/2009 (1.1.0)
 *   - Use the new rewritten Cromis.Exceptions
 *   - Log file limit is now working correctly
 * =============================================================================
 * 12/03/2011 (1.2.0)
 *   - Fixed a bug of missing hour in arhived log files
 *   - Added MaxFiles for LogObject so old logs are automatically cleaned
 * =============================================================================
 * 24/07/2011 (1.3.0)
 *   - Added LogProperties, so a user can control what is written to log
 *   - Added LogDelimiter that controls which delimiter is use as field separator
 * =============================================================================
 * 25/07/2011 (1.3.1)
 *   - RegisterLog is now a function that returns TLogInstance
 *   - TLogInstance is now accessible from other units
 *   - Added Thread ID as log property
 *   - Fixed CheckForMaxLogFiles
 * =============================================================================
*)
unit Cromis.SimpleLog;

interface

uses
  Windows, SysUtils, Classes, Contnrs, DateUtils,

  // cromis units
  Cromis.Streams, Cromis.StringUtils;

const
  cDefaultSize = 10000;
  cDefaultMaxFiles = 0;

type
  TLogLockType = (ltNone, ltProcess, ltMachine);
  TLogType = (ltError, ltWarning, ltHint, ltInfo);

  TLogProperty = (lpTimestamp, lpType, lpComputername, lpUsername, lpPID, lpTID, lpModuleName);
  TLogProperties = set of TLogProperty;

const
  clpDefault = [lpTimestamp, lpType, lpComputername, lpUsername, lpPID, lpTID, lpModuleName];

type
  TLogInstance = class
  private
    FLogName: string;
    FFileName: string;
    FUserName: string;
    FMaxFiles: Integer;
    FLogMutex: Cardinal;
    FMutexName: string;
    FModulePID: Integer;
    FSizeLimit: Cardinal;
    FModuleName: string;
    FFileStream: TFileStream;
    FLogDelimiter: string;
    FComputerName: string;
    FLogCRSection: TRTLCriticalSection;
    FLogProperties: TLogProperties;
    procedure SetFileName(const Value: string);
    function CreateMutexOrWait: Cardinal;
    procedure CheckForMaxLogFiles;
    procedure InitializeStreaming;
    procedure FinalizeStreaming;
    procedure CheckStreamLimit;
  public
    destructor Destroy; override;
    constructor Create(const LogName, FileName: string);
    procedure LogEvent(const Event: string; const LogType: TLogType);
    procedure InitializeThreadCheck(const LockType: TLogLockType);
    procedure FinalizeThreadCheck(const LockType: TLogLockType);
    property LogProperties: TLogProperties read FLogProperties write FLogProperties;
    property LogDelimiter: string read FLogDelimiter write FLogDelimiter;
    property SizeLimit: Cardinal read FSizeLimit write FSizeLimit;
    property MaxFiles: Integer read FMaxFiles write FMaxFiles;
    property FileName: string read FFileName write SetFileName;
    property LogName: string read FLogName write FLogName;
  end;

  TSimpleLog = class
  private
    FLogList: TObjectList;
    FLockType: TLogLockType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UnRegisterLog(const LogName: string);
    procedure LogInfo(const LogName, Event: string);
    procedure LogHint(const LogName, Event: string);
    procedure LogError(const LogName, Event: string);
    procedure LogWarning(const LogName, Event: string);
    function RegisterLog(const LogName, FileName: string;
                         const SizeLimit: Cardinal = cDefaultSize;
                         const MaxFiles: Integer = cDefaultMaxFiles;
                         const LogProperties: TLogProperties = clpDefault;
                         const LogDelimiter: string = ' | '): TLogInstance;
    procedure LogEvent(const LogName, Event: string; const LogType: TLogType = ltError);
    property LockType: TLogLockType read FLockType write FLockType;
  end;

  function SimpleLog: TSimpleLog;

implementation

var
  SimpleLogObj: TSimpleLog;

function SimpleLog: TSimpleLog;
begin
  if SimpleLogObj = nil then
    SimpleLogObj := TSimpleLog.Create;

  Result := SimpleLogObj;
end;

function LogTypeToString(const LogType: TLogType): string;
begin
  case LogType of
    ltError: Result := 'Error';
    ltWarning: Result := 'Warning';
    ltHint: Result := 'Hint';
    ltInfo: Result := 'Info';
  end;
end;

function GetModuleFileName: string;
var
  ModuleFileName: PChar;
begin
  GetMem(ModuleFileName, MAX_PATH + 1);
  try
    Windows.GetModuleFileName(hInstance, ModuleFileName, MAX_PATH);
    Result := ExtractFileName(StrPas(ModuleFileName));
  finally
    FreeMem(ModuleFileName);
  end;
end;

function GetUserName: string;
var
  Buffer: PChar;
  BufferSize: DWORD;
begin
  GetMem(Buffer, 128);
  try
    BufferSize := 128;
    Windows.GetUserName(Buffer, BufferSize);
    Result := string(Buffer);
  finally
    FreeMem(Buffer, 128);
  end;
end;

function GetComputerName: string;
var
  Buffer: PChar;
  BufferSize: DWORD;
begin
  GetMem(Buffer, 128);
  try
    BufferSize := 128;
    Windows.GetComputerName(Buffer, BufferSize);
    Result := string(Buffer);
  finally
    FreeMem(Buffer, 128);
  end;
end;

{ TSimpleLog }

constructor TSimpleLog.Create;
begin
  FLogList := TObjectList.Create(True);
end;

destructor TSimpleLog.Destroy;
begin
  FreeAndNil(FLogList);

  inherited;
end;

procedure TSimpleLog.LogError(const LogName, Event: string);
begin
  LogEvent(LogName, Event, ltError);
end;

procedure TSimpleLog.LogEvent(const LogName, Event: string; const LogType: TLogType);
var
  I: Integer;
  LI: TLogInstance;
begin
  for I := 0 to FLogList.Count - 1 do
  begin
    LI := TLogInstance(FLogList[I]);

    if LI.LogName = LogName then
    begin
      LI.InitializeThreadCheck(FLockType);
      try
        LI.LogEvent(Event, LogType);
        Exit;
      finally
        LI.FinalizeThreadCheck(FLockType);
      end;
    end;
  end;

  // there is no such log registered. raise exception.
  raise Exception.CreateFmt('Log "%s" is not registered', [LogName]);
end;

procedure TSimpleLog.LogHint(const LogName, Event: string);
begin
  LogEvent(LogName, Event, ltHint);
end;

procedure TSimpleLog.LogInfo(const LogName, Event: string);
begin
  LogEvent(LogName, Event, ltInfo);
end;

procedure TSimpleLog.LogWarning(const LogName, Event: string);
begin
  LogEvent(LogName, Event, ltWarning);
end;

function TSimpleLog.RegisterLog(const LogName, FileName: string;
                                const SizeLimit: Cardinal = cDefaultSize;
                                const MaxFiles: Integer = cDefaultMaxFiles;
                                const LogProperties: TLogProperties = clpDefault;
                                const LogDelimiter: string = ' | '): TLogInstance;
var
  I: Integer;
begin
  for I := 0 to FLogList.Count - 1 do
  begin
    if TLogInstance(FLogList[I]).LogName = LogName then
    begin
      Result := TLogInstance(FLogList[I]);
      Result.FileName := FileName;
      Exit;
    end;
  end;

  // if we came this far log is not present
  Result := TLogInstance.Create(LogName, FileName);
  Result.LogProperties := LogProperties;
  Result.LogDelimiter := LogDelimiter;
  Result.SizeLimit := SizeLimit;
  Result.MaxFiles := MaxFiles;
  FLogList.Add(Result)
end;

procedure TSimpleLog.UnRegisterLog(const LogName: string);
var
  I: Integer;
begin
  for I := 0 to FLogList.Count - 1 do
  begin
    if TLogInstance(FLogList[I]).LogName = LogName then
    begin
      FLogList.Delete(I);
      Exit;
    end;
  end;
end;

{ TLogObject }

procedure TLogInstance.CheckForMaxLogFiles;
var
  I: Integer;
  LogFiles: TStringList;
  SearchRec: TSearchRec;
  LogFileName: string;
  LogFilePath: string;
  LogFileExt: string;
begin
  if FMaxFiles > 0 then
  begin
    LogFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(FFileName));
    LogFileExt := ExtractFileExt(FFileName);

    LogFiles := TStringList.Create;
    try
      if FindFirst(Format('%s*%s', [LogFilePath, LogFileExt]), 0, SearchRec) = 0 then
      begin
        LogFileName := ExtractFileName(FFileName);
        LogFileName := Copy(LogFileName, 1, Pos('.', LogFileName) - 1);

        repeat
          if Pos(LogFileName + '_', SearchRec.Name) = 1 then
            LogFiles.Add(SearchRec.Name);
        until FindNext(SearchRec) <> 0;

        FindClose(SearchRec);
        LogFiles.Sort;
      end;

      if LogFiles.Count > FMaxFiles then
      begin
        for I := (LogFiles.Count - FMaxFiles) - 1 downto 0 do
          DeleteFile(LogFilePath + LogFiles[I]);
      end;
    finally
      LogFiles.Free;
    end;
  end;
end;

procedure TLogInstance.CheckStreamLimit;
var
  ArchiveName: string;
  TimeStamp: string;
  FilePath: string;
  FileExt: string;

begin
  if (FSizeLimit > 0) and ((FFileStream.Size / 1024) > FSizeLimit) then
  begin
    FreeAndNil(FFileStream);

    // get the current timestamp as string 
    TimeStamp := Format('%.4d%.2d%.2dT%.2d%.2d%.2d', [YearOf(Now),
                                                      MonthOf(Now),
                                                      DayOf(Now),
                                                      HourOf(Now),
                                                      MinuteOf(Now),
                                                      SecondOf(Now)]);

    ArchiveName := ExtractFileName(FFileName);
    FilePath := ExtractFilePath(FFileName);
    FileExt := ExtractFileExt(ArchiveName);

    // construct the arhive file name from filename and timestamp
    ArchiveName := Format('%s_%s%s', [StrBefore(FileExt, ArchiveName), TimeStamp, FileExt]);
    // rename the old file to an archived one
    RenameFile(FFileName, FilePath + ArchiveName);
    // chack if we are over the max file limit
    CheckForMaxLogFiles;

    // reinitialize stream
    InitializeStreaming;
  end;
end;

constructor TLogInstance.Create(const LogName, FileName: string);
begin
  InitializeCriticalSection(FLogCRSection);

  FMutexName := StringReplace(FileName, '\', '', [rfReplaceAll]);
  FComputerName := GetComputerName;
  FModuleName := GetModuleFileName;
  FModulePID := GetCurrentProcessId;
  FUserName := GetUserName;
  FFileName := FileName;
  FLogName := LogName;
end;

function TLogInstance.CreateMutexOrWait: Cardinal;
begin
  Result := CreateMutex(nil, False, PChar(Format('CSL_MUTEX_%s', [FMutexName])));
  WaitForSingleObject(Result, INFINITE);
end;

destructor TLogInstance.Destroy;
begin
  FreeAndNil(FFileStream);
  // release the critical section
  DeleteCriticalSection(FLogCRSection);

  inherited;
end;

procedure TLogInstance.FinalizeStreaming;
begin
  FreeAndNil(FFileStream);
end;

procedure TLogInstance.FinalizeThreadCheck(const LockType: TLogLockType);
begin
  case LockType of
    ltNone: ; // do nothing
    ltProcess: LeaveCriticalSection(FLogCRSection);
    ltMachine:
      begin
        ReleaseMutex(FLogMutex);
        CloseHandle(FLogMutex);
      end;
  end;
end;

procedure TLogInstance.InitializeStreaming;
begin
  ForceDirectories(ExtractFilePath(FFileName));

  // create the stream to make a file
  if not FileExists(FFileName) then
  begin
    FFileStream := TFileStream.Create(FFileName, fmCreate or fmShareExclusive);
    FreeAndNil(FFileStream);
  end;

  // open the file in readwrite mode for actual writing of log content
  FFileStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
  FFileStream.Position := FFileStream.Size;
end;

procedure TLogInstance.InitializeThreadCheck(const LockType: TLogLockType);
begin
  case LockType of
    ltNone: ; // do nothing
    ltProcess: EnterCriticalSection(FLogCRSection);
    ltMachine: FLogMutex := CreateMutexOrWait;
  end;
end;

procedure TLogInstance.LogEvent(const Event: string; const LogType: TLogType);
begin
  InitializeStreaming;
  try
    CheckStreamLimit;

    if lpTimestamp in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%s%s',
        [FormatDateTime('dd/mm/yyyy hh:nn:ss:zzz', Now), FLogDelimiter]));

    if lpType in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%s%s', [LogTypeToString(LogType), FLogDelimiter]));

    if lpComputername in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%s%s', [FComputerName, FLogDelimiter]));

    if lpUsername in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%s%s', [FUserName, FLogDelimiter]));

    if lpPID in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%d%s', [FModulePID, FLogDelimiter]));

    if lpTID in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%d%s', [GetCurrentThreadId, FLogDelimiter]));

    if lpModuleName in FLogProperties then
      WriteToStreamAsUTF8(FFileStream, Format('%s%s', [FModuleName, FLogDelimiter]));

    // always write the event message to the log
    WriteToStreamAsUTF8(FFileStream, Format('%s%s', [Event, #13#10]));
  finally
    FinalizeStreaming;
  end;
end;

procedure TLogInstance.SetFileName(const Value: string);
begin
  FMutexName := StringReplace(Value, '\', '', [rfReplaceAll]);
  FFileName := Value;
end;

initialization
  // nothing to do here
  
finalization
  FreeAndNil(SimpleLogObj);

end.
