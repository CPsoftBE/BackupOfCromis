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
 * ==================================================================================
 * A simple long interval scheduler. Good for scheduling tasks over long
 * periods of time. Low resolution but almost no resource usage.
 * ==================================================================================
 * 22/7/2006 (1.0.0)
 *   - Initial implementation.
 * 1/8/2006 (1.0.1)
 *   - Added progresive adjustment of resolution (period interval)
 *   - Adjusted code for linux (Kylix) - build "yes", work "no"
 * 15/8/2006 (1.0.2)
 *   - Added mutex for each schedule. Thas allows freeing threads on app
 *     termination.
 * 24/9/2006 (1.0.3)
 *   - Improved scheduling code to be more precise
 * 7/11/2007 (1.0.4)
 *   - Abandoned linux (Kylix) support because it is not worth the work
 * 25/2/2008 (1.1.0)
 *   - added support for diferent types of events The types are:
 *     I. stThreaded: event is run in the context of the scheduling thread
 *     II. stSynchronize: event is run in the context of the main thread
 *     III. stMessage: event is run in the context of the thread that created it
 * 27/2/2009 (2.0.0)
 *   - added support for chron type of events and schedules.
 *     I. for rules see: http://www.nncron.ru/help/EN/working/cron-format.htm
 *     II. not all variants of cron are supported yet, but most are. see bellow
 *   - periodic behaviour of old scheduler was moved to cron format
 *   - event can be paused
 * 10/10/2009 (2.0.1)
 *   - no need to register the WM_ON_SCHEDULE message on initialization
 *   - SetInterval now checks for limits of each value
 *   - RAD 2010 thread changes implemented (resume)
 * 18/01/2010 (2.0.2)
 *   - fixed a bug in valid interval calculation
 * 27/10/2010 (2.0.3)
 *   - added HasValue dunction for TChronEntry
 * 18/03/2012 (2.1.0)
 *   - 64 bit compiler compatible
 * 13/05/2012 (2.1.1)
 *   - Fixed a bug in calculation algorithm
 * 20/06/2012 (2.1.2)
 *   - Fixed a bug when deleting a schedule from the list (it was not removed)
 *     Thanks to Daniel for pointing that out.
 * 17/12/2012 (2.2.0)
 *   - Fixed a bug with day of the week not having a correct impact on month
 *   - raise exception if the schedule is not in the correct format (a start)
 *   - made FPC compatible (initial implementation)
 * 21/12/2012 (2.2.1)
 *   - use QueryPerformanceCounter instead of GetTickCount (thanks Roberto Neto)
 *   - code adaptation for compatibility with Delphi 7
 * 21/12/2012 (2.2.2)
 *   - ExecuteLimit added. Allows the event to be executed only N times
 * 21/12/2012 (2.2.3)
 *   - Improved cron format support. Now "1,3,2,5-9,15-20/2,5,1,18-25/3" gets parsed correctly
 *   - Sort numbers on the value list to get correct sequence
 *   - Eliminate duplicate values
 *   - Additional format checks
 * 21/12/2012 (2.2.4)
 *   - Terminate event is now created only when schedule is run
 * 29/07/2013 (2.2.5)
 *   - LastEvent property for schedule
 * 30/07/2013 (2.2.6)
 *   - OnScheduleAdd and OnScheduleRemove handlers added to events list
 * 02/08/2013 (2.2.7)
 *   - OnScheduleRemove must cover also inherited Remove and Delete
 * 06/08/2013 (2.2.8)
 *   - stQueue added as signal type
 *   - In schedule destructor call stop
 *   - Stop the schedule thread in a nicer way
 * 28/01/2014 (2.2.9)
 *   - If RunAtOnce is specified and ValidFrom is valid date then execute then
 * ==================================================================================
*)

unit Cromis.Scheduler;

{$IFDEF FPC}
   {$MODE DELPHI}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ELSE}
  LMessages, LCLType, LCLIntf,
{$ENDIF}
  SysUtils, Classes, DateUtils, Math, Contnrs,

  // cromis units
  Cromis.StringUtils{$IFDEF MSWINDOWS},Cromis.Utils{$ENDIF};

const
  MIN_RESOLUTION = 50;
  MAX_RESOLUTION = 86400000;

  // chron constants
  CHRON_Second = 0;
  CHRON_Minute = 1;
  CHRON_Hour = 2;
  CHRON_DayInMonth = 3;
  CHRON_Month = 4;
  CHRON_DayInWeek = 5;
  CHRON_Year = 6;

  // number of fields
  CHRON_MaxField = 6;

const
  cShutdownTimeout = 3000;

const
  WM_ON_SCHEDULE = WM_USER + 1;

type
  TSchEventList = class;
  TScheduledEvent = class;
  TCPHandle = {$IFDEF MSWINDOWS}THandle{$ELSE}PRTLEvent{$ENDIF};
  TSchListSchEvent = procedure(Sender: TSchEventList; ScheduledEvent: TScheduledEvent) of Object;
  TSchNotifyEvent = procedure(Sender: TScheduledEvent) of object;
  TScheduleStatus = (ssIdleWait, ssTriggered, ssInvalid, ssAbort);
  TSignalType = (stThreaded, stSynchronize, stMessage, stQueue);

  TChronEntry = record
    MinValue: Integer;
    MaxValue: Integer;
    Interval: Integer;
    Relative: Boolean;
    Values: array of Integer;
  end;

 TChronEntryHelper = class
    class procedure SetMaxValue(var ChronEntry: TChronEntry; const Value: Integer);
    class function GetNextValue(var ChronEntry: TChronEntry; const Current: Integer): Integer;
    class function GetFirstValue(var ChronEntry: TChronEntry; const Current: Integer): Integer;
    class function GetValue(var ChronEntry: TChronEntry; const Current, Interval: Integer): Integer;
    class function HasValue(var ChronEntry: TChronEntry; const Value: Integer): Boolean;
  end;

  // chron representation of schedules
  TChronSchedule = array [0..CHRON_MaxField] of TChronEntry;

  TWorkerThread = class(TThread)
  private
    FIntSchedule: TScheduledEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const IntSchedule: TScheduledEvent);
  end;

  // main schedule class
  TSchedule = class
  strict private
    FEventPlan: string;
    FValidTo: TDateTime;
    FNextEvent: TDateTime;
    FLastEvent: TDateTime;
    FValidFrom: TDateTime;
    FChronSchedule: TChronSchedule;
    FScheduleStatus: TScheduleStatus;
    procedure SetEventPlan(const Value: string);
    function EncodeNextEvent: Boolean;
  private
  public
    constructor Create;
    function ResetSchedule: Boolean;
    procedure CheckSchedule(var RunAtOnce: Boolean);
    property NextEvent: TDateTime read FNextEvent;
    property LastEvent: TDateTime read FLastEvent;
    property ValidTo: TDateTime read FValidTo write FValidTo;
    property ValidFrom: TDateTime read FValidFrom write FValidFrom;
    property EventPlan: string read FEventPlan write SetEventPlan;
    property ScheduleStatus: TScheduleStatus read FScheduleStatus;
    procedure SetInterval(const Seconds, Minutes, Hours, Days, Months, Years: Integer);
  end;

  // the schedule thread that acts as a timer
  TScheduleThread = class(TThread)
  strict private
    { Private declarations }
    FHWND: HWND;
    FTermEvent: TCPHandle;
    FRunAtOnce: Boolean;
    FResolution: Int64;
    FIntSchedule: TScheduledEvent;
    procedure CallEventHandlerFunction;
  protected
    procedure Execute; override;
  public
    constructor Create(const IntSchedule: TScheduledEvent;
                       const WindowHWND: HWND;
                       const TermEvent: TCPHandle;
                       const RunAtOnce: Boolean);
  end;

  // the event object encapsulating TSchedule
  TScheduledEvent = class
  strict private
    FName: string;
    FPaused: Boolean;
    FRunning: Boolean;
    FSchedule: TSchedule;
    FTermEvent: TCPHandle;
    FWinHandle: HWND;
    FSchThread: TScheduleThread;
    FRestarting: Boolean;
    FSignalType: TSignalType;
    FExecuteCount: Int64;
    FExecuteLimit: Int64;
    FOnScheduleRun: TSchNotifyEvent;
    FOnScheduleStop: TSchNotifyEvent;
    FOnScheduleAbort: TSchNotifyEvent;
    FOnScheduleEvent: TSchNotifyEvent;
    FOnScheduleInvalid: TSchNotifyEvent;
    FOnMaxEventsExecuted: TSchNotifyEvent;
  {$IFDEF MSWINDOWS}
    procedure WndProc(var msg: TMessage);
  {$ENDIF}
    procedure OnScheduleTerminated(Sender: TObject);
  private
    procedure SetSignalType(const Value: TSignalType);
  public
    constructor Create(Name: string);
    destructor Destroy; override;
    function Run(const RunAtOnce: Boolean = False): Boolean;
    procedure ExecuteEvent;
    procedure Restart;
    procedure Resume;
    procedure Pause;
    procedure Stop;
    property Name: string read FName;
    property Running: Boolean read FRunning;
    property Schedule: TSchedule read FSchedule;
    property Restarting: Boolean read FRestarting;
    property SignalType: TSignalType read FSignalType write SetSignalType;
    property ExecuteLimit: Int64 read FExecuteLimit write FExecuteLimit;
    property OnScheduleRun: TSchNotifyEvent read FOnScheduleRun write FOnScheduleRun;
    property OnScheduleStop: TSchNotifyEvent read FOnScheduleStop write FOnScheduleStop;
    property OnScheduleEvent: TSchNotifyEvent read FOnScheduleEvent write FOnScheduleEvent;
    property OnScheduleAbort: TSchNotifyEvent read FOnScheduleAbort write FOnScheduleAbort;
    property OnScheduleInvalid: TSchNotifyEvent read FOnScheduleInvalid write FOnScheduleInvalid;
    property OnMaxEventsExecuted: TSchNotifyEvent read FOnMaxEventsExecuted write FOnMaxEventsExecuted;
  end;

  // list of scheduled events
  TSchEventList = class(TObjectList)
  strict private
    FOnScheduleAdd: TSchListSchEvent;
    FOnScheduleRemove: TSchListSchEvent;
    function GetItem(Index : Integer) : TScheduledEvent;
    procedure SetItem(Index: Integer; Value: TScheduledEvent);
  public
    function GetFirstScheduled: TScheduledEvent;
    function FindSchEvent(Name: string): TScheduledEvent;
    function Add(EventName: string): TScheduledEvent; overload;
    function Delete(EventName: string): Boolean; overload;
    function Remove(AObject: TObject): Integer; reintroduce;
    procedure Delete(Index: Integer) overload; reintroduce;
    property Items[Index: Integer]: TScheduledEvent read GetItem write SetItem; default;
    property OnScheduleAdd: TSchListSchEvent read FOnScheduleAdd write FOnScheduleAdd;
    property OnScheduleRemove: TSchListSchEvent read FOnScheduleRemove write FOnScheduleRemove;
  end;

  // chron format schedule calculation and parsing functions
  function FindNextScheduleDate(const BaseDate: TDateTime;
                                var ChronSchedule: TChronSchedule;
                                const ValidFrom: TDateTime = 0;
                                const ValidTo: TDateTime = 0
                                ): TDateTime;

  procedure ParseScheduleEventPlan(const Schedule: string; var ChronSchedule: TChronSchedule);

implementation

function FindNextScheduleDate(const BaseDate: TDateTime;
                              var ChronSchedule: TChronSchedule;
                              const ValidFrom, ValidTo: TDateTime
                              ): TDateTime;
var
  Previous: array [0..CHRON_MaxField] of Integer;
  Current: array [0..CHRON_MaxField] of Integer;

  ResetIndex: Integer;
  ResetToMin: Boolean;
  StartDate: TDateTime;
  TempDate: TDateTime;
  K, J: Integer;

  function IsSchedulePartValid(const Position: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;

    if Current[Position] >= Previous[Position] then
    begin
      Result := True;
      Exit;
    end;

    for I := CHRON_MaxField downto 0 do
    begin
      if (Position < I) and (Current[I] > Previous[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  procedure ResetSubsetDateValues(var Index: Integer);
  var
    I: Integer;
  begin
    for I := 0 to CHRON_MaxField do
    begin
      if (Index > I) and not ChronSchedule[I].Relative then
      begin
        Current[I] := ChronSchedule[I].MinValue;
        Previous[I] := Current[I];
      end;
    end;

    // reset it
    Index := 0;
  end;

  function CheckLoopBreakCondition(const CronPart: Integer): Boolean;
  begin
    Result := False;

    if Previous[CronPart] = Current[CronPart] then
    begin
      Result := True;
      Exit;
    end;

    if Previous[CronPart]= ChronSchedule[CronPart].MaxValue then
      if  Current[CronPart] < Previous[CronPart] then
        Result := True;
  end;

begin
  StartDate := Max(ValidFrom, BaseDate);
  ResetToMin := False;
  ResetIndex := 0;
  Result := 0;

  if (ValidTo > 0) and (StartDate > ValidTo) then
    Exit;

  Previous[CHRON_Second] := SecondOf(StartDate);
  Previous[CHRON_Minute] := MinuteOf(StartDate);
  Previous[CHRON_Hour] := HourOf(StartDate);
  Previous[CHRON_DayInMonth] := DayOf(StartDate);
  Previous[CHRON_Month] := MonthOf(StartDate);
  Previous[CHRON_DayInWeek] := DayOfTheWeek(StartDate);
  Previous[CHRON_Year] := YearOf(StartDate);

  // set variable max value for days in moth as they vary
  ChronSchedule[CHRON_DayInMonth].MaxValue := DaysInMonth(StartDate);
  // calculate the first values
  for K := CHRON_MaxField downto 0 do
  begin
    case ResetToMin and not ChronSchedule[K].Relative of
      False: Current[K] := TChronEntryHelper.GetFirstValue(ChronSchedule[K], Previous[K]);
      True: Current[K] := ChronSchedule[K].MinValue;
    end;

    J := K;
    while ChronSchedule[J].Relative and (Current[J] < Previous[J]) and (J < CHRON_MaxField) do
    begin
      Inc(J);
      Current[J] := (Current[J] + 1) mod (ChronSchedule[J].MaxValue + 1);
      ChronSchedule[CHRON_DayInMonth].MaxValue := DaysInAMonth(Current[CHRON_Year],
                                                               Current[CHRON_Month]);
    end;

    if K <> CHRON_DayInWeek then
      ResetToMin := ResetToMin or (Current[K] > Previous[K]);
  end;

  // no valid year was found, finish
  while IsSchedulePartValid(CHRON_Year) do
  begin
    ResetSubsetDateValues(ResetIndex);

    while IsSchedulePartValid(CHRON_Month) do
    begin
      ResetSubsetDateValues(ResetIndex);

      while IsSchedulePartValid(CHRON_DayInMonth) do
      begin
        if IsValidDate(Current[CHRON_Year],
                       Current[CHRON_Month],
                       Current[CHRON_DayInMonth]) then
        begin
          TempDate := EncodeDate(Current[CHRON_Year], Current[CHRON_Month], Current[CHRON_DayInMonth]);
          Current[CHRON_DayInWeek] := TChronEntryHelper.GetFirstValue(ChronSchedule[CHRON_DayInWeek], DayOfTheWeek(TempDate));

          // is this the correct day of the week
          if DayOfTheWeek(TempDate) = Current[CHRON_DayInWeek] then
          begin
            // we found the correct day, now find the hours minutes and seconds
            ResetSubsetDateValues(ResetIndex);

            while IsSchedulePartValid(CHRON_Hour) do
            begin
              ResetSubsetDateValues(ResetIndex);

              while IsSchedulePartValid(CHRON_Minute) do
              begin
                ResetSubsetDateValues(ResetIndex);

                while IsSchedulePartValid(CHRON_Second) do
                begin
                  TempDate := EncodeDateTime(Current[CHRON_Year],
                                             Current[CHRON_Month],
                                             Current[CHRON_DayInMonth],
                                             Current[CHRON_Hour],
                                             Current[CHRON_Minute],
                                             Current[CHRON_Second],
                                             0);

                  if RecodeMilliSecond(StartDate, 0) <> TempDate then
                  begin
                    if (ValidFrom = 0) or (TempDate > ValidFrom) then
                    begin
                      if (ValidTo > 0) and (TempDate > ValidTo) then
                        Result := 0
                      else
                        Result := TempDate;

                      Exit;
                    end;
                  end;

                  // get next possible second
                  ResetIndex := CHRON_Second;
                  Previous[CHRON_Second] := Current[CHRON_Second];
                  Current[CHRON_Second] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_Second],
                                                                          Previous[CHRON_Second]);


                  // break if there was no change
                  if CheckLoopBreakCondition(CHRON_Second) then
                    Break;
                end;

                // get next possible minute
                ResetIndex := CHRON_Minute;
                Previous[CHRON_Minute] := Current[CHRON_Minute];
                Current[CHRON_Minute] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_Minute],
                                                                        Previous[CHRON_Minute]);
                Previous[CHRON_Second] := Current[CHRON_Second];

                // break if there was no change
                if CheckLoopBreakCondition(CHRON_Minute) then
                  Break;
              end;

              // get next possible hour
              ResetIndex := CHRON_Hour;
              Previous[CHRON_Hour] := Current[CHRON_Hour];
              Current[CHRON_Hour] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_Hour],
                                                                    Previous[CHRON_Hour]);
              Previous[CHRON_Minute] := Current[CHRON_Minute];

              // break if there was no change
              if CheckLoopBreakCondition(CHRON_Hour) then
                Break;
            end;
          end;
        end;

        // get next possible day
        ResetIndex := CHRON_DayInMonth;
        Previous[CHRON_DayInMonth] := Current[CHRON_DayInMonth];
        ChronSchedule[CHRON_DayInMonth].MaxValue := DaysInAMonth(Current[CHRON_Year], Current[CHRON_Month]);
        Current[CHRON_DayInMonth] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_DayInMonth],
                                                                    Previous[CHRON_DayInMonth]);
        Previous[CHRON_Hour] := Current[CHRON_Hour];

        // break if there was no change
        if CheckLoopBreakCondition(CHRON_DayInMonth) then
          Break;
      end;

      // get next possible month
      ResetIndex := CHRON_Month;
      Previous[CHRON_Month] := Current[CHRON_Month];
      Current[CHRON_Month] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_Month],
                                                             Previous[CHRON_Month]);
      Previous[CHRON_DayInMonth] := Current[CHRON_DayInMonth];

      // break if there was no change
      if CheckLoopBreakCondition(CHRON_Month) then
        Break;
    end;

    // get next possible year
    ResetIndex := CHRON_Year;
    Previous[CHRON_Year] := Current[CHRON_Year];
    Current[CHRON_Year] := TChronEntryHelper.GetNextValue(ChronSchedule[CHRON_Year],
                                                          Previous[CHRON_Year]);
    Previous[CHRON_Month] := Current[CHRON_Month];

    // break if there was no change
    if CheckLoopBreakCondition(CHRON_Year) then
      Break;
  end;
end;

function SetRepeatingInterval(const Min, Max, Interval: Integer): TChronEntry;
begin
  Result.Interval := Interval;
  SetLength(Result.Values, 0);
  Result.MinValue := Min;
  Result.MaxValue := Max;
end;

function SetIntervalValues(const Min, Max, Interval: Integer; const Values: TStringList): TChronEntry;
var
  I: Integer;
begin
  SetLength(Result.Values, Values.Count);
  Result.Interval := Interval;
  Result.MinValue := Max;
  Result.MaxValue := Min;

  for I := 0 to Values.Count - 1 do
  begin
    Result.Values[I] := StrToInt(Values[I]);

    if Result.Values[I] < Result.MinValue then
      Result.MinValue := Result.Values[I];
    if Result.Values[I] > Result.MaxValue then
      Result.MaxValue := Result.Values[I];
  end;
end;

function SortValuesArray(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
end;

// single schedule part formating routine
function ParseSchedulePart(const Min, Max: Integer; const Schedule: string): TChronEntry;
var
  I,K: Integer;
  Values: string;
  MinVal: Integer;
  MaxVal: Integer;
  Interval: Integer;
  ValuesList: TStringList;
  ValuesArray: TStringList;
begin
  // start normal procedure
  Result.Relative := False;
  Values := Schedule;
  Interval := 0;

  if (Pos('/', Values) > 0) and (Pos(',', Values) = 0) then
  begin
    Interval := StrToInt(StrAfter('/', Values));
    Values := StrBefore('/', Values);
  end;

  // parse the seconds values
  if Pos('*', Values) > 0 then
    Result := SetRepeatingInterval(Min, Max, Math.Max(1, Interval))
  else if Pos('?', Values) > 0 then
  begin
    Result := SetRepeatingInterval(Min, Max, Interval);
    Result.Relative := True;
  end
  else if (Pos('-', Values) > 0) and (Pos(',', Values) = 0) then
  begin
    MaxVal := StrToInt(StrAfter('-', Values));
    MinVal := StrToInt(StrBefore('-', Values));
    Result := SetRepeatingInterval(MinVal, MaxVal, Math.Max(1, Interval));
  end
  else
  begin
    ValuesList := TStringList.Create;
    try
      ValuesList.StrictDelimiter := True;
      ValuesList.Delimiter := ',';

      ValuesArray := TStringList.Create;
      try
        // get the values as delimited text
        ValuesArray.CaseSensitive := False;
        ValuesList.DelimitedText := Values;

        for I := 0 to ValuesList.Count - 1 do
        begin
          if Pos('-', ValuesList[I]) > 0 then
          begin
            Interval := 1;

            if Pos('/', ValuesList[I]) > 0 then
            begin
              Interval := StrToInt(StrAfter('/', ValuesList[I]));
              Values := StrBefore('/', ValuesList[I]);
            end
            else
              Values := ValuesList[I];

            // get the min and max of the interval
            MinVal := StrToInt(StrBefore('-', Values));
            MaxVal := StrToInt(StrAfter('-', Values));
            K := MinVal;

            while K <= MaxVal do
            begin
              if ValuesArray.IndexOf(IntToStr(K)) = -1 then
                ValuesArray.Add(IntToStr(K));
              Inc(K, Interval);
            end;
          end
          else
            if ValuesArray.IndexOf(ValuesList[I]) = -1 then
              ValuesArray.Add(ValuesList[I]);
        end;

        // sort the array by number values
        ValuesArray.CustomSort(SortValuesArray);
        // set the interval values parse out from the values list
        Result := SetIntervalValues(Min, Max, Interval, ValuesArray);
      finally
        ValuesArray.Free;
      end;
    finally
      ValuesList.Free;
    end;
  end;
end;

procedure ParseScheduleEventPlan(const Schedule: string; var ChronSchedule: TChronSchedule);
var
  I: Integer;
  ScheduleParts: TStringList;
  IdleRelativeCount: Integer;
begin
  ScheduleParts := TStringList.Create;
  try
    ScheduleParts.StrictDelimiter := True;
    ScheduleParts.Delimiter := #32;

    // parse the delimited schedule value
    ScheduleParts.DelimitedText := Schedule;
    IdleRelativeCount := 0;

    if ScheduleParts.Count < CHRON_MaxField + 1 then
      for I := ScheduleParts.Count to CHRON_MaxField do
        ScheduleParts.Add('*');

    // parse all parts of the schedule
    ChronSchedule[CHRON_Second] := ParseSchedulePart(0, 59, ScheduleParts[CHRON_Second]);
    ChronSchedule[CHRON_Minute] := ParseSchedulePart(0, 59, ScheduleParts[CHRON_Minute]);
    ChronSchedule[CHRON_Hour] := ParseSchedulePart(0, 23, ScheduleParts[CHRON_Hour]);
    ChronSchedule[CHRON_DayInMonth] := ParseSchedulePart(1, 31, ScheduleParts[CHRON_DayInMonth]);
    ChronSchedule[CHRON_Month] := ParseSchedulePart(1, 12, ScheduleParts[CHRON_Month]);
    ChronSchedule[CHRON_DayInWeek] := ParseSchedulePart(1, 7, ScheduleParts[CHRON_DayInWeek]);
    ChronSchedule[CHRON_Year] := ParseSchedulePart(1900, 3000, ScheduleParts[CHRON_Year]);

    for I := 0 to CHRON_MaxField do
      if ChronSchedule[I].Relative and (ChronSchedule[I].Interval = 0) then
        Inc(IdleRelativeCount);

    if IdleRelativeCount = (CHRON_MaxField + 1) then
      raise Exception.Create('Schedule is not in valid CRON format');
  finally
    ScheduleParts.Free;
  end;
end;

{ TIntervalMeter }

type
  TIntervalMeter = class
  private
    FStart: Int64;
    FStartFrac: TDateTime;
    FFreq: Int64;
    FHighFreq: boolean;
    function getElapsed: Int64;
  public
    constructor Create;
    procedure Reset;

    property Elapsed: Int64 read getElapsed;
  end;

{ TScheduleThread }

procedure TScheduleThread.CallEventHandlerFunction;
begin
  FIntSchedule.ExecuteEvent;
end;

constructor TScheduleThread.Create(const IntSchedule: TScheduledEvent;
                                   const WindowHWND: HWND;
                                   const TermEvent: TCPHandle;
                                   const RunAtOnce: Boolean);
begin
  // set initial resolution based on interval length between now and schedule
  FResolution := MilliSecondsBetween(Now, IntSchedule.Schedule.NextEvent);
  // assign schedule to thread
  FIntSchedule := IntSchedule;
  // set if run at once
  FRunAtOnce := RunAtOnce;
  // set the term. event
  FTermEvent := TermEvent;
  // set the window
  FHWND := WindowHWND;

  // check if run at once and ValidFrom is specified
  if RunAtOnce and (IntSchedule.Schedule.ValidFrom > Now) then
  begin
    IntSchedule.Schedule.CheckSchedule(FRunAtOnce);
    FResolution := MilliSecondsBetween(Now, IntSchedule.Schedule.NextEvent);
  end;

  inherited Create(False);
end;

procedure TScheduleThread.Execute;
var
  IntervalMeter: TIntervalMeter;
  SleepInterval: Int64;
{$IFDEF MSWINDOWS}
   WaitStatus: Cardinal;
{$ELSE}
   WaitStart: TDateTime;
{$ENDIF}
begin
  while not Terminated do
  begin
    IntervalMeter := TIntervalMeter.Create;
    try
      IntervalMeter.Reset;
      // halve the resolution (progressive)
      FResolution := FResolution div 2;
      // check if schedule is already triggered
      FIntSchedule.Schedule.CheckSchedule(FRunAtOnce);

      // check if we have to abort
      if FIntSchedule.Schedule.ScheduleStatus = ssAbort then
        Exit;

      if (FIntSchedule.Schedule.ScheduleStatus in [ssTriggered, ssInvalid]) then
      begin
        if Assigned(FIntSchedule.OnScheduleEvent) then
        begin
          FResolution := MilliSecondsBetween(Now, FIntSchedule.Schedule.NextEvent) div 2;

          case FIntSchedule.SignalType of
            stSynchronize: Synchronize(CallEventHandlerFunction);
            stMessage: PostMessage(FHWND, WM_ON_SCHEDULE, 0, 0);
            stThreaded: TWorkerThread.Create(FIntSchedule);
            stQueue: Queue(CallEventHandlerFunction);
          end;

          // if invalid status stop the schedule
          if FIntSchedule.Schedule.ScheduleStatus = ssInvalid then
            Exit;
        end
        else
          raise Exception.Create('OnSchedule is not defined');
      end;
      
      // adjust the sleep interval
      SleepInterval := FResolution;
      if SleepInterval > MAX_RESOLUTION then
        SleepInterval := MAX_RESOLUTION
      else if SleepInterval < MIN_RESOLUTION then
        SleepInterval := MIN_RESOLUTION;
      // sleep for the interval period
      SleepInterval := SleepInterval - IntervalMeter.Elapsed;
    {$IFDEF MSWINDOWS}
      // start the wait. Finish waiting if the event is set or timeout reached
      WaitStatus := WaitForSingleObject(FTermEvent, Max(0, SleepInterval));
      // check what was the cause for signalization
      if WaitStatus <> WAIT_TIMEOUT then
      begin
        Terminate;
        Exit;
      end;
    {$ELSE}
      WaitStart := Now;
      RTLEventWaitFor(FTermEvent, Max(0, SleepInterval));
      // check what was the cause for signalization
      if MilliSecondsBetween(Now, WaitStart) < SleepInterval then
      begin
        Terminate;
        Exit;
      end;
    {$ENDIF}
    finally
      IntervalMeter.Free;
    end;
  end;
end;

{ TSchedule }

constructor TSchedule.Create;
begin
  FScheduleStatus := ssIdleWait;
  FNextEvent := Now;
  FValidFrom := 0;
  FValidTo := 0;
end;

function TSchedule.EncodeNextEvent: Boolean;
begin
  FLastEvent := FNextEvent;

  FNextEvent := FindNextScheduleDate(FNextEvent, FChronSchedule, FValidFrom, FValidTo);
  Result := FNextEvent <> 0;
end;

procedure TSchedule.CheckSchedule(var RunAtOnce: Boolean);
var
  NextValid: Boolean;
begin
  FScheduleStatus := ssIdleWait;

  if (FValidTo > 0) and (Now > FValidTo) then
  begin
    FScheduleStatus := ssAbort;
    Exit;
  end;

  if RunAtOnce then
  begin
    if FValidFrom > Now then
      FNextEvent := FValidFrom
    else
      FScheduleStatus := ssTriggered;

    RunAtOnce := False;
    Exit;
  end;

  if CompareDateTime(FNextEvent, Now) < 1 then
  begin
    NextValid := EncodeNextEvent;

    if not NextValid then
    begin
      FScheduleStatus := ssInvalid;
      Exit;
    end;

    if (FValidFrom = 0) or (Now >= FValidFrom) then
    begin
      FScheduleStatus := ssTriggered;
      Exit;
    end;
  end;
end;

function TSchedule.ResetSchedule: Boolean;
begin
  FNextEvent := Now;
  Result := EncodeNextEvent;
end;

procedure TSchedule.SetEventPlan(const Value: string);
begin
  if FEventPlan <> Value then
  begin
    ParseScheduleEventPlan(Value, FChronSchedule);
    FEventPlan := Value;
    ResetSchedule;
  end;
end;

procedure TSchedule.SetInterval(const Seconds, Minutes, Hours, Days, Months, Years: Integer);
var
  SectionChar: Char;

  procedure EncodeSingleValue(const Value, Max: Integer; var PlanAsStr: string);
  begin
    if Value > Max then
      raise Exception.CreateFmt('Value "%d" is above the max value "%d"', [Value, Max]);

    // years interval
    if Value > 0 then
    begin
      SectionChar := '?';
      PlanAsStr := Format('%s/%d %s', [SectionChar, Value, PlanAsStr]);
    end
    else
      PlanAsStr := Format('%s %s', [SectionChar, PlanAsStr]);
  end;

begin
  SectionChar := '*';
  FEventPlan := '';

  EncodeSingleValue(Years, 9999, FEventPlan);
  // every day in week
  FEventPlan := Format('* %s', [FEventPlan]);
  // the rest of the plan parts
  EncodeSingleValue(Months, 12, FEventPlan);
  EncodeSingleValue(Days, 31, FEventPlan);
  EncodeSingleValue(Hours, 24, FEventPlan);
  EncodeSingleValue(Minutes, 60, FEventPlan);
  EncodeSingleValue(Seconds, 60, FEventPlan);
  // trim any cron schedule spaces
  EventPlan := Trim(FEventPlan);
end;

{ TScheduledEvent }

constructor TScheduledEvent.Create(Name: string);
begin
  FName := Name;
  // set the signal type to message
  SignalType := {$IFDEF MSWINDOWS}stMessage{$ELSE}stThreaded{$ENDIF};
  // create the internal schedule
  FSchedule := TSchedule.Create;
end;

destructor TScheduledEvent.Destroy;
begin
  Stop;
  FreeAndNil(FSchedule);

  inherited;
end;

procedure TScheduledEvent.ExecuteEvent;
begin
  if ((FExecuteCount < FExecuteLimit) or (FExecuteLimit = 0)) and FRunning then
  begin
    Inc(FExecuteCount);

    if Assigned(FOnScheduleEvent) then
      FOnScheduleEvent(Self);

    if (FExecuteCount >= FExecuteLimit) and (FExecuteLimit > 0) then
    begin
      if Assigned(FOnMaxEventsExecuted) then
        FOnMaxEventsExecuted(Self);
      Stop;
    end;
  end;
end;

procedure TScheduledEvent.OnScheduleTerminated(Sender: TObject);
begin
  case FSchedule.ScheduleStatus of
    ssIdleWait: ; //do nothing
    ssTriggered: ; //do nothing
    ssInvalid:
      begin
        if Assigned(FOnScheduleInvalid) then
          FOnScheduleInvalid(Self);
      end;
    ssAbort:
      begin
        if Assigned(FOnScheduleAbort) then
          FOnScheduleAbort(Self);
      end;
  end;
end;

procedure TScheduledEvent.Pause;
begin
  FPaused := True;
end;

procedure TScheduledEvent.Restart;
begin
  FRestarting := True;
  try
    Stop;
    Run;
  finally
    FRestarting := False;
  end;
end;

procedure TScheduledEvent.Resume;
begin
  FPaused := False;
end;

function TScheduledEvent.Run(const RunAtOnce: Boolean = False): Boolean;
begin
  FRunning := False;
  // check if the shcedule is valid
  Result := FSchedule.ResetSchedule;

  if Result then
  begin
    {$IFDEF MSWINDOWS}
      if FSignalType = stMessage then
      begin
        // create the dummy window for messages
        FWinHandle := TSAllocateHWND(WndProc);

        if FWinHandle = 0 then
          raise Exception.Create('AllocateHWND failed');
      end;
    {$ENDIF}

    // we are running
    FRunning := True;
    // create the event needed to signal thread termination
    FTermEvent := {$IFDEF MSWINDOWS}CreateEvent(nil, False, False, nil){$ELSE} RTLEventCreate{$ENDIF};
    // create the timer thread and pass the correct parameters to it
    FSchThread := TScheduleThread.Create(Self, FWinHandle, FTermEvent, RunAtOnce);
    // set the on terminated event handler
    FSchThread.OnTerminate := OnScheduleTerminated;
    // set maximum priority class
    FSchThread.Priority := tpTimeCritical;

    // fire the on schedule run event
    if Assigned(FOnScheduleRun) then
      FOnScheduleRun(Self);
  end;
end;

procedure TScheduledEvent.SetSignalType(const Value: TSignalType);
begin
  if Running then
    raise Exception.Create('Cannot change signaling type while running');

  // set new signal type
  FSignalType := Value;
end;

procedure TScheduledEvent.Stop;
{$IFDEF MSWINDOWS}
var
  AResult: Cardinal;
{$ENDIF}
begin
  if FRunning then
  begin
    // dealocate the event handler
    FSchThread.OnTerminate := nil;
    FRunning := False;

    // set the terminate event, so that the thread can safely exit
    {$IFDEF FPC}RTLEventSetEvent{$ELSE}SetEvent{$ENDIF}(FTermEvent);

    {$IFNDEF FPC}
      // wait and block until the scheduling thread is finished
      AResult := WaitForSingleObject(FSchThread.Handle, cShutdownTimeout);

      // check if we timed out
      if AResult = WAIT_TIMEOUT then
        TerminateThread(FSchThread.Handle, 0);
    {$ELSE}
      // wait and block until the scheduling thread is finished
      RTLEventWaitFor(FSchThread, cShutdownTimeout);
      FSchThread.Terminate;
    {$ENDIF}

    // close the event handle and free the thread
    {$IFDEF FPC}RTLEventDestroy{$ELSE}CloseHandle{$ENDIF}(FTermEvent);
    FreeAndNil(FSchThread);

    {$IFDEF MSWINDOWS}
      //destroy the hidden window
      TSDeallocateHWnd(FWinHandle);
    {$ENDIF}

    // fire the on schedule stop event
    if Assigned(FOnScheduleStop) then
      FOnScheduleStop(Self);
  end;
end;

{$IFDEF MSWINDOWS}
procedure TScheduledEvent.WndProc(var msg: TMessage);
begin
  if Msg.Msg = WM_ON_SCHEDULE then
  begin
    {
     if the message id is WM_ON_SCHEDULE
     do our own processing
    }
    if Assigned(FOnScheduleEvent) and not FPaused then
      ExecuteEvent;
  end
  else
    {
     for all other messages call
     the default window procedure
    }
    Msg.Result := DefWindowProc(FWinHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;
{$ENDIF}

{ TSchEventList }

function TSchEventList.Add(EventName: string): TScheduledEvent;
begin
  Result := TScheduledEvent.Create(EventName);
  Add(Result);

  if Assigned(FOnScheduleAdd) then
    FOnScheduleAdd(Self, Result);
end;

function TSchEventList.Delete(EventName: string): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to Count - 1 do
  begin
    if Items[I].Name = EventName then
    begin
      if Assigned(FOnScheduleRemove) then
        FOnScheduleRemove(Self, Items[I]);

      // remove the item
      Remove(Items[I]);
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSchEventList.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < Count) then
  begin
    if Assigned(FOnScheduleRemove) then
      FOnScheduleRemove(Self, Items[Index]);

      inherited Delete(Index);
  end;
end;

function TSchEventList.FindSchEvent(Name: string): TScheduledEvent;
var
  I: Integer;
begin
  Result := nil;
  
  for I := 0 to Count - 1 do
  begin
    if Items[I].Name = Name then
    begin
      Result := Items[I];
      Exit;
    end;  
  end;
end;

function TSchEventList.GetFirstScheduled: TScheduledEvent;
var
  I: Integer;
begin
  if Count = 0 then
    Result := nil
  else
  begin
    Result := Items[0];
    for I := 1 to Count - 1 do
      if CompareDateTime(Items[I].Schedule.NextEvent, Result.Schedule.NextEvent) = 1 then
        Result := Items[I];
  end;
end;

function TSchEventList.GetItem(Index: Integer): TScheduledEvent;
begin
  Result := (inherited GetItem(Index)) as TScheduledEvent;
end;

function TSchEventList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);

  if Result >= 0 then
  begin
    if Assigned(FOnScheduleRemove) then
      FOnScheduleRemove(Self, Items[Result]);

     Result := inherited Remove(AObject);
  end;
end;

procedure TSchEventList.SetItem(Index: Integer; Value: TScheduledEvent);
begin
  inherited SetItem(Index, Value);
end;

{ TWorkerThread }

constructor TWorkerThread.Create(const IntSchedule: TScheduledEvent);
begin
  FIntSchedule := IntSchedule;
  FreeOnTerminate := True;

  inherited Create(False);
end;

procedure TWorkerThread.Execute;
begin
  inherited;

  FIntSchedule.ExecuteEvent;
end;

{ TIntervalMeter }

constructor TIntervalMeter.Create;
begin
  FHighFreq := QueryPerformanceFrequency(FFreq);

  case fHighFreq of
    True: QueryPerformanceCounter(FStart);
    False: FFreq := MSecsPerSec;
  end;

  Reset;
end;

function TIntervalMeter.getElapsed: Int64;
begin
  case FHighFreq of
    True:
      begin
        QueryPerformanceCounter(Result);
        Result := (MSecsPerSec * (Result - FStart)) div FFreq;
      end;
    False: Result := MilliSecondsBetween(FStartFrac, Now);
  end;
end;

procedure TIntervalMeter.Reset;
begin
  case FHighFreq of
    True: QueryPerformanceCounter(FStart);
    False: FStartFrac := Now;
  end;
end;

{ TChronEntryHelper }

class function TChronEntryHelper.GetFirstValue(var ChronEntry: TChronEntry; const Current: Integer): Integer;
begin
  with ChronEntry do
  begin
    Result := GetValue(ChronEntry, Current, Interval);
  end;
end;

class function TChronEntryHelper.GetNextValue(var ChronEntry: TChronEntry; const Current: Integer): Integer;
begin
  with ChronEntry do
  begin
    if Relative then
      Result := GetValue(ChronEntry, Current, Min(Interval, 1))
    else
      Result := GetValue(ChronEntry, Current + 1, Interval);
  end;
end;

class function TChronEntryHelper.GetValue(var ChronEntry: TChronEntry; const Current, Interval: Integer): Integer;
var
  I: Integer;
begin
  with ChronEntry do
  begin
    Result := MinValue;

    // if relative
    if Relative then
    begin
      // calculate the next relative step
      if (Current + Interval) > MaxValue then
        Result := Max(MinValue, (Current + Interval) mod (MaxValue - MinValue + 1))
      else
        Result := (Current + Interval);

      Exit;
    end;

    // if we have fixed values
    if Length(Values) > 0 then
    begin
      for I := 0 to Length(Values) - 1 do
      begin
        if Values[I] >= Current then
        begin
          Result := Values[I];
          Exit;
        end;
      end;

      // take the first one
      Result := Values[0];
      Exit;
    end;

    // if no fixed values then check min max
    // also check the interval for the value
    if (Current >= MinValue) and (Current <= MaxValue) then
    begin
      if ((Current - MinValue) mod Interval) <> 0 then
      begin
        Result := Current + (Interval - (Current - MinValue) mod Interval);

        if Result > MaxValue then
          Result := MinValue;
      end
      else
        Result := Current;
    end;
  end;
end;

class function TChronEntryHelper.HasValue(var ChronEntry: TChronEntry; const Value: Integer): Boolean;
var
  I: Integer;
begin
  with ChronEntry do
  begin
    Result := False;

    for I := 0 to Length(ChronEntry.Values) - 1 do
    begin
      if ChronEntry.Values[I] = Value then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

class procedure TChronEntryHelper.SetMaxValue(var ChronEntry: TChronEntry; const Value: Integer);
begin
  with ChronEntry do
  begin
    MaxValue := Value;
  end;
end;

end.
