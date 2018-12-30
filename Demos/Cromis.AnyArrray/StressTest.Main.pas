unit StressTest.Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  DateUtils, ExtCtrls, TypInfo, ComCtrls,

  // generics units
  Generics.Collections, Generics.Defaults,

  // cromis units
  Cromis.Threading, Cromis.AnyValue, Cromis.StringUtils;

type
  PControlData = ^TControlData;
  TControlData = record
    Task: ITask;
    Counter: Integer;
    AnyArray: IAnyArray;
    ReplayData: TStringList;
    ControlList: TList<TAnyValue>;
  end;

  TfMain = class(TForm)
    lbArrayActions: TListBox;
    btnStart: TButton;
    btnStop: TButton;
    tmCurrentTime: TTimer;
    lbRunningText: TLabel;
    lbRunningValue: TLabel;
    lbSliceData: TListBox;
    lbNumSlicesText: TLabel;
    lbNumSlicesValue: TLabel;
    btnReplay: TButton;
    pbReplayProgress: TProgressBar;
    btnSimulate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmCurrentTimeTimer(Sender: TObject);
    procedure btnReplayClick(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
  private
    FTaskPool: TTaskPool;
    FStartTime: TDateTime;
    FSimulateTask: ITask;
    FStressTask: ITask;
    FReplayTask: ITask;

    function DoPopElements(const Data: PControlData; const Elements: Integer): Boolean;
    function DoPushElements(const Data: PControlData; const Elements: Integer): Boolean;
    function DoInsertElements(const Data: PControlData; const Elements: Integer): Boolean;
    function DoDeleteElements(const Data: PControlData; const Elements: Integer): Boolean;

    procedure OnArrayStressTest(const Task: ITask);
    procedure OnArrayReplayAction(const Task: ITask);
    procedure OnSimulateTaskAction(const Task: ITask);
    procedure DoLoadReplayData(const Data: PControlData);
    procedure OnStressTestMessage(const Msg: ITaskMessage);
    procedure WriteListsToHardDrive(const Data: PControlData);
    procedure SendSliceStructureData(const Data: PControlData);
    procedure WriteDataToHardDrive(const Data: PControlData; const Action: string; const Replay, Error: Boolean);
    function CheckDataConsistency(const Data: PControlData; const Action: string; const Replay: Boolean): Boolean;
    procedure DoSendMessage(const Task: ITask; const Name, Value: string; const LoopCount: Integer);
  public
  end;

var
  fMain: TfMain;

implementation

const
  cFileReplayList = 'Data\ReplayList.txt';
  cFileControlList = 'Data\ControlList.txt';
  cFileAnyArrayData = 'Data\AnyArrayData.txt';
  cFileAnyArrayConf = 'Data\AnyArrayCont.txt';
  cFileControlTextList = 'Data\ControlTextList.txt';
  cFileAnyArrayTextList = 'Data\AnyArrayTextList.txt';

{$R *.dfm}

procedure TfMain.btnReplayClick(Sender: TObject);
begin
  FReplayTask := FTaskPool.AcquireTask(OnArrayReplayAction, 'ReplayTask');
  FReplayTask.Run;
end;

procedure TfMain.btnSimulateClick(Sender: TObject);
begin
  FSimulateTask := FTaskPool.AcquireTask(OnSimulateTaskAction, 'SimulateTask');
  FSimulateTask.Run;
end;

procedure TfMain.btnStartClick(Sender: TObject);
begin
  tmCurrentTime.Enabled := True;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  FStartTime := Now;

  FStressTask := FTaskPool.AcquireTask(OnArrayStressTest, 'StressTest');
  FStressTask.Run;
end;

procedure TfMain.btnStopClick(Sender: TObject);
begin
  FStressTask.Terminate;

  btnStop.Enabled := False;
  btnStart.Enabled := True;
  tmCurrentTime.Enabled := False;
end;

function TfMain.CheckDataConsistency(const Data: PControlData; const Action: string; const Replay: Boolean): Boolean;
var
  I, K: Integer;
begin
  Result := True;
  try
    for I := 0 to Data.ControlList.Count - 1 do
    begin
      if not Data.AnyArray.Item[I].Equal(Data.ControlList.Items[I]) then
      begin
        DoSendMessage(Data.Task, 'Message', 'Found inconsistency!!!', I);
        SendSliceStructureData(Data);
        WriteListsToHardDrive(Data);
        Data.Task.Terminate;
        Result := False;
        Exit;
      end;
    end;
  finally
    WriteDataToHardDrive(Data, Action, Replay, not Result);
  end;
end;

function TfMain.DoDeleteElements(const Data: PControlData; const Elements: Integer): Boolean;
var
  I: Integer;
  Index: Integer;
begin
  for I := 1 to (Elements div 2) + (Random(Elements)) do
  begin
    if Data.ControlList.Count = 0 then
      Break;

    Index := Random(Data.AnyArray.Count - 1);
    Data.ReplayData.Add(IntToStr(Index));
    Data.AnyArray.DeleteIndex(Index);
    Data.ControlList.Delete(Index);
  end;

  // check for consistency
  Result := CheckDataConsistency(Data, 'Delete', False);
end;

function TfMain.DoInsertElements(const Data: PControlData; const Elements: Integer): Boolean;
var
  I: Integer;
  Index: Integer;
  Value: TAnyValue;
begin
  if Data.ControlList.Count = 0 then
  begin
    Value := Random(1000000);
    Data.AnyArray.Push(Value);
    Data.ControlList.Add(Value);
    Data.ReplayData.Add(Format('%d,%d', [0, Value.AsInteger]));
  end;

  for I := 1 to (Elements div 2) + (Random(Elements)) do
  begin
    Index := Random(Data.AnyArray.Count - 1);
    Value := Random(1000000);

    Data.ReplayData.Add(Format('%d,%d', [Index, Value.AsInteger]));
    Data.ControlList.Insert(Index, Value);
    Data.AnyArray.Insert(Index, Value);
  end;

  // check for consistency
  Result := CheckDataConsistency(Data, 'Insert', False);
end;

procedure TfMain.DoLoadReplayData(const Data: PControlData);
var
  I, K: Integer;
  Value: Integer;
  RootDir: string;
  Elements: Integer;
  ListData: TStringList;
  ArrayData: TFileStream;
begin
  ListData := TStringList.Create;
  try
    RootDir := ExtractFilePath(ParamStr(0));
    ListData.LoadFromFile(RootDir + cFileAnyArrayConf);

    Data.ReplayData.LoadFromFile(RootDir + cFileReplayList);
    Data.AnyArray.SliceBufferMpl := StrToFloat(ListData.Values['SliceBufferMpl']);
    Data.AnyArray.SliceSize := StrToInt(ListData.Values['SliceSize']);
    Data.AnyArray.SliceCount := StrToInt(ListData.Values['SliceCount']);

    // trick that lets the last slice intact from deletion
    Inc(Data.AnyArray.RawData^[Data.AnyArray.SliceCount - 1].Last);

    ArrayData := TFileStream.Create(RootDir + cFileAnyArrayData, fmOpenRead);
    try
      for I := 0 to Data.AnyArray.SliceCount - 1 do
      begin
        ArrayData.Read(Value, SizeOf(Integer));
        Data.AnyArray.RawData^[I].Index := Value;
        ArrayData.Read(Value, SizeOf(Integer));
        Data.AnyArray.RawData^[I].Start := Value;
        ArrayData.Read(Value, SizeOf(Integer));
        Data.AnyArray.RawData^[I].Last := Value;
        Elements := Data.AnyArray.RawData^[I].Last -
                    Data.AnyArray.RawData^[I].Start;
        Data.AnyArray.RawData^[I].Last := Data.AnyArray.RawData^[I].Start;

        for K := 0 to Elements - 1 do
        begin
          ArrayData.Read(Value, SizeOf(Integer));
          Data.AnyArray.Insert(Data.AnyArray.Count, Value);
        end;
      end;
    finally
      ArrayData.Free;
    end;

    ListData.LoadFromFile(RootDir + cFileControlList);
    for I := 0 to ListData.Count - 1 do
      Data.ControlList.Add(StrToInt(ListData[I]));
  finally
    ListData.Free;
  end;
end;

function TfMain.DoPopElements(const Data: PControlData; const Elements: Integer): Boolean;
var
  I: Integer;
  Value: TAnyValue;
begin
  for I := 1 to (3 * (Elements div 4)) + (Random(Elements div 2)) do
  begin
    Data.ControlList.Delete(Data.ControlList.Count - 1);

    Value := Data.AnyArray.Pop;
    Data.ReplayData.Add(Value.AsString);
  end;

  // check for consistency
  Result := CheckDataConsistency(Data, 'Pop', False);
end;

function TfMain.DoPushElements(const Data: PControlData; const Elements: Integer): Boolean;
var
  I: Integer;
  Value: TAnyValue;
begin
  for I := 1 to (3 * (Elements div 4)) + (Random(Elements div 2)) do
  begin
    Value := Random(1000000);
    Data.ReplayData.Add(Value.AsString);
    Data.ControlList.Add(Value);
    Data.AnyArray.Push(Value);
  end;

  // check for consistency
  Result := CheckDataConsistency(Data, 'Push', False);
end;

procedure TfMain.DoSendMessage(const Task: ITask; const Name, Value: string; const LoopCount: Integer);
begin
  if Name = 'Message' then
    Task.Message.Ensure(Name).AsString := Value + Format(' [%d]', [LoopCount])
  else
    Task.Message.Ensure(Name).AsString := Value;
  Task.SendMessageAsync;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FTaskPool := TTaskPool.Create(5);
  FTaskPool.OnTaskMessage := OnStressTestMessage;

  lbNumSlicesValue.Caption := '';
  lbRunningValue.Caption := '';
  btnStop.Enabled := False;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTaskPool);
end;

procedure TfMain.OnArrayReplayAction(const Task: ITask);
var
  I: Integer;
  Index: Integer;
  Value: Integer;
  RootDir: string;
  ListData: TStringList;
  ControlData: TControlData;
  ActionType: string;
begin
  ListData := TStringList.Create;
  try
    RootDir := ExtractFilePath(ParamStr(0));
    ListData.LoadFromFile(RootDir + cFileAnyArrayConf);
    ActionType := ListData.Values['ActionType'];

    ControlData.ControlList := TList<TAnyValue>.Create;
    try
      ControlData.ReplayData := TStringList.Create;
      try
        DoSendMessage(Task, 'Message', 'Started replaying data...', 0);
        ControlData.AnyArray := CreateAnyArray;
        DoLoadReplayData(@ControlData);
        ControlData.Counter := 0;
        ControlData.Task := Task;
        Randomize;

        if not CheckDataConsistency(@ControlData, ActionType, True) then
        begin
          DoSendMessage(Task, 'Message', 'Error replaying data', 0);
          Exit;
        end;

        if ActionType = 'Pop' then
        begin
          for I := 0 to ControlData.ReplayData.Count - 1 do
          begin
            ControlData.AnyArray.Pop;
            ControlData.ControlList.Delete(ControlData.ControlList.Count - 1);

            if not CheckDataConsistency(@ControlData, ActionType, True) then
            begin
              DoSendMessage(Task, 'Message', 'Error replaying data', I);
              Exit;
            end;
          end;
        end
        else if ActionType = 'Push' then
        begin
          for I := 0 to ControlData.ReplayData.Count - 1 do
          begin
            ControlData.AnyArray.Push(StrToInt(ControlData.ReplayData[I]));
            ControlData.ControlList.Add(StrToInt(ControlData.ReplayData[I]));

            if not CheckDataConsistency(@ControlData, ActionType, True) then
            begin
              DoSendMessage(Task, 'Message', 'Error replaying data', I);
              Exit;
            end;
          end;
        end
        else if ActionType = 'Delete' then
        begin
          for I := 0 to ControlData.ReplayData.Count - 1 do
          begin
            ControlData.AnyArray.DeleteIndex(StrToInt(ControlData.ReplayData[I]));
            ControlData.ControlList.Delete(StrToInt(ControlData.ReplayData[I]));

            if not CheckDataConsistency(@ControlData, ActionType, True) then
            begin
              DoSendMessage(Task, 'Message', 'Error replaying data', I);
              Exit;
            end;
          end;
        end
        else if ActionType = 'Insert' then
        begin
          for I := 0 to ControlData.ReplayData.Count - 1 do
          begin
            if ControlData.ControlList.Count = 0 then
            begin
              Value := StrToInt(StrAfter(',', ControlData.ReplayData[I]));
              ControlData.ControlList.Add(Value);
              ControlData.AnyArray.Push(Value);
              Continue;
            end;

            Index := StrToInt(StrBefore(',', ControlData.ReplayData[I]));
            Value := StrToInt(StrAfter(',', ControlData.ReplayData[I]));
            ControlData.ControlList.Insert(Index, Value);
            ControlData.AnyArray.Insert(Index, Value);

            if not CheckDataConsistency(@ControlData, ActionType, True) then
            begin
              DoSendMessage(Task, 'Message', 'Error replaying data', I);
              Exit;
            end;
          end;
        end;

        // we finished without any errors
        DoSendMessage(Task, 'Message', 'Finished replaying data. No errors.', 0);
      finally
        ControlData.ReplayData.Free;
      end;
    finally
      ControlData.ControlList.Free;
    end;
  finally
    ListData.Free;
  end;
end;

procedure TfMain.OnArrayStressTest(const Task: ITask);
var
  LogMessage: string;
  ControlData: TControlData;
begin
  try
    ControlData.ControlList := TList<TAnyValue>.Create;
    try
      ControlData.ReplayData := TStringList.Create;
      try
        DoSendMessage(Task, 'Message', 'Adding 1.000.000 items to begin with', 0);

        ControlData.AnyArray := CreateAnyArray;
        ControlData.AnyArray.SliceSize := 5000;
        ControlData.Counter := 1;
        ControlData.Task := Task;
        Randomize;

        // add 200.000 integers
        if not DoPushElements(@ControlData, 200000) then
          Exit;

        while not Task.Terminated do
        begin
          DoSendMessage(Task, 'Message', 'Beginning 5 loop cycle of tests', 0);

          while ControlData.Counter mod 5 <> 0 do
          begin
            DoSendMessage(Task, 'Message', 'Deleting random items...', ControlData.Counter);

            // Delete 20.000 elements
            if not DoDeleteElements(@ControlData, 30000) then
              Exit;

            DoSendMessage(Task, 'Message', 'Inserting random items...', ControlData.Counter);
            SendSliceStructureData(@ControlData);

            // Insert 20.000 elements
            if not DoInsertElements(@ControlData, 30000) then
              Exit;

            DoSendMessage(Task, 'Message', 'Poping last items...', ControlData.Counter);
            SendSliceStructureData(@ControlData);

            // pop 20.000 items
            if not DoPopElements(@ControlData, 30000) then
              Exit;

            DoSendMessage(Task, 'Message', 'Adding new items...', ControlData.Counter);
            SendSliceStructureData(@ControlData);

            // add 20.000 integers
            if not DoPushElements(@ControlData, 30000) then
              Exit;

            LogMessage := Format('Test loop number %d compeleted', [ControlData.Counter]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
            Inc(ControlData.Counter);

            LogMessage := Format('Control List has %d Elements', [ControlData.ControlList.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
            LogMessage := Format('Any Array has %d Elements', [ControlData.AnyArray.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
            SendSliceStructureData(@ControlData);
          end;

          // finalize the 5 loop cycle and then begin new one
          DoSendMessage(Task, 'Message', 'Finalizing 5 loop cycle of tests', ControlData.Counter);
          DoSendMessage(Task, 'Message', 'Deleting all elements', ControlData.Counter);

          // delete random items
          while ControlData.ControlList.Count > 0 do
          begin
            // Delete 20.000 elements
            if not DoDeleteElements(@ControlData, 20000) then
              Exit;

            LogMessage := Format('%d elements left', [ControlData.ControlList.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
            LogMessage := Format('AnyArray has %d elements', [ControlData.AnyArray.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
          end;

          DoSendMessage(Task, 'Message', 'Inserting 200000 elements', ControlData.Counter);

          // Insert 200.000 elements
          while ControlData.ControlList.Count < 200000 do
          begin
            // Insert 20.000 elements
            if not DoInsertElements(@ControlData, 20000) then
              Exit;

            LogMessage := Format('%d elements inserted', [ControlData.ControlList.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
            LogMessage := Format('AnyArray has %d elements', [ControlData.AnyArray.Count]);
            DoSendMessage(Task, 'Message', LogMessage, ControlData.Counter);
          end;

          Inc(ControlData.Counter);
        end;
      finally
        ControlData.ReplayData.Free;
      end;
    finally
      ControlData.ControlList.Free;
    end;
  except
    on E: Exception do
      DoSendMessage(Task, 'Message', Format('Worker Thread error: %s', [E.Message]), -1);
  end;
end;

procedure TfMain.OnSimulateTaskAction(const Task: ITask);
var
  I: Integer;
  Index: Integer;
  Value: Integer;
  ControlData: TControlData;

  function CheckReplayDataConsistency(const Data: PControlData): Boolean;
  var
    K: Integer;
  begin
    Result := True;

    for K := 0 to Data.ControlList.Count - 1 do
    begin
      if not Data.AnyArray.Item[K].Equal(Data.ControlList.Items[K]) then
      begin
        DoSendMessage(Task, 'Message', 'Data is not consistent after Insert!!', Data.Counter);
        Result := False;
        Exit;
      end;
    end;
  end;

begin
  try
    ControlData.ControlList := TList<TAnyValue>.Create;
    try
      ControlData.ReplayData := TStringList.Create;
      try
        DoSendMessage(Task, 'Message', 'Simulating small push, delete and insert...', 0);

        ControlData.AnyArray := CreateAnyArray;
        ControlData.AnyArray.SliceSize := 5000;
        ControlData.Counter := 1;
        ControlData.Task := Task;
        Randomize;

        DoSendMessage(Task, 'Message', 'Adding 200.000 items...', ControlData.Counter);
        SendSliceStructureData(@ControlData);

        // add 200.000 integers
        if not DoPushElements(@ControlData, 200000) then
          Exit;

        DoSendMessage(Task, 'Message', 'Deleting random items...', ControlData.Counter);
        SendSliceStructureData(@ControlData);

        // Delete 20.000 elements
        if not DoDeleteElements(@ControlData, 30000) then
          Exit;

        DoSendMessage(Task, 'Message', 'Inserting random items...', ControlData.Counter);
        SendSliceStructureData(@ControlData);
        ControlData.ReplayData.Clear;

        // Insert 20.000 elements
        for I := 1 to (30000 div 2) + (Random(30000)) do
        begin
          if ControlData.ControlList.Count = 0 then
          begin
            Value := Random(1000000);
            ControlData.AnyArray.Push(Value);
            ControlData.ControlList.Add(Value);
            ControlData.ReplayData.Add(Format('%d,%d', [0, Value]));
            Continue;
          end;

          Index := Random(ControlData.AnyArray.Count - 1);
          Value := Random(1000000);

          ControlData.ReplayData.Add(Format('%d,%d', [Index, Value]));
          ControlData.ControlList.Insert(Index, Value);
          ControlData.AnyArray.Insert(Index, Value);
        end;

        // simulate that error occured durring an insert action
        WriteDataToHardDrive(@ControlData, 'Insert', False, True);

        // now reload the data to do the simulation
        ControlData.ControlList.Clear;
        ControlData.ReplayData.Clear;
        ControlData.AnyArray.Clear;

        // load the data from hard disk
        DoLoadReplayData(@ControlData);

        if not CheckReplayDataConsistency(@ControlData) then
         Exit;

        for I := 0 to ControlData.ReplayData.Count - 1 do
        begin
          if ControlData.ControlList.Count = 0 then
          begin
            Value := StrToInt(StrAfter(',', ControlData.ReplayData[I]));
            ControlData.ControlList.Add(Value);
            ControlData.AnyArray.Push(Value);
            Continue;
          end;

          Index := StrToInt(StrBefore(',', ControlData.ReplayData[I]));
          Value := StrToInt(StrAfter(',', ControlData.ReplayData[I]));
          ControlData.ControlList.Insert(Index, Value);
          ControlData.AnyArray.Insert(Index, Value);
        end;

        if not CheckReplayDataConsistency(@ControlData) then
         Exit;

        // all is ok, data is correct and consistent
        DoSendMessage(Task, 'Message', 'Data is consistent on simulation.', ControlData.Counter);
      finally
        ControlData.ReplayData.Free;
      end;
    finally
      ControlData.ControlList.Free;
    end;
  except
    on E: Exception do
      DoSendMessage(Task, 'Message', Format('Worker Thread error: %s', [E.Message]), -1);
  end;
end;

procedure TfMain.OnStressTestMessage(const Msg: ITaskMessage);
begin
  if Msg.Values.Exists('Message') then
  begin
    lbArrayActions.Items.BeginUpdate;
    try
      lbArrayActions.Items.Add(Msg.Values.Get('Message').AsString);
      lbArrayActions.ItemIndex := lbArrayActions.Count - 1;
      Application.ProcessMessages;
    finally
      lbArrayActions.Items.EndUpdate;
    end;
  end
  else if Msg.Values.Exists('SliceStructure') then
  begin
    lbSliceData.Items.BeginUpdate;
    try
      lbSliceData.Items.Text := Msg.Values.Get('SliceStructure').AsString;
      lbNumSlicesValue.Caption := IntToStr(lbSliceData.Count);
      Application.ProcessMessages;
    finally
      lbSliceData.Items.EndUpdate;
    end;
  end;
end;

procedure TfMain.SendSliceStructureData(const Data: PControlData);
var
  I: Integer;
  ItemCount: Integer;
  CurrentSlice: PArraySlice;
  SliceDataList: TStringList;
begin
  SliceDataList := TStringList.Create;
  try
    for I := 0 to Data.AnyArray.SliceCount - 1 do
    begin
      CurrentSlice := Data.AnyArray.RawData^[I];
      ItemCount := CurrentSlice.Last - CurrentSlice.Start;
      SliceDataList.Add(Format('Slice %d: %d - %d [%d]', [CurrentSlice.Index,
                                                          CurrentSlice.Start,
                                                          CurrentSlice.Last,
                                                          ItemCount]));
    end;

    DoSendMessage(Data.Task, 'SliceStructure', SliceDataList.Text, 0);
  finally
    SliceDataList.Free;
  end;
end;

procedure TfMain.tmCurrentTimeTimer(Sender: TObject);
var
  DeltaTime: TDateTime;
begin
  DeltaTime := Now - FStartTime;
  lbRunningValue.Caption := FormatDateTime('hh:nn:ss', DeltaTime);
end;

procedure TfMain.WriteDataToHardDrive(const Data: PControlData; const Action: string; const Replay, Error: Boolean);
var
  K, I: Integer;
  Value: Integer;
  RootDir: string;
  ListData: TStringList;
  ArrayData: TFileStream;
begin
  ListData := TStringList.Create;
  try
    RootDir := ExtractFilePath(ParamStr(0));
    ForceDirectories(RootDir + 'Data');

    if (not Replay) and (not Error) then
    begin
      // save any array raw data
      ArrayData := TFileStream.Create(RootDir + cFileAnyArrayData, fmCreate);
      try
        for K := 0 to Data.AnyArray.SliceCount - 1 do
        begin
          ArrayData.Write(Data.AnyArray.RawData^[K].Index, SizeOf(Integer));
          ArrayData.Write(Data.AnyArray.RawData^[K].Start, SizeOf(Integer));
          ArrayData.Write(Data.AnyArray.RawData^[K].Last, SizeOf(Integer));

          for I := Data.AnyArray.RawData^[K].Start to Data.AnyArray.RawData^[K].Last - 1 do
          begin
            Value := Data.AnyArray.RawData^[K].Data[I].AsInteger;
            ArrayData.Write(Value, SizeOf(Integer));
          end;
        end;
      finally
        ArrayData.Free;
      end;

      // save any array configuration
      ListData.Values['SliceBufferMpl'] := FloatToStr(Data.AnyArray.SliceBufferMpl);
      ListData.Values['SliceCount'] := IntToStr(Data.AnyArray.SliceCount);
      ListData.Values['SliceSize'] := IntToStr(Data.AnyArray.SliceSize);
      ListData.Values['ActionType'] := Action;
      ListData.SaveToFile(cFileAnyArrayConf);
      ListData.Clear;

      for K := 0 to Data.ControlList.Count - 1 do
        ListData.Add(IntToStr(Data.ControlList[K]));

      ListData.SaveToFile(RootDir + cFileControlList);
      ListData.Clear;
    end;

    if Error and not Replay then
      Data.ReplayData.SaveToFile(RootDir + cFileReplayList);

    if not Replay then
      Data.ReplayData.Clear;
  finally
    ListData.Free;
  end;
end;

procedure TfMain.WriteListsToHardDrive(const Data: PControlData);
var
  I: Integer;
  RootDir: string;
  ListData: TStringList;
begin
  ListData := TStringList.Create;
  try
    RootDir := ExtractFilePath(ParamStr(0));
    ForceDirectories(RootDir + 'Data');

    for I := 0 to Data.AnyArray.Count - 1 do
      ListData.Add(Data.AnyArray[I].AsString);

    ListData.SaveToFile(RootDir + cFileAnyArrayTextList);
    ListData.Clear;

    for I := 0 to Data.ControlList.Count - 1 do
      ListData.Add(IntToStr(Data.ControlList[I]));

    ListData.SaveToFile(RootDir + cFileControlTextList);
    ListData.Clear;
  finally
    ListData.Free;
  end;
end;

end.
