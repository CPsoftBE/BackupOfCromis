unit ThreadPool.MainForm;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, XPMan, ExtCtrls,

  // cromis units
  Cromis.Threading;

type
  TfMain = class(TForm)
    btnStart: TButton;
    ePoolSize: TEdit;
    lbThreadResults: TListBox;
    pbPoolSize: TProgressBar;
    btnStop: TButton;
    tbThreadTimeout: TTrackBar;
    tbCreationTimeout: TTrackBar;
    XPManifest: TXPManifest;
    stPoolSizeText: TStaticText;
    stFreeThreadsText: TStaticText;
    stPoolSizeValue: TStaticText;
    stFreeThreadsValue: TStaticText;
    tmPoolStatus: TTimer;
    stCreationTimeout: TStaticText;
    stThreadTimeout: TStaticText;
    StaticText1: TStaticText;
    cbDynamicPoolSize: TCheckBox;
    stThreadsFinishedLabel: TStaticText;
    stThreadsFinishedValue: TStaticText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmPoolStatusTimer(Sender: TObject);
    procedure tbThreadTimeoutChange(Sender: TObject);
    procedure tbCreationTimeoutChange(Sender: TObject);
    procedure cbDynamicPoolSizeClick(Sender: TObject);
  private
    { Private declarations }
    FTaskPool: TTaskPool;
    FTerminate: Boolean;
    FTaskCounter: Int64;
    procedure OnTaskExecute(const Task: ITask);
    procedure OnTaskMessage(const Msg: ITaskMessage);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnStartClick(Sender: TObject);
var
  Task: ITask;
begin
  FTaskPool.DynamicSize := cbDynamicPoolSize.Checked;
  FTaskPool.MinPoolSize := StrToInt(ePoolSize.Text);
  FTaskPool.OnTaskMessage := OnTaskMessage;
  FTaskPool.Initialize;

  tmPoolStatus.Enabled := False;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  FTerminate := False;

  while not FTerminate do
  begin
    Task := FTaskPool.AcquireTask(OnTaskExecute, 'RandomTask');
    Task.Values.Ensure('RandomNumber').AsInteger := Random(tbThreadTimeout.Position);
    Task.Run;

    pbPoolSize.Position := FTaskPool.PoolSize - FTaskPool.FreeTasks;
    stFreeThreadsValue.Caption := IntToStr(FTaskPool.FreeTasks);
    stPoolSizeValue.Caption := IntToStr(FTaskPool.PoolSize);
    Sleep(Random(tbCreationTimeout.Position));
    Application.ProcessMessages;
  end;
end;

procedure TfMain.btnStopClick(Sender: TObject);
begin
  FTaskPool.Finalize;

  tmPoolStatus.Enabled := True;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  FTerminate := True;
end;

procedure TfMain.cbDynamicPoolSizeClick(Sender: TObject);
begin
  FTaskPool.DynamicSize := cbDynamicPoolSize.Checked;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FTerminate := True;
  FTaskPool.Finalize;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FTaskPool := TTaskPool.Create(StrToInt(ePoolSize.Text));
  pbPoolSize.Max := StrToInt(ePoolSize.Text);

  stCreationTimeout.Caption := IntToStr(tbCreationTimeout.Position);
  stThreadTimeout.Caption := IntToStr(tbThreadTimeout.Position);

  Randomize;
  tmPoolStatus.Enabled := True;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTaskPool);
end;

procedure TfMain.OnTaskExecute(const Task: ITask);
var
  Interval: Integer;
begin
  Interval := Task.Values.Get('RandomNumber').AsInteger;
  try
    Task.Message.Ensure('Result').AsInteger := Interval;
    Sleep(Interval);
  finally
    Task.SendMessageAsync;
  end;
end;

procedure TfMain.OnTaskMessage(const Msg: ITaskMessage);
var
  Interval: Integer;
begin
  Inc(FTaskCounter);
  Interval := Msg.Values.Get('Result').AsInteger;
  stThreadsFinishedValue.Caption := IntToStr(FTaskCounter);
end;

procedure TfMain.tbCreationTimeoutChange(Sender: TObject);
begin
  stCreationTimeout.Caption := IntToStr(tbCreationTimeout.Position);
end;

procedure TfMain.tbThreadTimeoutChange(Sender: TObject);
begin
  stThreadTimeout.Caption := IntToStr(tbThreadTimeout.Position);
end;

procedure TfMain.tmPoolStatusTimer(Sender: TObject);
begin
  stFreeThreadsValue.Caption := IntToStr(FTaskPool.FreeTasks);
  stPoolSizeValue.Caption := IntToStr(FTaskPool.PoolSize);
end;

end.
