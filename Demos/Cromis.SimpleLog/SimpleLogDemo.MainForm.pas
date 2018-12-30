unit SimpleLogDemo.MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,

  // cromis units
  Cromis.SimpleLog, Cromis.Exceptions;

type
  TfMain = class(TForm)
    btnLogEvent: TButton;
    btnHandledException: TButton;
    btnUnhadnledException: TButton;
    stLogLocations: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure btnLogEventClick(Sender: TObject);
    procedure btnHandledExceptionClick(Sender: TObject);
    procedure btnUnhadnledExceptionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnHandledExceptionClick(Sender: TObject);
var
  DummyObject: TStringList;
begin
  try
    DummyObject.Add('Dummy string');
  except
    on E: Exception do
      SimpleLog.LogEvent('DebugLog', 'Handled exception: ' + E.Message, ltError);
  end;
end;

procedure TfMain.btnLogEventClick(Sender: TObject);
begin
  SimpleLog.LogEvent('DebugLog','This is warning', ltWarning);
  SimpleLog.LogEvent('DebugLog','This is error', ltError);
  SimpleLog.LogEvent('DebugLog','This is info', ltInfo);
  SimpleLog.LogEvent('DebugLog','This is hint', ltHint);
end;

procedure TfMain.btnUnhadnledExceptionClick(Sender: TObject);
var
  DummyObject: TStringList;
begin
  DummyObject.Add('Dummy string');
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  RootDir: string;
begin
  RootDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  SimpleLog.RegisterLog('StackLog', RootDir + 'StackLog.txt');
  SimpleLog.RegisterLog('DebugLog', RootDir + 'DebugLog.txt');
  ExceptionHandler.UnhandledExceptionsOnly := True;
  ExceptionHandler.SimpleLogID := 'StackLog';
  SimpleLog.LockType := ltNone;
end;

end.
