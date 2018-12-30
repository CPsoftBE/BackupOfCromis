program ThreadPool;

uses
  Forms,
  ThreadPool.MainForm in 'ThreadPool.MainForm.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
