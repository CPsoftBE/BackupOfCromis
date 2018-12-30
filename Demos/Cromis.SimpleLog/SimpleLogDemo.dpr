program SimpleLogDemo;

uses
  Forms,
  SimpleLogDemo.MainForm in 'SimpleLogDemo.MainForm.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
