program StressTest;

uses
  Forms,
  StressTest.Main in 'StressTest.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
