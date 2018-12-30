program SpeedMain;

uses
  Forms,
  SpeedTest.Main in 'SpeedTest.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
