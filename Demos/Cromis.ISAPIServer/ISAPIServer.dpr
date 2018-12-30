program ISAPIServer;

uses
  Forms,
  f_Main in 'f_Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
