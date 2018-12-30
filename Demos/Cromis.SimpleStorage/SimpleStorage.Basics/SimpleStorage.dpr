program SimpleStorage;

uses
  FastMM4,
  Forms,
  SimpleStorage.Main in 'SimpleStorage.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
