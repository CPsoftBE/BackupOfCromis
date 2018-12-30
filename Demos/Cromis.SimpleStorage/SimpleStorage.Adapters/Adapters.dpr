program Adapters;

uses
  Forms,
  Adapters.Main in 'Adapters.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
