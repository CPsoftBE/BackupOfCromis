program DocFilters;

uses
  Forms,
  DocFilters.Main in 'DocFilters.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
