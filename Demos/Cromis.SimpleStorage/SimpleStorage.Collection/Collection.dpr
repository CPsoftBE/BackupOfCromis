program Collection;

uses
  Forms,
  Collection.Main in 'Collection.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
