program Builder;

uses
  FastMM4,
  Forms,
  Builder.Main in 'Builder.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
