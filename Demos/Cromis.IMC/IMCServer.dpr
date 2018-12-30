program IMCServer;

uses
  Forms,
  IMC.Server.Main in 'IMC.Server.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
{$IF CompilerVersion > 18}
  Application.MainFormOnTaskbar := True;
{$IFEND}
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
