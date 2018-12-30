program IMCClient;

uses
  Forms,
  IMC.Client.Main in 'IMC.Client.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
{$IF CompilerVersion > 18}
  Application.MainFormOnTaskbar := True;
{$IFEND}
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
