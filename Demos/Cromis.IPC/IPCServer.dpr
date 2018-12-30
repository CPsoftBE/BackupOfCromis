program IPCServer;

uses
  Forms,
  IPC.Server.Main in 'IPC.Server.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
