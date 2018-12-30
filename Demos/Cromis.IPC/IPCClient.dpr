program IPCClient;

uses
  Forms,
  IPC.Client.Main in 'IPC.Client.Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
