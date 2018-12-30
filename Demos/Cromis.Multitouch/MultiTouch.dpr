program MultiTouch;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
