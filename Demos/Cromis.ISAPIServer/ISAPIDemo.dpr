library ISAPIDemo;

uses
  ActiveX,
  ComObj,
  WebBroker,
  ISAPIApp,
  ISAPIThreadPool,
  ISAPI.Main in 'ISAPI.Main.pas' {WebModule1: TWebModule};

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.
