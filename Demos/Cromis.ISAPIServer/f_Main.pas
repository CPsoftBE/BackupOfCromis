unit f_Main;

interface

{$I ISAPIServer.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,

  // indy components
  IdBaseComponent, IdComponent, IdTCPServer, IdCustomHTTPServer, IdHTTPServer,
  {$IFNDEF Indy9}IdCustomTCPServer, idContext,{$ENDIF}

  // cromis units
  Cromis.ISAPI.Server, Cromis.ISAPI.Indy, Cromis.StringUtils;


type
  TfMain = class(TForm)
    HTTPServer: TIdHTTPServer;
    mmInstructions: TMemo;
    procedure HTTPServerCommandGet({$IFDEF Indy9}
                                     AThread: TIdPeerThread;
                                   {$ELSE}
                                    AContext: TIdContext;
                                   {$ENDIF}
                                   ARequestInfo: TIdHTTPRequestInfo;
                                   AResponseInfo: TIdHTTPResponseInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FISAPIServer: TISAPIServer;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  FISAPIServer := TISAPIServer.Create;
  HTTPServer.DefaultPort := 9999;
  HTTPServer.Active := True;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FISAPIServer);
end;

procedure TfMain.HTTPServerCommandGet({$IFDEF Indy9}
                                        AThread: TIdPeerThread;
                                      {$ELSE}
                                        AContext: TIdContext;
                                      {$ENDIF}
                                      ARequestInfo: TIdHTTPRequestInfo;
                                      AResponseInfo: TIdHTTPResponseInfo);
var
  ECB: TECBData;
  Port: string;
  TempStr: string;
  PathStr: string;
  RootDir: string;
  DDLFileName: string;
begin
  TempStr := Copy(ARequestInfo.Document, 2, Length(ARequestInfo.Document));
  RootDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  PathStr := '/' + StrAfter('/', TempStr);
  DDLFileName := StrBefore('/', TempStr);

  if FileExists(RootDir + DDLFileName) then
  begin
    {$IFDEF Indy9}
      Port := IntToStr(AThread.Connection.Socket.Binding.Port);
    {$ELSE}
      Port := IntToStr(AContext.Binding.Port);
    {$ENDIF}

    ECB := ECBDataList.AcquireNewECB;
    try
      FillECBFromRequest(ECB, HTTPServer.KeepAlive, ARequestInfo, RootDir, DDLFileName, Port, PathStr);
      try
        FISAPIServer.Execute(RootDir + DDLFileName, ECB);
      finally
        FillResponseFromECB(ECB, AResponseInfo);
      end;
    finally
      ECBDataList.DeleteECB(ECB.ECB.ConnID);
    end;
  end;
end;

end.
