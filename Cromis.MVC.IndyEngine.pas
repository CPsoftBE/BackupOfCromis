unit Cromis.MVC.IndyEngine;

interface

uses
  SysUtils, Classes,

  // cromis units
  Cromis.MVC.Sessions, Cromis.MVC.Routing, Cromis.MVC.Common, Cromis.StringUtils,

  // indy units and  the HTTPApp unit
  idHttpServer, idContext, IdCustomHTTPServer, IDCookie, HTTPApp;

type
  TOnGetSessionID = procedure(const Request: TIdHTTPRequestInfo;
                              var SessionClass: TClass;
                              var ID: string) of Object;
  TOnSetSessionID = procedure(const Response: TIdHTTPResponseInfo;
                              const Session: IMVCSession) of Object;

  IHTTPRequest = Interface(IInterface)
  ['{11F5ADED-1B02-48A6-89D5-AEA4C29CD229}']
    function GetRequest: TIdHTTPRequestInfo;
    function GetResponse: TIdHTTPResponseInfo;
    property Request: TIdHTTPRequestInfo read GetRequest;
    property Response: TIdHTTPResponseInfo read GetResponse;
  end;

  TMVCIndyEngine = class
  private
    FOnMVCRoute: TRouteProc;
    FHTTPServer: TIdHTTPServer;
    FOnGetSessionID: TOnGetSessionID;
    FOnSetSessionID: TOnSetSessionID;
    FMVCSessionManager: TMVCSessionManager;
    procedure OnCommandGet(AContext: TIdContext;
                           ARequestInfo: TIdHTTPRequestInfo;
                           AResponseInfo: TIdHTTPResponseInfo);
    procedure DoOnMVCRoute(const Sender: IBaseRoute; const HttpRequest: IInterface);
    function FillMVCData(const HttpRequest: IHTTPRequest; const Session: IMVCSession): IMVCData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FinalizeEngine;
    procedure InitializeEngine;
    function GetMimeType(const FileName: string): string;
    property HTTPServer: TIdHTTPServer read FHTTPServer;
    property OnMVCRoute: TRouteProc read FOnMVCRoute write FOnMVCRoute;
    property SessionManager: TMVCSessionManager read FMVCSessionManager;
    property OnGetSessionID: TOnGetSessionID read FOnGetSessionID write FOnGetSessionID;
    property OnSetSessionID: TOnSetSessionID read FOnSetSessionID write FOnSetSessionID;
  end;

implementation

type
  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  private
    FRequest: TIdHTTPRequestInfo;
    FResponse: TIdHTTPResponseInfo;
    function GetRequest: TIdHTTPRequestInfo;
    function GetResponse: TIdHTTPResponseInfo;
  public
    constructor Create(const Request: TIdHTTPRequestInfo;
                       const Response: TIdHTTPResponseInfo);
    property Response: TIdHTTPResponseInfo read GetResponse;
    property Request: TIdHTTPRequestInfo read GetRequest;
  end;

{ TMVCIndyEngine }

constructor TMVCIndyEngine.Create;
begin
  FOnMVCRoute := DoOnMVCRoute;

  // create and initialize HTTP server
  FHTTPServer := TIdHTTPServer.Create;
  FHTTPServer.OnCommandGet := OnCommandGet;
  FHTTPServer.ParseParams := False;

  // create the session manager object
  FMVCSessionManager := TMVCSessionManager.Create;
end;

destructor TMVCIndyEngine.Destroy;
begin
  FreeAndNil(FMVCSessionManager);
  FreeAndNil(FHTTPServer);

  inherited;
end;

function TMVCIndyEngine.FillMVCData(const HttpRequest: IHTTPRequest; const Session: IMVCSession): IMVCData;
var
  I: Integer;
  Params: TStringList;
begin
  Result := AcquireMVCData;

  Params := TStringList.Create;
  try
    Params.StrictDelimiter := True;
    Params.Delimiter := '&';

    // parse the parameters and store them into temporary string list
    Params.DelimitedText := UTF8ToString(HTTPDecode(UTF8Encode(HttpRequest.Request.UnparsedParams)));

    for I := 0 to Params.Count - 1 do
      Result.Values.Ensure(Params.Names[I]).AsString := Params.ValueFromIndex[I];
  finally
    Params.Free;
  end;
end;

procedure TMVCIndyEngine.FinalizeEngine;
begin
  FHTTPServer.Active := False;
  WebRouting.ClearAllRoutes;
end;

function TMVCIndyEngine.GetMimeType(const FileName: string): string;
begin
  Result := FHTTPServer.MIMETable.GetFileMIMEType(FileName);
end;

procedure TMVCIndyEngine.InitializeEngine;
begin
  FHTTPServer.Active := True;
end;

procedure TMVCIndyEngine.OnCommandGet(AContext: TIdContext;
                                      ARequestInfo: TIdHTTPRequestInfo;
                                      AResponseInfo: TIdHTTPResponseInfo);
var
  WebRoute: IBaseRoute;
  HTTPRequest: IHTTPRequest;
begin
  HTTPRequest := THTTPRequest.Create(ARequestInfo, AResponseInfo);
  WebRoute := WebRouting.FindRoute(ARequestInfo.Document);

  case WebRoute.RouteType of
    rtBase: raise Exception.Create('BaseRoute interface is not allowed');
    rtCustom: ICustomRoute(WebRoute).RouteProc(WebRoute, HTTPRequest);
    rtMVC: FOnMVCRoute(WebRoute, HTTPRequest);
  end;
end;

procedure TMVCIndyEngine.DoOnMVCRoute(const Sender: IBaseRoute; const HttpRequest: IInterface);
var
  Session: IMVCSession;
  SessiondID: string;
  SessionClass: TClass;
  RedirectURL: string;
  TempRequest: IHTTPRequest;
begin
  TempRequest := IHTTPRequest(HttpRequest);

  // dispatch the request to MVC and wait for result
  FOnGetSessionID(TempRequest.Request, SessionClass, SessiondID);
  Session := FMVCSessionManager.AcquireSession(SessiondID, SessionClass);

  Session.InitializeRequest;
  try
    Session.DispatchRequest(IMVCRoute(Sender), FillMVCData(TempRequest, Session));

    if Session.SessionData.DispatchResult = drForbiden then
    begin
      TempRequest.Response.ResponseNo := 401;
      Exit;
    end
    else if Session.SessionData.DispatchResult = drRedirectToLogin then
    begin
      RedirectURL := Format('/%s/%s', [Session.AuthenticationRoute.Controller,
                                       Session.AuthenticationRoute.AuthCommand]);
      TempRequest.Response.Redirect(RedirectURL);
      Exit;
    end;

    // if all is ok then send content back
    TempRequest.Response.ContentStream := TMemoryStream.Create;
    Session.SessionData.SessionContent.Data.Seek(0, soFromBeginning);
    TempRequest.Response.ContentStream.CopyFrom(Session.SessionData.SessionContent.Data, 0);
    TempRequest.Response.ContentType := Session.SessionData.SessionContent.Encoding;
    TempRequest.Response.ResponseNo := Session.SessionData.SessionContent.ExitCode;
    FOnSetSessionID(TempRequest.Response, Session);
  finally
    Session.FinalizeRequest;
  end;
end;

{ THTTPRequest }

constructor THTTPRequest.Create(const Request: TIdHTTPRequestInfo;
                                const Response: TIdHTTPResponseInfo);
begin
  FRequest := Request;
  FResponse := Response;
end;

function THTTPRequest.GetRequest: TIdHTTPRequestInfo;
begin
  Result := FRequest;
end;

function THTTPRequest.GetResponse: TIdHTTPResponseInfo;
begin
  Result := FResponse;
end;

end.
