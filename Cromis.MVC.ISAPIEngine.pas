unit Cromis.MVC.ISAPIEngine;

interface

uses
  SysUtils, Classes, HTTPApp,

  // cromis units
  Cromis.MVC.Controller,
  Cromis.MVC.Sessions,
  Cromis.MVC.Routing,
  Cromis.MVC.Common,

  Cromis.StringUtils,
  Cromis.Streams;

type
  IHTTPRequest = Interface(IInterface)
  ['{11F5ADED-1B02-48A6-89D5-AEA4C29CD229}']
    function GetRequest: TWebRequest;
    function GetResponse: TWebResponse;
    property Request: TWebRequest read GetRequest;
    property Response: TWebResponse read GetResponse;
  end;

  TMVCISAPIEngine = class
  private
    FDefaultSession: IMVCSession;
    procedure DoOnMVCRoute(const Sender: IBaseRoute; const HttpRequest: IInterface);
    function FillMVCData(const HTTPRequest: IHTTPRequest; const Session: IMVCSession): IMVCData;
  public
    destructor Destroy; override;
    constructor Create(const SessionClass: TClass);
    property DefaultSession: IMVCSession read FDefaultSession;
    procedure ExecuteISAPIRequest(const Request: TWebRequest; const Response: TWebResponse);
  end;

implementation

type
  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  private
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    function GetRequest: TWebRequest;
    function GetResponse: TWebResponse;
  public
    constructor Create(const Request: TWebRequest;
                       const Response: TWebResponse);
    property Response: TWebResponse read GetResponse;
    property Request: TWebRequest read GetRequest;
  end;

{ TMVCISAPIEngine }

constructor TMVCISAPIEngine.Create;
begin
  FDefaultSession := TMVCSessionClass(SessionClass).Create('ISAPIDefaultSession');
  FDefaultSession.MVCFactory.Assign(AcquireMVCFactory);
  FDefaultSession.Initialize;
end;

destructor TMVCISAPIEngine.Destroy;
begin

  inherited;
end;

procedure TMVCISAPIEngine.DoOnMVCRoute(const Sender: IBaseRoute; const HttpRequest: IInterface);
var
  RedirectURL: string;
  Request: IHTTPRequest;
begin
  Request := IHTTPRequest(HttpRequest);

  FDefaultSession.InitializeRequest;
  try
    FDefaultSession.DispatchRequest(IMVCRoute(Sender), FillMVCData(Request, FDefaultSession));

    if FDefaultSession.SessionData.DispatchResult = drForbiden then
    begin
      Request.Response.StatusCode := 401;
      Exit;
    end
    else if FDefaultSession.SessionData.DispatchResult = drRedirectToLogin then
    begin
      RedirectURL := Format('/%s/%s', [FDefaultSession.AuthenticationRoute.Controller,
                                       FDefaultSession.AuthenticationRoute.AuthCommand]);
      Request.Response.Location := UTF8Encode(RedirectURL);
      Exit;
    end;

    // if all is ok then send content back
    FDefaultSession.SessionData.SessionContent.Data.Seek(0, soFromBeginning);
    Request.Response.Content := ReadFromStreamAsUTF8(FDefaultSession.SessionData.SessionContent.Data);
    Request.Response.ContentType := UTF8Encode(FDefaultSession.SessionData.SessionContent.Encoding);
    Request.Response.StatusCode := FDefaultSession.SessionData.SessionContent.ExitCode;
    Request.Response.SendResponse;
  finally
    FDefaultSession.FinalizeRequest;
  end;
end;

procedure TMVCISAPIEngine.ExecuteISAPIRequest(const Request: TWebRequest; const Response: TWebResponse);
var
  WebRoute: IBaseRoute;
  HTTPRequest: IHTTPRequest;
begin
  HTTPRequest := THTTPRequest.Create(Request, Response);
  WebRoute := WebRouting.FindRoute(string(Request.PathInfo));

  case WebRoute.RouteType of
    rtBase: raise Exception.Create('BaseRoute interface is not allowed');
    rtCustom: ICustomRoute(WebRoute).RouteProc(WebRoute, HTTPRequest);
    rtMVC: DoOnMVCRoute(WebRoute, HTTPRequest);
  end;
end;

function TMVCISAPIEngine.FillMVCData(const HTTPRequest: IHTTPRequest; const Session: IMVCSession): IMVCData;
var
  I: Integer;
  Name: string;
  Value: string;
begin
  Result := AcquireMVCData;

  for I := 0 to HTTPRequest.Request.QueryFields.Count - 1 do
  begin
    Value := HTTPRequest.Request.QueryFields.ValueFromIndex[I];
    Name := HTTPRequest.Request.QueryFields.Names[I];
    Result.Values.Ensure(Name).AsString := Value;
  end;

  for I := 0 to HTTPRequest.Request.ContentFields.Count - 1 do
  begin
    Value := HTTPRequest.Request.ContentFields.ValueFromIndex[I];
    Name := HTTPRequest.Request.ContentFields.Names[I];
    Result.Values.Ensure(Name).AsString := Value;
  end;
end;

{ THTTPRequest }

constructor THTTPRequest.Create(const Request: TWebRequest; const Response: TWebResponse);
begin
  FRequest := Request;
  FResponse := Response;
end;

function THTTPRequest.GetRequest: TWebRequest;
begin
  Result := FRequest;
end;

function THTTPRequest.GetResponse: TWebResponse;
begin
  Result := FResponse;
end;

end.
