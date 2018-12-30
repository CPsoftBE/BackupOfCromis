unit Cromis.MVC.Sessions;

interface

uses
  Windows, SysUtils, Classes, Contnrs, DateUtils,

  // library units
  Cromis.MVC.Controller,
  Cromis.MVC.Routing,
  Cromis.MVC.Common,

  // other cromis units
  Cromis.Cryptography,
  Cromis.Scheduler,
  Cromis.Threading,
  HashList;

type
  IAuthenticationRoute = Interface(IInterface)
  ['{EE9B20C3-D8C5-469D-919C-B4B6970339FA}']
    function GetController: string;
    function GetAuthCommand: string;
    function GetLoginCommand: string;
    procedure SetController(const Value: string);
    procedure SetAuthCommand(const Value: string);
    procedure SetLoginCommand(const Value: string);
    property Controller: string read GetController write SetController;
    property AuthCommand: string read GetAuthCommand write SetAuthCommand;
    property LoginCommand: string read GetLoginCommand write SetLoginCommand;
  end;

  IMVCSession = Interface(IInterface)
  ['{BE3A976A-9B80-4606-8D84-E4748E11F536}']
    function GetID: string;
    function GetMVCFactory: IMVCFactory;
    function GetLastRequest: TDateTime;
    function GetSessionData: ISessionData;
    function GetAuthenticationRoute: IAuthenticationRoute;
    procedure DispatchRequest(const WebRoute: IMVCRoute; const Data :IMVCData);
    procedure InitializeRequest;
    procedure FinalizeRequest;
    procedure Initialize;
    procedure Finalize;
    // properties of the MVCSession interface
    property AuthenticationRoute: IAuthenticationRoute read GetAuthenticationRoute;
    property SessionData: ISessionData read GetSessionData;
    property LastRequest: TDateTime read GetLastRequest;
    property MVCFactory: IMVCFactory read GetMVCFactory;
    property ID: string read GetID;
  end;

  IDispatchData = Interface(IInterface)
  ['{DBEFDBB4-04F4-45E0-9249-405A7B4BA050}']
    function GetSession: IMVCSession;
    function GetDispatchData: IMVCData;
    property DispatchData: IMVCData read GetDispatchData;
    property Session: IMVCSession read GetSession;
  end;

  // on redirect login action
  TOnSessionDispatch = procedure(const DispatchInfo: IDispatchData) of Object;

  ISessionEvents = Interface(IInterface)
  ['{EED2B578-F936-4D5F-8063-AAC7918C4554}']
    function GetOnSessionDispatch: TOnSessionDispatch;
    procedure SetOnSessionDispatch(const Value: TOnSessionDispatch);
    property OnSessionDispatch: TOnSessionDispatch read GetOnSessionDispatch write SetOnSessionDispatch;
  end;

  TMVCSession = class(TInterfacedObject, IMVCSession)
  private
    FID: string;
    FTaskQueue: ITaskQueue;
    FMVCFactory: IMVCFactory;
    FLastRequest: TDateTime;
    FSessionData: ISessionData;
    FSessionEvents: ISessionEvents;
    FAuthenticationRoute: IAuthenticationRoute;
    function GetAuthenticationRoute: IAuthenticationRoute;
    function GetSessionData: ISessionData;
    function GetMVCFactory: IMVCFactory;
    function GetLastRequest: TDateTime;
    function GetID: string;
  public
    destructor Destroy; override;
    constructor Create(const ID: string; const SessionEvents: ISessionEvents = nil);
    procedure DispatchRequest(const WebRoute: IMVCRoute; const Data :IMVCData);
    procedure InitializeRequest; virtual;
    procedure FinalizeRequest; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // properties of the MVCSession interface
    property AuthenticationRoute: IAuthenticationRoute read GetAuthenticationRoute;
    property SessionData: ISessionData read GetSessionData;
    property LastRequest: TDateTime read GetLastRequest;
    property MVCFactory: IMVCFactory read GetMVCFactory;
    property ID: string read GetID;
  end;

  // the family of MVC sessions
  TMVCSessionClass = class of TMVCSession;

  TMVCSessionManager = class
  private
    FSessions: THashList;
    FIdleEvent: TScheduledEvent;
    FSessionCS: TRTLCriticalSection;
    FMVCFactory: IMVCFactory;
    FSessionEvents: ISessionEvents;
    procedure OnIdleSchedule(Sender: TScheduledEvent);
    function OnIdleCleanup(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
    function CreateNewSession(const ID: string; const SessionClass: TClass): IMVCSession;
    function GetSessionEvents: ISessionEvents;
    function GetMVCFactory: IMVCFactory;
  public
    constructor Create;
    destructor Destroy; override;
    function SessionCount: Integer;
    procedure ReleaseSession(const ID: string);
    function AcquireSession(const ID: string; const SessionClass: TClass): IMVCSession;
    property SessionEvents: ISessionEvents read GetSessionEvents;
    property MVCFactory: IMVCFactory read GetMVCFactory;
  end;

implementation

type
  TSessionWrapper = class
  private
    FSession: IMVCSession;
  public
    destructor Destroy; override;
    constructor Create(const Session: IMVCSession);
    property Session: IMVCSession read FSession;
  end;

  TAuthenticationRoute = class(TInterfacedObject, IAuthenticationRoute)
  private
    FController: string;
    FAuthCommand: string;
    FLoginCommand: string;
    function GetController: string;
    function GetAuthCommand: string;
    function GetLoginCommand: string;
    procedure SetController(const Value: string);
    procedure SetAuthCommand(const Value: string);
    procedure SetLoginCommand(const Value: string);
  public
    property Controller: string read GetController write SetController;
    property AuthCommand: string read GetAuthCommand write SetAuthCommand;
    property LoginCommand: string read GetLoginCommand write SetLoginCommand;
  end;

  TDispatchData = class(TInterfacedObject, IDispatchData)
  private
    FSession: IMVCSession;
    FDispatchData: IMVCData;
    function GetSession: IMVCSession;
    function GetDispatchData: IMVCData;
  public
    constructor Create(const DispatchData: IMVCData; const Session: IMVCSession);
    property DispatchData: IMVCData read GetDispatchData;
    property Session: IMVCSession read GetSession;
  end;

  TSessionEvents = class(TInterfacedObject, ISessionEvents)
  private
    FOnSessionDispatch: TOnSessionDispatch;
    function GetOnSessionDispatch: TOnSessionDispatch;
    procedure SetOnSessionDispatch(const Value: TOnSessionDispatch);
  public
    property OnSessionDispatch: TOnSessionDispatch read GetOnSessionDispatch write SetOnSessionDispatch;
  end;

{ TMVCSessionManager }

function TMVCSessionManager.AcquireSession(const ID: string; const SessionClass: TClass): IMVCSession;
var
  SessionWrapper: TSessionWrapper;
begin
  SessionWrapper := TSessionWrapper(FSessions.Data[ID]);

  // session was not found
  if SessionWrapper = nil then
  begin
    EnterCriticalSection(FSessionCS);
    try
      Result := CreateNewSession(ID, SessionClass);
      Result.Initialize;
    finally
      LeaveCriticalSection(FSessionCS);
    end;
  end
  else
    Result := SessionWrapper.Session;
end;

constructor TMVCSessionManager.Create;
begin
  InitializeCriticalSection(FSessionCS);
  FSessionEvents := TSessionEvents.Create;
  FMVCFactory := AcquireMVCFactory;

  FSessions := THashList.Create(CaseInsensitiveTraits, 1000);
  FSessions.OwnsObjets := True;

  FIdleEvent := TScheduledEvent.Create('OnIdleCleanup');
  FIdleEvent.Schedule.SetEventInterval(0,0,0,0,5,0);
  FIdleEvent.OnSchedule := OnIdleSchedule;
  FIdleEvent.Run;
end;

function TMVCSessionManager.CreateNewSession(const ID: string; const SessionClass: TClass): IMVCSession;
var
  SessionWrapper: TSessionWrapper;
begin
  SessionWrapper := TSessionWrapper.Create(TMVCSessionClass(SessionClass).Create(GetNewFormatedGUID, FSessionEvents));
  FSessions.Add(SessionWrapper.Session.ID, SessionWrapper);
  SessionWrapper.Session.MVCFactory.Assign(FMVCFactory);
  Result := SessionWrapper.Session;
end;

destructor TMVCSessionManager.Destroy;
begin
  DeleteCriticalSection(FSessionCS);
  FreeAndNil(FIdleEvent);
  FreeAndNil(FSessions);
  FMVCFactory.Cleanup;
  FMVCFactory := nil;

  inherited;
end;

function TMVCSessionManager.GetMVCFactory: IMVCFactory;
begin
  Result := FMVCFactory;
end;

function TMVCSessionManager.GetSessionEvents: ISessionEvents;
begin
  Result := FSessionEvents;
end;

function TMVCSessionManager.SessionCount: Integer;
begin
  EnterCriticalSection(FSessionCS);
  try
    Result := FSessions.Count;
  finally
    LeaveCriticalSection(FSessionCS);
  end;
end;

function TMVCSessionManager.OnIdleCleanup(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
begin
  Result := True;
  try
    if SecondsBetween(TSessionWrapper(APtr).Session.LastRequest, Now) > 300 then
      TObjectList(AUserData).Add(TSessionWrapper(APtr));
  except
    Result := False;
  end;
end;

procedure TMVCSessionManager.OnIdleSchedule(Sender: TScheduledEvent);
var
  I: Integer;
  IdleCleanList: TObjectList;
  SessionWrapper: TSessionWrapper;
begin
  EnterCriticalSection(FSessionCS);
  try
    IdleCleanList := TObjectList.Create(False);
    try
      FSessions.IterateMethod(IdleCleanList, OnIdleCleanup);
    finally
      try
        for I := IdleCleanList.Count - 1 downto 0 do
        begin
          SessionWrapper := TSessionWrapper(IdleCleanList[I]);
          FSessions.Remove(SessionWrapper.Session.ID);
          IdleCleanList[I].Free;
        end;
      finally
        IdleCleanList.Free;
      end;
    end;
  finally
    LeaveCriticalSection(FSessionCS);
  end;
end;

procedure TMVCSessionManager.ReleaseSession(const ID: string);
var
  SessionWrapper: TSessionWrapper;
begin
  EnterCriticalSection(FSessionCS);
  try
    SessionWrapper := FSessions.Data[ID];
    FSessions.Remove(ID);
    SessionWrapper.Free;
  finally
    LeaveCriticalSection(FSessionCS);
  end;
end;

{ TMVCWebSession }

constructor TMVCSession.Create(const ID: string; const SessionEvents: ISessionEvents);
begin
  FAuthenticationRoute := TAuthenticationRoute.Create;
  FSessionData := AcquireSessionData(ID);
  FTaskQueue := AcquireTaskQueue;

  // create the MVC factory with session data attached
  FMVCFactory := AcquireMVCFactory(FSessionData);

  // passed on data from manager
  FSessionEvents := SessionEvents;
  FID := ID;
end;

destructor TMVCSession.Destroy;
begin
  FMVCFactory.Cleanup;
  FMVCFactory := nil;

  inherited;
end;

procedure TMVCSession.Finalize;
begin
  // virtual stub
end;

procedure TMVCSession.FinalizeRequest;
begin
  SessionData.SessionContent.Encoding := '';
  SessionData.SessionContent.Clear;
end;

function TMVCSession.GetAuthenticationRoute: IAuthenticationRoute;
begin
  Result := FAuthenticationRoute;
end;

function TMVCSession.GetID: string;
begin
  Result := FID;
end;

function TMVCSession.GetLastRequest: TDateTime;
begin
  Result := FLastRequest;
end;

function TMVCSession.GetMVCFactory: IMVCFactory;
begin
  Result := FMVCFactory;
end;

function TMVCSession.GetSessionData: ISessionData;
begin
  Result := FSessionData;
end;

procedure TMVCSession.Initialize;
begin
  FLastRequest := Now;
end;

procedure TMVCSession.InitializeRequest;
begin
  SessionData.SessionContent.Encoding := 'text/html';
end;

procedure TMVCSession.DispatchRequest(const WebRoute: IMVCRoute; const Data: IMVCData);
var
  DispatchData: IDispatchData;
  MVCController: IMVCController;
  AuthController: IMVCController;
begin
  FTaskQueue.EnqueueTask.WaitFor;
  try
    SessionData.SessionContent.ExitCode := 200;
    SessionData.DispatchResult := drSuccess;
    FLastRequest := Now;

    if not FSessionData.Authenticated then
    begin
      if SameText(FAuthenticationRoute.Controller, WebRoute.Controller) and
         SameText(FAuthenticationRoute.AuthCommand, WebRoute.Command) then
      begin
        SessionData.DispatchResult := drDoLogin;
        AuthController := MVCFactory.GetControler(AuthenticationRoute.Controller);
        AuthController.GetModel.ExecuteCommand(AuthenticationRoute.AuthCommand, WebRoute.ID, Data);
      end
      else
      begin
        SessionData.DispatchResult := drRedirectToLogin;
        AuthController := MVCFactory.GetControler(AuthenticationRoute.Controller);
        AuthController.GetModel.ExecuteCommand(AuthenticationRoute.LoginCommand, WebRoute.ID, Data);
      end;
    end
    else
    begin
      // try to get the controller if it exists
      MVCController := MVCFactory.GetControler(WebRoute.Controller);

      if MVCController = nil then
        MVCController := MVCFactory.DefaultControler;
        
      if MVCController = nil then
      begin
         SessionData.DispatchResult := drFailed;
         Exit;
      end;

      // normal MVC action for the session
      MVCController.GetModel.ExecuteCommand(WebRoute.Command, WebRoute.ID, Data);
    end;

    // inform the listener of the session dispatch
    if (FSessionEvents <> nil) and Assigned(FSessionEvents.OnSessionDispatch) then
    begin
      DispatchData := TDispatchData.Create(Data, Self);
      FSessionEvents.OnSessionDispatch(DispatchData);
    end;
  finally
    FTaskQueue.DequeueTask;
  end;
end;

{ TSessionWrapper }

constructor TSessionWrapper.Create(const Session: IMVCSession);
begin
  FSession := Session;
end;

destructor TSessionWrapper.Destroy;
begin
  FSession := nil;

  inherited;
end;

{ TAuthenticationRoute }

function TAuthenticationRoute.GetAuthCommand: string;
begin
  Result := FAuthCommand;
end;

function TAuthenticationRoute.GetController: string;
begin
  Result := FController;
end;

function TAuthenticationRoute.GetLoginCommand: string;
begin
  Result := FLoginCommand;
end;

procedure TAuthenticationRoute.SetAuthCommand(const Value: string);
begin
  FAuthCommand := Value;
end;

procedure TAuthenticationRoute.SetController(const Value: string);
begin
  FController := Value;
end;

procedure TAuthenticationRoute.SetLoginCommand(const Value: string);
begin
  FLoginCommand := Value;
end;

{ TSessionEvents }

function TSessionEvents.GetOnSessionDispatch: TOnSessionDispatch;
begin
  Result := FOnSessionDispatch;
end;

procedure TSessionEvents.SetOnSessionDispatch(const Value: TOnSessionDispatch);
begin
  FOnSessionDispatch := Value;
end;

{ TDispatchData }

constructor TDispatchData.Create(const DispatchData: IMVCData; const Session: IMVCSession);
begin
  FDispatchData := DispatchData;
  FSession := Session;
end;

function TDispatchData.GetDispatchData: IMVCData;
begin
  Result := FDispatchData;
end;

function TDispatchData.GetSession: IMVCSession;
begin
  Result := FSession;
end;

end.

