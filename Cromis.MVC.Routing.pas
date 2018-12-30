unit Cromis.MVC.Routing;

interface

uses
  Windows, SysUtils, Classes, Masks,

  // cromis units
  Cromis.StringUtils;

type
  TRouteType = (rtBase, rtMVC, rtCustom);

  IBaseRoute = interface(IInterface)
  ['{C566C15E-93D0-43B3-A83C-854088F2336C}']
    procedure Initialize;
    function GetMask: string;
    function GetRouteType: TRouteType;
    procedure SetMask(const Value: string);
    procedure SetRouteType(const Value: TRouteType);
    property Mask: string read GetMask write SetMask;
    property RouteType: TRouteType read GetRouteType write SetRouteType;
  end;

  // custom route procedure. if assigned it points to a proc to execute
  TRouteProc = procedure(const Sender: IBaseRoute; const Data: IInterface) of Object;

  ICustomRoute = interface(IBaseRoute)
  ['{E8E8BE0D-D6D6-4BAC-B724-565FA276659C}']
    function GetRouteProc: TRouteProc;
    procedure SetRouteProc(const Value: TRouteProc);
    property RouteProc: TRouteProc read GetRouteProc write SetRouteProc;
  end;

  IMVCRoute = interface(IBaseRoute)
  ['{C566C15E-93D0-43B3-A83C-854088F2336C}']
    function GetID: string;
    function GetCommand: string;
    function GetController: string;
    procedure SetID(const Value: string);
    procedure SetCommand(const Value: string);
    procedure SetController(const Value: string);
    property ID: string read GetID write SetID;
    property Command: string read GetCommand write SetCommand;
    property Controller: string read GetController write SetController;
  end;

  IWebRouting = interface(IInterface)
  ['{5CB8A82F-B787-406B-8F49-664FD882F863}']
    function GetPathRoot: string;
    procedure SetPathRoot(const Value: string);
    property PathRoot: string read GetPathRoot write SetPathRoot;
    procedure AddCustomRoute(const Mask, Controller, Command: string); overload;
    procedure AddCustomRoute(const Mask: string; const RouteProc: TRouteProc); overload;
    function FindRoute(const URL: string): IBaseRoute;
    procedure ClearAllRoutes;
  end;

  function WebRouting: IWebRouting;

implementation

var
  WebRoutingSingleton: IWebRouting;

type
  TBaseRoute = class(TInterfacedObject, IBaseRoute)
  private
    FMask: string;
    FRouteType: TRouteType;
    function GetMask: string;
    function GetRouteType: TRouteType;
    procedure SetMask(const Value: string);
    procedure SetRouteType(const Value: TRouteType);
  public
    constructor Create; virtual;
    procedure Initialize; virtual;
    property Mask: string read GetMask write SetMask;
    property RouteType: TRouteType read GetRouteType write SetRouteType;
  end;

  TCustomRoute = class(TBaseRoute, ICustomRoute)
  private
    FRouteProc: TRouteProc;
    function GetRouteProc: TRouteProc;
    procedure SetRouteProc(const Value: TRouteProc);
  public
    constructor Create; override;
    procedure Initialize; override;
    property RouteProc: TRouteProc read GetRouteProc write SetRouteProc;
  end;

  TMVCRoute = class(TBaseRoute, IMVCRoute)
  private
    FID: string;
    FCommand: string;
    FController: string;
    function GetID: string;
    function GetCommand: string;
    function GetController: string;
    procedure SetID(const Value: string);
    procedure SetCommand(const Value: string);
    procedure SetController(const Value: string);
  public
    constructor Create; override;
    procedure Initialize; override;
    property ID: string read GetID write SetID;
    property Command: string read GetCommand write SetCommand;
    property Controller: string read GetController write SetController;
  end;

  TWebRouteWrapper = class
  private
    FWebRoute: IBaseRoute;
  public
    constructor Create(const WebRoute: IBaseRoute);
    property WebRoute: IBaseRoute read FWebRoute;
  end;

  TWebRouting = class(TInterfacedObject, IWebRouting)
  private
    FPathRoot: string;
    FCustomRoutes: TStringList;
    function ParseURL(URL: string): IMVCRoute;
  public
    constructor Create;
    destructor Destroy; override;
    function GetPathRoot: string;
    procedure SetPathRoot(const Value: string);
    property PathRoot: string read GetPathRoot write SetPathRoot;
    procedure AddCustomRoute(const Mask, Controller, Command: string); overload;
    procedure AddCustomRoute(const Mask: string; const RouteProc: TRouteProc); overload;
    function FindRoute(const URL: string): IBaseRoute;
    procedure ClearAllRoutes;
  end;

function WebRouting: IWebRouting;
begin
  if WebRoutingSingleton = nil then
    WebRoutingSingleton := TWebRouting.Create;
  Result := WebRoutingSingleton;
end;

{ TMVCWebRouting }

procedure TWebRouting.AddCustomRoute(const Mask, Controller, Command: string);
var
  CustomRoute: IMVCRoute;
begin
  CustomRoute := TMVCRoute.Create;
  CustomRoute.Controller := Controller;
  CustomRoute.Command := Command;
  CustomRoute.Mask := Mask;

  FCustomRoutes.AddObject(Mask, TWebRouteWrapper.Create(CustomRoute));
end;

procedure TWebRouting.AddCustomRoute(const Mask: string; const RouteProc: TRouteProc);
var
  CustomRoute: ICustomRoute;
begin
  CustomRoute := TCustomRoute.Create;
  CustomRoute.RouteProc := RouteProc;
  CustomRoute.Mask := Mask;

  FCustomRoutes.AddObject(Mask, TWebRouteWrapper.Create(CustomRoute));
end;

procedure TWebRouting.ClearAllRoutes;
var
  I: Integer;
begin
  for I := 0 to FCustomRoutes.Count - 1 do
    FCustomRoutes.Objects[I].Free;
  FCustomRoutes.Clear;
end;

constructor TWebRouting.Create;
begin
  FCustomRoutes := TStringList.Create;
end;

destructor TWebRouting.Destroy;
begin
  FCustomRoutes.Clear;
  FreeAndNil(FCustomRoutes);

  inherited;
end;

function TWebRouting.ParseURL(URL: string): IMVCRoute;
var
  URLParts: TStringList;
begin
  Result := TMVCRoute.Create;
  Result.Mask := '/*/*/*';
  Result.Initialize;

  if Pos(FPathRoot, URL) = 1 then
    URL := StrAfter(FPathRoot, URL);

  if Length(URL) > 0 then
  begin
    URLParts := TStringList.Create;
    try
      URLParts.StrictDelimiter := True;
      URLParts.Delimiter := '/';

      case URL[1] = '/' of
        True: URLParts.DelimitedText := Copy(URL, 2, Length(URL) - 1);
        False: URLParts.DelimitedText := URL;
      end;

      if URLParts.Count > 0 then
        Result.Controller := URLParts[0]
      else
        Result.Controller := 'Home';

      if URLParts.Count > 1 then
        Result.Command := URLParts[1]
      else
        Result.Command := 'Index';

      if URLParts.Count > 2 then
      begin
        Result.ID := URLParts[2];

        if Pos(Result.ID, '?') > 0 then
          Result.ID := StrBefore('?', Result.ID);
      end;
    finally
      URLParts.Free;
    end;
  end;
end;

procedure TWebRouting.SetPathRoot(const Value: string);
begin
  FPathRoot := Value;
end;

function TWebRouting.FindRoute(const URL: string): IBaseRoute;
var
  I: Integer;
begin
  for I := 0 to FCustomRoutes.Count - 1 do
  begin
    if MatchesMask(FPathRoot + URL, FCustomRoutes[I]) then
    begin
      Result := TWebRouteWrapper(FCustomRoutes.Objects[I]).WebRoute;
      Exit;
    end;
  end;

  // make default routing
  Result := ParseURL(URL);
end;

function TWebRouting.GetPathRoot: string;
begin
  Result := FPathRoot;
end;

{ TMVCRoute }

constructor TMVCRoute.Create;
begin
  inherited;

  FRouteType := rtMVC;
end;

function TMVCRoute.GetCommand: string;
begin
  Result := FCommand;
end;

function TMVCRoute.GetController: string;
begin
  Result := FController;
end;

function TMVCRoute.GetID: string;
begin
  Result := FID;
end;

procedure TMVCRoute.Initialize;
begin
  inherited;

  FID := '';
  FCommand := '';
  FController := '';
end;

procedure TMVCRoute.SetCommand(const Value: string);
begin
  FCommand := Value;
end;

procedure TMVCRoute.SetController(const Value: string);
begin
  FController := Value;
end;

procedure TMVCRoute.SetID(const Value: string);
begin
  FID := Value
end;

{ TWebRouteWrapper }

constructor TWebRouteWrapper.Create(const WebRoute: IBaseRoute);
begin
  FWebRoute := WebRoute;
end;

{ TBaseRoute }

constructor TBaseRoute.Create;
begin
  FRouteType := rtBase;
end;

function TBaseRoute.GetMask: string;
begin
  Result := FMask;
end;

function TBaseRoute.GetRouteType: TRouteType;
begin
  Result := FRouteType;
end;

procedure TBaseRoute.Initialize;
begin
  FRouteType := rtMVC;
  FMask := '';
end;

procedure TBaseRoute.SetMask(const Value: string);
begin
  FMask := Value;
end;

procedure TBaseRoute.SetRouteType(const Value: TRouteType);
begin
  FRouteType := Value;
end;

{ TCustomRoute }

constructor TCustomRoute.Create;
begin
  inherited;

  FRouteType := rtCustom;
end;

function TCustomRoute.GetRouteProc: TRouteProc;
begin
  Result := FRouteProc;
end;

procedure TCustomRoute.Initialize;
begin
  inherited;

  FRouteProc := nil;
end;

procedure TCustomRoute.SetRouteProc(const Value: TRouteProc);
begin
  FRouteProc := Value;
end;

initialization

finalization
  WebRoutingSingleton := nil;

end.
