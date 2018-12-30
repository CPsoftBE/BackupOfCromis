unit Cromis.MVC.Controller;

interface

uses
  Windows, SysUtils, Classes,

  // MVC cromis units
  Cromis.MVC.Common,
  Cromis.MVC.Events,
  Cromis.MVC.Model,
  Cromis.MVC.View,

  // other cromis units
  Cromis.Cryptography;

type
  IMVCController = Interface(IInterface)
  ['{748A20D1-8CE7-4E4C-A930-D9A9A59620AB}']
    // getters and setters
    function GetName: string;
    // properties of the interface
    property Name: string read GetName;
    // functions and procedure  of the interface
    function GetModel: IMVCModel;
    function GetView(const Name: string): IMVCView;
    procedure RegisterView(const Name: string; const View: IMVCView);
    procedure DeactivateViews(const Data: IMVCData = nil);
    procedure Cleanup;
  end;

  // dummy definition
  ViewClass = class of TObject;

  IMVCFactory = Interface(IInterface)
  ['{297F0EC8-E06F-4FC6-BC12-6A631F6BEFB7}']
    function GetDefaultControler: IMVCController;
    procedure SetDefaultControler(const Value: IMVCController);
    property DefaultControler: IMVCController read GetDefaultControler write SetDefaultControler;
    function RegisterControler(const Name: string; const Model: TClass; const Default: Boolean = False): IMVCController;
    procedure DeactivateView(const Controler, View: string; const Data: IMVCData = nil);
    procedure ActivateView(const Controler, View: string; const Data: IMVCData = nil);
    function GetControler(const Index: Integer): IMVCController; overload;
    function GetControler(const Name: string): IMVCController; overload;
    procedure Assign(const Factory: IMVCFactory);
    function ControllerCount: Integer;
    procedure Cleanup;
  end;

  // acuires the main factory interface
  function AcquireMVCFactory(const SessionData: ISessionData = nil): IMVCFactory;
  function AcquireEventID: string;

implementation

uses
  Cromis.MVC.Model.Impl;

type
  TViewWrapper = class
  private
    FView: IMVCView;
  public
    destructor Destroy; override;
    constructor Create(const View: IMVCView);
    property View: IMVCView read FView;
  end;

  TMVCControler = class(TInterfacedObject, IMVCController)
  private
    FName: string;
    FModel: IMVCModel;
    FOwner: IMVCFactory;
    FViews: TStringList;
    function GetName: string;
  public
    constructor Create(const Owner: IMVCFactory;
                       const Model: TClass;
                       const Name: string);
    // properties of the interface
    property Name: string read GetName;
    // functions and procedure  of the interface
    function GetView(const Name: string): IMVCView;
    procedure RegisterView(const Name: string; const View: IMVCView);
    procedure DeactivateViews(const Data: IMVCData = nil);
    function GetModel: IMVCModel;
    procedure Cleanup;
  end;

  TMVCFactory = class(TInterfacedObject, IMVCFactory)
  private
    FSessionData: ISessionData;
    FControlerList: TInterfaceList;
    FDefaultControler: IMVCController;
    function GetDefaultControler: IMVCController;
    procedure SetDefaultControler(const Value: IMVCController);
  public
    destructor Destroy; override;
    procedure Assign(const Factory: IMVCFactory);
    constructor Create(const SessionData: ISessionData);
    property DefaultControler: IMVCController read GetDefaultControler write SetDefaultControler;
    function RegisterControler(const Name: string; const Model: TClass; const Default: Boolean = False): IMVCController;
    procedure DeactivateView(const Controler, View: string; const Data: IMVCData = nil);
    procedure ActivateView(const Controler, View: string; const Data: IMVCData = nil);
    function GetControler(const Index: Integer): IMVCController; overload;
    function GetControler(const Name: string): IMVCController; overload;
    function ControllerCount: Integer;
    procedure Cleanup;
  end;

function AcquireEventID: string;
begin
  Result := GetNewFormatedGUID;
end;

function AcquireMVCFactory(const SessionData: ISessionData): IMVCFactory;
begin
  Result := TMVCFactory.Create(SessionData);
end;

{ TMVCControler }

procedure TMVCControler.Cleanup;
var
  I: Integer;
begin
  for I := 0 to FViews.Count - 1 do
  begin
    TViewWrapper(FViews.Objects[I]).View.Cleanup;
    FViews.Objects[I].Free;
  end;

  FreeAndNil(FViews);
  FModel := nil;
end;

constructor TMVCControler.Create(const Owner: IMVCFactory;
                                 const Model: TClass;
                                 const Name: string);
begin
  FName := Name;
  // owner factory
  FOwner := Owner;
  // create views hash table
  FViews := TStringList.Create;
  // create the model from class
  FModel := TModelClass(Model).Create(FName, FOwner);
end;

function TMVCControler.GetModel: IMVCModel;
begin
  Result := FModel;
end;

function TMVCControler.GetView(const Name: string): IMVCView;
begin
  case FViews.IndexOf(Name) > -1 of
    True: Result := TViewWrapper(FViews.Objects[FViews.IndexOf(Name)]).View;
    False: Result := nil;
  end;
end;

procedure TMVCControler.DeactivateViews(const Data: IMVCData);
var
  I: Integer;
begin
  for I := 0 to FViews.Count - 1 do
    TViewWrapper(FViews.Objects[I]).View.Deactivate(Data);
end;

procedure TMVCControler.RegisterView(const Name: string; const View: IMVCView);
begin
  FViews.AddObject(Name, TViewWrapper.Create(View));
  View.SessionData := FModel.SessionData;
  View.SetViewModel(FModel);
  View.SetViewID(Name);

  // register the view as observer
  FModel.RegisterObserver(View);
end;

function TMVCControler.GetName: string;
begin
  Result := FName;
end;

{ TMVCFactory }

constructor TMVCFactory.Create(const SessionData: ISessionData);
begin
  FControlerList := TInterfaceList.Create;
  FSessionData := SessionData;
end;

procedure TMVCFactory.DeactivateView(const Controler, View: string; const Data: IMVCData);
var
  MVCControler: IMVCController;
begin
  MVCControler := GetControler(Controler);

  if MVCControler <> nil then
  begin
    MVCControler.GetView(View).Deactivate(Data);
    MVCNotifications.Notify(meViewDeactivated, AcquireEventID, Controler, View, Data);
  end;
end;

destructor TMVCFactory.Destroy;
begin
  FreeAndNil(FControlerList);

  inherited;
end;

function TMVCFactory.GetControler(const Index: Integer): IMVCController;
begin
  if Index < FControlerList.Count then
    Result := IMVCController(FControlerList[Index])
  else
    Result := nil;
end;

function TMVCFactory.GetControler(const Name: string): IMVCController;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FControlerList.Count - 1 do
  begin
    if IMVCController(FControlerList[I]).Name = Name then
    begin
      Result := IMVCController(FControlerList[I]);
      Exit;
    end;
  end;
end;

function TMVCFactory.GetDefaultControler: IMVCController;
begin
  Result := FDefaultControler;
end;

procedure TMVCFactory.ActivateView(const Controler, View: string; const Data: IMVCData = nil);
var
  MVCControler: IMVCController;
begin
  MVCControler := GetControler(Controler);

  if MVCControler <> nil then
  begin
    // signal the view change to listeners
    MVCControler.GetView(View).Activate(Data);
    MVCNotifications.Notify(meViewActivated, AcquireEventID, Controler, View, Data);
  end;
end;

procedure TMVCFactory.Assign(const Factory: IMVCFactory);
var
  I: Integer;
begin
  for I := 0 to Factory.ControllerCount - 1 do
    FControlerList.Add(Factory.GetControler(I));
end;

procedure TMVCFactory.Cleanup;
var
  I: Integer;
begin
  for I := 0 to FControlerList.Count - 1 do
    IMVCController(FControlerList[I]).Cleanup;

  FControlerList.Clear;
end;

function TMVCFactory.ControllerCount: Integer;
begin
  Result := FControlerList.Count;
end;

function TMVCFactory.RegisterControler(const Name: string;
                                       const Model: TClass;
                                       const Default: Boolean
                                       ): IMVCController;
begin
  Result := TMVCControler.Create(Self, Model, Name);
  Result.GetModel.SessionData := FSessionData;
  FControlerList.Add(Result);

  if Default then
    FDefaultControler := Result;
end;

procedure TMVCFactory.SetDefaultControler(const Value: IMVCController);
begin
  FDefaultControler := Value;
end;

{ TViewWrapper }

constructor TViewWrapper.Create(const View: IMVCView);
begin
  FView := View;
end;

destructor TViewWrapper.Destroy;
begin
  FView := nil;

  inherited;
end;

end.
