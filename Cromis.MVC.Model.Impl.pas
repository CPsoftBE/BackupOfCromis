unit Cromis.MVC.Model.Impl;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs,

  // cromis units
  Cromis.MVC.Model,
  Cromis.MVC.Common,
  Cromis.MVC.Events,
  Cromis.MVC.Controller;

type
  TMVCModel = class(TInterfacedObject, IMVCModel)
  private
    FWinHandle: Cardinal;
    FMVCFactory: IMVCFactory;
    FUpdateCount: Integer;
    FControllerID: string;
    FSessionData: ISessionData;
    FLastRequests: TStringList;
    FObserverList: TInterfaceList;
    procedure DeallocateHWnd(Wnd: HWND);
    function GetSessionData: ISessionData;
    procedure SetSessionData(const Value: ISessionData);
    //procedure WndProc(var Msg: TMessage);
    procedure NotifyUpdate(const Command, ID: string; const Data: IMVCData);
    procedure EndUpdate(const Command, ID: string; const Data: IMVCData);
    procedure BeginUpdate;
  protected
    //function ExecuteRequest(const Data: IMVCData; const RequestClass: TClass; const Name: string = ''): string;
    procedure SendCommand(const Controler, Command, ID: string; const Data: IMVCData = nil); overload;
    procedure SendCommand(const Controler, Command: string; const Data: IMVCData = nil); overload;
    procedure ActivateView(const Controler, View: string; const Data: IMVCData = nil); overload;
    procedure ActivateView(const View: string; const Data: IMVCData = nil); overload;
    //procedure DoOnRequestBegin(const RequestData: IRequestData); virtual;
    //procedure DoOnRequestDone(const RequestData: IRequestData); virtual;
    property MVCFactory: IMVCFactory read FMVCFactory;
  public
    destructor Destroy; override;
    property SessionData: ISessionData read GetSessionData write SetSessionData;
    constructor Create(const ControllerID: string; const MVCFactory: IMVCFactory); virtual;
    procedure ExecuteCommand(const Name, ID: string; const Data: IMVCData = nil); overload;
    procedure ExecuteCommand(const Name: string; const Data: IMVCData = nil); overload;
    procedure UnregisterObserver(const Observer: IMVCObserver);
    procedure RegisterObserver(const Observer: IMVCObserver);
  end;

  // class definition for controler
  TModelClass = class of TMVCModel;

implementation

{ TMVCModel }

constructor TMVCModel.Create(const ControllerID: string; const MVCFactory: IMVCFactory);
begin
  FObserverList := TInterfaceList.Create;
  FLastRequests := TStringList.Create;
  FControllerID := ControllerID;
  FMVCFactory := MVCFactory;
  FUpdateCount := 0;

  // create the dummy window for messages
  //FWinHandle := AllocateHWND(WndProc);
end;

procedure TMVCModel.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  if Instance <> @DefWindowProc then
  begin
    { make sure we restore the default
      windows procedure before freeing memory }
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;
  DestroyWindow(Wnd);
end;

destructor TMVCModel.Destroy;
begin
  DeallocateHWnd(FWinHandle);

  FreeAndNil(FObserverList);
  FreeAndNil(FLastRequests);

  inherited;
end;

procedure TMVCModel.EndUpdate(const Command, ID: string; const Data: IMVCData);
begin
  Dec(FUpdateCount);

  if FUpdateCount = 0 then
    NotifyUpdate(Command, ID, Data);
end;

procedure TMVCModel.ExecuteCommand(const Name: string; const Data: IMVCData);
begin
  ExecuteCommand(Name, '', Data);
end;

procedure TMVCModel.ActivateView(const Controler, View: string; const Data: IMVCData);
begin
  FMVCFactory.ActivateView(Controler, View, Data);
end;

procedure TMVCModel.ActivateView(const View: string; const Data: IMVCData);
begin
  FMVCFactory.ActivateView(FControllerID, View, Data);
end;

procedure TMVCModel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

{
procedure TMVCModel.DoOnRequestBegin(const RequestData: IRequestData);
begin
  // nothing to do here
end;

procedure TMVCModel.DoOnRequestDone(const RequestData: IRequestData);
begin
  // nothing to do here
end;
}

procedure TMVCModel.ExecuteCommand(const Name, ID: string; const Data: IMVCData);
var
  CommandID: string;
  CommandResult: IMVCData;
  CommandMethod: TCommandMethod;
begin
  BeginUpdate;
  try
    CommandID := AcquireEventID;

    MVCNotifications.Notify(meCommandStarted, CommandID, FControllerID, Name, Data);
    try
      TMethod(CommandMethod).Code := Self.MethodAddress(Format('Command_%s', [Name]));
      CommandResult := AcquireMVCData;

      if TMethod(CommandMethod).Code <> nil then
      begin
        TMethod(CommandMethod).Data := Self;
        CommandMethod(ID, Data, CommandResult);
      end;
    finally
      MVCNotifications.Notify(meCommandComplete, CommandID, FControllerID, Name, CommandResult);
    end;
  finally
    EndUpdate(Name, ID, CommandResult);
  end;
end;

function TMVCModel.GetSessionData: ISessionData;
begin
  Result := FSessionData;
end;

{
function TMVCModel.ExecuteRequest(const Data: IMVCData; const RequestClass: TClass; const Name: string): string;
var
  Request: TMVCRequest;
begin
  Request := RequestPool.ExecuteRequest(Data, FWinHandle, RequestClass, Name);
  Result := Request.RequestID;

  if Name <> '' then
    FLastRequests.Values[Name] := Result;
end;
}

procedure TMVCModel.NotifyUpdate(const Command, ID: string; const Data: IMVCData);
var
  I: Integer;
begin
  if Data.UpdateViews then
  begin
    for I := 0 to FObserverList.Count - 1 do
    begin
      if not Data.ActiveOnly or IMVCObserver(FObserverList[I]).IsActive then
      begin
        IMVCObserver(FObserverList[I]).UpdateView(Command, ID, Data);
        MVCNotifications.Notify(meViewUpdated, AcquireEventID, FControllerID, Command, Data);
      end;
    end;
  end;
end;

procedure TMVCModel.RegisterObserver(const Observer: IMVCObserver);
begin
  FObserverList.Add(Observer);
end;

procedure TMVCModel.SendCommand(const Controler, Command, ID: string; const Data: IMVCData);
begin
  FMVCFactory.GetControler(Controler).GetModel.ExecuteCommand(Command, ID, Data);
end;

procedure TMVCModel.SendCommand(const Controler, Command: string; const Data: IMVCData);
begin
  SendCommand(Controler, Command, '', Data);
end;

procedure TMVCModel.SetSessionData(const Value: ISessionData);
begin
  FSessionData := Value;
end;

procedure TMVCModel.UnregisterObserver(const Observer: IMVCObserver);
begin
  FObserverList.Remove(Observer);
end;

(*
procedure TMVCModel.WndProc(var Msg: TMessage);
var
  RequestData: IRequestData;
  Parameters: ISimpleStorage;
begin
  if (Msg.Msg >= WM_REQUEST_STARTED) and (Msg.Msg <= WM_REQUEST_FINISHED) then
  begin
    Parameters := ISimpleStorage(Msg.LParam);
    RequestData := AcquireRequestData(Parameters.Get('Status').AsStringDef,
                                      ISimpleStorage(Msg.WParam),
                                      Parameters.Get('Name').AsStringDef,
                                      Parameters.Get('ID').AsStringDef);

    if FLastRequests.Values[RequestData.Name] = RequestData.ID then
    begin
      RequestData.Result := ModelData;

      case Msg.Msg of
        WM_REQUEST_FINISHED:
          begin
            RequestData.UpdateViews := True;
            DoOnRequestDone(RequestData);

            MVCNotifications.Notify(meRequestComplete,
                                    RequestData.ID,
                                    FControler.Name,
                                    RequestData.Name,
                                    RequestData.RootNode);
          end;
        WM_REQUEST_STARTED:
          begin
            RequestData.UpdateViews := False;
            DoOnRequestBegin(RequestData);

            MVCNotifications.Notify(meRequestStarted,
                                    RequestData.ID,
                                    FControler.Name,
                                    RequestData.Name,
                                    RequestData.RootNode)
          end;
      end;

      // notify all views
      if RequestData.UpdateViews then
        NotifyUpdate(RequestData.Name, RequestData.Result);
    end;
  end
  else
    {
     for all other messages call
     the default window procedure
    }
    Msg.Result := DefWindowProc(FWinHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;
*)

end.
