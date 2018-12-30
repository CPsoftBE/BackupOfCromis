unit Cromis.MVC.Events;

interface

uses
  SysUtils, Contnrs, Classes,

  // cromis MVC units
  Cromis.MVC.Common;

type
  TMVCEvent = (meRequestStarted, meRequestComplete, meCommandStarted, meCommandComplete,
               meViewActivated, meViewDeactivated, meViewUpdated);
  TOnMVCEvent = procedure(const Event: TMVCEvent;
                          const ActionID: string;
                          const Controler: string;
                          const EventName: string;
                          const EventData: IMVCData
                          ) of Object;
  TEventFilter = set of TMVCEvent;

const
  AllEvents = [Low(TMVCEvent)..High(TMVCEvent)];

type
  TObserver = class
    ID: Integer;
    Method: TMethod;
    Filter: TEventFilter;
  end;

  TMVCNotifications = class
  private
    FIDCounter: Integer;
    FObservers: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Notify(const Event: TMVCEvent;
                     const ActionID: string;
                     const Controler: string;
                     const EventName: string;
                     const EventData: IMVCData);
    function AddNotificationHandler(const HandlerProc: TOnMVCEvent;
      const Owner: TObject; const Filter: TEventFilter = AllEvents): Integer;
    function RemoveNotificationHandler(const NotificationID: Integer): Boolean;
  end;

var
  MVCNotifications: TMVCNotifications;

implementation

{ TMVCNotifications }

constructor TMVCNotifications.Create;
begin
  FObservers := TObjectList.Create;
end;

destructor TMVCNotifications.Destroy;
begin
  FreeAndNil(FObservers);

  inherited;
end;

function TMVCNotifications.AddNotificationHandler(const HandlerProc: TOnMVCEvent;
  const Owner: TObject; const Filter: TEventFilter): Integer;
var
  Observer: TObserver;
begin
  Inc(FIDCounter);

  Observer := TObserver.Create;
  Observer.Method.Code := @HandlerProc;
  Observer.Method.Data := Owner;
  Observer.Filter := Filter;
  Observer.ID := FIDCounter;
  FObservers.Add(Observer);
  Result := FIDCounter;
end;

procedure TMVCNotifications.Notify(const Event: TMVCEvent;
                                   const ActionID: string;
                                   const Controler: string;
                                   const EventName: string;
                                   const EventData: IMVCData);
var
  I: Integer;
  MVCEvent: TOnMVCEvent;
begin
  for I := 0 to FObservers.Count - 1 do
  begin
    if Event in TObserver(FObservers[I]).Filter then
    begin
      MVCEvent := TOnMVCEvent(TObserver(FObservers[I]).Method);
      MVCEvent(Event, ActionID, Controler, EventName, EventData);
    end;
  end;
end;

function TMVCNotifications.RemoveNotificationHandler(const NotificationID: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FObservers.Count - 1 do
  begin
    if TObserver(FObservers[I]).ID = NotificationID then
    begin
      FObservers.Remove(FObservers[I]);
      Result := True;
      Exit;
    end;
  end;
end;

initialization
  MVCNotifications := TMVCNotifications.Create;

finalization
  FreeAndNil(MVCNotifications);

end.
