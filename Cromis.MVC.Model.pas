unit Cromis.MVC.Model;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs,

  // cromis units
  Cromis.MVC.Common,
  Cromis.MVC.Events;

type
  TCommandMethod = procedure(const ID: string; const Data, Result: IMVCData) of object;

  IMVCModel = Interface(IInterface)
  ['{16479395-6F70-42D4-AC43-B4DF14E5499E}']
    function GetSessionData: ISessionData;
    procedure SetSessionData(const Value: ISessionData);
    property SessionData: ISessionData read GetSessionData write SetSessionData;
    procedure ExecuteCommand(const Name, ID: string; const Data: IMVCData = nil); overload;
    procedure ExecuteCommand(const Name: string; const Data: IMVCData = nil); overload;
    procedure UnregisterObserver(const Observer: IMVCObserver);
    procedure RegisterObserver(const Observer: IMVCObserver);
  end;

implementation

end.
