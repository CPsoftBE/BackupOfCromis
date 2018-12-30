unit Cromis.MVC.View;

interface

uses
  Windows, SysUtils, Classes,

  // cromis units
  Cromis.MVC.Common, Cromis.MVC.Model;

type
  TUpdateViewProc = procedure(const ID: string; const Data: IMVCData) of Object;

  IMVCView = Interface(IMVCObserver)
  ['{66F56940-12A1-4A03-B8BD-CCC417000BE4}']
    function GetSessionData: ISessionData;
    procedure SetSessionData(const Value: ISessionData);
    property SessionData: ISessionData read GetSessionData write SetSessionData;
    procedure SetViewModel(const Model: IMVCModel);
    procedure Deactivate(const Data: IMVCData = nil);
    procedure Activate(const Data: IMVCData = nil);
    procedure SetViewID(const Value: string);
    function ViewModel: IMVCModel;
    function IsActive: Boolean;
    function ViewID: string;
    procedure Cleanup;
  end;

implementation

end.
