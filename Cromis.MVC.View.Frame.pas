unit Cromis.MVC.View.Frame;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs,

  // cromis units
  Cromis.MVC.View, Cromis.MVC.Common, Cromis.MVC.Model;

type
  TMVCFrameView = class(TFrame, IMVCView)
  private
    FViewID: string;
    FIsActive: Boolean;
    FViewModel: IMVCModel;
    FSessionData: ISessionData;
    function GetSessionData: ISessionData;
    procedure SetSessionData(const Value: ISessionData);
  protected
    procedure DoOnViewDeactivated(const Data: IMVCData); virtual;
    procedure DoOnViewActivated(const Data: IMVCData); virtual;
  public
    property SessionData: ISessionData read GetSessionData write SetSessionData;
    procedure UpdateView(const Section, ID: string; const Data: IMVCData); virtual;
    procedure Deactivate(const Data: IMVCData = nil); virtual;
    procedure Activate(const Data: IMVCData = nil); virtual;
    procedure SetViewModel(const Model: IMVCModel);
    procedure SetViewID(const Value: string);
    function ViewModel: IMVCModel;
    function IsActive: Boolean;
    function ViewID: string;
    procedure Cleanup;
  end;

implementation

{$R *.dfm}

{ TMVCFrameView }

procedure TMVCFrameView.Activate(const Data: IMVCData);
var
  I: Integer;
begin
  for I := 0 to Parent.ControlCount - 1 do
  begin
    if Parent.Controls[I].ClassParent = TMVCFrameView then
    begin
      case Parent.Controls[I] <> Self of
        True: Parent.Controls[I].Visible := False;
        False: Parent.Controls[I].Visible := True;
      end;
    end;
  end;

  // now we are active
  FIsActive := True;
  // show frame
  Self.Show;

  // notify of activation
  DoOnViewActivated(Data);
end;

procedure TMVCFrameView.Cleanup;
begin
  FViewModel := nil;
end;

procedure TMVCFrameView.Deactivate(const Data: IMVCData);
begin
  FIsActive := False;
  Self.Hide;

  // notify of deactivation
  DoOnViewDeactivated(Data);
end;

procedure TMVCFrameView.DoOnViewActivated(const Data: IMVCData);
begin
  Align := alClient;
end;

procedure TMVCFrameView.DoOnViewDeactivated(const Data: IMVCData);
begin
  Align := alNone;
end;

function TMVCFrameView.GetSessionData: ISessionData;
begin
  Result := FSessionData;
end;

function TMVCFrameView.IsActive: Boolean;
begin
  Result := FIsActive
end;

function TMVCFrameView.ViewModel: IMVCModel;
begin
  Result := FViewModel;
end;

procedure TMVCFrameView.SetSessionData(const Value: ISessionData);
begin
  FSessionData := Value;
end;

procedure TMVCFrameView.SetViewID(const Value: string);
begin
  FViewID := Value;
end;

procedure TMVCFrameView.SetViewModel(const Model: IMVCModel);
begin
  FViewModel := Model;
end;

procedure TMVCFrameView.UpdateView(const Section, ID: string; const Data: IMVCData);
var
  UpdateMethod: TUpdateViewProc;
begin
  TMethod(UpdateMethod).Code := Self.MethodAddress(Format('UpdateView_%s', [Section]));

  if TMethod(UpdateMethod).Code <> nil then
  begin
    TMethod(UpdateMethod).Data := Self;
    UpdateMethod(ID, Data);
  end;
end;

function TMVCFrameView.ViewID: string;
begin
  Result := FViewID;
end;

end.
