unit Cromis.MVC.View.HTML;

interface

uses
  SysUtils, Classes,

  Cromis.PageProducer,

  // cromis units
  Cromis.MVC.View, Cromis.MVC.Common, Cromis.MVC.Model;

type
  TMVCViewHTML = class(TInterfacedObject, IMVCView)
  private
    FViewID: string;
    FIsActive: Boolean;
    FViewModel: IMVCModel;
    FContentType: string;
    FSessionData: ISessionData;
    function GetSessionData: ISessionData;
    procedure SetSessionData(const Value: ISessionData);
  protected
    procedure ProduceContentPage(const Data: IMVCData;
                                 const FileName: string;
                                 const OnContentTag: TContentTagEvent);
    procedure DoOnViewActivated(const Data: IMVCData); virtual;
    property ContentType: string read FContentType;
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

{ TMVCViewPlain }

procedure TMVCViewHTML.Activate(const Data: IMVCData);
begin
  FIsActive := True;
  // notify of activation
  DoOnViewActivated(Data);
end;

procedure TMVCViewHTML.Cleanup;
begin
  FViewModel := nil;
end;

procedure TMVCViewHTML.Deactivate(const Data: IMVCData);
begin
  FIsActive := False;
end;

procedure TMVCViewHTML.DoOnViewActivated(const Data: IMVCData);
begin
  // nothing to do
end;

function TMVCViewHTML.GetSessionData: ISessionData;
begin
  Result := FSessionData;
end;

function TMVCViewHTML.IsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TMVCViewHTML.ProduceContentPage(const Data: IMVCData;
                                          const FileName: string;
                                          const OnContentTag: TContentTagEvent);
var
  I: Integer;
  Name: string;
  FS: TFileStream;
  PageProducer: TPageProducerEx;
  SessionContent: ISessionContent;
begin
  PageProducer := TPageProducerEx.Create;
  try
    SessionContent := SessionData.SessionContent;

    // map MVCData parameters to page parameters
    for I := 0 to Data.Parameters.Count - 1 do
    begin
      Name := Data.Parameters.Items[I].Name;
      PageProducer.PageParams.Values[Name] := Data.Parameters.Items[I].AsString;
    end;

    FS := TFileStream.Create(SessionContent.HTMLRoot + FileName, fmOpenRead or fmShareDenyNone);
    try
      PageProducer.SourceStream := FS;
      PageProducer.OnContentTag := OnContentTag;
      PageProducer.ProduceContent(SessionContent.Data);
    finally
      FS.Free;
    end;
  finally
    PageProducer.Free;
  end;
end;

procedure TMVCViewHTML.SetSessionData(const Value: ISessionData);
begin
  FSessionData := Value;
end;

procedure TMVCViewHTML.SetViewID(const Value: string);
begin
  FViewID := Value;
end;

procedure TMVCViewHTML.SetViewModel(const Model: IMVCModel);
begin
  FViewModel := Model;
end;

procedure TMVCViewHTML.UpdateView(const Section, ID: string; const Data: IMVCData);
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

function TMVCViewHTML.ViewID: string;
begin
  Result := FViewID;
end;

function TMVCViewHTML.ViewModel: IMVCModel;
begin
  Result := FViewModel;
end;

end.
