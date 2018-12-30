unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, StdCtrls, Controls, Forms,

  // scheduler code
  Cromis.Scheduler;

type
  TfMain = class(TForm)
    lbEvents: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    EventList: TSchEventList;
    FDynamicEvent: TScheduledEvent;
    procedure OnScheduleOneTrigger(Sender: TScheduledEvent);
    procedure OnScheduleTwoTrigger(Sender: TScheduledEvent);
    procedure OnScheduleThreeTrigger(Sender: TScheduledEvent);
    procedure OnScheduleFourTrigger(Sender: TScheduledEvent);
    procedure OnDynamicScheduleTrigger(Sender: TScheduledEvent);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
var
  NewSchedule: TScheduledEvent;
begin
  // clear memo
  lbEvents.Clear;
  // crete new event list that will hold events
  EventList := TSchEventList.Create;
  // first event
  NewSchedule := EventList.Add('ScheduleOne');
  NewSchedule.Schedule.EventPlan := '0 */2 * * * * *';
  NewSchedule.OnScheduleEvent := OnScheduleOneTrigger;
  NewSchedule.Run;
  // second event
  NewSchedule := EventList.Add('ScheduleTwo');
  NewSchedule.Schedule.EventPlan := '*/30 * * * * * *';
  NewSchedule.OnScheduleEvent := OnScheduleTwoTrigger;
  NewSchedule.Run;
  // third event
  NewSchedule := EventList.Add('ScheduleThree');
  NewSchedule.Schedule.EventPlan := '*/5 * * * * * *';
  NewSchedule.OnScheduleEvent := OnScheduleThreeTrigger;
  NewSchedule.Run;
  // fourth event
  NewSchedule := EventList.Add('ScheduleFour');
  NewSchedule.Schedule.EventPlan := '15 * * * * * *';
  NewSchedule.OnScheduleEvent := OnScheduleFourTrigger;
  NewSchedule.Run;
  // start stop dynamic event
  FDynamicEvent := EventList.Add('DynamicSchedule');
  FDynamicEvent.Schedule.EventPlan := '*/5 * * * * * *';
  FDynamicEvent.OnScheduleEvent := OnDynamicScheduleTrigger;

  Caption := Format('Started at : %s ( Press ''d'' for dynamic event )', [DateTimeToStr(Now)]);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EventList);
end;

procedure TfMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = 'd' then
  begin
    if FDynamicEvent.Running then
    begin
      FDynamicEvent.Stop;
      lbEvents.Items.Add(Format('Dynamic event was stopped at : %s', [DateTimeToStr(now)]));
    end
    else
    begin
      FDynamicEvent.Run;
      lbEvents.Items.Add(Format('Dynamic event was started at : %s', [DateTimeToStr(now)]));
    end;
  end;
end;

procedure TfMain.OnDynamicScheduleTrigger(Sender: TScheduledEvent);
begin
  lbEvents.Items.Add(Format('Dynamic event was trigered at : %s', [DateTimeToStr(now)]));
end;

procedure TfMain.OnScheduleFourTrigger(Sender: TScheduledEvent);
begin
  lbEvents.Items.Add(Format('Event four was trigered at : %s', [DateTimeToStr(now)]));
end;

procedure TfMain.OnScheduleOneTrigger(Sender: TScheduledEvent);
begin
  lbEvents.Items.Add(Format('Event one was trigered at : %s', [DateTimeToStr(now)]));
end;

procedure TfMain.OnScheduleThreeTrigger(Sender: TScheduledEvent);
begin
  lbEvents.Items.Add(Format('Event three was trigered at : %s', [DateTimeToStr(now)]));
end;

procedure TfMain.OnScheduleTwoTrigger(Sender: TScheduledEvent);
begin
  lbEvents.Items.Add(Format('Event two was trigered at : %s', [DateTimeToStr(now)]));
end;

end.
