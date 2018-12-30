unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, ComCtrls, Menus,

  // cromis units
  Cromis.Hooks.Window;

type
  TfMain = class(TForm)
    lvWindowMessages: TListView;
    mmMainMenu: TMainMenu;
    miActions: TMenuItem;
    miStartListening: TMenuItem;
    miStopListening: TMenuItem;
    miDiv: TMenuItem;
    miExit: TMenuItem;
    miActiveHooks: TMenuItem;
    miHCBTDESTROYWND: TMenuItem;
    sbStatusBar: TStatusBar;
    pmActions: TPopupMenu;
    pmiStartListening: TMenuItem;
    pmiStopListening: TMenuItem;
    pmiClearView: TMenuItem;
    miHCBTCREATEWND: TMenuItem;
    miHCBTMOVESIZE: TMenuItem;
    miHCBTACTIVATE: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miStartListeningClick(Sender: TObject);
    procedure miStopListeningClick(Sender: TObject);
    procedure pmiClearViewClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miHCBTDESTROYWNDClick(Sender: TObject);
    procedure miHCBTCREATEWNDClick(Sender: TObject);
    procedure miHCBTMOVESIZEClick(Sender: TObject);
    procedure miHCBTACTIVATEClick(Sender: TObject);
  private
    FWindowHook: TWindowHook;
    function GetActiveHooks: TCBTActiveHooks;
    procedure OnWindowMessage(const Sender: TObject; var WindowMessage: TWindowMsg);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

procedure TfMain.FormCreate(Sender: TObject);
begin
  FWindowHook := TWindowHook.Create;
  FWindowHook.OnWindowMessage := OnWindowMessage;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FWindowHook.Finalize;
end;

function TfMain.GetActiveHooks: TCBTActiveHooks;
begin
  Result := [];

  if miHCBTDESTROYWND.Checked then
    Result := Result + [cbtDestroyWnd];
  if miHCBTCREATEWND.Checked then
    Result := Result + [cbtCreateWnd];
  if miHCBTMOVESIZE.Checked then
    Result := Result + [cbtMoveSize];
  if miHCBTACTIVATE.Checked then
    Result := Result + [cbtActivate];
end;

procedure TfMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.miHCBTACTIVATEClick(Sender: TObject);
begin
  miHCBTACTIVATE.Checked := not miHCBTACTIVATE.Checked;
  FWindowHook.ActiveHooks := GetActiveHooks;
end;

procedure TfMain.miHCBTCREATEWNDClick(Sender: TObject);
begin
  miHCBTCREATEWND.Checked := not miHCBTCREATEWND.Checked;
  FWindowHook.ActiveHooks := GetActiveHooks;
end;

procedure TfMain.miHCBTDESTROYWNDClick(Sender: TObject);
begin
  miHCBTDESTROYWND.Checked := not miHCBTDESTROYWND.Checked;
  FWindowHook.ActiveHooks := GetActiveHooks;
end;

procedure TfMain.miHCBTMOVESIZEClick(Sender: TObject);
begin
  miHCBTMOVESIZE.Checked := not miHCBTMOVESIZE.Checked;
  FWindowHook.ActiveHooks := GetActiveHooks;
end;

procedure TfMain.miStartListeningClick(Sender: TObject);
var
  AResult: Boolean;
begin
  FWindowHook.ActiveHooks := GetActiveHooks;

  // initialize the hooks
  AResult := FWindowHook.Initialize(cAllThreads);

  case AResult of
    True: sbStatusBar.SimpleText := 'Started Listening: SUCCESS';
    False: sbStatusBar.SimpleText := 'Started Listening: FAILURE';
  end;
end;

procedure TfMain.miStopListeningClick(Sender: TObject);
var
  AResult: Boolean;
begin
  AResult := FWindowHook.Finalize;

  case AResult of
    True: sbStatusBar.SimpleText := 'Stoped Listening: SUCCESS';
    False: sbStatusBar.SimpleText := 'Stoped Listening: FAILURE';
  end;
end;

procedure TfMain.OnWindowMessage(const Sender: TObject; var WindowMessage: TWindowMsg);
var
  MessageItem: TListItem;
begin
  lvWindowMessages.Items.BeginUpdate;
  try
    MessageItem := lvWindowMessages.Items.Add;

    case WindowMessage.MsgCode of
      HCBT_CREATEWND: MessageItem.Caption := 'Create';
      HCBT_DESTROYWND: MessageItem.Caption := 'Destroy';
      HCBT_MOVESIZE: MessageItem.Caption := 'MoveSize';
      HCBT_ACTIVATE: MessageItem.Caption := 'Activate';
    end;

    MessageItem.SubItems.BeginUpdate;
    try
      MessageItem.SubItems.Add(WindowMessage.WindowTitle);
      MessageItem.SubItems.Add(WindowMessage.WindowClass);
      MessageItem.SubItems.Add(IntToStr(WindowMessage.WindowHandle));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.ParentHandle));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.ThreadHandle));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.ProcessID));
      MessageItem.SubItems.Add(WindowMessage.ProcessName);
      MessageItem.SubItems.Add(IntToStr(WindowMessage.MenuHandle));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.PositionX));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.PositionY));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.WindowWidth));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.WindowHeight));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.WindowStyle));
      MessageItem.SubItems.Add(IntToStr(WindowMessage.WindowStyleEx));
    finally
      MessageItem.SubItems.EndUpdate;
    end;
  finally
    lvWindowMessages.Items.EndUpdate;
  end;
end;

procedure TfMain.pmiClearViewClick(Sender: TObject);
begin
  lvWindowMessages.Clear;
end;

{$R *.dfm}

end.
