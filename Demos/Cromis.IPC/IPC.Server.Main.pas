unit IPC.Server.Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,

  // cromis units
  Cromis.Comm.Custom, Cromis.Comm.IPC, Cromis.Streams.Filters, Cromis.Threading.TS, Cromis.AnyValue;

const
  WM_OnListBoxMessage = WM_USER + 1;
  WM_OnRequestFinished = WM_USER + 2;

type
  TfMain = class(TForm)
    eServerName: TEdit;
    ListBox1: TListBox;
    btnStart: TButton;
    lbServerName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    FIPCServer: TIPCServer;
    FRequestCount: Integer;
    FMessageQueue: TThreadSafeQueue;
    procedure WriteToListBox(const AMessage: string);
    procedure OnClientConnect(const Context: ICommContext);
    procedure OnClientDisconnect(const Context: ICommContext);
    procedure OnServerError(const Context: ICommContext; const Error: TServerError);
    procedure OnExecuteRequest(const Context: ICommContext; const Request, Response: IMessageData);
    procedure OnRequestFinished(var Msg: TMessage); message WM_OnRequestFinished;
    procedure OnListBoxMessage(var Msg: TMessage); message WM_OnListBoxMessage;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses
  Cromis.Streams.Filters.XTEA,
  Cromis.Streams.Filters.ZLIB;

{$R *.dfm}

procedure TfMain.btnStartClick(Sender: TObject);
begin
  FIPCServer.ServerName := eServerName.Text;

  if FIPCServer.Listening then
  begin
    btnStart.Caption := 'Start';
    FIPCServer.Stop;
  end
  else
  begin
    btnStart.Caption := 'Stop';
    FIPCServer.Start;
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FMessageQueue := TThreadSafeQueue.Create;

  FIPCServer := TIPCServer.Create;
  FIPCServer.OnServerError := OnServerError;
  FIPCServer.OnClientConnect := OnClientConnect;
  FIPCServer.OnExecuteRequest := OnExecuteRequest;
  FIPCServer.OnClientDisconnect := OnClientDisconnect;

  FIPCServer.Filters.AddFilter(ZLIBFilter);
  FIPCServer.Filters.AddFilter(XTEAFilter('EncryptionKey'));
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIPCServer);
  FreeAndNil(FMessageQueue);
end;

procedure TfMain.OnClientConnect(const Context: ICommContext);
begin
  WriteToListBox(Format('Client %s connected', [Context.Client.ID]));
end;

procedure TfMain.OnClientDisconnect(const Context: ICommContext);
begin
  WriteToListBox(Format('Client %s disconnected', [Context.Client.ID]));
end;

procedure TfMain.OnExecuteRequest(const Context: ICommContext; const Request, Response: IMessageData);
var
  Command: AnsiString;
  LocalCount: Integer;
begin
  Command := Request.Data.ReadUTF8String('Command');
  WriteToListBox(Format('%s request recieved from client %s (Sent at: %s)', [Command,
                                                                             Context.Client.ID,
                                                                             Request.ID]));
  // increase the request count thread safe way
  LocalCount := InterlockedIncrement(FRequestCount);

  Response.ID := Format('Response nr. %d', [LocalCount]);
  Response.Data.WriteDateTime('TDateTime', Now);
  Response.Data.WriteInteger('Integer', 5);
  Response.Data.WriteReal('Real', 5.33);
  Response.Data.WriteUTF8String('String', 'to je testni string');

  PostMessage(Handle, WM_OnRequestFinished, 0, 0);
end;

procedure TfMain.OnListBoxMessage(var Msg: TMessage);
var
  MessageValue: TAnyValue;
begin
  while FMessageQueue.Dequeue(MessageValue) do
    ListBox1.Items.Add(MessageValue.AsString);
end;

procedure TfMain.OnRequestFinished(var Msg: TMessage);
var
  LocalCount: Integer;
begin
  InterlockedExchange(LocalCount, FRequestCount);
  Caption := Format('%d requests processed', [LocalCount]);
end;

procedure TfMain.OnServerError(const Context: ICommContext; const Error: TServerError);
begin
  WriteToListBox(Format('Server error: %d - %s', [Error.Code, Error.Desc]));
end;

procedure TfMain.WriteToListBox(const AMessage: string);
begin
  FMessageQueue.Enqueue(AMessage);
  PostMessage(Handle, WM_OnListBoxMessage, 0, 0);
end;

end.
