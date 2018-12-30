unit f_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls, FileCtrl, TypInfo,

  // cromis units
  Cromis.DirectoryWatch, Cromis.Unicode;

type
  TfMain = class(TForm)
    lvWatchActions: TListView;
    pnBottom: TPanel;
    eDirectoryName: TEdit;
    sbSelectDirectory: TSpeedButton;
    cbWatchSubdirectories: TCheckBox;
    btnStop: TButton;
    btnStart: TButton;
    procedure sbSelectDirectoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure eDirectoryNameDblClick(Sender: TObject);
  private
    FDirectoryWatch: TDirectoryWatch;
    procedure SelectWatchDirectory;
    procedure OnError(const Sender: TObject;
                      const ErrorCode: Integer;
                      const ErrorMessage: ustring);
    procedure OnNotify(const Sender: TObject;
                       const Action: TWatchAction;
                       const FileName: ustring);
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btnStartClick(Sender: TObject);
begin
  FDirectoryWatch.WatchSubTree := cbWatchSubdirectories.Checked;
  FDirectoryWatch.Directory := eDirectoryName.Text;
  FDirectoryWatch.Start;

  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TfMain.btnStopClick(Sender: TObject);
begin
  FDirectoryWatch.Stop;

  btnStart.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TfMain.eDirectoryNameDblClick(Sender: TObject);
begin
  SelectWatchDirectory;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FDirectoryWatch := TDirectoryWatch.Create;
  FDirectoryWatch.OnNotify := OnNotify;
  FDirectoryWatch.OnError := OnError;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDirectoryWatch);
end;

procedure TfMain.OnError(const Sender: TObject; const ErrorCode: Integer; const ErrorMessage: ustring);
begin
  ShowMessage(Format('Error with code %d and description: %s', [ErrorCode, ErrorMessage]));
end;

procedure TfMain.OnNotify(const Sender: TObject;
                          const Action: TWatchAction;
                          const FileName: ustring);
var
  ListItem: TListItem;
begin
  lvWatchActions.Items.BeginUpdate;
  try
    ListItem := lvWatchActions.Items.Add;
    ListItem.Caption := GetEnumName(TypeInfo(TWatchAction), Integer(Action)) ;
    ListItem.SubItems.Add(FileName);
  finally
    lvWatchActions.Items.EndUpdate;
  end;
end;

procedure TfMain.sbSelectDirectoryClick(Sender: TObject);
begin
  SelectWatchDirectory;
end;

procedure TfMain.SelectWatchDirectory;
var
  Dir: String;
begin
  SelectDirectory('Select a directory to monitor', '', Dir);
  eDirectoryName.Text := Dir;
end;

end.
