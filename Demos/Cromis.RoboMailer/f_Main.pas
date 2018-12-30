unit f_Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Buttons, XPMan,

  // TMS components and ICS units
  AdvEdit, AdvEdBtn, AdvFileNameEdit, OverbyteIcsSmtpProt,

  // cromis library units
  Cromis.SimpleStorage, Cromis.StringUtils, Cromis.RoboMailer;

const
  cDataDelimiter = '¤';

type
  TfMain = class(TForm)
    ProgressBar: TProgressBar;
    gbMailSettings: TGroupBox;
    XPManifest: TXPManifest;
    mmSentList: TListBox;
    gbMailStatus: TGroupBox;
    lbCurrentSendingMail: TLabel;
    lbSendingMail: TStaticText;
    lbSentMailCount: TLabel;
    eHMTLContent: TAdvFileNameEdit;
    ePlainTextContent: TAdvFileNameEdit;
    lbHMTLContent: TLabel;
    lbPlainTextContent: TLabel;
    eMailSubject: TEdit;
    eFromAdrress: TEdit;
    lbMailSubject: TLabel;
    lbFromAdrress: TLabel;
    eTargetMailList: TAdvFileNameEdit;
    lbTargetMailList: TLabel;
    gbServerSettings: TGroupBox;
    eUserName: TEdit;
    eUserPassword: TEdit;
    eServerName: TEdit;
    lbUserPassword: TLabel;
    lbUserName: TLabel;
    lbServerName: TLabel;
    RoboMailer: TRoboMailer;
    sbSendMail: TSpeedButton;
    sbStopSending: TSpeedButton;
    gbOtherSettings: TGroupBox;
    lbMailImages: TLabel;
    lbMailAttachments: TLabel;
    lbContentEncoding: TLabel;
    eContentEncoding: TEdit;
    eMailAttachments: TAdvFileNameEdit;
    eMailImages: TAdvFileNameEdit;
    lbSendingStatus: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure sbStopSendingClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbSendMailClick(Sender: TObject);
    procedure RoboMailerBeforeMailSend(const Sender: TObject;
                                       const Status: TMailStatus;
                                       var SendMail: Boolean);
    procedure RoboMailerMailSuccess(const Sender: TObject; const Status: TMailStatus);
    procedure RoboMailerMailFailure(const Sender: TObject; const Status: TMailStatus);
    procedure RoboMailerMailsDone(const Sender: TObject);
  private
    { Private declarations }
    FRoot: string;
    FMailsList: TstringList;
    FAbortSending: Boolean;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.sbSendMailClick(Sender: TObject);
begin
  RoboMailer.PlainText.LoadFromFile(ePlainTextContent.FileName);
  RoboMailer.HTMLText.LoadFromFile(eHMTLContent.FileName);
  RoboMailer.MailSubject := eMailSubject.Text;
  RoboMailer.FromName := eFromAdrress.Text;
  RoboMailer.UserPass := eUserPassword.Text;
  RoboMailer.UserName := eUserName.Text;

  if Pos(':', eServerName.Text) > 0 then
  begin
    RoboMailer.Host := StrBefore(':', eServerName.Text);
    RoboMailer.Port := StrAfter(':', eServerName.Text);
  end
  else
  begin
    RoboMailer.Host := eServerName.Text;
    RoboMailer.Port := '25';
  end;

  // set the charset for the mail content
  RoboMailer.Charset := eContentEncoding.Text;

  // check to authenticate
  if eUserName.Text <> '' then
    RoboMailer.AuthType := smtpAuthLogin
  else
    RoboMailer.AuthType := smtpAuthNone;

  // set the list of mail attachemnts
  if FileExists(eMailAttachments.FileName) then
    RoboMailer.Attachments.LoadFromFile(eMailAttachments.FileName);

  // set the list of mail images
  if FileExists(eMailImages.FileName) then
    RoboMailer.Images.LoadFromFile(eMailImages.FileName);

  // set mails the status to sending
  lbSendingStatus.Caption := 'STATUS: SENDING';

  FMailsList := TstringList.Create;
  try
    FMailsList.LoadFromFile(eTargetMailList.FileName);
    RoboMailer.SendMailList(FMailsList);
  finally
    FMailsList.Free;
  end;
end;

procedure TfMain.sbStopSendingClick(Sender: TObject);
begin
  FAbortSending := True;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  Settings: ISimpleStorage;
begin
  FRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  RoboMailer.DataDelimiter := cDataDelimiter;


  Settings := StorageFromFile(FRoot + 'Settings.conf');
  eHMTLContent.FileName := Settings.Get('MailSettings/HTMLFile').AsStringDef;
  ePlainTextContent.FileName := Settings.Get('MailSettings/PaintTextFile').AsStringDef;
  eMailSubject.Text := Settings.Get('MailSettings/Subject').AsStringDef;
  eFromAdrress.Text := Settings.Get('MailSettings/From').AsStringDef;
  eTargetMailList.FileName := Settings.Get('MailSettings/TargetListFile').AsStringDef;

  eUserName.Text := Settings.Get('ServerSettings/UserName').AsStringDef;
  eServerName.Text := Settings.Get('ServerSettings/HostName').AsStringDef;
  eUserPassword.Text := Settings.Get('ServerSettings/UserPass').AsStringDef;

  eContentEncoding.Text := Settings.Get('OtherSettings/ContentEncoding').AsStringDef;
  eMailAttachments.Text := Settings.Get('OtherSettings/MailAttachments').AsStringDef;
  eMailImages.Text := Settings.Get('OtherSettings/MailImages').AsStringDef;
end;

procedure TfMain.FormDestroy(Sender: TObject);
var
  Settings: ISimpleStorage;
begin
  Settings := CreateStorage('Settings');

  Settings.Ensure('MailSettings/HTMLFile').AsString := eHMTLContent.FileName;
  Settings.Ensure('MailSettings/PaintTextFile').AsString := ePlainTextContent.FileName;
  Settings.Ensure('MailSettings/Subject').AsString := eMailSubject.Text;
  Settings.Ensure('MailSettings/From').AsString := eFromAdrress.Text;
  Settings.Ensure('MailSettings/TargetListFile').AsString := eTargetMailList.FileName;

  Settings.Ensure('ServerSettings/UserName').AsString := eUserName.Text;
  Settings.Ensure('ServerSettings/HostName').AsString := eServerName.Text;
  Settings.Ensure('ServerSettings/UserPass').AsString := eUserPassword.Text;

  Settings.Ensure('OtherSettings/ContentEncoding').AsString := eContentEncoding.Text;
  Settings.Ensure('OtherSettings/MailAttachments').AsString := eMailAttachments.Text;
  Settings.Ensure('OtherSettings/MailImages').AsString := eMailImages.Text;

  Settings.SaveToFile(FRoot + 'Settings.conf');
end;

procedure TfMain.RoboMailerBeforeMailSend(const Sender: TObject;
                                          const Status: TMailStatus;
                                          var SendMail: Boolean);
begin
  lbSendingMail.Caption := Status.MailAddress;
end;

procedure TfMain.RoboMailerMailFailure(const Sender: TObject; const Status: TMailStatus);
begin
  MessageBox(Self.Handle, PChar(Format('Error Sending Mail "%s"', [Status.MailAddress])),
    'Error', MB_OK or MB_TOPMOST);
end;

procedure TfMain.RoboMailerMailsDone(const Sender: TObject);
begin
  // set mails the status to finished
  lbSendingStatus.Caption := 'STATUS: FINISHED';
end;

procedure TfMain.RoboMailerMailSuccess(const Sender: TObject; const Status: TMailStatus);
begin
  lbSentMailCount.Caption := Format('Sent "%d" of "%d" mails', [Status.CurrentIndex,
                                                                Status.AllMailsCount]);
  mmSentList.Items.Add(Status.MailAddress);
  lbSendingMail.Caption := '';
end;

end.
