(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the Iztok Kacin nor the names of its contributors may be
 *   used to endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * =============================================================================
 * Mass mailer that can send personalized emails from a simple TAB format
 * =============================================================================
 * 11/06/2012 (1.1.0)
 *   - Important fixes that change how the mailer behaves on mail errors
 *     It will now ensure that all mails in the list will at least try
 *     to be sent.
 * 25/09/2012 (1.1.1)
 *   - IsCaseSensitive property
 * 01/02/2013 (1.1.2)
 *   - Added support for custom headers
 * =============================================================================
*)
unit Cromis.RoboMailer;

interface

uses
  Windows, SysUtils, Classes, StrUtils,

  // cromis units
  Cromis.StringUtils,

  // ICS units
  OverbyteIcsSmtpProt;

const
  cDefaultDelimiter = '%';

  // image replace tag
  cReplaceImage = '¤IMAGE¤';

const
  cDefaultCodePage = CP_UTF8;
  cDefaultCharset = 'UTF-8';

type
  TMailStatus = record
    MailAddress: string;
    CurrentIndex: Integer;
    AllMailsCount: Integer;
    LastErrorCode: Integer;
  end;

  TOnBeforeMailSend =  procedure(const Sender: TObject; const Status: TMailStatus; var SendMail: Boolean) of Object;
  TOnMailSuccess = procedure(const Sender: TObject; const Status: TMailStatus) of Object;
  TOnMailFailure = procedure(const Sender: TObject; const Status: TMailStatus) of Object;
  TOnMailsDone = procedure(const Sender: TObject) of Object;
  TParamsArray = array of string;

  TRoboMailer = class(TComponent)
  private
    FAdress: string;
    FHeaders: TStringList;
    FFailure: Boolean;
    FFromName: string;
    FBlocking: Boolean;
    FFinished: Boolean;
    FMailList: TStringList;
    FHTMLText: TStringList;
    FMailCount: Integer;
    FBulkMails: Boolean;
    FPlainText: TStringList;
    FMailSubject: string;
    FDataDelimiter: string;
    FCustomHeaders: TStringList;
    FHtmlSmtpClient: THtmlSmtpCli;
    FOnMailsDone: TOnMailsDone;
    FOnMailSuccess: TOnMailSuccess;
    FOnMailFailure: TOnMailFailure;
    FIsCaseSensitive: Boolean;
    FOnBeforeMailSend: TOnBeforeMailSend;
    function GetConvertToCharSet: Boolean;
    procedure SetConvertToCharSet(const Value: Boolean);
    function GetCodePage: Cardinal;
    procedure SetCodePage(const Value: Cardinal);
    function GetHTMLCharSet: string;
    procedure SetHTMLCharSet(const Value: string);
    // getters and setters
    function GetPort: string;
    function GetHost: string;
    function GetImages: TStrings;
    function GetCharSet: string;
    function GetReplyTo: string;
    function GetUserName: string;
    function GetUserPass: string;
    function GetReturnPath: string;
    function GetAttachments: TStrings;
    function GetAuthType: TSmtpAuthType;
    function GetContentType: TSmtpContentType;
    procedure SetPort(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetImages(const Value: TStrings);
    procedure SetCharSet(const Value: string);
    procedure SetReplyTo(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetUserPass(const Value: string);
    procedure SetHTMLText(const Value: TStringList);
    procedure SetPlainText(const Value: TStringList);
    procedure SetReturnPath(const Value: string);
    procedure SetAttachments(const Value: TStrings);
    procedure SetAuthType(const Value: TSmtpAuthType);
    procedure SetContentType(const Value: TSmtpContentType);
    procedure OnProcessHeader(Sender: TObject; HdrLines: TStrings);
    // internal functions and procedures
    procedure SmtpClientRequestDone(Sender: TObject; RqType: TSmtpRequest; ErrorCode: Word);
    procedure InternalSendSingleMail(const Address: string; ReplaceData: TStringList);
    procedure InternalSendMailList;
    procedure InternalDoOnFailure;
    // get current mail status from component
    function GetMailStatus(const Address: string; const ErrorCode: Integer): TMailStatus;
    procedure SetCaseSensitive(const Data: TStringList);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure SendMailList(const MailList: TStringList);
    procedure SendSingleMail(const Address: string; ReplaceData: TStringList = nil);
  published
    property OnBeforeMailSend: TOnBeforeMailSend read FOnBeforeMailSend write FOnBeforeMailSend;
    property OnMailSuccess: TOnMailSuccess read FOnMailSuccess write FOnMailSuccess;
    property OnMailFailure: TOnMailFailure read FOnMailFailure write FOnMailFailure;
    property ConvertToCharSet: Boolean read GetConvertToCharSet write SetConvertToCharSet;
    property IsCaseSensitive: Boolean read FIsCaseSensitive write FIsCaseSensitive;
    property ContentType: TSmtpContentType read GetContentType write SetContentType;
    property DataDelimiter: string read FDataDelimiter write FDataDelimiter;
    property OnMailsDone: TOnMailsDone read FOnMailsDone write FOnMailsDone;
    property Attachments: TStrings read GetAttachments write SetAttachments;
    property HTMLCharSet: string read GetHTMLCharSet write SetHTMLCharSet;
    property MailSubject: string read FMailSubject write FMailSubject;
    property ReturnPath: string read GetReturnPath write SetReturnPath;
    property PlainText: TStringList read FPlainText write SetPlainText;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property AuthType: TSmtpAuthType read GetAuthType write SetAuthType;
    property CodePage: Cardinal read GetCodePage write SetCodePage;
    property UserName: string read GetUserName write SetUserName;
    property UserPass: string read GetUserPass write SetUserPass;
    property CharSet: string read GetCharSet write SetCharSet;
    property Blocking: Boolean read FBlocking write FBlocking;
    property FromName: string read FFromName write FFromName;
    property ReplyTo: string read GetReplyTo write SetReplyTo;
    property Images: TStrings read GetImages write SetImages;
    property CustomHeaders: TStringList read FCustomHeaders;
    property Host: string read GetHost write SetHost;
    property Port: string read GetPort write SetPort;
  end;

  procedure Register;

implementation

{ TRoboMail }

constructor TRoboMailer.Create(AOwner: TComponent);
begin
  inherited;

  FHeaders := TStringList.Create;
  FMailList := TStringList.Create;
  FHTMLText := TStringList.Create;
  FPlainText := TStringList.Create;
  FCustomHeaders := TStringList.Create;

  // set default data delimiter
  FDataDelimiter := cDefaultDelimiter;
  // set headers delimiter properties
  FHeaders.StrictDelimiter := True;
  FHeaders.Delimiter := #9;

  FHtmlSmtpClient := THtmlSmtpCli.Create(Self);
  FHtmlSmtpClient.OnProcessHeader := OnProcessHeader;
  FHtmlSmtpClient.OnRequestDone := SmtpClientRequestDone;

  FHtmlSmtpClient.HtmlCharSet := cDefaultCharset;
  FHtmlSmtpClient.CodePage := cDefaultCodePage;
  FHtmlSmtpClient.CharSet := cDefaultCharset;

  FHtmlSmtpClient.DefaultEncoding := smtpEncBase64;
  FHtmlSmtpClient.Allow8bitChars := False;
end;

destructor TRoboMailer.Destroy;
begin
  FreeAndNil(FHtmlSmtpClient);
  FreeAndNil(FCustomHeaders);
  FreeAndNil(FPlainText);
  FreeAndNil(FHTMLText);
  FreeAndNil(FMailList);
  FreeAndNil(FHeaders);

  inherited;
end;

function TRoboMailer.GetAttachments: TStrings;
begin
  Result := FHtmlSmtpClient.EmailFiles;
end;

function TRoboMailer.GetAuthType: TSmtpAuthType;
begin
  Result := FHtmlSmtpClient.AuthType;
end;

function TRoboMailer.GetCharSet: string;
begin
  Result := FHtmlSmtpClient.CharSet;
end;

function TRoboMailer.GetCodePage: Cardinal;
begin
  Result := FHtmlSmtpClient.CodePage;
end;

function TRoboMailer.GetContentType: TSmtpContentType;
begin
  Result := FHtmlSmtpClient.ContentType;
end;

function TRoboMailer.GetConvertToCharSet: Boolean;
begin
  Result := FHtmlSmtpClient.ConvertToCharset;
end;

function TRoboMailer.GetHost: string;
begin
  Result := FHtmlSmtpClient.Host;
end;

function TRoboMailer.GetHTMLCharSet: string;
begin
  Result := FHtmlSmtpClient.HtmlCharSet;
end;

function TRoboMailer.GetImages: TStrings;
begin
  Result := FHtmlSmtpClient.EmailImages;
end;

function TRoboMailer.GetPort: string;
begin
  Result := FHtmlSmtpClient.Port;
end;

function TRoboMailer.GetReplyTo: string;
begin
  Result := FHtmlSmtpClient.HdrReplyTo;
end;

function TRoboMailer.GetReturnPath: string;
begin
  Result := FHtmlSmtpClient.HdrReturnPath;
end;

function TRoboMailer.GetMailStatus(const Address: string; const ErrorCode: Integer): TMailStatus;
begin
  Result.MailAddress := Address;
  Result.CurrentIndex := FMailCount;
  Result.LastErrorCode := ErrorCode;
  Result.AllMailsCount := FMailList.Count - 1;
end;

function TRoboMailer.GetUserName: string;
begin
  Result := FHtmlSmtpClient.Username;
end;

function TRoboMailer.GetUserPass: string;
begin
  Result := FHtmlSmtpClient.Password;
end;

procedure TRoboMailer.InternalDoOnFailure;
begin
  Inc(FMailCount);
  FFailure := True;
  FHtmlSmtpClient.Abort;
  InternalSendMailList;
end;

procedure TRoboMailer.InternalSendMailList;
var
  Index: Integer;
  LineArray: TStringList;
  ParamsList: TStringList;
begin
  if FMailCount < FMailList.Count then
  begin
    LineArray := TStringList.Create;
    try
      LineArray.CaseSensitive := FIsCaseSensitive;
      // get the mail list headers
      FHeaders.DelimitedText := FMailList[0];

      LineArray.StrictDelimiter := True;
      LineArray.Delimiter := #9;

      LineArray.DelimitedText := FMailList[FMailCount];
      ParamsList := TStringList.Create;
      try
        for Index := 1 to LineArray.Count - 1 do
          ParamsList.Values[FHeaders[Index]] := LineArray[Index];

        try
          InternalSendSingleMail(LineArray[0], ParamsList);
        except
          on E: Exception do
          begin
            if Assigned(FOnMailFailure) then
            begin
              case LineArray.Count > 0 of
                True: FOnMailFailure(Self, GetMailStatus(LineArray[0], GetLastError));
                False: FOnMailFailure(Self, GetMailStatus(EmptyStr, GetLastError));
              end;
            end;

            InternalDoOnFailure;
          end;
        end;
      finally
        ParamsList.Free;
      end;
    finally
      LineArray.Free;
    end;
  end
  else
  begin
    if Assigned(FOnMailsDone) then
      FOnMailsDone(Self);

    FBulkMails := False;
    FFinished := True;
  end;
end;

procedure TRoboMailer.InternalSendSingleMail(const Address: string; ReplaceData: TStringList);
var
  SendMail: Boolean;

  function ReplaceContentTags(const ContentText: string): string;
  var
    ReplaceTag: string;
    ContentTag: string;
  begin
    Result := ContentText;

    if ReplaceData <> nil then
    begin
      while StrBetween(FDataDelimiter, FDataDelimiter, Result) <> '' do
      begin
        ContentTag := StrBetween(FDataDelimiter, FDataDelimiter, Result);
        ReplaceTag := Format('%s%s%s', [FDataDelimiter, ContentTag, FDataDelimiter]);

        Result := StringReplace(Result, ReplaceTag, ReplaceData.Values[ContentTag], []);
      end;
    end;
  end;

begin
  if Assigned(FOnBeforeMailSend) then
  begin
    SendMail := True;
    FOnBeforeMailSend(Self, GetMailStatus(Address, 0), SendMail);

    // do we send the mail?
    if not SendMail then
    begin
      Inc(FMailCount);
      Exit;
    end;
  end;

  // clear failure
  FFailure := False;
  // clear recipients and set address
  FHtmlSmtpClient.RcptName.Clear;
  FAdress := Address;

  // Recipient list is computed from To, Cc and Bcc fields
  FHtmlSmtpClient.RcptNameAdd(Trim(Address), '', '');

  FHtmlSmtpClient.HdrSubject := ReplaceContentTags(FMailSubject);
  FHtmlSmtpClient.FromName := ReplaceContentTags(FFromName);
  FHtmlSmtpClient.HdrFrom  := ReplaceContentTags(FFromName);

  // set the processed plain text to the component
  FHtmlSmtpClient.PlainText.Text := ReplaceContentTags(FPlainText.Text);
  // set the processed html text to the component
  FHtmlSmtpClient.HtmlText.Text := ReplaceContentTags(FHTMLText.Text);

  // set the final properties and send the mail
  FHtmlSmtpClient.HdrTo := FAdress;
  FHtmlSmtpClient.Connect;
end;

procedure TRoboMailer.OnProcessHeader(Sender: TObject; HdrLines: TStrings);
begin
  HdrLines.AddStrings(FCustomHeaders)
end;

procedure TRoboMailer.SendMailList(const MailList: TStringList);
begin
  SetCaseSensitive(MailList);
  FBulkMails := True;
  FFinished := False;
  FMailCount := 1;

  // send all the mails out
  FMailList.Assign(MailList);
  InternalSendMailList;

  // if blocking wait for the finish
  while FBlocking and not FFinished do
  begin
    FHtmlSmtpClient.ProcessMessages;
    Sleep(1);
  end;
end;

procedure TRoboMailer.SendSingleMail(const Address: string; ReplaceData: TStringList);
begin
  FFinished := False;
  SetCaseSensitive(ReplaceData);
  InternalSendSingleMail(Address, ReplaceData);

  // if blocking wait for the finish
  while FBlocking and not FFinished do
  begin
    FHtmlSmtpClient.ProcessMessages;
    Sleep(1);
  end;
end;

procedure TRoboMailer.SetAttachments(const Value: TStrings);
begin
  FHtmlSmtpClient.EmailFiles.Assign(Value);
end;

procedure TRoboMailer.SetAuthType(const Value: TSmtpAuthType);
begin
  FHtmlSmtpClient.AuthType := Value;
end;

procedure TRoboMailer.SetCaseSensitive(const Data: TStringList);
begin
  if Data <> nil then
    Data.CaseSensitive := FIsCaseSensitive;
    
  FHeaders.CaseSensitive := FIsCaseSensitive;
  FMailList.CaseSensitive := FIsCaseSensitive;
  FHTMLText.CaseSensitive := FIsCaseSensitive;
  FPlainText.CaseSensitive := FIsCaseSensitive;
end;

procedure TRoboMailer.SetCharSet(const Value: string);
begin
  FHtmlSmtpClient.CharSet := Value;
end;

procedure TRoboMailer.SetCodePage(const Value: Cardinal);
begin
  FHtmlSmtpClient.CodePage := Value;
end;

procedure TRoboMailer.SetContentType(const Value: TSmtpContentType);
begin
  FHtmlSmtpClient.ContentType := Value;
end;

procedure TRoboMailer.SetConvertToCharSet(const Value: Boolean);
begin
  FHtmlSmtpClient.ConvertToCharset := Value;
end;

procedure TRoboMailer.SetHost(const Value: string);
begin
  FHtmlSmtpClient.Host := Value;
end;

procedure TRoboMailer.SetHTMLCharSet(const Value: string);
begin
  FHtmlSmtpClient.HtmlCharSet := Value;
end;

procedure TRoboMailer.SetHTMLText(const Value: TStringList);
begin
  FHTMLText.Assign(Value);
end;

procedure TRoboMailer.SetImages(const Value: TStrings);
begin
  FHtmlSmtpClient.EmailImages.Assign(Value);
end;

procedure TRoboMailer.SetPlainText(const Value: TStringList);
begin
  FPlainText.Assign(Value);
end;

procedure TRoboMailer.SetPort(const Value: string);
begin
  FHtmlSmtpClient.Port := Value;
end;

procedure TRoboMailer.SetReplyTo(const Value: string);
begin
  FHtmlSmtpClient.HdrReplyTo := Value;
end;

procedure TRoboMailer.SetReturnPath(const Value: string);
begin
  FHtmlSmtpClient.HdrReturnPath := Value
end;

procedure TRoboMailer.SetUserName(const Value: string);
begin
  FHtmlSmtpClient.Username := Value;
end;

procedure TRoboMailer.SetUserPass(const Value: string);
begin
  FHtmlSmtpClient.Password := Value;
end;

procedure TRoboMailer.SmtpClientRequestDone(Sender: TObject; RqType: TSmtpRequest; ErrorCode: Word);

  procedure FinalizeStatus;
  begin
    if FBulkMails then
    begin
      Inc(FMailCount);
      InternalSendMailList;
    end
    else
    begin
      if Assigned(FOnMailsDone) then
        FOnMailsDone(Self);
      FFinished := True;
    end;
  end;

begin
  if ErrorCode <> 0 then
  begin
    if Assigned(FOnMailFailure) then
      FOnMailFailure(Self, GetMailStatus(FAdress, ErrorCode));
    InternalDoOnFailure;
    Exit;
  end;

  case RqType of
    smtpConnect:
      begin
        case FHtmlSmtpClient.AuthType = smtpAuthNone of
          False: FHtmlSmtpClient.Ehlo;
          True: FHtmlSmtpClient.Helo;
        end;
      end;
    smtpEhlo:     FHtmlSmtpClient.Auth;
    smtpHelo:     FHtmlSmtpClient.MailFrom;
    smtpAuth:     FHtmlSmtpClient.MailFrom;
    smtpMailFrom: FHtmlSmtpClient.RcptTo;
    smtpRcptTo:   FHtmlSmtpClient.Data;
    smtpData:     FHtmlSmtpClient.Quit;
    smtpQuit:
      begin
        if Assigned(FOnMailSuccess) and not FFailure then
          FOnMailSuccess(Self, GetMailStatus(FAdress, ErrorCode));
        FinalizeStatus;
      end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Cromis', [TRoboMailer]);
end;

end.
