(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009-2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * ISAPI server Indy proxy. Read and writes data from / to ECB block
 * =============================================================================
 * 24/08/2010 (1.0.0)
 *  - Initial implementation"
 * 02/06/2014 (1.1.0)
 *  - Correctly set the response headers
 * ============================================================================
*)
unit Cromis.ISAPI.Indy;

interface

uses
  Windows, SysUtils, Classes,

  // indy library units
  IdTCPServer, IdCustomHTTPServer, IdHTTPServer, idCookie, IdHTTPHeaderInfo,

  // cromis library units
  Cromis.ISAPI.Server, Cromis.Streams, Cromis.StringUtils;

procedure FillECBFromRequest(var ControlBlock: TECBData;
                             const KeepAlive: Boolean;
                             const ARequestInfo: TIdHTTPRequestInfo;
                             const DocumentRoot: string;
                             const ScriptName: string;
                             const HostPort: string;
                             const Action: string);


procedure FillResponseFromECB(var ControlBlock: TECBData; const AResponseInfo: TIdHTTPResponseInfo);

implementation

type
  THeaderHelper = class helper for TIdEntityHeaderInfo
  public
    procedure DoProcessHeaders;
  end;


procedure FillECBFromRequest(var ControlBlock: TECBData;
                             const KeepAlive: Boolean;
                             const ARequestInfo: TIdHTTPRequestInfo;
                             const DocumentRoot: string;
                             const ScriptName: string;
                             const HostPort: string;
                             const Action: string);
var
  BufSize: Cardinal;
  FullPath: string;
  TempString: AnsiString;
begin
  ControlBlock.HTTPHeaders.Assign(ARequestInfo.RawHeaders);

  ControlBlock.InputParams.SetParam('AuthUsername', ARequestInfo.AuthUsername);
  ControlBlock.InputParams.SetParam('AuthPassword', ARequestInfo.AuthPassword);
  ControlBlock.InputParams.SetParam('QueryParams', ARequestInfo.QueryParams);
  ControlBlock.InputParams.SetParam('RemoteHost', ARequestInfo.RemoteIP);
  ControlBlock.InputParams.SetParam('ScriptName', ScriptName);
  ControlBlock.InputParams.SetParam('Document', ARequestInfo.Document);
  ControlBlock.InputParams.SetParam('Host', ARequestInfo.Host);
  ControlBlock.InputParams.SetParam('Port', HostPort);
  ControlBlock.InputParams.SetParam('Software', 'Cromis ISAPI Server');
  ControlBlock.InputParams.SetParam('AppPool', 'Cromis ISAPI Pool');
  ControlBlock.InputParams.SetParam('RemoteIP', ARequestInfo.RemoteIP);
  ControlBlock.InputParams.SetParam('Protocol', ARequestInfo.Version);
  ControlBlock.InputParams.SetParam('Command', ARequestInfo.Command);
  ControlBlock.InputParams.SetParam('DocumentRoot', DocumentRoot);
  ControlBlock.InputParams.SetParam('ServerAdmin', 'Cromis');
  ControlBlock.KeepAlive := KeepAlive;

  FullPath := ExpandFilename(IncludeTrailingPathDelimiter(DocumentRoot) + ARequestInfo.Document);
  AssignStringToPAnsiChar(ControlBlock.ECB.lpszContentType, ARequestInfo.ContentType);
  AssignStringToPAnsiChar(ControlBlock.ECB.lpszQueryString, ARequestInfo.QueryParams);
  AssignStringToPAnsiChar(ControlBlock.ECB.lpszMethod, ARequestInfo.Command);
  AssignStringToPAnsiChar(ControlBlock.ECB.lpszPathTranslated, FullPath);
  AssignStringToPAnsiChar(ControlBlock.ECB.lpszPathInfo, Action);
  ControlBlock.ECB.dwHttpStatusCode := 0;

  // get session data if present
  if ARequestInfo.Session <> nil then
    ControlBlock.InputParams.SetParam('SessionID', ARequestInfo.Session.Content.Text);

  if SameText(ARequestInfo.Command, 'POST') then
  begin
    if ARequestInfo.ContentLength > 0 then
     begin
       BufSize := ARequestInfo.ContentLength;
       ControlBlock.ECB.cbAvailable := BufSize;
       ControlBlock.ECB.cbTotalBytes := BufSize;
       ControlBlock.ECB.lpbData := AllocMem(BufSize + cNullCharSize);
       TempString := ReadFromStreamAsUTF8(ARequestInfo.PostStream);
       StrPCopy(ControlBlock.ECB.lpbData, TempString);
     end;
  end
  else if SameText(ARequestInfo.Command, 'PUT') then
  begin
    if ARequestInfo.ContentLength > 0 then
    begin
       BufSize := ARequestInfo.ContentLength;
       ControlBlock.ECB.cbAvailable := BufSize;
       ControlBlock.ECB.cbTotalBytes := BufSize;
       ControlBlock.ECB.lpbData := AllocMem(BufSize);
       ARequestInfo.PostStream.Seek(0, soFromBeginning);
       ARequestInfo.PostStream.Read(ControlBlock.ECB.lpbData^, BufSize);
    end;
  end;
end;

procedure FillResponseFromECB(var ControlBlock: TECBData; const AResponseInfo: TIdHTTPResponseInfo);
var
  I: Integer;
  Cookie: TIdCookieRFC2109;
begin
  ControlBlock.OutputStream.Position := 0;

  AResponseInfo.ContentStream := TMemoryStream.Create;
  AResponseInfo.ContentStream.CopyFrom(ControlBlock.OutputStream, 0);
  AResponseInfo.ResponseNo := ControlBlock.ECB.dwHttpStatusCode;
  AResponseInfo.RawHeaders.Assign(ControlBlock.OutputParams);
  AResponseInfo.DoProcessHeaders;

  // set all cookies specified
  for I := 0 to ControlBlock.CookieList.Count - 1 do
  begin
    Cookie := AResponseInfo.Cookies.Add;
    Cookie.CookieText := ControlBlock.CookieList[I];
  end;

  // handle possible redirection
  if ControlBlock.RedirectURL <> '' then
    AResponseInfo.Redirect(ControlBlock.RedirectURL);
end;

{ THeaderHelper }

procedure THeaderHelper.DoProcessHeaders;
begin
  ProcessHeaders;
end;

end.
