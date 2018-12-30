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
 * ISAPI server Implementation. Allows to build standalone ISAPI enabled servers
 * =============================================================================
 * NOTICE OF CODE ORIGIN
 *
 * This code was derived from the original code of author "Serhiy Perevoznyk"
 * The original code "idISAPIRunner" can still be found at VCLComponents.com site.
 * The URL is: http://www.vclcomponents.com/Delphi/Winsock___Internet/idRunner-info.html
 *
 * The code was taken as a starting point and then mainly written from scratch
 * keeping some of the healthy code parts. So I am not in any way an author of
 * the original idea. But I am the author of all the changes and new code parts.
 *
 * ============================================================================
 * 24/08/2010 (1.0.0)
 *  - Initial implementation
 * 24/08/2010 (1.0.1)
 *  - ReturnCode added
 *  - Do not silently eat exceptions
 * 12/06/2012 (1.1.0)
 *  - Use the new hash unit
 * ============================================================================
*)
unit Cromis.ISAPI.Server;

interface

uses
  Windows, SysUtils, Classes, Isapi2, WinSock, Math, {$IFDEF UNICODE} AnsiStrings, {$ENDIF}

  IniFiles,

  // cromis units
  Cromis.Streams, Cromis.Hashing, Cromis.Threading.Sync, Cromis.Unicode;

const
  cNullCharSize = 2;

const
  cErrorExtVersionNotAssigned = 'Internal server error. GetExtensionVersion procedure not assigned';
  cErrorExtProcNotAssigned =  'Internal server error. HttpExtensionProc procedure not assigned';
  cErrorProcessingRequest = 'Internal server error. Error Processing Request';
  cErrorIncorectVersion = 'Internal server error. Incorrect version';
  cErrorModuleNotFound = 'The module was not found';

const
  { Session support for ISAPI modules }
  HSE_REQ_IS_KEEP_CONN            = (HSE_REQ_END_RESERVED + 8);
  HSE_REQ_SEND_RESPONSE_HEADER_EX = (HSE_REQ_END_RESERVED + 16);
  HSE_REQ_MAP_URL_TO_PATH_EX      = (HSE_REQ_END_RESERVED + 12);
  HSE_REQ_ABORTIVE_CLOSE          = (HSE_REQ_END_RESERVED + 14);

  ERROR_INSUFFICIENT_BUFFER = 122; { Some call require the
                                     application to pass in a buffer
                                     filled with data.  This error is
                                     returned if the data buffer is too
                                     small.  For example: DosSetFileInfo
                                     requires 4 bytes of data.  If a
                                     two byte buffer is passed in then
                                     this error is returned.
                                     error_buffer_overflow is used when
                                     the output buffer in not big enough. }

type
  HSE_URL_MAPEX_INFO =
    record
      lpszPath: array[0..MAX_PATH - 1] of CHAR; // Physical path root mapped to
      dwFlags: DWORD; // Flags associated with this URL path
      cchMatchingPath: DWORD; // Number of matching characters in physical path
      cchMatchingURL: DWORD; // Number of matching characters in URL
      dwReserved1: DWORD;
      dwReserved2: DWORD;
    end;

  LPHSE_URL_MAPEX_INFO = ^HSE_URL_MAPEX_INFO;
  THSE_URL_MAPEX_INFO = HSE_URL_MAPEX_INFO;

type
  {
  	HSE_SEND_HEADER_EX_INFO allows an ISAPI application to send headers
  	and specify keep-alive behavior in the same call.
  }
  HSE_SEND_HEADER_EX_INFO =
    record
      pszStatus: LPCSTR; // HTTP status code  eg: "200 OK"
      pszHeader: LPCSTR; // HTTP header
      cchStatus: DWORD; // number of characters in status code
      cchHeader: DWORD; // number of characters in header
      fKeepConn: BOOL; // keep client connection alive?
    end;

  // header ex info
  LPHSE_SEND_HEADER_EX_INFO = ^HSE_SEND_HEADER_EX_INFO;
  THSE_SEND_HEADER_EX_INFO = HSE_SEND_HEADER_EX_INFO;

type
  TOnLogParameter = procedure(const LogMsg: string; const ConnID: HCONN) of Object;

  TParamsList = class(TStringList)
  public
    procedure InitializeList;
    function GetParam(const Name: string): string;
    procedure SetParam(const Name, Value: string);
  end;

  TECBData = class
    // the ECB block as defined in ISAPI
    ECB: TEXTENSION_CONTROL_BLOCK;
    // other ECB related fields
    RedirectURL: string;
    WaitEvent: Cardinal;
    KeepAlive: Boolean;
    DataPos: Cardinal;

    // response related data
    OutputStream: TMemoryStream;
    OutputParams: TParamsList;
    CookieList: TParamsList;

    // request related data
    HTTPHeaders: TParamsList;
    InputParams: TParamsList;

    // ECBData notification routines
    LogParameterNotify: TOnLogParameter;

    // save and load to stream methods
    procedure LoadFromStream(const Stream: TStreamStorage);
    procedure SaveToStream(const Stream: TStreamStorage);

    // constructor and destructor
    constructor Create(const ConnID: Cardinal);
    destructor Destroy; override;
  end;

  TECBDataList = class
  private
    FLock: TSRWLock;
    FEBCList: TCardinalHashTable;
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireNewECB: TECBData;
    function GetECB(const ConnID: HCONN): TECBData;
    procedure DeleteECB(const ConnID: HCONN);
  end;

  TISAPIServer = class
  private
    FReturnCode: Cardinal;
    FLibraryList: TStringList;
    FOnLogParameter: TOnLogParameter;
    FCriticalSection: TRTLCriticalSection;
    function InternalUnloadDLL(const DLLIndex: Integer; const Ask: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(const DllName: string; const ControlBlock: TECBData; const Unload: Boolean = False): Boolean;
    property OnLogParameter: TOnLogParameter read FOnLogParameter write FOnLogParameter;
    property ReturnCode: Cardinal read FReturnCode;
  end;

   VersionFunc = function(var Ver: THSE_VERSION_INFO): Boolean; stdcall;
   ProcFunc = function(var ECB: TEXTENSION_CONTROL_BLOCK): LongInt; stdcall;

  {
    The CBGetServerVariable function retrieves information about an HTTP
    connection or about IIS itself.

   Parameters:
    ConnID - Specifies the connection handle.
    VariableName - A null-terminated string that indicates which variable
    is requested. The following table lists the possible variables.
    Buffer - Points to the buffer to receive the requested information.
    Size - Points to a DWORD that indicates the size of the buffer
    pointed to by Buffer. On successful completion, the DWORD contains
    the size of bytes transferred into the buffer, including the
    null-terminating byte.
   }
  function CBGetServerVariable(ConnID: HCONN; VariableName: PAnsiChar; Buffer: Pointer;
    var Size: DWORD): Boolean; stdcall;

  {
    The ServerSupportFunction is a callback function that is supplied in the
    EXTENSION_CONTROL_BLOCK that is associated with the current HTTP request.
    ServerSupportFunction can be used to perform a variety of tasks.
  }
  function CBServerSupport(ConnID: HCONN; HSERRequest: DWORD; Buffer: Pointer;
    Size: LPDWORD; DataType: LPDWORD): Boolean; stdcall;

  {
    The WriteClient function is a callback function that is supplied in the
    EXTENSION_CONTROL_BLOCK for a request sent to the ISAPI extension.
    It sends the data present in the given buffer to the client that made the request.
  }
  function CBWriteClient(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD;
    dwReserved: DWORD): Boolean; stdcall;

  //The CBReadClient function reads data from the body of the client's HTTP request
  function CBReadClient(ConnID: HCONN; Buffer: Pointer; var Size: DWORD): Boolean; stdcall;

  // utility function that assigns an unicode string to a new PAnsiChar buffer
  procedure AssignStringToPAnsiChar(var Destination: PAnsiChar; const Source: ustring);

var
  ECBDataList: TECBDataList;
  VariableHash: TStringHash;

implementation

procedure FillVariableHashTable;
begin
  // fill the hash table
  VariableHash.Add('AUTH_TYPE', 1);
  VariableHash.Add('AUTH_NAME', 2);
  VariableHash.Add('AUTH_PASS', 3);
  VariableHash.Add('CONTENT_LENGTH', 4);
  VariableHash.Add('CONTENT_TYPE', 5);
  VariableHash.Add('GATEWAY_INTERFACE', 6);
  VariableHash.Add('PATH_INFO', 7);
  VariableHash.Add('PATH_TRANSLATED', 8);
  VariableHash.Add('QUERY_STRING', 9);
  VariableHash.Add('REMOTE_ADDR', 10);
  VariableHash.Add('REMOTE_HOST', 11);
  VariableHash.Add('REMOTE_USER', 12);
  VariableHash.Add('REQUEST_METHOD', 13);
  VariableHash.Add('SCRIPT_NAME', 14);
  VariableHash.Add('SERVER_NAME', 15);
  VariableHash.Add('SERVER_PORT', 16);
  VariableHash.Add('SERVER_PROTOCOL', 17);
  VariableHash.Add('SERVER_SOFTWARE', 18);
  VariableHash.Add('HTTP_COOKIE', 19);
  VariableHash.Add('HTTP_USER_AGENT', 20);
  VariableHash.Add('URL', 21);
  VariableHash.Add('HTTP_CACHE_CONTROL', 22);
  VariableHash.Add('HTTP_DATE', 23);
  VariableHash.Add('HTTP_ACCEPT', 24);
  VariableHash.Add('HTTP_FROM', 25);
  VariableHash.Add('HTTP_HOST', 26);
  VariableHash.Add('HTTP_IF_MODIFIED_SINCE', 27);
  VariableHash.Add('HTTP_REFERER', 28);
  VariableHash.Add('HTTP_CONTENT_ENCODING', 29);
  VariableHash.Add('HTTP_CONTENT_VERSION', 30);
  VariableHash.Add('HTTP_DERIVED_FROM', 31);
  VariableHash.Add('HTTP_EXPIRES', 32);
  VariableHash.Add('HTTP_TITLE', 33);
  VariableHash.Add('HTTP_CONNECTION', 34);
  VariableHash.Add('HTTP_AUTHORIZATION', 35);
  VariableHash.Add('DOCUMENT_ROOT', 36);
  VariableHash.Add('SERVER_ADMIN', 37);
  VariableHash.Add('SERVER_ADDR', 38);
  VariableHash.Add('HTTP_ACCEPT_LANGUAGE', 39);
  VariableHash.Add('HTTP_ACCEPT_ENCODING', 40);
  VariableHash.Add('HTTP_CLIENT_IP', 41);
  VariableHash.Add('REDIRECT_STATUS', 42);
  VariableHash.Add('HTTP_REDIRECT_STATUS', 43);
  VariableHash.Add('REDIRECT_URL', 44);
  VariableHash.Add('HTTP_IDSESSION', 45);
  VariableHash.Add('ALL_RAW', 46);
  VariableHash.Add('APP_POOL_ID', 47);
end;

procedure AssignStringToPAnsiChar(var Destination: PAnsiChar; const Source: ustring);
begin
  Destination := StrNew(PAnsiChar(AnsiString(Source)));
end;

function EncodeStringToAnsiString(const Value: string): AnsiString;
begin
  Result := {$IFDEF UNICODE}UTF8Encode(Value){$ELSE}Value{$ENDIF};
end;

function DecodeAnsiString(const Value: AnsiString): ustring;
begin
  Result := {$IFDEF UNICODE}UTF8ToWideString(Value){$ELSE}UTF8Decode(Value){$ENDIF};
end;

function IPAddrToName(const IPAddr: string): string;
var
  SockAddrIn: TSockAddrIn;
  HostEnt: PHostEnt;
  WSAData: TWSAData;
begin
  WSAStartup($101, WSAData);
  SockAddrIn.sin_addr.s_addr := inet_addr(PAnsiChar(Utf8Encode(IPAddr)));
  HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.S_addr, 4, AF_INET);

  if HostEnt <> nil then
    Result := string(StrPas(Hostent^.h_name))
  else
    Result := '';
end;

function CBGetServerVariable(ConnID: HCONN; VariableName: PAnsiChar; Buffer: Pointer; var Size: DWORD): Boolean;
const
  v_AUTH_TYPE = 1;
  v_AUTH_NAME = 2;
  v_AUTH_PASS = 3;
  v_CONTENT_LENGTH = 4;
  v_CONTENT_TYPE = 5;
  v_GATEWAY_INTERFACE = 6;
  v_PATH_INFO = 7;
  v_PATH_TRANSLATED = 8;
  v_QUERY_STRING = 9;
  v_REMOTE_ADDR = 10;
  v_REMOTE_HOST = 11;
  v_REMOTE_USER = 12;
  v_REQUEST_METHOD = 13;
  v_SCRIPT_NAME = 14;
  v_SERVER_NAME = 15;
  v_SERVER_PORT = 16;
  v_SERVER_PROTOCOL = 17;
  v_SERVER_SOFTWARE = 18;
  v_HTTP_COOKIE = 19;
  v_HTTP_USER_AGENT = 20;
  v_URL = 21;
  v_HTTP_CACHE_CONTROL = 22;
  v_HTTP_DATE = 23;
  v_HTTP_ACCEPT = 24;
  v_HTTP_FROM = 25;
  v_HTTP_HOST = 26;
  v_HTTP_IF_MODIFIED_SINCE = 27;
  v_HTTP_REFERER = 28;
  v_HTTP_CONTENT_ENCODING = 29;
  v_HTTP_CONTENT_VERSION = 30;
  v_HTTP_DERIVED_FROM = 31;
  v_HTTP_EXPIRES = 32;
  v_HTTP_TITLE = 33;
  v_HTTP_CONNECTION = 34;
  v_HTTP_AUTHORIZATION = 35;
  v_DOCUMENT_ROOT = 36;
  v_SERVER_ADMIN = 37;
  v_SERVER_ADDR = 38;
  v_HTTP_ACCEPT_LANGUAGE = 39;
  v_HTTP_ACCEPT_ENCODING = 40;
  v_HTTP_CLIENT_IP = 41;
  v_REDIRECT_STATUS = 42;
  v_HTTP_REDIRECT_STATUS = 43;
  v_REDIRECT_URL = 44;
  v_HTTP_IDSESSION = 45;
  v_ALL_RAW = 46;
  v_APP_POOL_ID = 47;

  function GetFieldByName(const ECB: TECBData; const AFieldName : AnsiString) : string;
  begin
    Result := ECB.HTTPHeaders.GetParam(string(AFieldName));
  end;

  procedure CreateStringEntry(const Input: string);
  var
    TempString: AnsiString;
  begin
    {$IFDEF UNICODE}TempString := UTF8Encode(Input){$ELSE}TempString := Input{$ENDIF};
    StrPCopy(PAnsiChar(Buffer), PAnsiChar(TempString));
    Size := Length(TempString) + SizeOf(AnsiChar);
  end;

  function AdjustHTTP(const Name: AnsiString): AnsiString;
  const
    SHttp = AnsiString('HTTP_'); { do not localize }
  begin
    if Pos(SHttp, Name) = 1 then
      Result := Copy(Name, 6, MaxInt)
    else
      Result := Name;
  end;

  function GetFieldByNameEx(const ECB: TECBData; const AFieldName : AnsiString) : string;
  var
    NewFieldName : AnsiString;
  begin
    NewFieldName := AdjustHTTP(AFieldName);

    // get the result from the HTTP headers
    Result := ECB.HTTPHeaders.GetParam(string(NewFieldName));

    if Result = '' then
    begin
      NewFieldName := StringReplace(NewFieldName, AnsiChar('_'), AnsiChar('-'), [rfReplaceALL]);
      Result := ECB.HTTPHeaders.GetParam(string(NewFieldName));
    end;
  end;

var
  ECB: TECBData;
  tmpS: string;
  VarNum: Integer;
  NamePos: Integer;
begin
  VarNum := VariableHash.ValueOf(string(VariableName));
  ECB := ECBDataList.GetECB(ConnID);
  Result := True;

  if ECB = nil then
  begin
    raise Exception.CreateFmt('ECB with connection ID %d not found in "CBGetServerVariable"', [ConnID]);
    Exit;
  end;

  if VarNum = -1 then
  begin
    tmpS := GetFieldByNameEx(ECB, VariableName);

    if tmpS <> '' then
    begin
      CreateStringEntry(tmpS);
      Result := True;
    end
    else
    begin
      StrPCopy(PAnsiChar(Buffer), #0);
      Result := False;
      Size := 0;
    end;

    Exit;
  end;

  case VarNum of
    v_AUTH_TYPE: CreateStringEntry('Basic');
    v_AUTH_NAME: CreateStringEntry(ECB.InputParams.GetParam('AuthUsername'));
    v_AUTH_PASS: CreateStringEntry(ECB.InputParams.GetParam('AuthPassword'));
    v_CONTENT_LENGTH: CreateStringEntry(ECB.HTTPHeaders.GetParam('Content-Length'));
    v_CONTENT_TYPE: CreateStringEntry(ECB.HTTPHeaders.GetParam('Content-Type'));
    v_GATEWAY_INTERFACE: CreateStringEntry('ISAPI');
    v_PATH_INFO: CreateStringEntry(string(ECB.ECB.lpszPathTranslated));
    v_PATH_TRANSLATED: CreateStringEntry(ECB.InputParams.GetParam('Document'));
    v_QUERY_STRING: CreateStringEntry(ECB.InputParams.GetParam('QueryParams'));
    v_REMOTE_ADDR: CreateStringEntry(ECB.InputParams.GetParam('RemoteIP'));
    v_REMOTE_HOST: CreateStringEntry(IPAddrToName(ECB.InputParams.GetParam('RemoteIP')));
    v_REMOTE_USER: CreateStringEntry(ECB.InputParams.GetParam('AuthUsername'));
    v_REQUEST_METHOD: CreateStringEntry(ECB.InputParams.GetParam('Command'));
    v_SCRIPT_NAME: CreateStringEntry(ECB.InputParams.GetParam('ScriptName'));
    v_SERVER_NAME: CreateStringEntry(ECB.InputParams.GetParam('Host'));
    v_SERVER_PORT: CreateStringEntry(ECB.InputParams.GetParam('Port'));
    v_SERVER_PROTOCOL: CreateStringEntry(ECB.InputParams.GetParam('Protocol'));
    v_SERVER_SOFTWARE: CreateStringEntry(ECB.InputParams.GetParam('Software'));
    v_HTTP_COOKIE: CreateStringEntry(ECB.HTTPHeaders.GetParam('Cookie'));
    v_HTTP_USER_AGENT: CreateStringEntry(ECB.HTTPHeaders.GetParam('User-Agent'));
    v_APP_POOL_ID: CreateStringEntry(ECB.InputParams.GetParam('AppPool'));
    v_ALL_RAW: CreateStringEntry(ECB.HTTPHeaders.Text);
    v_URL:
      begin
        NamePos := Pos('.DLL', UpperCase(ECB.InputParams.GetParam('Document')));

        case NamePos > 0 of
          True: tmpS := Copy(ECB.InputParams.GetParam('Document'), 1, NamePos + 3);
          False: tmpS := ECB.InputParams.GetParam('Document');
        end;

        CreateStringEntry(tmpS);
      end;

    //v 3.0
    v_HTTP_CACHE_CONTROL: CreateStringEntry(GetFieldByNameEx(ECB, 'CACHE_CONTROL'));
    v_HTTP_DATE: CreateStringEntry(GetFieldByName(ECB, 'DATE'));
    v_HTTP_ACCEPT: CreateStringEntry(GetFieldByName(ECB, 'ACCEPT'));
    v_HTTP_FROM: CreateStringEntry(GetFieldByName(ECB, 'FROM'));
    v_HTTP_HOST: CreateStringEntry(GetFieldByName(ECB, 'HOST'));
    v_HTTP_IF_MODIFIED_SINCE: CreateStringEntry(GetFieldByNameEx(ECB, 'IF-MODIFIED-SINCE'));
    v_HTTP_REFERER: CreateStringEntry(GetFieldByName(ECB, 'IF-MODIFIED-REFERER'));
    v_HTTP_CONTENT_ENCODING: CreateStringEntry(GetFieldByName(ECB, 'CONTENT-ENCODING'));
    v_HTTP_CONTENT_VERSION: CreateStringEntry(GetFieldByName(ECB, 'CONTENT-VERSION'));
    v_HTTP_DERIVED_FROM: CreateStringEntry(GetFieldByName(ECB, 'DERIVED-FROM'));
    v_HTTP_EXPIRES: CreateStringEntry(GetFieldByName(ECB, 'EXPIRES'));
    v_HTTP_TITLE: CreateStringEntry(GetFieldByName(ECB, 'TITLE'));
    v_HTTP_CONNECTION: CreateStringEntry(GetFieldByName(ECB, 'CONNECTION'));
    v_HTTP_AUTHORIZATION: CreateStringEntry(GetFieldByName(ECB, 'AUTHORIZATION'));
    v_DOCUMENT_ROOT: CreateStringEntry(ECB.InputParams.GetParam('DocumentRoot'));
    v_SERVER_ADMIN: CreateStringEntry(ECB.InputParams.GetParam('ServerAdmin'));
    v_SERVER_ADDR: CreateStringEntry(ECB.InputParams.GetParam('RemoteIP'));
    v_HTTP_ACCEPT_LANGUAGE: CreateStringEntry(GetFieldByName(ECB, 'ACCEPT-LANGUAGE'));
    v_HTTP_ACCEPT_ENCODING: CreateStringEntry(GetFieldByName(ECB, 'ACCEPT-ENCODING'));
    v_HTTP_CLIENT_IP: CreateStringEntry(ECB.InputParams.GetParam('RemoteIP'));
    v_REDIRECT_STATUS: CreateStringEntry('200');
    v_HTTP_REDIRECT_STATUS: CreateStringEntry('200');
    v_REDIRECT_URL: CreateStringEntry(ECB.InputParams.GetParam('Document'));
    v_HTTP_IDSESSION: CreateStringEntry(ECB.InputParams.GetParam('SessionID'));
    else
      Result := False;
  end;
end;

function CBServerSupport(ConnID: HCONN; HSERRequest: DWORD; Buffer: Pointer; Size: LPDWORD; dataType: LPDWORD): Boolean;
var
  ECB: TECBData;
  tmpS: string;
  Index: Integer;
  Cookie: string;
  tmpPath: string;
  TempUTF8: AnsiString;
  OutputData: AnsiString;
  MapInfo: THSE_URL_MAPEX_INFO;
  HeaderInfoEx: THSE_SEND_HEADER_EX_INFO;
begin
  ECB := ECBDataList.GetECB(ConnID);
  tmpS := string(PAnsiChar(Buffer));
  Result := True;

  if ECB = nil then
  begin
    raise Exception.CreateFmt('ECB with connection ID %d not found in "CBServerSupport"', [ConnID]);
    Exit;
  end;

  case HSERRequest of
    HSE_REQ_END_RESERVED + 20: ECB.InputParams.SetParam('SessionID', tmpS);
    HSE_REQ_ABORTIVE_CLOSE: ;
    HSE_REQ_MAP_URL_TO_PATH :
      begin
        tmpPath := IncludeTrailingPathDelimiter(ECB.InputParams.GetParam('DocumentRoot'));
        tmpPath := ExpandFilename(tmpPath + tmpS);

        // encode the string based on the unicode or not
        TempUTF8 := EncodeStringToAnsiString(tmpPath);

        if (DWORD(Length(TempUTF8)) > size^) then
        begin
          SetLastError(ERROR_INSUFFICIENT_BUFFER);
          Result := False;
          Exit;
        end;

        StrPCopy(PAnsiChar(Buffer), PAnsiChar(TempUTF8));
        // set the size for the buffer for the content
        Size^ := Length(TempUTF8) + SizeOf(AnsiChar);
    end;

    HSE_REQ_MAP_URL_TO_PATH_EX :
      begin
        tmpPath := IncludeTrailingPathDelimiter(ECB.InputParams.GetParam('DocumentRoot'));
        TmpPath := ExpandFilename(tmpPath + tmpS);

        // encode the string based on the unicode or not
        TempUTF8 := EncodeStringToAnsiString(tmpPath);

        MapInfo := THSE_URL_MAPEX_INFO(Pointer(DataType)^);
        Move(TempUTF8[1], MapInfo.lpszPath[0], Length(TempUTF8));
      end;

    HSE_APPEND_LOG_PARAMETER:
      begin
        if Assigned(ECB.LogParameterNotify) then
          ECB.LogParameterNotify(tmpS, ConnID);
      end;

    // not supported by our server
    HSE_REQ_TRANSMIT_FILE: Result := False;
    HSE_REQ_DONE_WITH_SESSION: SetEvent(ECB.WaitEvent);
    HSE_REQ_IS_KEEP_CONN: ECB.KeepAlive := Boolean(Buffer);
    HSE_REQ_SEND_URL_REDIRECT_RESP, HSE_REQ_SEND_URL: ECB.RedirectURL := Copy(tmpS,1, Length(tmpS));

    HSE_REQ_SEND_RESPONSE_HEADER_EX :
      begin
        HeaderInfoEx := HSE_SEND_HEADER_EX_INFO(Buffer^);

        ECB.OutputParams.InitializeList;
        ECB.OutputParams.Text := string(Copy(HeaderInfoEx.pszHeader, 0, HeaderInfoEx.cchHeader));

        if ECB.OutputParams[ECB.OutputParams.Count - 1] = '' then
          ECB.OutputParams.Delete(ECB.OutputParams.Count - 1);

        if HeaderInfoEx.fKeepConn then
          ECB.OutputParams.SetParam('Connection', 'Keep-Alive');

        for Index := 0 to ECB.OutputParams.Count - 1 do
        begin
          if Pos('Set-Cookie: ', ECB.OutputParams[Index]) = 1 then
          begin
            Cookie := Copy(ECB.OutputParams[Index], 12, Length(ECB.OutputParams[Index]) - 11);
            ECB.CookieList.Add(Cookie);
          end;
        end;

        if ECB.OutputParams.GetParam('Location') <> '' then
          ECB.ECB.dwHttpStatusCode := 302;

        OutputData := Copy(HeaderInfoEx.pszStatus, 0, HeaderInfoEx.cchStatus);
        WriteToStreamAsUTF8(ECB.OutputStream, OutputData);
      end;

    HSE_REQ_SEND_RESPONSE_HEADER:
      begin
          ECB.OutputParams.InitializeList;
          ECB.OutputParams.Text := string(Copy(PAnsiChar(DataType), 1, Length(PAnsiChar(DataType)) - 2));;

          if ECB.KeepAlive then
            ECB.OutputParams.SetParam('Connection', 'Keep-Alive');

          for Index := 0 to ECB.OutputParams.Count - 1 do
          begin
            if Pos('Set-Cookie: ', ECB.OutputParams[Index]) = 1 then
             begin
               Cookie := Copy(ECB.OutputParams[Index], 12, Length(ECB.OutputParams[Index]) - 11);
               ECB.CookieList.Add(Cookie);
             end;
          end;

          if ECB.OutputParams.GetParam('Location') <> '' then
            ECB.ECB.dwHttpStatusCode := 302;
      end;
    else
      Result := False;
  end;
end;

function CBWriteClient(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD): Boolean;
begin
  Result := True;
  try
    ECBDataList.GetECB(ConnID).OutputStream.Write(Buffer^, Bytes);
  except
    Result := False;
    Bytes := 0;
  end;
end;

function CBReadClient(ConnID: HCONN; Buffer: Pointer; var Size: DWORD): Boolean;
var
  ECB: TECBData;
begin
  Result := True;
  try
    ECB := ECBDataList.GetECB(ConnID);

    if ECB.DataPos < ECB.ECB.cbTotalBytes then
    begin
      Size := Min(Size, ECB.ECB.cbTotalBytes - ECB.DataPos);
      Move(ECB.ECB.lpbData^, Buffer^, Size);
      Inc(ECB.DataPos, Size);
    end;
  except
    Result := False;
    Size := 0;
  end;
end;

{ TISAPIServer }

constructor TISAPIServer.Create;
begin
  InitializeCriticalSection(FCriticalSection);
  FLibraryList := TStringList.Create;
end;

destructor TISAPIServer.Destroy;
var
  I: Integer;
begin
  // unload all loaded dlls
  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to FLibraryList.Count -1 do
      InternalUnloadDLL(I, False);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // delete critical section and library list
  DeleteCriticalSection(FCriticalSection);
  FreeAndNil(FLibraryList);

  inherited;
end;

function TISAPIServer.Execute(const DllName: string; const ControlBlock: TECBData; const Unload: Boolean): Boolean;
var
  Version: VersionFunc;
  dllHandle: Cardinal;
  ProcessFunc: ProcFunc;
  VersionInfo: THSE_VERSION_INFO;
begin
  FReturnCode := HSE_STATUS_ERROR;

  if not FileExists(DllName) then
  begin
    WriteToStreamAsString(ControlBlock.OutputStream, cErrorModuleNotFound);
    ControlBlock.ECB.dwHttpStatusCode := 404;

    raise Exception.Create(cErrorModuleNotFound);
  end;

  EnterCriticalSection(FCriticalSection);
  try
    dllHandle := StrToIntDef(FLibraryList.Values[DllName], 0);

    if dllHandle = 0 then
    begin
      dllHandle := LoadLibrary(PChar(DllName));
      FLibraryList.Values[DllName] := IntToStr(dllHandle);
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // get function addresses from the loaded dll library
  @Version := GetProcAddress(dllHandle, PChar('GetExtensionVersion'));
  @ProcessFunc := GetProcAddress(dllHandle, PCHar('HttpExtensionProc'));

  if (@Version = nil) then
  begin
    WriteToStreamAsString(ControlBlock.OutputStream, cErrorExtVersionNotAssigned);
    ControlBlock.ECB.dwHttpStatusCode := 500;

    raise Exception.Create(cErrorExtVersionNotAssigned);
  end;

  if (@ProcessFunc = nil) then
  begin
    WriteToStreamAsString(ControlBlock.OutputStream, cErrorExtProcNotAssigned);
    ControlBlock.ECB.dwHttpStatusCode := 500;

    raise Exception.Create(cErrorExtProcNotAssigned);
  end;

  if not Version(VersionInfo) then
  begin
    WriteToStreamAsString(ControlBlock.OutputStream, cErrorIncorectVersion);
    ControlBlock.ECB.dwHttpStatusCode := 500;

    raise Exception.Create(cErrorIncorectVersion);
  end;

  ControlBlock.WaitEvent := CreateEvent(nil, false, false, nil);
  try
    // assign event handling routine to control block
    ControlBlock.LogParameterNotify := FOnLogParameter;
    FReturnCode := ProcessFunc(ControlBlock.ECB);
    Result := FReturnCode = HSE_STATUS_SUCCESS;

    if FReturnCode = HSE_STATUS_ERROR then
    begin
      WriteToStreamAsString(ControlBlock.OutputStream, cErrorProcessingRequest);
      ControlBlock.ECB.dwHttpStatusCode := 500;

      raise Exception.Create(cErrorProcessingRequest);
    end;

    // wait until the processing is complete
    if FReturnCode = HSE_STATUS_PENDING then
    begin
      WaitForSingleObject(ControlBlock.WaitEvent, INFINITE);
      FReturnCode := HSE_STATUS_SUCCESS;
      Result := True;
    end;
  finally
    // always close the event handle
    CloseHandle(ControlBlock.WaitEvent);
  end;

  if Unload then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      InternalUnloadDLL(FLibraryList.IndexOfName(DllName), False);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end;
end;

function TISAPIServer.InternalUnloadDLL(const DLLIndex: Integer; const Ask: Boolean): Boolean;
var
  Handle: THandle;
  CanUnload: Boolean;
  TermProc: TTerminateExtension;
begin
  CanUnload := True;
  Result := False;

  if DLLIndex > -1 then
  begin
    Handle := StrToInt(FLibraryList.ValueFromIndex[DLLIndex]);
    @TermProc := GetProcAddress(Handle, 'TerminateExtension');

    if Assigned(TermProc) then
      CanUnload := not Ask or TermProc(HSE_TERM_ADVISORY_UNLOAD);

    if CanUnload then
    begin
      TermProc(HSE_TERM_MUST_UNLOAD);
      FLibraryList.Delete(DLLIndex);
      FreeLibrary(Handle);
      Result := True;
    end;
  end;
end;

{ TECBData }

constructor TECBData.Create(const ConnID: Cardinal);
begin
  OutputStream := TMemoryStream.Create;
  OutputParams := TParamsList.Create;
  InputParams := TParamsList.Create;
  HTTPHeaders := TParamsList.Create;
  CookieList := TParamsList.Create;
  RedirectURL := '';
  DataPos := 0;

  ECB.cbSize := SizeOf(TEXTENSION_CONTROL_BLOCK);
  ECB.dwVersion := MakeLong(HSE_VERSION_MINOR, HSE_VERSION_MAJOR);
  ECB.ServerSupportFunction := @CBServerSupport;
  ECB.GetServerVariable := @CBGetServerVariable;
  ECB.lpszLogData := 'DEFAULT LOG DATA';
  ECB.WriteClient := @CBWriteClient;
  ECB.ReadClient := @CBReadClient;
  ECB.lpszPathTranslated := nil;
  ECB.lpszContentType := nil;
  ECB.lpszQueryString := nil;
  ECB.lpszPathInfo := nil;
  ECB.lpszMethod := nil;
  ECB.lpbData := nil;
  ECB.cbTotalBytes := 0;
  ECB.cbAvailable := 0;
  ECB.ConnID := ConnID;
end;

destructor TECBData.Destroy;
begin
  // clean allocated ECB memory
  StrDispose(ECB.lpszPathTranslated);
  StrDispose(ECB.lpszQueryString);
  StrDispose(ECB.lpszContentType);
  StrDispose(ECB.lpszPathInfo);
  StrDispose(ECB.lpszMethod);
  FreeMem(ECB.lpbData);
  ECB.lpbData := nil;

  // release all other data
  FreeAndNil(OutputStream);
  FreeAndNil(OutputParams);
  FreeAndNil(InputParams);
  FreeAndNil(HTTPHeaders);
  FreeAndNil(CookieList);

  inherited;
end;

procedure TECBData.LoadFromStream(const Stream: TStreamStorage);
var
  DataStream: TMemoryStream;
begin
  ECB.dwHttpStatusCode := Stream.ReadInteger('ECB_dwHttpStatusCode');
  AssignStringToPAnsiChar(ECB.lpszPathTranslated, Stream.ReadString('ECB_lpszPathTranslated'));
  AssignStringToPAnsiChar(ECB.lpszContentType, Stream.ReadString('ECB_lpszContentType'));
  AssignStringToPAnsiChar(ECB.lpszQueryString, Stream.ReadString('ECB_lpszQueryString'));
  AssignStringToPAnsiChar(ECB.lpszPathInfo, Stream.ReadString('ECB_lpszPathInfo'));
  AssignStringToPAnsiChar(ECB.lpszMethod, Stream.ReadString('ECB_lpszMethod'));

  // read the data
  if Stream.Exists('ECB_lpbData') then
  begin
    DataStream := TMemoryStream.Create;
    try
      Stream.ReadStream('ECB_lpbData', DataStream);
      DataStream.Seek(0, soFromBeginning);

      if DataStream.Size > 0 then
      begin
        ECB.cbAvailable := DataStream.Size;
        ECB.cbTotalBytes := DataStream.Size;
        ECB.lpbData := AllocMem(DataStream.Size + cNullCharSize);

        // read the actual data from the stream to buffer
        DataStream.Read(ECB.lpbData^, DataStream.Size);
      end;
    finally
      DataStream.Free;
    end;
  end;

  Stream.ReadStream('OutputStream', OutputStream);
  OutputParams.Text := Stream.ReadString('OutputParams');
  HTTPHeaders.Text := Stream.ReadString('HTTPHeaders');
  InputParams.Text := Stream.ReadString('InputParams');
  CookieList.Text := Stream.ReadString('CookieList');
  RedirectURL := Stream.ReadString('RedirectURL');
  KeepAlive := Stream.ReadBoolean('KeepAlive');
end;

procedure TECBData.SaveToStream(const Stream: TStreamStorage);
var
  DataStream: TMemoryStream;
begin
  Stream.WriteString('ECB_lpszPathTranslated', string(ECB.lpszPathTranslated));
  Stream.WriteString('ECB_lpszContentType', string(ECB.lpszContentType));
  Stream.WriteString('ECB_lpszQueryString', string(ECB.lpszQueryString));
  Stream.WriteString('ECB_lpszPathInfo', string(ECB.lpszPathInfo));
  Stream.WriteString('ECB_lpszMethod', string(ECB.lpszMethod));
  Stream.WriteInteger('ECB_dwHttpStatusCode', ECB.dwHttpStatusCode);

  // assign the form data
  if ECB.lpbData <> nil then
  begin
    DataStream := TMemoryStream.Create;
    try
      DataStream.Write(ECB.lpbData^, ECB.cbAvailable);
      DataStream.Seek(0, soFromBeginning);

      // write the buffer to the ECB data stream
      Stream.WriteStream('ECB_lpbData', DataStream);
    finally
      DataStream.Free;
    end;
  end;

  OutputStream.Seek(0, soFromBeginning);
  Stream.WriteStream('OutputStream', OutputStream);
  Stream.WriteString('OutputParams', OutputParams.Text);
  Stream.WriteString('HTTPHeaders', HTTPHeaders.Text);
  Stream.WriteString('InputParams', InputParams.Text);
  Stream.WriteString('CookieList', CookieList.Text);
  Stream.WriteString('RedirectURL', RedirectURL);
  Stream.WriteBoolean('KeepAlive', KeepAlive);
end;

{ TECBDataList }

constructor TECBDataList.Create;
begin
  FEBCList := TCardinalHashTable.Create;
  FLock.Initialize;
end;

destructor TECBDataList.Destroy;
begin
  FreeAndNil(FEBCList);

  inherited;
end;

function TECBDataList.AcquireNewECB: TECBData;
begin
  FLock.AcquireExclusive;
  try
    Result := TECBData.Create(FEBCList.Count);
    FEBCList.Add(FEBCList.Count, Result);
  finally
    FLock.ReleaseExclusive;
  end;
end;

procedure TECBDataList.DeleteECB(const ConnID: HCONN);
var
  ECB: TECBData;
begin
  ECB := GetECB(ConnID);

  FLock.AcquireExclusive;
  try
    if ECB <> nil then
    begin
      FEBCList.Remove(ConnID);
      ECB.Free;
    end;
  finally
    FLock.ReleaseExclusive;
  end;
end;

function TECBDataList.GetECB(const ConnID: HCONN): TECBData;
begin
  FLock.AcquireShared;
  try
    Result := TECBData(FEBCList.Item[ConnID].AsObject);
  finally
    FLock.ReleaseShared;
  end;
end;

{ TParamsList }

function TParamsList.GetParam(const Name: string): string;
begin
  InitializeList;
  Result := Trim(Values[Name]);
end;

procedure TParamsList.InitializeList;
begin
  NameValueSeparator := ':';
  CaseSensitive := False;
end;

procedure TParamsList.SetParam(const Name, Value: string);
begin
  InitializeList;
  Values[Name] := Value;
end;

initialization
  ECBDataList := TECBDataList.Create;
  VariableHash := TStringHash.Create;
  FillVariableHashTable;

finalization
  FreeAndNil(VariableHash);
  FreeAndNil(ECBDataList);

end.
