(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2012-2013 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Fast IMC client server communication based on Indy TCP. Very lightweight.
 * =============================================================================
 * 01/04/2012 (1.0.0)
 *   - Initial implementation
 * 01/04/2012 (1.1.0)
 *   - Optimized the code for speed
 *   - Restructured some of the code and some of the procedures and properties
 *   - Added execute timeouts, to both server and client
 * 07/04/2012 (1.1.1)
 *   - Code optimizations
 *   - Execute timeout for server
 *   - ErroDesc for client
 * 14/05/2012 (1.1.2)
 *   - Same code syntax as in IPC
 * 22/05/2012 (1.1.3)
 *   - Added IsConnected property
 * 26/03/2013 (1.2.0)
 *   - Made INDY9 and BDS 2006 upward compatible
 * 07/04/2013 (1.3.0)
 *   - Greatly improved speed by using buffering and eliminating some waits
 *   - Better handling of internal server errors when executing requests
 * 25/05/2013 (1.3.1)
 *   - Catch the Indy EIdNotConnected and not let it through
 * 29/05/2013 (1.3.2)
 *   - Do not move ID bytes if ID is not set
 * 18/07/2013 (1.4.0)
 *   - OnClientConnect, OnClientDisconnect, OnServerError handlers for server
 *   - Internal list of all connected clients (optional)
 *   - Pass client context inside OnExecute handler
 *   - Improved error handling and notifications
 * 19/07/2013 (1.5.0)
 *   - Exposed Bindings of TCP server with proxy interfaces
 *   - ServerPort changed to DefaultPort
 *   - Unit renamed to Cromis.Comm.IMC
 * 20/08/2013 (1.6.0)
 *   - Reorganized context and client classes
 * 10/03/2014 (1.7.0)
 *   - support for filters (encryption, compression...)
 * =============================================================================
*)
unit Cromis.Comm.IMC;

interface

uses
  Windows, SysUtils, Classes, DateUtils,

  // Indy units
  IdCustomTCPServer, idContext, IdExceptionCore, IdYarn,
  IdBaseComponent, IdComponent, IdTCPServer, IdTCPConnection, IdException,
  IdTCPClient, IdGlobal, IdSocketHandle,

  // cromis units
  Cromis.Streams, Cromis.Streams.Filters, Cromis.StringUtils, Cromis.Unicode,
  Cromis.Threading.CustomPool, Cromis.Comm.Custom;

const
  cBufferSize = 65536;

const
  cDefaultTimeout = 10000;

const
  cInitialPoolSize = 20;

type
  IIMCData = IMessageData;

type
  // indy version specific declarations
  TIMCContext = TIdContext;
  TIPVersion = (ipV4, ipV6);

  IBinding = Interface(IInterface)
  ['{0D479D46-085E-49A1-B27D-F999C578CDD3}']
    function GetIP: string;
    function GetPort: Integer;
    function GetIPVersion: TIPVersion;
    function GetClientPortMin: Integer;
    function GetClientPortMax: Integer;
    function GetBroadcastEnabled: Boolean;
    procedure SetIP(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetClientPortMin(const Value: Integer);
    procedure SetClientPortMax(const Value: Integer);
    procedure SetBroadcastEnabled(const Value: Boolean);
    property IP: string read GetIP write SetIP;
    property Port: Integer read GetPort write SetPort;
    property IPVersion: TIPVersion read GetIPVersion write SetIPVersion;
    property ClientPortMin: Integer read GetClientPortMin write SetClientPortMin;
    property ClientPortMax: Integer read GetClientPortMax write SetClientPortMax;
    property BroadcastEnabled: Boolean read GetBroadcastEnabled write SetBroadcastEnabled;
  end;

  IBindings = Interface(IInterface)
  ['{CDA81D85-8A0C-48AC-BBF0-FE2CE6DD0F7B}']
    function GetItem(const Index: Integer): IBinding;
    procedure SetItem(const Index: Integer; const Value: IBinding);
    procedure Clear;
    function Add: IBinding;
    procedure Delete(const Index: Integer);
    property Items[const Index: Integer]: IBinding read GetItem write SetItem; default;
  end;

  {
    our TCP server with single override
  }
  TIMCTCPServer = class(TIdTCPServer)
  private
    FCommClientClass: TCommClientClass;
  protected
    procedure DoConnect(AContext: TIMCContext); override;
    property CommClientClass: TCommClientClass read FCommClientClass write FCommClientClass;
  end;

  {
    main server class
  }
  TIMCServer = class
  private
    FFilters: TStreamFilters;
    FBindings: IBindings;
    FTCPServer: TIMCTCPServer;
    FContextList: TCommContextList;
    FOnServerError: TOnServerError;
    FExecuteTimeout: Cardinal;
    FOnClientConnect: TOnClientEvent;
    FOnExecuteRequest: TOnExecuteRequest;
    FOnClientDisconnect: TOnClientEvent;
    function GetListening: Boolean;
    function GetDefaultPort: Integer;
    function GetMinPoolSize: Integer;
    function GetCommClientClass: TCommClientClass;
    procedure SetDefaultPort(const Value: Integer);
    procedure SetMinPoolSize(const Value: Integer);
    procedure OnServerExecute(AContext: TIMCContext);
    procedure DoOnClientConnect(AContext: TIMCContext);
    procedure DoOnClientDisconnect(AContext: TIMCContext);
    procedure SetCommClientClass(const Value: TCommClientClass);
  public
    procedure Stop;
    procedure Start;
    procedure Restart;
    constructor Create;
    destructor Destroy; override;
    property Bindings: IBindings read FBindings;
    property Listening: Boolean read GetListening;
    property Filters: TStreamFilters read FFilters;
    property DefaultPort: Integer read GetDefaultPort write SetDefaultPort;
    property MinPoolSize: Integer read GetMinPoolSize write SetMinPoolSize;
    property ExecuteTimeout: Cardinal read FExecuteTimeout write FExecuteTimeout;
    property OnServerError: TOnServerError read FOnServerError write FOnServerError;
    property OnClientConnect: TOnClientEvent read FOnClientConnect write FOnClientConnect;
    property CommClientClass: TCommClientClass read GetCommClientClass write SetCommClientClass;
    property OnExecuteRequest: TOnExecuteRequest read FOnExecuteRequest write FOnExecuteRequest;
    property OnClientDisconnect: TOnClientEvent read FOnClientDisconnect write FOnClientDisconnect;
  end;

  {
    main client class
  }
  TIMCClient = class
  private
    FFilters: TStreamFilters;
    FErrorDesc: string;
    FLastError: Cardinal;
    FTCPClient: TIdTCPClient;
    FAnswerValid: Boolean;
    FIsConnected: Boolean;
    FServerAddress: string;
    FExecuteTimeout: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectClient(const ConnectTimeout: Cardinal = cDefaultTimeout);
    function ExecuteConnectedRequest(const Request: IMessageData): IMessageData; overload;
    function ExecuteRequest(const Request: IMessageData; const ConnectTimeout: Cardinal = cDefaultTimeout): IMessageData;
    procedure DisconnectClient;
    property ExecuteTimeout: Cardinal read FExecuteTimeout write FExecuteTimeout;
    property ServerAddress: string read FServerAddress write FServerAddress;
    property AnswerValid: Boolean read FAnswerValid;
    property IsConnected: Boolean read FIsConnected;
    property Filters: TStreamFilters read FFilters;
    property LastError: Cardinal read FLastError;
    property ErrorDesc: string read FErrorDesc;
  end;

  TIMCClientObject = class(TCustomObject)
  private
    FIMCClient: TIMCClient;
  public
    constructor Create; override;
    destructor Destroy; override;
    property IMCClient: TIMCClient read FIMCClient;
  end;

  TIMCClientPool = class(TTSObjectsPool)
  public
    constructor Create; reintroduce;
    function AcquireObject: TIMCClientObject; reintroduce;
  end;

  function AcquireIMCData: IMessageData;

implementation

type
  TBinding = class(TInterfacedObject, IBinding)
  private
    FSocketHandle: TIdSocketHandle;
    function GetIP: string;
    function GetPort: Integer;
    function GetIPVersion: TIPVersion;
    function GetClientPortMin: Integer;
    function GetClientPortMax: Integer;
    function GetBroadcastEnabled: Boolean;
    procedure SetIP(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetClientPortMin(const Value: Integer);
    procedure SetClientPortMax(const Value: Integer);
    procedure SetBroadcastEnabled(const Value: Boolean);
  public
    constructor Create(const SocketHandle: TIdSocketHandle);
    property IP: string read GetIP write SetIP;
    property Port: Integer read GetPort write SetPort;
    property IPVersion: TIPVersion read GetIPVersion write SetIPVersion;
    property ClientPortMin: Integer read GetClientPortMin write SetClientPortMin;
    property ClientPortMax: Integer read GetClientPortMax write SetClientPortMax;
    property BroadcastEnabled: Boolean read GetBroadcastEnabled write SetBroadcastEnabled;
  end;

  TBindings = class(TInterfacedObject, IBindings)
  private
    FBindings: TInterfaceList;
    FSocketHandles: TIdSocketHandles;
    function GetItem(const Index: Integer): IBinding;
    procedure SetItem(const Index: Integer; const Value: IBinding);
  public
    constructor Create(const SocketHandles: TIdSocketHandles);
    procedure Clear;
    function Add: IBinding;
    procedure Delete(const Index: Integer);
    property Items[const Index: Integer]: IBinding read GetItem write SetItem; default;
  end;

  TServerContext = class(TIdServerContext)
  private
    FContext: ICommContext;
  public
    property Context: ICommContext read FContext write FContext;
  end;

function AcquireIMCData: IMessageData;
begin
  Result := TMessageData.Create;
end;

// *****************************************************************************************
// ***************************** START OF INDY9/10 HELPERS *********************************
// *****************************************************************************************

function GetBufferSize(const AConnection: TIdTCPConnection): Integer;
begin
  Result := AConnection.IOHandler.InputBuffer.Size;
end;

procedure ReadBuff(const AConnection: TIdTCPConnection;
                   const DataLength: Integer;
                   var IDAsBytes: TIdBytes);
begin
  AConnection.IOHandler.ReadBytes(IDAsBytes, DataLength);
end;

function ReadInt(const AConnection: TIdTCPConnection):Int64;
begin
  Result := AConnection.IOHandler.ReadInt64;
end;

// *****************************************************************************************
// ******************************* END OF INDY9/10 HELPERS *********************************
// *****************************************************************************************

{ TIMCServer }

constructor TIMCServer.Create;
begin
  FContextList := TCommContextList.Create;
  FFilters := TStreamFilters.Create;

  // create the TCP server
  FTCPServer := TIMCTCPServer.Create(nil);
  FTCPServer.ListenQueue := cInitialPoolSize;
  FTCPServer.OnExecute := OnServerExecute;
  FTCPServer.OnConnect := DoOnClientConnect;
  FTCPServer.OnDisconnect := DoOnClientDisconnect;
  FTCPServer.ContextClass := TServerContext;

  // set the default client context class
  FTCPServer.CommClientClass := TCommClient;

  // create the bindings proxy object
  FBindings := TBindings.Create(FTCPServer.Bindings);
  FExecuteTimeout := 60000;
end;

destructor TIMCServer.Destroy;
begin
  Stop;
  FreeAndNil(FTCPServer);
  FreeAndNil(FFilters);
  FreeAndNil(FContextList);

  inherited;
end;

function TIMCServer.GetCommClientClass: TCommClientClass;
begin
  Result := FTCPServer.CommClientClass;
end;

function TIMCServer.GetListening: Boolean;
begin
  Result := FTCPServer.Active;
end;

function TIMCServer.GetMinPoolSize: Integer;
begin
  Result := FTCPServer.ListenQueue;
end;

function TIMCServer.GetDefaultPort: Integer;
begin
  Result := FTCPServer.DefaultPort;
end;

procedure TIMCServer.DoOnClientConnect(AContext: TIMCContext);
begin
  FContextList.AddContext(TServerContext(AContext).Context);

  if Assigned(FOnClientConnect) then
    FOnClientConnect(TServerContext(AContext).Context);
end;

procedure TIMCServer.DoOnClientDisconnect(AContext: TIMCContext);
begin
  if Assigned(FOnClientDisconnect) then
    FOnClientDisconnect(TServerContext(AContext).Context);

  FContextList.RemoveContext(TServerContext(AContext).Context);
end;

procedure TIMCServer.OnServerExecute(AContext: TIMCContext);
var
  Request: IMessageData;
  Response: IMessageData;
  IMCError: TServerError;
  IDAsBytes: TIdBytes;
  DataLength: Int64;
  IDAsString: ustring;
  UsedFilters: IUsedFilters;
begin
  try
    with AContext.Connection.IOHandler do
    begin
      ReadTimeout := FExecuteTimeout;

      // Acquire the data objects
      Request := AcquireIMCData;
      Response := AcquireIMCData;

      // read the message ID
      DataLength := ReadInt(AContext.Connection);

      if DataLength > 0 then
      begin
        ReadBuff(AContext.Connection, DataLength, IDAsBytes);
        SetLength(IDAsString, DataLength div SizeOf(uchar));
        Move(IDAsBytes[0], IDAsString[1], DataLength);
        Request.ID := IDAsString;
      end;

      // read the message data
      DataLength := ReadInt(AContext.Connection);
      ReadStream(Request.Data.Storage, DataLength, False);

      Request.Data.Storage.Seek(0, soFromBeginning);
      try
        if FFilters.Count > 0 then
          UsedFilters := Request.Data.ApplyOutputFilters(FFilters)
        else
          UsedFilters := nil;

        // execute the actual request handler
        FOnExecuteRequest(TServerContext(AContext).Context, Request, Response);

        if UsedFilters <> nil then
        begin
          if UsedFilters.FilterList.Count > 0 then
            Response.Data.ApplyInputFilters(UsedFilters.FilterList);
          // Interfaced Object, Auto Destroy
          UsedFilters := Nil;
        end;

        // send the response back to the caller
        WriteBufferOpen;
        try
          // write the data stream to TCP
          Response.Data.Storage.Seek(0, soFromBeginning);
          DataLength := Length(Response.ID) * SizeOf(uchar);
          SetLength(IDAsBytes, DataLength);

          if DataLength > 0 then
            // write ID as binary data
            Move(Response.ID[1], IDAsBytes[0], DataLength);

          // write data
          Write(DataLength);
          Write(IDAsBytes, DataLength);
          Write(Response.Data.Storage.Size);
          Write(Response.Data.Storage);
        finally
          WriteBufferClose;
        end;
      except
        on E: Exception do
        begin
          // we had an error, send back the -1 as data size to indicate that
          WriteBufferOpen;
          try
            DataLength := -1;
            Write(DataLength);
          finally
            WriteBufferClose;
          end;

          if Assigned(FOnServerError) then
          begin
            IMCError.Desc := E.Message;
            IMCError.Code := GetLastError;
            FOnServerError(TServerContext(AContext).Context, IMCError);
          end;
        end;
      end;
      // Clear
      Request.Clear;
      Response.Clear;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnServerError) and not (E is EIdConnClosedGracefully) then
      begin
        IMCError.Desc := E.Message;
        IMCError.Code := GetLastError;
        FOnServerError(TServerContext(AContext).Context, IMCError);
      end;
    end;
  end;
end;

procedure TIMCServer.Restart;
begin
  Stop;
  Start;
end;

procedure TIMCServer.SetCommClientClass(const Value: TCommClientClass);
begin
  FTCPServer.CommClientClass := Value;
end;

procedure TIMCServer.SetMinPoolSize(const Value: Integer);
begin
  FTCPServer.ListenQueue := Value;
end;

procedure TIMCServer.SetDefaultPort(const Value: Integer);
begin
  FTCPServer.DefaultPort := Value;
end;

procedure TIMCServer.Start;
begin
  if not Assigned(FOnExecuteRequest) then
    raise Exception.Create('OnExecuteRequest not assigned');

  FTCPServer.Active := True;
end;

procedure TIMCServer.Stop;
begin
  if FTCPServer.Active then
    FTCPServer.Active := False;
  FContextList.Clear;
end;

{ TIMCClient }

constructor TIMCClient.Create;
begin
  FTCPClient := TIdTCPClient.Create(nil);
  FFilters := TStreamFilters.Create;
  FExecuteTimeout := 60000;
end;

destructor TIMCClient.Destroy;
begin
  DisconnectClient;
  FreeAndNil(FFilters);
  FreeAndNil(FTCPClient);

  inherited;
end;

procedure TIMCClient.ConnectClient(const ConnectTimeout: Cardinal);
begin
  FIsConnected := False;

  if Trim(FServerAddress) = '' then
    raise Exception.Create('Server address is not defined!');

  // set host IP and host port
  FTCPClient.Host := StrBefore(':', FServerAddress);
  FTCPClient.Port := StrToInt(StrAfter(':', FServerAddress));

  FTCPClient.ConnectTimeout := ConnectTimeout;
  FTCPClient.Connect;

  // set if we are connected or not
  FIsConnected := FTCPClient.Connected;
end;

procedure TIMCClient.DisconnectClient;
begin
  try
    // if FTCPClient.Connected then // BugFix CPsoft
    // begin
    FTCPClient.Disconnect;
    FIsConnected := False;
    // end;
  except
    on E: EIdNotConnected do
    begin
      // just set connected to false
      FIsConnected := False;
    end;
  end;
end;

function TIMCClient.ExecuteRequest(const Request: IMessageData;
                                   const ConnectTimeout: Cardinal
                                   ): IMessageData;
begin
  ConnectClient(ConnectTimeout);
  try
    Result := ExecuteConnectedRequest(Request);
  finally
    DisconnectClient;
  end;
end;

function TIMCClient.ExecuteConnectedRequest(const Request: IMessageData): IMessageData;
var
  IDAsBytes: TIdBytes;
  DataLength: Int64;
  IDAsString: ustring;
begin
  Result := AcquireIMCData;
  FAnswerValid := False;
  try
    // check for filters
    if FFilters.Count > 0 then
      Request.Data.ApplyInputFilters(FFilters);

    with FTCPClient.IOHandler do
    begin
      WriteBufferOpen;
      try
        Request.Data.Storage.Seek(0, soFromBeginning);
        DataLength := Length(Request.ID) * SizeOf(uchar);
        SetLength(IDAsBytes, DataLength);

        if DataLength > 0 then
          // write ID as binary data
          Move(Request.ID[1], IDAsBytes[0], DataLength);

        // write data
        Write(DataLength);
        Write(IDAsBytes, DataLength);
        Write(Request.Data.Storage.Size);
        Write(Request.Data.Storage);
      finally
        WriteBufferClose;
      end;

      // set read timeout and read
      ReadTimeout := FExecuteTimeout;
      // read the message ID first
      DataLength := ReadInt(FTCPClient);

      if DataLength = -1 then
      begin
        // -1 as data lenght indicates internal server error while executing request
        FErrorDesc := 'Unhandled error while executing request on the server';
        FLastError := GetLastError;
        FAnswerValid := False;
        Exit;
      end;

      if DataLength > 0 then
      begin
        ReadBuff(FTCPClient, DataLength, IDAsBytes);
        SetLength(IDAsString, DataLength div SizeOf(uchar));
        Move(IDAsBytes[0], IDAsString[1], DataLength);
        Result.ID := IDAsString;
      end;

      // read the message data
      DataLength := ReadInt(FTCPClient);
      ReadStream(Result.Data.Storage, DataLength, False);

      // check for filters
      if FFilters.Count > 0 then
        Result.Data.ApplyOutputFilters(FFilters);

      // rewind the stream to be ready for reading
      Result.Data.Storage.Seek(0, soFromBeginning);
      // we were succesfull
      FAnswerValid := True;
    end;
  except
    on E: Exception do
    begin
      FLastError := GetLastError;
      FErrorDesc := E.Message;
      FAnswerValid := False;
    end;
  end;
end;

{ TIMCTCPServer }

procedure TIMCTCPServer.DoConnect(AContext: TIMCContext);
begin
  TServerContext(AContext).Context := AcquireCommContext(FCommClientClass);

  inherited;
end;

{ TBindings }

function TBindings.Add: IBinding;
begin
  Result := TBinding.Create(FSocketHandles.Add);
end;

procedure TBindings.Clear;
begin
  FBindings.Clear;
  FSocketHandles.Clear;
end;

constructor TBindings.Create(const SocketHandles: TIdSocketHandles);
begin
  FSocketHandles := SocketHandles;
end;

procedure TBindings.Delete(const Index: Integer);
begin
  FBindings.Delete(Index);
  FSocketHandles.Delete(Index);
end;

function TBindings.GetItem(const Index: Integer): IBinding;
begin
  Result := IBinding(FBindings.Items[Index]);
end;

procedure TBindings.SetItem(const Index: Integer; const Value: IBinding);
begin
  FBindings.Items[Index] := Value;
end;

{ TBinding }

constructor TBinding.Create(const SocketHandle: TIdSocketHandle);
begin
  FSocketHandle := SocketHandle;
end;

function TBinding.GetBroadcastEnabled: Boolean;
begin
  Result := FSocketHandle.BroadcastEnabled;
end;

function TBinding.GetClientPortMax: Integer;
begin
  Result := FSocketHandle.ClientPortMax;
end;

function TBinding.GetClientPortMin: Integer;
begin
  Result := FSocketHandle.ClientPortMin;
end;

function TBinding.GetIP: string;
begin
  Result := FSocketHandle.IP;
end;

function TBinding.GetIPVersion: TIPVersion;
begin
  case FSocketHandle.IPVersion of
    Id_IPv4: Result := ipV4;
    Id_IPv6: Result := ipV6;
    else
      raise Exception.Create('Unknown IP version!');
  end;
end;

function TBinding.GetPort: Integer;
begin
  Result := FSocketHandle.Port;
end;

procedure TBinding.SetBroadcastEnabled(const Value: Boolean);
begin
  FSocketHandle.BroadcastEnabled := Value;
end;

procedure TBinding.SetClientPortMax(const Value: Integer);
begin
  FSocketHandle.ClientPortMax := Value;
end;

procedure TBinding.SetClientPortMin(const Value: Integer);
begin
  FSocketHandle.ClientPortMin := Value;
end;

procedure TBinding.SetIP(const Value: string);
begin
  FSocketHandle.IP := Value;
end;

procedure TBinding.SetIPVersion(const Value: TIPVersion);
begin
  case Value of
    ipV4: FSocketHandle.IPVersion := Id_IPv4;
    ipV6: FSocketHandle.IPVersion := Id_IPv6;
    else
      raise Exception.Create('Unknown IP version!');
  end;
end;

procedure TBinding.SetPort(const Value: Integer);
begin
  FSocketHandle.Port := Value;
end;

{ TIMCClientObject }

constructor TIMCClientObject.Create;
begin
  inherited;

  FIMCClient := TIMCClient.Create;
end;

destructor TIMCClientObject.Destroy;
begin
  FreeAndNil(FIMCClient);

  inherited;
end;

{ TIMCClientPool }

constructor TIMCClientPool.Create;
begin
  inherited Create(TIMCClientObject);
end;

function TIMCClientPool.AcquireObject: TIMCClientObject;
begin
  Result := (inherited AcquireObject) as TIMCClientObject;
end;

end.
