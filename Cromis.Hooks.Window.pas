(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2010 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Global window creation hook. Notify the app before every window is created
 * =============================================================================
 * 26/10/2010 (1.0.0)
 *   - Initial implementation of CBT hook.
 * 28/11/2010 (1.1.0)
 *   - Reorganized the code so it is more compact
 *   - Added ActiveHooks property to control which hooks are processed
 *   - Optionally can listen to only single thread
 * =============================================================================
*)
unit Cromis.Hooks.Window;

interface

uses
  Windows, SysUtils, Messages, Classes,

  // cromis units
  Cromis.Comm.Custom, Cromis.Comm.IPC, Cromis.Streams;

type
  PRect = ^TRect;
  TRect = record
    Top: Longint;
    Left: Longint;
    Right: Longint;
    Bottom: Longint;
  end;

  PWindowMsg = ^TWindowMsg;
  TWindowMsg = record
    MsgCode: Integer;
    // this is the info part
    WindowHandle: Cardinal;
    ParentHandle: Cardinal;
    ThreadHandle: Cardinal;
    WindowHeight: Integer;
    WindowWidth: Integer;
    MenuHandle: Cardinal;
    ProcessID: Cardinal;
    PositionX: Integer;
    PositionY: Integer;
    ProcessName: string;
    WindowClass: string;
    WindowTitle: string;
    WindowStyle: Cardinal;
    WindowStyleEx: Cardinal;
    // this can be changed
    AllowAction: Boolean;
  end;

  TCBTHookValues = (cbtDestroyWnd = HCBT_DESTROYWND,
                    cbtCreateWnd = HCBT_CREATEWND,
                    cbtMoveSize = HCBT_MOVESIZE,
                    cbtActivate = HCBT_ACTIVATE);
  TCBTActiveHooks = set of TCBTHookValues;

  // the callback event that notify about the new window being created
  TOnWindowMessage = procedure(const Sender: TObject; var WindowMessage: TWindowMsg) of Object;

const
  cAllCBTHooks = [cbtDestroyWnd, cbtCreateWnd, cbtMoveSize, cbtActivate];
  cAllThreads = 0;

type
  TWindowHook = class
  private
    FHookDLL: string;
    FHookSet: Boolean;
    FDLLHandle: Cardinal;
    FWndHandle: Cardinal;
    FIPCServer: TIPCServer;
    FActiveHooks: TCBTActiveHooks;
    FAllowDataChange: Boolean;
    FOnWindowMessage: TOnWindowMessage;
    procedure DeallocateHWnd(Wnd: HWND);
    procedure WatchWndProc(var Msg: TMessage);
    procedure OnIPCMessage(const Context: ICommContext; const Request, Response: IMessageData);
    procedure Request_WindowMessage(var WinMsg: PWindowMsg; const Request, Response: IIPCData);
    procedure Response_WindowMessage(var WinMsg: PWindowMsg; const Request, Response: IIPCData);
  public
    constructor Create;
    destructor Destroy; override;
    function Finalize: Boolean;
    function Initialize(const ThreadID: Cardinal = cAllThreads): Boolean;
    property OnWindowMessage: TOnWindowMessage read FOnWindowMessage write FOnWindowMessage;
    property AllowDataChange: Boolean read FAllowDataChange write FAllowDataChange;
    property ActiveHooks: TCBTActiveHooks read FActiveHooks write FActiveHooks;
    property HookDLL: string read FHookDLL write FHookDLL;
  end;

implementation


function GetModuleFileNameEx(inProcess: THandle; inModule: THandle;
                             Filename: PChar; size: DWord): DWord;
                             stdcall; external 'psapi.dll'
                             {$IFDEF UNICODE}
                               name 'GetModuleFileNameExW';
                             {$ELSE}
                               name 'GetModuleFileNameExA';
                             {$ENDIF}

const
  cMsgProcessSync = WM_USER + 101;
  cMsgProcessAsync = WM_USER + 102;

type
  TRemoveCBTHook = function: Boolean; stdcall;
  TSetCBTHook = function(const ThreadID: Cardinal): Boolean; stdcall;

{ TWindowHook }

constructor TWindowHook.Create;
begin
  FWndHandle := AllocateHWnd(WatchWndProc);
  FHookDLL := 'Cromis.WindowHook.dll';
  FActiveHooks := cAllCBTHooks;
  FHookSet := False;

  FIPCServer := TIPCServer.Create;
  FIPCServer.ServerName := 'Cromis_Hooks_Window';
  FIPCServer.OnExecuteRequest := OnIPCMessage;
end;

procedure TWindowHook.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));

  if Instance <> @DefWindowProc then
  begin
    { make sure we restore the default
      windows procedure before freeing memory }
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;

  DestroyWindow(Wnd);
end;

destructor TWindowHook.Destroy;
begin
  Finalize;
  FreeAndNil(FIPCServer);
  DeallocateHWnd(FWndHandle);

  inherited;
end;

function TWindowHook.Finalize: Boolean;
var
  RemoveHook: TRemoveCBTHook;
begin
  Result := False;

  if FHookSet then
  begin
    if FDLLHandle <> 0 then
    begin
      @RemoveHook := GetProcAddress(FDLLHandle, 'RemoveCBTHook');
      Result := RemoveHook;
    end;

    FreeLibrary(FDLLHandle);
    FIPCServer.Stop;

    // set hook flag
    FHookSet := False;
  end;
end;

function TWindowHook.Initialize(const ThreadID: Cardinal): Boolean;
var
  SetHook: TSetCBTHook;
begin
  Result := False;

  if not FHookSet then
  begin
    FDLLHandle := LoadLibrary(PChar(FHookDLL));
    FIPCServer.Start;

    if FDLLHandle <> 0 then
    begin
      @SetHook := GetProcAddress(FDLLHandle, 'SetCBTHook');
      Result := SetHook(ThreadID);
    end
    else
      raise Exception.CreateFmt('Could not find the "%s" library', [FHookDLL]);

    FHookSet := True;
  end;
end;

procedure TWindowHook.OnIPCMessage(const Context: ICommContext; const Request, Response: IMessageData);
var
  WindowMessage: PWindowMsg;
begin
  New(WindowMessage);

  if FAllowDataChange then
  begin
    Request_WindowMessage(WindowMessage, Request, Response);
    SendMessage(FWndHandle, cMsgProcessSync, Integer(WindowMessage), WindowMessage.MsgCode);
    Response_WindowMessage(WindowMessage, Request, Response);
    Dispose(WindowMessage);
  end
  else
  begin
    Request_WindowMessage(WindowMessage, Request, Response);
    Response_WindowMessage(WindowMessage, Request, Response);
    PostMessage(FWndHandle, cMsgProcessAsync, Integer(WindowMessage), WindowMessage.MsgCode);
  end;
end;

procedure TWindowHook.Request_WindowMessage(var WinMsg: PWindowMsg; const Request, Response: IIPCData);
var
  ProcessID: PDWORD;
  ProcessName: PChar;
  ProcessHandle: Cardinal;
begin
  GetMem(ProcessID, SizeOf(DWORD));
  try
    WinMsg.MsgCode := Request.Data.ReadCardinal('MsgCode');
    WinMsg.WindowHandle := Request.Data.ReadCardinal('WindowHandle');
    WinMsg.ParentHandle := Request.Data.ReadCardinal('ParentHandle');
    WinMsg.ThreadHandle := GetWindowThreadProcessId(WinMsg.WindowHandle, ProcessID);
    WinMsg.MenuHandle := Request.Data.ReadCardinal('MenuHandle');
    WinMsg.ProcessID := ProcessID^;
    WinMsg.WindowClass := Request.Data.ReadString('WindowClass');
    WinMsg.WindowTitle := Request.Data.ReadString('WindowTitle');
    WinMsg.PositionX := Request.Data.ReadInteger('PositionX');
    WinMsg.PositionY := Request.Data.ReadInteger('PositionY');
    WinMsg.WindowWidth := Request.Data.ReadInteger('WindowWidth');
    WinMsg.WindowHeight := Request.Data.ReadInteger('WindowHeight');
    WinMsg.WindowStyle := Request.Data.ReadInteger('WindowStyle');
    WinMsg.WindowStyleEx := Request.Data.ReadCardinal('WindowStyleEx');
    WinMsg.AllowAction := True;

    // try to get the process name that is creating the window
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID^);
    try
      if ProcessHandle <> 0 then
      begin
        GetMem(ProcessName, MAX_PATH * SizeOf(Char));
        try
          GetModuleFileNameEx(ProcessHandle, 0, ProcessName, SizeOf(Char) * MAX_PATH);
          WinMsg.ProcessName := StrPas(ProcessName);
        finally
          FreeMem(ProcessName);
        end;
      end;
    finally
      CloseHandle(ProcessHandle);
    end;
  finally
    FreeMem(ProcessID);
  end;
end;

procedure TWindowHook.Response_WindowMessage(var WinMsg: PWindowMsg; const Request, Response: IIPCData);
begin
  Response.Data.WriteBoolean('AllowAction', WinMsg^.AllowAction);

  if WinMsg^.MsgCode = HCBT_CREATEWND then
  begin
    Response.Data.WriteInteger('PositionX', WinMsg^.PositionX);
    Response.Data.WriteInteger('PositionY', WinMsg^.PositionY);
  end;

  if WinMsg^.MsgCode = HCBT_MOVESIZE then
  begin
    Response.Data.WriteCardinal('PositionX', WinMsg^.PositionX);
    Response.Data.WriteCardinal('PositionY', WinMsg^.PositionY);
    Response.Data.WriteCardinal('WindowWidth', WinMsg^.WindowWidth);
    Response.Data.WriteCardinal('WindowHeight', WinMsg^.WindowHeight);
  end;
end;

procedure TWindowHook.WatchWndProc(var Msg: TMessage);
var
  WindowMessage: PWindowMsg;
begin
  if TCBTHookValues(Msg.LParam) in FActiveHooks then
  begin
    if Assigned(FOnWindowMessage) then
    begin
      WindowMessage := PWindowMsg(Msg.WParam);
      FOnWindowMessage(Self, WindowMessage^);

      if Msg.msg = cMsgProcessAsync then
        Dispose(WindowMessage);
    end;
  end
  else
    Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
