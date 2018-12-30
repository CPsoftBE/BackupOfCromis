{ $Cromis: lib/Cromis.ServiceManager.pas,v 1.3 2008/04/13 17:09:35 ikacin Exp $ }
(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2008 Iztok Kacin, Cromis.
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
*)
unit Cromis.ServiceManager;

interface

uses
  SysUtils, Windows, WinSvc;

type

  TServiceManager = class
  private
    { Private declarations }
    ServiceControlManager: SC_Handle;
    ServiceHandle: SC_Handle;
  protected
    function DoStartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean;
  public
    { Public declarations }
    function Connect(MachineName: PChar = nil; DatabaseName: PChar = nil;
      Access: DWORD = SC_MANAGER_ALL_ACCESS): Boolean;  // Access may be SC_MANAGER_ALL_ACCESS
    function OpenServiceConnection(ServiceName: PChar): Boolean;
    function StartService: Boolean; overload; // Simple start
    function StartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean; overload; // More complex start
    procedure ChangeDescription(Description: PChar);
    function StopService: Boolean;
    procedure PauseService;
    procedure ContinueService;
    procedure ShutdownService;
    procedure DisableService;
    function GetStatus: DWORD;
    function ServiceRunning: Boolean;
    function ServiceStopped: Boolean;
  end;

implementation

{ TServiceManager }

procedure TServiceManager.ChangeDescription(Description: PChar);
const
  SERVICE_CONFIG_DESCRIPTION = 1;
type
  TChangeServiceConfig2 = function (hService: SC_HANDLE; dwInfoLevel: DWORD;
                                    lpInfo : pointer): BOOL; stdcall;
  TServiceDescription   = record
                            lpDescription : PChar;
                          end;

var
  lpServiceArgVectors  : PChar;
  hLibModule           : THandle;
  ServiceDescription   : TServiceDescription;
  ChangeServiceConfig2 : TChangeServiceConfig2;

begin
  // This starts the service after install:
  if not WinSvc.StartService(ServiceHandle, 0, lpServiceArgVectors) then
    RaiseLastOSError;

  if (Win32MajorVersion > 4) and (Win32Platform = VER_PLATFORM_WIN32_NT)
  then
  begin {Win2K or above - set service descripton}
    hLibModule := GetModuleHandle(PChar(advapi32));

    if hLibModule <> 0 then
    begin
      {$IFDEF UNICODE}
        @ChangeServiceConfig2 := GetProcAddress(hLibModule, 'ChangeServiceConfig2W');
      {$ELSE}
        @ChangeServiceConfig2 := GetProcAddress(hLibModule, 'ChangeServiceConfig2A');
      {$ENDIF}

      if @ChangeServiceConfig2 <> nil then
      begin
        ServiceDescription.lpDescription := PChar(Description);

        if not ChangeServiceConfig2(ServiceHandle, SERVICE_CONFIG_DESCRIPTION, @ServiceDescription) then
          RaiseLastOSError;
      end;
    end;
  end;
end;

function TServiceManager.Connect(MachineName, DatabaseName: PChar;
  Access: DWORD): Boolean;
begin
  { open a connection to the windows service manager }
  ServiceControlManager := OpenSCManager(MachineName, DatabaseName, Access);
  Result := (ServiceControlManager <> 0);
end;


function TServiceManager.OpenServiceConnection(ServiceName: PChar): Boolean;
begin
  { open a connetcion to a specific service }
  ServiceHandle := OpenService(ServiceControlManager, ServiceName, SERVICE_ALL_ACCESS);
  Result := (ServiceHandle <> 0);
end;

procedure TServiceManager.PauseService;
var
  ServiceStatus: TServiceStatus;
begin
  { Pause the service: attention not supported by all services }
  ControlService(ServiceHandle, SERVICE_CONTROL_PAUSE, ServiceStatus);
end;

function TServiceManager.StopService: Boolean;
var
  ServiceStatus: TServiceStatus;
begin
  { Stop the service }
  Result := ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus);
end;

procedure TServiceManager.ContinueService;
var
  ServiceStatus: TServiceStatus;
begin
  { Continue the service after a pause: attention not supported by all services }
  ControlService(ServiceHandle, SERVICE_CONTROL_CONTINUE, ServiceStatus);
end;

procedure TServiceManager.ShutdownService;
var
  ServiceStatus: TServiceStatus;
begin
  { Shut service down: attention not supported by all services }
  ControlService(ServiceHandle, SERVICE_CONTROL_SHUTDOWN, ServiceStatus);
end;

function TServiceManager.StartService: Boolean;
begin
  Result := DoStartService(0, '');
end;

function TServiceManager.StartService(NumberOfArgument: DWORD;
  ServiceArgVectors: PChar): Boolean;
begin
  Result := DoStartService(NumberOfArgument, ServiceArgVectors);
end;

function TServiceManager.GetStatus: DWORD;
var
  ServiceStatus: TServiceStatus;
begin
{ Returns the status of the service. Maybe you want to check this
  more than once, so just call this function again.
  Results may be: SERVICE_STOPPED
                  SERVICE_START_PENDING
                  SERVICE_STOP_PENDING
                  SERVICE_RUNNING
                  SERVICE_CONTINUE_PENDING
                  SERVICE_PAUSE_PENDING
                  SERVICE_PAUSED   }
  QueryServiceStatus(ServiceHandle, ServiceStatus);
  Result := ServiceStatus.dwCurrentState;
end;

procedure TServiceManager.DisableService;
begin
  { Implementation is following... }
end;

function TServiceManager.ServiceRunning: Boolean;
begin
  Result := (GetStatus = SERVICE_RUNNING);
end;

function TServiceManager.ServiceStopped: Boolean;
begin
  Result := (GetStatus = SERVICE_STOPPED);
end;

function TServiceManager.DoStartService(NumberOfArgument: DWORD; ServiceArgVectors: PChar): Boolean;
begin
  Result := WinSvc.StartService(ServiceHandle, NumberOfArgument, ServiceArgVectors);
end;

end.


