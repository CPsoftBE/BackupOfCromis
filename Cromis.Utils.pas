(*
 * This software is distributed under BSD license.
 *
 * Copyright (c) 2009-2014 Iztok Kacin, Cromis (iztok.kacin@gmail.com).
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
 * Different utilities used in the Cromis library.
 * =============================================================================
 * 05/03/2014 (1.0.0)
 *   - ThreadSafe AllocateHWnd / DeallocateHWnd is copy of code by Primož Gabrijelèiè
 *     Check http://www.thedelphigeek.com/2007/06/allocatehwnd-is-not-thread-safe.html
 * =============================================================================
*)
unit Cromis.Utils;

interface

{$IF CompilerVersion >= 23}{$DEFINE ScopedUnitNames}{$IFEND}
uses
  {$IFDEF ScopedUnitNames}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF ScopedUnitNames}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF ScopedUnitNames}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF ScopedUnitNames}Winapi.Messages{$ELSE}Messages{$ENDIF};

// ThreadSafe AllocateHWnd / DeallocateHWnd base on code by Primož Gabrijelèiè
function TSAllocateHWnd(wndProcMethod: TWndMethod): HWND;
procedure TSDeallocateHWnd(wnd: HWND);

implementation

const //TSAllocateHwnd window extra data offsets
  GWL_METHODCODE = SizeOf(pointer) * 0;
  GWL_METHODDATA = SizeOf(pointer) * 1;

  //TSAllocateHwnd hidden window (and window class) name
  CTSHiddenWindowName = 'Cromis_TS_UtilWindow';

var
  //TSAllocateHwnd lock
  GTSWndHandlerCritSect: TRTLCriticalSection;
  //Count of registered windows in this instance
  GTSWndHandlerCount: integer;

//Class message dispatcher for the TSUtilWindow class. Fetches instance's WndProc from
//the window extra data and calls it.
function TSClassWndProc(Window: HWND; Message, WParam, LParam: longint): longint; stdcall;
var
  instanceWndProc: TMethod;
  msg            : TMessage;
begin
  {$IFDEF CPUX64}
  instanceWndProc.Code := pointer(GetWindowLongPtr(Window, GWL_METHODCODE));
  instanceWndProc.Data := pointer(GetWindowLongPtr(Window, GWL_METHODDATA));
  {$ELSE}
  instanceWndProc.Code := pointer(GetWindowLong(Window, GWL_METHODCODE));
  instanceWndProc.Data := pointer(GetWindowLong(Window, GWL_METHODDATA));
  {$ENDIF ~CPUX64}
  if Assigned(TWndMethod(instanceWndProc)) then
  begin
    msg.msg := Message;
    msg.wParam := WParam;
    msg.lParam := LParam;
    msg.Result := 0;
    TWndMethod(instanceWndProc)(msg);
    Result := msg.Result
  end
  else
    Result := DefWindowProc(Window, Message, WParam,LParam);
end; { TSClassWndProc }

//Thread-safe AllocateHwnd.
//  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
//                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
//  @since   2007-05-30
function TSAllocateHWnd(wndProcMethod: TWndMethod): HWND;
var
  alreadyRegistered: boolean;
  tempClass        : TWndClass;
  utilWindowClass  : TWndClass;
begin
  Result := 0;
  FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
  EnterCriticalSection(GTSWndHandlerCritSect);
  try
    alreadyRegistered := GetClassInfo(HInstance, CTSHiddenWindowName, tempClass);
    if (not alreadyRegistered) or (tempClass.lpfnWndProc <> @TSClassWndProc) then
    begin
      if alreadyRegistered then
        {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CTSHiddenWindowName, HInstance);
      utilWindowClass.lpszClassName := CTSHiddenWindowName;
      utilWindowClass.hInstance := HInstance;
      utilWindowClass.lpfnWndProc := @TSClassWndProc;
      utilWindowClass.cbWndExtra := SizeOf(TMethod);
      if {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.RegisterClass(utilWindowClass) = 0 then
        raise Exception.CreateFmt('Unable to register TSWin32 hidden window class. %s',
          [SysErrorMessage(GetLastError)]);
    end;
    Result := CreateWindowEx(WS_EX_TOOLWINDOW, CTSHiddenWindowName, '', WS_POPUP,
      0, 0, 0, 0, 0, 0, HInstance, nil);
    if Result = 0 then
      raise Exception.CreateFmt('Unable to create TSWin32 hidden window. %s',
              [SysErrorMessage(GetLastError)]);
    {$IFDEF CPUX64}
    SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(wndProcMethod).Data));
    SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(wndProcMethod).Code));
    {$ELSE}
    SetWindowLong(Result, GWL_METHODDATA, cardinal(TMethod(wndProcMethod).Data));
    SetWindowLong(Result, GWL_METHODCODE, cardinal(TMethod(wndProcMethod).Code));
    {$ENDIF ~CPUX64}
    Inc(GTSWndHandlerCount);
  finally
    LeaveCriticalSection(GTSWndHandlerCritSect);
  end;
end; { TSAllocateHWnd }

//Thread-safe DeallocateHwnd.
//  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
//                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
//  @since   2007-05-30
procedure TSDeallocateHWnd(wnd: HWND);
begin
  if wnd = 0 then
    Exit;
  DestroyWindow(wnd);
  EnterCriticalSection(GTSWndHandlerCritSect);
  try
    Dec(GTSWndHandlerCount);
    if GTSWndHandlerCount <= 0 then
      {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CTSHiddenWindowName, HInstance);
  finally
    LeaveCriticalSection(GTSWndHandlerCritSect);
  end;
end; { TSDeallocateHWnd }

initialization
  InitializeCriticalSection(GTSWndHandlerCritSect);

finalization
  DeleteCriticalSection(GTSWndHandlerCritSect);

end.
