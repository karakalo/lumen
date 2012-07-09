-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package Win32.User32 is

   function CreateWindowEx
     (dwExStyle    : DWORD_Type;
      lpClassName  : LPCTSTR_Type;
      lpWindowName : LPCTSTR_Type;
      dwStyle      : DWORD_Type;
      x            : Interfaces.C.int;
      y            : Interfaces.C.int;
      nWidth       : Interfaces.C.int;
      nHeight      : Interfaces.C.int;
      hWndParent   : HWND_Type;
      hMenu        : HMENU_Type;
      hInstance    : HINSTANCE_Type;
      lpParam      : System.Address)
      return HWND_Type;
   pragma Import(StdCall,CreateWindowEx,"CreateWindowExA");

   function DefWindowProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LResult_Type;
   pragma Import(StdCall,DefWindowProc,"DefWindowProcA");

   function SetFocus
     (hWnd : HWND_Type)
      return HWND_Type;
   pragma Import(StdCall,SetFocus,"SetFocus");

   function ShowWindow
     (hWnd     : HWND_Type;
      nCmdShow : Interfaces.C.int)
      return BOOL_Type;
   pragma Import(StdCall,ShowWindow,"ShowWindow");

   function GetDC
     (hWnd : HWND_Type)
      return HDC_Type;
   pragma Import(StdCall,GetDC,"GetDC");

   -- Replaced LONG_PTR_TYPE by LONG_Type as return type since
   -- it makes much more sense as an integer type than as a pointer
   function SetWindowLongPtr
     (hWnd      : HWND_Type;
      nIndex    : Interfaces.C.int;
      dwNewLong : LONG_PTR_Type)
      return LONG_Type;
   pragma Import(StdCall,SetWindowLongPtr,"SetWindowLongA");

   function GetWindowLongPtr
     (hWnd : HWND_Type;
      nIndex : Interfaces.C.int)
      return LONG_PTR_Type;
   pragma Import(StdCall,GetWindowLongPtr,"GetWindowLongA");

   function SetForegroundWindow
     (hWnd : HWND_Type)
      return BOOL_Type;
   pragma Import(StdCall,SetForegroundWindow,"SetForegroundWindow");

   function LoadIcon
     (hInstance  : HINSTANCE_Type;
      lpIconName : LPCTSTR_Type)
      return HICON_Type;
   pragma Import(StdCall,LoadIcon,"LoadIconA");

   function LoadCursor
     (hInstance : HINSTANCE_Type;
      lpCursorName : LPCTSTR_Type)
      return HCURSOR_Type;
   pragma Import(StdCall,LoadCursor,"LoadCursorA");

   function RegisterClass
     (lpWndClass : access WNDCLASS_Type)
      return ATOM_Type;
   pragma Import(StdCall,RegisterClass,"RegisterClassA");

   function ReleaseDC
     (hWnd : HWND_Type;
      hDC  : HDC_Type)
      return Interfaces.C.int;
   pragma Import(StdCall,ReleaseDC,"ReleaseDC");

   function DestroyWindow
     (hWnd : HWND_Type)
      return BOOL_Type;
   pragma Import(StdCall,DestroyWindow,"DestroyWindow");

   function UnregisterClass
     (lpClassName : LPCTSTR_Type;
      hInstance   : HINSTANCE_Type)
      return BOOL_Type;
   pragma Import(StdCall,UnregisterClass,"UnregisterClassA");

   function GetMessage
     (lpMsg         : access MSG_Type;
      hWnd          : HWND_Type;
      wMsgFilterMin : UINT_Type;
      wMsgFilterMax : UINT_Type)
      return BOOL_Type;
   pragma Import(StdCall,GetMessage,"GetMessageA");

   function PeekMessage
     (lpMsg         : access MSG_Type;
      hWnd          : HWND_Type;
      wMsgFilterMin : UINT_Type;
      wMsgFilterMax : UINT_Type;
      wRemoveMsg    : UINT_Type)
      return BOOL_Type;
   pragma Import(StdCall,PeekMessage,"PeekMessageA");

   function PostMessage
     (hWnd   : HWND_Type;
      Msg    : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return BOOL_Type;
   pragma Import(StdCall,PostMessage,"PostMessageA");

   function TranslateMessage
     (lpMsg : access MSG_Type)
      return BOOL_Type;
   pragma Import(StdCall,TranslateMessage,"TranslateMessage");

   function DispatchMessage
     (lpMsg : access MSG_Type)
      return LRESULT_Type;
   pragma Import(StdCall,DispatchMessage,"DispatchMessageA");

   function SetCapture
     (hWnd : HWND_Type)
      return HWND_Type;
   pragma Import(StdCall,SetCapture,"SetCapture");

   function ReleaseCapture
     return BOOL_Type;
   pragma Import(StdCall,ReleaseCapture,"ReleaseCapture");

   function SetTimer
     (hwnd        : HWND_Type;
      nIDEvent    : UINT_PTR_Type;
      uElapse     : UINT_Type;
      lpTimerFunc : System.Address)
     return UINT_PTR_Type;
   pragma Import(StdCall,SetTimer,"SetTimer");

end Win32.User32;
