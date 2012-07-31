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

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Unchecked_Conversion;

package Win32 is
   pragma Linker_Options ("-lgdi32");
   pragma Linker_Options ("-lopengl32");
   pragma Linker_Options ("-lglu32");

   type UINT_Type is new Interfaces.Unsigned_32;
   type DWORD_Type is new Interfaces.Unsigned_32;
   type HANDLE_Type is new System.Address;
   type HANDLE_Access is access HANDLE_Type;
   type UINT_PTR_Type is new Interfaces.C.ptrdiff_t;
   subtype LONG_PTR_Type is System.Address;
   type LONG_Type is new Interfaces.C.ptrdiff_t;
   subtype ULONG_PTR_Type is System.Address;
   subtype HWND_Type is HANDLE_Type;
   type WPARAM_Type is new Interfaces.C.ptrdiff_t;
   subtype LPARAM_Type is LONG_PTR_Type;
   subtype HINSTANCE_Type is HANDLE_Type;
   subtype HICON_Type is HANDLE_Type;
   subtype HCURSOR_Type is HICON_Type;
   subtype HBRUSH_Type is HANDLE_Type;
   subtype LPCTSTR_Type is Interfaces.C.Strings.chars_ptr;
   type WORD_Type is new Interfaces.Unsigned_16;
   type ATOM_Type is new WORD_Type;
   subtype HMENU_Type is HANDLE_TYPE;
   type LRESULT_Type is new LONG_Type;
   subtype HDC_Type is HANDLE_Type;
   type BOOL_Type is new Interfaces.C.int;
   type BYTE_Type is new Interfaces.Unsigned_8;
   subtype HGLRC_Type is HANDLE_Type;

   function HANDLEToInteger is new Ada.Unchecked_Conversion
     (Source => HANDLE_Type,
      Target => Interfaces.C.ptrdiff_t);

   NULLHANDLE : constant HANDLE_Type:=HANDLE_Type(System.Null_Address);
   TRUE       : constant BOOL_Type:=1;
   FALSE      : constant BOOL_Type:=0;

   function MAKEINTRESOURCE
     (wInteger : WORD_Type)
      return LPCTSTR_Type;
   function GET_X_LPARAM
     (lParam : LPARAM_Type)
      return Integer;
   function GET_Y_LPARAM
     (lParam : LPARAM_Type)
      return Integer;

   function LOWORD
     (lParam : LPARAM_Type)
      return WORD_Type;

   function HIWORD
     (lParam : LPARAM_Type)
      return WORD_Type;

   CS_HREDRAW : constant UINT_Type:=2;
   CS_VREDRAW : constant UINT_Type:=1;
   CS_OWNDC   : constant UINT_Type:=32;

   WS_OVERLAPPEDWINDOW : constant DWORD_Type:=16#cf0000#;
   WS_CLIPCHILDREN     : constant DWORD_Type:=16#2000000#;
   WS_CLIPSIBLINGS     : constant DWORD_Type:=16#4000000#;
   WS_EX_APPWINDOW     : constant DWORD_Type:=16#40000#;
   WS_EX_CLIENTEDGE    : constant DWORD_Type:=512;

   WM_MOUSELEAVE    : constant := 16#2A3#;
   WM_PAINT         : constant := 15;
   WM_CREATE        : constant := 1;
   WM_SIZE          : constant := 5;
   WM_SIZING        : constant := 532;
   WM_LBUTTONDBLCLK : constant := 515;
   WM_RBUTTONDBLCLK : constant := 518;
   WM_LBUTTONDOWN   : constant := 513;
   WM_LBUTTONUP     : constant := 514;
   WM_RBUTTONDOWN   : constant := 516;
   WM_RBUTTONUP     : constant := 517;
   WM_MOUSEMOVE     : constant := 512;
   WM_DESTROY       : constant := 2;
   WM_ERASEBKGND    : constant := 20;
   WM_KEYDOWN       : constant := 256;
   WM_KEYUP         : constant := 257;
   WM_CHAR          : constant := 258;
   WM_TIMER         : constant := 275;
   WM_QUIT          : constant := 18;

   SW_SHOW : constant := 5;

   PM_NOREMOVE : constant := 0;
   PM_REMOVE   : constant := 1;
   PM_NOYIELD  : constant := 2;

   PFD_DRAW_TO_WINDOW : constant:=4;
   PFD_SUPPORT_OPENGL : constant:=16#20#;
   PFD_TYPE_RGBA      : constant:=0;
   PFD_MAIN_PLANE     : constant:=0;

   IDI_WINLOGO     : constant:=32517;
   IDI_ASTERISK    : constant:=32516;
   IDI_APPLICATION : constant:=32512;
   IDI_ERROR       : constant:=32513;
   IDI_EXCLAMATION : constant:=32515;
   IDC_ARROW       : constant:=32512;

   SW_SHOWNOACTIVATE : constant:=8;

   STILL_ACTIVE : constant DWORD_Type:=259;

   type WNDPROC_Access is
     access function
       (hWnd    : HWND_Type;
        message : UINT_Type;
        wParam  : WPARAM_Type;
        lParam  : LPARAM_Type)
        return LRESULT_Type;
   pragma Convention(C,WNDPROC_Access);

   type WNDCLASS_Type is
      record
         style         : UINT_Type        := 0;
         lpfnWndProc   : WNDPROC_Access   := null;
         cbClsExtra    : Interfaces.C.int := 0;
         cbWndExtra    : Interfaces.C.int := 0;
         hInstance     : HINSTANCE_Type   := Win32.NULLHANDLE;
         hIcon         : HICON_Type       := Win32.NULLHANDLE;
         hCursor       : HCURSOR_Type     := Win32.NULLHANDLE;
         hbrBackground : HBRUSH_Type      := Win32.NULLHANDLE;
         lpszMenuName  : LPCTSTR_Type     := Interfaces.C.Strings.Null_Ptr;
         lpszClassName : LPCTSTR_Type     := Interfaces.C.Strings.Null_Ptr;
      end record;
   pragma Convention(C,WNDCLASS_Type);

   type CREATESTRUCT_Type is
      record
         lpCreateParams : System.Address:=System.Null_Address;
         hInstance      : HINSTANCE_Type:=Win32.NULLHANDLE;
         hMenu          : HMENU_Type:=Win32.NULLHANDLE;
         hwndParent     : HWND_Type:=Win32.NULLHANDLE;
         cy             : Interfaces.C.int:=0;
         cx             : Interfaces.C.int:=0;
         y              : Interfaces.C.int:=0;
         x              : Interfaces.C.int:=0;
         style          : LONG_Type:=0;
         lpszName       : LPCTSTR_Type:=Interfaces.C.Strings.Null_Ptr;
         lpszClass      : LPCTSTR_Type:=Interfaces.C.Strings.Null_Ptr;
         dwExStyle      : DWORD_Type:=0;
      end record;
   type CREATESTRUCT_Access is access CREATESTRUCT_Type;
   pragma Convention(C,CREATESTRUCT_Type);

   type POINT_Type is
      record
         x : LONG_Type;
         y : LONG_Type;
      end record;

   type MSG_Type is
      record
         hwnd    : HWND_Type;
         message : UINT_Type;
         wParam  : UINT_Type;
         lParam  : UINT_Type;
         time    : DWORD_Type;
         pt      : POINT_Type;
      end record;
   pragma Convention(C,MSG_Type);

   HANDLE_FLAG_INHERIT            : constant DWORD_Type:=1;
   HANDLE_FLAG_PROTECT_FROM_CLOSE : constant DWORD_Type:=2;

   type COMMTIMEOUTS_Type is
      record
         ReadIntervalTimeout         : DWORD_Type:=DWORD_Type'Last;
         ReadTotalTimeoutMultiplier  : DWORD_Type:=0;
         ReadTotalTimeoutConstant    : DWORD_Type:=0;
         WriteTotalTimeoutMultiplier : DWORD_Type:=0;
         WriteTotalTimeoutConstant   : DWORD_Type:=0;
      end record;
   pragma Convention(C,COMMTIMEOUTS_Type);

   type SECURITY_ATTRIBUTES_Type is
      record
         nLength              : DWORD_Type;
         lpSecurityDescriptor : System.Address:=System.Null_Address;
         bInheritHandle       : BOOL_Type;
      end record;
   pragma Convention(C,SECURITY_ATTRIBUTES_Type);
   type SECURITY_ATTRIBUTES_Access is access SECURITY_ATTRIBUTES_Type;

   type OVERLAPPED_Type is
      record
         Internal     : ULONG_PTR_Type := System.Null_Address;
         InternalHigh : ULONG_PTR_Type := System.Null_Address;
         Pointer      : System.Address := System.Null_Address;
         hEvent       : HANDLE_Type    := NULLHANDLE;
      end record;
   pragma Convention(C,OVERLAPPED_Type);

   type PIXELFORMATDESCRIPTOR_Type is
      record
         nSize           : WORD_Type  := 0;
         nVersion        : WORD_Type  := 0;
         dwFlags         : DWORD_Type := 0;
         iPixelType      : BYTE_Type  := 0;
         cColorBits      : BYTE_Type  := 0;
         cRedBits        : BYTE_Type  := 0;
         cRedShift       : BYTE_Type  := 0;
         cGreenBits      : BYTE_Type  := 0;
         cGreenShift     : BYTE_Type  := 0;
         cBlueBits       : BYTE_Type  := 0;
         cBlueShift      : BYTE_Type  := 0;
         cAlphaBits      : BYTE_Type  := 0;
         cAlphaShift     : BYTE_Type  := 0;
         cAccumBits      : BYTE_Type  := 0;
         cAccumRedBits   : BYTE_Type  := 0;
         cAccumGreenBits : BYTE_Type  := 0;
         cAccumBlueBits  : BYTE_Type  := 0;
         cAccumAlphaBits : BYTE_Type  := 0;
         cDepthBits      : BYTE_Type  := 0;
         cStencilBits    : BYTE_Type  := 0;
         cAuxBuffers     : BYTE_Type  := 0;
         iLayerType      : BYTE_Type  := 0;
         bReserved       : BYTE_Type  := 0;
         dwLayerMask     : DWORD_Type := 0;
         dwVisibleMask   : DWORD_Type := 0;
         dwDamageMask    : DWORD_Type := 0;
      end record;
   pragma Convention(C,PIXELFORMATDESCRIPTOR_Type);

   type STARTUPINFO_Type is
      record
         cb              : Interfaces.Unsigned_32;
         lpReserved      : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
         lpDesktop       : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
         lpTitle         : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
         dwX             : Interfaces.Unsigned_32 := 0;
         dwY             : Interfaces.Unsigned_32 := 0;
         dwXSize         : Interfaces.Unsigned_32 := 0;
         dwYSize         : Interfaces.Unsigned_32 := 0;
         dwXCountChars   : Interfaces.Unsigned_32 := 0;
         dwYCountChars   : Interfaces.Unsigned_32 := 0;
         dwFillAttribute : Interfaces.Unsigned_32 := 0;
         dwFlags         : Interfaces.Unsigned_32 := 0;
         wShowWindow     : Interfaces.Unsigned_16 := 0;
         cbReserved2     : Interfaces.Unsigned_16 := 0;
         hStdInput       : HANDLE_Type            := NULLHANDLE;
         hStdOutput      : HANDLE_Type            := NULLHANDLE;
         hStderr         : HANDLE_Type            := NULLHANDLE;
      end record;
   pragma Convention(C,STARTUPINFO_Type);

   type PROCESS_INFORMATION_Type is
      record
         hProcess    : HANDLE_Type := NULLHANDLE;
         hThread     : HANDLE_Type := NULLHANDLE;
         dwProcessID : Interfaces.Unsigned_32 := 0;
         dwThreadID  : Interfaces.Unsigned_32 := 0;
      end record;
   pragma Convention(C,PROCESS_INFORMATION_Type);

   function GetLastError
     return DWORD_TYPE;
   pragma Import(StdCall,GetLastError,"GetLastError");

   STARTF_USESTDHANDLES : constant:=16#100#;

end Win32;
