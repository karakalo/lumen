-- Win32 version

with Win32; use Win32;
with Win32.User32; use Win32.User32;
with Win32.GDI32; use Win32.GDI32;
with Win32.OpenGL32; use Win32.OpenGL32;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with System;
with Lumen.Events; use Lumen.Events;
with Lumen.Events.Keys; use Lumen.Events.Keys;

package body Lumen.Window is

   KeyUnknown : constant := 0;

   KeySymbols : constant array(0..255) of Key_Symbol:=
     (KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 0..3
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 4..7
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 8..11
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 12..15
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 16..19
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 20..23
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 24..27
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 28..31
      KeyUnknown,KeyUnknown,KeyUnknown,End_Key,     -- 32..35
      Home      ,Left      ,KeyUnknown,Right,       -- 36..39
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 40..43
      KeyUnknown,KeyUnknown,KP_Delete ,KeyUnknown,  -- 44..47
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 48..51
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 52..55
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 56..59
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 60..63
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 64..67
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 68..71
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 72..75
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 76..79
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 80..83
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 84..87
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 88..91
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 92..95
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 96..99
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 100..103
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 104..107
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 108..111
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 112..115
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 116..119
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 120..123
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 124..127
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 128..131
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 132..135
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 136..139
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 140..143
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 144..147
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 148..151
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 152..155
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 156..159
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 160..163
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 164..167
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 168..171
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 172..175
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 176..179
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 180..183
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 184..187
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 188..191
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 192..195
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 196..199
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 200..203
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 204..207
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 208..211
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 212..215
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 216..219
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 220..223
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 224..227
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 228..231
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 232..235
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 236..239
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 240..243
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 244..247
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown,  -- 248..251
      KeyUnknown,KeyUnknown,KeyUnknown,KeyUnknown); -- 252..255

   KeyCategory : constant array(0..255) of Key_Category:=
     (Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 0..3
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 4..7
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 8..11
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 12..15
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 16..19
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 20..23
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 24..27
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 28..31
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Control,      -- 32..35
      Key_Control,Key_Control,Key_Unknown,Key_Control,  -- 36..39
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 40..43
      Key_Unknown,Key_Unknown,Key_Control, Key_Unknown,   -- 44..47
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 48..51
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 52..55
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 56..59
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 60..63
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 64..67
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 68..71
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 72..75
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 76..79
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 80..83
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 84..87
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 88..91
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 92..95
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 96..99
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 100..103
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 104..107
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 108..111
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 112..115
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 116..119
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 120..123
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 124..127
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 128..131
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 132..135
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 136..139
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 140..143
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 144..147
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 148..151
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 152..155
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 156..159
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 160..163
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 164..167
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 168..171
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 172..175
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 176..179
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 180..183
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 184..187
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 188..191
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 192..195
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 196..199
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 200..203
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 204..207
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 208..211
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 212..215
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 216..219
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 220..223
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 224..227
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 228..231
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 232..235
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 236..239
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 240..243
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 244..247
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown,  -- 248..251
      Key_Unknown,Key_Unknown,Key_Unknown,Key_Unknown); -- 252..255

   function GetModuleHandle
     (lpModuleName : LPCTSTR_Type)
      return HInstance_Type;
   pragma Import(StdCall,GetModuleHandle,"GetModuleHandleA");

   type Win32Window_Type is new Window_Type with
      record
         WindowHandle        : HWND_Type         := NULLHANDLE;
         DeviceContext       : HDC_Type          := NULLHANDLE;
         RenderContext       : HGLRC_Type        := NULLHANDLE;
         DoubleBuffered      : Boolean;
         ContextInitialized  : Boolean           := False;
         MouseButtonsPressed : Button_Set        := NoButtons;
         HasCapture          : Boolean           := False;
         CSTR_ClassName : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         CSTR_Title     : Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.Null_Ptr;
         CurrentModifiers    : Modifier_Set:=No_Modifiers;
         DestroySignalSend   : Boolean           := False;
      end record;
   type Win32Window_Handle is access all Win32Window_Type;

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type;
   pragma Convention(C,WndProc);

   function WndProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LRESULT_Type is

      -- Context belonging to this window
      Win : Win32Window_Handle;

   begin
      -- Extract the Context_Access from the windows LongPtr storage
      -- associated with this window.
      -- Context will be null if WM_CREATE has not been called before
      -- PORTABILITY : Expects Context_Access to be a simple pointer
      --   Shown to work with GNAT GPL 2010 (20100603)
      declare
         function ConvertLongPtrToWin32Window_Handle is new Ada.Unchecked_Conversion
           (Source => LONG_PTR_Type,
            Target => Win32Window_Handle);
      begin
         Win:=ConvertLongPtrToWin32Window_Handle
           (GetWindowLongPtr(hWnd,0));
      end;

      case uMsg is
         when WM_MOUSELEAVE =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_PAINT =>
            if Win.Exposed/=null then
               Win.Exposed
                 (Top => 0,
                  Left => 0,
                  Height => Win.Height,
                  Width => Win.Width);
            end if;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CREATE =>
            declare
               Result : UINT_PTR_Type;
               pragma Unreferenced(Result);
               -- TODO: Maybe you should check this result, but a way
               --       to propagate the error to NewContext is required first.
            begin
               Result:=SetTimer
                 (hwnd        => hwnd,
                  nIDEvent    => 1,
                  uElapse     => 10,
                  lpTimerFunc => System.Null_Address);
            end;

            declare
               function ConvertLParamToCREATESTRUCTPtr is
                 new Ada.Unchecked_Conversion
                   (Source => LPARAM_Type,
                    Target => CREATESTRUCT_Access);

               CreateStruct : constant CREATESTRUCT_Access
                 :=ConvertLParamToCREATESTRUCTPtr(lParam);
               Result : LONG_Type;
               pragma Unreferenced(Result);
            begin
               Result:=User32.SetWindowLongPtr
                 (hWnd      => hWnd,
                  nIndex    => 0,
                  dwNewLong => CreateStruct.lpCreateParams);
            end;
            return 0;
         when WM_SIZE =>
            if Win.Resize/=null then
               Win.Resize
                 (Height => Integer(HIWORD(lParam)),
                  Width  => Integer(LOWORD(lParam)));
            end if;
            return 0;
         when WM_SIZING =>
            return 0;
         when WM_LBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_RBUTTONDBLCLK =>
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONDOWN =>
            if Win.Mouse_Down/=null then
               Win.Mouse_Down
                 (X => GET_X_LPARAM(lParam),
                  Y => GET_Y_LPARAM(lParam),
                  Button => Button_1,
                  Modifiers => Win.CurrentModifiers);
            end if;

            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONUP =>
            if Win.Mouse_Up/=null then
               Win.Mouse_Up
                 (X => GET_X_LPARAM(lParam),
                  Y => GET_Y_LPARAM(lParam),
                  Button => Button_1,
                  Modifiers => Win.CurrentModifiers);
            end if;

            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONDOWN =>
            if Win.Mouse_Down/=null then
               Win.Mouse_Down
                 (X         => GET_X_LPARAM(lParam),
                  Y         => GET_Y_LPARAM(lParam),
                  Button    => Button_2,
                  Modifiers => Win.CurrentModifiers);
            end if;

            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONUP =>
            if Win.Mouse_Up/=null then
               Win.Mouse_Up
                 (X         => GET_X_LPARAM(lParam),
                  Y         => GET_Y_LPARAM(lParam),
                  Button    => Button_2,
                  Modifiers => Win.CurrentModifiers);
            end if;

            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_MOUSEMOVE =>

            if Win.Mouse_Move/=null then
               Win.Mouse_Move
                 (X         => GET_X_LPARAM(lParam),
                  Y         => GET_Y_LPARAM(lParam),
                  Modifiers => Win.CurrentModifiers);
            end if;

            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_DESTROY =>
            declare
               BoolResult : BOOL_Type;
               pragma Unreferenced(BoolResult);
            begin
               BoolResult:=PostMessage
                 (hWnd   => hWnd,
                  Msg    => WM_QUIT,
                  wParam => 0,
                  lParam => System.Null_Address);
            end;
            Win.DestroySignalSend:=True;
            return 0;

         when WM_TIMER =>
--            Paint(Context.all);
            return 0;

         when WM_ERASEBKGND =>
            return 1;

         when WM_KEYDOWN =>
            if wParam<=255 then
               null;
--               GUI.ContextKeyDown
--                 (Context => Context_ClassAccess(Context),
--                  Key     => KeyTable(Integer(wparam)));
            end if;
            return User32.DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_KEYUP =>
            if wParam<=255 then
               null;
--               GUI.ContextKeyUp
--                 (Context => Context_ClassAccess(Context),
--                  Key     => KeyTable(Integer(wParam)));
            end if;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
         when WM_CHAR =>
            declare
               -- Warnings are unnecessary, this is a dirty 32 to 16 bit
               -- convert.
               pragma Warnings(Off);
               function Convert is new Ada.Unchecked_Conversion
                 (Source => WPARAM_Type,
                  Target => Character);
               pragma Warnings(On);
            begin
               if wParam>=32 then
                  if Win.Character/=null then
                     Win.Character(Character'Image(Convert(wParam)),
                                   Modifiers => Win.CurrentModifiers);
                  end if;

               end if;
            end;
            return 0;
         when others =>
            return DefWindowProc
              (hWnd   => hWnd,
               uMsg   => uMsg,
               wParam => wParam,
               lParam => lParam);
      end case;
   end WndProc;
   ---------------------------------------------------------------------------

   procedure Create (Win           : in out Window_Handle;
                     Parent        : in     Window_Handle      := No_Window;
                     Width         : in     Natural            := 400;
                     Height        : in     Natural            := 400;
                     Name          : in     String             := "";
                     Icon_Name     : in     String             := "";
                     Class_Name    : in     String             := "";
                     Instance_Name : in     String             := "";
                     Depth         : in     Color_Depth        := True_Color;
                     Direct        : in     Boolean            := True;
                     Animated      : in     Boolean            := True;
                     Attributes    : in     Context_Attributes := Default_Context_Attributes) is

      use type Interfaces.C.int;

      NWin        : Win32Window_Handle;
      WndClass   : aliased WNDCLASS_Type;
      HInstance  : HINSTANCE_Type;

      dwStyle    : DWORD_Type;
      dwExStyle  : DWORD_Type;

      BoolResult : BOOL_Type;
      HWNDResult : HWND_Type;
      pragma Unreferenced(BoolResult);
      pragma Unreferenced(HWNDResult);

   begin
      -- Create new context object and add it to the contexts list
      NWin:=new Win32Window_Type;

      NWin.DoubleBuffered:=False;

      -- Obtain handle to current process
      HInstance:=GetModuleHandle
        (lpModuleName => Interfaces.C.Strings.Null_Ptr);

      if Class_Name="" then
         NWin.CSTR_ClassName
           := Interfaces.C.Strings.New_String("DefaultLumenClassName");
      else
         NWin.CSTR_ClassName
           := Interfaces.C.Strings.New_String(Class_Name);
      end if;
      NWin.CSTR_Title
        := Interfaces.C.Strings.New_String(Name);

      -- Create and Register a window class for an ordinary window
      WndClass.Style
        := CS_HREDRAW
        or CS_VREDRAW
        or CS_OWNDC;

      WndClass.lpfnWndProc := WndProc'Access;
      WndClass.hInstance   := HInstance;
      WndClass.lpszClassName := NWin.CSTR_ClassName;
      WndClass.cbWndExtra    := System.Address'Size/8;
      WndClass.hIcon         := LoadIcon
        (hInstance  => NULLHANDLE,
         lpIconName => MAKEINTRESOURCE(IDI_APPLICATION));
      WndClass.hCursor       := LoadCursor
        (hInstance    => NULLHANDLE,
         lpCursorName => MAKEINTRESOURCE(IDC_ARROW));

      if RegisterClass
        (lpWndClass => WndClass'Access)=0 then

         null; -- Ignore failed RegisterClass, it just needs to exist.
--         Destroy(Window_Handle(NWin));
--         raise FailedToCreateContext
--           with "RegisterClass failed with "
--             &DWORD_Type'Image(GetLastError);

      end if;

      -- Create the window with the previously created window class
      dwStyle := WS_OVERLAPPEDWINDOW
        or WS_CLIPCHILDREN
        or WS_CLIPSIBLINGS;
      dwExStyle := WS_EX_APPWINDOW
        or WS_EX_CLIENTEDGE;

      NWin.WindowHandle
        := CreateWindowEx
          (dwExStyle    => dwExStyle,
           lpClassName  => NWin.CSTR_ClassName,
           lpWindowName => NWin.CSTR_Title,
           dwStyle      => dwStyle,
           x            => 100,
           y            => 100,
           nwidth       => Interfaces.C.int(Width),
           nheight      => Interfaces.C.int(Height),
           hWndParent   => NULLHANDLE,
           hMenu        => NULLHANDLE,
           hInstance    => HInstance,
           lpParam      => NWin.all'Address);

      NWin.Height:=Height;
      NWin.Width:=Width;

      if NWin.WindowHandle=NULLHANDLE then
         Destroy(Window_Handle(NWin));
         raise FailedToCreateContext
           with "Failed call to CreateWindowEx exited with "
             &DWORD_Type'Image(GetLastError);
      end if;

      -- Obtain a device context for the window
      NWin.DeviceContext := GetDC(NWin.WindowHandle);
      if NWin.DeviceContext=NULLHANDLE then
         Destroy(Window_Handle(NWin));
         raise FailedToCreateContext
           with "Failed call to GetDC exited with "
             &DWORD_Type'Image(GetLastError);
      end if;

      -- Ensure window is visible and focused
      BoolResult:=ShowWindow
        (hWnd     => NWin.WindowHandle,
         nCmdShow => SW_SHOW);

      BoolResult:=SetForegroundWindow
        (hWnd => NWin.WindowHandle);

      HWNDResult:=SetFocus
        (hWnd => NWin.WindowHandle);

      -- Setup OpenGL context
      declare
         pdf         : aliased PIXELFORMATDESCRIPTOR_Type;
         PixelFormat : Interfaces.C.int;
      begin
         pdf.nSize    := PIXELFORMATDESCRIPTOR_Type'Size/8;
         pdf.nVersion := 1;
         pdf.dwFlags
           := PFD_DRAW_TO_WINDOW
           or PFD_SUPPORT_OPENGL;
         pdf.iPixelType := PFD_TYPE_RGBA;
         pdf.cColorBits := 24;
         pdf.cDepthBits := 24;
         pdf.iLayerType := PFD_MAIN_PLANE;
         PixelFormat := ChoosePixelFormat
           (hdc => NWin.DeviceContext,
            ppfd => pdf'Access);
         if PixelFormat=0 then
            Destroy(Window_Handle(NWin));
            raise FailedToCreateContext
              with "Failed to pick a Pixelformat using ChoosePixelFormat. Error code :"
                &Interfaces.C.int'Image(PixelFormat);
         end if;
         if GDI32.SetPixelFormat
           (hdc          => NWin.DeviceContext,
            iPixelFormat => PixelFormat,
            ppfd         => pdf'Access)/=Standard.Win32.TRUE then
            Destroy(Window_Handle(NWin));
            raise FailedToCreateContext
              with "Failed to set the Pixelformat using SetPixelFormat. Error code :"
                &DWORD_Type'Image(GetLastError);
         end if;
      end;

      NWin.RenderContext
        :=wglCreateContext(NWin.DeviceContext);

      if wglMakeCurrent
        (hdc   => NWin.DeviceContext,
         hglrc => NWin.RenderContext)/=Standard.Win32.TRUE then
         Destroy(Window_Handle(NWin));
         raise FailedToCreateContext
           with "Failed call to wglMakeCurrent with "
             &DWORD_Type'Image(GetLastError);
      end if;

      NWin.ContextInitialized:=True;

      Win:=Window_Handle(NWin);
   end Create;

   ---------------------------------------------------------------------------

   procedure Destroy (Win : in out Window_Handle) is

      procedure Free is new Ada.Unchecked_Deallocation(Window_Type'Class,Window_Handle);
      BoolResult : BOOL_Type;
      pragma Unreferenced(BoolResult);
      IntResult  : Interfaces.C.int;
      pragma Unreferenced(IntResult);

      NWin : Win32Window_Handle:=Win32Window_Handle(Win);

   begin

      -- Free Device Context
      if NWin.DeviceContext/=NULLHANDLE then
         IntResult:=ReleaseDC
           (hWnd => NWin.WindowHandle,
            hDC => NWin.DeviceContext);
      end if;

      -- Destroy and free window
      if NWin.WindowHandle/=NULLHANDLE then
         BoolResult:=DestroyWindow
           (hWnd => NWin.WindowHandle);
      end if;

      -- Remove Window class
      BoolResult:=UnregisterClass
        (NWin.CSTR_ClassName,
         GetModuleHandle(Interfaces.C.Strings.Null_Ptr));

      -- Free C String memory
      Interfaces.C.Strings.Free(NWin.CSTR_ClassName);
      Interfaces.C.Strings.Free(NWin.CSTR_Title);

      Free(Win);

   end Destroy;

   ---------------------------------------------------------------------------

   procedure Set_Names (Win           : in Window_Handle;
                        Name          : in String           := "";
                        Icon_Name     : in String           := "";
                        Class_Name    : in String           := "";
                        Instance_Name : in String           := "") is
   begin
      null;
   end Set_Names;

   ---------------------------------------------------------------------------

   procedure Make_Current (Win : in Window_Handle) is
      Win32Win : Win32Window_Handle:=Win32Window_Handle(Win);

      Result : Win32.BOOL_Type;
      Pragma Unreferenced(Result);

   begin  -- Make_Current

      Result:=Win32.OpenGL32.wglMakeCurrent(Win32Win.DeviceContext,Win32Win.RenderContext);

   end Make_Current;

   ---------------------------------------------------------------------------

   -- Promotes the back buffer to front; only valid if the window is double
   -- buffered, meaning Animated was true when the window was created.  Useful
   -- for smooth animation.
   procedure Swap (Win : in Window_Handle) is
      Win32Win : Win32Window_Handle:=Win32Window_Handle(Win);
      Result : BOOL_Type;
      pragma Unreferenced(Result);
   begin  -- Swap
      Result:=Win32.OpenGL32.SwapBuffers(Win32Win.DeviceContext);
   end Swap;

   ---------------------------------------------------------------------------

   -- Return current window width
   function Width (Win : in Window_Handle) return Natural is
   begin  -- Width
      return Win.Width;
   end Width;

   ---------------------------------------------------------------------------

   -- Return current window width
   function Height (Win : in Window_Handle) return Natural is
   begin  -- Height
      return Win.Height;
   end Height;

   ---------------------------------------------------------------------------

   function Process_Events (Win : in Window_Handle)
                            return Boolean is

      lMsg        : aliased MSG_Type;
      Win32Win    : Win32Window_Handle:=Win32Window_Handle(Win);

   begin

      MessageLoop:
      while User32.PeekMessage
        (lpMsg         => lMsg'Access,
         hWnd          => Win32Win.WindowHandle,
         wMsgFilterMin => 0,
         wMsgFilterMax => 0,
         wRemoveMsg    => PM_REMOVE)/=0 loop

         declare
            BoolResult : BOOL_Type;
            LResult    : LRESULT_Type;
            pragma Unreferenced(BoolResult);
            pragma Unreferenced(LResult);
         begin
            BoolResult := TranslateMessage(lMsg'Access);
            LResult    := DispatchMessage(lMsg'Access);
         end;

      end loop MessageLoop;
      ---------------------------------------------------------------------

      return not Win32Win.DestroySignalSend;

   end Process_Events;

   ---------------------------------------------------------------------------

end Lumen.Window;
