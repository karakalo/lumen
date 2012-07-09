-- Win32 version

with Win32; use Win32;
with Win32.User32; use Win32.User32;
with Win32.GDI32; use Win32.GDI32;
with Win32.OpenGL32; use Win32.OpenGL32;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with System;

package body Lumen.Window is

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
--            Paint(Context.all);
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
            -- TODO : Extract Resize
--            GUI.PropagateContextResize
--              (Context => Context_ClassAccess(Context),
--               Width   => Integer(LOWORD(lParam)),
--               Height  => Integer(HIWORD(lParam)));
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
--            MouseDown
--              (Context     => Context,
--               MouseButton => LeftButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_LBUTTONUP =>
--            MouseUp
--              (Context     => Context,
--               MouseButton => LeftButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONDOWN =>
--            MouseDown
--              (Context     => Context,
--               MouseButton => RightButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_RBUTTONUP =>
--            MouseUp
--              (Context     => Context,
--               MouseButton => RightButton,
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);

         when WM_MOUSEMOVE =>
--            ContextMouseMove
--              (Context     => Context_ClassAccess(Context),
--               AbsX        => GET_X_LPARAM(lParam),
--               AbsY        => GET_Y_LPARAM(lParam));
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
--            Context.DestroySignalSend:=True;
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
                  Target => Wide_Character);
               pragma Warnings(On);
            begin
               if wParam>=32 then
                  null;
--                  GUI.ContextCharacterInput
--                    (Context => Context_ClassAccess(Context),
--                     Chars   => UCS2ToUTF8(Convert(wParam)));
               end if;
            end;
            return DefWindowProc
              (hWnd => hWnd,
               uMsg => uMsg,
               wParam => wParam,
               lParam => lParam);
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

      NWin.CSTR_ClassName
        := Interfaces.C.Strings.New_String("OpenGLWindow");
      NWin.CSTR_Title
        := Interfaces.C.Strings.New_String("ParallelSim");

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

         Destroy(Window_Handle(NWin));
         raise FailedToCreateContext
           with "RegisterClass failed with "
             &DWORD_Type'Image(GetLastError);

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
           nwidth       => 1024,
           nheight      => 768,
           hWndParent   => NULLHANDLE,
           hMenu        => NULLHANDLE,
           hInstance    => HInstance,
           lpParam      => NWin.all'Address);

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
   begin  -- Make_Current
      null;
   end Make_Current;

   ---------------------------------------------------------------------------

   -- Promotes the back buffer to front; only valid if the window is double
   -- buffered, meaning Animated was true when the window was created.  Useful
   -- for smooth animation.
   procedure Swap (Win : in Window_Handle) is

   begin  -- Swap
      null;
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

--      if Context.DestroySignalSend
--        and Context.OnClose/=null then
--         Context.OnClose(Context.CallBackObject);
--      end if;

      return True;
   end Process_Events;

   ---------------------------------------------------------------------------

end Lumen.Window;
