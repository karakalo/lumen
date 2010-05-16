
-- Lumen.Window -- Create and destroy native windows and associated OpenGL
-- rendering contexts
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

-- This code is covered by the ISC License:
--
-- Copyright (c) 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
-- SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
-- IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-- The declarations below include a minimal Xlib binding, adapted from code by
-- Hans-Frieder Vogt and Vadim Godunko.  Their code was GPLv2, but I've raped
-- it so badly, and they haven't touched it in so long, that I feel okay about
-- including its derivation here.  Also included is a minimal binding to GLX,
-- adapted from another bit of abandonware originally by David Holm.  His
-- license was a different one still, and the above excuse also applies to it.


-- Environment
with System;
with GL;

package body Lumen.Window is

   ---------------------------------------------------------------------------

   -- Xlib stuff needed for our window info record
   type Display_Pointer is new System.Address;
   Null_Display_Pointer : constant Display_Pointer := Display_Pointer (System.Null_Address);
   type Window_ID is new Long_Integer;

   ---------------------------------------------------------------------------

   -- The (opaque) native window type
   type Window_Info is record
      Display : Display_Pointer := Null_Display_Pointer;
      Window  : Window_ID       := 0;
      Context : Context_Handle  := null;
   end record;

   ---------------------------------------------------------------------------

   -- A GLX type needed by our context info record
   type GLX_Context is new System.Address;
   Null_Context : constant GLX_Context := GLX_Context (System.Null_Address);

   ---------------------------------------------------------------------------

   -- The (opaque) rendering context type
   type Context_Info is record
      Context : GLX_Context := Null_Context;
   end record;

   ---------------------------------------------------------------------------

   -- More Xlib stuff, needed by both Create and Destroy

   ---------------------------------------------------------------------------

   -- Create a native window
   procedure Create (Win     :    out Handle;
                     Options : in     Configuration) is

      -- Xlib types needed only by Create
      type Alloc_Mode               is (Alloc_None, Alloc_All);
      type Atom                     is new Long_Integer;
      type Atom_Array               is array (Positive range <>) of Atom;
      type Color_Depth              is new Natural;
      type Colormap_ID              is new Long_Integer;
      type Data_Format_Type         is (Invalid, Bits_8, Bits_16, Bits_32);
      for  Data_Format_Type         use (Invalid => 0, Bits_8 => 8, Bits_16 => 16, Bits_32 => 32);
      type Screen_Number            is new Natural;
      type X_Window_Attributes_Mask is mod 2 ** Integer'Size;
      type Visual_ID                is new Long_Integer;
      type Window_Class             is (Copy_From_Parent, Input_Output, Input_Only);
      type X_Event_Mask             is mod 2 ** Long_Integer'Size;

      subtype Dimension is Short_Integer;
      subtype Pixel     is Long_Integer;
      subtype Position  is Short_Integer;

      -- An extremely abbreviated version of the XSetWindowAttributes
      -- structure, containing only the fields we care about.
      --
      -- NOTE: offset multiplier values are for 64-bit systems; values for
      -- 32-bit systems are 10 and 13, since long size equals int size and the
      -- record has no padding.
      Long_Bytes : constant := Long_Integer'Size / 8;
      Long_Bits  : constant := Long_Integer'Size - 1;
      type X_Set_Window_Attributes is record
         Event_Mask  : X_Event_Mask := 0;
         Colormap    : Colormap_ID  := 0;
      end record;
      for X_Set_Window_Attributes use record
         Event_Mask  at Long_Bytes *  9 range 0 .. Long_Bits;
         Colormap    at Long_Bytes * 12 range 0 .. Long_Bits;
      end record;

      type X_Class_Hint is record
         Instance_Name : System.Address;
         Class_Name    : System.Address;
      end record;

      type X_Text_Property is record
         Value    : System.Address;
         Encoding : Atom;
         Format   : Data_Format_Type;
         NItems   : Long_Integer;
      end record;

      type X_Visual_Info is record
         Visual        : System.Address;
         Visual_Ident  : Visual_ID;
         Screen        : Screen_Number;
         Depth         : Color_Depth;
         Class         : Integer;
         Red_Mask      : Long_Integer;
         Green_Mask    : Long_Integer;
         Blue_Mask     : Long_Integer;
         Colormap_Size : Natural;
         Bits_Per_RGB  : Natural;
      end record;
      type X_Visual_Info_Pointer is access all X_Visual_Info;

      -- Xlib functions needed only by Create
      function X_Create_Colormap (Display : Display_Pointer;
                                  W       : Window_ID;
                                  Visual  : System.Address;
                                  Alloc   : Alloc_Mode)
      return Colormap_ID;
      pragma Import (C, X_Create_Colormap, "XCreateColormap");

      function X_Create_Window (Display      : Display_Pointer;
                                Parent       : Window_ID;
                                X            : Position;
                                Y            : Position;
                                Width        : Dimension;
                                Height       : Dimension;
                                Border_Width : Natural;
                                Depth        : Color_Depth;
                                Class        : Window_Class;
                                Visual       : System.Address;
                                Valuemask    : X_Window_Attributes_Mask;
                                Attributes   : System.Address)
      return Window_ID;
      pragma Import (C, X_Create_Window, "XCreateWindow");

      function X_Default_Screen (Display : Display_Pointer) return Screen_Number;
      pragma Import (C, X_Default_Screen, "XDefaultScreen");

      function X_Intern_Atom (Display        : Display_Pointer;
                              Name           : System.Address;
                              Only_If_Exists : Natural) return Atom;
      pragma Import (C, X_Intern_Atom, "XInternAtom");

      procedure X_Map_Window (Display : in Display_Pointer;   W : in Window_ID);
      pragma Import (C, X_Map_Window, "XMapWindow");

      function X_Open_Display (Display_Name : System.Address := System.Null_Address) return Display_Pointer;
      pragma Import (C, X_Open_Display, "XOpenDisplay");

      function X_Root_Window (Display : Display_Pointer;   Screen_Num : Screen_Number) return Window_ID;
      pragma Import (C, X_Root_Window, "XRootWindow");

      procedure X_Set_Class_Hint (Display : in Display_Pointer;
                                  Window  : in Window_ID;
                                  Hint    : in X_Class_Hint);
      pragma Import (C, X_Set_Class_Hint, "XSetClassHint");

      procedure X_Set_WM_Name (Display   : in Display_Pointer;
                               W         : in Window_ID;
                               Text_Prop : in System.Address);
      pragma Import (C, X_Set_WM_Name, "XSetWMName");

      procedure X_Set_WM_Protocols (Display   : in Display_Pointer;
                                    Window    : in Window_ID;
                                    Protocols : in System.Address;
                                    Count     : in Integer);
      pragma Import (C, X_Set_WM_Protocols, "XSetWMProtocols");

      -- Xlib constants needed only by Create
      Configure_Event_Mask : constant X_Window_Attributes_Mask := 2#00_1000_0000_0000#;  -- 11th bit
      Configure_Colormap   : constant X_Window_Attributes_Mask := 2#10_0000_0000_0000#;  -- 13th bit

      -- Atom names
      WM_Del          : String := "WM_DELETE_WINDOW" & ASCII.NUL;
      String_Encoding : String := "STRING" & ASCII.NUL;

      -- Property names
      Instance_Name    : String := "fixme" & ASCII.NUL;
      Class_Name       : String := "Lumen" & ASCII.NUL;
      Application_Name : String := "Fixme";

      ------------------------------------------------------------------------

      -- GLX types needed only by Create
      type GLX_Attribute_List     is array (1 .. 2) of Integer;
      type GLX_Attribute_List_Ptr is new System.Address;

      -- GLX functions needed only by Create
      function GLX_Choose_Visual (Display        : Display_Pointer;
                                  Screen         : Screen_Number;
                                  Attribute_List : GLX_Attribute_List_Ptr)
      return X_Visual_Info_Pointer;
      pragma Import (C, GLX_Choose_Visual, "glXChooseVisual");

      function GLX_Create_Context (Display    : Display_Pointer;
                                   Visual     : X_Visual_Info_Pointer;
                                   Share_List : GLX_Context;
                                   Direct     : GL.GLboolean)
      return GLX_Context;
      pragma Import (C, GLX_Create_Context, "glXCreateContext");

      function GLX_Make_Current (Display  : Display_Pointer;
                                 Drawable : Window_ID;
                                 Context  : GLX_Context)
      return GL.GLboolean;
      pragma Import (C, GLX_Make_Current, "glXMakeCurrent");

      -- GLX constants needed only by Create
      GLX_None : constant := 0;
      GLX_RGBA : constant := 4;

      ------------------------------------------------------------------------

      -- Temporary
      Key_Press_Mask        : constant X_Event_Mask := 2#00_0000_0000_0000_0001#;  -- 1st bit
      Exposure_Mask         : constant X_Event_Mask := 2#00_1000_0000_0000_0000#;  -- 15th bit
      Structure_Notify_Mask : constant X_Event_Mask := 2#10_0000_0000_0000_0000#;  -- 17th bit

      -- Variables used in Create
      Con_Attributes : GLX_Attribute_List := (GLX_RGBA, GLX_None);
      Context        : GLX_Context;
      Did            : GL.GLboolean;
      Display        : Display_Pointer;
      Mask           : X_Event_Mask := Key_Press_Mask or Exposure_Mask or Structure_Notify_Mask;
      Name_Property  : X_Text_Property;
      Protocol_Atom  : Atom_Array (1 .. 1);
      Visual         : X_Visual_Info_Pointer;
      Win_Attributes : X_Set_Window_Attributes;
      Window         : Window_ID;

      ------------------------------------------------------------------------

   begin  -- Create

      -- Connect to the X server
      Display := X_Open_Display;
      if Display = Null_Display_Pointer then
         raise Connection_Failed;
      end if;

      -- Get an appropriate visual
      Visual := GLX_Choose_Visual (Display, X_Default_Screen (Display), GLX_Attribute_List_Ptr (Con_Attributes'Address));

      -- Create the window and map it
      Win_Attributes.Event_Mask := Mask;
      Win_Attributes.Colormap   := X_Create_Colormap (Display, X_Root_Window (Display, Visual.Screen), Visual.Visual, Alloc_None);
      Window := X_Create_Window (Display, X_Root_Window (Display, Visual.Screen), 0, 0, 100, 100, 0,
                                 Visual.Depth, Input_Output, Visual.Visual,
                                 Configure_Colormap or Configure_Event_Mask, Win_Attributes'Address);
      X_Map_Window (Display, Window);

      -- Tell the window manager that we want the close button sent to us
      Protocol_Atom (1) := X_Intern_Atom (Display, WM_Del'Address, 0);
      X_Set_WM_Protocols (Display, Window, Protocol_Atom'Address, Protocol_Atom'Length);

      -- Set the window's name and class
      Name_Property := (Application_Name'Address,
                        X_Intern_Atom (Display, String_Encoding'Address, 0),
                        Bits_8,
                        Application_Name'Length);
      X_Set_WM_Name (Display, Window, Name_Property'Address);
      X_Set_Class_Hint (Display, Window, (Instance_Name'Address, Class_Name'Address));

      -- Connect the OpenGL context to the new X window
      Context := GLX_Create_Context (Display, Visual, GLX_Context (System.Null_Address), GL.GL_TRUE);
      Did := GLX_Make_Current (Display, Window, Context);
      declare
         use GL;
      begin
         if Did /= GL_TRUE then
            raise Context_Failed;
         end if;
      end;

      -- Return the results
      Win := new Window_Info'(Display => Display,
                              Window  => Window,
                              Context => new Context_Info'(Context => Context));
   end Create;

   ---------------------------------------------------------------------------

   -- Destroy a native window, including its current rendering context.
   procedure Destroy (Win : in out Handle) is
   begin  -- Destroy
      null;
   end Destroy;

   ---------------------------------------------------------------------------

   -- Create an OpenGL rendering context; needed only when you want a second
   -- or subsequent context for a window, since Create makes one to start
   -- with
   procedure Create_Context (Win     : in     Handle;
                             Context :    out Context_Handle) is
   begin  -- Create_Context
      null;
   end Create_Context;

   ---------------------------------------------------------------------------

   -- Destroy an OpenGL rendering context
   procedure Destroy_Context (Win     : in     Handle;
                              Context :    out Context_Handle) is
   begin  -- Destroy_Context
      null;
   end Destroy_Context;

   ---------------------------------------------------------------------------

   -- Make a rendering context the current one for a window
   procedure Make_Current (Win     : in out Handle;
                           Context : in     Context_Handle) is
   begin  -- Make_Current
      null;
   end Make_Current;

   ---------------------------------------------------------------------------

end Lumen.Window;
