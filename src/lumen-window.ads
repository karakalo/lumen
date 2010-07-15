
-- Lumen.Window -- Create and destroy native windows and associated OpenGL
-- rendering contexts
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

-- This code is covered by the ISC License:
--
-- Copyright © 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- The software is provided "as is" and the author disclaims all warranties
-- with regard to this software including all implied warranties of
-- merchantability and fitness. In no event shall the author be liable for any
-- special, direct, indirect, or consequential damages or any damages
-- whatsoever resulting from loss of use, data or profits, whether in an
-- action of contract, negligence or other tortious action, arising out of or
-- in connection with the use or performance of this software.

-- Environment
with Ada.Finalization;

with Lumen.Internal;


package Lumen.Window is

   -- Local exceptions raised by these procedures
   Connection_Failed : exception;  -- can't connect to X server
   Context_Failed    : exception;  -- can't create or attach OpenGL context
   Not_Available     : exception;  -- can't find a visual with given attributes

   -- Handle for an OpenGL rendering context
   subtype Context_Handle is Internal.GLX_Context;

   -- Means "no GL context"; for Create, means create a new one
   No_Context : constant Context_Handle := Internal.Null_Context;

   -- Types of events wanted, and a set listing them.
   type Wanted_Event is (Want_Key_Press, Want_Key_Release, Want_Button_Press,  Want_Button_Release,
                         Want_Window_Enter, Want_Window_Leave,
                         Want_Pointer_Move, Want_Pointer_Drag,
                         Want_Exposure, Want_Focus_Change);
   type Wanted_Event_Set is array (Wanted_Event) of Boolean;
   Want_No_Events  : Wanted_Event_Set := (others => False);
   Want_All_Events : Wanted_Event_Set := (others => True);

   -- Rendering context's color depth
   type Color_Depth is (Pseudo_Color, True_Color);

   -- OpenGL context ("visual") attribute specifiers
   type Context_Attribute_Name is
      (
       Attr_None,
       Attr_Use_GL,             -- unused
       Attr_Buffer_Size,        -- color index buffer size, ignored if TrueColor
       Attr_Level,              -- buffer level for over/underlays
       Attr_RGBA,               -- set by Depth => TrueColor
       Attr_Doublebuffer,       -- set by Animate => True
       Attr_Stereo,             -- wow, you have stereo visuals?
       Attr_Aux_Buffers,        -- number of auxiliary buffers
       Attr_Red_Size,           -- bit depth, red
       Attr_Green_Size,         -- bit depth, green
       Attr_Blue_Size,          -- bit depth, blue
       Attr_Alpha_Size,         -- bit depth, alpha
       Attr_Depth_Size,         -- depth buffer size
       Attr_Stencil_Size,       -- stencil buffer size
       Attr_Accum_Red_Size,     -- accumulation buffer bit depth, red
       Attr_Accum_Green_Size,   -- accumulation buffer bit depth, green
       Attr_Accum_Blue_Size,    -- accumulation buffer bit depth, blue
       Attr_Accum_Alpha_Size    -- accumulation buffer bit depth, alpha
      );

   type Context_Attribute (Name  : Context_Attribute_Name := Attr_None) is record
      case Name is
         when Attr_None | Attr_Use_GL | Attr_RGBA | Attr_Doublebuffer | Attr_Stereo =>
            null;  -- all present or not, no value
         when Attr_Level =>
            Level : Integer := 0;
         when Attr_Buffer_Size | Attr_Aux_Buffers | Attr_Depth_Size | Attr_Stencil_Size |
              Attr_Red_Size | Attr_Green_Size | Attr_Blue_Size | Attr_Alpha_Size |
              Attr_Accum_Red_Size | Attr_Accum_Green_Size | Attr_Accum_Blue_Size | Attr_Accum_Alpha_Size =>
            Size : Natural := 0;
      end case;
   end record;

   type Context_Attributes is array (Positive range <>) of Context_Attribute;

   -- These are what we normally use, but other values are also possible
   Default_Context_Attributes : constant Context_Attributes :=
      (
       (Attr_Red_Size,    8),
       (Attr_Green_Size,  8),
       (Attr_Blue_Size,   8),
       (Attr_Alpha_Size,  8),
       (Attr_Depth_Size, 24)
      );

   -- Handle for a Lumen window
   type Info_Pointer is access Internal.Window_Info;
   type Handle is new Ada.Finalization.Limited_Controlled with record
      Info : Info_Pointer;
   end record;
   procedure Finalize   (Win : in out Handle);

   -- Null window; in X, this means the root window is the parent
   function No_Window return Handle;

   -- Create a native window, with defaults for configuration intended to
   -- create a "usable" window.  Details about the parameters are:
   --
   -- Parent: Handle of an existing window that will act as the new window's
   -- parent, or No_Window, meaning an independent top-level window.
   --
   -- Width, Height: Window dimensions in pixels.
   --
   -- Events: Set of events this window wishes to receive.
   --
   -- Name: Name associated with new window, often displayed in the window's
   -- title bar.  If blank, the executable's filename is used.
   --
   -- Icon_Name: Name associated with the window's icon.  If blank, the
   -- executable's filename is used.
   --
   -- Class_Name, Instance_Name: Together these make up the window's "class".
   -- If blank, the class name is set to the executable's filename with
   -- initial capitalization; the instance name is set to the executable's
   -- filename.
   --
   -- Context: An existing GL rendering context to be used in the new window,
   -- or No_Context, meaning a new context is created for the window.
   --
   -- Depth: The color depth of the GL rendering context, either true-color or
   -- indexed/pseudo-color.
   --
   -- Animated: Whether the GL rendering context will be double-buffered, thus
   -- allowing smooth animation.
   procedure Create (Win           : in out Handle;
                     Parent        : in     Handle             := No_Window;
                     Width         : in     Natural            := 400;
                     Height        : in     Natural            := 400;
                     Events        : in     Wanted_Event_Set   := Want_No_Events;
                     Name          : in     String             := "";
                     Icon_Name     : in     String             := "";
                     Class_Name    : in     String             := "";
                     Instance_Name : in     String             := "";
                     Context       : in     Context_Handle     := No_Context;
                     Depth         : in     Color_Depth        := True_Color;
                     Animated      : in     Boolean            := True;
                     Attributes    : in     Context_Attributes := Default_Context_Attributes);

   -- Destroy a native window, including its current rendering context.
   procedure Destroy (Win : in out Handle);

   -- Set various textual names associated with a window.  Null string means
   -- leave the current value unchanged;
   procedure Set_Names (Win           : in Handle;
                        Name          : in String           := "";
                        Icon_Name     : in String           := "";
                        Class_Name    : in String           := "";
                        Instance_Name : in String           := "");

   -- Create an OpenGL rendering context; needed only when you want a second
   -- or subsequent context for a window, since Create makes one to start
   -- with
   function Create_Context (Win : Handle) return Context_Handle;

   -- Destroy a window's OpenGL rendering context; should be followed either
   -- by a Make_Current or a Destroy_Window
   procedure Destroy_Context (Win : in out Handle);

   -- Make a rendering context the current one for a window
   procedure Make_Current (Win     : in out Handle;
                           Context : in     Context_Handle);

   -- Promotes the back buffer to front; only valid if the window is double
   -- buffered, meaning Animated was true when the window was created.  Useful
   -- for smooth animation.
   procedure Swap (Win : in Handle);

private

   pragma Linker_Options ("-lX11");
   pragma Linker_Options ("-lGL");

end Lumen.Window;
