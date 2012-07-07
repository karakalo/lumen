
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

package Lumen.Window is

   -- Null window; in X, this means the root window is the parent
   No_Window : constant Window_Handle := null;

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

   -- These are what we normally use, but other values are also possible
   Default_Context_Attributes : constant Context_Attributes :=
      (
       (Attr_Red_Size,     8),
       (Attr_Green_Size,   8),
       (Attr_Blue_Size,    8),
       (Attr_Alpha_Size,   8),
       (Attr_Depth_Size,  24),
       (Attr_Stencil_Size, 8)
      );

   -- Local exceptions raised by these procedures
   Connection_Failed : exception;  -- can't connect to X server
   Context_Failed    : exception;  -- can't create or attach OpenGL context
   Not_Available     : exception;  -- can't find a visual with given attributes
   Invalid_ID        : exception;  -- format of explicit visual ID (LUMEN_VISUAL_ID) is invalid

   -- Create a native window, with defaults for configuration intended to
   -- create a "usable" window.  Details about the parameters are:
   --
   -- Win: Object into which the new window's handle will be stored.
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
   -- Direct: Whether you want direct rendering or server-based rendering.
   --
   -- Animated: Whether the GL rendering context will be double-buffered, thus
   -- allowing smooth animation.
   --
   -- Attributes: The various graphic display ("visual") attributes which can
   -- be set.  The defaults work for most modern systems.
   procedure Create (Win           : in out Window_Handle;
                     Parent        : in     Window_Handle      := No_Window;
                     Width         : in     Natural            := 400;
                     Height        : in     Natural            := 400;
                     Events        : in     Wanted_Event_Set   := Want_No_Events;
                     Name          : in     String             := "";
                     Icon_Name     : in     String             := "";
                     Class_Name    : in     String             := "";
                     Instance_Name : in     String             := "";
                     Depth         : in     Color_Depth        := True_Color;
                     Direct        : in     Boolean            := True;
                     Animated      : in     Boolean            := True;
                     Attributes    : in     Context_Attributes := Default_Context_Attributes);

   -- Destroy a native window, including its current rendering context.
   procedure Destroy (Win : in out Window_Handle);

   -- Set various textual names associated with a window.  Null string means
   -- leave the current value unchanged;
   procedure Set_Names (Win           : in Window_Handle;
                        Name          : in String           := "";
                        Icon_Name     : in String           := "";
                        Class_Name    : in String           := "";
                        Instance_Name : in String           := "");

   -- Select a window to use for subsequent OpenGL calls
   procedure Make_Current (Win : in Window_Handle);

   -- Promotes the back buffer to front; only valid if the window is double
   -- buffered, meaning Animated was true when the window was created.  Useful
   -- for smooth animation.
   procedure Swap (Win : in Window_Handle);

   -- Return current window width
   function Width (Win : in Window_Handle) return Natural;

   -- Return current window width
   function Height (Win : in Window_Handle) return Natural;

end Lumen.Window;
