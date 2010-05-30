
-- Lumen.Internal -- Internal declarations not intended for user applications
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
with System;
with Ada.Calendar;


package Lumen.Internal is

   ---------------------------------------------------------------------------

   -- Xlib stuff needed for our window info record
   type Atom            is new Long_Integer;
   type Display_Pointer is new System.Address;
   Null_Display_Pointer : constant Display_Pointer := Display_Pointer (System.Null_Address);
   type Screen_Depth    is new Natural;
   type Screen_Number   is new Natural;
   type Visual_ID       is new Long_Integer;
   type Window_ID       is new Long_Integer;

   type X_Visual_Info is record
      Visual        : System.Address;
      Visual_Ident  : Visual_ID;
      Screen        : Screen_Number;
      Depth         : Screen_Depth;
      Class         : Integer;
      Red_Mask      : Long_Integer;
      Green_Mask    : Long_Integer;
      Blue_Mask     : Long_Integer;
      Colormap_Size : Natural;
      Bits_Per_RGB  : Natural;
   end record;
   type X_Visual_Info_Pointer is access all X_Visual_Info;

   ---------------------------------------------------------------------------

   -- The GL rendering context type
   type GLX_Context is new System.Address;
   Null_Context : constant GLX_Context := GLX_Context (System.Null_Address);

   ---------------------------------------------------------------------------

   -- A time that won't ever happen during the execution of a Lumen app
   Never : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => Ada.Calendar.Year_Number'First,
                                                               Month => Ada.Calendar.Month_Number'First,
                                                               Day   => Ada.Calendar.Day_Number'First);
   -- The native window type
   type Window_Info is record
      Display     : Display_Pointer       := Null_Display_Pointer;
      Window      : Window_ID             := 0;
      Visual      : X_Visual_Info_Pointer := null;
      Width       : Natural               := 0;
      Height      : Natural               := 0;
      Prior_Frame : Ada.Calendar.Time     := Never;
      App_Start   : Ada.Calendar.Time     := Never;
      Last_Start  : Ada.Calendar.Time     := Never;
      App_Frames  : Long_Integer          := 0;
      Last_Frames : Long_Integer          := 0;
      Context     : GLX_Context           := Null_Context;
   end record;

   ---------------------------------------------------------------------------

   -- Our "delete window" atom value
   Delete_Window_Atom : Atom;

   ---------------------------------------------------------------------------

   -- The maximum length of an event data record
   type Padding is array (1 .. 23) of Long_Integer;

   -- Binding to XNextEvent, used by Window for mapping notify events, and by
   -- Events for everything else
   procedure X_Next_Event (Display : in Display_Pointer;
                           Event   : in System.Address);
   pragma Import (C, X_Next_Event, "XNextEvent");

   ---------------------------------------------------------------------------

end Lumen.Internal;
